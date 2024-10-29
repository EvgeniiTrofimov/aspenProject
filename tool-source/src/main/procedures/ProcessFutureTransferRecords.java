/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.*;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2SAXBuilder;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.JDOMException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Automatically transfers students to their receiving school based upon the transfer date field.
 * <p>
 * Runs as a scheduled job within a district context. All student records with a transfer date on or
 * before the current run date will be queried and processed.
 *
 * @author Follett Software Company
 */
public class ProcessFutureTransferRecords extends ProcedureJavaSource {
    /**
     * Extensive logging will appear in the job results log when enabled.
     */
    private static final boolean enableLogging = true;

    /**
     * Students that were unsuccessfully transferred will have transfer information deleted so they
     * will not be transferred the next time the procedure is ran.
     */
    private static final boolean deleteUnsuccessfulTransfers = true;

    /**
     * This is solely used to determine run time of procedure if logging is enabled.
     */
    private long m_procedureInitialized = 0;

    private ModelProperty studentEnrollmentDateModelProperty;
    private ModelProperty studentEnrollmentCodeModelProperty;
    private ModelProperty studentEnrollmentReasonModelProperty;

    private MessageResources messages;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        DateFormat formatter = null;
        messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        if (enableLogging) {
            m_procedureInitialized = System.currentTimeMillis();

            formatter = new SimpleDateFormat("HH:mm");

            logMessage("***** Process Future Transfers Procedure Initiated *****");
            logMessage("Processing start time: " + formatter.format(new PlainTime(m_procedureInitialized)));
        }

        Collection<SisStudent> transferredStudents = new ArrayList<>();
        Collection<SisStudent> notTransferredStudents = new ArrayList<>();

        // We are running in a school context
        if (getSchool() != null) {
            if (enableLogging) {
                logMessage(messages.getMessage("procedure.processFutureTransferRecords.schoolContext",
                        getSchool().getName()));
            }

            Collection<SisStudent> studentsToBeTransferred = getStudentsToBeTransferred(getSchool().getOid());
            transferStudents(studentsToBeTransferred, transferredStudents, notTransferredStudents);
        } else {
            if (enableLogging) {
                logMessage(messages.getMessage("procedure.processFutureTransferRecords.runAllSchools"));
            }

            Collection<SisStudent> studentsToBeTransferred = getStudentsToBeTransferred(null);
            transferStudents(studentsToBeTransferred, transferredStudents, notTransferredStudents);
        }

        EnrollmentManager enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        if (deleteUnsuccessfulTransfers) {
            for (SisStudent student : notTransferredStudents) {
                enrollmentManager.deleteScheduleTransfer(student);
                getBroker().saveBeanForced(student);
            }
        }

        logMessage("");
        logMessage(messages.getMessage("procedure.processFutureTransferRecords.studentsTransferred"));
        for (SisStudent student : transferredStudents) {
            logMessage(student.getNameView());
        }

        logMessage("");
        logMessage(messages.getMessage("procedure.processFutureTransferRecords.studentsNotTransferred"));
        for (SisStudent student : notTransferredStudents) {
            logMessage(student.getNameView());
        }

        if (enableLogging) {
            long endTimeMillis = System.currentTimeMillis();
            logMessage("");
            logMessage("***** Process Future Transfers Procedure Completed *****");
            logMessage("Processing end time: " + formatter.format(new PlainTime(endTimeMillis)));
            logMessage("Total runtime: " + getDuration(endTimeMillis - m_procedureInitialized));
        }
    }

    private void transferStudents(Collection<SisStudent> studentsToBeTransferred,
                                  Collection<SisStudent> transferredStudents,
                                  Collection<SisStudent> notTransferredStudents) {
        EnrollmentManager enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        initializeStudentEnrollmentModelProperties();

        for (SisStudent student : studentsToBeTransferred) {
            logMessage("");
            logMessage(messages.getMessage("procedure.processFutureTransferRecords.beginTransferring",
                    student.getNameView()));
            TransferDetails transferDetails = new TransferDetails(student);
            boolean isValid = transferDetails.parseAndValidateTransferDetails();

            if (!isValid) {
                notTransferredStudents.add(student);
                continue;
            }

            // Save off transferSchool since it gets cleared out.
            if (student.getTransferSchool() == null) {
                logMessage(messages.getMessage("procedure.processFutureTransferRecords.transferSchoolNotSet",
                        student.getNameView()));
                logMessage("");
                notTransferredStudents.add(student);
                continue;
            }

            String originalSchoolName = student.getSchool().getName();
            String transferSchoolName = student.getTransferSchool().getName();

            List<ValidationError> validationErrors = enrollmentManager.transferStudent(student, (SisUser) getUser(),
                    transferDetails.getWithdrawalDate(),
                    transferDetails.getWithdrawalCode(), transferDetails.getWithdrawalReason(),
                    transferDetails.getEntryDate(), transferDetails.getEntryCode(), transferDetails.getEntryReason(),
                    student.getTransferSchool(), null, transferDetails.getStudentEnrollment(),
                    getLocale());

            if (validationErrors.isEmpty()) {
                logMessage(messages.getMessage("procedure.processFutureTransferRecords.successfulTransfer",
                        student.getNameView(), originalSchoolName, student.getSchool().getName()));
                transferredStudents.add(student);
            } else {
                logMessage(messages.getMessage("procedure.processFutureTransferRecords.unsuccessfulTransfer",
                        student.getNameView(), originalSchoolName, transferSchoolName));
                for (ValidationError validationError : validationErrors) {
                    logMessage(validationError.toString());
                }
                notTransferredStudents.add(student);
            }
        }
    }

    private Collection<SisStudent> getStudentsToBeTransferred(String schoolOid) {
        X2Criteria criteria = new X2Criteria();
        if (schoolOid != null) {
            criteria.addEqualTo(Student.COL_TRANSFER_SCHOOL_OID, schoolOid);
        }
        criteria.addEqualTo(Student.COL_TRANSFER_PENDING_INDICATOR, Boolean.TRUE);
        criteria.addLessOrEqualThan(Student.COL_TRANSFER_ENROLLMENT_DATE,
                new PlainDate(OrganizationManager.getTimeZone(getOrganization())));

        QueryByCriteria query = new QueryByCriteria(Student.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }

    private void initializeStudentEnrollmentModelProperties() {
        DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        studentEnrollmentDateModelProperty = new ModelProperty(StudentEnrollment.class,
                StudentEnrollment.COL_ENROLLMENT_DATE, districtDictionary);

        studentEnrollmentCodeModelProperty = new ModelProperty(StudentEnrollment.class,
                StudentEnrollment.COL_ENROLLMENT_CODE, districtDictionary);

        studentEnrollmentReasonModelProperty = new ModelProperty(StudentEnrollment.class,
                StudentEnrollment.COL_REASON_CODE, districtDictionary);
    }

    /**
     * Converts the milliseconds into Hours, Minutes, and Seconds for readability.
     *
     * @param milliseconds long
     * @return String
     */
    private String getDuration(long milliseconds) {
        long hours = TimeUnit.MILLISECONDS.toHours(milliseconds);
        milliseconds -= TimeUnit.HOURS.toMillis(hours);

        long minutes = TimeUnit.MILLISECONDS.toMinutes(milliseconds);
        milliseconds -= TimeUnit.MINUTES.toMillis(minutes);

        long seconds = TimeUnit.MILLISECONDS.toSeconds(milliseconds);

        return hours + " Hours, " + minutes + " Minutes, " + seconds + " Seconds";
    }

    /**
     * Represents information contained in the transfer details xml on the Student record.
     */
    private class TransferDetails {
        private Student student;

        private String withdrawalCode;
        private String withdrawalReason;
        private PlainDate withdrawalDate;

        private String entryCode;
        private String entryReason;
        private PlainDate entryDate;

        // Contains additional info such as UDFs that will be set both on the withdrawal and entry
        // student enrollment records.
        private StudentEnrollment studentEnrollment;

        private boolean isValid = true;

        public TransferDetails(Student student) {
            this.student = student;
        }

        public boolean parseAndValidateTransferDetails() {
            if (StringUtils.isBlank(student.getTransferDetails())) {
                logTransferDetailsMessage();
                return false;
            }

            org.jdom.Document document = null;
            try {
                X2SAXBuilder builder = new X2SAXBuilder();
                document = builder.build(new ByteArrayInputStream(student.getTransferDetails().getBytes()));
            } catch (JDOMException | IOException e) {
                logTransferDetailsMessage();
                return false;
            }

            Element root = document.getRootElement();
            if (root == null) {
                isValid = false;
            } else {
                parseAndValidateRootXmlElement(root);
            }

            if (!isValid) {
                logTransferDetailsMessage();
            }

            return isValid;
        }

        private void parseAndValidateRootXmlElement(Element root) {
            Element withdrawalElement = root.getChild(EnrollmentManager.WITHDRAWAL);
            Element entryElement = root.getChild(EnrollmentManager.ENTRY);

            if (withdrawalElement == null) {
                logMessage("Cannot find element in transfer details xml with name: " + EnrollmentManager.WITHDRAWAL);
                isValid = false;
            }

            if (entryElement == null) {
                logMessage("Cannot find element in transfer details xml with name: " + EnrollmentManager.ENTRY);
                isValid = false;
            }

            if (isValid) {
                parseWithdrawalElement(withdrawalElement);
                parseEntryElement(entryElement);
                parseAdditionalInfo(root);
            }
        }

        private void parseEntryElement(Element entryElement) {
            entryDate = parseDateAttribute(entryElement);
            entryCode = parseAttribute(entryElement, studentEnrollmentCodeModelProperty.getFieldId());
            entryReason = parseAttribute(entryElement, studentEnrollmentReasonModelProperty.getFieldId());
        }

        private void parseWithdrawalElement(Element withdrawalElement) {
            withdrawalDate = parseDateAttribute(withdrawalElement);
            withdrawalCode = parseAttribute(withdrawalElement, studentEnrollmentCodeModelProperty.getFieldId());
            withdrawalReason = parseAttribute(withdrawalElement, studentEnrollmentReasonModelProperty.getFieldId());
        }

        private PlainDate parseDateAttribute(Element element) {
            Attribute dateAttribute = element.getAttribute(studentEnrollmentDateModelProperty.getFieldId());
            if (dateAttribute == null) {
                logMessage("Cannot find attribute on element: " + element.getName()
                        + " in transfer details xml with attribute name: "
                        + studentEnrollmentDateModelProperty.getFieldId());
                isValid = false;
                return null;
            }

            PlainDate date = DateUtils.getDate(dateAttribute.getValue());
            if (date == null) {
                logMessage("Unable to parse element: " + element.getName() + " attribute date with value: "
                        + dateAttribute.getValue());
                isValid = false;
                return null;
            }

            return date;
        }

        private String parseAttribute(Element element, String attributeName) {
            Attribute attribute = element.getAttribute(attributeName);
            if (attribute != null) {
                return attribute.getValue();
            }

            return null;
        }

        /**
         * Parses additional fields that need to be set when both student enrollment records are
         * created.
         */
        private void parseAdditionalInfo(Element root) {
            studentEnrollment = X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());
            Element additionalInfoElement = root.getChild(EnrollmentManager.ADDITIONAL_INFO);
            if (additionalInfoElement != null) {
                List<Attribute> attributes = additionalInfoElement.getAttributes();

                for (Attribute attribute : attributes) {
                    try {
                        ModelProperty property =
                                new ModelProperty(attribute.getName(), getBroker().getPersistenceKey());
                        Object attributeValue = attribute.getValue();
                        Converter converter = ConverterFactory.getConverterForClass(property.getField().getJavaType());
                        if (converter != null) {
                            attributeValue = converter.stringToJava(attribute.getValue());
                        }

                        studentEnrollment.setFieldValueByProperty(property, attributeValue);
                    } catch (BeanPathException | InvalidDictionaryIdException ex) {
                        logMessage("Exception: " + ex);
                        logMessage("Warning: Unable to set value on Student Enrollment with bean path: "
                                + attribute.getName() + " and value: " + attribute.getValue());
                    }
                }
            }
        }

        private void logTransferDetailsMessage() {
            if (enableLogging) {
                logMessage("ERROR: Unable to transfer student due to invalid transfer details xml" +
                        "\r\n" + "Student name: " + student.getNameView() +
                        "\r\n" + "Transfer Detail XML: " + student.getTransferDetails());
            }
        }

        public String getWithdrawalCode() {
            return withdrawalCode;
        }

        public String getWithdrawalReason() {
            return withdrawalReason;
        }

        public PlainDate getWithdrawalDate() {
            return withdrawalDate;
        }

        public String getEntryCode() {
            return entryCode;
        }

        public String getEntryReason() {
            return entryReason;
        }

        public PlainDate getEntryDate() {
            return entryDate;
        }

        public StudentEnrollment getStudentEnrollment() {
            return studentEnrollment;
        }
    }
}
