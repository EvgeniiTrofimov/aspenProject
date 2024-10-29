/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.common;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.SYS_EMAIL_REPLY_TO_ADDRESS;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.*;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.LocalResultHandler;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.Breakable;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisDocument;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports5.engine.JasperExportManager;
import net.sf.jasperreports5.engine.JasperPrint;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.pdfbox.util.PDFMergerUtility;

/**
 * A class which manages all functionality required to publish a ran report's results to users.
 * Core class has been extended in order to use a customized JasperEngineFiller
 *
 * This filler is passed in pre populated with reports so the format and data source are not passed
 * in or used.
 * This class would be used for concatenated reports (as one example) and so is called for one bean
 * at a time.
 *
 * The secondary school report for Ontario ix an example
 * It uses this class to combine landscape and portrait reports
 *
 * Private methods from the extended class have been copied.
 *
 * The new method which will be called to publish reports is publishReportEachBean
 *
 * If documents are being created modify calling report as follows:
 * Add in parameters:
 * - // Parameters - create documents
 * - public static final String PARM_CREATE_DOCUMENTS = "createDocuments";
 * - public static final String PARM_DOCUMENT_TYPE = "documentType";
 * - public static final String PARM_DOCUMENT_FORMAT = "documentFormat";
 * - public static final String PARM_EMAIL_RESULTS = "emailResults";
 * - // Parameters - to be added for creating documents
 * - public static final String PUBLISH_REPORT = "report";
 *
 * Add in save state:
 * - addParameter(PUBLISH_REPORT, userData.getToolInput().getTool());
 *
 * * Add in initialize:
 * - // check that only publish or create documents has been selected to avoid conflicts
 * - if ((getBooleanParameter(PARM_CREATE_DOCUMENTS)) &&
 * (getJob().getInput().getParameterValue(ReportDeliveryJob.COL_PUBLISHED_INDICATOR)
 * .equals(PublishType.PUBLISH.toString()))) {
 * - String errorMessage = "Either publish or create document should be selected for each run";
 * - this.addCustomErrorMessage("NOTE: " + errorMessage);
 * - throw new X2RuntimeException(AppGlobals.getMessagesBundle(getLocale().toString()),
 * CONST_EMPTY);
 * - }
 *
 * Add in condition for create instance of this class
 * - (getBooleanParameter("createDocuments")) ||
 * - Change set of stdOidPrev to null instead of empty
 *
 * Add in call to publishReportEachBeans
 * - (log returned message which will show if document created or error)
 *
 * @author Follett Software Company
 */
public class PublishReportsManagerMultyPerBeanJe5 extends PublishReportsManager {
    // Parameters - create documents
    public static final String PARM_CREATE_DOCUMENTS = "createDocuments";
    public static final String PARM_DOCUMENT_TYPE = "documentType";
    public static final String PARM_DOCUMENT_FORMAT = "documentFormat";
    public static final String PARM_EMAIL_RESULTS = "emailResults";

    // Parameters - to be added by calling report
    public static final String PUBLISH_REPORT = "report";

    // Aliases
    private static final String ALIAS_REPORT_OID = "report-oid";
    private static final String ALIAS_SCHOOL_ID = "school-id";
    private static final String ALIAS_UPLOAD_DATE = "upload-date";
    private static final String ALIAS_USER_OID = "user-oid";

    // Constants
    private static final String CONST_NEW_LINE = "\n";
    private static final String CONST_HYPHEN = "-";
    private static final String CONST_EXT_PDF = ".pdf";

    // Data Dictionary Fields
    private DataDictionaryField m_docNameField;
    private DataDictionaryField m_fileNameField;
    private DataDictionaryField m_reportOidField;
    private DataDictionaryField m_schoolIdField;
    private DataDictionaryField m_uploadDateField;
    private DataDictionaryField m_userOidField;

    // Variables
    private X2Broker m_broker;
    private Map m_parameters;
    private Organization m_organization;
    private Publishable m_publishReport;
    private School m_school;
    private ToolJob m_toolJob;
    private User m_user;
    private Map<String, User> m_usersByPerson;

    // Variables - create documents
    private boolean m_createDocuments;
    private DataDictionary m_dictionary;
    private FolderFile m_folderFile;
    private Report m_report;
    private String m_reportName;
    private String m_reportDate;
    private boolean m_sendEmail;

    /**
     * This is the default constructor without specific parameters to create documents
     *
     * @param toolJob
     * @param jasperFormat //not used so will be null
     * @param parameters
     * @param dataSource //not used so will be null
     * @param publishReport
     * @param school
     * @param organization
     * @param broker
     */
    public PublishReportsManagerMultyPerBeanJe5(ToolJob toolJob, InputStream jasperFormat, Map parameters,
                                                Breakable dataSource, Publishable publishReport, School school, Organization organization,
                                                X2Broker broker) {
        super(toolJob, null, parameters, dataSource, publishReport, school, organization, broker);

        // set needed variables which are private in extended clase
        m_broker = broker;
        m_parameters = parameters;
        m_organization = organization;
        m_publishReport = publishReport;
        m_school = school;
        m_toolJob = toolJob;
        m_user = toolJob.getUser();

        // Get lookup map for users who can receive the published reports
        getLookupMapForUsers();

        // check/process if create document
        processCreateDocuments();
    }

    /**
     * Get lookup map for users who can receive the published reports
     */
    public final void getLookupMapForUsers() {
        // Get lookup map for users who can receive the published reports
        PlainDate today = new PlainDate(OrganizationManager.getTimeZone(m_organization));

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(User.COL_ACCOUNT_EXPIRATION_DATE, m_broker.getPersistenceKey());

        Criteria criteria = new Criteria();
        criteria.addGreaterThan(User.COL_ACCOUNT_EXPIRATION_DATE, today);
        criteria.addOrCriteria(orCriteria);

        QueryByCriteria query = new QueryByCriteria(User.class, criteria);

        m_usersByPerson = m_broker.getMapByQuery(query, User.COL_PERSON_OID, 2000);
    }

    /**
     * Process for create documents if create documents parameter is set
     */
    public final void processCreateDocuments() {
        /*
         * Check to see if the "Create Documents" input parameter exists and then determine what to
         * do. If the
         * parameter does not exist, set m_createDocuments to false.
         */
        if (m_parameters.get(PARM_CREATE_DOCUMENTS) != null) {
            m_report = (Report) m_parameters.get(PUBLISH_REPORT);

            if (m_parameters.get(PARM_EMAIL_RESULTS) != null) {
                m_sendEmail = ((Boolean) m_parameters.get(PARM_EMAIL_RESULTS)).booleanValue();
            } else {
                m_sendEmail = false;
            }

            m_createDocuments = ((Boolean) m_parameters.get(PARM_CREATE_DOCUMENTS)).booleanValue();

            // set up create documents if selected
            if (m_createDocuments) {
                // Sets "Publish type" to "Preview" and "publishTo" to "1"
                m_toolJob.getInput().setParameterAsString(ReportDeliveryJob.COL_PUBLISHED_INDICATOR, PublishType.PREVIEW
                        .toString());
                m_toolJob.getInput().setParameterAsString("publishTo", Integer.toString(1));

                // Dictionary and fields
                m_dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
                m_docNameField = m_dictionary.findDataDictionaryField(Document.class.getName(), Document.COL_NAME);
                m_fileNameField = m_dictionary.findDataDictionaryField(Document.class.getName(), Document.COL_FILENAME);
                m_reportOidField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_REPORT_OID);
                m_schoolIdField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_ID);
                m_uploadDateField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_UPLOAD_DATE);
                m_userOidField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_USER_OID);

                // initialize variables
                m_reportName = m_report.getName();
                if (m_reportName.length() > m_docNameField.getLength()) {
                    m_reportName = m_reportName.substring(0, m_docNameField.getLength());
                }
                m_reportDate = (new PlainDate()).toString();
            }
        } else {
            m_createDocuments = false;
        }
    }

    /**
     * Handles all functionality required to publish a ran report's results to users.
     *
     * A new method has been written to publish the report for one bean only.
     * The reason is as the filler has been customized to combine multiple reports and this is
     * passed in.
     *
     * Adding reports to the filler is done in the calling class.
     *
     * See java doc for extended class for publishing and file logic
     *
     * @param filler - filler for report, already filled
     * @param toolBean
     *
     * @return String - results of publish
     */
    public String publishReportEachBean(JasperEngine5MultyReportFiller filler, ToolBean toolBean) {
        X2BaseBean x2Bean = null;
        if (toolBean != null) {
            x2Bean = toolBean.getX2Bean();
        }
        return publishReportEachBean(filler, x2Bean);
    }

    /**
     * Handles all functionality required to publish a ran report's results to users.
     *
     * A new method has been written to publish the report for one bean only.
     * The reason is as the filler has been customized to combine multiple reports and this is
     * passed in.
     *
     * Adding reports to the filler is done in the calling class.
     *
     * See java doc for extended class for publishing and file logic
     *
     * @param filler - filler for report, already filled
     * @param x2Bean - expected to be student
     *
     * @return String - results of publish
     */
    public String publishReportEachBean(JasperEngine5MultyReportFiller filler, X2BaseBean x2Bean) {
        StringBuilder result = new StringBuilder();
        UserToolLog userToolLog = m_toolJob.getJobLogManager().getUserToolLog();

        // Broker interactions and physical file creation is rolled back upon encountering any
        // errors
        boolean wasInTransaction = m_broker.isInTransaction();
        if (!wasInTransaction) {
            m_broker.beginTransaction();
        }

        ReportDeliveryJob reportDeliveryJob = createReportDeliveryJob();

        try {
            if (x2Bean == null) {
                return result.toString();
            }

            // Get recipients
            Collection<Person> recipients = m_publishReport.getEmailRecipients(x2Bean);
            if (recipients == null || recipients.isEmpty()) {
                return result.toString();
            }

            StringBuilder logMessageRecipients = new StringBuilder();
            for (Person recipient : recipients) {
                logMessageRecipients.append(recipient.getNameView() + "|");
            }

            // File is created and filled for the bean passed in
            try {
                m_folderFile = createFolderFile(reportDeliveryJob, x2Bean);
                ResultHandler handler = null;

                try {
                    // if create documents
                    if (m_createDocuments) {
                        handler = createResultHandler(m_folderFile);
                        handler.open(m_toolJob, null);

                        updateFolderFileInfo(m_folderFile);

                        // Create ReportDeliveryRecipient and FolderFile records
                        createReportDeliveryRecipients(recipients, reportDeliveryJob, m_folderFile);

                        // combine reports into one and create the rawValue for the document
                        List<JasperPrint> jasperPrints = filler.getSavedReports();
                        if ((jasperPrints != null) && (!jasperPrints.isEmpty())) {
                            Student student = (Student) x2Bean;
                            PDFMergerUtility mergePdf = new PDFMergerUtility();
                            ByteArrayOutputStream output = new ByteArrayOutputStream();
                            mergePdf.setDestinationStream(output);
                            for (JasperPrint jasperPrint : jasperPrints) {
                                byte[] rawValueJasperPrint = JasperExportManager.exportReportToPdf(jasperPrint);
                                mergePdf.addSource(new ByteArrayInputStream(rawValueJasperPrint));
                            }
                            mergePdf.mergeDocuments();
                            output = (ByteArrayOutputStream) mergePdf.getDestinationStream();
                            byte[] rawValue = output.toByteArray();

                            // call create document
                            result.append(createDoc(student, rawValue));
                        }
                    } else
                        // if publish
                    {
                        // combine reports into one and create the rawValue for the document
                        List<JasperPrint> jasperPrints = filler.getSavedReports();
                        if ((jasperPrints != null) && (!jasperPrints.isEmpty())) {
                            PDFMergerUtility mergePdf = new PDFMergerUtility();
                            ByteArrayOutputStream output = new ByteArrayOutputStream();
                            mergePdf.setDestinationStream(output);
                            for (JasperPrint jasperPrint : jasperPrints) {
                                byte[] rawValueJasperPrint = JasperExportManager.exportReportToPdf(jasperPrint);
                                mergePdf.addSource(new ByteArrayInputStream(rawValueJasperPrint));
                            }
                            mergePdf.mergeDocuments();
                            output = (ByteArrayOutputStream) mergePdf.getDestinationStream();
                            byte[] rawValue = output.toByteArray();

                            // create and save file from raw data
                            File file = FileAttachmentManager.createDestinationFile(m_organization,
                                    new ByteArrayInputStream(rawValue));
                            m_folderFile.setSize(rawValue.length);
                            m_folderFile.setBinaryFile(file);
                            m_broker.saveBeanForced(m_folderFile);
                        }

                        // Create ReportDeliveryRecipient and FolderFile records
                        createReportDeliveryRecipients(recipients, reportDeliveryJob, m_folderFile);
                    }
                } catch (Exception e) {
                    m_toolJob.getJobLogManager().fail(e);
                } finally {
                    if (handler != null) {
                        handler.close();
                    }
                }
            } catch (IOException ioe) {
                m_toolJob.getJobLogManager().fail(ioe);
            } catch (NullPointerException npe) {
                m_toolJob.getJobLogManager().fail(npe);
            }

            // Send out e-mails if publishing and there were no failures
            String publish = m_toolJob.getInput().getParameterValue(ReportDeliveryJob.COL_PUBLISHED_INDICATOR);

            if (PublishType.PUBLISH.toString().equals(publish) && userToolLog
                    .getEndTypeEnum() != UserToolLog.EndType.ERROR) {
                publishEmail(recipients, reportDeliveryJob);
            }
        } catch (

                BeanPathException bpe) {
            String msg = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(
                    "message.publishReports.errorDataBreak", reportDeliveryJob.getOid()) + "-" + bpe.getMessage();
            m_toolJob.getJobLogManager().fail(msg);
        } catch (IndexOutOfBoundsException ioobe) {
            String msg = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(
                    "message.publishReports.errorDataBreak", reportDeliveryJob.getOid()) + "-" + ioobe.getMessage();
            m_toolJob.getJobLogManager().fail(msg);
        } finally {
            // If there were errors, we must rollback saved data and delete reports created
            if (userToolLog.getEndTypeEnum() == UserToolLog.EndType.ERROR) {
                for (FolderFile file : m_toolJob.getResultHandler().getResultFiles()) {
                    FileAttachmentManager.deleteFromDisk(file);
                }

                if (!wasInTransaction) {
                    m_broker.rollbackTransaction();
                }

                m_toolJob.getResultHandler().clearResultFiles();
            } else if (!wasInTransaction) {
                m_broker.commitTransaction();
            }
        }

        return result.toString();
    }

    /**
     * Creates the ReportDeliveryJob.
     *
     * @return ReportDeliveryJob
     */
    private ReportDeliveryJob createReportDeliveryJob() {
        Map<String, Object> reportParameters = m_toolJob.getInput().generateInputMap();

        ReportDeliveryJob rdj = X2BaseBean.newInstance(ReportDeliveryJob.class, m_broker.getPersistenceKey());
        rdj.setCleanupDate((PlainDate) reportParameters.get(ReportDeliveryJob.COL_CLEANUP_DATE));
        rdj.setDate(new PlainDate(OrganizationManager.getTimeZone(m_organization)));
        rdj.setViewEndDate((PlainDate) reportParameters.get(ReportDeliveryJob.COL_VIEW_END_DATE));
        rdj.setPublishedIndicator(PublishType.PUBLISH.toString().equals(reportParameters.get(
                ReportDeliveryJob.COL_PUBLISHED_INDICATOR)));
        rdj.setPublishedMessage(getPublishedMessage((String) reportParameters.get(
                ReportDeliveryJob.COL_PUBLISHED_MESSAGE)));
        rdj.setPublishedSubject((String) reportParameters.get(ReportDeliveryJob.COL_PUBLISHED_SUBJECT));
        rdj.setReportOid(((Report) m_toolJob.getTool()).getOid());
        rdj.setSchoolOid((m_school != null) ? m_school.getOid() : null);
        rdj.setViewStartDate((PlainDate) reportParameters.get(ReportDeliveryJob.COL_VIEW_START_DATE));
        rdj.setTime(new PlainTime(OrganizationManager.getTimeZone(m_organization)));
        rdj.setUserOid(m_user.getOid());

        m_broker.saveBeanForced(rdj);

        return rdj;
    }

    /**
     * Creates a FolderFile given the result handler. The file is owned by the root organization.
     *
     * @param reportDeliveryJob ReportDeliveryJob
     * @param bean X2BaseBean
     * @return FolderFile
     */
    private FolderFile createFolderFile(ReportDeliveryJob reportDeliveryJob, X2BaseBean bean) {
        FolderFile folderFile = X2BaseBean.newInstance(FolderFile.class, m_broker.getPersistenceKey());

        folderFile.setCreationDate(new PlainDate());
        folderFile.setExpireDate(reportDeliveryJob.getViewEndDate());
        folderFile.setFileExtension(ResultHandler.getFileExtention(m_toolJob.getInput().getFormat()));
        folderFile.setName(reportDeliveryJob.getReport().getName());
        folderFile.setOwnerOid(m_organization.getOid());
        folderFile.setOwnerType(Ownable.OWNER_TYPE_ORG1);
        folderFile.setPublicIndicator(false);
        folderFile.setUploadTimestamp(System.currentTimeMillis());
        folderFile.setUserCreatorOid(m_user.getOid());

        OrganizationManager.setOrganizationOids(folderFile, m_organization);

        try {
            String description = m_publishReport.getDescription(bean);
            if (description != null) {
                folderFile.setDescription(description);
            }
        } catch (BeanPathException bpe) {
            String msg = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(
                    "message.publishReports.errorDataBreak", reportDeliveryJob.getOid()) + "-" + bpe.getMessage();
            m_toolJob.getJobLogManager().fail(msg);
        }

        m_broker.saveBeanForced(folderFile);

        // Outer toolJob tracks all files created
        m_toolJob.getResultHandler().addResultFile(folderFile);

        return folderFile;
    }

    /**
     * Creates a result handler for the given directory and file.
     *
     * @param folderFile FolderFile
     * @return ResultHandler
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private ResultHandler createResultHandler(FolderFile folderFile) throws IOException {
        File destinationDirectory = new File(AppGlobals.getBinaryDataDirectoryPath(m_broker.getPersistenceKey())
                + File.separator + FileAttachmentManager.getSubFolderPathForFile(m_organization));
        FileAttachmentManager.createDestinationDirectory(destinationDirectory);
        File file = FileAttachmentManager.getFileFromDisk(folderFile);

        ResultHandler handler = new LocalResultHandler(destinationDirectory, file.getName(), -1, m_toolJob.getTool());

        return handler;
    }

    /**
     * Updates information into the folder file which was not available when the folderFile was
     * created.
     *
     * @param folderFile FolderFile
     */
    private void updateFolderFileInfo(FolderFile folderFile) {
        File file = FileAttachmentManager.getFileFromDisk(folderFile);

        folderFile.setFilename(file.getName());
        folderFile.setSize(file.length());

        m_broker.saveBeanForced(folderFile);
    }

    /**
     * Find the users with the email addresses passed in and create ReportDeliveryRecipient records
     * identifying who receives an email for the passed file.
     *
     * @param recipients Collection<Person>
     * @param reportDeliveryJob ReportDeliveryJob
     * @param folderFile FolderFile
     */
    private void createReportDeliveryRecipients(Collection<Person> recipients,
                                                ReportDeliveryJob reportDeliveryJob,
                                                FolderFile folderFile) {
        for (Person person : recipients) {
            ReportDeliveryRecipient rdr = X2BaseBean.newInstance(ReportDeliveryRecipient.class, m_broker
                    .getPersistenceKey());
            rdr.setFolderFileOid(folderFile.getOid());
            rdr.setReportDeliveryJobOid(reportDeliveryJob.getOid());

            User user = m_usersByPerson.get(person.getOid());
            if (user != null) {
                rdr.setUserOid(user.getOid());
            }

            m_broker.saveBeanForced(rdr);
        }
    }

    /**
     * Generates a PERSON_DOCUMENT for the passed student and path. The path will point to a newly
     * created PDF in the
     * user's temp folder.
     *
     * @param student
     * @param rawValue - concatenated with all reports for student
     *
     * @return String - results of create document
     *
     * @throws IOException
     */
    public String createDoc(Student student, byte[] rawValue) throws IOException {
        StringBuilder result = new StringBuilder();

        try {
            // create file from raw data
            File file = FileAttachmentManager.createDestinationFile(m_organization, new ByteArrayInputStream(rawValue));

            // get file name
            String stateId = (student.getStateId() != null) ? student.getStateId() + "_" : "";
            String schoolYear = Integer.valueOf(m_organization.getCurrentContext().getSchoolYear()).toString();
            schoolYear = (schoolYear != null) ? schoolYear + "_" : "";
            String gradeLevel = (student.getGradeLevel() != null) ? student.getGradeLevel() + "_" : "";
            String schoolId = m_school.getSchoolId() + "_";

            String fileName = schoolId + stateId + schoolYear + gradeLevel + m_reportName;
            // remove accents in order to save document from report name
            fileName = StringUtils.stripAccents(fileName);
            // remove single/double quotes in order to save document from report name
            fileName = fileName.replaceAll("\\u201d|\\u201c|\\u2018|\\u2019|\\u0022|\\u0027", "");

            int fieldLength = m_fileNameField.getLength();
            int fileSuffixLength = CONST_HYPHEN.length() + m_reportDate.length() + CONST_EXT_PDF.length();
            int fileNameLength = fileName.length() + fileSuffixLength;
            // Make sure the file name isn't too long
            if (fileNameLength > fieldLength) {
                fileName = (fileName.substring(0, Math.min(fileNameLength, fieldLength - 4)));
            }

            fileName = fileName + "_" + m_reportDate + CONST_EXT_PDF;

            SisDocument document = X2BaseBean.newInstance(SisDocument.class, m_broker.getPersistenceKey());
            document.setPersonOid(student.getPersonOid());
            document.setName(m_reportName);
            document.setFilename(fileName);
            document.setTypeCode((String) m_parameters.get(PARM_DOCUMENT_TYPE));
            document.setFormatCode((String) m_parameters.get(PARM_DOCUMENT_FORMAT));
            document.setDocument(rawValue);
            document.setBinaryFile(file);

            /*
             * Upload Date user defined field
             */
            if (m_uploadDateField != null) {
                document.setFieldValueByBeanPath(m_uploadDateField.getJavaName(), m_reportDate);
            }

            /*
             * Report OID user defined field
             */
            if (m_reportOidField != null) {
                document.setFieldValueByBeanPath(m_reportOidField.getJavaName(), m_report.getOid());
            }

            /*
             * User OID user defined field
             */
            if (m_userOidField != null) {
                document.setFieldValueByBeanPath(m_userOidField.getJavaName(), m_user.getOid());
            }

            /*
             * School ID user defined field
             */
            if (m_schoolIdField != null) {
                document.setFieldValueByBeanPath(m_schoolIdField.getJavaName(), student.getSchool().getSchoolId());
            }

            // save document and check if any error
            result.append(saveBeanSafely("Document save error for student: " + student.getNameView(), document));
            // result will be empty if no error
            if (result.toString().isEmpty()) {
                /*
                 * Send an email to student with student's email 01.
                 */
                if (m_sendEmail) {
                    String bodyStudent = "Document for " + m_report.getName()
                    + " report added to your documents folder";
                    String subjectStudent = "Created Document - " + m_report.getName();

                    sendEmail(student.getPerson().getEmail01(), null, bodyStudent, subjectStudent);
                }

                result.append(student.getNameView() + " - Document Created" + CONST_NEW_LINE);
            }
        } catch (Exception e) {
            AppGlobals.getLog().severe(e.getMessage());
        }

        return result.toString();
    }

    /**
     * Returns the formatted message.
     *
     * @param message String
     * @return String
     */
    private String getPublishedMessage(String message) {
        if (StringUtils.isBlank(message)) {
            message = "{user}{organization}{school}";
        }
        try {
            message = message.replace("{user}",
                    m_user != null && m_user.getNameView() != null ? m_user.getNameView() : "Support, User");
            message = message.replace("{organization}", m_organization.getName());
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.INFO, "Message ==> " + message);
        }

        String school = "";
        if (m_parameters.get("school") != null) {
            school = ((School) m_parameters.get("school")).getName();
        }

        message = message.replace("{school}", school);

        Map<String, Object> reportParameters = m_toolJob.getInput().generateInputMap();
        for (String id : reportParameters.keySet()) {
            Object reportParam = reportParameters.get(id);

            if (reportParam instanceof PlainDate) {
                SimpleDateFormat dateFormatter = new SimpleDateFormat("MM/dd/yyyy");

                message = message.replace("{" + id + "}", dateFormatter.format(reportParam));
            } else if (reportParam != null) {
                message = message.replace("{" + id + "}", reportParam.toString());
            } else {
                message = message.replace("{" + id + "}", "");
            }
        }

        return message;
    }

    /**
     * Sends notification to specified email addresses about the report which has been published.
     *
     * @param recipients Collection<Person>
     * @param reportDeliveryJob ReportDeliveryJob
     */
    private void publishEmail(Collection<Person> recipients, ReportDeliveryJob reportDeliveryJob) {
        if (!recipients.isEmpty()) {
            // Gather the email addresses, filtering duplicates
            List<String> validatedRecipients = new ArrayList<String>();
            for (Person recipient : recipients) {
                String email = m_publishReport.getEmailAddress(recipient);

                if (!StringUtils.isEmpty(email) && !validatedRecipients.contains(email)) {
                    validatedRecipients.add(email);
                }
            }

            WriteEmailManager manager = new WriteEmailManager(m_organization, m_user);
            if (manager.connect()) {
                try {
                    manager.sendMassEmail(validatedRecipients, m_toolJob.getInput().getParameterValue(
                            ReportDeliveryJob.COL_PUBLISHED_SUBJECT),
                            getPublishedMessage(m_toolJob.getInput()
                                    .getParameterValue(ReportDeliveryJob.COL_PUBLISHED_MESSAGE)),
                            null);
                } finally {
                    manager.disconnect();
                }
            } else {
                String msg = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(
                        "message.publishReports.errorEmail", reportDeliveryJob.getOid());
                m_toolJob.getJobLogManager().fail(msg);
            }
        } else {
            String msg = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(
                    "message.publishReports.errorNoEmails", reportDeliveryJob.getOid());
            m_toolJob.getJobLogManager().fail(msg);
        }
    }

    /**
     * Sends an email to the email address passed.
     *
     * @param emailAddress
     * @param ccEmail
     * @param messageBody
     * @param messageSubject
     */
    private void sendEmail(String emailAddress, String ccEmail, String messageBody, String messageSubject) {
        String sender = PreferenceManager.getPreferenceValue(m_organization, SYS_EMAIL_REPLY_TO_ADDRESS);
        if (emailAddress != null && EmailManager.validateEmailAddress(emailAddress)) {
            WriteEmailManager emailManager = new WriteEmailManager(m_organization, m_school.getOid(),
                    Ownable.OWNER_TYPE_SCHOOL, m_user);
            if (emailManager.connect()) {
                try {
                    List<String> emailList = new ArrayList<String>();
                    emailList.add(emailAddress);
                    MessageProperties message = new MessageProperties(emailList, null, null, sender, messageSubject,
                            messageBody, null, null, null, null);

                    emailManager.sendMail(message);
                } finally {
                    emailManager.disconnect();
                }
            }
        }
    }

    /**
     * Checks the result of a saveBean call for errors, throws exception and logs if there are
     * errors.
     *
     * @param errorMsg - message to display in the event of an error saving the bean
     * @param bean - the bean to save
     *
     * @return String errorMsg - An error message - populated if there was an error
     */
    protected String saveBeanSafely(String errorMsg, X2BaseBean bean) {
        StringBuilder successSaveBean = new StringBuilder();
        if (bean.isDirty()) {
            Collection<ValidationError> valErrors = m_broker.saveBean(bean);
            if (valErrors.size() > 0) {
                StringBuffer errorCause = new StringBuffer();
                errorCause.append("Save bean error: " + bean.getClass().getName() + " | Error Message: " + errorMsg);

                for (ValidationError valError : valErrors) {
                    errorCause.append(valError.toString());

                    errorCause.append(" | Val error: " + valError.toString());
                }

                successSaveBean.append(errorCause);
                successSaveBean.append(CONST_NEW_LINE);
            }
        }

        return successSaveBean.toString();
    }
}
