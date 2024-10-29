/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap;
import com.x2dev.procedures.statereporting.on.OnsisOenAssignmentImport.OenDetail.OenDetailField;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultHeaderHelper.OnsisResultHeader;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class OnsisOenAssignmentImport extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private enum OenDataType {
        A, E, M;
    }

    static class OenDetail {
        enum OenDetailField {
            ASPEN_MESSAGES,
            //
            BIRTH_DATE,
            //
            BLN_OEN_ASSIGN,
            //
            BSID_SCHOOL_NUMBER,
            //
            DATA_ERROR_DETAILS,
            //
            LEGAL_SURNAME,
            //
            ELEMENT_STUDENT_REF,
            //
            LEGAL_FIRST_NAME,
            //
            LEGAL_SECOND_NAME,
            //
            GENDER,
            //
            GENDER_ENGLISH_DESC,
            //
            GENDER_FRENCH_DESC,
            //
            OEN_NUMBER,
            //
            SOURCE_DOCUMENT,
            //
            SOURCE_DOC_ENGLISH_DESC,
            //
            SOURCE_DOC_FRENCH_DESC,
            //
            STUDENT_REF;
        }

        private Student m_student;
        private Map<OenDetailField, String> m_valuesByFields = new HashMap<>();

        static OenDetail parseOen(Element element) {
            OenDetail oenDetail = new OenDetail();
            for (OenDetailField field : OenDetailField.values()) {
                String value = OnsisResultsHelper.getValueByElementName(element, field.toString());
                oenDetail.m_valuesByFields.put(field, value);
            }
            return oenDetail;
        }

        String get(OenDetailField field) {
            return m_valuesByFields.get(field);
        }

        void addAspenMessage(String message) {
            StringBuilder messagesBuilder = new StringBuilder();
            String currentMessages = m_valuesByFields.get(OenDetailField.ASPEN_MESSAGES);
            if (currentMessages != null) {
                messagesBuilder.append(currentMessages);
                messagesBuilder.append("\n");
            }

            messagesBuilder.append(message);
            m_valuesByFields.put(OenDetailField.ASPEN_MESSAGES, messagesBuilder.toString());
        }

        void setStudent(Student student) {
            m_student = student;
        }
    }

    private class OenAssignment {
        private static final String ALIAS_ACTION_TYPE = "oen-action-type";
        private static final String ALIAS_ASPEN_MESSAGE = "aspen-message";
        private static final String ALIAS_CREATION_DATE = "creation-date";
        private static final String ALIAS_OEN = "oen";
        private static final String ALIAS_STD_NAME = "student-name";

        private final UserDefinedTableB m_bean;
        private final OenDetail m_oenDetail;

        /**
         * @param result
         * @param oenDetail
         */
        public OenAssignment(OenDetail oenDetail) {
            m_oenDetail = oenDetail;
            m_bean = new UserDefinedTableB(getBroker().getPersistenceKey());
            m_bean.setExtendedDataDictionaryOid(getDictionary(DDX_ID).getExtendedDictionaryOid());
            m_bean.setOrganization1Oid(getOrganization().getOid());
            setFieldValueByAlias(ALIAS_CREATION_DATE, OnsisResultHeader.getCurrentDate());
            setFieldValueByAlias(ALIAS_ACTION_TYPE, m_oenDetail.get(OenDetailField.BLN_OEN_ASSIGN));
            String oen = m_oenDetail.get(OenDetailField.OEN_NUMBER);
            setFieldValueByAlias(ALIAS_OEN, oen);
            setFieldValueByAlias(ALIAS_ASPEN_MESSAGE, m_oenDetail.get(OenDetailField.ASPEN_MESSAGES));
            setFieldValueByAlias(ALIAS_STD_NAME, getStudentLegalName(m_oenDetail));
            m_bean.setStudentOid(getStudentOid());
            m_bean.setUserDefinedTableCOid(m_headerOid);
        }

        public void save() {
            getBroker().saveBean(m_bean);
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisResultHeaderHelper.OnsisImportedRecord#getStudentOid()
         */
        private String getStudentOid() {
            Student student = m_oenDetail.m_student;
            return student == null ? null : student.getOid();
        }

        private void setFieldValueByAlias(String alias, Object value) {
            m_bean.setFieldValueByAlias(alias, value, getDictionary(DDX_ID));
        }
    }

    public static final String DDX_ID = "ON-SIS-OEN-ASMNT";

    private static final String ELEMENT_NAME_OEN_DETAILS = "OEN_DETAILS";

    private static final String ENCODING_UTF_8 = "UTF-8";

    private static final String PARAM_CHARSET = "charset";
    private static final String PARAM_IS_REVIEW = "isReview";

    private Filterable<OenDetail> m_detailsHelper;
    private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
    private DictionaryExtractor m_dictionaryExtractor;
    private String m_headerOid;
    private List<String> m_messages = new ArrayList<>();
    private List<OenDetail> m_oenDetails = new ArrayList<>();

    /**
     * Gets the dictionary.
     *
     * @param ddxId String
     * @return Data dictionary
     */
    public DataDictionary getDictionary(String ddxId) {
        DataDictionary dictionary = m_dictionariesById.get(ddxId);
        if (dictionary == null) {
            if (ddxId == null) {
                dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                m_dictionariesById.put(null, dictionary);
            } else {
                ExtendedDictionaryAttributes extDictAttributes = null;

                X2Criteria ddxCriteria = new X2Criteria();
                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                ExtendedDataDictionary ddx = getBroker().getBeanByQuery(ddxQuery);
                if (ddx == null) {
                    X2Criteria asdCriteria = new X2Criteria();
                    asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ddxId);
                    QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCriteria);
                    AssessmentDefinition asd = getBroker().getBeanByQuery(asdQuery);
                    if (asd == null) {
                        throw new X2RuntimeException();
                    }
                    extDictAttributes = asd;
                } else {
                    extDictAttributes = ddx;
                }
                dictionary =
                        DataDictionary.getDistrictDictionary(extDictAttributes, getBroker().getPersistenceKey());
                m_dictionariesById.put(ddxId, dictionary);
            }
        }
        return dictionary;
    }

    /**
     * Public for unit tests
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getResultHandler()
     */
    @Override
    public ResultHandler getResultHandler() {
        return super.getResultHandler();
    }

    /**
     * Public for unit tests
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#setOrganization(com.follett.fsc.core.k12.beans.Organization)
     */
    @Override
    public void setOrganization(Organization organization) {
        super.setOrganization(organization);
    }

    /**
     * Export the ONSIS XML batch file
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder exportString = new StringBuilder();
        if (m_messages.isEmpty()) {
            if (isReview()) {
                exportString.append("Review\n");
            } else {
                exportString.append("Commit\n");
            }
            exportString.append("\n");

            List<String> schoolNumbers = getSchoolNumbers(getDetailsHelper());
            for (String schoolNumber : schoolNumbers) {
                Filterable<OenDetail> schoolRecordsHelper =
                        getDetailsHelper().filter(OenDetailField.BSID_SCHOOL_NUMBER.toString(), schoolNumber);

                List<OenDetail> recordsA =
                        new ArrayList<>(schoolRecordsHelper.filter(OenDetailField.BLN_OEN_ASSIGN.toString(),
                                OenDataType.A.toString()).extract());
                sortByName(recordsA);
                exportString.append("New OEN(s) assigned (" + recordsA.size() + "):\n");
                addStudentsToOutput(exportString, recordsA);

                List<OenDetail> recordsM =
                        new ArrayList<>(schoolRecordsHelper.filter(OenDetailField.BLN_OEN_ASSIGN.toString(),
                                OenDataType.M.toString()).extract());
                sortByName(recordsM);
                exportString.append("Match(es) found (" + recordsM.size() + "):\n");
                addStudentsToOutput(exportString, recordsM);

                List<OenDetail> recordsE =
                        new ArrayList<>(schoolRecordsHelper.filter(OenDetailField.BLN_OEN_ASSIGN.toString(),
                                OenDataType.E.toString()).extract());
                sortByName(recordsE);
                exportString.append("Record(s) containing invalid data (" + recordsE.size() + "):\n");
                addStudentsToOutput(exportString, recordsE);

                Filterable<OenDetail> invalidRecordsHelper =
                        schoolRecordsHelper.filter(new Filter<OenDetail>() {
                            @Override
                            public boolean isFiltered(OenDetail toFilter) {
                                return !StringUtils.isEmpty(toFilter.get(OenDetailField.ASPEN_MESSAGES));
                            }
                        });

                List<OenDetail> withErrors = new ArrayList<>(invalidRecordsHelper.extract());
                sortByName(withErrors);
                exportString.append("Record(s) with aspen messages (" + withErrors.size() + "):\n");
                addStudentsWithErrorsToResult(exportString, withErrors);
                exportString.append("\n");
                exportString.append("\n");
                exportString.append("\n");
                exportString.append("\n");
            }
        } else {
            for (String message : m_messages) {
                exportString.append("Error: " + message);
                exportString.append("\n");
            }
        }

        try {
            ByteArrayInputStream inputStream =
                    new ByteArrayInputStream(exportString.toString().getBytes(ENCODING_UTF_8));
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Import Onsis Student OEN response file
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        String charsetName = (String) getParameter(PARAM_CHARSET);
        if (StringUtils.isEmpty(charsetName)) {
            charsetName = ENCODING_UTF_8;
        }

        try (BufferedReader reader = new BufferedReader(new FileReader(sourceFile))) {
            StringBuilder sourceFileStringBuilder = new StringBuilder();
            String line = null;
            boolean firstLine = true;
            while ((line = reader.readLine()) != null) {
                // skip past Byte Order Mark
                if (firstLine) {
                    int startPos = line.indexOf('<');
                    if (startPos >= 0) {
                        line = line.substring(startPos);
                    }
                    firstLine = false;
                }
                sourceFileStringBuilder.append(line);
            }
            List<Node> oenDetails =
                    OnsisResultsHelper.getNodesByName(sourceFileStringBuilder.toString(), ELEMENT_NAME_OEN_DETAILS);
            processOenElements(oenDetails);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_dictionaryExtractor = new DictionaryExtractor(getBroker());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        UserDefinedTableC headerRec = userData.getCurrentRecord(UserDefinedTableC.class);
        if (headerRec == null) {
            throw new RuntimeException("OEN Assigment Import must be run from existing OEN Assigment Export header");
        }
        m_headerOid = headerRec.getOid();
    }

    private void addAspenMessage(OenDetail detail, String message) {
        detail.addAspenMessage(message);
        m_oenDetails.add(detail);
    }

    private void addStudentsToOutput(StringBuilder resultString, Collection<OenDetail> students) {
        for (OenDetail student : students) {
            resultString.append(getStudentRecord(student));
            resultString.append("\n");
        }
        resultString.append("\n");
    }

    private void addStudentsWithErrorsToResult(StringBuilder resultString, Collection<OenDetail> students) {
        for (OenDetail student : students) {
            resultString.append("\n");
            resultString.append(getStudentWithErrorRecord(student));
        }
    }

    private String buildRow(String... blocks) {
        StringBuilder row = new StringBuilder();
        for (int i = 0; i < blocks.length; i++) {
            String block = blocks[i];
            if (!StringUtils.isEmpty(block)) {
                row.append(block);
                if (blocks.length - 1 != i) {
                    row.append("; ");
                }
            }
        }
        return row.toString();
    }

    private Filterable<OenDetail> getDetailsHelper() {
        if (m_detailsHelper == null) {
            m_detailsHelper = FilterableFactory.create(m_oenDetails,
                    Arrays.<String>asList(OenDetailField.LEGAL_FIRST_NAME.toString(),
                            OenDetailField.LEGAL_SURNAME.toString(), OenDetailField.BIRTH_DATE.toString()),
                    new MultiLevelMap.ValueByKeyResolver<OenDetail>() {
                @Override
                public Object getValue(String key, OenDetail entity) {
                    return entity.get(OenDetailField.valueOf(key));
                }
            });
        }
        return m_detailsHelper;
    }


    private String getMessageBlock(String label, String value) {
        return StringUtils.isEmpty(value) ? "" : "[" + label + "]: " + value;
    }

    private List<String> getSchoolNumbers(Filterable<OenDetail> detailsHelper) {
        Set<String> schoolNumbersSet = new HashSet(detailsHelper.extract(OenDetailField.BSID_SCHOOL_NUMBER.toString()));
        List<String> schoolNumbers = new ArrayList<>(schoolNumbersSet);
        sortBySchoolNumber(schoolNumbers);
        return schoolNumbers;
    }

    private String getStudentRecord(OenDetail oenDetail) {
        String oen =
                getMessageBlock("OEN", oenDetail.get(OenDetailField.OEN_NUMBER));
        String name = getMessageBlock("Legal Name",
                oenDetail.get(OenDetailField.LEGAL_SURNAME) + ", " + oenDetail.get(OenDetailField.LEGAL_FIRST_NAME));
        String dob = getMessageBlock("Date of Birth", oenDetail.get(OenDetailField.BIRTH_DATE));
        String gender = getMessageBlock("Gender", oenDetail.get(OenDetailField.GENDER));

        return buildRow(oen, name, dob, gender);
    }

    private String getStudentWithErrorRecord(OenDetail oenDetail) {
        String error = getMessageBlock("Aspen error messages", oenDetail.get(OenDetailField.ASPEN_MESSAGES));
        return buildRow(getStudentRecord(oenDetail), error);
    }

    private String getStudentLegalName(OenDetail student) {
        String firstName = student.get(OenDetailField.LEGAL_FIRST_NAME);
        String lastName = student.get(OenDetailField.LEGAL_SURNAME);

        return lastName + ", " + firstName;
    }

    private boolean isReview() {
        Boolean isReviewFlag = (Boolean) getParameter(PARAM_IS_REVIEW);
        return isReviewFlag != null && isReviewFlag;
    }

    /**
     *
     * @param oenDetails
     */
    private void processOenElements(List<Node> oenDetails) {
        /*
         * Iterate and save each Student's OEN
         */
        getBroker().beginTransaction();
        boolean isError = false;
        String errorMessage = null;
        try {
            for (Node oenDetail : oenDetails) {
                processStudentOen((Element) oenDetail, getBroker());
            }
            removePreviousRecords();
            m_oenDetails.stream().map(currentOenDetails -> new OenAssignment(currentOenDetails))
            .forEach(oenAssignment -> oenAssignment.save());
        } catch (Exception e) {
            isError = true;
            errorMessage = LoggerUtils.convertThrowableToString(e);
            m_messages.add(errorMessage);
        } finally {
            if (isError || isReview()) {
                getBroker().rollbackTransaction();

                removePreviousExceptions();

                // OnsisException exception =
                // new OnsisResultsHelper.OnsisException(getBroker(), getOrganization(),
                // errorMessage,
                // m_dictionaryExtractor);
                // exception.getBean().setUserDefinedTableCOid(m_headerOid);
                // getBroker().saveBean(exception.getBean());
            } else {
                getBroker().commitTransaction();
            }
        }
    }

    /**
     * Given a <OEN_DETAILS> element, locate and update the matching Student bean
     *
     * @param oenDetailNode
     * @param broker
     */
    private void processStudentOen(Element oenDetailNode, X2Broker broker) {
        OenDetail oenDetail = OenDetail.parseOen(oenDetailNode);

        String oen = oenDetail.get(OenDetailField.OEN_NUMBER);

        Student student = null;

        String studentOid = oenDetail.get(OenDetailField.STUDENT_REF);
        if (!StringUtils.isEmpty(studentOid)) {
            student = (Student) broker.getBeanByOid(Student.class, studentOid);
        }

        if (student == null) {
            addAspenMessage(oenDetail, "Student record not found in Aspen");
            return;
        }

        oenDetail.setStudent(student);

        if (StringUtils.isEmpty(oen)) {
            String errorMessage = null;
            if (OenDataType.M.toString().equals(oenDetail.get(OenDetailField.BLN_OEN_ASSIGN))) {
                errorMessage =
                        "Match Found: Review matches on OEN site to determine valid OEN for student. "
                                + "If existing OEN found, copy and paste the OEN into the OEN field on the Student demographics tab in Aspen. "
                                + "If no valid OEN match found, assign new OEN via OEN Site.";
            } else {
                errorMessage = "OEN not found in the record";
            }
            addAspenMessage(oenDetail, errorMessage);
            return;
        }

        String oenBeanPath = OnStudent.FIELD_OEN.resolve(m_dictionaryExtractor);

        String oldOen = (String) student.getFieldValueByBeanPath(oenBeanPath);

        if (!oen.equals(oldOen) && !StringUtils.isEmpty(oldOen)) {
            addAspenMessage(oenDetail, "Student record already contains different OEN");
            return;
        }

        if (OenDataType.A.toString().equals(oenDetail.get(OenDetailField.BLN_OEN_ASSIGN))) {
            student.setFieldValueByBeanPath(oenBeanPath, oen);
            getBroker().saveBeanForced(student);
        }

        m_oenDetails.add(oenDetail);
    }

    private void removePreviousExceptions() {
        X2Criteria previousExceptionsCriteria = new X2Criteria();
        previousExceptionsCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, m_headerOid);
        previousExceptionsCriteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID, m_dictionaryExtractor
                .getDictionary(OnsisResultsHelper.OnsisException.DDX_ID).getExtendedDictionaryOid());
        getBroker().deleteByQuery(new QueryByCriteria(UserDefinedTableB.class, previousExceptionsCriteria));
    }

    private void removePreviousRecords() {
        X2Criteria headerRecordsCriteria = new X2Criteria();
        headerRecordsCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, m_headerOid);

        getBroker().deleteByQuery(new QueryByCriteria(UserDefinedTableB.class, headerRecordsCriteria));
    }

    private void sortByName(List<OenDetail> details) {
        Collections.sort(details, new Comparator<OenDetail>() {
            @Override
            public int compare(OenDetail o1, OenDetail o2) {
                return getStudentLegalName(o1).compareTo(getStudentLegalName(o2));
            }
        });
    }

    private void sortBySchoolNumber(List<String> details) {
        Collections.sort(details, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                String schoolNum1 = o1 == null ? "" : o1;
                String schoolNum2 = o2 == null ? "" : o2;
                return schoolNum1.compareTo(schoolNum2);
            }
        });
    }
}
