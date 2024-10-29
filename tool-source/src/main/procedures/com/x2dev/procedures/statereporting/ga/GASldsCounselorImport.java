/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Import class for SLDS Councelor's companion student data.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class GASldsCounselorImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Enum Header.
     */
    private enum Header {
        FILE_ID(6),
        // Fiscal Year
        FISCAL_YEAR(4),
        // System Code
        SYSTEM_CODE(3),
        // School Code
        SCHOOL_CODE(4),
        // GTID
        GTID(10),
        // Student Last Name
        STUDENT_LAST_NAME(30),
        // Student First Name
        STUDENT_FIRST_NAME(30),
        // Student Middle Name
        STUDENT_MIDDLE_NAME(30),
        // Career Aptitude Inventory 8th Grade
        CAREER_APTITUDE_8(1),
        // Career Interest & Aptitude Inventory 10th Grade
        CAREER_APTITUDE_10(1),
        // Career Interest Inventories
        CAREER_INTEREST(2),
        // Individual Graduation Plan
        INDIVIDUAL_GRADUATION_PLAN(1);

        private final int value;

        /**
         * Instantiates a new Header.
         *
         * @param value int
         */
        Header(int value) {
            this.value = value;
        }

        /**
         * Get value of Header instance.
         *
         * @return int value
         */
        public int getValue() {
            return value;
        }
    }

    /**
     * The Class Record.
     */
    private class Record {
        private final String m_careerAptitude8;
        private final String m_careerAptitude10;
        private final String m_careerInterest;
        private final String m_fiscalYear;
        private final String m_GTID;
        private final String m_individualGraduationPlan;
        private final int m_lineNumber;
        private final String m_schoolCode;

        /**
         * Instantiates a new record.
         *
         * @param lineNumber int
         * @param record List<String>
         */
        public Record(int lineNumber, List<String> record) {
            m_lineNumber = lineNumber;
            m_fiscalYear = record.get(Header.FISCAL_YEAR.ordinal());
            m_schoolCode = record.get(Header.SCHOOL_CODE.ordinal());
            m_GTID = record.get(Header.GTID.ordinal());
            m_careerAptitude8 = record.get(Header.CAREER_APTITUDE_8.ordinal());
            m_careerAptitude10 = record.get(Header.CAREER_APTITUDE_10.ordinal());
            m_careerInterest = record.get(Header.CAREER_INTEREST.ordinal());
            m_individualGraduationPlan = record.get(Header.INDIVIDUAL_GRADUATION_PLAN.ordinal());
        }

        /**
         * Record processing code.
         *
         * @return -1 for error, 0 for update and 1 for insert
         */
        public int applyChanges() {
            int result = ACTION_SKIP;
            String studentOid = findStudent();
            if (studentOid != null) {
                String schoolOid = findSchool();
                if (schoolOid != null) {
                    X2BaseBean bean = currentRecord(studentOid, schoolOid);
                    if (bean == null) {
                        bean = X2BaseBean.newInstance(m_beanClass, getBroker().getPersistenceKey());
                        bean.setFieldValueByBeanPath(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID,
                                m_extendedDataDictionary.getExtendedDictionary().getOid());
                        // etc
                        result = ACTION_INSERT;
                    } else {
                        result = ACTION_UPDATE;
                    }
                    updateFields(bean, studentOid, schoolOid, result == 1);
                    if (!bean.isDirty() || !saveBean(bean)) {
                        result = ACTION_SKIP;
                    }
                } else {
                    addError("Record skipped - School Code not found");
                }
            } else {
                addError("Record skipped - GTID not found");
            }
            return result;
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            boolean errorsFound = false;
            if (!PATTERN_FISCAL_YEAR.matcher(m_fiscalYear).matches()) {
                errorsFound = addError("Fiscal Year should be 4-digits number");
            }
            return !errorsFound;
        }

        /**
         * Adds the bean validation errors.
         *
         * @param errors List<ValidationError>
         */
        private void addBeanValidationErrors(List<ValidationError> errors) {
            for (ValidationError error : errors) {
                addError("Validation Error-" + error.toString());
            }
        }

        /**
         * Adds the error.
         *
         * @param errorMessage String
         * @return true, if successful
         */
        private boolean addError(String errorMessage) {
            m_errors.add(m_lineNumber + ". " + errorMessage);
            return true;
        }

        /**
         * Current record.
         *
         * @param studentOid String
         * @param schoolOid String
         * @return X2BaseBean
         */
        private X2BaseBean currentRecord(String studentOid, String schoolOid) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID,
                    m_extendedDataDictionary.getExtendedDictionary().getOid());
            criteria.addEqualTo(UserDefinedTableA.COL_STUDENT_OID, studentOid);
            criteria.addEqualTo(UserDefinedTableA.COL_SCHOOL_OID, schoolOid);
            criteria.addEqualTo(m_dataFieldsJavaNames.get(ALIAS_FISCAL_YEAR), m_fiscalYear);
            QueryByCriteria query = new QueryByCriteria(m_beanClass, criteria);
            return getBroker().getBeanByQuery(query);
        }

        /**
         * Find school.
         *
         * @return String
         */
        private String findSchool() {
            if (!m_schoolCodeToOidMap.containsKey(m_schoolCode)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisSchool.COL_SCHOOL_ID, m_schoolCode);
                QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
                SisSchool school = getBroker().getBeanByQuery(query);
                m_schoolCodeToOidMap.put(m_schoolCode, school != null ? school.getOid() : null);
            }
            return m_schoolCodeToOidMap.get(m_schoolCode);
        }

        /**
         * Find student.
         *
         * @return String
         */
        private String findStudent() {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(m_studentGTIDFieldJavaName, m_GTID);
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            SisStudent student = getBroker().getBeanByQuery(query);
            return student != null ? student.getOid() : null;
        }

        /**
         * Save bean.
         *
         * @param bean X2BaseBean
         * @return true, if successful
         */
        private boolean saveBean(X2BaseBean bean) {
            boolean ok = true;
            if (m_commitChanges) {
                List<ValidationError> errors = getBroker().saveBean(bean);
                if (errors != null && !errors.isEmpty()) {
                    addBeanValidationErrors(errors);
                    ok = false;
                }
            }
            return ok;
        }

        /**
         * Update fields.
         *
         * @param bean X2BaseBean
         * @param studentOid String
         * @param schoolOid String
         * @param insert boolean
         */
        private void updateFields(X2BaseBean bean, String studentOid, String schoolOid, boolean insert) {
            bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_CAREER_APTITUDE_8), m_careerAptitude8);
            bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_CAREER_APTITUDE_10),
                    m_careerAptitude10);
            bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_CAREER_INTEREST), m_careerInterest);
            bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_INDIVIDUAL_GRADUATION_PLAN),
                    m_individualGraduationPlan);
            if (insert) {
                bean.setFieldValueByBeanPath(UserDefinedTableA.COL_STUDENT_OID, studentOid);
                bean.setFieldValueByBeanPath(UserDefinedTableA.COL_SCHOOL_OID, schoolOid);
                bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_SCHOOL_CODE), m_schoolCode);
                bean.setFieldValueByBeanPath(m_dataFieldsJavaNames.get(ALIAS_FISCAL_YEAR), m_fiscalYear);
            }
        }
    }

    private static final int ACTION_INSERT = 1;
    private static final int ACTION_SKIP = -1;
    private static final int ACTION_UPDATE = 0;

    private static final String ALIAS_CAREER_APTITUDE_8 = "DOE Career Aptitude Inv 8th";
    private static final String ALIAS_CAREER_APTITUDE_10 = "DOE Career Int & Apt 10th";
    private static final String ALIAS_CAREER_INTEREST = "DOE Career Interest";
    private static final String ALIAS_FISCAL_YEAR = "DOE CC School Year";
    private static final String ALIAS_GTID = "GTID";
    private static final String ALIAS_INDIVIDUAL_GRADUATION_PLAN = "DOE Individual Graduation Plan";
    private static final String ALIAS_SCHOOL_CODE = "DOE CC School Code";

    private static final String EXTENDED_DATA_DICTIONARY_ID = "STD-SLDS-CC";

    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String INPUT_PARAM_SKIP_ROWS = "skipRows";

    private static final Pattern PATTERN_FISCAL_YEAR = Pattern.compile("\\d{4}");

    private Class m_beanClass;
    private ModelBroker m_broker;
    private boolean m_commitChanges;
    private Map<String, String> m_dataFieldsJavaNames = new HashMap<>();
    private List<String> m_errors = new LinkedList();
    private DataDictionary m_extendedDataDictionary;
    private Map<String, String> m_schoolCodeToOidMap = new HashMap<>();
    private List<String> m_setupErrors = new LinkedList();
    private int m_skipRows;
    private String m_studentGTIDFieldJavaName;

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.sis.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Display only init errors, if any, and exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        if (!m_setupErrors.isEmpty()) {
            exportErrors(m_setupErrors);
        } else if (!m_errors.isEmpty()) {
            m_errors.add("\n");
            exportErrors(m_errors);
        } else {
            StringBuilder buffer = new StringBuilder(m_commitChanges ? "Commit Mode." : "Review Mode.");
            buffer.append("\n\n");
            buffer.append(getImportStatistics().toString());
            try {
                export(buffer.toString());
            } catch (IOException ioe) {
                throw new X2BaseException(ioe);
            }
        }
    }

    /**
     * Gets the fields count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return Header.values().length;
    }

    /**
     * Returns lengths of all fields (including ignored) in order of they appearance
     *
     * @return int[]
     */
    @Override
    protected int[] getFieldLengths() {
        return new int[] {
                Header.FILE_ID.getValue(),
                Header.FISCAL_YEAR.getValue(),
                Header.SYSTEM_CODE.getValue(),
                Header.SCHOOL_CODE.getValue(),
                Header.GTID.getValue(),
                Header.STUDENT_LAST_NAME.getValue(),
                Header.STUDENT_FIRST_NAME.getValue(),
                Header.STUDENT_MIDDLE_NAME.getValue(),
                Header.CAREER_APTITUDE_8.getValue(),
                Header.CAREER_APTITUDE_10.getValue(),
                Header.CAREER_INTEREST.getValue(),
                Header.INDIVIDUAL_GRADUATION_PLAN.getValue()
        };
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_setupErrors.isEmpty()) {
            super.importData(sourceFile);
        }
    }

    /**
     * Import record.
     *
     * @param fields List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> fields, int lineNumber) throws Exception {
        if (lineNumber > m_skipRows) {
            Record record = new Record(lineNumber, fields);
            int result = ACTION_SKIP;
            boolean skipWhenBlank =
                    StringUtils.isEmpty(record.m_careerAptitude10) && StringUtils.isEmpty(record.m_careerAptitude8)
                            && StringUtils.isEmpty(record.m_careerInterest)
                            && StringUtils.isEmpty(record.m_individualGraduationPlan);
            if (!skipWhenBlank && record.isValid()) {
                result = record.applyChanges();
            }
            if (ACTION_SKIP == result) {
                incrementSkipCount();
            } else if (ACTION_UPDATE == result) {
                incrementUpdateCount();
            } else {
                incrementInsertCount();
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_skipRows = ((Integer) getParameter(INPUT_PARAM_SKIP_ROWS)).intValue();
        Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
        m_commitChanges = commit == null || commit.booleanValue();
        loadStudentGTIDFieldJavaName();
        loadExtendedDataDictionary();
        if (m_setupErrors.isEmpty()) {
            initializeReader();
        }
    }

    /**
     * Adds one validation error.
     *
     * @param errorType String
     * @param errorMessage String
     */
    private void addSetupError(String errorType, String errorMessage) {
        m_setupErrors.add(errorType + "-" + errorMessage);
    }

    /**
     * Export.
     *
     * @param str String
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void export(String str) throws IOException {
        ByteArrayInputStream inputStream = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8));
        try {
            StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
        } finally {
            inputStream.close();
        }
    }

    /**
     * Export errors.
     *
     * @param errors List<String>
     * @throws X2BaseException exception
     */
    private void exportErrors(List<String> errors) throws X2BaseException {
        StringBuilder buffer = new StringBuilder();
        buffer.append('\n');
        buffer.append("Errors:" + '\n');
        for (String err : errors) {
            buffer.append(err);
            buffer.append('\n');
        }
        try {
            export(buffer.toString());
        } catch (IOException ioex) {
            throw new X2BaseException(ioex);
        }
    }

    /**
     * Initialize reader.
     */
    private void initializeReader() {
        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
        setUseValueDelimiters(false);
        setUseEscapes(false);
    }

    /**
     * Load extended data dictionary.
     */
    private void loadExtendedDataDictionary() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DATA_DICTIONARY_ID);
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = getBroker().getBeanByQuery(byCriteria);
        m_extendedDataDictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        if (m_extendedDataDictionary == null) {
            addSetupError("Extended Data Dictionary", "not found: " + EXTENDED_DATA_DICTIONARY_ID);
        } else {
            loadFields();
        }
    }

    /**
     * Load fields.
     */
    private void loadFields() {
        translateAliasToJavaName(ALIAS_FISCAL_YEAR, true);
        translateAliasToJavaName(ALIAS_SCHOOL_CODE, false);
        translateAliasToJavaName(ALIAS_CAREER_APTITUDE_8, false);
        translateAliasToJavaName(ALIAS_CAREER_APTITUDE_10, false);
        translateAliasToJavaName(ALIAS_CAREER_INTEREST, false);
        translateAliasToJavaName(ALIAS_INDIVIDUAL_GRADUATION_PLAN, false);
    }

    /**
     * Load Java Name by GTID alias for student.
     */
    private void loadStudentGTIDFieldJavaName() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField fieldGTID = dictionary.findDataDictionaryFieldByAlias(ALIAS_GTID);
        m_studentGTIDFieldJavaName = null;
        if (fieldGTID != null) {
            m_studentGTIDFieldJavaName = fieldGTID.getJavaName();
        }
        if (m_studentGTIDFieldJavaName == null) {
            addSetupError("Data Dictionary Field", "not found: GTID (for Student)");
        }
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @param setBeanClass boolean
     */
    private void translateAliasToJavaName(String alias, boolean setBeanClass) {
        DataDictionaryField dataDictionaryField = m_extendedDataDictionary.findDataDictionaryFieldByAlias(alias);
        String javaName = null;
        if (dataDictionaryField != null) {
            if (setBeanClass) {
                m_beanClass = dataDictionaryField.getDataTable().getDataClass();
            }
            javaName = dataDictionaryField.getJavaName();
        }
        if (StringUtils.isEmpty(javaName)) {
            addSetupError("Extended Data Dictionary Field", "not found: " + alias);
        } else {
            m_dataFieldsJavaNames.put(alias, javaName);
        }
    }

}
