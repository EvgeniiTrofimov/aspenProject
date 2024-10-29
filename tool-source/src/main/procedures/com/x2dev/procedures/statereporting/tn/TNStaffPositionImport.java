/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNStaffPositionImport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class TNStaffPositionImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Enum IMPORT_FIELDS.
     */
    // Import indexes
    private enum IMPORT_FIELD {
        STAFF_ID_LOCAL(0, true, "LocalID"), STAFF_NAME(1, false, "Name"), SCHOOL_STATE_ID(2, true,
                "SKLStateID"), SCHOOL_NAME(3, false, "SKLName"), JOB_CODE(4, true, "Job"), START_DATE(5, true,
                        "StartDate"), END_DATE(6, false, "EndDate"), INSTRUCTIONAL_PROGRAM(7, true,
                                "InstructionalProgram"), PROGRAM_ASSIGNMENT(8, true, "ProgAssign");

        private final int m_index;
        private final boolean isRequired;
        private final String m_fieldName;

        /**
         * Instantiates a new import fields.
         *
         * @param index int
         */
        IMPORT_FIELD(int index, boolean isRequired, String fieldName) {
            this.m_index = index;
            this.isRequired = isRequired;
            this.m_fieldName = fieldName;
        }

        /**
         * Gets the index.
         *
         * @return int
         */
        public int getIndex() {
            return m_index;
        }

        public String getFieldName() {
            return m_fieldName;
        }

        public boolean isRequired() {
            return isRequired;
        }
    }

    /**
     * The Class StaffPositionRecord.
     */
    protected class StaffPositionRecord {

        private boolean m_isValid = true;
        private int m_lineNumber;
        private List<String> m_record;
        private Map<IMPORT_FIELD, Object> m_fields;
        private String m_schoolOid;
        private String m_staffOid;

        /**
         * Instantiates a new staff position record.
         *
         * @param record List<String>
         * @param lineNumber int
         */
        public StaffPositionRecord(List<String> record, int lineNumber) {
            this.m_record = record;
            this.m_lineNumber = lineNumber;
            this.m_fields = new HashMap<IMPORT_FIELD, Object>();

            setStaffLocalId();
            setStaffName();
            setSchoolOidBySchoolStateId();
            setSchoolName();
            setJobCode();
            setStartDate();
            setEndDate();
            if (isValid()) {
                validateDateRange();
            }
            setInstructionalProgram();
            setProgramAssignment();
            if (isValid()) {
                validateSameOverlap();
            }
        }

        public void setField(IMPORT_FIELD field, Object value) {
            if (isValid() && validateRequired(field, value)) {
                m_fields.put(field, value);
            }
        }

        public Object getField(IMPORT_FIELD field) {
            return m_fields.get(field);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return (PlainDate) getField(IMPORT_FIELD.END_DATE);
        }

        /**
         * Gets the instructional program.
         *
         * @return String
         */
        public String getInstructionalProgram() {
            return (String) getField(IMPORT_FIELD.INSTRUCTIONAL_PROGRAM);
        }

        /**
         * Gets the job code.
         *
         * @return String
         */
        public String getJobCode() {
            return (String) getField(IMPORT_FIELD.JOB_CODE);
        }

        /**
         * Gets the program assignment.
         *
         * @return String
         */
        public String getProgramAssignment() {
            return (String) getField(IMPORT_FIELD.PROGRAM_ASSIGNMENT);
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        public String getSchoolName() {
            return (String) getField(IMPORT_FIELD.SCHOOL_NAME);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return m_schoolOid;
        }

        /**
         * Gets the school state id.
         *
         * @return String
         */
        public String getSchoolStateId() {
            return (String) getField(IMPORT_FIELD.SCHOOL_STATE_ID);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return (PlainDate) getField(IMPORT_FIELD.START_DATE);
        }

        /**
         * Gets the staff name.
         *
         * @return String
         */
        public String getStaffName() {
            return (String) getField(IMPORT_FIELD.STAFF_NAME);
        }

        /**
         * Gets the staff oid.
         *
         * @return String
         */
        public String getStaffOid() {
            return m_staffOid;
        }

        public String getStaffLocalId() {
            return (String) getField(IMPORT_FIELD.STAFF_ID_LOCAL);
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            return m_isValid;
        }

        /**
         * Validate same overlap.
         */
        public void validateSameOverlap() {
            X2Criteria staffPositionCriteria = new X2Criteria();
            staffPositionCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getSchoolOid());
            staffPositionCriteria.addEqualTo(StaffPosition.COL_STAFF_OID, getStaffOid());
            staffPositionCriteria.addLessThan(StaffPosition.COL_START_DATE, getStartDate());

            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addGreaterThan(StaffPosition.COL_END_DATE, getStartDate());
            X2Criteria endDateEmptyCriteria = new X2Criteria();
            endDateEmptyCriteria.addIsNull(StaffPosition.COL_END_DATE);
            endDateCriteria.addOrCriteria(endDateEmptyCriteria);

            staffPositionCriteria.addAndCriteria(endDateCriteria);

            QueryByCriteria staffPositionQuery = new QueryByCriteria(StaffPosition.class, staffPositionCriteria);
            if (m_modelBroker.getCount(staffPositionQuery) > 0) {
                logInvalidRecord(m_lineNumber, "Same record with overlapped start/end date exist");
                m_isValid = false;
            }
        }

        protected boolean validateRequired(IMPORT_FIELD field, Object value) {
            if (field.isRequired()) {
                if (value == null || value.toString().isEmpty()) {
                    logInvalidRecord(m_lineNumber, makeMessage(m_record, field.getFieldName() + " not defined"));
                    m_isValid = false;
                }
            }
            return m_isValid;
        }

        /**
         * Sets the end date.
         */
        protected void setEndDate() {
            PlainDate endDate = m_converter.stringToJava(m_record.get(IMPORT_FIELD.END_DATE.getIndex()));
            setField(IMPORT_FIELD.END_DATE, endDate);
        }

        /**
         * Sets the instructional program.
         */
        protected void setInstructionalProgram() {
            setField(IMPORT_FIELD.INSTRUCTIONAL_PROGRAM,
                    m_record.get(IMPORT_FIELD.INSTRUCTIONAL_PROGRAM.getIndex()).trim());
        }

        /**
         * Sets the job code.
         */
        protected void setJobCode() {
            setField(IMPORT_FIELD.JOB_CODE, m_record.get(IMPORT_FIELD.JOB_CODE.getIndex()).trim());
        }

        /**
         * Sets the program assignment.
         */
        protected void setProgramAssignment() {
            setField(IMPORT_FIELD.PROGRAM_ASSIGNMENT,
                    m_record.get(IMPORT_FIELD.PROGRAM_ASSIGNMENT.getIndex()).trim());
        }

        /**
         * Sets the school name.
         */
        protected void setSchoolName() {
            setField(IMPORT_FIELD.SCHOOL_NAME, m_record.get(IMPORT_FIELD.SCHOOL_NAME.getIndex()));
        }

        /**
         * Sets the school oid by school state id.
         */
        protected void setSchoolOidBySchoolStateId() {

            setField(IMPORT_FIELD.SCHOOL_STATE_ID, m_record.get(IMPORT_FIELD.SCHOOL_STATE_ID.getIndex()));

            if (isValid()) {
                setField(IMPORT_FIELD.SCHOOL_STATE_ID, StringUtils.padLeft(getSchoolStateId(), 4, '0'));
                if (isValid() && !m_schoolsMap.containsKey(getSchoolStateId())) {
                    logInvalidRecord(m_lineNumber, makeMessage(m_record, "State school ID is not available"));
                    m_isValid = false;
                } else {
                    SisSchool school = m_schoolsMap.get(getSchoolStateId());
                    if (isValid() && (school.getInactiveIndicator() || school.getArchiveIndicator())) {
                        logInvalidRecord(m_lineNumber,
                                makeMessage(m_record, "School defined by State School ID is not active or archived"));
                        m_isValid = false;
                    }
                    m_schoolOid = school.getOid();
                }
            }
        }

        /**
         * Sets the start date.
         */
        protected void setStartDate() {
            PlainDate startDate = m_converter.stringToJava(m_record.get(IMPORT_FIELD.START_DATE.getIndex()));
            setField(IMPORT_FIELD.START_DATE, startDate);
        }

        /**
         * Sets the staff local Id
         */
        protected void setStaffLocalId() {
            setField(IMPORT_FIELD.STAFF_ID_LOCAL, m_record.get(IMPORT_FIELD.STAFF_ID_LOCAL.getIndex()));
            if (isValid()) {
                setField(IMPORT_FIELD.STAFF_ID_LOCAL, getStaffLocalId().replaceFirst("^0+(?!$)", ""));
                if (isValid() && !m_staffMap.containsKey(getStaffLocalId())) {
                    logInvalidRecord(m_lineNumber,
                            makeMessage(m_record, "Staff with such local id is not available"));
                    m_isValid = false;
                } else {
                    m_staffOid = m_staffMap.get(getStaffLocalId()).getOid();
                }
            }
        }

        /**
         * Sets the staff name.
         */
        protected void setStaffName() {
            setField(IMPORT_FIELD.STAFF_NAME, m_record.get(IMPORT_FIELD.STAFF_NAME.getIndex()));
        }

        /**
         * Validate date range.
         */
        protected void validateDateRange() {
            if ((m_selectedContext.getStartDate().after(getStartDate())
                    || m_selectedContext.getEndDate().before(getStartDate()))) {
                logInvalidRecord(m_lineNumber,
                        makeMessage(m_record, "Date range outside range of School Year selected"));
                m_isValid = false;
            }
        }
    }

    private static final String ALIAS_INSTRUCTIONAL_PROGRAM = "DOE INSTRUCTIONAL PROGRAM STF";
    private static final String ALIAS_PROGRAM_ASSIGNMENT = "sfp_edfi_prog_assignment";

    private static final String PARAM_COMMIT = "commit";
    private static final String PARAM_CONTEXT_OID = "districtContextOid";
    private static final String PARAM_SKIP_FIRST_ROW = "skipFirstRow";

    private boolean m_commit;
    private DateConverter m_converter;
    private DataDictionary m_dictionary;

    private ModelBroker m_modelBroker;
    private Map<String, SisSchool> m_schoolsMap;
    private DistrictSchoolYearContext m_selectedContext;
    private DataDictionaryField m_sfpInstructionalProgramField;
    private DataDictionaryField m_sfpProgramAssignmentField;
    private boolean m_skipFirstRow;
    private Map<String, Staff> m_staffMap;
    private List<String> m_setupErrors = new LinkedList();

    /**
     * Display only init errors if exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        if (!m_setupErrors.isEmpty()) {
            exportList(m_setupErrors);
        } else {
            super.exportResults();
        }
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return IMPORT_FIELD.values().length;
    }

    /**
     * Return StringBuilder which contain the text that would show to user when the import procedure
     * finished.
     *
     * @return StringBuilder
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder messageToReturn = new StringBuilder(312);
        String mode = m_commit ? "Commit Mode." : "Review Mode.";
        messageToReturn.append(mode);
        messageToReturn.append('\n');
        messageToReturn.append('\n');
        StringBuilder bufferMessage = super.getImportStatistics();
        messageToReturn.append(bufferMessage);
        return messageToReturn;
    }

    /**
     * Gets the staff position.
     *
     * @param staffOid String
     * @param schoolOid String
     * @param startDate PlainDate
     * @return Staff position
     */
    protected StaffPosition getStaffPosition(String staffOid, String schoolOid, PlainDate startDate, String jobCode) {
        X2Criteria staffPositionCriteria = new X2Criteria();
        staffPositionCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, schoolOid);
        staffPositionCriteria.addEqualTo(StaffPosition.COL_STAFF_OID, staffOid);
        staffPositionCriteria.addEqualTo(StaffPosition.COL_START_DATE, startDate);
        staffPositionCriteria.addEqualTo(StaffPosition.COL_JOB_CODE, jobCode);

        QueryByCriteria staffPositionQuery = new QueryByCriteria(StaffPosition.class, staffPositionCriteria);
        StaffPosition staffPosition = (StaffPosition) m_modelBroker.getBeanByQuery(staffPositionQuery);
        if (staffPosition == null) {
            incrementInsertCount();
            staffPosition = X2BaseBean.newInstance(StaffPosition.class, getBroker().getPersistenceKey());
        } else {
            incrementMatchCount();
            incrementUpdateCount();
        }

        return staffPosition;
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
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        try {
            if ((m_skipFirstRow && lineNumber > 1) || (!m_skipFirstRow)) {

                StaffPositionRecord staffPositionCurrentRecord = new StaffPositionRecord(record, lineNumber);

                if (staffPositionCurrentRecord.isValid()) {
                    StaffPosition staffPostion =
                            getStaffPosition(staffPositionCurrentRecord.getStaffOid(),
                                    staffPositionCurrentRecord.getSchoolOid(),
                                    staffPositionCurrentRecord.getStartDate(),
                                    staffPositionCurrentRecord.getJobCode());
                    updateField(staffPostion, StaffPosition.COL_SCHOOL_OID, staffPositionCurrentRecord.getSchoolOid());
                    updateField(staffPostion, StaffPosition.COL_STAFF_OID, staffPositionCurrentRecord.getStaffOid());
                    updateField(staffPostion, StaffPosition.COL_START_DATE, staffPositionCurrentRecord.getStartDate());
                    updateField(staffPostion, StaffPosition.COL_END_DATE, staffPositionCurrentRecord.getEndDate());
                    updateField(staffPostion, StaffPosition.COL_JOB_CODE, staffPositionCurrentRecord.getJobCode());
                    updateField(staffPostion, m_sfpInstructionalProgramField.getJavaName(),
                            staffPositionCurrentRecord.getInstructionalProgram());
                    updateField(staffPostion, m_sfpProgramAssignmentField.getJavaName(),
                            staffPositionCurrentRecord.getProgramAssignment());

                    if (m_commit) {
                        m_modelBroker.saveBean(staffPostion);
                    }
                } else {
                    incrementSkipCount();
                }
            }
        } catch (Exception ex) {
            logInvalidRecord(lineNumber, "system error: record malformed");
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
        super.initialize();

        m_modelBroker = new ModelBroker(getPrivilegeSet());
        m_converter = (DateConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());

        // Gets the input parameters
        String m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_selectedContext =
                (DistrictSchoolYearContext) m_modelBroker.getBeanByOid(DistrictSchoolYearContext.class, m_contextOid);

        m_skipFirstRow = ((Boolean) getParameter(PARAM_SKIP_FIRST_ROW)).booleanValue();
        Boolean commit = (Boolean) getParameter(PARAM_COMMIT);
        m_commit = commit != null ? commit.booleanValue() : true; // default: commit & review

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_sfpInstructionalProgramField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_INSTRUCTIONAL_PROGRAM);
        m_sfpProgramAssignmentField =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_PROGRAM_ASSIGNMENT);

        File sourceFile = (File) getParameter(FILE_KEY);
        if (sourceFile == null || !sourceFile.getAbsolutePath().endsWith(".csv")) {
            addSetupError("Setup Error", "Incorrect File Format");
        }

        loadSchools();
        loadStaff();
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
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<String> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        for (String err : list) {
            buffer.append(err);
            buffer.append('\n');
        }
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
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
     * Load schools.
     */
    private void loadSchools() {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias("DOE SCHOOL STATE ID");
        QueryByCriteria schoolsQuery = new QueryByCriteria(SisSchool.class, null);
        m_schoolsMap = m_modelBroker.getMapByQuery(schoolsQuery, field.getJavaName(), 100);
    }

    private void loadStaff() {
        QueryByCriteria staffQuery = new QueryByCriteria(Staff.class, null);
        Collection<Staff> staffCollection =
                m_modelBroker.getCollectionByQuery(staffQuery);

        m_staffMap = new HashMap<String, Staff>();
        for (Staff staff : staffCollection) {
            String trimmedStaffLocalId = staff.getLocalId().replaceFirst("^0+(?!$)", "");
            if (!m_staffMap.containsKey(trimmedStaffLocalId)) {
                m_staffMap.put(trimmedStaffLocalId, staff);
            }
        }
    }

    /**
     * Make message.
     *
     * @param record List<String>
     * @param message String
     * @return String
     */
    private String makeMessage(List<String> record, String message) {
        return record.get(IMPORT_FIELD.STAFF_NAME.getIndex()).trim() + ": " + message;
    }

    /**
     * Updates the passed field with the passed value if the value is not null.
     *
     * @param bean X2BaseBean
     * @param beanPath String
     * @param newValue Object
     */
    private void updateField(X2BaseBean bean, String beanPath, Object newValue) {
        Object value = bean.getFieldValueByBeanPath(beanPath);
        if (value == null || newValue != null) {
            if (newValue instanceof String && !StringUtils.isEmpty((String) newValue)) {
                bean.setFieldValueByBeanPath(beanPath, newValue);
            } else if (!(newValue instanceof String)) {
                bean.setFieldValueByBeanPath(beanPath, newValue);
            }
        }
    }
}
