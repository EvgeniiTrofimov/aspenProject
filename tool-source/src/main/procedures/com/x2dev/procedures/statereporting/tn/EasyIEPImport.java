/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.regex.Pattern;
import org.apache.commons.lang.WordUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Procedure to import IEP data.
 *
 * @author Follett Software Company
 */
public class EasyIEPImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final long serialVersionUID = 1L;

    private static final int FIELD_COUNT = Field.values().length;

    private static final String INPUT_PARAM_COMMIT = "commit";
    private final static String PARAM_DOE_PIN_ALIAS = "studentDoePinAlias";
    private final static String PARAM_IMPORT_FLAG_ALIAS = "programImportFlagAlias";
    private final static String PARAM_IMPORT_TYPE_ALIAS = "programImportTypeAlias";

    protected final static String CATEGORY_DISABILITY = "Disability";
    protected final static String CATEGORY_OPTIONS = "Options";

    private static final String MESSAGE_FIELD_INFO = "field[%d] (%s)";
    private static final String MESSAGE_INVALID_DATA_FORMAT = "ERROR: Invalid data provided for %s, value = %s";
    private static final String MESSAGE_INVALID_STUDENT_ID =
            "ERROR: Type = %s, Invalid student ID, student with SSN/PIN not found, LASID = %s";
    private static final String MESSAGE_INVALID_DISTRICT_ID = "ERROR: Invalid district ID.";
    private static final String MESSAGE_INVALID_REQUEST = "ERROR: Type = %s, Invalid request, reason = %s";
    private static final String MESSAGE_INVALID_PROGRAM_CODE = "ERROR: Invalid program code, (%s)";
    private static final String MESSAGE_UNEXPECTED_ERROR = "ERROR: Unexpected error during processing (%s)";
    private static final String MESSAGE_MULTIPLE_IDS = "WARNING: Student SSN and PIN provided";
    private static final String REASON_NO_MATCHING_RECORDS =
            "No matching records were found for student with SSN/PIN, LASID = %s. Record ID = %s.";
    private static final String MESSAGE_NO_PRIMARY_OPTION_ALIAS =
            "WARNING: Alias [EasyIEP Primary Option] should be defined on student";

    private static final String ALIAS_STD_PRIMARY_OPTION = "EasyIEP Primary Option";

    private static final String FLAG_RECORD_ID_DISABILITY_CODES = "093";
    private static final String FLAG_RECORD_ID_FUNDING_OPTIONS = "092";
    private static final String FLAG_RECORD_TYPE_NEW = "N";
    private static final String FLAG_RECORD_TYPE_EDIT = "E";
    private static final String FLAG_RECORD_TYPE_DELETE = "D";
    private static final String FLAG_DISABILITY_LEVEL_PRIMARY = "P";
    private static final String FLAG_DISABILITY_LEVEL_SECONDARY = "S";
    private static final String FLAG_ACTIVE = "Active";
    private static final String FLAG_INACTIVE = "Inactive";
    private static final String FLAG_PROGRAM_CODE_DISABILITY = "d";
    private static final String FLAG_PROGRAM_CODE_OPTIONS = "o";
    private static final String FLAG_PROGRAM_IMPORTED = "1";

    private static final String REGEX_DISABILITY_CODE = "[0-9]{2}";
    private static final String REGEX_DISTRICT_ID = "[0-9]{3}";
    private static final String REGEX_ID = "[0-9]{9}";
    private static final String REGEX_LASID = "[0-9]{1,10}";
    private static final String REGEX_PRIMARY_OPTION = "(?i)op\\d{2}";
    private static final String REGEX_SSN_DASHES = "\\d{3}-\\d{2}-\\d{4}";
    private static final String REGEX_ZERO_FILLED = "[0]{9}";
    private static final String REGEX_RECORD_TYPE = String.format("(%s)|(%s)|(%s)", FLAG_RECORD_TYPE_NEW,
            FLAG_RECORD_TYPE_EDIT, FLAG_RECORD_TYPE_DELETE);
    private static final String REGEX_RECORD_ID = String.format("(%s)|(%s)",
            FLAG_RECORD_ID_DISABILITY_CODES,
            FLAG_RECORD_ID_FUNDING_OPTIONS);
    private static final String REGEX_DISABILITY_LEVEL = String.format("[%s%s]{1}", FLAG_DISABILITY_LEVEL_PRIMARY,
            FLAG_DISABILITY_LEVEL_SECONDARY);

    private static final String FORMAT_DATE = "yyyyMMdd";
    private static final String FORMAT_EMPTY_DATE = "00000000";
    private static final String FORMAT_PROGRAM_CODE = "%s%s%s";

    private Collection<String> m_allCodes;
    private List<Record> m_allRecords;
    private Boolean m_commitChanges;
    private X2Broker m_modelBroker;
    private PlainDate m_currentDate;
    private String m_doePinField;
    private String m_importFlagAlias;
    private String m_importTypeField;
    private String m_inputDefinition;
    private boolean m_ftpJobRunningInd = false;
    private String m_primaryOptionField;
    private Map<String, List<Record>> m_recordsByStudentOid;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return FIELD_COUNT;
    }



    /**
     * Set the m_doePinField.
     *
     * @param doePinField void
     */
    public void setDoePinField(String doePinField) {
        this.m_doePinField = doePinField;
    }


    /**
     * Set the m_importFlagAlias.
     *
     * @param importFlagAlias void
     */
    public void setImportFlagAlias(String importFlagAlias) {
        this.m_importFlagAlias = importFlagAlias;
    }


    /**
     * Set the m_importTypeField.
     *
     * @param importTypeField void
     */
    public void setImportTypeField(String importTypeField) {
        this.m_importTypeField = importTypeField;
    }


    /**
     * Sets the all records.
     *
     * @param allRecords void
     */
    public void setAllRecords(List<Record> allRecords) {
        this.m_allRecords = allRecords;
    }



    /**
     * Sets the records by student oid.
     *
     * @param recordsByStudentOid Map<String,List<Record>>
     */
    public void setRecordsByStudentOid(Map<String, List<Record>> recordsByStudentOid) {
        this.m_recordsByStudentOid = recordsByStudentOid;
    }



    /**
     * Sets the current date.
     *
     * @param currentDate void
     */
    public void setCurrentDate(PlainDate currentDate) {
        this.m_currentDate = currentDate;
    }


    /**
     * Sets the all codes.
     *
     * @param allCodes void
     */
    public void setAllCodes(Collection<String> allCodes) {
        this.m_allCodes = allCodes;
    }

    /**
     * Gets the all codes.
     *
     * @return the m_allCodes
     */
    public Collection<String> getAllCodes() {
        return m_allCodes;
    }

    /**
     * Sets the broker.
     *
     * @param broker void
     */
    public void setBroker(X2Broker broker) {
        this.m_modelBroker = broker;
    }

    /**
     * Sets the ftp job running ind.
     *
     * @param ftpJobRunningInd void
     */
    public void setFtpJobRunningInd(boolean ftpJobRunningInd) {
        this.m_ftpJobRunningInd = ftpJobRunningInd;
    }

    /**
     * Returns the input definition of the export.
     *
     * @return String
     */
    public String getInputDefinition() {
        return m_inputDefinition;
    }

    /**
     * Set the input definition of the export.
     *
     * @param inputDefinition void
     */
    public void setInputDefinition(String inputDefinition) {
        this.m_inputDefinition = inputDefinition;
    }

    /**
     * Commit changes.
     *
     * @return true, if changes should be committed
     */
    protected boolean commitChanges() {
        if (m_commitChanges == null) {
            Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
            m_commitChanges = commit != null ? commit : Boolean.FALSE; // default: review
        }
        return m_commitChanges.booleanValue();
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        return m_modelBroker;
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
        String mode = commitChanges() ? "Commit Mode." : "Review Mode.";
        messageToReturn.append(mode);
        messageToReturn.append('\n');
        messageToReturn.append('\n');
        StringBuilder bufferMessage = super.getImportStatistics();
        messageToReturn.append(bufferMessage);
        return messageToReturn;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        super.importData(sourceFile);
        processAllRecords();
        if (m_primaryOptionField != null) {
            updateAllPrimaryOption();
        } else {
            this.logInvalidRecord(0, MESSAGE_NO_PRIMARY_OPTION_ALIAS);
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
        if (lineNumber != 1) {
            Record currentRecord = new Record();
            currentRecord.line = lineNumber - 1;

            try {
                String currentDistrictID = getOrganization().getId();
                String districtID = getFieldValue(record, Field.DISTRICT_ID, REGEX_DISTRICT_ID);
                if (!districtID.equals(currentDistrictID)) {
                    throw new RecordProcessingException(MESSAGE_INVALID_DISTRICT_ID);
                }
                currentRecord.recordId = getFieldValue(record, Field.RECORD_ID, REGEX_RECORD_ID);
                currentRecord.programCode = getProgramCode(record);
                currentRecord.level = getFieldValue(record, Field.DISABILITY_LEVEL, REGEX_DISABILITY_LEVEL);
                currentRecord.startDate = getDateField(record, Field.DISABILITY_START_DATE);
                currentRecord.endDate = getDateField(record, Field.DISABILITY_END_DATE);
                try {
                    currentRecord.studentSSN = getFieldValue(record, Field.STUDENT_SSN, REGEX_ID);
                } catch (RecordProcessingException rpe) {
                    currentRecord.studentSSN = getFieldValue(record, Field.STUDENT_SSN, REGEX_SSN_DASHES);
                }

                currentRecord.studentPIN = getFieldValue(record, Field.STUDENT_PIN, REGEX_ID);
                currentRecord.recordType = getFieldValue(record, Field.RECORD_TYPE, REGEX_RECORD_TYPE);
                currentRecord.studentLasid = getFieldValue(record, Field.STUDENT_LOCAL_KEY, REGEX_LASID);
                currentRecord.valid = true;

            } catch (RecordProcessingException e) {
                currentRecord.valid = false;
                currentRecord.message = e.getMessage();
            }

            m_allRecords.add(currentRecord);
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
        setValueDelimiter('\t');
        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
        setBroker(getBroker());
        m_allRecords = new ArrayList<Record>();
        m_recordsByStudentOid = new HashMap<String, List<Record>>();
        m_currentDate = new PlainDate();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField primaryOptionField = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_PRIMARY_OPTION);
        m_primaryOptionField = primaryOptionField != null ? primaryOptionField.getJavaName() : null;

        if (!m_ftpJobRunningInd) {
            String doePinAlias = (String) getParameter(PARAM_DOE_PIN_ALIAS);

            DataDictionaryField doePinField = dictionary.findDataDictionaryFieldByAlias(doePinAlias);
            m_doePinField = doePinField.getJavaName();

            m_importFlagAlias = (String) getParameter(PARAM_IMPORT_FLAG_ALIAS);
            String importTypeAlias = (String) getParameter(PARAM_IMPORT_TYPE_ALIAS);
            DataDictionaryField importTypeField = dictionary.findDataDictionaryFieldByAlias(importTypeAlias);
            m_importTypeField = importTypeField != null ? importTypeField.getJavaName() : null;

        }

        loadDisabilityAndOptionsCodes();
    }

    /**
     * Loads all codes that are associated with the 'Disability' and 'Options' category.
     */
    protected void loadDisabilityAndOptionsCodes() {
        m_allCodes = new HashSet<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        Criteria categoryCriteria = new Criteria();
        categoryCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, CATEGORY_DISABILITY);
        Criteria optionsCriteria = new Criteria();
        optionsCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, CATEGORY_OPTIONS);
        categoryCriteria.addOrCriteria(optionsCriteria);
        criteria.addAndCriteria(categoryCriteria);

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);

        ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (queryItr.hasNext()) {
                Object[] row = (Object[]) queryItr.next();
                m_allCodes.add((String) row[0]);
            }
        } finally {
            queryItr.close();
        }
    }



    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_modelBroker = new ModelBroker(userData);
    }

    /**
     * Creates a new student program participation record in the database based on the record data
     * passed in. This
     * method will throw a RecordProcessingException if a duplicate student program participation
     * record already exists
     *
     * @param record
     *        data needed to create a new student program participation record
     * @throws RecordProcessingException
     *         if record already exists
     */
    private void createProgramParticipation(Record record) throws RecordProcessingException {
        Criteria matchingProgramCriteria = getMatchingProgramCriteria(record);
        int programCount = getProgramCount(matchingProgramCriteria);
        if (programCount == 0) {
            StudentProgramParticipation program = X2BaseBean.newInstance(
                    StudentProgramParticipation.class, getBroker().getPersistenceKey());

            program.setStudentOid(record.studentOid);
            program.setProgramCode(record.programCode);
            program.setStartDate(record.startDate);
            program.setEndDate(record.endDate);
            program.setFieldValueByAlias(m_importFlagAlias, FLAG_PROGRAM_IMPORTED);
            program.setFieldValueByBeanPath(m_importTypeField, record.level);
            if (commitChanges()) {
                getBroker().saveBeanForced(program);
            }
        } else {
            throw new RecordProcessingException(
                    String.format(MESSAGE_INVALID_REQUEST, record.recordType, "Matching Record Exists for New Record"));
        }
    }

    /**
     * Deletes all student program participation records that match the record passed in. This
     * method will throw a
     * RecordProcessingException if no matching records are found for deletion.
     *
     * @param record Record
     * @throws RecordProcessingException if record does not exist
     */
    private void deleteProgramParticipation(Record record) throws RecordProcessingException {
        Criteria deleteCriteria = getMatchingProgramCriteria(record);

        int beforeCount = getProgramCount(deleteCriteria);

        QueryByCriteria deleteQuery = new QueryByCriteria(StudentProgramParticipation.class, deleteCriteria);
        getBroker().deleteByQuery(deleteQuery);

        int afterCount = getProgramCount(deleteCriteria);

        if (beforeCount - afterCount < 1) {
            throw new RecordProcessingException(
                    String.format(MESSAGE_INVALID_REQUEST, record.recordType,
                            String.format(REASON_NO_MATCHING_RECORDS, record.studentLasid, record.recordId)));
        }
    }

    /**
     * This method returns a criteria that can be used to get all the student program participation
     * records that match
     * the student oid passed in and the program codes for the 'Disability' and 'Options'
     * categories, and whose end date
     * is either null or after the current date.
     *
     * @param studentOid String
     * @return the criteria for finding active student program participation records
     */
    private Criteria getActiveSpedCriteria(String studentOid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, m_allCodes);

        Criteria endDateCriteria = new Criteria();
        endDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        Criteria endDateSetCriteria = new Criteria();
        endDateSetCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, m_currentDate);

        endDateCriteria.addOrCriteria(endDateSetCriteria);

        criteria.addAndCriteria(endDateCriteria);
        return criteria;
    }

    /**
     * Parses the record field list based on the the field enum type passed in. This method checks
     * to verify that the
     * field is formatted to a date and converts the data to a PlainDate. A
     * RecordProcessingException is thrown if the
     * data does not match the date format yyyyMMdd.
     *
     * @param record List<String>
     * @param field Field
     * @return The date that is associated with the field for the incoming record
     * @throws RecordProcessingException if the field data does not match a date format
     */
    private PlainDate getDateField(List<String> record, Field field) throws RecordProcessingException {
        String dateField = record.get(field.ordinal());
        SimpleDateFormat dateFormat = new SimpleDateFormat(FORMAT_DATE);
        PlainDate date = null;
        try {
            if (!dateField.equals(FORMAT_EMPTY_DATE)) {
                date = new PlainDate(dateFormat.parse(dateField));
            }
        } catch (ParseException pe) {
            throw new RecordProcessingException(
                    String.format(MESSAGE_INVALID_DATA_FORMAT, field.getDescription(), dateField));
        }
        return date;
    }

    /**
     * Parses the field list based on the the field enum type passed in. This method checks to
     * verify that the field is
     * matches the format of the regular expression that is passed in. A RecordProcessingException
     * is thrown if the the
     * data does not match the field.
     *
     * @param record List<String>
     * @param field Field
     * @param regex String
     * @return The string representation for this field
     * @throws RecordProcessingException if the field data does not match a date format
     */
    private String getFieldValue(List<String> record, Field field, String regex) throws RecordProcessingException {
        String value = record.get(field.ordinal());
        if (!Pattern.matches(regex, value)) {
            throw new RecordProcessingException(
                    String.format(MESSAGE_INVALID_DATA_FORMAT, field.getDescription(), value));
        }
        return value;
    }

    /**
     * Returns the criteria where the student oid, program code and start date of a student program
     * participation
     * matches the record information.
     *
     * @param record Record
     * @return criteria for matching a student program participation record
     */
    private Criteria getMatchingProgramCriteria(Record record) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, record.studentOid);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, record.programCode);
        criteria.addEqualTo(StudentProgramParticipation.COL_START_DATE, record.startDate);
        criteria.addEqualTo(StudentProgramParticipation.COL_START_DATE, record.startDate);
        criteria.addEqualTo(this.m_importTypeField, record.level);
        return criteria;
    }

    /**
     * Assembles the program code based on the RECORD_ID, DISABILITY_LEVEL, and DISABILITY_CODE
     * fields from the import
     * record. This method verifies that the program code that is assembled exists in the list of
     * known 'Disability' and
     * 'Options' codes. If the program code does not exist, a RecordProcessingException is thrown.
     *
     * @param record List<String>
     * @return The the program code that is assembled for the record
     * @throws RecordProcessingException if the program code that is assembled is not found in the
     *         list of known program codes.
     */
    private String getProgramCode(List<String> record) throws RecordProcessingException {
        String recordID = getFieldValue(record, Field.RECORD_ID, REGEX_RECORD_ID);
        String level = getFieldValue(record, Field.DISABILITY_LEVEL, REGEX_DISABILITY_LEVEL);
        String option = getFieldValue(record, Field.DISABILITY_CODE, REGEX_DISABILITY_CODE);
        String programCodeFlag = recordID.equals(FLAG_RECORD_ID_FUNDING_OPTIONS) ? FLAG_PROGRAM_CODE_OPTIONS
                : FLAG_PROGRAM_CODE_DISABILITY;

        String programCode = String.format(FORMAT_PROGRAM_CODE, programCodeFlag, level, option);
        if (!m_allCodes.contains(programCode)) {
            throw new RecordProcessingException(String.format(MESSAGE_INVALID_PROGRAM_CODE, programCode));
        }

        return programCode;
    }

    /**
     * Counts the number of student program participation records matching the criteria passed in.
     *
     * @param criteria Criteria
     * @return count of the number of matching program participation records.
     */
    private int getProgramCount(Criteria criteria) {
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        return getBroker().getCount(query);
    }

    /**
     * Builds a map that maps Student IDs (SSN or PIN) to the student oid.
     *
     * @param idAttribute
     *        column name of the id attribute for this mapping (SSN or PIN)
     * @param ids
     *        the list of SNN or PIN to look up and match with student oid
     * @return a map of student IDs (SSN or PIN) to the student oid
     */
    private Map<String, String> getStudentOidsByID(String idAttribute, Collection<String> ids) {
        Map<String, String> studentOidMap = new HashMap<String, String>();

        if (ids == null || ids.size() == 0) {
            return studentOidMap;
        }

        Criteria criteria = new Criteria();
        criteria.addIn(idAttribute, ids);
        // criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, FLAG_ACTIVE);

        String[] columns = new String[] {idAttribute, X2BaseBean.COL_OID};
        ColumnQuery query = new ColumnQuery(SisStudent.class, columns, criteria);
        ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (queryItr.hasNext()) {
                Object[] row = (Object[]) queryItr.next();
                String studentId = (String) row[0];
                String oid = (String) row[1];
                if (!StringUtils.isEmpty(studentId) && !StringUtils.isEmpty(oid)) {
                    studentOidMap.put(studentId, oid);
                }
            }
        } finally {
            queryItr.close();
        }

        return studentOidMap;
    }

    /**
     * Get a list of valid program codes that are primary option codes.
     *
     * @return Collection
     */
    private Collection<String> loadPrimaryOptionCodes() {
        Collection<String> codes = new ArrayList(m_allCodes.size());
        for (String code : m_allCodes) {
            if (code.matches(REGEX_PRIMARY_OPTION)) {
                codes.add(code);
            }
        }
        return codes;
    }

    /**
     * get a map of students who have primary option programs that are current and the primary
     * option code.
     *
     * @return Map
     */
    private Map<String, String> loadPrimaryOptionStudents() {
        Map<String, String> map = new HashMap();

        PlainDate now = new PlainDate();
        X2Criteria criteria = new X2Criteria();

        // Active programs
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, now);
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, now);
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        // Primary option codes
        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, loadPrimaryOptionCodes());

        String[] columns = new String[] {StudentProgramParticipation.COL_STUDENT_OID,
                StudentProgramParticipation.COL_PROGRAM_CODE};

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentProgramParticipation.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                // indeterminate selection of multiple primary options
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                String programCode = (String) row[1];
                map.put(studentOid, programCode);
            }
        } finally {
            iterator.close();
        }
        return map;
    }

    /**
     * Processes all the records read in and associates a student oid with each record. If a student
     * oid cannot be found
     * based on the SSN or PIN given, the record is marked invalid and an error message is set for
     * that record.
     */
    private void loadStudentOids() {
        Set<String> studentSSNs = new HashSet<String>();
        Set<String> studentPINs = new HashSet<String>();
        for (Record record : m_allRecords) {
            if (record.valid) {
                if (!record.isSSNEmpty()) {
                    if (record.studentSSN.matches(REGEX_ID)) {
                        record.additionalSSN = record.studentSSN.replaceFirst("(\\d{3})(\\d{2})(\\d{4})", "$1-$2-$3");
                    } else if (record.studentSSN.matches(REGEX_SSN_DASHES)) {
                        record.additionalSSN = record.studentSSN.replaceAll("\\D", "");
                    }

                    if (!StringUtils.isEmpty(record.additionalSSN)) {
                        studentSSNs.add(record.additionalSSN);
                    }
                    studentSSNs.add(record.studentSSN);
                }

                if (!record.isPINEmpty()) {
                    studentPINs.add(record.studentPIN);
                }
            }
        }

        Map<String, String> studentsBySSN = loadStudentsBySSN(studentSSNs);
        Map<String, String> studentsByPIN = loadStudentsByPIN(studentPINs);

        for (Record record : m_allRecords) {
            if (record.valid) {
                if (!record.isPINEmpty() && !record.isSSNEmpty()) {
                    record.message = MESSAGE_MULTIPLE_IDS;
                }

                String studentOid = null;
                if (!record.isSSNEmpty()) {
                    studentOid = studentsBySSN.get(record.studentSSN);
                    if (StringUtils.isEmpty(studentOid)) {
                        studentOid = studentsBySSN.get(record.additionalSSN);
                    }
                } else if (!record.isPINEmpty()) {
                    studentOid = studentsByPIN.get(record.studentPIN);
                }

                if (studentOid != null) {
                    record.studentOid = studentOid;
                    List<Record> records = m_recordsByStudentOid.get(record.studentOid);
                    if (records == null) {
                        records = new ArrayList<Record>();
                        m_recordsByStudentOid.put(record.studentOid, records);
                    }
                    records.add(record);

                } else {
                    record.valid = false;
                    record.message = String.format(MESSAGE_INVALID_STUDENT_ID, record.recordType, record.studentLasid);
                }
            }
        }
    }

    /**
     * Returns a map that maps student PIN to student oid.
     *
     * @param studentPINs
     *        list of all student PINS found in the import
     * @return a map of student PIN to student oid
     */
    private Map<String, String> loadStudentsByPIN(Collection<String> studentPINs) {
        return getStudentOidsByID(m_doePinField, studentPINs);
    }

    /**
     * Returns a map that maps student SSN to student oid.
     *
     * @param studentSSNs
     *        list of all student SSNs found in the import
     * @return a map of student SSN to student oid
     */
    private Map<String, String> loadStudentsBySSN(Collection<String> studentSSNs) {
        String idAttribute = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + Person.COL_PERSON_ID;
        return getStudentOidsByID(idAttribute, studentSSNs);
    }

    /**
     * Processes of all records found in the import file for the student matching the passed in
     * student oid. Once all
     * the records have been processed for the student, the SPED_STATUS for the student record is
     * updated to 'Active' if
     * there are still program participation records on going with a program code in the Disability'
     * or 'Options'
     * category. The student SPED_STATUS is set to 'Inactive' if there or no current program
     * participation records with
     * a program code in the Disability' or 'Options' category.
     *
     * @param studentOid String
     * @param records List<Record>
     */
    private void procesBatch(String studentOid, List<Record> records) {
        for (Record record : records) {
            try {
                if (record.valid) {
                    if (record.isCreate()) {
                        try {
                            createProgramParticipation(record);
                            incrementInsertCount();
                        } catch (RecordProcessingException exp) {
                            incrementMatchCount();
                        }
                    } else if (record.isUpdate()) {
                        if (commitChanges()) {
                            updateProgramParticipation(record);
                        }
                        incrementUpdateCount();
                    } else if (record.isDelete()) {
                        if (commitChanges()) {
                            deleteProgramParticipation(record);
                        }
                        incrementMatchCount();
                    }
                }
            } catch (RecordProcessingException e) {
                record.valid = false;
                record.message = e.getMessage();
            } catch (Exception e) {
                record.valid = false;
                record.message = String.format(MESSAGE_UNEXPECTED_ERROR, e.getMessage());
                AppGlobals.getLog().log(Level.WARNING, record.message, e);
            }

        }
        if (commitChanges()) {
            updateStudentSped(studentOid);
        }
    }

    /**
     * This method is called after all the records have been read in from the import file. This
     * method will process all
     * the records for a given student in a batch.
     */
    private void processAllRecords() {
        loadStudentOids();

        for (Entry<String, List<Record>> entry : m_recordsByStudentOid.entrySet()) {
            procesBatch(entry.getKey(), entry.getValue());
        }

        for (Record record : m_allRecords) {
            if (!record.valid) {
                incrementSkipCount();
            }

            if (!StringUtils.isEmpty(record.message)) {
                logInvalidRecord(record.line, record.message);
            }
        }
    }

    /**
     * Process all students that contain a current primary option program and set the primary option
     * field in the student.
     * Remove the primary option from the student when there is no corresponding primary option
     * program.
     */
    private void updateAllPrimaryOption() {
        Map<String, String> primaryOptions = loadPrimaryOptionStudents();

        // Process all students with current primary option
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(m_primaryOptionField, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                if (primaryOptions.containsKey(student.getOid())) {
                    student.setFieldValueByBeanPath(m_primaryOptionField, primaryOptions.get(student.getOid()));
                    if (student.isDirty() && commitChanges()) {
                        getBroker().saveBeanForced(student);
                    }
                    primaryOptions.remove(student.getOid());
                } else {
                    student.setFieldValueByBeanPath(m_primaryOptionField, null);
                    if (student.isDirty() && commitChanges()) {
                        getBroker().saveBeanForced(student);
                    }
                }
            }
        } finally {
            students.close();
        }

        for (Entry<String, String> entry : primaryOptions.entrySet()) {
            SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, entry.getKey());
            student.setFieldValueByBeanPath(m_primaryOptionField, entry.getValue());
            if (commitChanges()) {
                getBroker().saveBeanForced(student);
            }
        }

    }

    /**
     * Updates the end date for any student program participation records that match the record
     * passed in. If there are
     * no student program participation records that match the record passed in, a
     * RecordProcessingException is thrown.
     *
     * @param record Record
     * @throws RecordProcessingException if no matching records are found for updating
     */
    private void updateProgramParticipation(Record record) throws RecordProcessingException {
        int updateCount = 0;

        Criteria updateCriteria = getMatchingProgramCriteria(record);

        UpdateQuery updateQuery = new UpdateQuery(StudentProgramParticipation.class, updateCriteria,
                StudentProgramParticipation.COL_END_DATE, record.endDate);

        updateCount = getBroker().executeUpdateQuery(updateQuery);

        if (updateCount < 1) {
            throw new RecordProcessingException(String.format(MESSAGE_INVALID_REQUEST, record.recordType,
                    String.format(REASON_NO_MATCHING_RECORDS, record.studentLasid, record.recordId)));
        }
    }

    /**
     * Updates the student SPED_STATUS to 'Active' if there are still program participation records
     * on going with a
     * program code in the Disability' or 'Options' category. The student SPED_STATUS is set to
     * 'Inactive' if there or
     * no current program participation records with a program code in the Disability' or 'Options'
     * category
     *
     * @param studentOid String
     */
    private void updateStudentSped(String studentOid) {
        Criteria spedCriteria = getActiveSpedCriteria(studentOid);
        int spedCount = getProgramCount(spedCriteria);
        String spedStatus = (spedCount > 0) ? FLAG_ACTIVE : FLAG_INACTIVE;

        Criteria updateCriteria = new Criteria();
        updateCriteria.addEqualTo(X2BaseBean.COL_OID, studentOid);
        UpdateQuery updateQuery =
                new UpdateQuery(SisStudent.class, updateCriteria, SisStudent.COL_SPED_STATUS_CODE, spedStatus);

        getBroker().executeUpdateQuery(updateQuery);
    }

    /**
     * A Field represents a column in the import file.
     *
     * @author Follett Software Company
     */
    private enum Field {
        RECORD_ID, RECORD_VERSION, RECORD_TYPE, FILLER_1, DISTRICT_ID, SCHOOL_ID, SCHOOL_YEAR, STUDENT_SSN, STUDENT_PIN, STUDENT_LOCAL_KEY, DISABILITY_LEVEL, DISABILITY_CODE, DISABILITY_START_DATE, DISABILITY_END_DATE, STATE_STUDENT_ID, FILLER_2;

        /**
         * Returns a description of the Field.
         *
         * @return The description of the field to help identify it in the import file
         */
        public String getDescription() {
            String fieldName = WordUtils.capitalize(name().toLowerCase(), new char[] {'_'}).replace('_', ' ');
            return String.format(MESSAGE_FIELD_INFO, Integer.valueOf(ordinal() + 1), fieldName);
        }
    }

    /**
     * A record is the information extracted from a single row in the import file, and is used when
     * creating new student
     * program participation records.
     *
     * @author Follett Software Company
     */
    private class Record {
        String additionalSSN;
        String recordId;
        String programCode;
        String level;
        String studentLasid;
        String studentPIN;
        String studentOid;
        String studentSSN;
        PlainDate startDate;
        PlainDate endDate;
        String recordType;

        boolean valid;
        String message;
        int line;

        /**
         * Instantiates a new record.
         */
        public Record() {}

        /**
         * Returns true if the record represents a create request.
         *
         * @return true if RECORD_TYPE is 'N'
         */
        public boolean isCreate() {
            return recordType != null && recordType.equals(FLAG_RECORD_TYPE_NEW);
        }

        /**
         * Returns true if the record represents a delete request.
         *
         * @return true if RECORD_TYPE is 'D'
         */
        public boolean isDelete() {
            return recordType != null && recordType.equals(FLAG_RECORD_TYPE_DELETE);
        }

        /**
         * Returns true if the student PIN field has been set.
         *
         * @return true if student PIN has been set
         */
        public boolean isPINEmpty() {
            return studentPIN == null || studentPIN.isEmpty() || Pattern.matches(REGEX_ZERO_FILLED, studentPIN);
        }

        /**
         * Returns true if the student SSN field has been set.
         *
         * @return true if student SSN has been set
         */
        public boolean isSSNEmpty() {
            return studentSSN == null || studentSSN.isEmpty() || Pattern.matches(REGEX_ZERO_FILLED, studentSSN);
        }

        /**
         * Returns true if the record represents an update request.
         *
         * @return true if RECORD_TYPE is 'E'
         */
        public boolean isUpdate() {
            return recordType != null && recordType.equals(FLAG_RECORD_TYPE_EDIT);
        }
    }

    /**
     * Thrown to indicate that a record (line) in the import file could not be processed.
     *
     * @author Follett Software Company
     */
    private class RecordProcessingException extends RuntimeException {
        private static final long serialVersionUID = 1L;

        /**
         * Constructs a new exception with the specified detail message.
         *
         * @param message the detail message
         */
        public RecordProcessingException(String message) {
            super(message);
        }
    }
}
