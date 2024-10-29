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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNStudentProgramImportProcess.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class TNStudentProgramImportProcess extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static final String HEADING_CODE = "CODE";
    public static final String HEADING_EIS_ID = "EIS STATE ID";
    public static final String HEADING_END_DATE = "END DATE";
    public static final String HEADING_LASID = "LASID";
    public static final String HEADING_PIN = "PIN";
    public static final String HEADING_SSN = "SSN";
    public static final String HEADING_START_DATE = "START DATE";
    private static final String PARAM_COMMIT = "commit";
    private static final String PARAM_CONTEXT_OID = "districtContextOid";
    public static final String PARAM_SKIP_ROWS = "skipRows";

    private static final String ALIAS_DOE_PIN = "DOE PIN";
    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_PSN_ARCHIVE_SSN = "all-psn-archiveSSN";
    // Make sure the date formats are in preferred parsing order.
    private static final DateFormat[] DATE_FORMATS =
            {new SimpleDateFormat("MM/dd/yy"), new SimpleDateFormat("MM-dd-yy"), new SimpleDateFormat("MMddyy")};
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";


    protected String m_fieldPsnSSN;
    protected String m_studentPin;
    protected String m_studentEIS;

    private ModelBroker m_broker;
    private boolean m_commit;
    private DataDictionary m_dictionary;
    private List<String> m_errors = new LinkedList();
    private int m_fieldCount;
    private Map<String, Integer> m_headingIndexes = new HashMap();
    private DistrictSchoolYearContext m_selectedContext;

    /**
     * A list of errors encountered during initialization. Accessible through getSetupErrors().
     */
    private List<String> m_setupErrors = new LinkedList();

    private int m_skipRows;

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
     * Display only init errors if exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        String mode = m_commit ? "Commit Mode." : "Review Mode.";
        buffer.append(mode);
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(getImportStatistics().toString());
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
        if (m_setupErrors.isEmpty()) {
            if (!m_errors.isEmpty()) {
                m_errors.add("\n");
                exportList(m_errors);
            }
        } else {
            exportList(m_setupErrors);
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
        return m_fieldCount;
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

        if (lineNumber > m_skipRows) {
            String ssn = stripNonNumericChars(getRecordValue(record, HEADING_SSN));
            String programCode = getRecordValue(record, HEADING_CODE);
            String startDateString = getRecordValue(record, HEADING_START_DATE);
            String endDateString = getRecordValue(record, HEADING_END_DATE);
            String studentPin = getRecordValue(record, HEADING_PIN);
            String localId = getRecordValue(record, HEADING_LASID);
            String eisStateId = stripNonNumericChars(getRecordValue(record, HEADING_EIS_ID));

            // validate dates
            boolean errorsFound = false;
            PlainDate startDate = null;
            PlainDate endDate = null;
            if (StringUtils.isEmpty(programCode)) {
                errorsFound = addError(lineNumber, "Program code cannot be empty ");
            }
            if (StringUtils.isEmpty(startDateString)) {
                errorsFound = addError(lineNumber, "Start date cannot be empty ");
            } else {
                startDate = getStartEndDate(startDateString);
                if (startDate == null) {
                    errorsFound = addError(lineNumber, "Start date format is not valid ");
                }
            }
            if (!StringUtils.isEmpty(endDateString)) {
                endDate = getStartEndDate(endDateString);
                if (endDate == null) {
                    errorsFound = addError(lineNumber, "End date format is not valid ");
                } else if (startDate != null && endDate.before(startDate)) {
                    errorsFound = addError(lineNumber, "End date cannot be before start date ");
                }
            }

            X2Criteria criteria = new X2Criteria();
            if (!StringUtils.isEmpty(localId)) {
                criteria.addEqualTo(SisStudent.COL_LOCAL_ID, localId);
            } else if (!StringUtils.isEmpty(eisStateId)) {
                criteria.addEqualTo(m_studentEIS, eisStateId);
            } else if (!StringUtils.isEmpty(studentPin)) {
                if (!StringUtils.isEmpty(ssn)) {
                    errorsFound = addError(lineNumber, "Students matched by Student Pin must not have SSN");
                }
                criteria.addEqualTo(m_studentPin, studentPin);
            } else if (!StringUtils.isEmpty(ssn)) {
                criteria.addEqualTo(SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + m_fieldPsnSSN,
                        ssn);
            } else {
                criteria.addEqualTo(X2BaseBean.COL_OID, "--NO MATCH--");
                errorsFound = addError(lineNumber, "No suitable student identification method found");
            }

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);

            if (student == null) {
                errorsFound = addError(lineNumber, "Student not found lasid = " + localId + ", pin = " + studentPin
                        + ", EIS StateId = " + eisStateId + ", SSN = " + ssn);
            } else if (!isEnrolledForSelectedCtx(student)) {
                errorsFound = addError(lineNumber,
                        "Student is not enrolled for the selected school year. Lasid = " + localId + ", Pin = "
                                + studentPin
                                + ", EIS StateId = " + eisStateId + ", SSN = " + ssn);
            }
            if (!errorsFound) {
                boolean isFound = false;
                for (StudentProgramParticipation prgm : student.getProgramParticipation()) {
                    if ((programCode.equals(prgm.getProgramCode()))) {
                        // code match
                        if ((startDate.equals(prgm.getStartDate()))) {
                            // record match
                            if (endDate != null) {
                                if (prgm.getEndDate() == null) {
                                    prgm.setEndDate(endDate);
                                    saveRecord(lineNumber, prgm);
                                } else if (!endDate.equals(prgm.getEndDate())) {
                                    errorsFound = addError(lineNumber,
                                            "End date in file[" + endDate + "] does not match Aspen["
                                                    + prgm.getEndDate() + "]" + prgm.getProgramCode() + " for "
                                                    + student.getNameView());
                                    incrementSkipCount();
                                } else {
                                    incrementMatchCount();
                                }
                            } else {
                                incrementSkipCount();
                            }
                            isFound = true;
                            break;
                        }
                        // check for overlapping range
                        if (prgm.getStartDate() != null) {
                            if (prgm.getStartDate().before(startDate)) { // program before new
                                                                         // program
                                if (prgm.getEndDate() == null || !prgm.getEndDate().before(startDate)) {
                                    errorsFound = addError(lineNumber, "Program " + prgm.getProgramCode()
                                            + " exists for " + student.getNameView());
                                    incrementSkipCount();
                                    break;
                                }
                            } else { // program on or after new program
                                if (endDate == null || !endDate.before(prgm.getStartDate())) {
                                    errorsFound = addError(lineNumber, "Program " + prgm.getProgramCode()
                                            + " exists for " + student.getNameView());
                                    incrementSkipCount();
                                    break;
                                }
                            }
                        }
                    }
                }
                if (!isFound && !errorsFound) {
                    StudentProgramParticipation stPrgm =
                            X2BaseBean.newInstance(StudentProgramParticipation.class, getBroker().getPersistenceKey());
                    stPrgm.setStudentOid(student.getOid());
                    stPrgm.setProgramCode(programCode);
                    stPrgm.setStartDate(startDate);
                    stPrgm.setEndDate(endDate);
                    saveRecord(lineNumber, stPrgm);

                }
            } else {
                incrementSkipCount();
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
        m_skipRows = ((Integer) getParameter(PARAM_SKIP_ROWS)).intValue();
        // Gets the input parameters
        String m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_selectedContext =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, m_contextOid);
        Boolean commit = (Boolean) getParameter(PARAM_COMMIT);
        m_commit = commit != null ? commit.booleanValue() : true; // default: commit & review
        m_studentPin = translateAliasToJavaName(ALIAS_DOE_PIN, true);
        m_studentEIS = translateAliasToJavaName(ALIAS_EIS_STATE_ID, true);
        m_fieldPsnSSN = translateAliasToJavaName(ALIAS_PSN_ARCHIVE_SSN, true);

        readInputHeading();
    }

    /**
     * Adds one validation error.
     *
     * @param lineNumber int
     * @param errorMessage String
     * @return true, if successful
     */
    private boolean addError(int lineNumber, String errorMessage) {
        m_errors.add("" + lineNumber + ". " + errorMessage);
        return true;
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
        buffer.append('\n');
        if (!list.isEmpty()) {
            buffer.append("Errors:" + '\n');
        }
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
     * Gets the data dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Gets the record value.
     *
     * @param record List<String>
     * @param heading String
     * @return String
     */
    private String getRecordValue(List<String> record, String heading) {
        String value = null;
        Integer index = m_headingIndexes.get(heading);
        if (index != null) {
            value = record.get(index.intValue());
        }
        return value;
    }

    /**
     * Gets the start end date.
     *
     * @param inputdate String
     * @return Plain date
     */
    private PlainDate getStartEndDate(String inputdate) {
        PlainDate plainDate = null;
        if (!StringUtils.isEmpty(inputdate)) {
            Date date = null;
            for (DateFormat format : DATE_FORMATS) {
                try {
                    date = format.parse(inputdate);
                    break;
                } catch (ParseException e) {
                    // try next format
                }
            }
            if (date != null) {
                plainDate = new PlainDate(date);
            }
        }
        return plainDate;
    }

    /**
     * Validate date range.
     */
    private boolean isEnrolledForSelectedCtx(SisStudent std) {
        boolean isEnrolled = true;
        Collection<StudentEnrollment> enrs = std.getEnrollments();
        StudentEnrollment entryEnr = null;
        StudentEnrollment exitEnr = null;
        for (StudentEnrollment enr : enrs) {
            if (!enr.getEnrollmentDate().after(m_selectedContext.getEndDate())) {
                if (StudentEnrollment.WITHDRAWAL.equals(enr.getEnrollmentType())
                        && (exitEnr == null || exitEnr.getEnrollmentDate().before(enr.getEnrollmentDate()))) {
                    exitEnr = enr;
                }
                if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType())
                        && (entryEnr == null || entryEnr.getEnrollmentDate().before(enr.getEnrollmentDate()))) {
                    entryEnr = enr;
                }
            }
        }
        if (entryEnr != null && exitEnr != null && entryEnr.getEnrollmentDate().before(exitEnr.getEnrollmentDate())
                && exitEnr.getEnrollmentDate().before(m_selectedContext.getStartDate())) {
            isEnrolled = false;
        }
        return isEnrolled;
    }

    /**
     * Read a list of column headings from the input file.
     *
     * @return List
     */
    private List<String> readInputHeading() {
        // Set temporary field count - needed to use splitLine()
        m_fieldCount = 100;
        List<String> record = new ArrayList();
        File sourceFile = (File) getParameter(FILE_KEY);
        if (sourceFile != null && sourceFile.getAbsolutePath().endsWith(".csv")) {
            setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
            setUseValueDelimiters(true);
            setValueWrapper('"');

            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new FileReader(sourceFile), 4096);
                String line = reader.readLine();
                record = splitLine(line, 1);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException e) {
                        // Do nothing
                    }
                }
            }
            int index = 0;
            for (String heading : record) {
                m_headingIndexes.put(heading.toUpperCase(), Integer.valueOf(index));
                ++index;
            }

            m_fieldCount = record.size();
        } else {
            addSetupError("Setup Error", "Incorrect File Format");
        }
        return record;
    }

    /**
     * Save record.
     *
     * @param lineNumber int
     * @param studentPrgm StudentProgramParticipation
     */
    private void saveRecord(int lineNumber, StudentProgramParticipation studentPrgm) {
        if (studentPrgm.isDirty()) {
            boolean isNew = studentPrgm.isNew();
            List<ValidationError> errors = null;
            if (m_commit) {
                errors = getBroker().saveBean(studentPrgm);
            }
            if (errors == null || errors.isEmpty()) {
                if (isNew) {
                    incrementInsertCount();
                } else {
                    incrementUpdateCount();
                }
            } else {
                for (ValidationError error : errors) {
                    addError(lineNumber, "Validation Error: " + error.toString());
                }
            }
        } else {
            incrementMatchCount();
        }
    }

    /**
     * Returns numeric values without invalid characters.
     *
     * @param value String
     * @return String
     */
    private String stripNonNumericChars(String value) {
        String result = null;
        if (!StringUtils.isEmpty(value)) {
            result = value.replaceAll("[^0-9]", "");
        }
        return result;
    }


    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return javaName;
    }
}
