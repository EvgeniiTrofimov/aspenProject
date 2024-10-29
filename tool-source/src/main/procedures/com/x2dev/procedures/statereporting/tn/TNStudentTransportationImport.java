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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentTransportation;
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
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNStudentTransportationImport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class TNStudentTransportationImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    /**
     * Constants
     */
    protected static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    protected static final String ALIAS_STR_BUS_AM = "DOE AM BUS";
    protected static final String ALIAS_STR_BUS_PM = "DOE PM BUS";
    protected static final String ALIAS_STR_EST_MILES = "DOE EST MILES TRANSPORTED";
    protected static final DateFormat[] DATE_FORMATS =
            {new SimpleDateFormat("MM/dd/yy"), new SimpleDateFormat("MM-dd-yy"), new SimpleDateFormat("MMddyy")};
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    protected static final String ERROR_EMPTY = " cannot be empty.";
    protected static final String HEADING_ORG_YEAR = "YEAR";
    protected static final String HEADING_SKL_STATE_ID = "SKLSTATEID";
    protected static final String HEADING_STR_BUS_AM = "AMBUS";
    protected static final String HEADING_STR_BUS_PM = "PMBUS";
    protected static final String HEADING_STR_DATE_END = "ENDDATE";
    protected static final String HEADING_STR_DATE_START = "STARTDATE";
    protected static final String HEADING_STD_LASID = "LASID";
    protected static final String HEADING_STR_MILES = "ESTMILESTRANSPORTED";
    protected static final String INPUT_PARAM_COMMIT = "commit";
    protected static final String INPUT_PARAM_SKIP_ROWS = "skipRows";
    protected static final String MILES_MATCH = "\\d+([.]\\d{0,2})?";

    /**
     * Class members
     */
    private ModelBroker m_broker;
    private boolean m_commitChanges;
    private DataDictionary m_dictionary;
    private List<String> m_errors = new LinkedList();
    private int m_fieldCount;
    private Map<String, Integer> m_headingIndexes = new HashMap();
    private List<String> m_setupErrors = new LinkedList();
    private int m_skipRows;
    private String m_sklStateId;
    private String m_strBusAM;
    private String m_strBusPM;
    private String m_strEstMiles;

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
        String mode = m_commitChanges ? "Commit Mode." : "Review Mode.";
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
            String strStartDateString = getRecordValue(record, HEADING_STR_DATE_START);
            String strEndDateString = getRecordValue(record, HEADING_STR_DATE_END);
            String stdLocalId = getRecordValue(record, HEADING_STD_LASID);
            String orgYear = getRecordValue(record, HEADING_ORG_YEAR);
            String sklStateId = getRecordValue(record, HEADING_SKL_STATE_ID);
            String strMiles = getRecordValue(record, HEADING_STR_MILES);
            String strAM = getRecordValue(record, HEADING_STR_BUS_AM);
            String strPM = getRecordValue(record, HEADING_STR_BUS_PM);
            boolean errorsFound = false;
            PlainDate strStartDate = null;
            DistrictSchoolYearContext ctx = null;
            PlainDate strEndDate = null;
            SisStudent student = null;
            SisSchool school = null;
            BigDecimal miles = null;
            StudentTransportation str = null;
            if (StringUtils.isEmpty(orgYear)) {
                errorsFound = addError(lineNumber, HEADING_ORG_YEAR + ERROR_EMPTY);
            } else {
                if (orgYear.matches("\\d{4}")) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(orgYear));
                    QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
                    ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
                    if (ctx == null) {
                        errorsFound = addError(lineNumber, "School year not found Year = " + orgYear);
                    }
                } else {
                    errorsFound = addError(lineNumber, HEADING_ORG_YEAR + " should be 4 digits numeric.");
                }
            }
            if (StringUtils.isEmpty(stdLocalId)) {
                errorsFound = addError(lineNumber, HEADING_STD_LASID + ERROR_EMPTY);
            } else {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SisStudent.COL_LOCAL_ID, stdLocalId);
                QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
                student = (SisStudent) getBroker().getBeanByQuery(query);
                if (student == null) {
                    errorsFound = addError(lineNumber, "Student not found lasid = " + stdLocalId);
                }
            }
            if (StringUtils.isEmpty(strStartDateString)) {
                errorsFound = addError(lineNumber, HEADING_STR_DATE_START + ERROR_EMPTY);
            } else {
                strStartDate = getStartEndDate(strStartDateString);
                if (strStartDate == null) {
                    errorsFound = addError(lineNumber, "Start date format is not valid.");
                } else if (ctx != null
                        && (ctx.getStartDate().after(strStartDate) || strStartDate.after(ctx.getEndDate()))) {
                    errorsFound = addError(lineNumber, "Start date can't be before school year start date.");
                }
            }
            if (!StringUtils.isEmpty(strEndDateString)) {
                strEndDate = getStartEndDate(strEndDateString);
                if (strEndDate == null) {
                    errorsFound = addError(lineNumber, "End date format is not valid.");
                } else {
                    if (strStartDate != null && strEndDate.before(strStartDate)) {
                        errorsFound = addError(lineNumber, "End date can't be before start date ");
                    }
                    if (ctx != null
                            && (ctx.getStartDate().after(strEndDate) || strEndDate.after(ctx.getEndDate()))) {
                        errorsFound = addError(lineNumber, "End date can't be before school year start date.");
                    }
                }
            }
            if (StringUtils.isEmpty(sklStateId)) {
                errorsFound = addError(lineNumber, HEADING_SKL_STATE_ID + ERROR_EMPTY);
            } else if (sklStateId.matches("\\d{0,4}")) {
                sklStateId = String.format("%04d", Integer.valueOf(sklStateId));
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(m_sklStateId, sklStateId);
                QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
                school = (SisSchool) getBroker().getBeanByQuery(query);
                if (school == null) {
                    errorsFound = addError(lineNumber, "School not found stateId = " + sklStateId);
                } else if (school.getArchiveIndicator()) {
                    errorsFound = addError(lineNumber, "School with stateId = " + sklStateId + " is archive.");
                } else if (school.getInactiveIndicator()) {
                    errorsFound = addError(lineNumber, "School with stateId = " + sklStateId + " is inactive.");
                }
            } else {
                errorsFound = addError(lineNumber,
                        HEADING_SKL_STATE_ID + " should be numeric with maximum length = 4 digits.");
            }
            if (!strMiles.isEmpty()) {
                if (strMiles.matches(MILES_MATCH)) {
                    miles = new BigDecimal(strMiles);
                    if (miles.compareTo(new BigDecimal(0.0)) != 1) {
                        miles = null;
                        errorsFound = addError(lineNumber, HEADING_STR_MILES + " cann't be negative or 0.");
                    }
                } else {
                    errorsFound = addError(lineNumber,
                            HEADING_STR_MILES + " should be numeric with maximum 2 decimal places.");
                }
            }
            if (!errorsFound) {
                if (isStrWithinSchoolEnrSpan(student, school.getOid(), ctx.getStartDate())) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentTransportation.COL_DISTRICT_CONTEXT_OID, ctx.getOid());
                    criteria.addEqualTo(StudentTransportation.COL_START_DATE, strStartDate);
                    criteria.addEqualTo(StudentTransportation.COL_STUDENT_OID, student.getOid());
                    criteria.addEqualTo(StudentTransportation.COL_SCHOOL_OID, school.getOid());
                    QueryByCriteria query = new QueryByCriteria(StudentTransportation.class, criteria);
                    str = (StudentTransportation) getBroker().getBeanByQuery(query);
                    if (str == null) {
                        str = X2BaseBean.newInstance(StudentTransportation.class, getBroker().getPersistenceKey());
                    }
                    str.setStudentOid(student.getOid());
                    str.setDistrictContextOid(ctx.getOid());
                    str.setSchoolOid(school.getOid());
                    str.setStartDate(strStartDate);
                    str.setEndDate(strEndDate);
                    str.setFieldValueByBeanPath(m_strBusAM, strAM);
                    str.setFieldValueByBeanPath(m_strBusPM, strPM);
                    str.setFieldValueByBeanPath(m_strEstMiles, miles.toString());
                    saveRecord(lineNumber, str);
                } else {
                    incrementSkipCount();
                    addError(lineNumber,
                            "Transportation Record must be within school year enrollment span for designated school.");
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
        m_skipRows = ((Integer) getParameter(INPUT_PARAM_SKIP_ROWS)).intValue();
        Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
        m_commitChanges = commit != null ? commit.booleanValue() : true; // default: commit & review
        m_sklStateId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_strBusAM = translateAliasToJavaName(ALIAS_STR_BUS_AM, true);
        m_strBusPM = translateAliasToJavaName(ALIAS_STR_BUS_PM, true);
        m_strEstMiles = translateAliasToJavaName(ALIAS_STR_EST_MILES, true);
        readInputHeading();
        if (m_setupErrors.isEmpty()) {
            if (!m_headingIndexes.containsKey(HEADING_ORG_YEAR) ||
                    !m_headingIndexes.containsKey(HEADING_ORG_YEAR) ||
                    !m_headingIndexes.containsKey(HEADING_SKL_STATE_ID) ||
                    !m_headingIndexes.containsKey(HEADING_STR_BUS_AM) ||
                    !m_headingIndexes.containsKey(HEADING_STR_BUS_PM) ||
                    !m_headingIndexes.containsKey(HEADING_STR_DATE_END) ||
                    !m_headingIndexes.containsKey(HEADING_STR_DATE_START) ||
                    !m_headingIndexes.containsKey(HEADING_STD_LASID) ||
                    !m_headingIndexes.containsKey(HEADING_STR_MILES)) {
                addSetupError("Incorrect Heading!", "Please check heading to contain: " +
                        HEADING_ORG_YEAR + ", " +
                        HEADING_SKL_STATE_ID + ", " +
                        HEADING_STR_BUS_AM + ", " +
                        HEADING_STR_BUS_PM + ", " +
                        HEADING_STR_DATE_END + ", " +
                        HEADING_STR_DATE_START + ", " +
                        HEADING_STD_LASID + ", " +
                        HEADING_STR_MILES + ".");
            }
        }
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
     * Checks if is str within school enr span.
     *
     * @param student SisStudent
     * @param schoolOid String
     * @param startDate PlainDate
     * @return true, if is str within school enr span
     */
    private boolean isStrWithinSchoolEnrSpan(SisStudent student,
                                             String schoolOid,
                                             PlainDate startDate) {
        boolean isEnrWithin = true;
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        enrCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, schoolOid);
        QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, enrCriteria);
        enrQuery.addOrderByDescending((StudentEnrollment.COL_ENROLLMENT_DATE));
        StudentEnrollment enrollment = (StudentEnrollment) getBroker().getBeanByQuery(enrQuery);
        if (enrollment == null
                || (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())
                        && !enrollment.getEnrollmentDate().after(startDate))) {
            isEnrWithin = false;
        }
        return isEnrWithin;
    }

    /**
     * Read a list of column headings from the input file.
     *
     * @return List
     */
    private List<String> readInputHeading() {
        // Set temporary field count - needed to use splitLine()
        m_fieldCount = 100;
        File sourceFile = (File) getParameter(FILE_KEY);
        List<String> record = new ArrayList();
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
            if (record != null) {
                for (String heading : record) {
                    m_headingIndexes.put(heading.toUpperCase(), Integer.valueOf(index));
                    ++index;
                }
                m_fieldCount = record.size();
            }
        } else {
            addSetupError("Setup Error", "Incorrect File Format");
        }
        return record;
    }

    /**
     * Save record.
     *
     * @param lineNumber int
     * @param str {@link StudentTransportation}
     */
    private void saveRecord(int lineNumber, StudentTransportation str) {
        if (str.isDirty()) {
            boolean isNew = str.isNew();
            if (isNew) {
                incrementInsertCount();
            } else {
                incrementUpdateCount();
            }
            if (m_commitChanges) {
                getBroker().saveBeanForced(str);
            }
        } else {
            addError(lineNumber,
                    "Nothing to Update!!!Transportation Record exists.");
            incrementSkipCount();
        }
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
