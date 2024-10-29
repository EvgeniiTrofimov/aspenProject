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
package com.x2dev.reports.statereporting.wa;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Report class for WA Unexcused Absence Annual Report-report separated by 1-8 and 9-12 students.
 * Uses procedure with id EXPDATA-WA-UA.
 */
public class WAUnexcusedAbsenceReportData extends ReportJavaSourceNet {
    /**
     * Simple wrapper over Map for counting values.
     */
    protected static class OrganizationCounter {
        private Map<String, Integer> m_counter = new HashMap<String, Integer>();
        private WAUnexcusedAbsenceReportData m_report = null;
        private String m_orgName = null;
        private String m_gradeGroup = null;
        private String m_oid = null;

        /**
         * Instantiates a new organization counter.
         *
         * @param report WAUnexcusedAbsenceReportData
         * @param orgName String
         * @param gradeGroup String
         * @param oid String
         */
        public OrganizationCounter(WAUnexcusedAbsenceReportData report, String orgName, String gradeGroup, String oid) {
            m_report = report;
            m_orgName = orgName;
            m_gradeGroup = gradeGroup;
            m_oid = oid;
            initCounter();
        }

        /**
         * Gets the counter map.
         *
         * @return Map
         */
        public Map<String, Integer> getCounterMap() {
            return m_counter;
        }

        /**
         * Inits the counter.
         */
        public void initCounter() {
            for (String column : m_report.m_stdColumnList) {
                for (String group : m_report.m_stdGroupList) {
                    m_counter.put(group + column, Integer.valueOf(0));
                }
            }
            m_counter.put(KEY_DAYS, Integer.valueOf(0));
            m_counter.put(KEY_STUDENT, Integer.valueOf(0));
            m_counter.put(KEY_FIVE_ABSENCES, Integer.valueOf(0));
            m_counter.put(KEY_TEN_ABSENCES, Integer.valueOf(0));
            m_counter.put(KEY_TRUANCY_JUNE_AUG, Integer.valueOf(0));
            m_counter.put(KEY_TRUANCY_SEPT_MAY, Integer.valueOf(0));
        }

        /**
         * Update group.
         *
         * @param group String
         * @param ueAbsences Integer
         * @param aprilEnrollment Boolean
         * @param octoberEnrollment Boolean
         */
        public void updateGroup(String group,
                                Integer ueAbsences,
                                Boolean aprilEnrollment,
                                Boolean octoberEnrollment) {
            updateValue(group + KEY_ABSENCES, ueAbsences);
            if (aprilEnrollment.booleanValue()) {
                updateValue(group + KEY_APRIL, Integer.valueOf(1));
            }
            if (octoberEnrollment.booleanValue()) {
                updateValue(group + KEY_OCTOBER, Integer.valueOf(1));
            }
        }

        /**
         * Update value.
         *
         * @param key String
         * @param value Integer
         */
        public void updateValue(String key, Integer value) {
            if (value != null) {
                Integer curValue = m_counter.get(key);
                Integer newValue = Integer.valueOf(curValue.intValue() + value.intValue());
                m_counter.put(key, newValue);
            }
        }

        /**
         * Gets the.
         *
         * @param key String
         * @return Integer
         */
        public Integer get(String key) {
            return m_counter.get(key);
        }

        /**
         * Sets the.
         *
         * @param key String
         * @param value Integer
         */
        public void set(String key, Integer value) {
            m_counter.put(key, value);
        }

        /**
         * Gets the org name.
         *
         * @return String
         */
        public String getOrgName() {
            return m_orgName;
        }

        /**
         * Gets the grade group.
         *
         * @return String
         */
        public String getGradeGroup() {
            return m_gradeGroup;
        }

        /**
         * Gets the oid.
         *
         * @return String
         */
        public String getOid() {
            return m_oid;
        }
    }

    private static final String GRID_FIELD_COUNT_MAP = "orgCountMap";
    private static final String GRID_FIELD_ORG_NAME = "orgName";
    private static final String GRID_FIELD_GRADE_GROUP = "gradeGroup";
    // Input params
    private static final String INPARAM_PROCEDURE_ID = "procedureId";
    private static final String INPARAM_CONTEXT_YEAR = "contextYear";
    private static final String INPARAM_DELIMITER = "charDelimiter";
    private static final String INPARAM_UNEXCUSED_ABSENCES = "efUnexcusedAbsences";
    private static final String INPARAM_IS_SPED = "efIsSped";
    private static final String INPARAM_IS_ELL = "efIsEll";
    private static final String INPARAM_IS_LOW_INCOME = "efIsLowIncome";
    private static final String INPARAM_RACE_CODE = "efRaceCode";
    private static final String INPARAM_IS_FIVE_ABSENCES = "efIsFiveAbsences";
    private static final String INPARAM_IS_OCT_MEMBER = "efIsOctMember";
    private static final String INPARAM_IS_APR_MEMBER = "efIsAprMember";
    private static final String INPARAM_HIGH_SKL_GRADES = "highSchoolGrades";
    private static final String INPARAM_ONLY_DIST_SUMMARY = "onlySummary";
    private static final String INPARAM_TRUANCY_YEAR = "efTruancyYear";
    private static final String INPARAM_TRUANCY_SUMMER = "efTruancySummer";
    // Report params
    private static final String PARAM_REPORT_YEAR = "reportYear";
    private static final String PARAM_REPORT_DATE = "reportDate";

    private static final String KEY_ALL = "ALL";
    private static final String KEY_ABSENCES = "ABSENCES";
    private static final String KEY_APRIL = "APRIL";
    private static final String KEY_GRADE_9_12 = "9-12";
    private static final String KEY_GRADE_1_8 = "1-8";
    private static final String KEY_DAYS = "DAYS";
    private static final String KEY_FIVE_ABSENCES = "FIVE_ABSENCES";
    private static final String KEY_TEN_ABSENCES = "TEN_ABSENCES";
    private static final String KEY_SPED = "SPED";
    private static final String KEY_OCTOBER = "OCTOBER";
    private static final String KEY_ELL = "ELL";
    private static final String KEY_LOW_INCOME = "LI";
    private static final String KEY_STUDENT = "STUDENT";
    private static final String KEY_TRUANCY_SEPT_MAY = "truancySeptMay";
    private static final String KEY_TRUANCY_JUNE_AUG = "truancyJuneAug";

    private static final String RACE_CODE_ASIAN = "A";
    private static final String RACE_CODE_HISPANIC = "H";
    private static final String RACE_CODE_NATIVE = "N";
    private static final String RACE_CODE_BLACK = "B";
    private static final String RACE_CODE_PACIFIC = "P";
    private static final String RACE_CODE_WHITE = "W";
    private static final String RACE_CODE_TWO_OR_MORE = "T";
    private static final String RACE_CODE_NONE = "X";
    // ef - stands for Entity Field
    private String m_efUnexcusedAbsences;
    private String m_efIsSped;
    private String m_efIsEll;
    private String m_efIsLowIncome;
    private String m_efRaceCode;
    private String m_efIsFiveAbsences;
    private String m_efIsOctMember;
    private String m_efIsAprMember;
    private String m_efTruancyYear;
    private String m_efTruancySummer;

    private int m_efPosUnexcusedAbsences = -1;
    private int m_efPosIsSped = -1;
    private int m_efPosIsEll = -1;
    private int m_efPosIsLowIncome = -1;
    private int m_efPosRaceCode = -1;
    private int m_efPosIsFiveAbsences = -1;
    private int m_efPosIsOctMember = -1;
    private int m_efPosIsAprMember = -1;
    private int m_efPosTruancyYear = -1;
    private int m_efPosTruancySummer = -1;

    private Map<String, Integer> m_daysInSession = new HashMap<String, Integer>();
    private Map<String, Map<Integer, Integer>> m_schoolDaysCounts = new HashMap<String, Map<Integer, Integer>>();
    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;
    private List m_highSchoolGrades = null;
    private DistrictSchoolYearContext m_context = null;
    protected final List<String> m_stdGroupList = Arrays.asList(KEY_ALL, KEY_ELL, KEY_SPED,
            KEY_LOW_INCOME,
            RACE_CODE_ASIAN,
            RACE_CODE_BLACK,
            RACE_CODE_HISPANIC,
            RACE_CODE_NATIVE,
            RACE_CODE_PACIFIC,
            RACE_CODE_TWO_OR_MORE,
            RACE_CODE_WHITE,
            RACE_CODE_NONE);
    protected final List<String> m_stdColumnList = Arrays.asList(KEY_OCTOBER, KEY_APRIL,
            KEY_ABSENCES);

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        m_initErrors = new ArrayList<StateReportValidationError>();
        // Lookup State report source data procedure
        String procedureId = (String) getParameter(INPARAM_PROCEDURE_ID);
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if ((m_reportData != null) && (m_initErrors.size() == 0)) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in WABasicSupport";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
        if (m_initErrors.size() > 0) {
            StringBuilder str = new StringBuilder();
            for (StateReportValidationError error : m_initErrors) {
                str.append(error.getErrorMessage());
                str.append("\n");
            }
            throw new X2BaseException(new Exception(str.toString()));
        }
        m_efIsEll = (String) getParameter(INPARAM_IS_ELL);
        m_efIsLowIncome = (String) getParameter(INPARAM_IS_LOW_INCOME);
        m_efIsSped = (String) getParameter(INPARAM_IS_SPED);
        m_efUnexcusedAbsences = (String) getParameter(INPARAM_UNEXCUSED_ABSENCES);
        m_efRaceCode = (String) getParameter(INPARAM_RACE_CODE);
        m_efIsFiveAbsences = (String) getParameter(INPARAM_IS_FIVE_ABSENCES);
        m_efIsOctMember = (String) getParameter(INPARAM_IS_OCT_MEMBER);
        m_efIsAprMember = (String) getParameter(INPARAM_IS_APR_MEMBER);
        m_efTruancyYear = (String) getParameter(INPARAM_TRUANCY_YEAR);
        m_efTruancySummer = (String) getParameter(INPARAM_TRUANCY_SUMMER);

        String delimiter = (String) getParameter(INPARAM_DELIMITER);
        m_highSchoolGrades = StringUtils.convertDelimitedStringToList((String) getParameter(INPARAM_HIGH_SKL_GRADES),
                delimiter.charAt(0));
        // get enrollments sets for 1 of october and 1 of april
        String contextOid = (String) getParameter(INPARAM_CONTEXT_YEAR);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                contextOid);
        String year = String.valueOf(m_context.getSchoolYear() - 1) + "-" + String.valueOf(m_context.getSchoolYear());
        addParameter(PARAM_REPORT_YEAR, year);

        addParameter(PARAM_REPORT_DATE, new PlainDate());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        ReportDataGrid dataGrid = new ReportDataGrid();

        if ((m_reportData != null) && m_reportData.open() &&
                initializeEntityFieldPositions()) {
            try {
                Map<String, OrganizationCounter> schoolCounters = new HashMap<String, OrganizationCounter>();

                StateReportEntity entity = null;
                while ((entity = m_reportData.next()) != null) {
                    entity.preProcess();
                    proccessEntity(schoolCounters, entity);
                    entity.postProcess();
                }
                buildGrid(dataGrid, schoolCounters);
            } finally {
                m_reportData.close();
            }
        }
        dataGrid.beforeTop();
        dataGrid.sort(Arrays.asList(GRID_FIELD_ORG_NAME, GRID_FIELD_GRADE_GROUP), false);
        return dataGrid;
    }

    /**
     * Proccess entity.
     *
     * @param schoolCounters Map<String,OrganizationCounter>
     * @param entity StateReportEntity
     */
    private void proccessEntity(Map<String, OrganizationCounter> schoolCounters, StateReportEntity entity) {
        SisStudent student = (SisStudent) entity.getBean();
        SisSchool school = student.getSchool();
        Integer ueAbsences = Integer.valueOf(entity.getFieldValue(m_efPosUnexcusedAbsences));
        Boolean isFiveAbsences =
                Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsFiveAbsences)));
        Boolean aprilEnrollment =
                Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsAprMember)));
        Boolean octoberEnrollment =
                Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsOctMember)));
        String raceCode = entity.getFieldValue(m_efPosRaceCode);
        Boolean isEll = Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsEll)));
        Boolean isSped = Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsSped)));
        Boolean isLowIncome =
                Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosIsLowIncome)));
        Integer truancyYear = Integer.valueOf(entity.getFieldValue(m_efPosTruancyYear));
        Integer truancySummer = Integer.valueOf(entity.getFieldValue(m_efPosTruancySummer));

        OrganizationCounter schoolCounter = null;
        // getting appropriate counter for school and grade
        String gradeKey = m_highSchoolGrades.contains(student.getGradeLevel()) ? KEY_GRADE_9_12 : KEY_GRADE_1_8;
        schoolCounter = schoolCounters.get(school.getName() + gradeKey);
        if (schoolCounter == null) {
            schoolCounter = new OrganizationCounter(this, school.getName(), gradeKey, school.getOid());
            schoolCounters.put(school.getName() + gradeKey, schoolCounter);
        }
        // updating values in counter
        schoolCounter.updateGroup(KEY_ALL, ueAbsences, aprilEnrollment, octoberEnrollment);
        if (ueAbsences.intValue() > 0) {
            schoolCounter.updateValue(KEY_STUDENT, Integer.valueOf(1));
        }
        schoolCounter.updateGroup(raceCode, ueAbsences, aprilEnrollment, octoberEnrollment);
        if (isSped.booleanValue()) {
            schoolCounter.updateGroup(KEY_SPED, ueAbsences, aprilEnrollment, octoberEnrollment);
        }
        if (isEll.booleanValue()) {
            schoolCounter.updateGroup(KEY_ELL, ueAbsences, aprilEnrollment, octoberEnrollment);
        }
        if (isLowIncome.booleanValue()) {
            schoolCounter.updateGroup(KEY_LOW_INCOME, ueAbsences, aprilEnrollment, octoberEnrollment);
        }

        if (isFiveAbsences.booleanValue()) {
            schoolCounter.updateValue(KEY_FIVE_ABSENCES, Integer.valueOf(1));
        }
        if (ueAbsences.intValue() > 10) {
            schoolCounter.updateValue(KEY_TEN_ABSENCES, Integer.valueOf(1));
        }
        schoolCounter.updateValue(KEY_TRUANCY_JUNE_AUG, truancySummer);
        schoolCounter.updateValue(KEY_TRUANCY_SEPT_MAY, truancyYear);

        updateDaysInSessionCounts(school.getOid(), student.getCalendarCode());
    }

    /**
     * "Transfer" calculated values from OrganizationCounter to DataGrid .
     *
     * @param schoolGrid ReportDataGrid
     * @param schoolCounters Map<String,OrganizationCounter>
     */
    private void buildGrid(ReportDataGrid schoolGrid, Map<String, OrganizationCounter> schoolCounters) {
        Boolean onlySummary = (Boolean) getParameter(INPARAM_ONLY_DIST_SUMMARY);
        if (isSchoolContext()) {
            for (OrganizationCounter schoolCounter : schoolCounters.values()) {
                if (schoolCounter.get(KEY_DAYS).intValue() == 0) {
                    schoolCounter.set(KEY_DAYS, getInSessionCount(schoolCounter.getOid()));
                }
                schoolGrid.append();
                schoolGrid.set(GRID_FIELD_ORG_NAME, schoolCounter.getOrgName());
                schoolGrid.set(GRID_FIELD_GRADE_GROUP, schoolCounter.getGradeGroup());
                schoolGrid.set(GRID_FIELD_COUNT_MAP, schoolCounter.getCounterMap());
            }
        } else {
            Map<String, OrganizationCounter> districtCounters = buildDistrictCounter(schoolCounters);

            for (OrganizationCounter districtCounter : districtCounters.values()) {
                if (districtCounter.get(KEY_DAYS).intValue() == 0) {
                    districtCounter.set(KEY_DAYS, getInSessionCount(null));
                }
                schoolGrid.append();
                schoolGrid.set(GRID_FIELD_ORG_NAME, districtCounter.getOrgName());
                schoolGrid.set(GRID_FIELD_COUNT_MAP, districtCounter.getCounterMap());
                schoolGrid.set(GRID_FIELD_GRADE_GROUP, districtCounter.getGradeGroup());
            }
            if (!onlySummary.booleanValue()) {
                for (OrganizationCounter schoolCounter : schoolCounters.values()) {
                    if (schoolCounter.get(KEY_DAYS).intValue() == 0) {
                        schoolCounter.set(KEY_DAYS, getInSessionCount(schoolCounter.getOid()));
                    }
                    schoolGrid.append();
                    schoolGrid.set(GRID_FIELD_ORG_NAME, schoolCounter.getOrgName());
                    schoolGrid.set(GRID_FIELD_GRADE_GROUP, schoolCounter.getGradeGroup());
                    schoolGrid.set(GRID_FIELD_COUNT_MAP, schoolCounter.getCounterMap());
                }
            }
        }
    }

    /**
     * Summarize values for whole district.
     *
     * @param schoolCounters Map<String,OrganizationCounter>
     * @return Map
     */
    private Map<String, OrganizationCounter> buildDistrictCounter(Map<String, OrganizationCounter> schoolCounters) {
        Map<String, OrganizationCounter> districtCounters = new HashMap<String, OrganizationCounter>();
        for (OrganizationCounter schoolCounter : schoolCounters.values()) {
            OrganizationCounter districtCounter = districtCounters.get(schoolCounter.getGradeGroup());
            if (districtCounter == null) {
                districtCounter =
                        new OrganizationCounter(this, getOrganization().getName(), schoolCounter.getGradeGroup(), null);
                districtCounters.put(schoolCounter.getGradeGroup(), districtCounter);
            }
            // main chart values
            for (String column : m_stdColumnList) {
                for (String group : m_stdGroupList) {
                    String key = group + column;
                    Integer schoolValue = schoolCounter.get(key);
                    districtCounter.updateValue(key, schoolValue);
                }
            }
            // header values
            Integer schoolValue = schoolCounter.get(KEY_STUDENT);
            districtCounter.updateValue(KEY_STUDENT, schoolValue);

            schoolValue = schoolCounter.get(KEY_FIVE_ABSENCES);
            districtCounter.updateValue(KEY_FIVE_ABSENCES, schoolValue);

            schoolValue = schoolCounter.get(KEY_TEN_ABSENCES);
            districtCounter.updateValue(KEY_TEN_ABSENCES, schoolValue);

            schoolValue = schoolCounter.get(KEY_TRUANCY_JUNE_AUG);
            districtCounter.updateValue(KEY_TRUANCY_JUNE_AUG, schoolValue);

            schoolValue = schoolCounter.get(KEY_TRUANCY_SEPT_MAY);
            districtCounter.updateValue(KEY_TRUANCY_SEPT_MAY, schoolValue);

            // We will take largest number of daysInSession for district.
            // Not sure that daysInSession same for all school in district.
            schoolValue = schoolCounter.get(KEY_DAYS);
            Integer districtValue = districtCounter.get(KEY_DAYS);
            if (districtValue.intValue() < schoolValue.intValue()) {
                districtCounter.set(KEY_DAYS, schoolValue);
            }
        }
        return districtCounters;
    }

    /**
     * Gets the in session count.
     *
     * @param schoolOid String
     * @return Integer
     */
    private Integer getInSessionCount(String schoolOid) {
        Integer retValue = null;
        Map<Integer, Integer> daysCounts = new HashMap<Integer, Integer>();
        if (schoolOid == null) {
            for (Entry<String, Map<Integer, Integer>> schoolEntry : m_schoolDaysCounts.entrySet()) {
                Map<Integer, Integer> sklCounts = schoolEntry.getValue();
                for (Entry<Integer, Integer> entry : sklCounts.entrySet()) {
                    Integer currValue = daysCounts.get(entry.getKey());
                    if (currValue == null) {
                        currValue = Integer.valueOf(0);
                    }
                    daysCounts.put(entry.getKey(), Integer.valueOf(currValue.intValue() + entry.getValue().intValue()));
                }
            }
        } else {
            daysCounts = m_schoolDaysCounts.get(schoolOid);
        }
        if (daysCounts != null) {
            int maxCount = 0;
            for (Entry<Integer, Integer> entry : daysCounts.entrySet()) {
                if (entry.getValue().intValue() > maxCount) {
                    maxCount = entry.getValue().intValue();
                    retValue = entry.getKey();
                }
            }
        }
        return retValue;
    }

    /**
     * Initialize entity field positions.
     *
     * @return true, if successful
     */
    private boolean initializeEntityFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();

            if (m_efIsEll.equals(fieldName)) {
                m_efPosIsEll = pos;
            } else if (m_efIsLowIncome.equals(fieldName)) {
                m_efPosIsLowIncome = pos;
            } else if (m_efIsSped.equals(fieldName)) {
                m_efPosIsSped = pos;
            } else if (m_efUnexcusedAbsences.equals(fieldName)) {
                m_efPosUnexcusedAbsences = pos;
            } else if (m_efRaceCode.equals(fieldName)) {
                m_efPosRaceCode = pos;
            } else if (m_efIsFiveAbsences.equals(fieldName)) {
                m_efPosIsFiveAbsences = pos;
            } else if (m_efIsOctMember.equals(fieldName)) {
                m_efPosIsOctMember = pos;
            } else if (m_efIsAprMember.equals(fieldName)) {
                m_efPosIsAprMember = pos;
            } else if (m_efTruancyYear.equals(fieldName)) {
                m_efPosTruancyYear = pos;
            } else if (m_efTruancySummer.equals(fieldName)) {
                m_efPosTruancySummer = pos;
            }
        }

        if ((m_efPosIsEll < 0) ||
                (m_efPosIsLowIncome < 0) ||
                (m_efPosIsSped < 0) ||
                (m_efPosUnexcusedAbsences < 0) ||
                (m_efPosRaceCode < 0) ||
                (m_efPosIsFiveAbsences < 0) ||
                (m_efPosIsOctMember < 0) ||
                (m_efPosIsAprMember < 0) ||
                (m_efPosTruancyYear < 0) ||
                (m_efPosTruancySummer < 0)) {
            return false;
        }
        return true;
    }

    /**
     * Update days in session counts.
     *
     * @param schoolOid String
     * @param calendarCode String
     */
    private void updateDaysInSessionCounts(String schoolOid, String calendarCode) {
        Integer daysInSession = m_daysInSession.get(schoolOid + calendarCode);
        if (daysInSession == null) {
            Calendar calendar = new GregorianCalendar();
            calendar.set(m_context.getSchoolYear(), Calendar.MAY, 31);
            daysInSession = Integer.valueOf(CalendarManager.getInSessionDates(m_context.getStartDate(),
                    new PlainDate(calendar.getTime()),
                    schoolOid, calendarCode,
                    getBroker()).size());
            m_daysInSession.put(schoolOid + calendarCode, daysInSession);
        }
        Map<Integer, Integer> daysCounts = m_schoolDaysCounts.get(schoolOid);
        if (daysCounts == null) {
            daysCounts = new HashMap<Integer, Integer>();
            m_schoolDaysCounts.put(schoolOid, daysCounts);
        }
        Integer currValue = daysCounts.get(daysInSession);
        if (currValue == null) {
            currValue = Integer.valueOf(0);
        }
        daysCounts.put(daysInSession, Integer.valueOf(currValue.intValue() + 1));
    }

}
