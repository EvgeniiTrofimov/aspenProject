/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.StudentDemoDatasets.DemoDataset;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois report data source for School Report Cards attendance data.
 *
 * @author X2 Development Corporation
 */
public class SchoolReportCardAtt extends StateReportData {
    /**
     * Entity class for School Report Cars attendance data.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolReportCardAttEntity extends StateReportEntity {
        /**
         *
         */
        private static final String ACTIVE_IEP_STATE_CODE_01 = "01";

        /**
         * SchoolReportCardAtt data.
         */
        SchoolReportCardAtt m_srcData = null;

        /**
         * Map of calculated values that are accessible to the retrievers.
         */
        private Map<String, Object> m_valuesMap;

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = (String) m_srcData.getSchool().getFieldValueByBeanPath(m_srcData.m_fieldSchoolCode);

            return school;
        }

        /**
         * Returns a calculated value from the values map by key.
         *
         * @param key String
         * @return Double
         */
        public Object getMembershipValue(String key) {
            return m_valuesMap.get(key);
        }

        /**
         * Initialize student, calculate membership and attendance counts.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;
            m_srcData = (SchoolReportCardAtt) data;
            String schoolCode = (String) m_srcData.getSchool().getFieldValueByBeanPath(m_srcData.m_fieldSchoolCode);
            Set<PlainDate> memberDates = new HashSet();

            if (!StringUtils.isEmpty(schoolCode)) {
                for (PlainDate date : getInSessionDates(student)) {
                    Collection<DemoDataset> datasets = m_srcData.m_ilExitDemoHelper.getDatasets(student, date);
                    if (datasets != null) {
                        for (DemoDataset dataset : datasets) {
                            if (schoolCode.equals(dataset.getServiceSchoolCode())
                                    && dataset.getFte().doubleValue() > .5) {
                                memberDates.add(date);
                            }
                        }
                    }
                }
            }



            // Calculate membership and attendance.
            if (!memberDates.isEmpty()) {
                HashMap<PlainDate, StudentAttendance> attendances = new HashMap();
                double membership = memberDates.size();

                List<StudentAttendance> items = m_srcData.m_helper.getStudentAttendances(student.getOid());
                if (items != null) {
                    for (StudentAttendance attendance : items) {
                        if (!"A".equals(m_srcData.lookupStateValue(SisSchool.class, SisSchool.COL_SCHOOL_TYPE_CODE,
                                attendance.getSchool().getSchoolTypeCode())) &&
                                attendance.getPortionAbsent() != null &&
                                attendance.getPortionAbsent().compareTo(BigDecimal.ZERO) > 0 &&
                                memberDates.contains(attendance.getDate())) {
                            attendances.put(attendance.getDate(), attendance);
                        }
                    }
                }

                double absent = 0.0;
                double truant = 0.0;
                for (Entry<PlainDate, StudentAttendance> entry : attendances.entrySet()) {
                    if (memberDates.contains(entry.getKey())) {
                        absent += entry.getValue().getPortionAbsent().doubleValue();
                        truant += entry.getValue().getExcusedIndicator() ? 0.0
                                : entry.getValue().getPortionAbsent().doubleValue();
                    }
                }
                // Put the membership into the values map.
                m_valuesMap = new HashMap<String, Object>(10);
                String raceCode = m_srcData.getRaceCode(student);
                boolean homelessInd = getHomelessIndicator(student);

                m_valuesMap.put(PARAM_MEMBERSHIP, Double.valueOf(membership));

                m_valuesMap.put(PARAM_ABSENT, Double.valueOf(absent));
                m_valuesMap.put(PARAM_ABSENT + "_" + student.getPerson().getGenderCode(), Double.valueOf(absent));
                m_valuesMap.put(PARAM_ABSENT + "_" + raceCode, Double.valueOf(absent));
                m_valuesMap.put(PARAM_ABSENT + POSTFIX_HOMELESS,
                        homelessInd ? Double.valueOf(absent) : Double.valueOf(0.0));

                m_valuesMap.put(PARAM_ATTENDANCE,
                        Double.valueOf((membership - absent) > 0 ? membership - absent : 0.0d));
                m_valuesMap.put(PARAM_ATTENDANCE + "_" + student.getPerson().getGenderCode(),
                        Double.valueOf((membership - absent) > 0 ? membership - absent : 0.0d));
                m_valuesMap.put(PARAM_ATTENDANCE + "_" + raceCode,
                        Double.valueOf((membership - absent) > 0 ? membership - absent : 0.0d));
                m_valuesMap.put(PARAM_ATTENDANCE + POSTFIX_HOMELESS,
                        Double.valueOf((membership - absent) > 0 && homelessInd ? membership - absent : 0.0d));

                m_valuesMap.put(PARAM_TRUANT, Double.valueOf(truant));
                m_valuesMap.put(PARAM_TRUANT_IND, Boolean.valueOf(truant >= 9.00));

                Collection<StudentProgramParticipation> ellPgms = m_srcData.getEllPgmsByStudent(student);
                Set<PlainDate> ellDates = getProgramDates(ellPgms, memberDates);
                Set<PlainDate> farmsDates = getFarmsDates(student, memberDates);

                double absEll = countAbsences(ellDates, attendances);
                m_valuesMap.put(PARAM_ABSENT + POSTFIX_ELL, Double.valueOf(absEll));
                m_valuesMap.put(PARAM_ATTENDANCE + POSTFIX_ELL, Double.valueOf((ellDates.size()) - absEll));

                double absFarms = countAbsences(farmsDates, attendances);
                m_valuesMap.put(PARAM_ABSENT + POSTFIX_FARMS, Double.valueOf(absFarms));
                m_valuesMap.put(PARAM_ATTENDANCE + POSTFIX_FARMS, Double.valueOf((farmsDates.size()) - absFarms));

                Set<PlainDate> iepDates = getIepDates(student, memberDates);
                double absIeps = countAbsences(iepDates, attendances);
                m_valuesMap.put(PARAM_ABSENT + POSTFIX_IEP, Double.valueOf(absIeps));
                m_valuesMap.put(PARAM_ATTENDANCE + POSTFIX_IEP, Double.valueOf(iepDates.size() - absIeps));

            } else {
                setRowCount(0);
            }
        }

        /**
         * Return the number of absences limited to days in the dates set.
         *
         * @param dates Set<PlainDate>
         * @param attendances HashMap<PlainDate,StudentAttendance>
         * @return double
         */
        private double countAbsences(Set<PlainDate> dates, HashMap<PlainDate, StudentAttendance> attendances) {
            double absent = 0.0;
            if (!dates.isEmpty()) {
                for (Entry<PlainDate, StudentAttendance> entry : attendances.entrySet()) {
                    if (dates.contains(entry.getKey())) {
                        absent += entry.getValue().getPortionAbsent().doubleValue();
                    }
                }
            }

            return absent;
        }

        /**
         * calculate farms dates
         * if school is farms or student is farms - retunr allMemberDates
         * otherwise return dates calculating by program span.
         *
         * @param student SisStudent
         * @param allMemberDates Set<PlainDate>
         * @return Sets the
         */
        private Set<PlainDate> getFarmsDates(SisStudent student, Set<PlainDate> allMemberDates) {

            Set<PlainDate> returnSet = null;
            School school = m_srcData.getSchool();
            String isSchoolFarms = (String) school.getFieldValueByBeanPath(m_srcData.m_fieldfarmsCode);
            if (BooleanAsStringConverter.TRUE.equals(isSchoolFarms)) {
                returnSet = new TreeSet<PlainDate>(allMemberDates);
            } else {
                Collection<StudentProgramParticipation> farmsPgms = m_srcData.getFarmsPgmsByStudent(student);

                if (farmsPgms != null && farmsPgms.size() > 0) {
                    returnSet = getProgramDates(farmsPgms, allMemberDates);
                } else {
                    String frlLowINcInd = (String) student.getFieldValueByBeanPath(m_srcData.m_fieldFrlLowInc);
                    if (BooleanAsStringConverter.TRUE.equals(frlLowINcInd)) {
                        returnSet = new TreeSet<PlainDate>(allMemberDates);
                    }
                }
            }
            return returnSet == null ? new TreeSet<PlainDate>() : returnSet;

        }

        /**
         * Gets the homeless indicator.
         *
         * @param student SisStudent
         * @return boolean
         */
        private boolean getHomelessIndicator(SisStudent student) {
            return m_srcData.m_homelessIndSet.contains(student.getOid());
        }

        /**
         * Return set of dates from the IEP limited to dates in parameter.
         *
         * @param student SisStudent
         * @param dates Set<PlainDate>
         * @return Sets the
         */
        private Set<PlainDate> getIepDates(SisStudent student, Set<PlainDate> dates) {
            Set<PlainDate> values = null;
            Collection<IepData> ieps = m_srcData.getIepsByStudent(student);
            if (ieps != null && !ieps.isEmpty()) {
                values = new HashSet();
                for (IepData iep : ieps) {
                    PlainDate startDate = iep.getStartDate();
                    PlainDate endDate =
                            iep.getEndDate() != null ? iep.getEndDate() : m_srcData.getCurrentContext().getEndDate();
                    for (PlainDate date : dates) {
                        if (!date.before(startDate) && !date.after(endDate)) {
                            values.add(date);
                        }
                    }
                }

            } else {
                String spedStatusCode = student.getSpedStatusCode();
                String spedStateStatus =
                        m_srcData.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE,
                                spedStatusCode, ReferenceMapTypeCode.STATE.ordinal());
                if (ACTIVE_IEP_STATE_CODE_01.equals(spedStateStatus)) {
                    values = new HashSet<PlainDate>(dates);
                }
            }
            return values == null ? new HashSet() : values;
        }

        /**
         * Gets the in session dates.
         *
         * @param student SisStudent
         * @return Sets the
         */
        private Set<PlainDate> getInSessionDates(SisStudent student) {
            Set<PlainDate> insessionDates =
                    m_srcData.m_helper.getCalendarDays((SisSchool) m_srcData.getSchool(), student.getCalendarCode());
            if (insessionDates == null) {
                SchoolCalendar calendar = m_srcData.m_mostCommonCalendars.get(m_srcData.getSchool().getOid());
                if (calendar != null) {
                    insessionDates = m_srcData.m_helper.getCalendarDays((SisSchool) m_srcData.getSchool(),
                            calendar.getCalendarId());
                }
            }
            if (insessionDates == null) {
                insessionDates = new HashSet();
                for (SchoolCalendarDate date : m_srcData.m_sklCalDates.get(m_srcData.getSchool().getOid())) {
                    insessionDates.add(date.getDate());
                }
            }
            return insessionDates;
        }

        /**
         * Return set of dates from the programs limited to dates in the parameter.
         *
         * @param pgms Collection<StudentProgramParticipation>
         * @param dates Set<PlainDate>
         * @return Sets the
         */
        private Set<PlainDate> getProgramDates(Collection<StudentProgramParticipation> pgms, Set<PlainDate> dates) {
            Set<PlainDate> values = new HashSet();
            if (pgms != null) {
                for (StudentProgramParticipation lepPgm : pgms) {
                    PlainDate startDate = lepPgm.getStartDate();
                    PlainDate endDate = lepPgm.getEndDate() != null ? lepPgm.getEndDate()
                            : m_srcData.getCurrentContext().getEndDate();
                    for (PlainDate date : dates) {
                        if (!date.before(startDate) && !date.after(endDate)) {
                            values.add(date);
                        }
                    }
                }
            }
            return values;
        }
    }

    /*
     * Aliases
     */
    public static final String ALIAS_FRL_LOW_INC = "DOE FRL LOW INC IND";
    public static final String ALIAS_FARMS_SKL = "DOE SCHOOL-WIDE FARMS";
    public static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    public static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    public static final String ALIAS_PGM_LUNCH = "DOE LUNCH STATUS";
    public static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    public static final String ALIAS_SCHOOL_NUMBER = "DOE SCHOOL ID";
    public static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";

    public static final String PARAM_ABSENT = "ABSENT";
    public static final String PARAM_ATTENDANCE = "ATTENDANCE";
    public static final String PARAM_MEMBERSHIP = "MEMBERSHIP";
    public static final String PARAM_REPORT_DATE = "reportDate";
    public static final String PARAM_TRUANT = "TRUANT";
    public static final String PARAM_TRUANT_IND = "TRUANT_IND";
    public static final String POSTFIX_ELL = "_LEP";
    public static final String POSTFIX_FARMS = "_FARMS";
    public static final String POSTFIX_HOMELESS = "_HMLS";
    public static final String POSTFIX_IEP = "_IEP";
    public static final String POSTFIX_MIGRANT = "_MIG";

    public static final String CODE_PROGRAM_ELL = "ELL";
    public static final String CODE_PROGRAM_FARMS = "FARMS";
    public static final String CODE_PROGRAM_FREE_REDUCED = "F";
    public static final String CODE_PROGRAM_HOMELESS = "HMLS";
    public static final List<String> PROGRAM_FARM_CODES = Arrays.asList("F", "R");

    /**
     * Instance variables.
     */
    protected Map<String, List<StudentProgramParticipation>> m_ellParticipationMap;
    protected String m_excludeSklField;
    protected Map<String, List<StudentProgramParticipation>> m_farmsMap;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolId;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldfarmsCode;
    protected String m_fieldFrlLowInc;
    protected StudentHistoryHelper m_helper;
    protected Set<String> m_homelessIndSet = new HashSet();
    protected Map<String, List<IepData>> m_iepMap;
    protected DemoExitDataHelper m_ilExitDemoHelper;
    protected Map<String, SchoolCalendar> m_mostCommonCalendars;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<SchoolCalendarDate>> m_sklCalDates;

    /**
     * Retrieve a calculated membership value from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembership implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            SchoolReportCardAttEntity srcEntity = (SchoolReportCardAttEntity) entity;

            return srcEntity.getMembershipValue(param);
        }
    }

    /**
     * Return the school ID of the selected school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            School school = data.getSchool();
            return school.getName();
        }
    }

    private static final String RACE_HISPANIC = "11";
    private static final String RACE_MULTIPLE = "17";

    /**
     * Initialize the export.
     * Prepare the StudentHistoryHelper and retrievers.
     */
    @Override
    public void initialize() {

        m_fieldFrlLowInc = translateAliasToJavaName(ALIAS_FRL_LOW_INC, true);
        m_fieldfarmsCode = translateAliasToJavaName(ALIAS_FARMS_SKL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);

        // Get user parameters.
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        initializeCtx();

        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);

        PlainDate startDate = null;
        PlainDate endDate = null;
        loadInSessionDatesMap();
        for (SchoolCalendarDate item : m_sklCalDates.get(getSchool().getOid())) {
            PlainDate date = item.getDate();
            if (startDate == null || startDate.after(date)) {
                startDate = date;
            }
            if (endDate == null || endDate.before(date)) {
                endDate = date;
            }
        }
        if (endDate.after(m_reportDate)) {
            endDate = m_reportDate;
        }

        m_ilExitDemoHelper = new DemoExitDataHelper(this);
        m_ilExitDemoHelper.setLastSubmissionDate(startDate);
        m_ilExitDemoHelper.setCurrentSubmissionDate(endDate);

        // Prepare the StudentHistoryHelper.
        m_helper = m_ilExitDemoHelper.getEnrollmentHelper();
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);

        m_ilExitDemoHelper.initializeDatasets();

        // Prepare the StateReportData.
        setQuery(m_helper.getStudentQuery(false));
        setEntityClass(SchoolReportCardAttEntity.class);

        loadHomelessIndicatorSet();
        loadEllPgmMap();
        loadIepMap();
        loadFarmsPgmMap();
        loadMostCommonCalendars();

        // Get race code reference codes for use in the race retriever.
        DataDictionaryField raceCodeField = getDataDictionaryField(Race.class, Race.COL_RACE_CODE);
        if (raceCodeField != null && !StringUtils.isEmpty(raceCodeField.getReferenceTableOid())) {
            m_raceCodes = getReferenceCodes(raceCodeField.getReferenceTableOid());
        }

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("SRC-ATT-MEMB", new RetrieveMembership());
        calcs.put("SRC-ATT-SCHOOL", new RetrieveSchool());
        super.addCalcs(calcs);
    }

    /**
     * Return list of ELL PGMs for the given Student.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentProgramParticipation> getEllPgmsByStudent(SisStudent student) {
        return m_ellParticipationMap.get(student.getOid());
    }

    /**
     * Return list of FARMS PGMs for the given Student.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentProgramParticipation> getFarmsPgmsByStudent(SisStudent student) {
        return m_farmsMap.get(student.getOid());
    }

    /**
     * Return list of IEPs for the given Student.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<IepData> getIepsByStudent(SisStudent student) {
        return m_iepMap.get(student.getOid());
    }

    /**
     * Retrieve an indicator of the student's race code. If the student has the Hispanic/Latino
     * indicator checked,
     * use the Hispanic state code. If the student has more than 1 race, use "Two or More Race"
     * state code. Otherwise, use the student's race's state code.
     * Return an indicator if the field parameter matches the calculated race code.
     *
     * @param student SisStudent
     * @return String
     */
    protected String getRaceCode(SisStudent student) {

        String raceCode = null;
        SisPerson person = student.getPerson();
        if (person != null && person.getHispanicLatinoIndicator()) {
            raceCode = RACE_HISPANIC;
        } else {
            Collection<Race> races = m_helper.getRaces(student);
            if (races != null) {
                if (races.size() > 1) {
                    raceCode = RACE_MULTIPLE;
                } else {
                    for (Race race : races) {
                        if (m_raceCodes.containsKey(race.getRaceCode())) {
                            ReferenceCode refCode = m_raceCodes.get(race.getRaceCode());
                            raceCode = refCode.getStateCode() != null ? refCode.getStateCode() : "";
                        }
                    }
                }
            }
        }

        return raceCode;// Boolean.valueOf(param.equals(raceCode));
    }

    /**
     * Set context by report date.
     */
    private void initializeCtx() {
        X2Criteria ctxCriteria = new X2Criteria();
        ctxCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        ctxCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, ctxCriteria));

        setCurrentContext(ctx);
    }

    /**
     * Initialize map of ELL programs keyed on student oid.
     */
    private void loadEllPgmMap() {
        // Load a map ELL student program participation records.
        // 1. Identify program codes for ell. These will have a state code of "ELL".
        List<String> programCode = new ArrayList();
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            programCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_PROGRAM_ELL);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programCriteria);
            programCode = (List<String>) getBroker().getSubQueryCollectionByQuery(query);
        }

        // 2. Query for student program participation for the selected students and program codes.
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        Criteria participationCriteria = new Criteria();
        participationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
        participationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

        X2Criteria ellPgmEndDatesCriteria = new X2Criteria();
        ellPgmEndDatesCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        X2Criteria ellPgmEndDatesOrCriteria = new X2Criteria();
        ellPgmEndDatesCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
        ellPgmEndDatesCriteria.addOrCriteria(ellPgmEndDatesOrCriteria);

        participationCriteria.addAndCriteria(ellPgmEndDatesOrCriteria);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_ellParticipationMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);
    }

    /**
     * Initialize map of FARMS programs keyed on student oid.
     */
    private void loadFarmsPgmMap() {
        // Load a map ELL student program participation records.
        // 1. Identify program codes for ell. These will have a state code of "ELL".
        List<String> programCode = new ArrayList();
        programCode.add("__dummy__");
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            programCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_PROGRAM_FARMS);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programCriteria);
            programCode = (List<String>) getBroker().getSubQueryCollectionByQuery(query);
        }

        String pgmLunchBeanPath = translateAliasToJavaName(ALIAS_PGM_LUNCH, true);

        // 2. Query for student program participation for the selected students and program codes.
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        Criteria participationCriteria = new Criteria();
        participationCriteria.addIn(pgmLunchBeanPath, PROGRAM_FARM_CODES);
        participationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
        participationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

        X2Criteria ellPgmEndDatesCriteria = new X2Criteria();
        ellPgmEndDatesCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        X2Criteria ellPgmEndDatesOrCriteria = new X2Criteria();
        ellPgmEndDatesCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
        ellPgmEndDatesCriteria.addOrCriteria(ellPgmEndDatesOrCriteria);

        participationCriteria.addAndCriteria(ellPgmEndDatesOrCriteria);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_farmsMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);
    }

    /**
     * Load homeless indicator set.
     */
    private void loadHomelessIndicatorSet() {
        // Query homeless student program participation records.
        // 1. Identify program codes for homeless. These will have a state code of "ELL".
        List<String> programCodes = new ArrayList();
        programCodes.add("__dummy__");
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            programCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_PROGRAM_HOMELESS);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programCriteria);
            programCodes.addAll(getBroker().getSubQueryCollectionByQuery(query));
        }

        // 2. Query for student program participation for the selected students and program codes.
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        Criteria participationCriteria = new Criteria();
        participationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
        participationCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        participationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());

        SubQuery participationQuery =
                new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                        participationCriteria);
        m_homelessIndSet.addAll(getBroker().getSubQueryCollectionByQuery(participationQuery));
    }

    /**
     * Initialize map of Ieps keyed on student oid.
     */
    private void loadIepMap() {
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        Criteria iepCriteria = new Criteria();

        iepCriteria.addIn(IepData.COL_STUDENT_OID, subQuery);
        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(StatusCode.ACTIVE.ordinal()));
        iepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDate);

        X2Criteria iepEndDatesCriteria = new X2Criteria();
        iepEndDatesCriteria.addIsNull(IepData.COL_END_DATE);

        X2Criteria iepEndDatesOrCriteria = new X2Criteria();
        iepEndDatesOrCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, getCurrentContext().getStartDate());
        iepEndDatesCriteria.addOrCriteria(iepEndDatesOrCriteria);

        iepCriteria.addAndCriteria(iepEndDatesCriteria);
        QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, iepCriteria);
        iepQuery.addOrderByDescending(IepData.COL_START_DATE);
        m_iepMap = getBroker().getGroupedCollectionByQuery(iepQuery, IepData.COL_STUDENT_OID, 512);
    }

    /**
     * Initialize map of map of school calendar dates. Inner map is keyed on calendar code, outer is
     * keyd on school oid.
     */
    private void loadInSessionDatesMap() {
        if (m_mostCommonCalendars == null) {
            loadMostCommonCalendars();
        }

        Criteria criteria = new Criteria();

        List<String> oids = new ArrayList();
        for (SchoolCalendar bean : m_mostCommonCalendars.values()) {
            oids.add(bean.getOid());
        }

        if (m_mostCommonCalendars.values().isEmpty()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + X2BaseBean.COL_OID, "__dummy__");
        } else {
            criteria.addIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + X2BaseBean.COL_OID, oids);
        }

        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, getCurrentContext().getEndDate());
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, getCurrentContext().getStartDate());

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        m_sklCalDates = getBroker().getGroupedCollectionByQuery(query,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                1024);
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    private void loadMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        m_mostCommonCalendars = new HashMap();

        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeStatus);

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!m_mostCommonCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        m_mostCommonCalendars.put(schoolOid, schoolCalendar);
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!m_mostCommonCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                m_mostCommonCalendars.put(oid, schoolCalendar);
            }
        }
    }

    /**
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        // Filter to eliminate unused schools.
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

        return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }
}
