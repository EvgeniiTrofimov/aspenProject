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
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: Habitual Truant export.
 * This class implements the data export for MD Habitual Truant export.
 *
 * @author X2 Development Corporation
 */
public class HabitualTruant extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD Habitual Truant export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class HabitualTruantEntity extends StateReportEntity {
        /**
         * List of school/district records to report.
         */
        private List<SchoolInfo> m_schools = new ArrayList<SchoolInfo>(24);

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public HabitualTruantEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolInfo info = m_schools.get(getCurrentRow());
            String name = "SCHOOL: " + info.m_schoolCode;

            return name;
        }

        /**
         * Returns the SchoolInfo for the current row.
         *
         * @return SchoolInfo
         */
        public SchoolInfo getCurrentSchoolInfo() {
            return m_schools.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            HabitualTruant htData = (HabitualTruant) data;

            Map<String, Float> stdDistMap = new HashMap<String, Float>();

            // Fields for tracking student totals, membership, attendance.
            SchoolInfo schoolInfo = null;
            String lastStdOid = null;
            String lastStdSklOid = null;
            float lastStdAbsenses = 0;

            // Identify appropriate attendance records. Based on attendance reason code with state
            // value of 020, 021.
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(data.getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(StudentAttendance.class.getName(),
                    StudentAttendance.COL_REASON_CODE);
            List<String> attendanceReasons = new ArrayList<String>();
            if (field != null) {
                ReferenceTable refTable = field.getReferenceTable();
                if (refTable != null) {
                    for (ReferenceCode code : refTable.getReferenceCodes()) {
                        if ("020".equals(code.getStateCode()) || "021".equals(code.getStateCode())) {
                            attendanceReasons.add(code.getCode());
                        }
                    }
                }
            }
            // If appropriate codes are found, use them to identify attendance.
            // If not, use absent and excused indicator.
            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, htData.m_beginDate);
            criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, htData.m_endDate);
            if (attendanceReasons.size() > 0) {
                criteria.addIn(StudentAttendance.COL_REASON_CODE, attendanceReasons);
            } else {
                criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.TRUE);
            }
            // Order by school and student to group students together by school enrollment ranges.
            BeanQuery query = new BeanQuery(StudentAttendance.class, criteria, false);
            query.addOrderBy(StudentAttendance.COL_SCHOOL_OID, true);
            query.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);

            QueryIterator iterator = data.getBroker().getIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    StudentAttendance att = (StudentAttendance) iterator.next();

                    // Increment the student at district level.
                    Float stdDstCnt = stdDistMap.get(att.getStudentOid());
                    if (stdDstCnt == null) {
                        stdDistMap.put(att.getStudentOid(),
                                Float.valueOf(att.getPortionAbsent() == null ? 0.0f : att.getPortionAbsent().floatValue()));
                    } else {
                        stdDistMap.put(att.getStudentOid(), Float.valueOf(stdDstCnt.floatValue()
                                + (att.getPortionAbsent() == null ? 0.0f : att.getPortionAbsent().floatValue())));
                    }

                    // Check for student change in school.
                    if (att.getStudentOid().equals(lastStdOid)) {
                        lastStdAbsenses +=
                                (att.getPortionAbsent() == null ? 0.0f : att.getPortionAbsent().floatValue());
                    } else {
                        if (lastStdOid != null) {
                            // Identify the reporting school for the current student.
                            String stdSklCode = getOverrideSchoolCode(att.getStudent());
                            if (StringUtils.isEmpty(stdSklCode)) {
                                stdSklCode = getSchoolCode(att.getSchool());
                            }

                            // Find the schoolInfo for the student.
                            schoolInfo = null;
                            for (SchoolInfo info : m_schools) {
                                if (!StringUtils.isEmpty(info.m_schoolCode) &&
                                        info.m_schoolCode.equals(stdSklCode)) {
                                    schoolInfo = info;
                                    break;
                                }
                            }
                            if (schoolInfo == null) {
                                schoolInfo = new SchoolInfo();
                                schoolInfo.m_schoolCode = stdSklCode;
                                m_schools.add(schoolInfo);
                            }

                            calculateStudent(lastStdOid, lastStdAbsenses, schoolInfo, lastStdSklOid);
                        }

                        lastStdOid = att.getStudentOid();
                        lastStdSklOid = att.getSchoolOid();
                        lastStdAbsenses = (att.getPortionAbsent() == null ? 0.0f : att.getPortionAbsent().floatValue());
                    }
                }
            } finally {
                iterator.close();
            }

            // Go through district student map and calculate each student.
            // Add district last.
            SchoolInfo orgInfo = new SchoolInfo();
            orgInfo.m_schoolCode = "XXXX";
            for (String studentOid : stdDistMap.keySet()) {
                Float attCount = stdDistMap.get(studentOid);
                calculateStudent(studentOid, attCount.intValue(), orgInfo, null);
            }
            m_schools.add(orgInfo);

            // Set the row count.
            setRowCount(m_schools.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * For a student,
         * 1. Find their membership count in the specified school (or district if school is null).
         * 2. compare to absence count.
         * 3. If the ratio is high enough, increment the count in the info record.
         *
         * @param studentOid String
         * @param stdAbsenses float
         * @param schoolInfo SchoolInfo
         * @param schoolOid String
         */
        private void calculateStudent(String studentOid, float stdAbsenses, SchoolInfo schoolInfo, String schoolOid) {

            HabitualTruant htData = (HabitualTruant) getData();
            SisStudent student = (SisStudent) getData().getBroker().getBeanByOid(SisStudent.class, studentOid);

            // Check if the student is in the age range.
            boolean ageOk = false;
            if (student.getPerson().getDob() != null) {
                // m_lowerAgeDate is 5 years ago.
                // m_upperAgeDate is 20 years ago.
                if (!student.getPerson().getDob().before(htData.m_upperAgeDate) &&
                        !student.getPerson().getDob().after(htData.m_lowerAgeDate)) {
                    ageOk = true;
                }
            }
            // If the age is not appropriate. skip the student.
            if (!ageOk) {
                return;
            }

            SisSchool school = null;

            // Determine if the student was a member on the begin date.
            boolean startingMember = true;
            Set<PlainDate> schoolSessionDays = null;
            if (!StringUtils.isEmpty(schoolOid)) {
                school = (SisSchool) getData().getBroker().getBeanByOid(SisSchool.class, schoolOid);
                startingMember = htData.getSchoolStartingMembers(school).contains(studentOid);
                schoolSessionDays = htData.getCalendarDays(school, student.getCalendarCode());
            } else {
                startingMember = htData.m_districtStartingMembers.contains(studentOid);
                schoolSessionDays = htData.getCalendarDays(student.getSchool(), student.getCalendarCode());
            }

            // Calculate the students membership days.
            int memberDays = htData.m_enrollmentManager.getMembershipTotal(
                    student,
                    schoolSessionDays,
                    startingMember,
                    htData.m_beginDate,
                    htData.m_endDate,
                    school);

            // Determine if the student is habitual truant based on ratio of attendance and
            // membership.
            if (memberDays >= 91) {
                if ((stdAbsenses / memberDays) > 0.20f) {
                    // Habitual truant.
                    schoolInfo.m_attCount++;
                }
            }
        }

        /**
         * Return the students override school code for the student if there is one.
         *
         * @param student SisStudent
         * @return String
         */
        private String getOverrideSchoolCode(SisStudent student) {
            String schoolId = null;
            if (!StringUtils.isEmpty(((HabitualTruant) getData()).m_overrideSchoolCodeField)) {
                String studentSchoolOverride = (String) student
                        .getFieldValueByBeanPath(((HabitualTruant) getData()).m_overrideSchoolCodeField);
                if (!StringUtils.isEmpty(studentSchoolOverride)) {
                    schoolId = ((HabitualTruant) getData()).m_schoolCodeMap.get(studentSchoolOverride);
                }
            }
            return schoolId;
        }

        /**
         * Return the school code from the school object.
         *
         * @param school SisSchool
         * @return String
         */
        private String getSchoolCode(SisSchool school) {
            String code = (String) school.getFieldValueByAlias(DOE_SCHOOL_CODE);
            return code;
        }
    }

    /**
     * inner class for holding counts for a given school or organization.
     *
     * @author X2 Development Corporation
     */
    protected static class SchoolInfo {
        public int m_attCount;
        public String m_schoolCode;
    }

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String BEGIN_DATE_PARAM = "beginDate";
    public static final String END_DATE_PARAM = "endDate";

    /*
     * Field aliases for "Student Program Participation" program type code
     */
    private static final String DOE_SCHOOL_CODE = "DOE SCHOOL CODE";
    private static final String DOE_SCHOOL_OVERRIDE = "DOE SCHOOL OVERRIDE";

    private static final String PARAM_SCHOOL_CODE = "CODE";
    private static final String PARAM_SCHOOL_COUNT = "COUNT";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected PlainDate m_beginDate;
    protected PlainDate m_endDate;
    protected String m_reportingContextYearOid;
    protected String m_schoolCodeField;
    protected EnrollmentManager m_enrollmentManager;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected Map<String, SisSchool> m_schoolCodeToSchool;
    protected Set<String> m_districtStartingMembers;
    protected String m_overrideSchoolCodeField;
    protected Map<String, String> m_schoolCodeMap;
    protected Map<String, Set<String>> m_schoolStartingMembers;
    protected PlainDate m_lowerAgeDate;
    protected PlainDate m_upperAgeDate;

    /**
     * Returns the school code for the given student.
     * <p>
     * Students can attend multiple schools in a year. Get the membership attendance record
     * for the current student instance to get the school
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * Retrieve the school code.
         * Check student school override, and translate that to school code if present.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            HabitualTruantEntity htEntity = (HabitualTruantEntity) entity;
            SchoolInfo info = htEntity.getCurrentSchoolInfo();
            Object result = null;

            if (PARAM_SCHOOL_CODE.equals(param)) {
                result = info.m_schoolCode;
            } else if (PARAM_SCHOOL_COUNT.equals(param)) {
                result = Integer.valueOf(info.m_attCount);
            }

            return result;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_beginDate = (PlainDate) getParameter(BEGIN_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_endDate);
        calendar.add(Calendar.YEAR, -5);
        m_lowerAgeDate = new PlainDate(calendar.getTimeInMillis());
        calendar.add(Calendar.YEAR, -15);
        m_upperAgeDate = new PlainDate(calendar.getTimeInMillis());

        // Load initialization data
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export organization.
             */
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(SisOrganization.class, criteria);

            // Set the query to be used for student selection.
            setQuery(query);
            setEntityClass(HabitualTruantEntity.class);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TRU-SCHOOL", new RetrieveSchoolCode());

            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set<PlainDate>
     */
    protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        String schoolOid = "XXXX";
        if (school != null) {
            schoolOid = school.getOid();
        }

        Map<String, Set<PlainDate>> calendars = m_schoolsToCalendars.get(schoolOid);

        if (calendars == null) {
            if (school != null) {
                calendars = m_enrollmentManager.getCalendarLookup(school, m_beginDate, m_endDate,
                        m_reportingContextYearOid);
            } else {
                calendars = new HashMap<String, Set<PlainDate>>();
            }

            m_schoolsToCalendars.put(schoolOid, calendars);
        }

        return calendars.get(calendar);
    }

    /**
     * Lookup starting members collections for the requested school.
     *
     * @param school SisSchool
     * @return Set<String>
     */
    protected Set<String> getSchoolStartingMembers(SisSchool school) {
        Set<String> schoolStartingMembers = m_schoolStartingMembers.get(school.getOid());
        if (schoolStartingMembers == null) {
            schoolStartingMembers = m_enrollmentManager.getMembershipAsOf(m_beginDate, school);
            m_schoolStartingMembers.put(school.getOid(), schoolStartingMembers);
        }
        return schoolStartingMembers;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_districtStartingMembers = m_enrollmentManager.getMembershipAsOf(m_beginDate, getOrganization());
        m_schoolsToCalendars = new HashMap<String, Map<String, Set<PlainDate>>>();
        m_schoolStartingMembers = new HashMap<String, Set<String>>();

        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_SCHOOL_OVERRIDE, false);
        m_schoolCodeMap = new HashMap<String, String>();
        if (!StringUtils.isEmpty(m_overrideSchoolCodeField)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field =
                    dictionary.findDataDictionaryField(SisStudent.class.getName(), m_overrideSchoolCodeField);
            if (field != null) {
                ReferenceTable refTbl = field.getReferenceTable();
                if (refTbl != null) {
                    for (ReferenceCode refCode : refTbl.getReferenceCodes()) {
                        m_schoolCodeMap.put(refCode.getCode(), refCode.getStateCode());
                    }
                }
            }
        }

        /*
         * Lookup the appropriate school year context in case the running year is not the current
         * year.
         * If unable to identify, leave null (use the current year).
         */
        Criteria criteria = new Criteria();
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_endDate);
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_beginDate);
        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker().getBeanByQuery(query);
        if (context != null) {
            m_reportingContextYearOid = context.getOid();
        } else {
            m_reportingContextYearOid = getCurrentContext().getOid();
        }

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL_CODE, true);
    }
}
