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
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Connecticut state report for SASID Testing export.
 * This class implements the data export for CT SASID Testing export.
 *
 * @author X2 Development Corporation
 */
public class PsisStudentMemAtt extends StateReportData {

    /**
     * The Class PsisStudentMemAttEntity.
     */
    public static class PsisStudentMemAttEntity extends StateReportEntity {
        public static final String DEFAULT_CALENDAR_ID = "Standard";

        private PsisStudentMemAtt m_data;
        private Student m_student;

        private boolean m_activeOnEndDate = false;

        private Integer m_membershipInPerson = new Integer(0);
        private Integer m_attendanceInPerson = new Integer(0);
        private Integer m_membershipRemote = new Integer(0);
        private Integer m_attendanceRemote = new Integer(0);

        /**
         * Instantiates a new psis student mem att entity.
         */
        public PsisStudentMemAttEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (PsisStudentMemAtt) data;
            m_student = (Student) bean;

            // in person membership days
            int totalMembership = 0;
            PlainDate currLastActiveDate = null;
            List<StudentEnrollmentSpan> spans = filterSpans(m_data.m_helper.getStudentEnrollmentSpans(m_student, true));
            for (StudentEnrollmentSpan span : spans) {
                totalMembership += getMembershipDays(span, currLastActiveDate);
                if ((span.getLastActiveDate() != null &&
                        span.getLastActiveDate().compareTo(m_data.m_monthEndDate) >= 0) ||
                        (span.getLastActiveDate() == null
                                && span.getFirstActiveDate().compareTo(m_data.m_monthEndDate) <= 0)) {
                    m_activeOnEndDate = true;
                }
                currLastActiveDate = span.getLastActiveDate();
            }
            if (spans.isEmpty() || m_activeOnEndDate == false) {
                setRowCount(0);
            }

            // fetch all student attendance for the month
            X2Criteria attCriteria = new X2Criteria();
            attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_student.getOid());
            attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_data.m_monthStartDate);
            attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_data.m_monthEndDate);
            BeanQuery query = new BeanQuery(StudentAttendance.class, attCriteria);

            // load rest of membership and attendance counts
            Collection<StudentAttendance> attendance = m_data.getBroker().getCollectionByQuery(query);
            int absenceInPerson = 0;
            for (StudentAttendance att : attendance) {
                if (isVirtual(att)) {
                    if (att.getAbsentIndicator() == false) {
                        m_attendanceRemote = new Integer(m_attendanceRemote.intValue() + 1);
                    }
                    m_membershipRemote = new Integer(m_membershipRemote.intValue() + 1);
                } else if (att.getAbsentIndicator() == true) {
                    absenceInPerson++;
                }
            }
            m_membershipInPerson = new Integer(totalMembership - m_membershipRemote.intValue());
            m_attendanceInPerson = new Integer(m_membershipInPerson.intValue() - absenceInPerson);
        }

        /**
         * Filter spans.
         *
         * @param spans List<StudentEnrollmentSpan>
         * @return List
         */
        private List<StudentEnrollmentSpan> filterSpans(List<StudentEnrollmentSpan> spans) {
            List<StudentEnrollmentSpan> filteredSpans = new ArrayList<StudentEnrollmentSpan>();
            for (StudentEnrollmentSpan span : spans) {
                School school = span.getSchool();
                String exclude = (String) school.getFieldValueByBeanPath(m_data.m_fieldDoeExcludeSkl);
                if (!StringUtils.isEqual(exclude, BooleanAsStringConverter.TRUE)) {
                    filteredSpans.add(span);
                }
            }

            return filteredSpans;
        }

        /**
         * Determine if attendance record has a virtual code.
         *
         * @param attendance StudentAttendance
         * @return True is either other code 1 or 2 are flagged as isVirtual
         */
        private boolean isVirtual(StudentAttendance attendance) {
            String other1 = attendance.getOtherCode();
            String other2 = attendance.getOtherCode02();
            String isVirtual = null;

            if (!StringUtils.isEmpty(other1)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            if (!StringUtils.isEmpty(other2) && StringUtils.isEmpty(isVirtual)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            return BooleanAsStringConverter.TRUE.equals(isVirtual);
        }

        /**
         * Gets the membership days.
         *
         * @param span StudentEnrollmentSpan
         * @param currLastActiveDate
         * @return int
         */
        public int getMembershipDays(StudentEnrollmentSpan span, PlainDate currLastActiveDate) {
            Set<PlainDate> insessionDates =
                    m_data.m_helper.getCalendarDays(span.getSchool(), m_student.getCalendarCode());
            if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(m_student.getCalendarCode())) {
                insessionDates =
                        m_data.m_helper.getCalendarDays(span.getSchool(), DEFAULT_CALENDAR_ID);
            }
            if (insessionDates == null) {
                insessionDates = m_data.m_helper.getCalendarDays(span.getSchool(),
                        StudentHistoryHelper.CALENDAR_ANY);
            }

            // Count in session days between (and including) first and last active dates.
            PlainDate endDate = span.getLastActiveDate();
            if (endDate == null || endDate.after(m_data.m_monthEndDate)) {
                endDate = m_data.m_monthEndDate;
            }
            PlainDate beginDate = span.getFirstActiveDate();
            if (beginDate == null || beginDate.before(m_data.m_monthStartDate)) {
                beginDate = m_data.m_monthStartDate;
            } else if (currLastActiveDate != null && currLastActiveDate.equals(beginDate)) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(beginDate);
                cal.add(Calendar.DATE, 1);
                beginDate = new PlainDate(cal.getTime());
            }
            int count = 0;
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (!date.before(beginDate) && !date.after(endDate)) {
                        count++;
                    }
                }
            }
            return count;
        }

        /**
         * Gets the in person membership.
         *
         * @return Integer
         */
        public String getInPersonMembership() {
            return String.format("%02d", m_membershipInPerson);
        }

        /**
         * Gets the in person attendance.
         *
         * @return Integer
         */
        public String getInPersonAttendance() {
            return String.format("%02d", m_attendanceInPerson);
        }

        /**
         * Gets the remote membership.
         *
         * @return Integer
         */
        public String getRemoteMembership() {
            return String.format("%02d", m_membershipRemote);
        }

        /**
         * Gets the remote attendance.
         *
         * @return Integer
         */
        public String getRemoteAttendance() {
            return String.format("%02d", m_attendanceRemote);
        }

    }

    /**
     * Retriever for a student's days in membership and attendance, broken down by in-person and
     * remote.
     */
    protected class RetrieveCounts implements FieldRetriever {
        private static final String CALC_ID = "PSIS-COUNTS";

        private static final String MEM_INPERSON = "MEM-INPERSON";
        private static final String ATT_INPERSON = "ATT-INPERSON";
        private static final String MEM_REMOTE = "MEM-REMOTE";
        private static final String ATT_REMOTE = "ATT-REMOTE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PsisStudentMemAttEntity psisEntity = (PsisStudentMemAttEntity) entity;
            String param = (String) field.getParameter();
            if (MEM_INPERSON.equals(param)) {
                return psisEntity.getInPersonMembership();
            } else if (ATT_INPERSON.equals(param)) {
                return psisEntity.getInPersonAttendance();
            } else if (MEM_REMOTE.equals(param)) {
                return psisEntity.getRemoteMembership();
            } else if (ATT_REMOTE.equals(param)) {
                return psisEntity.getRemoteAttendance();
            }

            return "";
        }
    }

    /**
     * Retriever for the report date.
     */
    protected class RetrieveReportDate implements FieldRetriever {
        private static final String CALC_ID = "PSIS-REPORTDATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return m_reportDate;
        }
    }

    /**
     * Parameters
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Aliases
     */
    public static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";
    public static final String ALIAS_DOE_EXCLUDE_SKL = "all-skl-ExcludefromStateReporting";
    public static final String ALIAS_RAT_VIRTUAL_FLAG = "all-rat-VirtualFlag";

    /*
     * List of grades to include by state code
     */
    public static final List<String> GRADES_INCLUDED =
            Arrays.asList("KF", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected StudentHistoryHelper m_helper;

    protected PlainDate m_reportDate;
    protected PlainDate m_monthStartDate;
    protected PlainDate m_monthEndDate;

    protected Map<String, RefAttendanceStudent> m_refAttendanceMap = null;
    protected List<String> m_gradeLevels;

    /**
     * Bean paths.
     */
    protected String m_fieldDoeExcludeStd;
    protected String m_fieldDoeExcludeSkl;
    protected String m_fieldRatIsVirtual;


    /**
     * Returns the heading with an end of line character appended.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder sb = new StringBuilder(super.getHeading());

        return sb.toString();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_fieldDoeExcludeStd = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STD, true);
        m_fieldDoeExcludeSkl = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_SKL, true);
        populateDates();
        loadRefAttendanceMap();
        loadGradeLevelRefList();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_monthStartDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_monthEndDate);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addNotEqualTo(m_fieldDoeExcludeStd, BooleanAsStringConverter.TRUE);
        studentCriteria.addIn(Student.COL_GRADE_LEVEL, m_gradeLevels);

        if (getSchool() == null) {
            studentCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }

        setQuery(m_helper.getStudentQuery(true));
        setEntityClass(PsisStudentMemAttEntity.class);

        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveCounts.CALC_ID, new RetrieveCounts());
        calcs.put(RetrieveReportDate.CALC_ID, new RetrieveReportDate());
        super.addCalcs(calcs);
    }

    /**
     * Gets the checks if is virtual field.
     *
     * @return String
     */
    public String getIsVirtualField() {
        if (m_fieldRatIsVirtual == null) {
            m_fieldRatIsVirtual = translateAliasToJavaName(ALIAS_RAT_VIRTUAL_FLAG, true);
        }

        return m_fieldRatIsVirtual;
    }

    /**
     * Initialize a map of Daily Attendance Reference Codes.
     */
    protected void loadRefAttendanceMap() {
        DataDictionaryField isVirtualField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_RAT_VIRTUAL_FLAG);
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, RefAttendanceStudent.ATTENDANCE_TYPE_DAILY);
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE, RefAttendanceStudent.TYPE_OTHER_CODE);
        if (isVirtualField != null) {
            criteria.addEqualTo(isVirtualField.getJavaName(), BooleanAsStringConverter.TRUE);
        }

        BeanQuery query = new BeanQuery(RefAttendanceStudent.class, criteria);

        m_refAttendanceMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 8);
    }

    /**
     * Load grade level ref map.
     */
    private void loadGradeLevelRefList() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ReferenceTable.REF_TABLE_OID_GRADE_LEVEL);

        BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

        StringBuilder output = new StringBuilder();
        List<String> gradeLevels = new ArrayList<String>();
        for (ReferenceCode code : codes) {
            output.append("Code: " + code.getCode() + " State: " + code.getStateCode() + " Included: "
                    + GRADES_INCLUDED.contains(code.getStateCode()) + "\n");
            if (GRADES_INCLUDED.contains(code.getStateCode())) {
                gradeLevels.add(code.getCode());
            }
        }
        m_gradeLevels = gradeLevels;
    }



    /**
     * Populate dates.
     */
    private void populateDates() {
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        Calendar cal = Calendar.getInstance();
        cal.setTime(m_reportDate);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        m_monthStartDate = new PlainDate(cal.getTime());

        cal.set(Calendar.DAY_OF_MONTH, cal.getActualMaximum(Calendar.DAY_OF_MONTH));
        m_monthEndDate = new PlainDate(cal.getTime());
    }
}
