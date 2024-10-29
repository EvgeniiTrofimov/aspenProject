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
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for Student Record Enrollment Level export.
 *
 * @author X2 Development Corporation
 */
public class SREnrollmentLevel extends StateReportData {
    private static final String ALIAS_EXCLUDE_STATE_REPORTING = "DOE EXCLUDE SKL";

    /**
     * Entity class for Student Record Discipline Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SREnrollmentLevelEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        SREnrollmentLevel m_enrollmentData = null;
        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        EnrollmentSnapshot m_snapshot = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SREnrollmentLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment school/membership
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            if (m_schoolList.size() == 0) {
                error = new StateReportValidationError(getEntityName(), "School membership",
                        "No school membership days", "");
            }

            return error;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Student student = (Student) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId();

            MembershipAttendance memb = getMembershipAttendance();
            School school = (School) getData().getBroker().getBeanByOid(School.class, memb.getSchoolOid());
            if (school != null) {
                name += ", SCHOOL: " + school.getName();
            }

            name += "]";

            return name;
        }

        /**
         * Returns the MembershipAttendance record for the current index.
         *
         * @return MembershipAttendance
         */
        public MembershipAttendance getMembershipAttendance() {
            return m_schoolList.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_enrollmentData = (SREnrollmentLevel) data;

            /*
             * Determine which schools the student attended in the year.
             * One entry for each school-enrollment the student attended in the school year, with
             * attendance for that school-enrollment.
             * 1. Start with the last enrollment record prior to beginning of school year.
             * 2. Work forward through all enrollment activity from the last entry prior to
             * beginning of year to report date.
             */
            Student student = (Student) bean;
            PlainDate boyDate = data.getCurrentContext().getStartDate();
            StudentEnrollment lastEnrollment = null;
            List<StudentEnrollment> enrollments = m_enrollmentData.m_studentEnrollments.get(student.getOid());

            if (enrollments != null) {
                // Progressing through enrollment records.
                for (StudentEnrollment enrollment : enrollments) {
                    // If before beginning of current school year, find the last E record
                    // not followed by a W records prior to BOY.
                    // (find the E record of status at BOY if active, otherwise nothing.)
                    if (boyDate.after(enrollment.getEnrollmentDate())) {
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            lastEnrollment = null;
                        } else {
                            lastEnrollment = enrollment;
                        }
                        continue;
                    }

                    // after BOY, check each enrollment and last enrollment to see what span they
                    // define.
                    if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        if (lastEnrollment != null) {
                            String enrollmentCode = m_enrollmentData.lookupStateFromReferenceCodes(
                                    lastEnrollment.getEnrollmentCode(), m_enrollmentData.m_enrollmentEntryCodes);
                            if (enrollmentCode == null) {
                                enrollmentCode = m_enrollmentData.lookupStateFromReferenceCodes(
                                        lastEnrollment.getEnrollmentCode(), m_enrollmentData.m_entryWithdrawalCodes);
                            }
                            String withdrawalCode = m_enrollmentData.lookupStateFromReferenceCodes(
                                    enrollment.getEnrollmentCode(), m_enrollmentData.m_entryWithdrawalCodes);
                            // A withdrawal preceded by an Enrollment. Define a span.
                            addMembership(lastEnrollment.getSchoolOid(),
                                    enrollmentCode,
                                    withdrawalCode,
                                    lastEnrollment.getEnrollmentDate(),
                                    enrollment.getEnrollmentDate(),
                                    m_enrollmentData);
                            lastEnrollment = null;
                        }
                    }
                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                    }
                }
                if (lastEnrollment != null
                        && m_enrollmentData.getActiveStatuses()
                                .contains(lastEnrollment.getStatusCode())) {
                    // Try to get the enrollment type from the last enrollment record.
                    String entryCode = null;
                    if (lastEnrollment != null) {
                        entryCode = m_enrollmentData.lookupStateFromReferenceCodes(lastEnrollment.getEnrollmentCode(),
                                m_enrollmentData.m_enrollmentEntryCodes);
                    }
                    // If not, try to get the enrollment type from the student.
                    if (entryCode == null) {
                        entryCode = m_enrollmentData.lookupStateFromReferenceCodes(student.getEnrollmentTypeCode(),
                                m_enrollmentData.m_enrollmentCodes);
                    }
                    addMembership(lastEnrollment.getSchoolOid(),
                            entryCode,
                            null,
                            lastEnrollment.getEnrollmentDate(),
                            null,
                            m_enrollmentData);
                }
            }

            setRowCount(m_schoolList.size());
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
         * Adds a school and date range to the students list of schools attended in the school year.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param enrollDate PlainDate
         * @param withdrawalDate PlainDate
         * @param enrollmentData SREnrollmentLevel
         */
        private void addMembership(String schoolOid,
                                   String enrollCode,
                                   String withdrawCode,
                                   PlainDate enrollDate,
                                   PlainDate withdrawalDate,
                                   SREnrollmentLevel enrollmentData) {
            SisStudent student = (SisStudent) getBean();

            // Check if this school is reportable by school selection.
            if ((getData().isSchoolContext() && getData().getSchool().getOid().equals(schoolOid)) ||
                    !getData().isSchoolContext()) {
                // check that the school is not archive/inactive.
                SisSchool school = (SisSchool) m_enrollmentData.getBroker().getBeanByOid(SisSchool.class, schoolOid);
                if (!school.getArchiveIndicator() && !school.getInactiveIndicator()) {
                    /*
                     * Get the attendance days for the student in the school.
                     *
                     * NOTE: Student calendar code would be for the current school
                     * when reporting a prior school. We only have the current calendar code.
                     * This could report incorrect membership days if:
                     * 1. The calendar code is different between the student in the prior school and
                     * current school.
                     * 2. The calendars in the prior school indicated by the students current
                     * calendar ID
                     * and their actual calendar ID for that school are different enough to
                     * calculate
                     * different day counts.
                     */
                    int layoutPos = 1;
                    for (MembershipAttendance ma : m_schoolList) {
                        if (ma.getSchoolOid().equals(schoolOid)) {
                            layoutPos++;
                        }
                    }
                    MembershipAttendance membership = new MembershipAttendance(schoolOid, enrollCode, withdrawCode,
                            enrollDate, withdrawalDate, layoutPos);
                    if (withdrawalDate == null) {
                        withdrawalDate = enrollmentData.m_reportDate;
                    }
                    int memberDays = m_enrollmentData.m_enrollmentManager.getMembershipTotal(
                            student,
                            enrollmentData.getCalendarDays(school, student.getCalendarCode()),
                            true,
                            enrollDate,
                            withdrawalDate,
                            school);

                    // See if there are member days.
                    if (memberDays > 0) {
                        float absenceDays = 0;
                        float unexAbsenceDays = 0;
                        List<StudentAttendance> absenses = enrollmentData.m_absences.get(student.getOid());
                        if (absenses != null) {
                            for (StudentAttendance attendance : absenses) {
                                if ((enrollDate.before(attendance.getDate()) || enrollDate.equals(attendance.getDate()))
                                        &&
                                        (withdrawalDate.after(attendance.getDate())
                                                || withdrawalDate.equals(attendance.getDate()))) {
                                    absenceDays += attendance.getPortionAbsent().floatValue();
                                    if (!attendance.getExcusedIndicator()) {
                                        unexAbsenceDays += attendance.getPortionAbsent().floatValue();
                                    }
                                }
                            }
                        }
                        membership.setMembership(memberDays);
                        membership.setAbsent(absenceDays);
                        membership.setUnexAbsent(unexAbsenceDays);

                        m_schoolList.add(membership);
                    }
                }
            }
        }
    }

    /**
     * A class for storing membership and attendance statistics for averages checking.
     *
     * @author X2 Development Corporation
     */
    protected static class MembershipAttendance {
        /*
         * Instance for school, dates, counts and codes.
         */
        float m_absent;
        String m_enrollCode;
        PlainDate m_enrollDate;
        int m_layoutPosition;
        int m_membership;
        String m_schoolOid;
        float m_unexAbsent;
        String m_withdrawCode;
        PlainDate m_withdrawalDate;

        /**
         * Constructor, set initial values.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param enrollDate PlainDate
         * @param withdrawalDate PlainDate
         * @param layoutPosition int
         */
        protected MembershipAttendance(String schoolOid, String enrollCode, String withdrawCode, PlainDate enrollDate,
                PlainDate withdrawalDate, int layoutPosition) {
            m_schoolOid = schoolOid;
            m_enrollCode = enrollCode;
            m_enrollDate = enrollDate;
            m_withdrawCode = withdrawCode;
            m_withdrawalDate = withdrawalDate;
            m_layoutPosition = layoutPosition;
        }

        /**
         * Return the accumulated absent count.
         *
         * @return float
         */
        protected float getAbsent() {
            return m_absent;
        }

        /**
         * Returns the students enrollment code for the current membership period.
         *
         * @return String
         */
        protected String getEnrollCode() {
            return m_enrollCode;
        }

        /**
         * Returns the membership begin date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getEnrollDate() {
            return m_enrollDate;
        }

        /**
         * Returns the layout position for this element.
         *
         * @return int
         */
        protected int getLayoutPosition() {
            return m_layoutPosition;
        }

        /**
         * Return the accumulated membership count for the student.
         *
         * @return int
         */
        protected int getMembership() {
            return m_membership;
        }

        /**
         * Returns the school for this record.
         *
         * @return String
         */
        protected String getSchoolOid() {
            return m_schoolOid;
        }

        /**
         * Return the accumulated unexcused absent count.
         *
         * @return float
         */
        protected float getUnexAbsent() {
            return m_unexAbsent;
        }

        /**
         * Returns the withdraw code for the students enrollment segment.
         *
         * @return String
         */
        protected String getWithdrawalCode() {
            return m_withdrawCode;
        }

        /**
         * Returns the membership end date for the enrollment period.
         *
         * @return PlainDate
         */
        protected PlainDate getWithdrawalDate() {
            return m_withdrawalDate;
        }

        /**
         * Set the accumulated absence count.
         *
         * @param absent void
         */
        protected void setAbsent(float absent) {
            m_absent = absent;
        }

        /**
         * Set the accumulated membership count for the student.
         *
         * @param membership void
         */
        protected void setMembership(int membership) {
            m_membership = membership;
        }

        /**
         * Set the accumulated unexcused absence count.
         *
         * @param unexAbsent void
         */
        protected void setUnexAbsent(float unexAbsent) {
            m_unexAbsent = unexAbsent;
        }
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Constants for reporting information
     */
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Parameters for field definitions that define the behavior of retreivers.
     */
    private static final String PARAM_ENROLL_ENTRY_CODE = "ENTRY_CODE";
    private static final String PARAM_ENROLL_ENTRY_DATE = "ENTRY_DATE";
    private static final String PARAM_WITHDRAWAL_ENTRY_CODE = "WITHDRAWAL_CODE";
    private static final String PARAM_WITHDRAWAL_ENTRY_DATE = "WITHDRAWAL_DATE";
    private static final String PARAM_DAYS_PRESENT = "PRESENT";
    private static final String PARAM_DAYS_ABSENT = "ABSENT";
    private static final String PARAM_DAYS_UNEX_ABSENT = "UNEX_ABSENT";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_EXCLUDE_STD_ALIAS = "DOE EXCLUDE STD";
    private static final String DOE_SCHOOL_CODE_ALIAS = "DOE School";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    /**
     * String constant
     */
    private static final String STRING_STANDARD = "Standard";
    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, List<StudentAttendance>> m_absences;
    protected Collection<String> m_activeStatuses;
    protected String m_overrideSchoolCodeField;
    protected Map<String, ReferenceCode> m_enrollmentCodes;
    protected Map<String, ReferenceCode> m_enrollmentEntryCodes;
    protected EnrollmentManager m_enrollmentManager;
    protected Map<String, ReferenceCode> m_entryWithdrawalCodes;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolStartDate;
    protected String m_schoolCodeField;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected Map<String, List<StudentEnrollment>> m_studentEnrollments;

    private Map<String, String> m_mostCommonCalendar;

    /**
     * Returns the a variety of row specific information based on the field parameter.
     *
     * "ENTRY_CODE"
     * "ENTRY_DATE"
     * "WITHDRAWAL_CODE"
     * "WITHDRAWAL_DATE"
     * "PRESENT"
     * "ABSENT"
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollment implements FieldRetriever {

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
            MembershipAttendance membership = ((SREnrollmentLevelEntity) entity).getMembershipAttendance();
            String param = (String) field.getParameter();
            Object value = null;

            if (PARAM_ENROLL_ENTRY_CODE.equals(param)) {
                value = membership.getEnrollCode();
            } else if (PARAM_ENROLL_ENTRY_DATE.equals(param)) {
                value = m_schoolStartDate;
                PlainDate enrollDate = membership.getEnrollDate();
                if (enrollDate != null && enrollDate.after(m_schoolStartDate)) {
                    value = enrollDate;
                }

            } else if (PARAM_WITHDRAWAL_ENTRY_CODE.equals(param)) {
                value = membership.getWithdrawalCode();
            } else if (PARAM_WITHDRAWAL_ENTRY_DATE.equals(param)) {
                value = membership.getWithdrawalDate();
            } else if (PARAM_DAYS_PRESENT.equals(param)) {
                float present = membership.getMembership() - membership.getAbsent();
                value = Float.valueOf(present);
            } else if (PARAM_DAYS_ABSENT.equals(param)) {
                value = Float.valueOf(membership.getAbsent());
            } else if (PARAM_DAYS_UNEX_ABSENT.equals(param)) {
                value = Float.valueOf(membership.getUnexAbsent());
            }
            return value;
        }
    }

    /**
     * Returns the Layout position for the enrollment segment.
     * The layout position is a field formatted as "F##"
     * where the ## is a number representing the count of
     * instances of student + school beginning at one.
     */
    protected class RetrieveRecordLayout implements FieldRetriever {
        private final DecimalFormat m_format = new DecimalFormat("00");

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
            SREnrollmentLevelEntity enrollmentEntity = (SREnrollmentLevelEntity) entity;
            MembershipAttendance membership = enrollmentEntity.getMembershipAttendance();

            String recordLayout = "F" + m_format.format(Long.valueOf(membership.getLayoutPosition()));

            return recordLayout;
        }
    }

    /**
     * Returns the school code for the enrollment segment.
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

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
            SREnrollmentLevelEntity enrollmentEntity = (SREnrollmentLevelEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            MembershipAttendance membership = enrollmentEntity.getMembershipAttendance();
            School school = (School) data.getBroker().getBeanByOid(School.class, membership.getSchoolOid());
            String schoolCode = null;
            schoolCode = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
            if (StringUtils.isEmpty(schoolCode) && (null != school)) {
                schoolCode = (String) getProperty(school, m_schoolCodeField);
            }

            return schoolCode;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        /*
         * Report date is current date.
         */
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_schoolStartDate = (PlainDate) getParameter(PARAM_START_DATE);

        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(Student.COL_YOG);
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;

                case 2: // LASID
                    studentQuery.addOrderByAscending(Student.COL_LOCAL_ID);
                    break;

                case 3: // SASID
                    studentQuery.addOrderByAscending(Student.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SREnrollmentLevelEntity.class);

            // Load student enrollment records.
            loadEnrollmentData(studentCriteria);

            // Load attendance maps.
            loadAbsenceDaysMaps(studentCriteria);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("GA-SR-ENROLLMENT", new RetrieveEnrollment());
            calcs.put("GA-SR-RECORD", new RetrieveRecordLayout());
            calcs.put("GA-SR-SCHOOL", new RetrieveSchoolCode());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = getCurrentContext().getStartDate();
            if (school.getActiveSchedule() != null &&
                    school.getActiveSchedule().getStartDate() != null) {
                startDate = school.getActiveSchedule().getStartDate();
            }
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }
        HashMap<String, Set<PlainDate>> schoolCalendarMap =
                (HashMap<String, Set<PlainDate>>) m_schoolsToCalendars.get(school.getOid());
        Set<PlainDate> dateSet = schoolCalendarMap.get(calendar);
        if (dateSet == null) {
            // try standard calendar
            dateSet = schoolCalendarMap.get(STRING_STANDARD);
        }
        if (dateSet == null) {
            // try most common calendar
            dateSet = schoolCalendarMap.get(mostCommonCalendar(school));
        }
        if (dateSet == null && !schoolCalendarMap.values().isEmpty()) {
            // use any calendar
            dateSet = schoolCalendarMap.values().iterator().next();
        }
        return dateSet;
    }

    /**
     * look a state code for a reference code value.
     * Use a provided map of ReferenceCode beans to perform the lookup.
     *
     * @param code String
     * @param codes Map<String,ReferenceCode>
     * @return String
     */
    protected String lookupStateFromReferenceCodes(String code, Map<String, ReferenceCode> codes) {
        ReferenceCode refCode = codes.get(code);
        String stateCode = null;
        if (refCode != null) {
            stateCode = refCode.getStateCode();
        }

        return stateCode;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    private Collection<String> getActiveStatuses() {
        if (m_activeStatuses == null) {
            m_activeStatuses = StudentManager.getActiveStudentCodeList(getOrganization());
        }
        return m_activeStatuses;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Students to include.
         *
         * 1. The student is active and in an active school.
         * or
         * 2. The student has (E,W) enrollment records within the school year.
         *
         */

        // Select students with enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        X2Criteria activityCriteria = new X2Criteria();
        PlainDate startDate = getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activityCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            activityCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        primaryCriteria.addOrCriteria(enrollCriteria);

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        String fieldExcludeStd = translateAliasToJavaName(DOE_EXCLUDE_STD_ALIAS, false);
        if (!StringUtils.isEmpty(fieldExcludeStd)) {
            userCriteria.addNotEqualTo(fieldExcludeStd, BooleanAsStringConverter.TRUE);
        }

        if (isSchoolContext()) {
            userCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            userCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            userCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL_CODE_ALIAS, true);
    }

    /**
     * Loads a map by student of absence days for that student.
     * Loads a map of school membership days by school/calendar code
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        m_absences = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 100);
    }

    /**
     * Loads the enrollment data required by this export.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollmentData(Criteria studentCriteria) {
        m_schoolsToCalendars = new HashMap();

        // Get reference table for incident codes.
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        X2Criteria criteria = null;
        BeanQuery query = null;

        // Get reference table for student enrollment type codes.
        DataDictionaryField field =
                dictionary.findDataDictionaryField(Student.class.getName(), Student.COL_ENROLLMENT_TYPE_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        // Get a collection of reportable incident codes.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_enrollmentCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Get a collection of StudentEnrollment Entry codes.
        referenceTableOid =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_enrollmentEntryCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Get a collection of StudentEnrollment Entry codes.
        referenceTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_entryWithdrawalCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Load all StudentEnrollment for the selected students into a map. Exclude enrollment
        // records from schools that are flagged not to be reported
        SubQuery studentQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        criteria = new X2Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentQuery);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        DataDictionaryField schoolExclusionField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_STATE_REPORTING);
        if (schoolExclusionField != null) {
            criteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + ModelProperty.PATH_DELIMITER + schoolExclusionField.getJavaName(),
                    BooleanAsStringConverter.TRUE);
        }
        query = new BeanQuery(StudentEnrollment.class, criteria, false);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, true);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, true);
        m_studentEnrollments = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 100);
    }

    /**
     * Most common calendar.
     *
     * @param school SisSchool
     * @return Object
     */
    private Object mostCommonCalendar(SisSchool school) {
        if (m_mostCommonCalendar == null) {
            m_mostCommonCalendar = new HashMap();
        }
        String calendarCode = m_mostCommonCalendar.get(school.getOid());
        if (!m_mostCommonCalendar.containsKey(school.getOid())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
            criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(school.getOrganization1(),
                    SisStudent.COL_ENROLLMENT_STATUS));
            String[] columns = new String[] {SisStudent.COL_CALENDAR_CODE};
            ColumnQuery query = new ColumnQuery(SisStudent.class, columns, criteria);
            query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
            query.addOrderByDescending("count(" + SisStudent.COL_CALENDAR_CODE + ")");

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                if (iterator.hasNext()) {
                    calendarCode = (String) ((Object[]) iterator.next())[0];
                }
            } finally {
                iterator.close();
            }
            m_mostCommonCalendar.put(school.getOid(), calendarCode);
        }
        return calendarCode;
    }

}
