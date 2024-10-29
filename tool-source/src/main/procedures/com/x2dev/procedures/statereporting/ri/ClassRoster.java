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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class ClassRoster extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ClassRosterEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        ClassRoster m_classRoster;
        StudentSchedule m_studentSchedule;
        Collection<ScheduleTeacher> m_sectionTeacher;
        List<SisPerson> m_teacherPerson;
        List<SisStaff> m_staff;

        /**
         * Instantiates a new class roster entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public ClassRosterEntity() {
            // Empty, no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize the entity for the student bean provided. This method
         * finds the student schedule and student schedule change records for
         * the student and generates a list of reportable schedule items. The
         * entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_classRoster = (ClassRoster) data;
            m_studentSchedule = (StudentSchedule) bean;
            m_staff = new ArrayList<SisStaff>();
            m_teacherPerson = new ArrayList<SisPerson>();
            m_sectionTeacher = m_studentSchedule.getSection().getTeacherSections();

            int numTeachers = 0;
            if (m_sectionTeacher != null) {
                for (ScheduleTeacher teacher : m_sectionTeacher) {
                    if (teacher != null) {
                        numTeachers++;
                        if (teacher.getStaff() != null) {
                            m_staff.add(teacher.getStaff());
                            if (teacher.getStaff().getPerson() != null) {
                                m_teacherPerson.add(teacher.getStaff().getPerson());
                            }
                        }
                    }
                }
            }
            setRowCount(numTeachers);
        }

        /**
         * Gets the primary teacher person.
         *
         * @return Sis person
         */
        public SisPerson getPrimaryTeacherPerson() {
            return m_teacherPerson.get(getCurrentRow());
        }

        /**
         * Gets the student schedule.
         *
         * @return Student schedule
         */
        public StudentSchedule getStudentSchedule() {
            return m_studentSchedule;
        }

        /**
         * Gets the primary staff.
         *
         * @return Sis staff
         */
        public SisStaff getPrimaryStaff() {
            return m_staff.get(getCurrentRow());
        }
    }

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

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
     * Name for the sasid students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";

    /**
     * Start Date Alias on the Student Schedule table.
     */
    // private static final String STUDENT_SCHEDULE_START_DATE = "Start Date";

    /**
     * End Date Alias on the Student Schedule table
     */
    // private static final String STUDENT_SCHEDULE_END_DATE = "End Date";

    /**
     * Retrieve the staff state ID for the primary teacher.
     */
    protected class RetrieveUSID implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStaff primaryStaff = ((ClassRosterEntity) entity).getPrimaryStaff();

            String usid = "";
            if (primaryStaff != null) {

                usid = primaryStaff.getStateId();
            }

            return usid;
        }
    }

    /**
     * Retrieve the last name of the primary teacher.
     */
    protected class RetrieveTeacherLastName implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisPerson primaryTeacher = ((ClassRosterEntity) entity).getPrimaryTeacherPerson();

            String lastName = "";
            if (primaryTeacher != null) {
                lastName = primaryTeacher.getLastName();
            }

            return lastName;
        }
    }

    /**
     * Retrieve the first name of the primary teacher.
     */
    protected class RetrieveTeacherFirstName implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisPerson primaryTeacher = ((ClassRosterEntity) entity).getPrimaryTeacherPerson();

            String firstName = "";
            if (primaryTeacher != null) {
                firstName = primaryTeacher.getFirstName();
            }

            return firstName;
        }
    }

    /**
     * Retrieve the middle name of the primary teacher.
     */
    protected class RetrieveTeacherMiddleName implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisPerson primaryTeacher = ((ClassRosterEntity) entity).getPrimaryTeacherPerson();

            String middleName = "";
            if (primaryTeacher != null) {
                middleName = primaryTeacher.getMiddleName();
            }

            return middleName;
        }
    }

    /**
     * Retrieve the student start date in the section.
     */
    protected class RetrieveSectionStartDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentSchedule studentSchedule = ((ClassRosterEntity) entity).getStudentSchedule();
            List<StudentScheduleChange> changeList = m_scheduleChangeMap.get(studentSchedule.getStudentOid());
            Date startDate = null;

            // First check for a schedule change add to find the start date.
            if (changeList != null) {
                for (StudentScheduleChange change : changeList) {
                    if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode()) &&
                            change.getMasterScheduleOid().equals(studentSchedule.getSectionOid())) {
                        startDate = change.getEffectiveDate();
                        break;
                    }
                }
            }

            // If there is no schedule change for this section, use the schedule term start date.
            if (startDate == null &&
                    studentSchedule.getSection() != null &&
                    studentSchedule.getSection().getScheduleTerm() != null) {
                ScheduleTerm term = studentSchedule.getSection().getScheduleTerm();
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (startDate == null ||
                            startDate.after(termDate.getStartDate())) {
                        startDate = termDate.getStartDate();
                    }
                }
            }

            return startDate;
        }
    }

    /**
     * Retrieve the student end date in the section.
     */
    protected class RetrieveSectionEndDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentSchedule studentSchedule = ((ClassRosterEntity) entity).getStudentSchedule();
            Date endDate = null;

            // Use the schedule term end date.
            if (studentSchedule.getSection() != null &&
                    studentSchedule.getSection().getScheduleTerm() != null) {
                ScheduleTerm term = studentSchedule.getSection().getScheduleTerm();
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (endDate == null ||
                            endDate.before(termDate.getEndDate())) {
                        endDate = termDate.getEndDate();
                    }
                }
            }

            return endDate;
        }
    }

    /**
     * student schedule start date bean path
     */
    protected PlainDate m_districtFirstSessionDate;
    protected String m_excludeStdField;
    protected PlainDate m_reportDate;
    // protected String m_studentScheduleStartDateBeanPath;
    // protected String m_studentScheduleEndDateBeanPath = null;
    protected Map<String, List<StudentScheduleChange>> m_scheduleChangeMap;
    protected boolean m_sasidStudentOnly;

    private String m_orgFieldStr = null;
    private String m_orgOid = null;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        SisOrganization district;
        if (organization == null) {
            district = (SisOrganization) OrganizationManager.getRootOrganization(getBroker());
        } else {
            district = (SisOrganization) OrganizationManager.getRootOrganization(organization);
        }

        DistrictCalendar districtCalender =
                CalendarManager.getDistrictInSessionStartEndDate(district, getCurrentContext(), true, getBroker());
        m_districtFirstSessionDate = districtCalender.getDate();
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);

        // default report to today - now
        // TODO change to be input from the input format definition, can default to today in the
        // input definition
        m_reportDate = new PlainDate(System.currentTimeMillis());

        if (getSetupErrors().size() == 0) {
            Criteria studentScheduleCriteria = getStudentScheduleCriteria();
            QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class,
                    studentScheduleCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentScheduleQuery
                            .addOrderByAscending(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentScheduleQuery
                            .addOrderByAscending(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL +
                                    +PATH_DELIMITER + SisSchool.COL_NAME);
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);

                    break;

                default:
                    studentScheduleQuery.addOrderByAscending(
                            StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentScheduleQuery);
            setEntityClass(ClassRosterEntity.class);

            // Load a map of schedule changes to find start dates.
            loadStudentScheduleChanges();

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CR-Usid", new RetrieveUSID());
            calcs.put("CR-LastName", new RetrieveTeacherLastName());
            calcs.put("CR-FirstName", new RetrieveTeacherFirstName());
            calcs.put("CR-MiddleName", new RetrieveTeacherMiddleName());
            calcs.put("CR-StartDate", new RetrieveSectionStartDate());
            calcs.put("CR-EndDate", new RetrieveSectionEndDate());
            super.addCalcs(calcs);
        }
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

        criteria.addIn(StudentSchedule.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                X2BaseBean.COL_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all student schedules that should be included in the
     * export.
     *
     * @return Criteria
     */
    private Criteria getStudentScheduleCriteria() {
        X2Criteria userCriteria = new X2Criteria();
        userCriteria.addAndCriteria(getReportingCriteria());

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return userCriteria;
    }

    /**
     * Returns the criteria that retrieves all student schedules that should be included in the
     * export.
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        X2Criteria studentSchedule = new X2Criteria();

        // String startDateCol = translateAliasToJavaName(STUDENT_SCHEDULE_START_DATE, true);
        // studentSchedule.addGreaterOrEqualThan(startDateCol, m_districtFirstSessionDate);
        // studentSchedule.addLessOrEqualThan(endDateCol, m_reportDate);

        // From Class type section
        studentSchedule.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // Student schedules from active school schedule.
        studentSchedule.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);


        if (isSchoolContext()) {
            studentSchedule.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                studentSchedule.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            studentSchedule.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            studentSchedule.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            studentSchedule.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentOnly) {
            studentSchedule.addNotEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        return studentSchedule;
    }

    /**
     * Initialize fields for the ClassRoster class.
     */
    private void initializeFields() {
        // just for validation purposes
        // m_studentScheduleStartDateBeanPath =
        // translateAliasToJavaName(STUDENT_SCHEDULE_START_DATE, true);
        // m_studentScheduleEndDateBeanPath = translateAliasToJavaName(STUDENT_SCHEDULE_END_DATE,
        // true);
        // TODO, put in all aliases even if not used in here?
        m_sasidStudentOnly = true;
        Boolean sasidStudentOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentOnly != null) {
            m_sasidStudentOnly = sasidStudentOnly.booleanValue();
        }
    }

    /**
     * Load a map of sets of student schedule change records by student oid for the students in the
     * export.
     * This method only loads Drops and does extra filtering to eliminate schedule changes
     * that happen before the scheduled term begins.
     */
    @SuppressWarnings("unchecked")
    private void loadStudentScheduleChanges() {
        X2Criteria scheduleCriteria = new X2Criteria();

        // From Class type section
        scheduleCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // From active Schedule
        scheduleCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);

        if (isSchoolContext()) {
            scheduleCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                scheduleCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            scheduleCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            scheduleCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                scheduleCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                scheduleCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                scheduleCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(scheduleCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        scheduleCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, scheduleCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, false);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
        m_scheduleChangeMap =
                getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 100);
    }
}
