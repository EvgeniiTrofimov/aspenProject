/*
 * ====================================================================
 * X2 Development Corporation
 * Copyright (c) 2002-2010 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">i4see Student Class export.</blockquote>
 *
 * Procedure ID:&nbsp; <code>nhi4seeStudentClass</code>
 * <p>
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHI4SeeStudentClass extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        private NHI4SeeStudentClass m_data;

        List<StudentScheduleSpan> m_scheduleSpans;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
            return name;
        }

        /**
         * Check the Course record to see if it should be excluded from the report.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            StudentScheduleSpan span = getScheduleSpan();

            MasterSchedule section = span.getSection();
            if (section != null) {
                SchoolCourse schoolCourse = section.getSchoolCourse();
                if (schoolCourse != null) {
                    Course course = schoolCourse.getCourse();
                    if (course != null) {
                        String excludeFlag = (String) course.getFieldValueByAlias(ALIAS_I4SEE_COURSE_EXCLUDE);

                        if (!StringUtils.isEmpty(excludeFlag)) {
                            // check enrollment count and membership days parameter.
                            if ("1".equals(excludeFlag)) {
                                // Course filtered.
                                error = new StateReportValidationError(this.toString(), section.getDescription(),
                                        " - excluded from export", "");
                            } else {
                                // No filtering.
                            }
                        }
                    }
                }
            }

            return error;
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

            m_data = (NHI4SeeStudentClass) getData();

            m_data.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
            m_scheduleSpans = m_data.m_helper.getStudentScheduleSpans((SisStudent) bean);

            setRowCount(m_scheduleSpans.size());
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
         * Gets the schedule span.
         *
         * @return Student schedule span
         */
        public StudentScheduleSpan getScheduleSpan() {
            return m_scheduleSpans.get(getCurrentRow());
        }
    }
    /**
     * Returns the enrollment information from the current schedule span.
     */
    protected class RetrieveLocalClassCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentScheduleSpan span = ((I4SeeEntity) entity).getScheduleSpan();

            if (span != null) {
                value = span.getSection().getFieldValueByAlias(ALIAS_I4SEE_1470);
            }
            return value;
        }

    }
    /**
     * Returns the enrollment information from the current schedule span.
     */
    protected class RetrieveSchedule implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentScheduleSpan span = ((I4SeeEntity) entity).getScheduleSpan();
            String parameter = (String) field.getParameter();
            Object value = null;

            if (span != null) {
                if (CALC_PARAM_DIST_NBR.equals(parameter)) {
                    value = span.getSection().getSchoolCourse().getSchool().getOrganization1()
                            .getFieldValueByAlias(ALIAS_I4SEE_040);
                } else if (CALC_PARAM_SAU_NBR.equals(parameter)) {
                    value = span.getSection().getSchoolCourse().getSchool().getOrganization1()
                            .getFieldValueByAlias(ALIAS_I4SEE_030);
                } else if (CALC_PARAM_SKL_NBR.equals(parameter)) {
                    value = span.getSection().getSchoolCourse().getSchool().getFieldValueByAlias(ALIAS_I4SEE_050);
                } else if (CALC_PARAM_CLASS_CODE.equals(parameter)) {
                    MasterSchedule masterSchedule = span.getSection();

                    if (masterSchedule != null && masterSchedule.getSchoolCourse() != null
                            && masterSchedule.getSchoolCourse().getCourse() != null) {
                        String courseNumber = masterSchedule.getSchoolCourse().getCourse().getNumber();
                        if (!StringUtils.isEmpty(courseNumber) && courseNumber.length() > 15) {
                            courseNumber = courseNumber.substring(0, 16);
                        }
                        value = courseNumber;
                    }
                } else if (CALC_PARAM_SECTION_ID.equals(parameter)) {
                    value = span.getSection().getFieldValueByAlias(ALIAS_I4SEE_1320);
                } else if (CALC_PARAM_SCHOOL_YEAR.equals(parameter)) {
                    value = Integer.valueOf(data.getCurrentContext().getSchoolYear());
                } else if (CALC_PARAM_TERM.equals(parameter)) {
                    MasterSchedule masterSchedule = span.getSection();
                    if (masterSchedule != null) {
                        ScheduleTerm scheduleTerm = masterSchedule.getScheduleTerm();
                        if (scheduleTerm != null) {
                            String termCode = scheduleTerm.getCode();
                            if (termCode != null) {
                                value = data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE,
                                        termCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                        }
                    }
                } else if (CALC_PARAM_GRADE_ID.equals(parameter)) {
                    value = null;
                    Transcript transcript = span.getTranscript();

                    if (transcript != null) {
                        value = transcript.getFinalGrade();
                    }
                }
            }

            return value;
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    public static final String SORT_PARAM = "sort";
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_SCHEDULE = "SC-RS";

    private static final String CALC_PARAM_CLASS_CODE = "CLASS";
    private static final String CALC_PARAM_DIST_NBR = "DIST_NBR";
    private static final String CALC_PARAM_GRADE_ID = "GRADE_ID";
    private static final String CALC_PARAM_SAU_NBR = "SAU_NBR";
    private static final String CALC_PARAM_SCHOOL_YEAR = "SCHOOL_YEAR";
    private static final String CALC_PARAM_SECTION_ID = "SECTION";
    private static final String CALC_PARAM_SKL_NBR = "SKL_NBR";
    private static final String CALC_PARAM_TERM = "TERM";

    /**
     * Alias constants
     */
    // ORGANIZATION
    private static final String ALIAS_I4SEE_030 = "i4see 030";
    private static final String ALIAS_I4SEE_040 = "i4see 040";

    // SCHOOL
    private static final String ALIAS_I4SEE_050 = "i4see 050";

    // COURSE
    private static final String ALIAS_I4SEE_1740_PROGRAM_ID = "i4see 1740";
    private static final String ALIAS_I4SEE_COURSE_EXCLUDE = "i4see Course Exclude";

    // SCHEDULE_MASTER
    private static final String ALIAS_I4SEE_1320 = "i4see 1320";
    private static final String ALIAS_I4SEE_1470 = "i4see 1470";

    /**
     * Supporting instance variables.
     */
    private Map m_schoolMap;
    protected Map<String, Schedule> m_scheduleMap;
    protected PlainDate m_reportDate;
    protected String m_excludeCourse;
    protected String m_I4see1470Prefix;
    protected String m_programId;
    protected StudentHistoryHelper m_helper;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return Student.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE Student Class";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * Load Schools
         */
        loadSchools();

        /*
         * Load Active Schedules
         */
        loadActiveSchedules();

        /*
         * Build query object that will be used to retrieve export students.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * Only include data in active schedules
         */
        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(SisStudent.REL_STUDENT_SCHEDULES + ModelProperty.PATH_DELIMITER +
                StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                m_scheduleMap.keySet());

        m_helper.getStudentCriteria().addAndCriteria(studentCriteria);

        /*
         * Limit the criteria to items that do NOT contain a CIP (i4see 1740)
         */
        X2Criteria programCriteria = new X2Criteria();
        programCriteria.addEmpty(
                SisStudent.REL_STUDENT_SCHEDULES + ModelProperty.PATH_DELIMITER +
                        StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        m_programId,
                getBroker().getPersistenceKey());

        m_helper.getStudentCriteria().addAndCriteria(programCriteria);

        // Set the query to be used for student selection.
        setQuery(m_helper.getStudentQuery(true));
        setEntityClass(I4SeeEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_SCHEDULE, new RetrieveSchedule());
        super.addCalcs(calcs);
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition("Student Name",
                SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null, null, null, null, null);

        return field;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        // Load Parameters
        if (getParameter(INCLUDE_STUDENT_NAMES_PARAM) != null
                && ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue()) {
            getFieldDefinitions().add(0,
                    new FieldDefinition("Student Name",
                            SisStudent.COL_NAME_VIEW,
                            null, false, 1, 32, null, null, null, null, null));
        }

        if (getParameter(REPORT_DATE_PARAM) != null) {
            m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        }

        // Load Alias database field Names
        m_excludeCourse = translateAliasToJavaName(ALIAS_I4SEE_COURSE_EXCLUDE, false);
        m_programId = translateAliasToJavaName(ALIAS_I4SEE_1740_PROGRAM_ID, true);
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();

        for (SisSchool school : schools) {
            if (school.getActiveSchedule() != null) {
                m_scheduleMap.put(school.getActiveSchedule().getOid(), school.getActiveSchedule());
            }
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());

        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

}
