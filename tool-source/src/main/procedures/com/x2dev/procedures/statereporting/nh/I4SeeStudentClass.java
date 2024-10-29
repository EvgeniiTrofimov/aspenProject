/*
 * ====================================================================
 * X2 Development Corporation
 * Copyright (c) 2002-2010 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
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
public class I4SeeStudentClass extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
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
            StudentSchedule studentSchedule = (StudentSchedule) getBean();
            MasterSchedule masterSchedule = studentSchedule.getSection();
            String name = studentSchedule.getStudent().getNameView() + " :"
                    + masterSchedule.getCourseView() + " [Desc: "
                    + masterSchedule.getDescription() + ", Section: "
                    + masterSchedule.getSectionNumber() + ", Term: "
                    + masterSchedule.getTermView()
                    + "]";

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

            String javaName = getData().translateAliasToJavaName(I4SEE_COURSE_EXCLUDE, false);
            String excludeFlag;
            try {
                excludeFlag = (String) WebUtils.getProperty(getBean(),
                        StudentSchedule.REL_SECTION + PATH_DELIMITER +
                                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                                javaName);

                if (excludeFlag != null) {
                    // check enrollment count and membership days parameter.
                    if ("1".equals(excludeFlag)) {
                        // Course filtered.
                        error = new StateReportValidationError(this.toString(),
                                ((StudentSchedule) getBean()).getSection().getDescription(), " - excluded from export",
                                "");
                    } else {
                        // No filtering.
                    }
                }
            } catch (X2BaseException e) {
                e.printStackTrace();
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
            setRowCount(1);
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
    }

    /**
     * Retrieves the local class code and reduces it to MAX characters, if possible, by
     * removing leading zeros and non-numeric characters (e.g. periods, hypens, etc.)
     *
     * - 1st leading zeros are removed (e.g. '0000123-0023' == '00123-0023')
     * - 2nd duplicate hyphens are removed (e.g. '1234567-A-1' == '1234567A-1')
     * - 3rd leading zeros after a hyphen are removed (e.g. '1234567-0023' == '1234567-23')
     * - 4th periods are removed (e.g. '1234567.A-1' == '1234567A-1')
     * - Last resort ... the course ID needs to be changed.
     */
    protected class Retrieve1470LocalClassCode implements FieldRetriever {
        String m_javaName;

        /**
         * Instantiates a new retrieve 1470 local class code.
         *
         * @param javaName String
         */
        Retrieve1470LocalClassCode(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            StudentSchedule studentSchedule = (StudentSchedule) entity.getBean();
            MasterSchedule masterSchedule = studentSchedule.getSection();
            String courseId = (String) WebUtils.getProperty(masterSchedule, m_javaName);

            /*
             * A bit of a kludge....
             * Go to the School Course record and see if there's an alias called 'i4see 1470
             * prefix'.
             * If there is, and the value is not empty, then prepend the courseID with that value.
             * This is to differentiate identical courses offered at more than one school. (E.g.
             * Nashua NH offers the same course at both North and South High Schools)
             */
            if (!StringUtils.isEmpty(m_I4see1470Prefix)) {
                try {
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    String value = (String) WebUtils.getProperty(schoolCourse, m_I4see1470Prefix);
                    if (!StringUtils.isEmpty(value)) {
                        courseId = value + courseId;
                    }
                } catch (Exception e) {
                    // ignore errors
                }
            }

            /*
             * Remove any leading zeros
             */
            while (courseId.length() > field.getMaxLength() && courseId.charAt(0) == '0') {
                courseId = courseId.substring(1);
            }

            /*
             * Remove duplicate hyphens
             */
            while (courseId.length() > field.getMaxLength() && courseId.matches("([^-]*-[^-]*)*-[^-]*")) {
                courseId = courseId.replaceFirst("-", "");
            }

            /*
             * Removed zeros after a hyphen
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains("-0")) {
                courseId = courseId.replace("-0", "-");
            }

            /*
             * Removed periods
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains(".")) {
                courseId = courseId.replace(".", "");
            }

            return courseId;
        }

    }

    private static final String I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String I4SEE_1740_PROGRAM_ID = "i4see 1740";
    private static final String I4SEE_1470_ID = "I4SEE-1470";
    private static final String I4SEE_1470_LOCAL_CLASS_CODE = "i4see 1470";
    private static final String I4SEE_1470_PREFIX = "i4see 1470 prefix";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";
    /*
     * Field alias for exclusion flag
     */
    private static final String I4SEE_COURSE_EXCLUDE = "i4see Course Exclude";

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
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";

    protected boolean m_includeStudentNames;
    protected PlainDate m_reportDate;
    protected Map<String, Schedule> m_scheduleMap;
    private Map m_schoolMap;
    protected String m_I4see1470Prefix;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return StudentSchedule.class;
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
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();

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
        X2Criteria studentScheduleCriteria = new X2Criteria();

        if (isSchoolContext()) {
            studentScheduleCriteria.addEqualTo(
                    StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        /*
         * Only include data in active schedules
         */
        studentScheduleCriteria.addIn(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                m_scheduleMap.keySet());

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                studentScheduleCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentScheduleCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Limit the criteria to items that do NOT contain a CIP (i4see 1740)
         */
        String javaName = translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
        studentScheduleCriteria.addEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                javaName, getBroker().getPersistenceKey());

        /*
         * Create the query
         */
        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class,
                studentScheduleCriteria);

        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Name
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_YOG);
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 2: // School
                studentScheduleQuery.addOrderByAscending(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + SisStudent.REL_SCHOOL + PATH_DELIMITER +
                        translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true));
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 3: // LASID
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                break;

            case 4: // SASID
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;

            default:
                studentScheduleQuery.addOrderByAscending(
                        StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;
        }
        studentScheduleQuery.addOrderByAscending(
                StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        studentScheduleQuery.addOrderByAscending(
                StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_SECTION_NUMBER);
        studentScheduleQuery.addOrderByAscending(StudentSchedule.COL_TERM_VIEW);

        // Set the query to be used for student selection.
        setQuery(studentScheduleQuery);
        setEntityClass(I4SeeEntity.class);

        /*
         * Set the field definition array to include student names if desired
         */
        if (m_includeStudentNames) {
            ArrayList<FieldDefinition> fieldDefinitions =
                    new ArrayList<FieldDefinition>(m_includeStudentNames ? 11 : 10);
            fieldDefinitions.add(getName());
            fieldDefinitions.addAll(getFieldDefinitions());
            setFieldDefinitions(fieldDefinitions);
        }

        m_I4see1470Prefix = translateAliasToJavaName(I4SEE_1470_PREFIX, false);

        // Build maps of retriever functions and validator functions
        String javaName1470 = translateAliasToJavaName(I4SEE_1470_LOCAL_CLASS_CODE, true);
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(I4SEE_1470_ID, new Retrieve1470LocalClassCode(javaName1470));

        super.addCalcs(calcs);

    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition("name view",
                StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
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
