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
package com.x2dev.procedures.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Course export.
 *
 * @author X2 Development Corporation
 */

public class NYCourses extends StateReportData {
    /**
     * Entity class for Student Record Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class CoursesEntity extends StateReportEntity {
        NYCourses m_Data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CoursesEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_Data = (NYCourses) data;
        }

        /**
         * Returns the current School Course.
         *
         * @return SchoolCourse
         */
        public SchoolCourse getSchoolCourse() {
            return (SchoolCourse) getBean();
        }
    }

    /**
     * This class returns the course details based on the current student schedule span.
     * 
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDetails implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String parameter = (String) field.getParameter();
            CoursesEntity courseEntity = ((CoursesEntity) entity);
            SchoolCourse schoolCourse = courseEntity.getSchoolCourse();

            if (schoolCourse != null) {
                Course course = schoolCourse.getCourse();
                if (course != null) {
                    String courseCode = course.getNumber();
                    if (CALC_PARAM_SUBJECT_AREA.equals(parameter)) {
                        if (courseCode != null && courseCode.length() >= 2) {
                            value = courseCode.substring(0, 2);
                        }
                    } else if (CALC_PARAM_COURSE_CODE.equals(parameter)) {
                        value = courseCode;
                    }
                }
            }

            return value;
        }
    }

    /**
     * This class returns the course details based on the current student schedule span.
     * 
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseIndicator implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;
            CoursesEntity courseEntity = ((CoursesEntity) entity);
            SchoolCourse schoolCourse = courseEntity.getSchoolCourse();
            String param = (String) field.getParameter();

            if (schoolCourse != null) {
                Course course = schoolCourse.getCourse();
                if (course != null && param != null) {
                    if (param.equals(course.getAcademicLevel())) {
                        value = CODE_YES;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_COURSE_DETAILS = "CRS-DETAILS";
    protected static final String CALC_ID_COURSE_INDICATOR = "CRS-IND";

    protected static final String CALC_PARAM_SUBJECT_AREA = "SUBJ-AREA";
    protected static final String CALC_PARAM_COURSE_CODE = "COURSE-CODE";

    /**
     * Aliases for excluding specific records from the export.
     */
    protected static final String ALIAS_ACTIVE_INDICATOR = "DOE ACTIVE";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE";

    /**
     * Other Constants
     */
    protected static final String CODE_YES = "Y";
    protected static final String CODE_NO = "N";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<Course>> m_courseMap;
    protected SimpleDateFormat m_dateFormat;
    protected String m_activeIndicatorField;
    protected String m_excludeCourseField;
    protected String m_excludeSchoolField;
    protected String m_stateCourseCodeField;
    protected boolean m_excludeSchoolIndicator;
    protected boolean m_removeHeaderIndicator;
    protected int m_schoolYear;

    /**
     * Overrides getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        X2Criteria schoolCourseCriteria = getCourseCriteria();

        // Prefix path might need to get to course, or school course... etc
        applyInputCriteria(schoolCourseCriteria, true, "");

        QueryByCriteria schoolCourseQuery = new QueryByCriteria(SchoolCourse.class, schoolCourseCriteria);


        setQuery(schoolCourseQuery);
        setEntityClass(CoursesEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_COURSE_DETAILS, new RetrieveCourseDetails());
        calcs.put(CALC_ID_COURSE_INDICATOR, new RetrieveCourseIndicator());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldRetriever>();
        super.addValidators(validators);
    }

    /**
     * Returns the criteria that retrieves all courses that should be included in the export.
     *
     * @return X2Criteria
     */
    private X2Criteria getCourseCriteria() {
        X2Criteria reportingCriteria = new X2Criteria();

        if (isSchoolContext()) {
            reportingCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            reportingCriteria.addEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            reportingCriteria.addEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        if (m_excludeSchoolIndicator) {
            reportingCriteria.addNotEqualTo(
                    SchoolCourse.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_excludeSchoolField,
                    BooleanAsStringConverter.TRUE);
        }

        reportingCriteria.addEqualTo(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER
                + Course.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER
                + DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(m_schoolYear));

        reportingCriteria.addEqualTo(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_activeIndicatorField,
                BooleanAsStringConverter.TRUE);

        reportingCriteria.addNotNull(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_stateCourseCodeField);

        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            reportingCriteria.addNotEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER + m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        return reportingCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        // System Parameters
        m_schoolYear = getCurrentContext().getSchoolYear();

        // Load Parameters
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeSchoolIndicator = false;
        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
            m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        }

        // Load Alias database field Names
        m_activeIndicatorField = translateAliasToJavaName(ALIAS_ACTIVE_INDICATOR, true);
        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeSchoolField = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_stateCourseCodeField = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
    }

}
