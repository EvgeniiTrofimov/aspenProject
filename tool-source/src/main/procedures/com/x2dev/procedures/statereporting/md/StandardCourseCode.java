/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.CourseRequisite;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland state report for Standard Course Code export.
 *
 * @author X2 Development Corporation
 */
public class StandardCourseCode extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Standard Course Code
     * Course export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author Follett Software Company
     */
    public static class StdCourseCodeEntity extends StateReportEntity {
        StdCourseCodeEntity m_StdCourseCode;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StdCourseCodeEntity() {
            // Empty constructor.
        }

        /**
         * Generate a display name to print on the validation report for the
         * entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Course course = (Course) getBean();
            String name = course.getNumber() + " " + course.getDescription();
            return name;
        }

        /**
         * Initialize the entity for the school course bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
        }

    }

    /**
     * Alias fields
     */
    private static final String ALIAS_COURSE_END_YEAR = "DOE COURSE END YEAR";
    private static final String ALIAS_COURSE_GRADE_SPAN = "DOE GRADE SPAN";
    private static final String ALIAS_COURSE_START_YEAR = "DOE COURSE START YEAR";
    private static final String ALIAS_CTE_CIP_NUMBER = "DOE CTE CIP";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_REMARKS = "DOE REMARKS";
    private static final String ALIAS_SCED_CODE = "DOE SCED CODE";

    /**
     * Parameter fields
     */
    private static final String PARAM_CARNEGIE_UNITS = "CARNEGIE_UNITS";
    private static final String PARAM_COURSE_LOWEST_GRADE = "LOWEST-GRADE";
    private static final String PARAM_COURSE_PREREQ = "PRE-REQUISITES";
    private static final String PARAM_COURSE_STATUS = "STATUS";
    private static final String PARAM_CTE_CIP = "CTE";
    private static final String PARAM_DESCRIPTION = "DESCRIPTION";
    private static final String PARAM_GRADUATION_CREDIT = "GRADUATION-CREDIT";
    private static final String PARAM_GRADESPAN = "GRADE-SPAN";
    private static final String PARAM_REMARKS = "REMARKS";
    private static final String PARAM_SCED_COURSE_ID = "SCED-COURSE-ID";
    private static final String PARAM_SCED_SUBJECT_AREA_CODE = "SCED-SUBJ-AREA";
    private static final String PARAM_SCED_TYPE = "SCED-TYPE";
    private static final String PARAM_SEMESTER = "SEMESTER";

    /**
     * Other Constants
     */
    private static final String APPLIED_TOWARDS_GRADUATION = "Y";
    private static final String COURSE_ACTIVE = "Y";
    private static final String COURSE_INACTIVE = "N";
    private static final int MAX_LENGTH = 250;
    private static final String NOT_APPLIED_TOWARDS_GRADUATION = "N";
    private static final String IS_CTE = "Y";
    private static final String IS_NOT_CTE = "N";
    private static final String SCED_TYPE_PRIOR_TO_SECONDARY = "P";
    private static final String SCED_TYPE_SECONDARY = "S";
    private static final String STRING_SPACE = " ";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    private static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    private static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    private static final String REQUIRE_SCED_PARAM = "requireSced";

    /**
     * Instance variables.
     */
    protected String m_courseEndYear = null;
    protected String m_courseGradeSpan = null;
    protected String m_courseStartYear = null;
    protected String m_cipNumber = null;
    private String m_fieldExlcudeCrs = null;
    protected SimpleDateFormat m_formatYear = new SimpleDateFormat("yyyy");
    protected String m_remarks = null;
    protected String m_scedCode = null;
    private DistrictSchoolYearContext m_schoolYearContext = null;


    /**
     * This class returns the subject area, course id and course type using the sced code.
     * 
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseSCEDCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            StdCourseCodeEntity courseEntity = (StdCourseCodeEntity) entity;
            Course course = (Course) courseEntity.getBean();
            if (course != null) {
                String scedCode = (String) course.getFieldValueByBeanPath(m_scedCode);
                scedCode = lookupReferenceCodeByBeanPath(Course.class, m_scedCode,
                        scedCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (scedCode != null && scedCode.length() > 2) {
                    if (PARAM_SCED_SUBJECT_AREA_CODE.equalsIgnoreCase(param) ||
                            PARAM_SCED_TYPE.equalsIgnoreCase(param)) {
                        String subjAreaCode = scedCode.substring(0, 2);
                        if (subjAreaCode != null) {
                            value = Integer.valueOf(subjAreaCode);
                            if (PARAM_SCED_TYPE.equalsIgnoreCase(param)) {
                                int scedSubjAreaCd = Integer.parseInt(subjAreaCode);
                                if (scedSubjAreaCd >= 1 && scedSubjAreaCd <= 23) {
                                    value = SCED_TYPE_SECONDARY;
                                } else if ((scedSubjAreaCd >= 51 && scedSubjAreaCd <= 58) ||
                                        (scedSubjAreaCd >= 60 && scedSubjAreaCd <= 73)) {
                                    value = SCED_TYPE_PRIOR_TO_SECONDARY;
                                }
                            }
                        }
                    } else if (PARAM_SCED_COURSE_ID.equalsIgnoreCase(param)) {
                        String courseId = scedCode.substring(2);
                        if (courseId != null) {
                            value = Integer.valueOf(courseId);
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class returns the course specific details.
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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            StdCourseCodeEntity courseEntity = (StdCourseCodeEntity) entity;
            Course course = (Course) courseEntity.getBean();
            if (course != null) {
                if (PARAM_COURSE_STATUS.equalsIgnoreCase(param)) {
                    value = COURSE_INACTIVE;
                    Collection<SchoolCourse> schoolCourses = course.getSchoolCourses();
                    for (SchoolCourse schoolCourse : schoolCourses) {
                        Collection<MasterSchedule> masterSchedules = schoolCourse.getMasterSchedules();
                        if (!masterSchedules.isEmpty()) {
                            value = COURSE_ACTIVE;
                        }
                        if (value == COURSE_ACTIVE) {
                            break;
                        }
                    }

                } else if (PARAM_COURSE_LOWEST_GRADE.equalsIgnoreCase(param)) {
                    String courseGradeSpan = (String) course.getFieldValueByBeanPath(m_courseGradeSpan);
                    if (courseGradeSpan != null && courseGradeSpan.length() >= 2) {
                        value = courseGradeSpan.substring(0, 2);
                    }
                } else if (PARAM_COURSE_PREREQ.equalsIgnoreCase(param)) {
                    Collection<CourseRequisite> requisites = course.getCourseRequisite(getBroker());
                    StringBuilder pre_requisites = new StringBuilder(250);
                    for (CourseRequisite requisite : requisites) {
                        if ((pre_requisites.length() + requisite.getCourseId().length()
                                + STRING_SPACE.length()) > MAX_LENGTH) {
                            break;
                        }
                        pre_requisites.append(requisite.getCourseId()).append(STRING_SPACE);
                    }
                    value = pre_requisites.toString();
                } else if (PARAM_GRADUATION_CREDIT.equalsIgnoreCase(param)) {
                    BigDecimal credit = course.getCredit();
                    value = NOT_APPLIED_TOWARDS_GRADUATION;
                    if (credit != null && credit.doubleValue() > 0) {
                        value = APPLIED_TOWARDS_GRADUATION;
                    }
                } else if (PARAM_CARNEGIE_UNITS.equals(param)) {
                    String scedCode = (String) course.getFieldValueByBeanPath(m_scedCode);
                    scedCode = lookupReferenceCodeByBeanPath(Course.class, m_scedCode,
                            scedCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (scedCode != null && scedCode.length() > 2) {
                        String subjAreaCode = scedCode.substring(0, 2);
                        if (subjAreaCode != null) {
                            int scedSubjAreaCd = Integer.parseInt(subjAreaCode);
                            if (scedSubjAreaCd >= 1 && scedSubjAreaCd <= 23) {
                                // this condition is reached if the course is secondary only.
                                int baseTermsPerYear = 0;
                                value = Float.valueOf(baseTermsPerYear);
                                Collection<SchoolCourse> schoolCourses = course.getSchoolCourses();
                                for (SchoolCourse schoolCourse : schoolCourses) {
                                    Collection<MasterSchedule> masterSchedules = schoolCourse.getMasterSchedules();
                                    for (MasterSchedule masterSchedule : masterSchedules) {
                                        ScheduleTerm scheduleTerm = masterSchedule.getScheduleTerm();
                                        if (scheduleTerm != null) {
                                            baseTermsPerYear = scheduleTerm.getBaseTermsPerYear();
                                        }
                                        break;
                                    }
                                    if (baseTermsPerYear > 0) {
                                        break;
                                    }
                                }
                                if (baseTermsPerYear > 0) {
                                    value = Float.valueOf(1 / baseTermsPerYear);
                                }
                            }
                        }
                    }
                } else if (PARAM_SEMESTER.equals(param)) {
                    value = null;
                    Collection<SchoolCourse> schoolCourses = course.getSchoolCourses();
                    for (SchoolCourse schoolCourse : schoolCourses) {
                        Collection<MasterSchedule> masterSchedules = schoolCourse.getMasterSchedules();
                        for (MasterSchedule masterSchedule : masterSchedules) {
                            ScheduleTerm scheduleTerm = masterSchedule.getScheduleTerm();
                            if (scheduleTerm != null) {
                                String code = scheduleTerm.getCode();
                                value = lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE,
                                        code, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                            break;
                        }
                        if (value != null) {
                            break;
                        }
                    }
                } else if (PARAM_CTE_CIP.equals(param)) {
                    value = IS_NOT_CTE;
                    String cipNumber = (String) course.getFieldValueByAlias(m_cipNumber);
                    if (cipNumber != null) {
                        value = IS_CTE;
                    }
                } else if (PARAM_REMARKS.equals(param)) {
                    String remarks = (String) course.getFieldValueByAlias(m_remarks);
                    value = remarks;
                    if (remarks != null) {
                        remarks = remarks.replaceAll(",\n", STRING_SPACE);
                        value = remarks;
                        if (remarks.length() > MAX_LENGTH) {
                            value = remarks.substring(0, 250);
                        }
                    }
                } else if (PARAM_DESCRIPTION.equals(param)) {
                    value = null;
                    String summary = course.getSummary();
                    if (summary != null) {
                        summary = summary.replaceAll(",\n", STRING_SPACE);
                        value = summary;
                        if (summary.length() > 500) {
                            value = summary.substring(0, 500);
                        }
                    }
                } else if (PARAM_GRADESPAN.equals(param)) {
                    String scedCode = (String) course.getFieldValueByBeanPath(m_scedCode);
                    scedCode = lookupReferenceCodeByBeanPath(Course.class, m_scedCode,
                            scedCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (scedCode != null && scedCode.length() > 2) {
                        String subjAreaCode = scedCode.substring(0, 2);
                        if (subjAreaCode != null) {
                            int scedSubjAreaCd = Integer.parseInt(subjAreaCode);
                            if ((scedSubjAreaCd >= 51 && scedSubjAreaCd <= 58) ||
                                    (scedSubjAreaCd >= 60 && scedSubjAreaCd <= 73)) {
                                // this condition is reached if the course is prior to secondary
                                // only.
                                value = course.getFieldValueByBeanPath(m_courseGradeSpan);
                            }
                        }
                    }
                }
            }
            return value;
        }
    }


    /**
     * Initialize the data module.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();


        m_schoolYearContext = getCurrentContext();
        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            Criteria courseCriteria = getCourseCriteria();
            QueryByCriteria courseQuery = new QueryByCriteria(Course.class,
                    courseCriteria);
            courseQuery.addOrderByAscending(Course.COL_NUMBER);
            // Set the query to be used for student selection.
            setQuery(courseQuery);
            setEntityClass(StdCourseCodeEntity.class);
            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CRS-SCED", new RetrieveCourseSCEDCode());
            calcs.put("CRS-DETAILS", new RetrieveCourseDetails());
            super.addCalcs(calcs);
            HashMap validators = new HashMap<String, FieldRetriever>();
            super.addValidators(validators);

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
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER +
                RecordSet.COL_NAME,
                recordSetName);
        criteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(RecordSetKey.class,
                        RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all school courses that should be
     * included in the export.
     *
     * @return Criteria
     */
    private Criteria getCourseCriteria() {
        X2Criteria courseCriteria = new X2Criteria();

        courseCriteria.addEqualTo(Course.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        courseCriteria.addEqualTo(Course.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.toString(m_schoolYearContext.getSchoolYear()));

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_fieldExlcudeCrs)) {
            courseCriteria.addNotEqualTo(m_fieldExlcudeCrs, BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                courseCriteria.addNotEmpty(field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Check other user criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch (queryBy == null ? 0 : queryBy.intValue()) {
            case 1: // Snapshot of Courses
                addRecordSetCriteria(courseCriteria, queryString);
                break;

            default:
                // Take all course in the current school year.
                courseCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
                break;
        }

        return courseCriteria;
    }

    /**
     * Initialize static report elements. Aliases.
     */
    private void initializeFields() {
        m_fieldExlcudeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
        m_scedCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
        m_courseStartYear = translateAliasToJavaName(ALIAS_COURSE_START_YEAR, true);
        m_courseEndYear = translateAliasToJavaName(ALIAS_COURSE_END_YEAR, true);
        m_courseGradeSpan = translateAliasToJavaName(ALIAS_COURSE_GRADE_SPAN, true);
        m_cipNumber = translateAliasToJavaName(ALIAS_CTE_CIP_NUMBER, true);
        m_remarks = translateAliasToJavaName(ALIAS_REMARKS, true);
    }
}
