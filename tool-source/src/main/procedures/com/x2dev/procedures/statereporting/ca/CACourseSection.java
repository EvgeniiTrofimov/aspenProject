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

package com.x2dev.procedures.statereporting.ca;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Course Section File.
 *
 * @author X2 Development Corporation
 */
public class CACourseSection extends StateReportData {
    /**
     * Entity class for Course Section export.
     *
     */
    public static class CACourseSectionEntity extends StateReportEntity // must be public static
    {
        CACourseSection m_sdData;
        List<ScheduleTeacher> m_teachers;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CACourseSectionEntity() {
            // Must have public no argument constructor
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            MasterSchedule schedule = (MasterSchedule) getBean();
            String name =
                    schedule.getSectionNumber() + "-" + schedule.getCourseView() + "-" + schedule.getDescription();
            return name;
        }

        /**
         * get teacher for current report row.
         *
         * @return Schedule teacher
         */
        public ScheduleTeacher getTeacher() {
            ScheduleTeacher res = null;
            int rowIndex = getCurrentRow();
            if (m_teachers != null && m_teachers.size() > rowIndex) {
                res = m_teachers.get(rowIndex);
            }
            return res;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity
         *      #intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            MasterSchedule schedule = (MasterSchedule) bean;
            m_sdData = (CACourseSection) data;

            // 1)to list all teachers if [DOE MULTIPLE TEACHER CODE] state code is 1 or 2,
            // 2)to list only primary teacher if [DOE MULTIPLE TEACHER CODE] has not code or one
            // teacher if section has
            // only one

            String code = (String) schedule.getFieldValueByBeanPath(m_sdData.m_multipleTeacherCode);
            String multipleTeacher =
                    m_sdData.lookupReferenceCodeByBeanPath(MasterSchedule.class, m_sdData.m_multipleTeacherCode,
                            code, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            // if [DOE MULTIPLE TEACHER CODE] state code is 1 or 2, get all teachers
            if ("1".equals(multipleTeacher) || "2".equals(multipleTeacher)) {
                m_teachers = m_sdData.m_teacherMap.get(schedule.getOid());
            }
            // if teachers for current section exist
            else if ((m_sdData.m_teacherMap.get(schedule.getOid()) != null)) {
                // initialize collection of them
                List<ScheduleTeacher> teachers = m_sdData.m_teacherMap.get(schedule.getOid());
                {
                    // then if only one teacher assigned for this section, get him
                    if ((teachers.size() == 1)) {
                        m_teachers = new ArrayList<ScheduleTeacher>(teachers);
                    }
                    // but if not, get first primary teacher
                    else {
                        for (ScheduleTeacher currentSchedule : teachers) {
                            if (currentSchedule.getPrimaryTeacherIndicator() == true) {
                                m_teachers = new ArrayList<ScheduleTeacher>();
                                m_teachers.add(currentSchedule);
                                break;
                            }
                        }
                    }
                }
            }

            setRowCount(m_teachers != null && m_teachers.size() > 0 ? m_teachers.size() : 1);
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
     * Class to calculate report fields values.
     */
    protected class RetrieveReportValue implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CACourseSectionEntity myEntity = (CACourseSectionEntity) entity; // cast bean and data
                                                                             // to correct types
            String param = (String) field.getParameter();
            if (param.equals(CACourseSection.CALC_PARAM_TYPE_CODE)) {
                return myEntity.m_sdData.m_operatingMode;
            } else if (param.equals(CACourseSection.CALC_PARAM_CLASS_ID)) {
                MasterSchedule master = (MasterSchedule) entity.getBean();
                String nclb = (String) master.getFieldValueByBeanPath(m_nclbInsert);
                return master.getCourseView().replace("-", StringUtils.unNullify(nclb) + "-");
            } else if (param.equals(CACourseSection.CALC_PARAM_COURSE_ID)) {
                MasterSchedule master = (MasterSchedule) entity.getBean();
                String nclb = (String) master.getFieldValueByBeanPath(m_nclbInsert);
                return master.getSchoolCourse().getCourse().getNumber() + StringUtils.unNullify(nclb);
            } else if (param.equals(CACourseSection.CALC_PARAM_MST_ID)) {
                MasterSchedule master = (MasterSchedule) entity.getBean();
                return myEntity.getFieldValue(FIELD_INDEX_COURSE_ID) + StringUtils.unNullify(master.getSectionNumber());
            } else if (param.equals(CACourseSection.CALC_PARAM_INSTR_LANG)) {
                if ("1".equals(myEntity.getFieldValue(FIELD_INDEX_EDUCATION_SERVICE_CODE)) ||
                        "600".equals(myEntity.getFieldValue(FIELD_INDEX_STRATEGY_CODE))) {
                    return "00";
                }
                return null;
            }

            ScheduleTeacher teacher = myEntity.getTeacher();
            if (teacher != null) {
                if (param.equals(CACourseSection.CALC_PARAM_HQT_COMPETENCY)) {
                    if (m_hqtCompetencyField != null) {
                        String code = (String) teacher.getStaff().getFieldValueByBeanPath(m_hqtCompetencyField);
                        if (code != null) {
                            return lookupReferenceCodeByBeanPath(SisStaff.class, m_hqtCompetencyField, code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                } else if (param.equals(CACourseSection.CALC_PARAM_LOCAL_STAFF_ID)) {
                    return teacher.getStaff().getLocalId();
                } else if (param.equals(CACourseSection.CALC_PARAM_SEID)) {
                    return teacher.getStaff().getStateId();
                }
            }
            return null;
        }
    }

    /**
     * Custom validation procedures implementation.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateReportValue implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            CACourseSectionEntity section = (CACourseSectionEntity) entity; // cast bean and data to
                                                                            // correct types
            String param = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            try {
                if (param.equals(CACourseSection.CALC_PARAM_PROVIDER_CODE) && !isEmptyOrBlank(value)) // field
                                                                                                      // 27
                {
                    if (!"154".equals(section.getFieldValue(FIELD_INDEX_COURSE_CONTENT_CODE))) {
                        int courseCode = Integer.parseInt(section.getFieldValue(FIELD_INDEX_STATE_COURSE_CODE));
                        if (courseCode < 4010 || courseCode > 5955) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "section provider code errror",
                                    "If CTE Course Section Provider Code is populated then Education Program Course Content code must equal 154 or a CRS-State Course Code must be populated with a CTE Course Code (range from 4010-5955)"));
                        }
                    }
                } else if (param.equals(CACourseSection.PARAM_CRS_NCLB) && m_operatingMode.equals(MODE_CRSE)) // field
                                                                                                              // 11,
                                                                                                              // CRSE
                                                                                                              // mode
                {
                    String hqt = section.getFieldValue(FIELD_INDEX_HQT);
                    if (hqt != null && hqt.length() > 0) {
                        if (!"S".equals(value) && !"E".equals(value)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "CRS-NCLB Core Course Instructional Level Code errror",
                                    "If Record Type Code = CRSE AND if HQT Competency Code on the Course Section record is populated Then NCLB Core Course Instructional Level Code must equal E (Elementary Core) or S (Secondary Core)"));
                        }
                    }
                } else if (param.equals(CACourseSection.PARAM_CRS_CTE) && "Y".equals(value)) // field
                                                                                             // 10
                {
                    if (!"154".equals(section.getFieldValue(FIELD_INDEX_COURSE_CONTENT_CODE))) {
                        String sCode = section.getFieldValue(FIELD_INDEX_STATE_COURSE_CODE);
                        int courseCode = 0;
                        if (!StringUtils.isEmpty(sCode)) {
                            courseCode = Integer.parseInt(sCode);
                        }
                        if (courseCode < 4010 || courseCode > 5955) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "CTE Technical Preparation Course Indicator errror",
                                    "If CTE Technical Preparation Course Indicator equeals 'Y' then Education Program Course Content code must equal 154 or a CRS-State Course Code must be populated with a CTE Course Code (range from 4010-5955)"));
                        }
                    }
                } else if (param.equals(CACourseSection.CALC_PARAM_HQT_COMPETENCY)) // field 28
                {
                    if (section.m_sdData.m_operatingMode.equals(MODE_CRSE) && !isEmptyOrBlank(value)) {
                        String nclb = section.getFieldValue(FIELD_INDEX_NCLB_CODE); // CRS-NCLB Code
                        if (!"E".equals(nclb) && !"S".equals(nclb)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "NCLB Core Course",
                                    "NCLB Core Course must be 'E' or 'S'"));
                        }
                    }
                } else if (param.equals(CACourseSection.PARAM_STRATEGY)) // field 22
                {
                    if (section.m_sdData.m_operatingMode.equals(MODE_CRSE) &&
                            ("400".equals(value) || "500".equals(value) || "600".equals(value)
                                    || "650".equals(value))) {
                        String service = section.getFieldValue(FIELD_INDEX_EDUCATION_SERVICE_CODE); // Education
                                                                                                    // Service
                        if (service == null || service.length() == 0) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Education Service",
                                    "Education Service cannot be empty"));
                        }
                    }

                }
                if (section.m_sdData.m_operatingMode.equals(MODE_CRSE)) {
                    if ("1".equals(section.getFieldValue(FIELD_INDEX_EDUCATION_SERVICE_CODE))
                            || "600".equals(section.getFieldValue(FIELD_INDEX_STRATEGY_CODE))) {
                        String lic = section.getFieldValue(FIELD_INDEX_LANGUAGE_OF_INSTRUCTION); // Language
                                                                                                 // of
                                                                                                 // Instruction
                                                                                                 // Code
                        if ("00".equals(lic) || "37".equals(lic)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Language of Instruction Code",
                                    "Wrong Language of Instruction Code"));
                        }
                    }
                }
                if (!isEmptyOrBlank(section.getFieldValue(FIELD_INDEX_EDUCATION_SERVICE_CODE))
                        && isEmptyOrBlank(section.getFieldValue(FIELD_INDEX_STRATEGY_CODE))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Instructional Strategy",
                            "then Instructional Strategy cannot be null"));
                }
            } catch (Exception ex) {
                errors.add(new StateReportValidationError(entity, field,
                        "error in validation",
                        "validation procedure failed for field " + field.getFieldId()));
            }
            return errors;
        }

    }

    // declare constants
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    private static final String ALIAS_HQT_COMPETANCY = "DOE HQT COMPETANCY";
    private static final String ALIAS_MULTIPLE_TEACHER = "DOE MULTIPLE TEACHER CODE";
    private static final String ALIAS_NCLB_INSERT = "DOE NCLB INSERT";
    private static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";

    private static final String CALC_PARAM_CLASS_ID = "CLASS-ID";
    private static final String CALC_PARAM_COURSE_ID = "COURSE-ID";
    private static final String CALC_PARAM_HQT_COMPETENCY = "HQT-COMPETENCY";
    private static final String CALC_PARAM_INSTR_LANG = "INST-LANG";
    private static final String CALC_PARAM_LOCAL_STAFF_ID = "LOCAL-STAFF-ID";
    public static final String CALC_PARAM_MST_ID = "MST-ID";
    private static final String CALC_PARAM_PROVIDER_CODE = "PROVIDER-CODE";
    private static final String CALC_PARAM_SEID = "SEID";
    private static final String CALC_PARAM_TYPE_CODE = "TYPE-CODE";

    private static final String CALCULATION_ID = "EXPDATA-CA-CSE";

    private static final int FIELD_INDEX_COURSE_CONTENT_CODE = 9;
    private static final int FIELD_INDEX_COURSE_ID = 7;
    private static final int FIELD_INDEX_EDUCATION_SERVICE_CODE = 19;
    private static final int FIELD_INDEX_HQT = 27;
    private static final int FIELD_INDEX_LANGUAGE_OF_INSTRUCTION = 20;
    private static final int FIELD_INDEX_NCLB_CODE = 10;
    private static final int FIELD_INDEX_STATE_COURSE_CODE = 6;
    private static final int FIELD_INDEX_STRATEGY_CODE = 21;

    private static final String MODE_CRSE = "CRSE";

    private static final String PARAM_CRS_CTE = "CRS-CTE";
    private static final String PARAM_CRS_NCLB = "CRS-NCLB";
    private static final String PARAM_OEPRATING_MODE = "operatingMode";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOL_NUMBER_SORT = "schoolNumber";
    private static final String PARAM_STRATEGY = "STRATEGY";
    private static final String PARAM_TEACHER_ID_FILTER = "teacherId";

    private static final String VALIDATION_ID = "EXP-CA-CSE-VAL";

    String m_excludeCrs;
    String m_excludeMst;
    String m_hqtCompetencyField;
    String m_multipleTeacherCode;
    String m_nclbInsert;
    String m_operatingMode;
    Map<String, List<ScheduleTeacher>> m_teacherMap;

    private PlainDate m_reportDate;

    /**
     * test for empty string.
     *
     * @param str String
     * @return true, if is empty or blank
     */
    static boolean isEmptyOrBlank(String str) {
        return str == null || str.trim().isEmpty();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Setup OK
        if (this.getSetupErrors().size() == 0) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
            m_hqtCompetencyField = this.translateAliasToJavaName(ALIAS_HQT_COMPETANCY, false);
            m_excludeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
            m_excludeMst = translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);
            m_multipleTeacherCode = translateAliasToJavaName(ALIAS_MULTIPLE_TEACHER, false);
            m_nclbInsert = translateAliasToJavaName(ALIAS_NCLB_INSERT, true);
            m_operatingMode = getParameter(PARAM_OEPRATING_MODE).toString();

            // Build query
            X2Criteria criteria = new X2Criteria();

            // select only classes
            criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_CLASS);
            // select only MasterSchedules where DOE EXCLUDE MST is not checked
            if (m_excludeCrs != null) {
                criteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                        + PATH_DELIMITER + m_excludeCrs, BooleanAsStringConverter.TRUE);
            }
            if (m_excludeMst != null) {
                criteria.addNotEqualTo(m_excludeMst, BooleanAsStringConverter.TRUE);
            }
            if (m_operatingMode.equals(MODE_CRSE)) {
                PlainDate endDate = DateUtils.add(m_reportDate, 30);
                X2Criteria criteriaSchedule = new X2Criteria();
                criteriaSchedule.addLessOrEqualThan(
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER + ScheduleTermDate.COL_START_DATE,
                        endDate);
                criteriaSchedule.addGreaterOrEqualThan(
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER + ScheduleTermDate.COL_END_DATE,
                        m_reportDate);
                SubQuery scheduleSubQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, criteriaSchedule);
                criteria.addIn(MasterSchedule.COL_SCHEDULE_TERM_OID, scheduleSubQuery);
            } else {
                String contextId = getCurrentContext().getOid();
                X2Criteria criteriaT = new X2Criteria();
                criteriaT.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, contextId);
                criteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(Transcript.class, Transcript.COL_MASTER_SCHEDULE_OID, criteriaT));
            }

            // add criteria from input definition
            String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + "1");
            if (PARAM_TEACHER_ID_FILTER.equals(queryBy)) {
                // using this parameter we ignore the reminder of selection parameters
                String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + "1");
                X2Criteria criteriaT = new X2Criteria();
                criteriaT.addEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + SisStaff.COL_LOCAL_ID, queryString);
                criteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_SECTION_OID, criteriaT));
            } else {
                this.applyInputCriteria(criteria, true, null);
            }

            fillTeachersMap(criteria);

            // create query - use the appropriate class
            QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);

            String sortBy = (String) getParameter(PARAM_SORT_BY_FIELDS);
            if (PARAM_SCHOOL_NUMBER_SORT.equals(sortBy)) {
                String schoolNumber = translateAliasToJavaName(ALIAS_SCHOOL_ID, false);
                query.addOrderByAscending(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                        + PATH_DELIMITER + schoolNumber);
            } else {
                this.applyInputSort(query, null);
            }
            this.setQuery(query);

            // Set Custom Entity
            this.setEntityClass(CACourseSectionEntity.class);

            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALCULATION_ID, new RetrieveReportValue());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(VALIDATION_ID, new ValidateReportValue());
            super.addValidators(validators);

        }
    }

    /**
     * prepare and store the section to teachers map to increase report generation performance.
     *
     * @param criteria X2Criteria
     */
    private void fillTeachersMap(X2Criteria criteria) {
        SubQuery sectionSubQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, criteria);
        X2Criteria teacherCriteria = new X2Criteria();
        teacherCriteria.addIn(ScheduleTeacher.COL_SECTION_OID, sectionSubQuery);
        QueryByCriteria teacherQuery = new QueryByCriteria(ScheduleTeacher.class, teacherCriteria);
        teacherQuery.addOrderBy(ScheduleTeacher.COL_SECTION_OID, true);
        m_teacherMap = getBroker().getGroupedCollectionByQuery(teacherQuery, ScheduleTeacher.COL_SECTION_OID, 500);
    }
}
