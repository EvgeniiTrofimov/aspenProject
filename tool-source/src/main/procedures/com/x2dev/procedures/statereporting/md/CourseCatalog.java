/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class CourseCatalog.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class CourseCatalog extends StateReportData {

    /**
     * The Class CourseCatalogEntity.
     */
    public static class CourseCatalogEntity extends StateReportEntity {
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
    }

    private static final String ALIAS_COURSE_GRADE_SPAN = "DOE GRADE SPAN";
    private static final String ALIAS_CTE_CIP = "DOE CTE CIP";
    private static final String ALIAS_DEPARTMENT = "all-crs-Department";
    private static final String ALIAS_DUAL_ENROLLMENT = "DOE DUAL ENROLLMENT";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_SCED_CODE = "DOE SCED CODE";

    private static final String PARAM_REQUIRE_SCED = "requireSced";

    protected static String removePunctuation(String value) {
        return value.replaceAll("[^a-zA-Z0-9 ]", "");
    }

    /**
     * This class returns the course specific details.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDetails implements FieldRetriever {
        protected static final String CALC_ID = "CRS-DETAILS";
        private static final String PARAM_CARNEGIE_UNITS = "CARNEGIE_UNITS";
        private static final String PARAM_FCS = "FCS";
        private static final String PARAM_GRADESPAN = "GRADE-SPAN";
        private static final String PARAM_DESCRIPTION = "DESCRIPTION";
        private static final String PARAM_SCED_COURSE_ID = "SCED-COURSE-ID";
        private static final String PARAM_SCED_SUBJECT_AREA_CODE = "SCED-SUBJ-AREA";
        private static final String VALUE_FCS = "FCS";

        private String m_courseGradeSpan;
        private String m_department;
        private String m_scedCode;

        /**
         * Instantiates a new retrieve course details.
         */
        public RetrieveCourseDetails() {
            m_courseGradeSpan = translateAliasToJavaName(ALIAS_COURSE_GRADE_SPAN, true);
            m_department = translateAliasToJavaName(ALIAS_DEPARTMENT, true);
            m_scedCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
        }

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
            Object value = null;
            String param = (String) field.getParameter();
            Course course = (Course) entity.getBean();

            if (PARAM_SCED_SUBJECT_AREA_CODE.equalsIgnoreCase(param)) {
                String scedCode = getScedCode(course);
                if (scedCode != null && scedCode.length() > 2) {
                    String subjAreaCode = scedCode.substring(0, 2);
                    if (subjAreaCode != null) {
                        value = Integer.valueOf(subjAreaCode);
                    }
                }
            } else if (PARAM_SCED_COURSE_ID.equalsIgnoreCase(param)) {
                String scedCode = getScedCode(course);
                if (scedCode != null && scedCode.length() > 2) {
                    String courseId = scedCode.substring(2);
                    if (courseId != null) {
                        value = Integer.valueOf(courseId);
                    }
                }
            } else if (PARAM_CARNEGIE_UNITS.equals(param)) {
                String scedCode = getScedCode(course);
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
            } else if (PARAM_DESCRIPTION.equals(param)) {
                value = null;
                String summary = course.getSummary();
                if (summary != null) {
                    summary = removePunctuation(summary);
                    value = summary;
                    if (summary.length() > 1000) {
                        value = summary.substring(0, 1000);
                    }
                }
            } else if (PARAM_GRADESPAN.equals(param)) {
                value = course.getFieldValueByBeanPath(m_courseGradeSpan);
            } else if (PARAM_FCS.equals(param)) {
                String department = (String) course.getFieldValueByBeanPath(m_department);
                String departmentStateCode =
                        data.lookupStateValue(entity.getBean().getClass(), m_department, department);
                value = VALUE_FCS.equals(departmentStateCode) ? Boolean.TRUE : Boolean.FALSE;
            }
            return value;
        }

        /**
         * Gets the sced code.
         *
         * @param course Course
         * @return String
         */
        private String getScedCode(Course course) {
            String scedCode = (String) course.getFieldValueByBeanPath(m_scedCode);
            scedCode = lookupReferenceCodeByBeanPath(Course.class, m_scedCode,
                    scedCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return scedCode;
        }
    }

    /**
     * The Class RetrieveCTE.
     */
    protected class RetrieveCTE implements FieldRetriever, FieldValidator {
        protected static final String CALC_ID = "CTE";
        private static final String FIELD_CTE_ASSOCIATED = "CTE Associated";
        private static final String FIELD_CTE_CERTIFICATION = "CTE Certification";
        private static final String PARAM_ASSOCIATED = "ASSOCIATED";
        private static final String PARAM_CERTIFICATION = "CERTIFICATION";

        private String m_cteCip;

        /**
         * Instantiates a new retrieve CTE.
         */
        public RetrieveCTE() {
            m_cteCip = translateAliasToJavaName(ALIAS_CTE_CIP, true);
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if ("Y".equals(entity.getFieldValue(FIELD_CTE_ASSOCIATED))
                    && StringUtils.isEmpty(entity.getFieldValue(FIELD_CTE_CERTIFICATION))) {
                errors.add(new StateReportValidationError(entity, field, "Value required",
                        "CTE Certification is required if CTE Associated = 'Y'"));
            }
            return errors;
        }

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
            Object value = null;
            String param = (String) field.getParameter();
            String cipValue = (String) entity.getBean().getFieldValueByBeanPath(m_cteCip);
            if (PARAM_ASSOCIATED.equalsIgnoreCase(param)) {
                value = StringUtils.isEmpty(cipValue) ? Boolean.FALSE : Boolean.TRUE;
            } else if (PARAM_CERTIFICATION.equalsIgnoreCase(param) && !StringUtils.isEmpty(cipValue)) {
                DataDictionaryField dictionaryField = getDataDictionaryField(entity.getBean().getClass(), m_cteCip);
                if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                    Map<String, ReferenceCode> refCodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
                    ReferenceCode code = refCodes.get(cipValue);
                    if (code != null) {
                        value = code.getDescription();
                    }
                }
            }
            return value;
        }

    }
    protected class RetrieveNoPunctuation implements FieldRetriever {
        protected static final String CALC_ID = "NO_PUNCTUATION";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = entity.getBean().getFieldValueByBeanPath(field.getBeanPath());
            if (value instanceof String) {
                value = removePunctuation((String) value);
            }
            return value;
        }

    }

    /**
     * The Class ValidateIPEDS.
     */
    protected class ValidateIPEDS implements FieldValidator {
        protected static final String CALC_ID = "IPEDS";

        private Set<String> m_setRequired = new HashSet();

        /**
         * Instantiates a new validate IPEDS.
         *
         * @param courseCriteria X2Criteria
         */
        public ValidateIPEDS(X2Criteria courseCriteria) {
            String fieldName = translateAliasToJavaName(ALIAS_DUAL_ENROLLMENT, false);
            if (!StringUtils.isEmpty(fieldName)) {
                X2Criteria criteria = courseCriteria.copy();
                criteria.addEqualTo(fieldName, BooleanAsStringConverter.TRUE);
                ColumnQuery query = new ColumnQuery(Course.class, new String[] {X2BaseBean.COL_OID}, criteria);
                ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String oid = (String) row[0];
                        m_setRequired.add(oid);
                    }
                } finally {
                    queryItr.close();
                }
            }
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (m_setRequired.contains(entity.getBean().getOid()) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required",
                        "IPEDS number is required for Dual Enrollment Courses"));
            }
            return errors;
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
        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            X2Criteria courseCriteria = getCourseCriteria();
            QueryByCriteria courseQuery = new QueryByCriteria(Course.class, courseCriteria);
            courseQuery.addOrderByAscending(Course.COL_NUMBER);
            // Set the query to be used for student selection.
            setQuery(courseQuery);
            setEntityClass(CourseCatalogEntity.class);

            // Build maps of retrievers
            RetrieveCTE cteRetriever = new RetrieveCTE();
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveCourseDetails.CALC_ID, new RetrieveCourseDetails());
            calcs.put(RetrieveNoPunctuation.CALC_ID, new RetrieveNoPunctuation());
            calcs.put(RetrieveCTE.CALC_ID, cteRetriever);
            super.addCalcs(calcs);

            // Build maps of validators
            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(RetrieveCTE.CALC_ID, cteRetriever);
            validators.put(ValidateIPEDS.CALC_ID, new ValidateIPEDS(courseCriteria));
            super.addValidators(validators);

        }
    }

    /**
     * Returns the criteria that retrieves all school courses that should be
     * included in the export.
     *
     * @return Criteria
     */
    private X2Criteria getCourseCriteria() {
        X2Criteria courseCriteria = new X2Criteria();

        courseCriteria.addEqualTo(Course.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        courseCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        // Check if the course exclusion custom field is present.
        String m_fieldExlcudeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
        if (!StringUtils.isEmpty(m_fieldExlcudeCrs)) {
            courseCriteria.addNotEqualTo(m_fieldExlcudeCrs, BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(PARAM_REQUIRE_SCED);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                courseCriteria.addNotEmpty(field.getJavaName(), getBroker().getPersistenceKey());
            }
        }

        applyInputCriteria(courseCriteria, false, "");
        return courseCriteria;
    }

}
