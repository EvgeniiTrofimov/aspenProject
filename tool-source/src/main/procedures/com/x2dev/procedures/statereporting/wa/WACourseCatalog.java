/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * State report for Washington Course Catalog export.
 *
 * @author X2 Development Corporation
 *
 */
public class WACourseCatalog extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the WA Course Catalog
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class WACourseCatalogEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WACourseCatalogEntity() {
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
            SchoolCourse schoolCourse = (SchoolCourse) getBean();

            String name = schoolCourse.getNumber() + " " + schoolCourse.getDescription()
                    + " [School: " + schoolCourse.getSchool().getName() + "]";

            return name;
        }
    }

    /**
     * Retrieve Content Area.
     */
    protected class RetrieveContentArea implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCourse schoolCourse = (SchoolCourse) entity.getBean();

            String value = null;

            value = (String) schoolCourse.getFieldValueByBeanPath(m_fieldCskContentArea);
            value = lookupStateValue(SchoolCourse.class, m_fieldCskContentArea, value);

            if (value == null) {
                value = m_crsContentAreasMap.get(schoolCourse.getCourseOid());
                value = lookupStateValue(Course.class, m_fieldContentArea, value);
            }

            return value;
        }
    }

    /**
     * Retrieve Course Designation
     */
    protected class RetrieveCourseDesignation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCourse schoolCourse = (SchoolCourse) entity.getBean();
            Course course = schoolCourse.getCourse();
            String returnValue = null;
            String value = (String) course.getFieldValueByBeanPath(m_fieldCrsDesignation);
            if (!StringUtils.isEmpty(value)) {
                value = lookupStateValue(Course.class, m_fieldCrsDesignation, value);
            }

            if (BooleanAsStringConverter.TRUE.equals(schoolCourse.getFieldValueByBeanPath(m_fieldCskCollegeInHs))) {
                returnValue = !StringUtils.isEmpty(value) ? value + "C" : "C";
            } else {
                returnValue = value;
            }

            return returnValue;
        }
    }

    /**
     * Retrieve CTE Course Equivalency Identification.
     */
    protected class RetrieveCteCrsIdentification implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            WACourseCatalog ccData = (WACourseCatalog) data;
            SchoolCourse csk = (SchoolCourse) entity.getBean();
            String localCode = (String) csk.getCourse().getFieldValueByBeanPath(ccData.m_fieldCrsCteIdent);

            if (!StringUtils.isEmpty(localCode)) {
                value = lookupReferenceCodeByBeanPath(Course.class, ccData.m_fieldCrsCteIdent, localCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (!StringUtils.isEmpty(entity.getFieldValue(FIELD_CIP))) {
                value = VALUE_DEFAULT_CTE_EQ;
            }
            return value;
        }
    }

    /**
     * Validate the Initial USA Place Date.
     */
    protected class ValidateCTEEquivalency implements FieldValidator {
        private static final String EXPORT_FIELD_CTE_CRS_EQUIV = "CTE CRS Equivalency";
        private static final String VAL_ID = "VAL_CTE_EQUIV";

        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String cteCrsEquival = entity.getFieldValue(EXPORT_FIELD_CTE_CRS_EQUIV);

            if (Arrays.asList("A", "B").contains(cteCrsEquival) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Missing value",
                        "If CTE CRS Equivalency = A or B, CTE Equivalency is cannot be blank/null."));

            }
            return errors;
        }
    }

    /**
     * Validate fields AP and IB, CIP on Designation Code.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFields implements FieldValidator {
        private List<String> m_validValues = new ArrayList<String>(
                Arrays.asList("I", "C", "T", "H", "A", "R", "B", "K", "O", "L", "N", "Q", "S", "Z"));
        private List<String> m_nonInstractionalWrongNeighbor =
                new ArrayList<String>(Arrays.asList("A", "K", "C", "I", "R", "S", "T"));
        private List<String> m_runningStartWrongNeighbor =
                new ArrayList<String>(Arrays.asList("I", "C", "T", "A", "K", "L", "N"));

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String apib = entity.getFieldValue(FIELD_AP_IB);
            String cip = entity.getFieldValue(FIELD_CIP);

            boolean invalidValue = false;
            if (!StringUtils.isEmpty(value)) {
                for (char validValue : value.toCharArray()) {
                    if (!m_validValues.contains(String.valueOf(validValue))) {
                        invalidValue = true;
                        break;
                    }
                }
            }
            if (invalidValue) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + "  must contain  valid values: " + m_validValues.toString(),
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            if (!StringUtils.isEmpty(value)) {

                // Element D07
                // Element D09
                if (value.contains("A") || value.contains("I")) {
                    if (StringUtils.isEmpty(apib)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Course Designation " + STYLE_BOLD + "A" + STYLE_END + " or " + STYLE_BOLD + "I"
                                        + STYLE_END + " requires AP/IB",
                                "Course Designation=" + STYLE_BOLD + value + STYLE_END + ", AP/IB=" + STYLE_BOLD + apib
                                        + STYLE_END));
                    }
                }

                // Element D07
                if (value.contains("R")) {
                    for (String wrong : m_runningStartWrongNeighbor) {
                        if (value.contains(wrong)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Course Designation " + STYLE_BOLD + "R" + STYLE_END
                                            + " may not also be reported as " + m_runningStartWrongNeighbor.toString(),
                                    "Course Designation=" + STYLE_BOLD + value + STYLE_END
                                            + ", may not also be reported as=" + STYLE_BOLD + wrong + STYLE_END));
                        }
                    }
                }

                // Element D07
                if (value.contains("Z")) {
                    for (String wrong : m_nonInstractionalWrongNeighbor) {
                        if (value.contains(wrong)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Course Designation " + STYLE_BOLD + "Z" + STYLE_END
                                            + " may not also be reported as "
                                            + m_nonInstractionalWrongNeighbor.toString(),
                                    "Course Designation=" + STYLE_BOLD + value + STYLE_END
                                            + ", may not also be reported as=" + STYLE_BOLD + wrong + STYLE_END));
                        }
                    }
                }

                // Element D10
                if (value.contains("T")) {
                    if (StringUtils.isEmpty(cip)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Course Designation " + STYLE_BOLD + "T" + STYLE_END + " requires CIP ",
                                "Course Designation=" + STYLE_BOLD + value + STYLE_END + ", CIP=" + STYLE_BOLD + cip
                                        + STYLE_END));
                    }
                }


            }

            String cteCourseEquivalencyIdentification = entity.getFieldValue(FIELD_CTE_CRS_EQ);
            // Element D12
            if (!StringUtils.isEmpty(cip) && StringUtils.isEmpty(cteCourseEquivalencyIdentification)) {
                errors.add(new StateReportValidationError(entity, field,
                        "CIP Code requires CTE CRS Equivalency",
                        "CIP Code " + STYLE_BOLD + cip + STYLE_END + ", CTE CRS Equivalency=" + STYLE_BOLD
                                + cteCourseEquivalencyIdentification + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String FIELD_AP_IB = "AP and IB Code";
    protected static final String FIELD_CIP = "CIP Code";
    protected static final String FIELD_CTE_CRS_EQ = "CTE CRS Equivalency";

    protected static final String VALUE_DEFAULT_CTE_EQ = "C";

    protected static final String ALIAS_CONTENT_AREA = "DOE CONTENT AREA";
    protected static final String ALIAS_CTE_CRS = "DOE CTE COURSE EQUIVALENCY";
    protected static final String ALIAS_CRS_DESIGNATION = "DOE CRS DESIGNATION";
    protected static final String ALIAS_CSK_CONTENT_AREA = "DOE CSK CONTENT AREA";
    protected static final String ALIAS_DESIGNATION_CODE = "DOE CRS DESIGNATION";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_CSK_IN_HS = "COLLEGE IN HS";

    protected static final String VALIDATION_ID = "EXP-WA-CC-VAL";

    protected static final String PARAM_COURSES_SCHEDULED = "scheduledCoursesOnly";
    protected static final String PARAM_COURSES_STATEDEF = "stateDefCoursesOnly";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

    protected static final String RETRIEVER_CONTENT_AREA = "CALC_CA";
    protected static final String RETRIEVER_CRS_DESIGNATION = "CALC-CRS-DESIGNATION";
    protected static final String RETRIEVER_CTE_CRS = "CALC-CTE";

    /**
     * Instance variables.
     */
    protected String m_excludeSchool;
    protected Map<String, String> m_crsContentAreasMap;
    protected String m_fieldContentArea;
    protected String m_fieldCrsCteIdent;
    protected String m_fieldCrsDesignation;
    protected String m_fieldCskContentArea;
    protected String m_fieldDesignationCode;
    protected String m_fieldExcludeCourse;
    protected String m_fieldCskCollegeInHs;

    /**
     * Initialize the data module.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            X2Criteria courseCriteria = new X2Criteria();

            // Course in the current school year
            courseCriteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                    getOrganization().getCurrentContextOid());

            // Courses with MASTER TYPE = Class
            courseCriteria.addEqualTo(SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            // Filter by school
            if (isSchoolContext()) {
                courseCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            // User option: Include Scheduled courses only
            Boolean scheduledCoursesOnly = (Boolean) getParameter(PARAM_COURSES_SCHEDULED);
            if (scheduledCoursesOnly != null ? scheduledCoursesOnly.booleanValue() : false) {
                courseCriteria.addEqualToField(SchoolCourse.REL_MASTER_SCHEDULES + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        SchoolCourse.REL_MASTER_SCHEDULES + PATH_DELIMITER +
                                MasterSchedule.COL_SCHEDULE_OID);
            }

            // User option: Include State Defined courses only (content area)
            Boolean stateDefCoursesOnly = (Boolean) getParameter(PARAM_COURSES_STATEDEF);
            if (stateDefCoursesOnly != null ? stateDefCoursesOnly.booleanValue() : false) {
                courseCriteria.addNotEmpty(SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldContentArea,
                        getBroker().getPersistenceKey());
            }

            // Exclude DOE EXCLUDE CRS courses
            courseCriteria.addNotEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldExcludeCourse,
                    BooleanAsStringConverter.TRUE);

            if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                        BooleanAsStringConverter.TRUE);
            }

            // Add criteria from input definition
            applyInputCriteria(courseCriteria, false, null);
            QueryByCriteria courseQuery =
                    new QueryByCriteria(SchoolCourse.class, courseCriteria, scheduledCoursesOnly.booleanValue());
            applyInputSort(courseQuery, null);

            initCrsContentAreas();

            // Set the query to be used for selection.
            setQuery(courseQuery);
            setEntityClass(WACourseCatalogEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RETRIEVER_CONTENT_AREA, new RetrieveContentArea());
            calcs.put(RETRIEVER_CTE_CRS, new RetrieveCteCrsIdentification());
            calcs.put(RETRIEVER_CRS_DESIGNATION, new RetrieveCourseDesignation());
            addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(VALIDATION_ID, new ValidateFields());
            validators.put(ValidateCTEEquivalency.VAL_ID, new ValidateCTEEquivalency());
            addValidators(validators);
        }
    }

    /**
     * Initialize Course Content Areas map to use if School Course Content Areas is null.
     */
    private void initCrsContentAreas() {
        Collection<String> contentAreasCodes = new ArrayList<String>();

        DataDictionaryField dictionaryField = getDataDictionaryField(Course.class, m_fieldContentArea);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    dictionaryField.getReferenceTableOid());
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    contentAreasCodes.add(code.getCode());
                }
            }
        }

        m_crsContentAreasMap = new HashMap<String, String>();

        final String COURSE_OID = X2BaseBean.COL_OID;
        final String CONTENT_AREA = m_fieldContentArea;

        String columns[] = {COURSE_OID, CONTENT_AREA};

        X2Criteria criteria = new X2Criteria();

        criteria.addIn(m_fieldContentArea, contentAreasCodes);

        ReportQueryByCriteria query = new ReportQueryByCriteria(Course.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Object record[] = (Object[]) iterator.next();
                m_crsContentAreasMap.put((String) record[0], (String) record[1]);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldDesignationCode = translateAliasToJavaName(ALIAS_DESIGNATION_CODE, true);
        m_fieldContentArea = translateAliasToJavaName(ALIAS_CONTENT_AREA, true);
        m_fieldCrsCteIdent = translateAliasToJavaName(ALIAS_CTE_CRS, true);
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, true);
        m_fieldCskContentArea = translateAliasToJavaName(ALIAS_CSK_CONTENT_AREA, true);
        m_fieldCrsDesignation = translateAliasToJavaName(ALIAS_CRS_DESIGNATION, true);
        m_fieldCskCollegeInHs = translateAliasToJavaName(ALIAS_CSK_IN_HS, true);
    }
}
