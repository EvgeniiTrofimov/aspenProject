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
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Virginia state report for Student Record Collection export.
 *
 * 'S' Enrollment Code Enhancement:
 * York needs to be able to report students transfering from district to home based school,
 * and from home based to district, without performing a complete withdrawal/enrollment in Aspen.
 * We accommodate this be adding an enrollment 'S' Record with a special pair of Home Based
 * entry/withdrawal codes.
 * The SRC export will artificially report a withdrawal/entry pair into a new row when an 'S' is
 * found with appropriate codes.
 * The purpose if the 'S' is to mark a transfer from school to home based, using the homebased
 * withdrawal code
 * in the reported exit, and the homebased entry in the reported new entry.
 *
 * Proper usage:
 * Only use 'S' from an active enrolled student to transfer to active homebound enrollment.
 * Only use 'S' to transfer from active homebound to active school based enrollment.
 * If the student is not enrolled/active, use an 'E' with a proper enrollment code to record entry
 * from inactive to homebound active.
 * If the student is withdrawing completely from homebound, use a 'W' and appropriate withdrawal to
 * withdraw the student.
 *
 * @author X2 Development Corporation
 */
public class CTECredentialCollection extends StateReportData {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Implementation of StateReportEntity to be used by the VA Student Record Collection export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTECredentialCollectionEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CTECredentialCollectionEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize.
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

            CTECredentialCollection cteCCData = (CTECredentialCollection) data;
            cteCCData.m_rowCount++;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentAssessment assessment = (StudentAssessment) getBean();

            String name = "Student - " + assessment.getStudent().getNameView() +
                    " [Testing ID: " + assessment.getStudent().getStateId() +
                    ", Local ID: " + assessment.getStudent().getLocalId() + "] ";
            return name;
        }

    }

    /**
     * Returns the student assessment values based on the param values.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAssessmentValue implements FieldRetriever {
        public static final String CALC_ID = "ASSESSMENTVALUE";

        Map<String, String> m_mapAliasBeanPath = new HashMap();

        /**
         * Instantiates a new retrieve assessment value.
         */
        public RetrieveAssessmentValue() {
            super();
            for (String alias : ASSESSMENT_ALIASES) {
                AssessmentColumnDefinition acd = getAssessmentColumnByAlias(alias, true);
                if (acd != null) {
                    m_mapAliasBeanPath.put(alias, acd.getDataFieldConfig().getDataField().getJavaName());
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String parameter = (String) field.getParameter();

            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            String beanPath = m_mapAliasBeanPath.get(parameter);
            if (!StringUtils.isEmpty(beanPath)) {
                value = data.getPropertyAsJavaType(studentAssessment, beanPath);
                if (value instanceof String && !StringUtils.isEmpty((String) value)) {
                    DataDictionaryField dictionaryField = getDataDictionaryField(StudentAssessment.class, beanPath);
                    if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                        value = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveCourseValue.
     */
    protected class RetrieveCourseValue implements FieldRetriever {
        public static final String CALC_ID = "COURSEVALUE";
        private static final String PARAM_SCED_CODE = "SCED CODE";
        private static final String PARAM_EXT_DESC = "EXT DESC";

        private String m_fieldAsmCourseCode;
        private Map<String, Course> m_mapCourse;

        /**
         * Instantiates a new retrieve course value.
         */
        public RetrieveCourseValue() {
            super();
            BeanQuery query = new BeanQuery(Course.class, getCourseCriteria());
            m_mapCourse = getBroker().getMapByQuery(query, Course.COL_NUMBER, 500);
            AssessmentColumnDefinition acd = getAssessmentColumnByAlias(ALIAS_ASM_DOE_COURSE_CODE, true);
            m_fieldAsmCourseCode = acd.getDataFieldConfig().getDataField().getJavaName();
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
            String value = null;
            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            String localCrsCode = (String) studentAssessment.getFieldValueByBeanPath(m_fieldAsmCourseCode);
            if (!StringUtils.isEmpty(localCrsCode)) {
                Course crs = m_mapCourse.get(localCrsCode);
                if (crs != null) {
                    if (PARAM_SCED_CODE.equals(field.getParameter())) {
                        String code = (String) crs.getFieldValueByBeanPath(m_fieldCrsScedCode);
                        if (!StringUtils.isEmpty(code)) {
                            DataDictionaryField dictionaryField =
                                    getDataDictionaryField(Course.class, m_fieldCrsScedCode);
                            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                                value = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            } else {
                                value = code;
                            }
                        }
                    } else if (PARAM_EXT_DESC.equals(field.getParameter())) {
                        value = (String) crs.getFieldValueByBeanPath(m_fieldCrsExtDesc);
                    }
                }
            }
            return value;
        }

    }

    /**
     * The Class RetrieveSchoolValue.
     */
    protected class RetrieveSchoolValue implements FieldRetriever, FieldValidator {
        public static final String CALC_ID = "SCHOOLVALUE";

        private String m_fieldAssessmentSchool;
        private String m_fieldSchoolId;
        private StudentHistoryHelper m_helper;

        /**
         * Instantiates a new retrieve school value.
         *
         * @param asmCriteria X2Criteria
         */
        public RetrieveSchoolValue(X2Criteria asmCriteria) {
            super();
            AssessmentColumnDefinition acd = getAssessmentColumnByAlias(ALIAS_ASM_DOE_SCHOOL_NBR, true);
            if (acd != null) {
                m_fieldAssessmentSchool = acd.getDataFieldConfig().getDataField().getJavaName();
            }
            m_fieldSchoolId = translateAliasToJavaName(ALIAS_SKL_DOE_SCHOOL_ID, true);
            m_helper = new StudentHistoryHelper(CTECredentialCollection.this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            SubQuery subQuery = new SubQuery(StudentAssessment.class, StudentAssessment.COL_STUDENT_OID, asmCriteria);
            m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, subQuery);
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
            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            String value = (String) studentAssessment.getFieldValueByBeanPath(m_fieldAssessmentSchool);
            if (StringUtils.isEmpty(value)) {
                PlainDate date = studentAssessment.getDate();
                if (date != null) {
                    List<StudentEnrollmentSpan> spans =
                            m_helper.getStudentEnrollmentSpans(studentAssessment.getStudent(), true);
                    if (spans != null && !spans.isEmpty()) {
                        for (StudentEnrollmentSpan span : spans) {
                            if (!date.before(span.getFirstActiveDate()) &&
                                    (span.getLastActiveDate() == null || !date.after(span.getLastActiveDate()))) {
                                SisSchool school = span.getSchool();
                                if (school != null) {
                                    value = (String) school.getFieldValueByBeanPath(m_fieldSchoolId);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }


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
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            if (StringUtils.isEmpty(value)) {
                PlainDate date = studentAssessment.getDate();
                if (date != null) {
                    List<StudentEnrollmentSpan> spans =
                            m_helper.getStudentEnrollmentSpans(studentAssessment.getStudent(), true);
                    if (spans != null && !spans.isEmpty()) {
                        boolean match = false;
                        for (StudentEnrollmentSpan span : spans) {
                            if (!date.before(span.getFirstActiveDate()) &&
                                    (span.getLastActiveDate() == null || !date.after(span.getLastActiveDate()))) {
                                SisSchool school = span.getSchool();
                                if (school == null) {
                                    StateReportValidationError error = new StateReportValidationError(entity, field,
                                            field.getFieldId() + " no enrolled school",
                                            "Active enrollment span for date " + date + " found with no school");
                                    errors.add(error);
                                } else {
                                    StateReportValidationError error = new StateReportValidationError(entity, field,
                                            field.getFieldId() + " school without id",
                                            "School " + school.getName() + " has no identifier");
                                    errors.add(error);
                                }
                                match = true;
                                break;
                            }
                        }
                        if (!match) {
                            StateReportValidationError error = new StateReportValidationError(entity, field,
                                    field.getFieldId() + " no matching enrollment",
                                    "No matching enrollment for date " + date);
                            errors.add(error);
                        }
                    } else {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                field.getFieldId() + " no enrollment spans",
                                "No enrollment for student in current year");
                        errors.add(error);
                    }
                } else {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            field.getFieldId() + " no assessment date ",
                            "Current enrollment requires an assessment date");
                    errors.add(error);
                }
            }

            return errors;
        }

    }

    /**
     * Returns the student assessment values based on the param values.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateExamCost implements FieldValidator {
        public static final String CALC_ID = "EXAMCOST";

        private static final String EXAM_COST = "Exam Cost";

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
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (value == null || value.isEmpty() || value.contains("-")) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        field.getFieldId() + " must have a non-negative value",
                        EXAM_COST + "=" + STYLE_BOLD + value + STYLE_END + ", " + field.getFieldId() + "=" + STYLE_BOLD
                                + value + STYLE_END);
                errors.add(error);
            }
            return errors;
        }

    }

    protected static final String ALIAS_ASM_DOE_COURSE_CODE = "DOE COURSE CODE";
    protected static final String ALIAS_ASM_DOE_EXAM_COST = "DOE EXAM COST";
    protected static final String ALIAS_ASM_DOE_EXAM_NBR = "DOE EXAM NBR";
    protected static final String ALIAS_ASM_DOE_PASS_FAIL = "DOE PASS FAIL";
    protected static final String ALIAS_ASM_DOE_SCHOOL_NBR = "DOE SCHOOL NBR";

    protected static final String ALIAS_CRS_DOE_SCED_COURSE = "DOE SCED COURSE";
    protected static final String ALIAS_CRS_EXT_DESC = "all-crs-VirginiaExtendedDescription";
    protected static final String ALIAS_ORG_DISTRICT_CODE = "DOE DISTRICT ID";
    protected static final String ALIAS_SKL_DOE_SCHOOL_ID = "DOE SCHOOL ID";

    protected static final List<String> ASSESSMENT_ALIASES = Arrays.asList(ALIAS_ASM_DOE_EXAM_COST,
            ALIAS_ASM_DOE_EXAM_NBR,
            ALIAS_ASM_DOE_PASS_FAIL);

    protected static final String CTECC = "CTECC";

    private static final String EOL = ExportJavaSource.FORMAT_EOL_WINDOWS;
    /*
     * Constants for reporting parameters.
     */
    protected static final String PARAM_EMAIL = "email";

    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected String m_fieldCrsExtDesc;
    protected String m_fieldCrsScedCode;

    protected int m_rowCount = 1;

    private Map<String, AssessmentColumnDefinition> m_mapAssessmentColumns = null;

    /**
     * Returns heading text to include at the top of the export file.
     * For CTECC Collection, this will include the heading and the A record.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // get values used in the heading.
        String divId = (String) getOrganization().getFieldValueByAlias(ALIAS_ORG_DISTRICT_CODE);
        String email = (String) getParameter(PARAM_EMAIL);
        if (StringUtils.isEmpty(email)) {
            email = "{email}";
        }

        // Header row
        StringBuilder header = new StringBuilder(70);
        java.util.Date currDate = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm:ss");
        int year = getOrganization().getCurrentContext().getSchoolYear() - 1;

        // Heading row.
        header.append("SenderID=").append(divId).append(EOL);
        header.append("CreateDate=").append(dateFormat.format(currDate)).append(EOL);
        header.append("CreateTime=").append(timeFormat.format(currDate)).append(EOL);
        header.append("EMAIL=").append(email).append(EOL);
        header.append("~~").append(EOL);
        header.append("DATATYPE=CTECC").append(EOL);
        header.append("~").append(EOL);

        // A record.
        header.append("A");
        header.append(Integer.toString(year));
        header.append(divId);
        header.append(EOL);


        return header.toString();
    }

    /**
     * Returns trailing text to include at the bottom of the export file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getTrailer() {
        StringBuilder trailer = new StringBuilder(70);
        trailer.append("RECORDCOUNT=").append(Integer.toString(m_rowCount)).append(EOL);
        return trailer.toString();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {

        m_fieldCrsScedCode = translateAliasToJavaName(ALIAS_CRS_DOE_SCED_COURSE, true);
        m_fieldCrsExtDesc = translateAliasToJavaName(ALIAS_CRS_EXT_DESC, true);
        setDataDictionary(getAssessmentDictionary());
        AssessmentColumnDefinition acdCourseCode = getAssessmentColumnByAlias(ALIAS_ASM_DOE_COURSE_CODE, true);
        AssessmentColumnDefinition acdExamNumber = getAssessmentColumnByAlias(ALIAS_ASM_DOE_EXAM_NBR, true);
        /*
         * Job parameters.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            X2Criteria assessmentCriteria = getAssessmentCriteria(acdCourseCode, acdExamNumber);
            QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
            applyInputSort(assessmentQuery, StudentAssessment.REL_STUDENT);

            setQuery(assessmentQuery);
            setEntityClass(CTECredentialCollectionEntity.class);

            RetrieveSchoolValue schoolRetriever = new RetrieveSchoolValue(assessmentCriteria);
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveAssessmentValue.CALC_ID, new RetrieveAssessmentValue());
            calcs.put(RetrieveCourseValue.CALC_ID, new RetrieveCourseValue());
            calcs.put(RetrieveSchoolValue.CALC_ID, schoolRetriever);
            super.addCalcs(calcs);

            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateExamCost.CALC_ID, new ValidateExamCost());
            validators.put(RetrieveSchoolValue.CALC_ID, schoolRetriever);
            super.addValidators(validators);
        }
    }

    /**
     * Gets the assessment column by alias.
     *
     * @param alias String
     * @param required boolean
     * @return Assessment column definition
     */
    private AssessmentColumnDefinition getAssessmentColumnByAlias(String alias, boolean required) {
        if (m_mapAssessmentColumns == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    AssessmentColumnDefinition.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER + AssessmentDefinition.COL_ID,
                    CTECC);
            criteria.addNotEmpty(AssessmentColumnDefinition.COL_ALIAS, getBroker().getPersistenceKey());
            BeanQuery query = new BeanQuery(AssessmentColumnDefinition.class, criteria);
            m_mapAssessmentColumns = getBroker().getMapByQuery(query, AssessmentColumnDefinition.COL_ALIAS, 16);
        }
        AssessmentColumnDefinition value = m_mapAssessmentColumns.get(alias);
        if (value == null && required) {
            addSetupError("Alias not found in assessment definition", alias);
        }
        return value;
    }

    /**
     * Compose and return an iepCriteria.
     *
     * @param acdCourseCode AssessmentColumnDefinition
     * @param acdExamNumber AssessmentColumnDefinition
     * @return studentCriteria
     */
    private X2Criteria getAssessmentCriteria(AssessmentColumnDefinition acdCourseCode,
                                             AssessmentColumnDefinition acdExamNumber) {
        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                AssessmentDefinition.COL_ID, CTECC);
        asmCriteria.addIn(acdCourseCode.getDataFieldConfig().getDataField().getJavaName(), getCourseCodeSubQuery());
        asmCriteria.addGreaterThan(StudentAssessment.COL_DATE, getOrganization().getCurrentContext().getStartDate());

        DataDictionaryField dictionaryField =
                getDataDictionaryField(StudentAssessment.class,
                        acdExamNumber.getDataFieldConfig().getDataField().getJavaName());
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            X2Criteria examCriteria = new X2Criteria();
            examCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, dictionaryField.getReferenceTableOid());
            examCriteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, examCriteria);
            asmCriteria.addIn(acdExamNumber.getDataFieldConfig().getDataField().getJavaName(), subQuery);
        } else {
            asmCriteria.addNotEmpty(acdExamNumber.getDataFieldConfig().getDataField().getJavaName(),
                    getBroker().getPersistenceKey());
        }
        return asmCriteria;
    }

    /**
     * Gets the assessment dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getAssessmentDictionary() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(AssessmentDefinition.COL_ID, CTECC);
        BeanQuery query = new BeanQuery(AssessmentDefinition.class, criteria);
        AssessmentDefinition asd = (AssessmentDefinition) getBroker().getBeanByQuery(query);
        return DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());
    }

    /**
     * Gets the course code sub query.
     *
     * @return Sub query
     */
    private SubQuery getCourseCodeSubQuery() {
        return new SubQuery(Course.class, Course.COL_NUMBER, getCourseCriteria());
    }

    /**
     * Gets the course criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getCourseCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addNotEmpty(m_fieldCrsScedCode, getBroker().getPersistenceKey());
        return criteria;
    }
}
