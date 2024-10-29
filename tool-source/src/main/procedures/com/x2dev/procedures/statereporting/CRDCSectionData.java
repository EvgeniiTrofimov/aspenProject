/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
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
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Dataset for Section data to be used in final CRDC export.
 *
 * @author X2 Development Corporation
 */
public class CRDCSectionData extends CRDCReportData {
    /**
     * Implementation of StateReportEntity to be used for Section CRDC Data export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SectionCRDCEntity extends StateReportEntity {

        /**
         * Instantiates a new section CRDC entity.
         */
        public SectionCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        MasterSchedule m_masterSchedule;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_masterSchedule.getSchedule().getSchool().getName() +
                    "-" + m_masterSchedule.getSchedule().getName() +
                    "-" + m_masterSchedule.getCourseView() +
                    "-" + m_masterSchedule.getDescription();

            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
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
            m_masterSchedule = (MasterSchedule) bean;
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
     * CA Retriever if a section is certified by teacher .
     *
     * @author X2 Development Corporation
     *
     */
    public class CARetrTeacherCert implements FieldRetriever {
        public static final String ALIAS_MST_DOE_HQT_CODE = "DOE HQT CODE";
        public static final String MST_STATE_CODE_HQT = "Y";

        Set<String> m_codes;

        /**
         * Instantiates a new CA retr teacher cert.
         */
        public CARetrTeacherCert() {
            super();

            m_codes = new HashSet<String>();
            DataDictionaryField mstHqtField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MST_DOE_HQT_CODE);

            if (mstHqtField != null && !StringUtils.isEmpty(mstHqtField.getReferenceTableOid())) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, mstHqtField.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, MST_STATE_CODE_HQT);

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

                QueryIterator iterator = CRDCSectionData.this.getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        ReferenceCode record = (ReferenceCode) iterator.next();
                        String code = record.getCode();
                        m_codes.add(code);
                    }
                } finally {
                    iterator.close();
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
            boolean value = false;

            MasterSchedule section = (MasterSchedule) entity.getBean();

            if (m_codes.contains(section.getFieldValueByAlias(ALIAS_MST_DOE_HQT_CODE))) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * FL retriever that returns if teacher is certified.
     *
     * @author Follett Software Company
     */
    public class FLRetrTeacherCert implements FieldRetriever {

        private static final String ALIAS_MTC_CERTIFICATION_STATUS = "all-mtc-CertificationStatus";

        private Collection<String> m_certifiedStateCodes = Arrays.asList("A", "H", "I", "M", "N", "O", "P", "S", "V");
        private Collection<String> m_certifiedCodes = null;
        private Set<String> m_setSectionOids = null;

        /**
         * Instantiates a new retriever teacher cert.
         */
        public FLRetrTeacherCert() {
            DataDictionaryField fieldCertStatus =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MTC_CERTIFICATION_STATUS);

            X2Criteria codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, fieldCertStatus.getReferenceTableOid());
            codesCriteria.addIn(ReferenceCode.COL_STATE_CODE, m_certifiedStateCodes);

            QueryByCriteria codesQuery = new QueryByCriteria(ReferenceCode.class, codesCriteria);

            m_certifiedCodes = new ArrayList<>();

            QueryIterator codesIterator = CRDCSectionData.this.getBroker().getIteratorByQuery(codesQuery);
            try {
                while (codesIterator.hasNext()) {
                    ReferenceCode record = (ReferenceCode) codesIterator.next();
                    String code = record.getCode();
                    m_certifiedCodes.add(code);
                }
            } finally {
                codesIterator.close();
            }

            if (fieldCertStatus != null) {
                m_setSectionOids = new HashSet();
                X2Criteria sectionCriteria = new X2Criteria();
                CRDCSectionData.this.addSectionCriteria(sectionCriteria,
                        ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER);
                X2Criteria mtcCriteria = new X2Criteria();
                mtcCriteria.addAndCriteria(sectionCriteria);
                mtcCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);
                mtcCriteria.addIn(fieldCertStatus.getJavaName(), m_certifiedCodes);

                String[] columns = new String[] {ScheduleTeacher.COL_SECTION_OID};

                ReportQueryByCriteria query = new ReportQueryByCriteria(ScheduleTeacher.class, columns, mtcCriteria);

                ReportQueryIterator iterator = CRDCSectionData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String oid = (String) record[0];
                        m_setSectionOids.add(oid);
                    }
                } finally {
                    iterator.close();
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
            boolean value = false;

            if (m_setSectionOids != null && m_setSectionOids.contains(entity.getBean().getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Returns sex allowed to attend for the section.
     *
     * @author Follett Software Company
     */
    public class RetrieverSingleSex implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            MasterSchedule section = (MasterSchedule) entity.getBean();

            if (m_fieldMstSingleSexIndicator != null) {
                value = ((CRDCReportData) data).lookupCRDCCodeByBeanPath(getBeanClass(), m_fieldMstSingleSexIndicator,
                        (String) section.getFieldValueByBeanPath(m_fieldMstSingleSexIndicator));
            }

            if (StringUtils.isEmpty(value) && m_fieldCrsSingleSexCode != null) {
                SchoolCourse sklCrs = section.getSchoolCourse();
                if (sklCrs != null) {
                    Course course = sklCrs.getCourse();
                    if (course != null) {
                        value = ((CRDCReportData) data).lookupCRDCCodeByBeanPath(course.getClass(),
                                m_fieldCrsSingleSexCode,
                                (String) course.getFieldValueByBeanPath(m_fieldCrsSingleSexCode));
                    }
                }
            }
            return value;
        }
    }

    /**
     * Returns Self Selection For AP field value.
     *
     * @author Follett Software Company
     */
    public class RetrieverSlfSelForAP implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = BooleanAsStringConverter.FALSE;

            if (m_fieldCrsCrdcAPSlfSelIndicator != null) {
                MasterSchedule section = (MasterSchedule) entity.getBean();
                SchoolCourse sklCrs = section.getSchoolCourse();
                if (sklCrs != null) {
                    Course course = sklCrs.getCourse();
                    if (course != null) {
                        String apSlfSelIndicator =
                                (String) course.getFieldValueByBeanPath(m_fieldCrsCrdcAPSlfSelIndicator);
                        if (apSlfSelIndicator != null) {
                            value = apSlfSelIndicator;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Returns if teacher certified.
     *
     * @author Follett Software Company
     */
    public class RetrieverTeacherCert implements FieldRetriever {

        /**
         * Instantiates a new retriever teacher cert.
         */
        public RetrieverTeacherCert() {
            String fieldCRDCCertified = CRDCSectionData.this.translateAliasToJavaName(ALIAS_MTC_CRDC_CERTIFIED, true);
            if (fieldCRDCCertified != null) {
                m_setSectionOids = new HashSet();
                X2Criteria criteria = new X2Criteria();
                CRDCSectionData.this.addSectionCriteria(criteria,
                        ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER);
                X2Criteria mtcCriteria = new X2Criteria();
                mtcCriteria.addAndCriteria(criteria);
                mtcCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);
                mtcCriteria.addEqualTo(fieldCRDCCertified, BooleanAsStringConverter.TRUE);

                String[] columns = new String[] {ScheduleTeacher.COL_SECTION_OID};

                ReportQueryByCriteria query = new ReportQueryByCriteria(ScheduleTeacher.class, columns, mtcCriteria);

                ReportQueryIterator iterator = CRDCSectionData.this.getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] record = (Object[]) iterator.next();
                        String oid = (String) record[0];
                        m_setSectionOids.add(oid);
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        private static final String ALIAS_MTC_CRDC_CERTIFIED = "all-mtc-CRDCCertified";

        private Set<String> m_setSectionOids = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            if (m_setSectionOids != null && m_setSectionOids.contains(entity.getBean().getOid())) {
                value = true;
            }
            return Boolean.valueOf(value);
        }
    }

    /**
     * Validate AP type.
     *
     * @author Follett Software Company
     */
    public class ValidationAPType implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value) && StringUtils.isEmpty(entity.getFieldValue("Type"))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Empty values.", "Either Type or APType should have a value"));
            } else if (!StringUtils.isEmpty(value) && !m_validCrdcApCourseCategories.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        STYLE_BOLD + value + STYLE_END + " is not valid AP Type.",
                        "AP Type = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * Validate type.
     *
     * @author Follett Software Company
     */
    public class ValidationType implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value) && !m_validCrdcCourseCategories.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        STYLE_BOLD + value + STYLE_END + " is not valid Type.",
                        "Type = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    protected static final List<String> m_validCrdcApCourseCategories =
            Collections.unmodifiableList(new ArrayList<String>(
                    Arrays.asList("AP Mathematics", "AP Science", "AP Other", "AP Computer Science", "AP Data Science")));

    protected static final List<String> m_validCrdcCourseCategories =
            Collections.unmodifiableList(new ArrayList<String>(
                    Arrays.asList("Algebra I", "Geometry", "Algebra II", "Advanced Mathematics", "Calculus", "Biology",
                            "Chemistry",
                            "Physics", "Other Mathematics", "Other Science", "Other Language", "Other Academic",
                            "Computer Science", "Data Science")));

    protected static final List<String> m_validCrdcSexAllowedToAttendCodes =
            Collections.unmodifiableList(new ArrayList<String>(
                    Arrays.asList("Male", "Female")));

    protected String m_fieldCrsCrdcAPCourseCategory = null;
    protected String m_fieldCrsCrdcAPSlfSelIndicator = null;
    protected String m_fieldCrsCrdcCourseCategory = null;
    protected String m_fieldCrsSingleSexCode = null;
    protected String m_fieldMstSingleSexIndicator = null;

    private final static String ALIAS_CRDC_AP_COURSE_CATEGORY = "all-crs-CRDCAPCourseCategory";
    private final static String ALIAS_CRDC_AP_SELF_SELECTION_IND = "all-crs-APSelfSelectionIndicator";
    private final static String ALIAS_CRDC_COURSE_CATEGORY = "all-crs-CRDCCourseCategory";
    private final static String ALIAS_SINGLE_SEX_CODE = "all-crs-CRDCSingleSexCode";
    private final static String ALIAS_SINGLE_SEX_INDICATOR = "all-mst-CRDCSingleSexCode";

    private final static String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    /**
     * @see com.x2dev.procedures.statereporting.CRDCReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        if (getSetupErrors().size() == 0) {
            initializeCtxByReportDate();

            X2Criteria sectionCriteria = new X2Criteria();

            addSectionCriteria(sectionCriteria, "");


            QueryByCriteria sectionQuery = new QueryByCriteria(MasterSchedule.class, sectionCriteria);

            setQuery(sectionQuery);
            setEntityClass(SectionCRDCEntity.class);

            CRDCDataHelper crdcHelper = new CRDCDataHelper(this);
            addCalcs(crdcHelper.getUsedRetrievers());
            addValidators(crdcHelper.getUsedValidators());
        }
    }

    /**
     * Additional section criteria.
     *
     * @param sectionCriteria X2Criteria
     * @param prefix String
     */
    void addSectionCriteria(X2Criteria sectionCriteria, String prefix) {
        sectionCriteria.addEqualTo(prefix + MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());


        if (isSchoolContext()) {
            sectionCriteria.addEqualTo(prefix + MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER +
                    SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());
        }

        /*
         * We do not need to include sections in the export that have no CRDC characteristics.
         * e.g. the value both in sex allowed to attend is not a CRDC characteristic,
         * so classes with both that do not have any other characteristic like a value in Type or
         * APType
         * do not need to be included.
         */
        X2Criteria andCriteria = new X2Criteria();

        X2Criteria courseCatCriteria = new X2Criteria();
        courseCatCriteria.addIn(prefix + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsCrdcCourseCategory,
                m_validCrdcCourseCategories);
        andCriteria.addOrCriteria(courseCatCriteria);

        X2Criteria apCourseCatCriteria = new X2Criteria();
        apCourseCatCriteria.addIn(prefix + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsCrdcAPCourseCategory,
                m_validCrdcApCourseCategories);
        andCriteria.addOrCriteria(apCourseCatCriteria);

        if (m_fieldMstSingleSexIndicator != null) {
            X2Criteria singleSexCodeMstCriteria = new X2Criteria();
            singleSexCodeMstCriteria.addIn(prefix + m_fieldMstSingleSexIndicator, m_validCrdcSexAllowedToAttendCodes);
            andCriteria.addOrCriteria(singleSexCodeMstCriteria);
        }

        if (m_fieldCrsSingleSexCode != null) {
            X2Criteria singleSexCodeCrsCriteria = new X2Criteria();
            singleSexCodeCrsCriteria.addIn(prefix + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsSingleSexCode,
                    m_validCrdcSexAllowedToAttendCodes);
            andCriteria.addOrCriteria(singleSexCodeCrsCriteria);
        }

        sectionCriteria.addAndCriteria(andCriteria);
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldCrsSingleSexCode = translateAliasToJavaName(ALIAS_SINGLE_SEX_CODE, false);
        m_fieldMstSingleSexIndicator = translateAliasToJavaName(ALIAS_SINGLE_SEX_INDICATOR, false);
        m_fieldCrsCrdcAPCourseCategory = translateAliasToJavaName(ALIAS_CRDC_AP_COURSE_CATEGORY, true);
        m_fieldCrsCrdcAPSlfSelIndicator = translateAliasToJavaName(ALIAS_CRDC_AP_SELF_SELECTION_IND, false);
        m_fieldCrsCrdcCourseCategory = translateAliasToJavaName(ALIAS_CRDC_COURSE_CATEGORY, true);
    }

    /**
     * Initialize CTX by input school year context parameter.
     */
    private void initializeCtxByReportDate() {
        String schoolYearContextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, schoolYearContextOid);

        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
        this.setCurrentContext(context);
    }
}
