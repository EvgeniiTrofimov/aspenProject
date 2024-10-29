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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class RICASAssessmentExport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class RICASAssessmentExport extends StateReportData {

    /**
     * The Class RICASAssessmentEntity.
     */
    public static class RICASAssessmentEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RICASAssessmentEntity() {
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
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            AssessmentDefinition assessmentDefinition = studentAssessment.getAssessmentDefinition();
            Student student = studentAssessment.getStudent();

            String name = " [" + assessmentDefinition.getId() + "]"
                    + " [" + student.getStateId() + "]"
                    + " [" + student.getLocalId() + "]"
                    + " [" + student.getNameView() + "]";

            return name;
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
     * The Class RetrieveAccommodation.
     */
    protected class RetrieveAccommodation implements FieldRetriever {
        public static final String CALC_ID = "ACCOMMODATION";
        public static final String CALC_PARAM_ASSISTIVE_TECH = "ASSISTIVE_TECH";
        public static final String CALC_PARAM_TEXT_SPEECH = "TEXT_SPEECH";

        private Map<String, Map<String, String>> m_accommodationCodeMaps = new HashMap();
        private Map<String, DataDictionary> m_mapDictionary = new HashMap();
        private Map<String, Set<String>> m_mapStudentAccommodations = new HashMap();

        /**
         * Instantiates a new retrieve accommodation.
         *
         * @param assessmentCriteria X2Criteria
         */
        public RetrieveAccommodation(X2Criteria assessmentCriteria) {
            X2Criteria criteria = new X2Criteria();
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(getIepAccommodationCriteria());
            orCriteria.addOrCriteria(get504AccommodationCriteria());
            orCriteria.addOrCriteria(getGeneralAccommodationCriteria());
            criteria.addIn(IepAccommodation.COL_STUDENT_OID,
                    new SubQuery(StudentAssessment.class, StudentAssessment.COL_STUDENT_OID, assessmentCriteria));
            criteria.addAndCriteria(orCriteria);

            QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);

            QueryIterator accommodations = getBroker().getIteratorByQuery(query);
            try {
                while (accommodations.hasNext()) {
                    IepAccommodation accommodation = (IepAccommodation) accommodations.next();
                    String stateCode = getStateValue(accommodation);
                    if (!StringUtils.isEmpty(stateCode)) {
                        Set<String> codes = m_mapStudentAccommodations.get(accommodation.getStudentOid());
                        if (codes == null) {
                            codes = new HashSet();
                            m_mapStudentAccommodations.put(accommodation.getStudentOid(), codes);
                        }
                        codes.add(stateCode);
                    }
                }
            } finally {
                accommodations.close();
            }

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
            StudentAssessment assessment = (StudentAssessment) entity.getBean();
            Set<String> codes = getAccommodationCodes(assessment.getStudentOid());

            boolean match = false;
            if (codes != null) {
                String param = (String) field.getParameter();
                String testCode = (String) assessment.getFieldValueByBeanPath(m_assessmentTestCodeField);
                if (CALC_PARAM_ASSISTIVE_TECH.equals(param)) {
                    if ((testCode.contains("ELA") && ((codes.contains("SA6") || codes.contains("SA3.2")
                            || codes.contains("EL4.2") || codes.contains("A10.2") || codes.contains("UF.3")))) ||
                            (testCode.contains("MAT") && ((codes.contains("EL4.2") || codes.contains("A10.2")
                                    || codes.contains("UF.3"))))) {
                        match = true;
                    }
                } else if (CALC_PARAM_TEXT_SPEECH.equals(param)) {
                    if ((testCode.contains("ELA") && (codes.contains("SA1.1"))) ||
                            (testCode.contains("MAT") && (codes.contains("A4")))) {
                        match = true;
                    }
                } else {
                    String[] params = param.split("\\|");
                    String[] matchCodes = params[0].split(",");
                    if (matchCodes.length > 0) {
                        for (String matchCode : matchCodes) {
                            if (codes.contains(matchCode)) {
                                match = true;
                                break;
                            }
                        }
                        if (match && params.length > 1) {
                            match = false;
                            String matchTest = params[1];
                            if (testCode.contains(matchTest)) {
                                match = true;
                            }
                        }
                    }
                }
            }
            return match ? "Y" : "";
        }

        /**
         * Gets the accommodation codes.
         *
         * @param studentOid String
         * @return Sets the
         */
        protected Set<String> getAccommodationCodes(String studentOid) {
            return m_mapStudentAccommodations.get(studentOid);
        }

        /**
         * Gets the 504 accommodation criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria get504AccommodationCriteria() {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    IepAccommodation.REL_STUDENT_ED_PLAN + ModelProperty.PATH_DELIMITER + StudentEdPlan.COL_STATUS_CODE,
                    Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));
            return criteria;
        }

        /**
         * Gets the dictionary.
         *
         * @param accommodation IepAccommodation
         * @return Data dictionary
         */
        private DataDictionary getDictionary(IepAccommodation accommodation) {
            DataDictionary dictionary = getDataDictionary();
            if (!StringUtils.isEmpty(accommodation.getExtendedDataDictionaryOid())) {
                if (!m_mapDictionary.containsKey(accommodation.getExtendedDataDictionaryOid())) {
                    SisExtendedDataDictionary extendedDictionary = accommodation.getExtendedDataDictionary();
                    if (extendedDictionary != null) {
                        dictionary = DataDictionary.getDistrictDictionary(extendedDictionary,
                                getBroker().getPersistenceKey());
                    }
                    m_mapDictionary.put(accommodation.getExtendedDataDictionaryOid(), dictionary);
                }
                dictionary = m_mapDictionary.get(accommodation.getExtendedDataDictionaryOid());
            }
            return dictionary;
        }

        /**
         * Gets the general accommodation criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getGeneralAccommodationCriteria() {
            X2Criteria criteria = new X2Criteria();
            criteria.addEmpty(IepAccommodation.COL_STUDENT_ED_PLAN_OID, getBroker().getPersistenceKey());
            criteria.addEmpty(IepAccommodation.COL_IEP_DATA_OID, getBroker().getPersistenceKey());
            criteria.addGreaterOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE,
                    getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(IepAccommodation.COL_IMPLEMENTATION_DATE, getCurrentContext().getEndDate());
            return criteria;
        }

        /**
         * Gets the iep accommodation criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getIepAccommodationCriteria() {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(IepAccommodation.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_STATUS_CODE,
                    Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
            return criteria;
        }

        /**
         * Gets the state value.
         *
         * @param accommodation IepAccommodation
         * @return String
         */
        private String getStateValue(IepAccommodation accommodation) {
            DataDictionary dictionary = getDictionary(accommodation);
            ModelProperty prop = new ModelProperty(IepAccommodation.class, IepAccommodation.COL_NAME,
                    getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            return lookupAccommodationStateCodeByRefTbl(field.getReferenceTableOid(), accommodation.getName());
        }

        /**
         * @param referenceTableOid
         * @param name
         * @return
         */
        private String lookupAccommodationStateCodeByRefTbl(String referenceTableOid, String code) {
            Map<String, String> codeMap = m_accommodationCodeMaps.get(referenceTableOid);
            if (codeMap == null) {
                codeMap = new HashMap();
                m_accommodationCodeMaps.put(referenceTableOid, codeMap);
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
                criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
                String[] columns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
                ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);
                ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (queryItr.hasNext()) {
                        Object[] row = (Object[]) queryItr.next();
                        String refCode = (String) row[0];
                        String stateCode = (String) row[1];
                        codeMap.put(refCode, stateCode);
                    }
                } finally {
                    queryItr.close();
                }
            }
            return codeMap.get(code);
        }
    }

    /**
     * The Class RetrieveAssessment.
     */
    protected class RetrieveAssessment implements FieldRetriever {
        public static final String CALC_ID = "ASM_DDX";
        public static final String CALC_PARAM_SESSION_NAME = "RICASSESSIONNAME";

        private DataDictionary m_dictionary;
        private Map<String, DataDictionaryField> m_aliasMap = new HashMap();

        /**
         * Instantiates a new retrieve assessment.
         *
         * @param asd AssessmentDefinition
         */
        public RetrieveAssessment(AssessmentDefinition asd) {
            m_dictionary = DataDictionary.getDistrictDictionary(m_asd, getBroker().getPersistenceKey());
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
            String alias = (String) field.getParameter();
            DataDictionaryField dictField = getDataField(alias);
            if (dictField != null) {
                String code = (String) entity.getBean().getFieldValueByBeanPath(dictField.getJavaName());
                String stateCode = null;
                if (dictField.hasReferenceTable()) {
                    stateCode = lookupReferenceCodeByRefTbl(dictField.getReferenceTableOid(), code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (!StringUtils.isEmpty(stateCode)) {
                    value = stateCode;
                } else {
                    value = code;
                }
            }
            if (CALC_PARAM_SESSION_NAME.equals(alias) && m_useHomeroomForSession) {
                value = ((StudentAssessment) entity.getBean()).getStudent().getHomeroom();
            }
            return value;
        }

        /**
         * Gets the java name.
         *
         * @param alias String
         * @return String
         */
        private DataDictionaryField getDataField(String alias) {
            if (!m_aliasMap.containsKey(alias)) {
                DataDictionaryField dictField = m_dictionary.findDataDictionaryFieldByAlias(alias);
                m_aliasMap.put(alias, dictField);
            }
            return m_aliasMap.get(alias);
        }

    }

    /**
     * The Class RetrieveDebug.
     */
    protected class RetrieveDebug implements FieldRetriever {
        private RetrieveAccommodation m_accommodationsRetriever;

        /**
         * Instantiates a new retrieve debug.
         *
         * @param accommodationsRetriever RetrieveAccommodation
         */
        public RetrieveDebug(RetrieveAccommodation accommodationsRetriever) {
            m_accommodationsRetriever = accommodationsRetriever;
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
            StudentAssessment assessment = (StudentAssessment) entity.getBean();
            Set<String> codes = m_accommodationsRetriever.getAccommodationCodes(assessment.getStudentOid());
            return codes == null ? "" : codes.toString();
        }

    }

    /**
     * The Class RetrieveDistrictCode.
     */
    protected class RetrieveDistrictCode implements FieldRetriever {
        public static final String CALC_ID = "DISTRICT_CODE";

        private static final String ALIAS_ADJUSTED_DISTRICT = "DOE ADJUSTED DISTRICT";
        private static final String ALIAS_REPORTING_DISTRICT = "RI Reporting District Code";

        private String m_fieldAdjustedDistrict;
        private String m_fieldReportingDistrict;


        /**
         * Instantiates a new retrieve district code.
         */
        public RetrieveDistrictCode() {
            super();

            m_fieldAdjustedDistrict = translateAliasToJavaName(ALIAS_ADJUSTED_DISTRICT, true);
            m_fieldReportingDistrict = translateAliasToJavaName(ALIAS_REPORTING_DISTRICT, true);
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
            String value = (String) getOrganization().getFieldValueByBeanPath(m_fieldReportingDistrict);
            StudentAssessment assessment = (StudentAssessment) entity.getBean();
            if (assessment.getSchool() != null) {
                String adjustedDistrict =
                        (String) assessment.getSchool().getFieldValueByBeanPath(m_fieldAdjustedDistrict);
                if (StringUtils.isEmpty(adjustedDistrict)) {
                    value = (String) assessment.getSchool().getOrganization1()
                            .getFieldValueByBeanPath(m_fieldReportingDistrict);
                } else {
                    value = adjustedDistrict;
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveTruncate.
     */
    protected class RetrieveTruncate implements FieldRetriever {
        public static final String CALC_ID = "TRUNCATE";

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
            String baseValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (baseValue != null && baseValue.length() > field.getMaxLength()) {
                baseValue = baseValue.substring(0, field.getMaxLength());
            }
            return baseValue;
        }
    }

    private static final String ALIAS_RICAS_EXCLUSION = "RICASTSTEXCLUSION";
    private static final String ALIAS_RICAS_TEST_CODE = "RICASTSTCODE";
    private static final String ALIAS_RICAS_TEST_YEAR = "RICASTSTYEAR";

    private static final String PARAM_ASM_DEF_ID = "asmDefinitionId";
    private static final String PARAM_CTX = "schoolYearContext";
    private static final String PARAM_DEBUG = "debug";
    private static final String PARAM_REMOVE_HEADER = "removeHeader";
    private static final String PARAM_USE_HOMEROOM_FOR_SN = "useHomeroomForSN";

    private AssessmentDefinition m_asd;
    private String m_assessmentExclusionField;
    private String m_assessmentTestCodeField;
    private String m_assessmentTestYearField;
    private boolean m_useHomeroomForSession;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getHeading()
     */
    @Override
    public String getHeading() {
        Boolean removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (removeHeader == null || removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initializeAsd();
        m_useHomeroomForSession = getParameter(PARAM_USE_HOMEROOM_FOR_SN) == null ? false
                : ((Boolean) getParameter(PARAM_USE_HOMEROOM_FOR_SN)).booleanValue();
        if (getSetupErrors().size() == 0) {
            X2Criteria criteria = getAssessmentCriteria();

            BeanQuery query = new BeanQuery(StudentAssessment.class, criteria);
            applyInputSort(query, StudentAssessment.REL_STUDENT);

            setQuery(query);
            setEntityClass(RICASAssessmentEntity.class);

            // Assign custom field retriever calculations.
            RetrieveAccommodation accommodationsRetriever = new RetrieveAccommodation(criteria);
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveAccommodation.CALC_ID, accommodationsRetriever);
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment(m_asd));
            calcs.put(RetrieveDistrictCode.CALC_ID, new RetrieveDistrictCode());
            calcs.put(RetrieveTruncate.CALC_ID, new RetrieveTruncate());
            super.addCalcs(calcs);

            if (getParameter(PARAM_DEBUG) != null && getParameter(PARAM_DEBUG) instanceof Boolean
                    && ((Boolean) getParameter(PARAM_DEBUG)).booleanValue()) {
                setDebugField(accommodationsRetriever);
            }
        }
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @param isRequired boolean
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary, boolean isRequired) {
        String javaName = null;
        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null) {
            javaName = dictField.getJavaName();
        } else if (isRequired) {
            addSetupError("Setup Error", "Assessment column for " + alias + " is not defined");
        }
        return javaName;
    }

    /**
     * Gets the assessment criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getAssessmentCriteria() {
        X2Criteria criteria = new X2Criteria();

        // Student assessment for correct year
        DistrictSchoolYearContext ctx =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) getParameter(PARAM_CTX));
        if (ctx == null) {
            throw new X2RuntimeException("", "A valid school year context must be selected");
        }
        criteria.addEqualTo(m_assessmentTestYearField, ctx.getContextId());

        criteria.addNotEqualTo(m_assessmentExclusionField, BooleanAsStringConverter.TRUE);

        criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, m_asd.getOid());

        applyInputCriteria(criteria, true, StudentAssessment.REL_STUDENT);

        return criteria;
    }

    /**
     * Initialize asd.
     */
    private void initializeAsd() {
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, getParameter(PARAM_ASM_DEF_ID));

        m_asd = (AssessmentDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));

        if (m_asd == null) {
            addSetupError("Setup Error", "Required assessment definition with ID = " + getParameter(PARAM_ASM_DEF_ID)
                    + " could not be found");
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(m_asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                addSetupError("Setup Error", "Extended Dictinary for assessment could not be loaded");
            } else {
                m_assessmentExclusionField = getAsmJavaName(ALIAS_RICAS_EXCLUSION, dataDictionary, true);
                m_assessmentTestCodeField = getAsmJavaName(ALIAS_RICAS_TEST_CODE, dataDictionary, true);
                m_assessmentTestYearField = getAsmJavaName(ALIAS_RICAS_TEST_YEAR, dataDictionary, true);
            }
        }
    }

    /**
     * Sets the debug field.
     *
     * @param accommodationsRetriever void
     */
    private void setDebugField(RetrieveAccommodation accommodationsRetriever) {
        FieldDefinition field = new FieldDefinition("Debug",
                null,
                null,
                0,
                0,
                1000,
                null,
                null,
                new RetrieveDebug(accommodationsRetriever),
                null,
                null);
        getFieldDefinitions().add(getFieldDefinitions().size(), field);
    }

}
