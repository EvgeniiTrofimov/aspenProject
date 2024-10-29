/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * NJ export for NJSLA-S Assessment
 *
 * @author X2 Development Corporation
 */
public class NJSLASAssessmentExportData extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used for NJSLA-S exports.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NJSLASAssessmentEntity extends StateReportEntity {

        private NJSLASAssessmentExportData m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NJSLASAssessmentEntity() {
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
         * Returns the current school.
         *
         * @return SisSchool
         */
        public SisSchool getSchool() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisSchool school = studentAssessment.getSchool();

            return school;
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisStudent student = studentAssessment.getStudent();

            return student;
        }

        /**
         * This method returns the latest student enrollment record for Entry type before the report
         * date.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getStudentEnrollment() {
            return m_data.m_helper.getEnrollmentForDate(getBean().getOid(), m_data.m_dateEnd, StudentEnrollment.ENTRY);
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            NJSLASAssessmentExportData asmData = (NJSLASAssessmentExportData) data;
            StudentAssessment assessment = (StudentAssessment) bean;

            String testYear =
                    (String) assessment.getFieldValueByAliasExtended(ALIAS_ACD_TEST_YEAR,
                            asmData.m_assessmentDictionary);
            String testCycle =
                    (String) assessment.getFieldValueByAliasExtended(ALIAS_ACD_TEST_CYCLE,
                            asmData.m_assessmentDictionary);
            String asmSchoolOid = assessment.getSchoolOid();

            boolean exportAssessment =
                    asmData.m_cycle.equals(testCycle) && asmData.m_contextIdValue.equals(testYear) &&
                            (!asmData.isSchoolContext() || asmData.getSchool().getOid().equals(asmSchoolOid));

            if (!exportAssessment) {
                setRowCount(0);
            }

            super.intitialize(data, bean);
            m_data = (NJSLASAssessmentExportData) data;
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
     * Student Assessment custom retriever class
     */
    protected class RetrieveAssessment implements FieldRetriever {

        protected static final String CALC_ID = "ASSESSMENT-ALIAS";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) njslasEntity.getBean();
            String alias = (String) field.getParameter();
            String beanPath = ((NJSLASAssessmentExportData) data).m_asmAlias.get(alias);
            if (!StringUtils.isEmpty(beanPath)) {
                value = (String) studentAssessment.getFieldValueByBeanPath(beanPath);
            }
            return value;
        }
    }

    /**
     * Retrieve ELL Exempt From Taking LAL.
     */
    protected class RetrieveEllExempt implements FieldRetriever {
        public static final String CALC_ID = "ELL_EXEMPT";

        private DateFormat m_df;

        /**
         * Instantiates a new retrieve ell exempt.
         */
        public RetrieveEllExempt() {
            m_df = new SimpleDateFormat("yyyy-MM-dd");
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;

            NJSLASAssessmentEntity pnpEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            String firstEntUsSkl = (String) student.getFieldValueByAlias(ALIAS_STD_FIRST_ENTRY_DATE_IN_US_SCHOOL);

            if (!StringUtils.isEmpty(firstEntUsSkl)) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(m_dateEnd);
                cal.add(Calendar.YEAR, -1);
                PlainDate dateToCompare = new PlainDate(cal.getTime());

                try {
                    Date firstEntUsSklDate = m_df.parse(firstEntUsSkl);
                    if (firstEntUsSklDate.after(dateToCompare)) {
                        String strGrade = student.getGradeLevel();

                        // Student's in any grade level except 3-8, shall be "N".
                        if (isValidGradeForLEP(strGrade)) {
                            if (englishLearner(student)) {
                                value = CODE_YES;
                            }
                        }
                    }
                } catch (ParseException e) {
                    return value;
                }
            }

            return value;
        }

        /**
         * Check if 3<= stdGradeLevel <= 8.
         *
         * @param strGrade String
         * @return true, if is valid grade for LEP
         */
        private boolean isValidGradeForLEP(String strGrade) {
            boolean valid = false;
            if (strGrade.matches("\\d+")) {
                if (strGrade.startsWith("0")) {
                    strGrade = strGrade.replaceFirst("0", "");
                }

                int intGrade = Integer.valueOf(strGrade).intValue();

                if (3 <= intGrade && intGrade <= 8) {
                    valid = true;
                }
            }
            return valid;
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
     */
    protected class RetrieveEllStatus implements FieldRetriever {
        public static final String CALC_ID = "ELL_STATUS";
        private Map<String, Pair<PlainDate, PlainDate>> m_statusByDatesMap = new HashMap<>();
        String m_ddxParentRefusedField;

        /**
         * Instantiates a new retrieve ell status.
         */
        public RetrieveEllStatus() {
            int latestYear = getOrganization().getCurrentContext().getSchoolYear() - 1;
            int earliestYear = latestYear - 3;
            int year = latestYear;
            int statusCode = 1;
            while (year >= earliestYear) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(year));
                QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
                Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
                if (!contexts.isEmpty()) {
                    DistrictSchoolYearContext context = contexts.iterator().next();
                    PlainDate start = context.getStartDate();
                    PlainDate end = context.getEndDate();
                    if (year == latestYear) {
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(new PlainDate(OrganizationManager.getTimeZone(getOrganization())));
                        cal.add(Calendar.DAY_OF_YEAR, -1);
                        end = new PlainDate(cal.getTime());
                    }
                    Pair<PlainDate, PlainDate> dates = Pair.of(start, end);
                    m_statusByDatesMap.put("F" + statusCode, dates);
                    statusCode++;
                    year--;
                } else {
                    break;
                }
            }

            X2Criteria ddxCriteria = new X2Criteria();
            ddxCriteria.addEqualTo(SisExtendedDataDictionary.COL_ID, "PGM-LEP");
            SisExtendedDataDictionary lepDdx = (SisExtendedDataDictionary) getBroker()
                    .getBeanByQuery(new QueryByCriteria(SisExtendedDataDictionary.class, ddxCriteria));
            if (lepDdx != null) {
                DataDictionary lepDictByDdx =
                        DataDictionary.getDistrictDictionary(lepDdx, getBroker().getPersistenceKey());
                DataDictionaryField dictField =
                        lepDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_DOE_PARENT_REFUSED_SERVICES);
                if (dictField != null) {
                    m_ddxParentRefusedField = dictField.getJavaName();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            NJSLASAssessmentEntity pnpEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

            if (pgms != null && !pgms.isEmpty()) {
                value = getLepStatus(pgms.iterator().next());
            }

            return value;
        }

        /**
         * Based on Program Participation = LEP
         * E.g. for 2016-2017 year:
         * R = where pgmActionStart = > 7/1/2016 where DOE PARENT REFUSED SERVICES = True
         * Y = where pgmActionStart = > 7/1/2016 and pgmActionEnd is blank or > "End Date" input
         * parameter.
         * F1 = where pgmActionEnd is between 7/1/15 and < = "End Date" input parameter
         * F2 = where pgmActionEnd is between 7/1/14 and 6/30/15
         * F3 = where pgmActionEnd is between 7/1/13 and 6/30/14
         * F4 = where pgmActionEnd date is between 7/1/12 and 6/30/13
         *
         *
         * @param lep StudentProgramParticipation
         * @return String
         */
        private String getLepStatus(StudentProgramParticipation lep) {
            String value = null;
            PlainDate startDate = lep.getStartDate();
            PlainDate endDate = lep.getEndDate();
            if (startDate != null) {
                if (BooleanAsStringConverter.TRUE
                        .equals(lep.getFieldValueByAlias(ALIAS_DDX_DOE_PARENT_REFUSED_SERVICES))
                        || (m_ddxParentRefusedField != null && BooleanAsStringConverter.TRUE.equals(lep
                                .getFieldValueByBeanPath(m_ddxParentRefusedField)))) {
                    value = "R";
                } else if (endDate == null) {
                    value = "Y";
                } else {
                    for (Entry<String, Pair<PlainDate, PlainDate>> entry : m_statusByDatesMap.entrySet()) {
                        if (!endDate.before(entry.getValue().getLeft())
                                && !endDate.after(entry.getValue().getRight())) {
                            value = entry.getKey();
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
     */
    protected class RetrieveEnglishLearner implements FieldRetriever {
        public static final String CALC_ID = "ENG_LEARNER";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;

            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = njslasEntity.getStudent();

            if (englishLearner(student)) {
                value = CODE_YES;
            }

            return value;
        }
    }

    /**
     * The Class RetrieveFormerIEP.
     */
    /*
     * Calcalute if student is former IEP
     */
    protected class RetrieveFormerIEP implements FieldRetriever {

        protected static final String CALC_ID = "FORMER-IEP";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            String value = "";
            StudentAssessment asm = (StudentAssessment) njslasEntity.getBean();
            SisStudent student = asm.getStudent();
            PlainDate spedExitDate = student.getSpedExitDate();
            PlainDate parccTestDate = null;
            String beanPath = ((NJSLASAssessmentExportData) data).m_asmAlias.get(ALIAS_ACD_TEST_DATE);
            if (!StringUtils.isEmpty(beanPath)) {
                parccTestDate =
                        (PlainDate) m_dateConverter.parseSystemString((String) asm.getFieldValueByBeanPath(beanPath));
            }

            if (spedExitDate != null && parccTestDate != null) {
                double diff =
                        ((double) (parccTestDate.getTime() - spedExitDate.getTime())) / (1000 * 60 * 60 * 24) / 365;
                if (diff <= 1.0) {
                    value = "F1";
                } else if (diff <= 2.0) {
                    value = "F2";
                }
            }
            return value;
        }
    }

    /**
     * Retrieve Free, Reduced, Full Price Lunch.
     */
    protected class RetrieveFreeReducedLunch implements FieldRetriever {
        public static final String CALC_ID = "FARM";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CODE_NO;

            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = njslasEntity.getStudent();

            String lunchStatus = (String) student.getFieldValueByAlias(ALIAS_STD_DOE_FREE_REDUCED_LUNCH);
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
            }
            lunchStatus = data.lookupReferenceCodeByAlias(ALIAS_STD_DOE_FREE_REDUCED_LUNCH, lunchStatus,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
            }

            return value;
        }
    }

    /**
     * The Class RetrieveHomeless.
     */
    protected class RetrieveHomeless implements FieldRetriever {

        protected static final String CALC_ID = "HOMELESS";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = CODE_NO;
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) njslasEntity.getBean();
            SisStudent student = studentAssessment.getStudent();
            String code = (String) student.getFieldValueByBeanPath(m_fieldStdHomeless);
            if (!StringUtils.isEmpty(code)) {
                code = lookupStateValue(SisStudent.class, m_fieldStdHomeless, code);
                if (!StringUtils.isEmpty(code) && Arrays.asList("Y1", "Y2").contains(code)) {
                    value = CODE_YES;

                }
            }
            return value;
        }
    }

    /**
     * Retriever for gifted and talented.
     *
     */
    protected class RetrieveLocation implements FieldRetriever {

        protected static final String CALC_ID = "LOCATION";

        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_RESP_DISTRICT = "RESP-DISTRICT";
        protected static final String CALC_PARAM_RESP_SCHOOL = "RESP-SCHOOL";
        protected static final String CALC_PARAM_STATE_ID = "STATE_ID";
        protected static final String CALC_PARAM_TEST_DISTRICT = "TEST-DISTRICT";
        protected static final String CALC_PARAM_TEST_SCHOOL = "TEST-SCHOOL";

        private String m_overrideBeanPath;
        private String m_overrideReferenceTable;

        /**
         * Instantiates a new retrieve location.
         */
        public RetrieveLocation() {
            DataDictionary dictionary = getAssessmentDictionary();
            if (dictionary != null) {
                DataDictionaryField dictField =
                        dictionary.findDataDictionaryFieldByAlias(ALIAS_ACD_TEST_SCHOOL_OVERRIDE);
                if (dictField != null) {
                    m_overrideBeanPath = dictField.getJavaName();
                }
                if (dictField == null || dictField.getReferenceTableOid() == null) {
                    addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_NJ
                            + " has no reference table for alias field " + ALIAS_ACD_TEST_SCHOOL_OVERRIDE);
                } else {
                    m_overrideReferenceTable = dictField.getReferenceTableOid();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field) throws X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NJSLASAssessmentEntity pnpEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            StudentAssessment studentAssessment = (StudentAssessment) pnpEntity.getBean();
            Object value = null;
            String param = (String) field.getParameter();
            SisSchool school = student.getSchool();

            if (CALC_PARAM_STATE_ID.equals(param)) {
                value = CODE_STATE_NJ;
            } else if (CALC_PARAM_TEST_DISTRICT.equals(param)) {
                String override = getTestSchoolOverride(entity.getBean());
                if (StringUtils.isEmpty(override)) {
                    String countyCode =
                            (String) school.getParentOrganization().getFieldValueByBeanPath(m_fieldOrgCountyCode);
                    DataDictionaryField ddCountyCodeField =
                            getDataDictionaryField(Organization.class, m_fieldOrgCountyCode);
                    if (ddCountyCodeField != null && ddCountyCodeField.hasReferenceTable()) {
                        countyCode = lookupReferenceCodeByBeanPath(Organization.class, m_fieldOrgCountyCode,
                                countyCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    String districtCode = (String) school.getFieldValueByBeanPath(m_fieldSklDistrictCode);
                    DataDictionaryField ddDistrictCodeField =
                            getDataDictionaryField(SisSchool.class, m_fieldSklDistrictCode);
                    if (ddDistrictCodeField != null && ddDistrictCodeField.hasReferenceTable()) {
                        districtCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_fieldSklDistrictCode,
                                districtCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    value = countyCode + districtCode;
                } else {
                    value = override.substring(0, 6);
                }
            } else if (CALC_PARAM_TEST_SCHOOL.equals(param)) {
                String override = getTestSchoolOverride(entity.getBean());
                if (StringUtils.isEmpty(override)) {
                    String schoolCode = (String) school.getFieldValueByBeanPath(m_fieldSklCode);

                    DataDictionaryField dictionaryField = getDataDictionaryField(SisSchool.class, m_fieldSklCode);
                    if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                        schoolCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_fieldSklCode,
                                schoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    value = schoolCode;
                } else {
                    value = override.substring(6, 9);
                }
            } else if (CALC_PARAM_RESP_DISTRICT.equalsIgnoreCase(param)) {
                String overrideBeanPath = m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE);
                if (overrideBeanPath != null) {
                    String tempValue = (String) studentAssessment.getFieldValueByBeanPath(overrideBeanPath);
                    if (!StringUtils.isEmpty(tempValue)) {
                        DataDictionary dictionary = getAssessmentDictionary();
                        DataDictionaryField dictField =
                                dictionary.findDataDictionaryFieldByAlias(ALIAS_ASD_SKL_OVERRIDE);
                        if (dictField != null && dictField.getReferenceTableOid() != null) {
                            tempValue = data.lookupReferenceCodeByRefTbl(dictField.getReferenceTableOid(), tempValue,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (!StringUtils.isEmpty(tempValue) && tempValue.length() > 5) {
                                value = tempValue.substring(0, 6);
                            }
                        }
                    }
                }
            } else if (CALC_PARAM_RESP_SCHOOL.equalsIgnoreCase(param)) {
                String overrideBeanPath = m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE);
                if (overrideBeanPath != null) {
                    String tempValue = (String) studentAssessment.getFieldValueByBeanPath(overrideBeanPath);
                    if (!StringUtils.isEmpty(tempValue)) {
                        DataDictionary dictionary = getAssessmentDictionary();
                        DataDictionaryField dictField =
                                dictionary.findDataDictionaryFieldByAlias(ALIAS_ASD_SKL_OVERRIDE);
                        if (dictField != null && dictField.getReferenceTableOid() != null) {
                            tempValue = data.lookupReferenceCodeByRefTbl(dictField.getReferenceTableOid(), tempValue,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (!StringUtils.isEmpty(tempValue) && tempValue.length() > 3) {
                                value = tempValue.substring(tempValue.length() - 3);
                            }
                        }
                    }
                }
            }

            return value;
        }

        /**
         * Gets the test school override.
         *
         * @param bean X2BaseBean
         * @return String
         */
        private String getTestSchoolOverride(X2BaseBean bean) {
            String value = null;
            if (!StringUtils.isEmpty(m_overrideBeanPath)) {
                String code = (String) bean.getFieldValueByBeanPath(m_overrideBeanPath);
                if (!StringUtils.isEmpty(code)) {
                    value = lookupReferenceCodeByRefTbl(m_overrideReferenceTable, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (value != null && value.length() < 9) {
                        value = null;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for Multi Races.
     *
     */
    protected class RetrieveMultRaces implements FieldRetriever {

        protected static final String CALC_ID = "MULT_RACE";

        /**
         * Fields' positions
         */
        protected static final String FIELD_AM_IND = "RaceAmericanIndian";
        protected static final String FIELD_ASIAN = "Asian";
        protected static final String FIELD_BLACK = "BlackorAfrican";
        protected static final String FIELD_PACIFIC = "Pacific";
        protected static final String FIELD_WHITE = "White";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field) throws X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            int count = 0;

            if (CODE_YES.equals(entity.getFieldValue(FIELD_AM_IND))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_ASIAN))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_BLACK))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }
            if (CODE_YES.equals(entity.getFieldValue(FIELD_PACIFIC))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }

            if (CODE_YES.equals(entity.getFieldValue(FIELD_WHITE))) {
                count += 1;

                if (count > 1) {
                    return CODE_YES;
                }
            }

            return CODE_NO;
        }
    }

    /**
     * Retrieves the Section 504 status.
     */
    protected class RetrieveSection504 implements FieldRetriever {
        protected static final String CALC_ID = "STD_DISABIL";

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
            Object value = CODE_NO;
            FieldDefinition disabilityField = data.getFieldDefinition("PrDisabilityType");
            String primaryDisability = (String) m_disabilityRetriever.getFieldValue(data, entity, disabilityField);
            SisStudent student = ((NJSLASAssessmentEntity) entity).getStudent();

            if (!StringUtils.isEmpty(primaryDisability)) {
                value = "IEP";
            } else {
                String activeCode = PreferenceManager.getPreferenceValue(student.getSchool(),
                        SisPreferenceConstants.SECTION_504_STUDENT_ACTIVE_CODE);
                if (activeCode != null && activeCode.equalsIgnoreCase(student.getSection504StatusCode())) {
                    value = "504";
                }
            }
            return value;
        }
    }

    /**
     * Class RetrievePrimaryDisability is the FieldRetriever used to retrieve
     * the primary disability state code for the current student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePrimaryDisability implements FieldRetriever {
        protected static final String CALC_ID = "PR_DIS_TYPE";

        /**
         * Returns the corresponding alias value.
         *
         * This retriever handles both disability fields but only the HAS disability has a param
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            int referenceCodeType = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();
            if ("LOCAL".equals(field.getParameter())) {
                referenceCodeType = ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal();
            }
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            SisStudent student = njslasEntity.getStudent();

            Object value = null;
            value = lookupReferenceCodeByBeanPath(SisStudent.class,
                    m_fieldStdSpedClassification,
                    (String) student.getFieldValueByBeanPath(m_fieldStdSpedClassification),
                    referenceCodeType);

            return value == null ? "" : value;
        }
    }

    /**
     * The Class RetrieveSessionName.
     */
    /*
     * Get course name from student assessment master schedule
     */
    protected class RetrieveSessionName implements FieldRetriever {

        protected static final String CALC_ID = "SESSION-NAME";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            StringBuilder value = new StringBuilder();
            String alternateValue = null;
            String valueToReturn = null;
            StudentAssessment studentAssessment = (StudentAssessment) njslasEntity.getBean();
            if (m_asmAlias.get(ALIAS_ASM_ADMIN_ID) != null && !StringUtils
                    .isEmpty(alternateValue =
                            (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASM_ADMIN_ID)))) {
                value.append(alternateValue);
            } else {
                MasterSchedule section = studentAssessment.getMasterSchedule();
                if (section != null && section.getPrimaryStaff() != null
                        && section.getPrimaryStaff().getPerson() != null) {
                    value.append(section.getCourseView());
                    value.append(" ");
                    value.append(section.getPrimaryStaff().getPerson().getLastName());
                    value.append(" ");
                    value.append(section.getScheduleDisplay());
                }
            }
            if (value.length() > 0) {
                valueToReturn = value.toString().replace(",", "");
            }
            return valueToReturn;
        }
    }

    /**
     * Retrieves the Section 504 status.
     */
    protected class RetrieveSpecialEdPlacement implements FieldRetriever {
        protected static final String CALC_ID = "SPED_PLACEMENT";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            String placement = null;

            StudentAssessment studentAssessment = (StudentAssessment) njslasEntity.getBean();
            SisStudent student = studentAssessment.getStudent();
            placement = (String) student.getFieldValueByAlias(ALIAS_STD_SPECIAL_ED_PLACEMENT);
            if (!StringUtils.isEmpty(placement)) {
                placement = data.lookupReferenceCodeByAlias(ALIAS_STD_SPECIAL_ED_PLACEMENT, placement,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            if (StringUtils.isEmpty(placement)) {
                placement = (String) student.getFieldValueByAlias(ALIAS_STD_FEDERAL_PLACEMENT);
                if (!StringUtils.isEmpty(placement)) {
                    placement = data.lookupReferenceCodeByAlias(ALIAS_STD_FEDERAL_PLACEMENT, placement,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return placement;
        }
    }

    /**
     * The Class to retrieve test administrator Id.
     */
    protected class RetrieveTestAdminId implements FieldRetriever {

        protected static final String CALC_ID = "ADMIN-ID";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            NJSLASAssessmentEntity njslasEntity = (NJSLASAssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) njslasEntity.getBean();
            if (m_asmAlias.get(ALIAS_ASM_ADMIN_ID) != null
                    && !StringUtils.isEmpty(
                            (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASM_ADMIN_ID)))) {
                String adminId = (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASM_ADMIN_ID));
                if (adminId.length() > 7) {
                    value = adminId.substring(0, 8);
                }
            } else {
                MasterSchedule section = studentAssessment.getMasterSchedule();
                if (section != null && section.getPrimaryStaff() != null) {
                    value = section.getPrimaryStaff().getStateId();
                }
            }
            return value;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_DDX_DOE_PARENT_REFUSED_SERVICES = "DOE PARENT REFUSED SERVICES";
    protected static final String ALIAS_STD_FIRST_ENTRY_DATE_IN_US_SCHOOL = "DOE FIRST ENT DATE US SC";
    protected static final String ALIAS_ACD_ASSESSMENT_NAME = "DOE STATE ASSESSMENT NAME";
    protected static final String ALIAS_ACD_EXCLUDE_STUDENT = "NJSLA-SEXCLUDESTUDENT";
    protected static final String ALIAS_ACD_TEST_CYCLE = "NJSLA-STSTCYCLE";
    protected static final String ALIAS_ACD_TEST_CODE = "NJSLA-STSTCODE";
    protected static final String ALIAS_ACD_TEST_DATE = "NJSLA-STSTDATE";
    protected static final String ALIAS_ACD_TEST_FORMAT = "NJSLA-STSTFORMAT";
    protected static final String ALIAS_ACD_TEST_SCHOOL_OVERRIDE = "DOE TEST SCHOOL OVERRIDE";
    protected static final String ALIAS_ACD_TEST_YEAR = "NJSLA-STSTYEAR";
    protected static final String ALIAS_ASM_ADMIN_ID = "DOE TEST ADMINISTRATOR ID";
    protected static final String ALIAS_ASD_SKL_OVERRIDE = "DOE ACCOUNT SCHOOL OVERRIDE";
    protected static final String ALIAS_ORG_COUNTY_CODE = "DOE COUNTY CODE";
    protected static final String ALIAS_STD_DOE_FREE_REDUCED_LUNCH = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_SKL_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_ENR_RESIDING_SKL = "DOE RECEIVING SCHOOL";
    protected static final String ALIAS_SKL_SCHOOL_CODE = "DOE SCHOOL CODE";
    protected static final String ALIAS_STD_FEDERAL_PLACEMENT = "DOE FEDERAL PLACEMENT";
    protected static final String ALIAS_STD_SPECIAL_ED_PLACEMENT = "DOE SPECIAL ED PLACEMENT";
    protected static final String ALIAS_STD_HOMELESS = "DOE HOMELESS";
    protected static final String ALIAS_STD_SPED_CLASSIFICATION = "DOE SPECIAL ED CLASSIFICATION";

    /**
     * Other helper constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_NJ = "NJSLA-S";
    protected static final String[] CODE_LUNCH_STATUS = {"F", "Free", "R", "Reduced"};
    protected static final String CODE_NO = "N";
    protected static final String CODE_PROGRAM_LEP = "LEP";
    protected static final String CODE_STATE_NJ = "NJ";
    protected static final String CODE_YES = "Y";

    /**
     * Error messages
     */
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_MESSAGE_NO_ACTIVE_STUDENTS =
            "No students were active in the previous School Year.";
    protected static final String ERROR_TYPE_WARNING = "Warning";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No NJSLA-S Student Assessment's were created by the selected students.";

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_CTX = "schoolYearContext";
    protected static final String PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    protected static final String PARAM_CYCLE = "cycle";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";

    protected static final String REF_TABLE_NAME_EXEMPT = "DOE STATE ASSESSMENT EXEMPTION CODE";

    protected Map<String, String> m_asmAlias = new HashMap();
    protected String m_contextIdValue;
    protected String m_cycle;
    protected DateAsStringConverter m_dateConverter;
    protected FieldRetriever m_disabilityRetriever;
    protected String m_fieldEnrResidingSkl;

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected PlainDate m_dateEnd;
    protected PlainDate m_dateStart;
    protected String m_fieldOrgCountyCode;
    protected String m_fieldSklDistrictCode;
    protected String m_fieldSklCode;
    protected String m_fieldStdHomeless;
    protected String m_fieldStdSpedClassification;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_refTableNameOidMap;
    protected Boolean m_removeHeader;
    protected Map<String, Collection<StudentProgramParticipation>> m_validLEPPgmsMap;
    private DataDictionary m_assessmentDictionary;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        // System Parameters
        if (m_dateStart == null) {
            m_dateStart = getOrganization().getCurrentContext().getStartDate();
        }

        if (m_dateEnd == null) {
            m_dateEnd = new PlainDate();
        }

        m_removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (m_removeHeader == null) {
            m_removeHeader = Boolean.valueOf(false);
        }

        DistrictSchoolYearContext ctx =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) getParameter(PARAM_CTX));
        m_contextIdValue = ctx.getFieldValueByBeanPath((String) getParameter(PARAM_CTX_BEAN_PATH)).toString();

        m_cycle = (String) getParameter(PARAM_CYCLE);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_dateStart);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_dateEnd);

        QueryByCriteria stdQueryBy = m_helper.getStudentQuery(false);
        int studentCount = getBroker().getCount(stdQueryBy);
        if (studentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_ACTIVE_STUDENTS);
        }

        loadAliasAssessmentDefinition();

        // Check Student Assessment count
        X2Criteria studentAssessmentCriteria = getStudentAssessmentCriteria();
        QueryByCriteria studentAssessmentQuery = m_helper.getStudentSelectionQuery(StudentAssessment.class,
                studentAssessmentCriteria, StudentAssessment.COL_STUDENT_OID);
        applyInputSort(studentAssessmentQuery, StudentAssessment.REL_STUDENT);

        int studentAssessmentCount = getBroker().getCount(studentAssessmentQuery);
        if (studentAssessmentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS);
        }

        if (getSetupErrors().size() == 0) {
            setQuery(studentAssessmentQuery);
            setEntityClass(NJSLASAssessmentEntity.class);
            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveLocation.CALC_ID, new RetrieveLocation());
            calcs.put(RetrieveMultRaces.CALC_ID, new RetrieveMultRaces());
            calcs.put(RetrieveSection504.CALC_ID, new RetrieveSection504());
            calcs.put(RetrieveSpecialEdPlacement.CALC_ID, new RetrieveSpecialEdPlacement());
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            calcs.put(RetrieveHomeless.CALC_ID, new RetrieveHomeless());
            m_disabilityRetriever = new RetrievePrimaryDisability();
            calcs.put(RetrievePrimaryDisability.CALC_ID, m_disabilityRetriever);
            calcs.put(RetrieveFreeReducedLunch.CALC_ID, new RetrieveFreeReducedLunch());
            calcs.put(RetrieveEllExempt.CALC_ID, new RetrieveEllExempt());
            calcs.put(RetrieveEnglishLearner.CALC_ID, new RetrieveEnglishLearner());
            calcs.put(RetrieveEllStatus.CALC_ID, new RetrieveEllStatus());
            calcs.put(RetrieveFormerIEP.CALC_ID, new RetrieveFormerIEP());
            calcs.put(RetrieveTestAdminId.CALC_ID, new RetrieveTestAdminId());
            super.addCalcs(calcs);

            loadLEPProgramsMap();
        }
    }

    /**
     * Returns true if student is english learner, otherwise return false.
     *
     * @param student SisStudent
     * @return boolean
     */
    protected boolean englishLearner(SisStudent student) {
        boolean englishLearner = false;

        Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

        if (pgms != null && !pgms.isEmpty()) {
            /*
             * Where DOE FIRST ENT DATE US SC is < one year from "End Date" (parameter) AND
             * student
             * has a
             * Program Participation = LEP where pgmActionEnd = blank or >"End Date" return "Y",
             * otherwise "N".
             */
            for (StudentProgramParticipation participation : pgms) {
                if (participation.getEndDate() == null ||
                        participation.getEndDate().after(m_dateEnd)) {
                    englishLearner = true;
                }
            }
        }
        return englishLearner;
    }

    /**
     * Returns reference table oid by name.
     *
     * @param refTableName String
     * @return String
     */
    protected String getRefTableOidByName(String refTableName) {
        if (m_refTableNameOidMap == null) {
            m_refTableNameOidMap = new HashMap<String, String>();
        }
        if (!m_refTableNameOidMap.containsKey(refTableName)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
            QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByQuery(query);
            if (table != null) {
                m_refTableNameOidMap.put(refTableName, table.getOid());
            }
        }
        return m_refTableNameOidMap.get(refTableName);
    }

    /**
     * Gets the assessment dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getAssessmentDictionary() {
        if (m_assessmentDictionary == null) {
            X2Criteria assessmentDefinitonCriteria = new X2Criteria();
            assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_NJ);

            QueryByCriteria assessmentDefinitonQuery =
                    new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

            AssessmentDefinition assessmentDefinition =
                    (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);
            if (assessmentDefinition == null) {
                addSetupError(ERROR_TYPE_WARNING,
                        CODE_ASSESSMENT_DEFINITION_ID_NJ + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_assessmentDictionary =
                        DataDictionary.getDistrictDictionary(assessmentDefinition, getBroker().getPersistenceKey());
            }
        }
        return m_assessmentDictionary;
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary) {
        String javaName = null;
        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        }
        return javaName;
    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentAssessmentCriteria() {
        X2Criteria studentAssessmentCriteria = new X2Criteria();

        studentAssessmentCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, CODE_ASSESSMENT_DEFINITION_ID_NJ);
        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getEndDate());

        studentAssessmentCriteria.addIn(
                StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                        SisStudent.COL_ENROLLMENT_STATUS,
                StudentManager.getActiveStudentCodeList(getOrganization()));

        String beanPath = m_asmAlias.get(ALIAS_ACD_EXCLUDE_STUDENT);
        if (!StringUtils.isEmpty(beanPath)) {
            studentAssessmentCriteria.addNotEqualTo(beanPath, BooleanAsStringConverter.TRUE);
        }

        return studentAssessmentCriteria;
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        // Load Alias database UDF names
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                true);
        m_fieldEnrResidingSkl = translateAliasToJavaName(ALIAS_ENR_RESIDING_SKL, true);
        m_fieldSklCode = translateAliasToJavaName(ALIAS_SKL_SCHOOL_CODE, true);
        m_fieldSklDistrictCode = translateAliasToJavaName(ALIAS_SKL_DISTRICT_CODE, true);
        m_fieldStdHomeless = translateAliasToJavaName(ALIAS_STD_HOMELESS, true);
        m_fieldStdSpedClassification = translateAliasToJavaName(ALIAS_STD_SPED_CLASSIFICATION, true);
        m_fieldOrgCountyCode = translateAliasToJavaName(ALIAS_ORG_COUNTY_CODE, true);
    }

    /**
     * Load map with current LEP programs keyed on stdOid.
     */
    private void loadLEPProgramsMap() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PROGRAM_LEP);

        QueryByCriteria query = m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, pgmCriteria,
                StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

        m_validLEPPgmsMap =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);

    }

    /**
     * Load Assessment Definition Alias field names.
     */
    private void loadAliasAssessmentDefinition() {

        if (getAssessmentDictionary() == null) {
            addSetupError(ERROR_TYPE_WARNING, CODE_ASSESSMENT_DEFINITION_ID_NJ + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            m_asmAlias.put(ALIAS_ACD_EXCLUDE_STUDENT,
                    getAsmJavaName(ALIAS_ACD_EXCLUDE_STUDENT, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_TEST_YEAR, getAsmJavaName(ALIAS_ACD_TEST_YEAR, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_TEST_CYCLE, getAsmJavaName(ALIAS_ACD_TEST_CYCLE, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_TEST_CODE, getAsmJavaName(ALIAS_ACD_TEST_CODE, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_TEST_FORMAT, getAsmJavaName(ALIAS_ACD_TEST_FORMAT, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_ASSESSMENT_NAME,
                    getAsmJavaName(ALIAS_ACD_ASSESSMENT_NAME, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ACD_TEST_DATE, getAsmJavaName(ALIAS_ACD_TEST_DATE, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ASM_ADMIN_ID, getAsmJavaName(ALIAS_ASM_ADMIN_ID, getAssessmentDictionary()));
            m_asmAlias.put(ALIAS_ASD_SKL_OVERRIDE, getAsmJavaName(ALIAS_ASD_SKL_OVERRIDE, getAssessmentDictionary()));
        }
    }
}
