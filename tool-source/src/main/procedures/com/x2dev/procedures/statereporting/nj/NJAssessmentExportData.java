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
import com.x2dev.procedures.statereporting.AssessmentExportData;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
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
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * NJ Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class NJAssessmentExportData extends AssessmentExportData {

    /**
     * The Class RetrieveAsmName.
     */
    /*
     * Get Student Assessment field
     */
    protected class RetrieveAsmName implements FieldRetriever {
        protected static final String CALC_ID = "ASM_NAME";
        private static final String FIELD_TEST_CODE = "TestCode";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();

            if (m_asmAlias.get(ALIAS_ASSESSMENT_NAME) != null) {
                String code = (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASSESSMENT_NAME));
                if (!StringUtils.isEmpty(code)) {
                    value = lookupReferenceCodeByRefTbl(getRefTableOidByName(ALIAS_ASSESSMENT_NAME), code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            if (StringUtils.isEmpty(value)) {
                String testCode = entity.getFieldValue(FIELD_TEST_CODE);

                if (testCode != null) {
                    switch (testCode) {
                        case "ELA03":
                        case "ELA04":
                        case "ELA05":
                        case "ELA06":
                        case "ELA07":
                        case "ELA08":
                        case "ELA09":
                        case "ELA10":
                        case "ELA11":
                        case "ELA12":
                        case "MAT03":
                        case "MAT04":
                        case "MAT05":
                        case "MAT06":
                        case "MAT07":
                        case "MAT08":
                        case "ALG01":
                        case "ALG02":
                        case "GEO01":
                            value = isEllProgram(studentAssessment.getStudentOid()) ? "5" : "1";
                            break;
                        case "EXELA":
                        case "EXEK2":
                            value = "2";
                            break;
                        case "ELANT":
                        case "MATNT":
                            value = "3";
                            break;
                        default:
                            break;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is ell program.
         *
         * @param studentOid String
         * @return true, if is ell program
         */
        private boolean isEllProgram(String studentOid) {
            boolean value = false;
            Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(studentOid);
            if (pgms != null && !pgms.isEmpty()) {
                for (StudentProgramParticipation pgm : pgms) {
                    if (pgm.getEndDate() == null) {
                        value = true;
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveAssessment.
     */
    /*
     * Get Student Assessment field
     */
    protected class RetrieveAssessment implements FieldRetriever {
        protected static final String CALC_ID = "PARCC-TEST";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            String alias = (String) field.getParameter();
            String beanPath = ((NJAssessmentExportData) data).m_asmAlias.get(alias);
            if (!StringUtils.isEmpty(beanPath)) {
                value = (String) studentAssessment.getFieldValueByBeanPath(beanPath);

                if (ALIAS_PARCC_PARCCEXPPASS.equals(field.getParameter())) {
                    value = lookupReferenceCodeByRefTbl(getRefTableOidByName(REF_TABLE_NAME_EXEMPT), value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveCourseName.
     */
    /*
     * Get course name from student assessment master schedule
     */
    protected class RetrieveCourseName implements FieldRetriever {

        protected static final String CALC_ID = "COURSE-NAME";

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = "";

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && !StringUtils.isEmpty(section.getCourseView())) {
                value += section.getCourseView();
                if (!StringUtils.isEmpty(section.getRoomView())) {
                    value += "-";
                    value += section.getRoomView();
                }
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

            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();

            String firstEntUsSkl = (String) student.getFieldValueByAlias(ALIAS_FIRST_ENTRY_DATE_IN_US_SCHOOL);

            if (!StringUtils.isEmpty(firstEntUsSkl)) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(m_endDate);
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
            int year = 2018;
            int statusCode = 1;
            while (year > 2014) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(year));
                QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
                Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
                if (!contexts.isEmpty()) {
                    DistrictSchoolYearContext context = contexts.iterator().next();
                    PlainDate start = context.getStartDate();
                    PlainDate end = context.getEndDate();
                    if (year == 2018) {
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
                        lepDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DOE_PARENT_REFUSED_SERVICES);
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

            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
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
                        .equals(lep.getFieldValueByAlias(ALIAS_DOE_PARENT_REFUSED_SERVICES))
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

            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String value = "";
            StudentAssessment asm = (StudentAssessment) parccEntity.getBean();
            SisStudent student = asm.getStudent();
            PlainDate spedExitDate = student.getSpedExitDate();
            PlainDate parccTestDate = null;
            String beanPath = ((NJAssessmentExportData) data).m_asmAlias.get(ALIAS_PARCC_TESTDATE);
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
        public static final String CALC_ID = "PARCC-FARM";

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

            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

            String lunchStatus = (String) student.getFieldValueByAlias(ALIAS_LUNCH_STATUS);
            if (Arrays.asList(CODE_LUNCH_STATUS).contains(lunchStatus)) {
                value = CODE_YES;
            }
            lunchStatus = data.lookupReferenceCodeByAlias(ALIAS_LUNCH_STATUS, lunchStatus,
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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            SisStudent student = studentAssessment.getStudent();
            String code = (String) student.getFieldValueByBeanPath(m_stdHomeless);
            if (!StringUtils.isEmpty(code)) {
                code = lookupStateValue(SisStudent.class, m_stdHomeless, code);
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
        private ReferenceTable m_overrideReferenceTable;

        /**
         * Instantiates a new retrieve location.
         */
        public RetrieveLocation() {
            DataDictionary dictionary = getParccDictionary();
            if (dictionary != null) {
                DataDictionaryField dictField = dictionary.findDataDictionaryFieldByAlias(ALIAS_TEST_SCHOOL_OVERRIDE);
                if (dictField != null) {
                    m_overrideBeanPath = dictField.getJavaName();
                }
                if (dictField == null || dictField.getReferenceTable() == null) {
                    addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId()
                            + " has no reference tablee for alias field " + ALIAS_TEST_SCHOOL_OVERRIDE);
                } else {
                    m_overrideReferenceTable = dictField.getReferenceTable();
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
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            StudentAssessment studentAssessment = (StudentAssessment) pnpEntity.getBean();
            Object value = null;
            String param = (String) field.getParameter();
            SisSchool school = student.getSchool();

            if (CALC_PARAM_STATE_ID.equals(param)) {
                value = STATE_CODE_NJ;
            } else if (CALC_PARAM_TEST_DISTRICT.equals(param)) {
                String override = getTestSchoolOverride(entity.getBean());
                if (StringUtils.isEmpty(override)) {
                    String countyCode =
                            (String) school.getParentOrganization().getFieldValueByBeanPath(m_countyCodeField);
                    DataDictionaryField ddCountyCodeField =
                            getDataDictionaryField(Organization.class, m_countyCodeField);
                    if (ddCountyCodeField != null && ddCountyCodeField.hasReferenceTable()) {
                        countyCode = lookupReferenceCodeByBeanPath(Organization.class, m_countyCodeField,
                                countyCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    String districtCode = (String) school.getFieldValueByBeanPath(m_districtCodeField);
                    DataDictionaryField ddDistrictCodeField =
                            getDataDictionaryField(SisSchool.class, m_districtCodeField);
                    if (ddDistrictCodeField != null && ddDistrictCodeField.hasReferenceTable()) {
                        districtCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_districtCodeField,
                                districtCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    value = countyCode + districtCode;
                } else {
                    value = override.substring(0, 6);
                }
            } else if (CALC_PARAM_TEST_SCHOOL.equals(param)) {
                String override = getTestSchoolOverride(entity.getBean());
                if (StringUtils.isEmpty(override)) {
                    String schoolCode = (String) school.getFieldValueByBeanPath(m_schoolCodeField);

                    DataDictionaryField dictionaryField = getDataDictionaryField(SisSchool.class, m_schoolCodeField);
                    if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                        schoolCode = lookupReferenceCodeByBeanPath(SisSchool.class, m_schoolCodeField,
                                schoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                    value = schoolCode;
                } else {
                    value = override.substring(6, 9);
                }
            } else if (CALC_PARAM_RESP_DISTRICT.equalsIgnoreCase(param)) {
                String tempValue = null;
                if (m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE) != null) {
                    tempValue =
                            (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE));
                    if (!StringUtils.isEmpty(tempValue) && tempValue.length() > 5) {
                        value = tempValue.substring(0, 6);
                    }
                }
                if (value == null) {
                    StudentEnrollment entryEnr =
                            m_helper.getEnrollmentForDate(student.getOid(), m_endDate, StudentEnrollment.ENTRY);
                    if (entryEnr != null) {
                        String code = (String) entryEnr.getFieldValueByBeanPath(m_enrResidingSkl);
                        if (!StringUtils.isEmpty(code)) {
                            code = lookupStateValue(StudentEnrollment.class, m_enrResidingSkl, code);

                            if (!StringUtils.isEmpty(code) && code.length() > 5) {
                                value = code.substring(0, 6);
                            }
                        }
                    }
                }
            } else if (CALC_PARAM_RESP_SCHOOL.equalsIgnoreCase(param)) {
                String tempValue = null;
                if (m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE) != null) {
                    tempValue =
                            (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_ASD_SKL_OVERRIDE));
                    if (!StringUtils.isEmpty(tempValue) && tempValue.length() > 3) {
                        value = tempValue.substring(tempValue.length() - 3);
                    }
                }
                if (value == null) {
                    StudentEnrollment entryEnr =
                            m_helper.getEnrollmentForDate(student.getOid(), m_endDate, StudentEnrollment.ENTRY);
                    if (entryEnr != null) {
                        String code = (String) entryEnr.getFieldValueByBeanPath(m_enrResidingSkl);
                        if (!StringUtils.isEmpty(code)) {
                            code = lookupStateValue(StudentEnrollment.class, m_enrResidingSkl, code);

                            if (!StringUtils.isEmpty(code) && code.length() > 3) {
                                value = code.substring(code.length() - 3);
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
                    value = lookupReferenceCodeByRefTbl(m_overrideReferenceTable.getOid(), code,
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
        protected static final String FIELD_ASIAN = "Race Asian";
        protected static final String FIELD_BLACK = "Race Black";
        protected static final String FIELD_PACIFIC = "Race Pacific";
        protected static final String FIELD_WHITE = "Race White";

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
            SisStudent student = ((AssessmentEntity) entity).getStudent();

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            SisStudent student = parccEntity.getStudent();

            Object value = null;
            value = lookupReferenceCodeByBeanPath(SisStudent.class,
                    m_stdSpedClassification,
                    (String) student.getFieldValueByBeanPath(m_stdSpedClassification),
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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            StringBuilder value = new StringBuilder();
            String alternateValue = null;
            String valueToReturn = null;
            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();

            if (!StringUtils
                    .isEmpty(alternateValue =
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_SESSION_NAME_OVERRIDE)))) {
                return alternateValue;
            }

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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            String placement = null;

            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
            SisStudent student = studentAssessment.getStudent();
            placement = (String) student.getFieldValueByAlias(ALIAS_SPECIAL_ED_PLACEMENT);
            if (!StringUtils.isEmpty(placement)) {
                placement = data.lookupReferenceCodeByAlias(ALIAS_SPECIAL_ED_PLACEMENT, placement,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            if (StringUtils.isEmpty(placement)) {
                placement = (String) student.getFieldValueByAlias(ALIAS_FEDERAL_PLACEMENT);
                if (!StringUtils.isEmpty(placement)) {
                    placement = data.lookupReferenceCodeByAlias(ALIAS_FEDERAL_PLACEMENT, placement,
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
            AssessmentEntity parccEntity = (AssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) parccEntity.getBean();
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
     * Retriever for fields with Y/N values except fields concerning assessments.
     *
     */
    protected class RetrieveYN implements FieldRetriever {

        protected static final String CALC_ID = "YN_RETRIEVE";

        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_ECON_DISADVAN_STATUS = "ECON_DISADVAN_STATUS";
        protected static final String CALC_PARAM_PR_DISAB_TYPE = "PR_DISAB_TYPE";

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
            String value = CODE_NO;
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            String param = (String) field.getParameter();
            if (CALC_PARAM_ECON_DISADVAN_STATUS.equalsIgnoreCase(param)) {
                if ("F".equals(student.getFieldValueByAlias(ALIAS_DOE_FREE_REDUCED_LUNCH)) ||
                        "R".equals(student.getFieldValueByAlias(ALIAS_DOE_FREE_REDUCED_LUNCH))) {
                    value = CODE_YES;
                }
            }
            return value;
        }
    }

    /*
     * Aliases
     */
    // ASSESSMENT
    protected static final String ALIAS_ASM_ADMIN_ID = "DOE TEST ADMINISTRATOR ID";
    protected static final String ALIAS_ASSESSMENT_NAME = "DOE STATE ASSESSMENT NAME";
    protected static final String ALIAS_PARCC_TESTDATE = "PARCCTSTDATE";
    protected static final String ALIAS_TEST_SCHOOL_OVERRIDE = "DOE TEST SCHOOL OVERRIDE";
    protected static final String ALIAS_SESSION_NAME_OVERRIDE = "DOE SESSION NAME OVERRIDE";

    // ASSESMENT DEFINITION
    protected static final String ALIAS_ASD_SKL_OVERRIDE = "DOE ACCOUNT SCHOOL OVERRIDE";


    // ORGANIZATION TABLE
    protected static final String ALIAS_COUNTY_CODE = "DOE COUNTY CODE";

    // COURSE Table
    protected static final String ALIAS_TEST_CODE = "all-crs-PARCCTestCode";

    // SCHOOL Table
    protected static final String ALIAS_DOE_FREE_REDUCED_LUNCH = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_ENR_RESIDING_SKL = "DOE RECEIVING SCHOOL";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";

    // STUDENT Table
    protected static final String ALIAS_ESL_FLAG = "ESL Flag";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_FEDERAL_PLACEMENT = "DOE FEDERAL PLACEMENT";
    protected static final String ALIAS_LUNCH_STATUS = "DOE FREE REDUCED LUNCH";
    protected static final String ALIAS_SPECIAL_ED_PLACEMENT = "DOE SPECIAL ED PLACEMENT";
    protected static final String ALIAS_STD_HOMELESS = "DOE HOMELESS";
    protected static final String ALIAS_STD_SPED_CLASSIFICATION = "DOE SPECIAL ED CLASSIFICATION";
    protected static final String ALIAS_TESTING_COUNTY = "DOE TESTING COUNTY";

    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_NJ = "NJ PARCC";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String ERROR_MESSAGE_STATE_CODE_NOT_DEFINED =
            " System State Code (sys.state) is not set in System Preferences";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No NJ PARCC Student Assessment's were created by the selected students.";
    protected static final String STATE_CODE_NJ = "NJ";
    protected static final String CODE_ONE = "1";
    protected static final String[] CODE_LUNCH_STATUS = {"F", "Free", "R", "Reduced"};
    protected static final String LEP_TEST_NOT_EXEMPT = "N";
    protected static final String LEP_TEST_EXEMPT = "Y";
    protected static final String REF_TABLE_NAME_EXEMPT = "DOE STATE ASSESSMENT EXEMPTION CODE";


    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_assessmentRetestField;
    protected String m_countyCodeField;
    protected DateAsStringConverter m_dateConverter;
    protected FieldRetriever m_disabilityRetriever;
    protected String m_districtCodeField;
    protected Map<String, String> m_refTableNameOidMap;
    protected String m_enrResidingSkl;
    protected String m_ResidingSklByAsm;
    protected String m_schoolCodeField;
    protected int m_schoolYear;
    protected String m_stdHomeless;
    protected String m_stdSpedClassification;
    protected String m_testCodeField;

    private DataDictionary m_parccDictionary;

    /**
     * Returns true if student is english learner, otherwise return false.
     *
     * @param student SisStudent
     * @return boolean
     */
    public boolean englishLearner(SisStudent student) {
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
                        participation.getEndDate().after(m_endDate)) {
                    englishLearner = true;
                }
            }
        }
        return englishLearner;
    }

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
     * Returns reference table oid by name.
     *
     * @param refTableName String
     * @return String
     */
    public String getRefTableOidByName(String refTableName) {
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
     * Initialize.
     *
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        super.initialize();

        if (getSetupErrors().size() == 0) {
            // Return assessments within the date range entered
            Criteria criteria = getQuery().getCriteria();
            criteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, m_startDate);
            criteria.addLessOrEqualThan(StudentAssessment.COL_DATE, m_endDate);
            criteria.addIn(
                    StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                            SisStudent.COL_ENROLLMENT_STATUS,
                    StudentManager.getActiveStudentCodeList(getOrganization()));
            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);
            setQuery(query);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveLocation.CALC_ID, new RetrieveLocation());
            calcs.put(RetrieveMultRaces.CALC_ID, new RetrieveMultRaces());
            calcs.put(RetrieveYN.CALC_ID, new RetrieveYN());
            calcs.put(RetrieveSection504.CALC_ID, new RetrieveSection504());
            calcs.put(RetrieveSpecialEdPlacement.CALC_ID, new RetrieveSpecialEdPlacement());
            calcs.put(RetrieveAssessment.CALC_ID, new RetrieveAssessment());
            calcs.put(RetrieveCourseName.CALC_ID, new RetrieveCourseName());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            calcs.put(RetrieveHomeless.CALC_ID, new RetrieveHomeless());
            m_disabilityRetriever = new RetrievePrimaryDisability();
            calcs.put(RetrievePrimaryDisability.CALC_ID, m_disabilityRetriever);
            calcs.put(RetrieveFreeReducedLunch.CALC_ID, new RetrieveFreeReducedLunch());
            calcs.put(RetrieveEllExempt.CALC_ID, new RetrieveEllExempt());
            calcs.put(RetrieveEnglishLearner.CALC_ID, new RetrieveEnglishLearner());
            calcs.put(RetrieveEllStatus.CALC_ID, new RetrieveEllStatus());
            calcs.put(RetrieveAsmName.CALC_ID, new RetrieveAsmName());
            calcs.put(RetrieveFormerIEP.CALC_ID, new RetrieveFormerIEP());
            calcs.put(RetrieveTestAdminId.CALC_ID, new RetrieveTestAdminId());
            super.addCalcs(calcs);

            loadPARCCAssessmentDefinition();
        }
    }

    /**
     * Gets the asm definition id.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAsmDefinitionId()
     */
    @Override
    protected String getAsmDefinitionId() {
        return CODE_ASSESSMENT_DEFINITION_ID_NJ;
    }

    /**
     * Gets the no student assessment message.
     *
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getNoStudentAssessmentMessage()
     */
    @Override
    protected String getNoStudentAssessmentMessage() {
        return ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS;
    }

    /**
     * Gets the parcc dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getParccDictionary() {
        if (m_parccDictionary == null) {
            X2Criteria assessmentDefinitonCriteria = new X2Criteria();
            assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, getAsmDefinitionId());

            QueryByCriteria assessmentDefinitonQuery =
                    new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

            AssessmentDefinition parccDefinition =
                    (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);
            if (parccDefinition == null) {
                addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_parccDictionary =
                        DataDictionary.getDistrictDictionary(parccDefinition, getBroker().getPersistenceKey());
            }
        }
        return m_parccDictionary;
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
     * initialize Fields.
     */
    private void initializeFields() {
        // Load Alias database UDF names
        m_countyCodeField = translateAliasToJavaName(ALIAS_COUNTY_CODE, true);
        m_schoolCodeField = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_enrResidingSkl = translateAliasToJavaName(ALIAS_ENR_RESIDING_SKL, true);
        m_districtCodeField = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_testCodeField = translateAliasToJavaName(ALIAS_TEST_CODE, true);
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                true);
        m_stdHomeless = translateAliasToJavaName(ALIAS_STD_HOMELESS, true);
        m_stdSpedClassification = translateAliasToJavaName(ALIAS_STD_SPED_CLASSIFICATION, true);
    }


    /**
     * Load PARCC Assessment Definition Alias field names.
     */
    private void loadPARCCAssessmentDefinition() {
        DataDictionary dataDictionary = getParccDictionary();

        if (dataDictionary == null) {
            addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            m_asmAlias.put(ALIAS_ASSESSMENT_NAME, getAsmJavaName(ALIAS_ASSESSMENT_NAME, getParccDictionary()));
            m_asmAlias.put(ALIAS_PARCC_TESTDATE, getAsmJavaName(ALIAS_PARCC_TESTDATE, getParccDictionary()));
            m_asmAlias.put(ALIAS_ASM_ADMIN_ID, getAsmJavaName(ALIAS_ASM_ADMIN_ID, getParccDictionary()));
            m_asmAlias.put(ALIAS_ASD_SKL_OVERRIDE, getAsmJavaName(ALIAS_ASD_SKL_OVERRIDE, getParccDictionary()));
            m_asmAlias.put(ALIAS_SESSION_NAME_OVERRIDE,
                    getAsmJavaName(ALIAS_SESSION_NAME_OVERRIDE, getParccDictionary()));
        }
    }
}
