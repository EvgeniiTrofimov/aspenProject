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
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
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
import java.util.ArrayList;
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
 * NJ Assessment export for NJSLA - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class NJSLAAssessmentExportData extends AssessmentExportData {

    /**
     * The Class NJSLAAssessmentEntity.
     */
    public static class NJSLAAssessmentEntity extends AssessmentEntity {

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
            NJSLAAssessmentExportData asmData = (NJSLAAssessmentExportData) data;
            StudentAssessment assessment = (StudentAssessment) bean;
            String testCode = asmData.getTestCode(assessment);
            boolean exportAssessment = !StringUtils.isEmpty(testCode) && asmData.m_testCodes.contains(testCode);
            if (!exportAssessment) {
                setRowCount(0);
            }
        }
    }

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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();

            if (m_asmAlias.get(ALIAS_NJSLA_ASM_NAME) != null) {
                String code = (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ASM_NAME));
                String rtbOid = m_asmRtbOid.get(ALIAS_NJSLA_ASM_NAME);
                if (!StringUtils.isEmpty(code) && !StringUtils.isEmpty(rtbOid)) {
                    value = lookupReferenceCodeByRefTbl(rtbOid, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            if (StringUtils.isEmpty(value)) {
                String testCode = entity.getFieldValue(FIELD_TEST_CODE);

                if (testCode != null) {
                    switch (testCode) {
                        case "EXELA":
                        case "EXEK2":
                            value = "2";
                            break;
                        case "ELANT":
                        case "MATNT":
                        case "SCNT":
                            value = "3";
                            break;
                        default:
                            value = isEllProgram(studentAssessment.getStudentOid()) ? "5" : "1";
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
            Collection<StudentProgramParticipation> pgms = m_pgmsLEPMap.get(studentOid);
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
        protected static final String CALC_ID = "NJSLA-TEST";

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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            String value = null;

            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();
            String alias = (String) field.getParameter();
            String beanPath = ((NJSLAAssessmentExportData) data).m_asmAlias.get(alias);
            if (!StringUtils.isEmpty(beanPath)) {
                value = (String) studentAssessment.getFieldValueByBeanPath(beanPath);

                if (ALIAS_NJSLA_EXPPASS.equals(field.getParameter())) {
                    value = lookupReferenceCodeByRefTbl(getRefTableOidByName(REF_TABLE_NAME_EXEMPT), value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (ALIAS_FIRST_HS_MATH.equals(field.getParameter())) {
                    if (value == null) {
                        value = "N";
                    }
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            String value = "";

            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();
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
            int year = 2023;
            int monthStart = Calendar.JULY;
            int monthEnd = Calendar.JUNE;
            int dayStart = 1;
            int dayEnd = 30;
            PlainDate startDate = null;
            PlainDate endDate = null;
            Calendar calStart = Calendar.getInstance();
            Calendar calEnd = Calendar.getInstance();
            int statusCode = 1;
            while (statusCode < 5) {
                if (2024 == year) {
                    calEnd.setTime(new PlainDate(OrganizationManager.getTimeZone(getOrganization())));
                    calEnd.add(Calendar.DAY_OF_YEAR, -1);
                    endDate = new PlainDate(calEnd.getTime());
                } else {
                    calEnd.set(year, monthEnd, dayEnd);
                    endDate = new PlainDate(calEnd.getTime());
                }
                year--;
                calStart.set(year, monthStart, dayStart);
                startDate = new PlainDate(calStart.getTime());
                Pair<PlainDate, PlainDate> dates = Pair.of(startDate, endDate);
                m_statusByDatesMap.put("F" + statusCode, dates);
                statusCode++;
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
            Collection<StudentProgramParticipation> pgms = m_pgmsLEPMap.get(student.getOid());
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            SisStudent student = njslaEntity.getStudent();
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
    protected class RetrieveFirstDateInUS implements FieldRetriever {

        protected static final String CALC_ID = "FIRST-DATE-US";

        private static final String ALIAS = "DOE FIRST ENT DATE US SC";
        private static final String INPUT_PARAM = "firstUSDateFormat";

        private String m_beanPath;
        private SimpleDateFormat m_dateFormatter = new SimpleDateFormat("yyyy/MM/dd");

        /**
         *
         */
        public RetrieveFirstDateInUS() {
            super();
            m_beanPath = NJSLAAssessmentExportData.this.translateAliasToJavaName(ALIAS, true);
            String parameter = (String) NJSLAAssessmentExportData.this.getParameter(INPUT_PARAM);
            String dateFormat = "yyyy-MM-dd";
            if (parameter != null) {
                dateFormat = parameter;
            }
            m_dateFormatter = new SimpleDateFormat(dateFormat);
        }

        /**
         * @throws X2BaseException
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentAssessment asm = (StudentAssessment) entity.getBean();
            SisStudent student = asm.getStudent();
            data.translateAliasToJavaName(ALIAS, true);
            PlainDate date = (PlainDate) data.getPropertyAsJavaType(student, m_beanPath);

            Object value = null;
            if (date != null) {
                value = m_dateFormatter.format(date);
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            String value = "";
            StudentAssessment asm = (StudentAssessment) njslaEntity.getBean();
            SisStudent student = asm.getStudent();
            PlainDate spedExitDate = student.getSpedExitDate();
            PlainDate parccTestDate = null;
            String beanPath = ((NJSLAAssessmentExportData) data).m_asmAlias.get(ALIAS_PARCC_TESTDATE);
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

            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            SisStudent student = njslaEntity.getStudent();

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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();
            SisStudent student = studentAssessment.getStudent();
            String code = (String) student.getFieldValueByBeanPath(m_stdHomeless);
            if (!StringUtils.isEmpty(code)) {
                value = lookupStateValue(SisStudent.class, m_stdHomeless, code);
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
        protected static final String CALC_PARAM_TEST_ADMIN = "TEST-ADMIN";

        private String m_overrideBeanPath;
        private ReferenceTable m_overrideReferenceTable;

        /**
         * Instantiates a new retrieve location.
         */
        public RetrieveLocation() {
            DataDictionary dictionary = getNJSLADictionary();
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

            // takes the input paramter "Cycle Selection" and converts it to the corresponding state
            // code
            if (CALC_PARAM_TEST_ADMIN.equals(param)) {
                if (m_cycle.equals(CYCLE_FALL_TEXT)) {
                    value = CYCLE_FALL_STATE_CODE;
                } else if (m_cycle.equals(CYCLE_SUMMER_TEXT)) {
                    value = CYCLE_SUMMER_STATE_CODE;
                } else if (m_cycle.equals(CYCLE_SPRING_TEXT)) {
                    value = CYCLE_SPRING_STATE_CODE;
                } else if (m_cycle.equals(CYCLE_ACCESS_DLM_SPRING_TEXT)) {
                    value = CYCLE_ACCESS_DLM_SPRING_STATE_CODE;
                }
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
                if (m_asmAlias.get(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE) != null) {
                    tempValue =
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE));
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
                if (m_asmAlias.get(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE) != null) {
                    tempValue =
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE));
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            SisStudent student = njslaEntity.getStudent();

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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            StringBuilder value = new StringBuilder();
            String alternateValue = null;
            String valueToReturn = null;
            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();

            if (!StringUtils
                    .isEmpty(alternateValue =
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_SESSION_NAME_OVERRIDE)))) {
                return alternateValue;
            }

            if (m_asmAlias.get(ALIAS_NJSLA_ASM_ADMIN_ID) != null && !StringUtils
                    .isEmpty(alternateValue =
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ASM_ADMIN_ID)))) {
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
                valueToReturn = value.toString().replaceAll("[(/]", "-").replaceAll("[),]", "");
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            String placement = null;

            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();
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
            AssessmentEntity njslaEntity = (AssessmentEntity) entity;
            StudentAssessment studentAssessment = (StudentAssessment) njslaEntity.getBean();
            if (m_asmAlias.get(ALIAS_NJSLA_ASM_ADMIN_ID) != null
                    && !StringUtils.isEmpty(
                            (String) studentAssessment
                                    .getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ASM_ADMIN_ID)))) {
                String adminId =
                        (String) studentAssessment.getFieldValueByBeanPath(m_asmAlias.get(ALIAS_NJSLA_ASM_ADMIN_ID));
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
    protected static final String ALIAS_NJSLA_EXPPASS = "all-asm-ExemptfromPassing";
    protected static final String ALIAS_NJSLA_TSTCODE = "all-asm-NJTestCode";
    protected static final String ALIAS_NJSLA_TSTFORMAT = "all-asm-TestFormat";
    protected static final String ALIAS_NJSLA_TESTPERIOD = "all-asm-TestPeriod";
    protected static final String ALIAS_NJSLA_TESTYEAR = "all-asm-AssessmentYear";
    protected static final String ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE = "all-asm-AccountSchOverride";
    protected static final String ALIAS_NJSLA_ASM_ADMIN_ID = "all-asm-TestAdministratorID";
    protected static final String ALIAS_NJSLA_ASM_NAME = "all-asm-StateAssessmentName";
    protected static final String ALIAS_NJSLA_SESSION_NAME_OVERRIDE = "all-asm-SessionNameOverride";
    protected static final String ALIAS_PARCC_TESTDATE = "PARCCTSTDATE";
    protected static final String ALIAS_TEST_SCHOOL_OVERRIDE = "all-asm-TestingSchoolOverride";
    protected static final String ALIAS_FIRST_HS_MATH = "all-asm-FirstHSMath";

    // ORGANIZATION TABLE
    protected static final String ALIAS_COUNTY_CODE = "DOE COUNTY CODE";

    // COURSE Table
    protected static final String ALIAS_TEST_CODE = "all-crs-NJTestCode";

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
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_NJ = "NJSLA";
    protected static final String CODE_SPED_STATUS_ACTIVE = "Active";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No NJ PARCC Student Assessment's were created by the selected students.";
    protected static final String CODE_ONE = "1";
    protected static final String[] CODE_LUNCH_STATUS = {"F", "Free", "R", "Reduced"};
    protected static final String CYCLE_ACCESS_DLM_SPRING_STATE_CODE = "NJ";
    protected static final String CYCLE_ACCESS_DLM_SPRING_TEXT = "ACCESS/DLM";
    protected static final String CYCLE_FALL_STATE_CODE = "FA";
    protected static final String CYCLE_FALL_TEXT = "Fall";
    protected static final String CYCLE_SPRING_STATE_CODE = "SP";
    protected static final String CYCLE_SPRING_TEXT = "Spring";
    protected static final String CYCLE_SUMMER_STATE_CODE = "SU";
    protected static final String CYCLE_SUMMER_TEXT = "Summer";
    protected static final String ERROR_MESSAGE_STATE_CODE_NOT_DEFINED =
            " System State Code (sys.state) is not set in System Preferences";
    protected static final String INPUT_PARAM_TEST_CODE = "testCodes";
    protected static final String LEP_TEST_EXEMPT = "Y";
    protected static final String LEP_TEST_NOT_EXEMPT = "N";
    protected static final String REF_TABLE_NAME_EXEMPT = "DOE STATE ASSESSMENT EXEMPTION CODE";
    protected static final String STATE_CODE_NJ = "NJ";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    public Map<String, String> m_asmRtbOid = new HashMap();
    protected String m_countyCodeField;
    protected DateAsStringConverter m_dateConverter;
    protected FieldRetriever m_disabilityRetriever;
    protected String m_districtCodeField;
    protected Map<String, String> m_refTableNameOidMap;
    protected String m_enrResidingSkl;
    public Map<String, Collection<StudentProgramParticipation>> m_pgmsLEPMap = new HashMap<>();
    protected String m_schoolCodeField;
    protected String m_stdHomeless;
    protected String m_stdSpedClassification;
    protected String m_testCodeField;
    protected ArrayList<String> m_testCodes = new ArrayList<>();
    private DataDictionary m_parccDictionary;

    /**
     * Returns true if student is english learner, otherwise return false.
     *
     * @param student SisStudent
     * @return boolean
     */
    public boolean englishLearner(SisStudent student) {
        boolean englishLearner = false;

        Collection<StudentProgramParticipation> pgms = m_pgmsLEPMap.get(student.getOid());

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

        // Initialize now in case setup errors are generated
        RetrieveFirstDateInUS retrieveFirstDateInUs = new RetrieveFirstDateInUS();

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
            calcs.put(RetrieveFirstDateInUS.CALC_ID, retrieveFirstDateInUs);
            super.addCalcs(calcs);
            loadNJSLAAssessmentDefinition();
            loadLEPProgramsMap();
            X2Criteria rcdTestCodesCriteria = new X2Criteria();
            if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_TEST_CODE))) {
                rcdTestCodesCriteria.addIn(X2BaseBean.COL_OID,
                        StringUtils.convertDelimitedStringToList((String) getParameter(INPUT_PARAM_TEST_CODE), ","));
            } else {
                String rtbTestCodeOid = getAsmRtbOid(ALIAS_NJSLA_TSTCODE, getNJSLADictionary());
                if (!StringUtils.isEmpty(rtbTestCodeOid)) {
                    rcdTestCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbTestCodeOid);
                }
            }
            QueryByCriteria rcdTestCodesQuery = new QueryByCriteria(ReferenceCode.class, rcdTestCodesCriteria);
            m_testCodes.addAll(getBroker()
                    .getGroupedCollectionByQuery(rcdTestCodesQuery, ReferenceCode.COL_CODE, 128).keySet());

            setEntityClass(NJSLAAssessmentEntity.class);
        }
    }

    /**
     * Gets the additional aliases.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAdditionalAliases()
     */
    @Override
    protected Collection<String> getAdditionalAliases() {
        return Arrays.asList(ALIAS_NJSLA_EXPPASS, ALIAS_NJSLA_TSTCODE, ALIAS_NJSLA_TSTFORMAT);
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
     * Gets the context value.
     *
     * @param assessment StudentAssessment
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getContextValue(com.x2dev.sis.model.beans.StudentAssessment)
     */
    @Override
    protected String getContextValue(StudentAssessment assessment) {
        return (String) assessment.getFieldValueByAliasExtended(ALIAS_NJSLA_TESTYEAR, m_dataDictionary);
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
     * Gets the NJSLA dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getNJSLADictionary() {
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
     * Gets the test code.
     *
     * @param assessment StudentAssessment
     * @return String
     */
    protected String getTestCode(StudentAssessment assessment) {
        return (String) assessment.getFieldValueByAliasExtended(ALIAS_NJSLA_TSTCODE, m_dataDictionary);
    }

    /**
     * Gets the test period.
     *
     * @param assessment StudentAssessment
     * @return String
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getTestPeriod(com.x2dev.sis.model.beans.StudentAssessment)
     */
    @Override
    protected String getTestPeriod(StudentAssessment assessment) {
        return (String) assessment.getFieldValueByAliasExtended(ALIAS_NJSLA_TESTPERIOD, m_dataDictionary);
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
     * Gets the asm rtb oid.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    private String getAsmRtbOid(String alias, DataDictionary dataDictionary) {
        String rtbOid = null;
        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dictField.hasReferenceTable()) {
            rtbOid = dictField.getReferenceTableOid();
        }
        return rtbOid;
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
     * Load map with current LEP programs keyed on stdOid.
     */
    private void loadLEPProgramsMap() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        if (dictionary != null) {
            DataDictionaryField ddPgmCodeField = dictionary.findDataDictionaryField(
                    StudentProgramParticipation.class.getName(), StudentProgramParticipation.COL_PROGRAM_CODE);
            if (ddPgmCodeField != null && ddPgmCodeField.hasReferenceTable()) {
                Map<String, ReferenceCode> codeMap = ddPgmCodeField.getReferenceTable().getCodeMap();
                if (codeMap != null && !codeMap.isEmpty()) {
                    Collection<String> lepCodesList = new ArrayList<String>();
                    for (String code : codeMap.keySet()) {
                        ReferenceCode refCode = codeMap.get(code);
                        if (PROGRAM_CODE_LEP.equals(refCode.getStateCode())) {
                            lepCodesList.add(code);
                        }
                    }
                    X2Criteria pgmCriteria = new X2Criteria();
                    if (!lepCodesList.isEmpty()) {
                        pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, lepCodesList);
                    } else {
                        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, "___dummy___");
                    }
                    QueryByCriteria query =
                            m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, pgmCriteria,
                                    StudentProgramParticipation.COL_STUDENT_OID);
                    query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
                    m_pgmsLEPMap =
                            getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                                    1024);
                }
            }
        }
    }

    /**
     * Load PARCC Assessment Definition Alias field names.
     */
    private void loadNJSLAAssessmentDefinition() {
        DataDictionary dataDictionary = getNJSLADictionary();

        if (dataDictionary == null) {
            addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            m_asmAlias.put(ALIAS_NJSLA_ASM_NAME, getAsmJavaName(ALIAS_NJSLA_ASM_NAME, getNJSLADictionary()));
            m_asmRtbOid.put(ALIAS_NJSLA_ASM_NAME, getAsmRtbOid(ALIAS_NJSLA_ASM_NAME, getNJSLADictionary()));
            m_asmAlias.put(ALIAS_PARCC_TESTDATE, getAsmJavaName(ALIAS_PARCC_TESTDATE, getNJSLADictionary()));
            m_asmAlias.put(ALIAS_NJSLA_ASM_ADMIN_ID, getAsmJavaName(ALIAS_NJSLA_ASM_ADMIN_ID, getNJSLADictionary()));
            m_asmAlias.put(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE,
                    getAsmJavaName(ALIAS_NJSLA_ACCOUNT_SKL_OVERRIDE, getNJSLADictionary()));
            m_asmAlias.put(ALIAS_NJSLA_SESSION_NAME_OVERRIDE,
                    getAsmJavaName(ALIAS_NJSLA_SESSION_NAME_OVERRIDE, getNJSLADictionary()));
            m_asmAlias.put(ALIAS_FIRST_HS_MATH,
                    getAsmJavaName(ALIAS_FIRST_HS_MATH, getNJSLADictionary()));
        }
    }
}
