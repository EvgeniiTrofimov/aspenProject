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
package com.x2dev.procedures.statereporting.fl;

import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_WDIS_REPORTING_YEAR;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.StaffInfo;
import com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import jersey.repackaged.com.google.common.collect.ImmutableSet;

/**
 * The Class FLStateReportData.
 */
public class FLStateReportData extends StateReportData {

    /**
     * Gets the fiscal year. This code must remain consistent in FLStateReportExport and
     * FLStateReportData
     *
     * @param calendar the calendar
     * @param context the context
     * @return the fiscal year
     */
    public static String getFiscalYear(Calendar calendar, DistrictSchoolYearContext context) {
        calendar.setTime(context.getStartDate());
        int start = calendar.get(Calendar.YEAR);
        calendar.setTime(context.getEndDate());
        int end = calendar.get(Calendar.YEAR);
        return String.format("%02d%02d", Integer.valueOf(start % 100), Integer.valueOf(end % 100));
    }

    /**
     * The Enum AliasField.
     */
    public enum AliasField {
        SINGLE_SEX_MST("tblSchMaster ", "Single Sex Course Override", "SingleSexOverride", "all-mst-CRDCSingleSexCode",
                "Character", "10", "Gender Codes");

        /**
         * Instantiates a new alias field.
         *
         * @param tableOid String
         * @param longName String
         * @param shortName String
         * @param alias String
         * @param userType String
         * @param userLength String
         * @param refTable String
         */
        private AliasField(String tableOid, String longName, String shortName, String alias, String userType,
                String userLength, String refTable) {
            m_tableOid = tableOid;
            m_longName = longName;
            m_shortName = shortName;
            m_alias = alias;
            m_userType = userType;
            m_userLength = userLength;
            m_refTable = refTable;
        }

        private String m_tableOid;
        private String m_longName;
        private String m_shortName;
        private String m_alias;
        private String m_userType;
        private String m_userLength;
        private String m_refTable;

        /**
         * Returns CalcParameter based on parameter name.
         *
         * @param alias String
         * @return CRDCDataHelper.CalcParameter
         */
        public static AliasField findAliasField(String alias) {
            AliasField match = null;

            for (AliasField field : AliasField.values()) {
                if (field.getAlias().equals(alias)) {
                    match = field;
                    break;
                }
            }

            return match;
        }

        /**
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            return m_alias;
        }

        /**
         * Gets the error msg.
         *
         * @return String
         */
        public String getErrorMsg() {
            StringBuilder output = new StringBuilder();
            output.append("The alias could not be located. \nAlias characteristics: \n");
            append(output, "Alias", m_alias);
            append(output, "Table", m_tableOid);
            append(output, "Long Name", m_longName);
            append(output, "Short Name", m_shortName);
            append(output, "User Type", m_userType);
            append(output, "User Length", m_userLength);
            append(output, "Ref Table", m_refTable);
            return output.toString();
        }

        /**
         * Append.
         *
         * @param output StringBuilder
         * @param field String
         * @param value String
         */
        private void append(StringBuilder output, String field, String value) {
            output.append(field);
            output.append(": ");
            output.append(value);
            output.append("\n");
        }
    }

    /**
     * The Interface AsmTypeInterface.
     */
    protected interface AsdTypeInterface {

        /**
         * Gets the id.
         *
         * @return String
         */
        public String getId();

        /**
         * Gets the test name.
         *
         * @return String
         */
        public String getTestName();
    }

    /**
     * The Interface RetrieveAsmFieldInterface.
     */
    protected interface RetrieveAsmFieldInterface {

        /**
         * Gets the asm types.
         *
         * @return Collection
         */
        public AsdTypeInterface[] getAsdTypes();
    }

    /**
     * Field retriever for student assessment fields.
     */
    protected abstract class RetrieveStudentAssessmentField implements FieldRetriever, RetrieveAsmFieldInterface {
        public static final String CALC_ID = "ASM_FIELD";

        public static final String CALC_PARAM_TF = "TF";
        public static final String CALC_PARAM_TL = "TL";
        public static final String CALC_PARAM_TPY = "TPY";
        public static final String CALC_PARAM_TS1 = "TS1";
        public static final String CALC_PARAM_TS2 = "TS2";
        public static final String CALC_PARAM_TSC = "TSC";
        public static final String CALC_PARAM_TST1 = "TST1";
        public static final String CALC_PARAM_TST2 = "TST2";

        private Map<String, AssessmentFields> m_asdIdsMap = null;

        private Collection<String> m_asmTypeIds = null;

        /**
         * The Class AssessmentFields.
         */
        private class AssessmentFields {
            private String m_assessmentDefinitionId;

            private String m_fieldAliasTF = "asm-%asdId%-testForm";
            private String m_fieldAliasTL = "asm-%asdId%-testLevel";
            private String m_fieldAliasTPY = "asm-%asdId%-testPublicationYear";
            private String m_fieldAliasTS1 = "asm-%asdId%-testScore1";
            private String m_fieldAliasTS2 = "asm-%asdId%-testScore2";
            private String m_fieldAliasTSC = "asm-%asdId%-testSubjectContent";
            private String m_fieldAliasTST1 = "asm-%asdId%-testScoreType1";
            private String m_fieldAliasTST2 = "asm-%asdId%-testScoreType2";

            private DataDictionaryField m_fieldTF;
            private DataDictionaryField m_fieldTL;
            private DataDictionaryField m_fieldTPY;
            private DataDictionaryField m_fieldTS1;
            private DataDictionaryField m_fieldTS2;
            private DataDictionaryField m_fieldTSC;
            private DataDictionaryField m_fieldTST1;
            private DataDictionaryField m_fieldTST2;

            /**
             * Instantiates a new assessment fields.
             *
             * @param assessmentDefinitionId String
             */
            private AssessmentFields(String assessmentDefinitionId) {
                m_assessmentDefinitionId = assessmentDefinitionId;

                if (getIds().contains(m_assessmentDefinitionId.toUpperCase())) {
                    m_fieldAliasTPY = m_fieldAliasTPY.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTSC = m_fieldAliasTSC.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTF = m_fieldAliasTF.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTL = m_fieldAliasTL.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTST1 = m_fieldAliasTST1.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTS1 = m_fieldAliasTS1.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTST2 = m_fieldAliasTST2.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                    m_fieldAliasTS2 = m_fieldAliasTS2.replace("%asdId%", m_assessmentDefinitionId.toLowerCase());
                }
            }

            /**
             * Gets the field value.
             *
             * @param data FLStudentAssessmentData
             * @param studentAssessment StudentAssessment
             * @param parameter String
             * @return Object
             * @throws X2BaseException exception
             */
            public Object getFieldValue(FLStateReportData data,
                                        StudentAssessment studentAssessment,
                                        String parameter)
                    throws X2BaseException {
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(studentAssessment.getExtendedDataDictionary(),
                                studentAssessment.getPersistenceKey());

                Object value = null;

                switch (parameter) {
                    case CALC_PARAM_TPY:
                        m_fieldTPY = translateAliasToDictionaryField(dictionary, m_fieldAliasTPY, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTPY);
                        break;
                    case CALC_PARAM_TSC:
                        m_fieldTSC = translateAliasToDictionaryField(dictionary, m_fieldAliasTSC, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTSC);
                        break;
                    case CALC_PARAM_TF:
                        m_fieldTF = translateAliasToDictionaryField(dictionary, m_fieldAliasTF, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTF);
                        break;
                    case CALC_PARAM_TL:
                        m_fieldTL = translateAliasToDictionaryField(dictionary, m_fieldAliasTL, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTL);
                        break;
                    case CALC_PARAM_TST1:
                        m_fieldTST1 = translateAliasToDictionaryField(dictionary, m_fieldAliasTST1, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTST1);
                        break;
                    case CALC_PARAM_TS1:
                        m_fieldTS1 = translateAliasToDictionaryField(dictionary, m_fieldAliasTS1, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTS1);
                        break;
                    case CALC_PARAM_TST2:
                        m_fieldTST2 = translateAliasToDictionaryField(dictionary, m_fieldAliasTST2, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTST2);
                        break;
                    case CALC_PARAM_TS2:
                        m_fieldTS2 = translateAliasToDictionaryField(dictionary, m_fieldAliasTS2, true);
                        value = data.getFieldValue(studentAssessment, m_fieldTS2);
                        break;
                }

                return value;
            }
        }

        /**
         * Instantiates a new retrieve student assessment field.
         *
         * @throws X2BaseException exception
         */
        public RetrieveStudentAssessmentField() throws X2BaseException {
            if (m_asdIdsMap == null) {
                m_asdIdsMap = new HashMap<>();

                for (String assessmentDefinitionId : getIds()) {
                    m_asdIdsMap.put(assessmentDefinitionId, new AssessmentFields(assessmentDefinitionId));
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
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();

            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();

            AssessmentFields assessmentFields = m_asdIdsMap.get(studentAssessment.getAssessmentDefinition().getId());

            return assessmentFields.getFieldValue((FLStateReportData) data, studentAssessment, parameter);
        }

        /**
         * Gets the ids.
         *
         * @return Collection
         */
        private Collection<String> getIds() {
            if (m_asmTypeIds == null) {
                m_asmTypeIds = new ArrayList<>();
                for (AsdTypeInterface asdType : getAsdTypes()) {
                    m_asmTypeIds.add(asdType.getId());
                }
            }
            return m_asmTypeIds;
        }
    }

    /**
     * The Class RetrieveFiscalYear.
     */
    public class RetrieveFiscalYear implements FieldRetriever {
        public static final String CALC_ID = "FISCAL_YEAR";

        private Calendar m_calendar = Calendar.getInstance();

        /**
         * Gets the field value.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @return the field value
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return getFiscalYear(m_calendar, data.getCurrentContext());
        }
    }

    /**
     * The Class RetrieveSurveyPeriod.
     */
    public class RetrieveSurveyPeriod implements FieldRetriever, FieldValidator {
        public static final String CALC_ID = "SURVEY_PERIOD";

        /**
         * Gets the field value.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @return the field value
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLStateReportData flData = (FLStateReportData) data;
            return flData.getSurveyPeriodCode();
        }

        /**
         * Gets the field validation.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @param value the value
         * @return the field validation
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<>(0);
            if (!SURVEY_PERIOD_VALID_CODES.contains(value)) {
                String error = "Invalid Value";
                String message = "Unexpected Survey Period Code=" + (value != null ? value : "(null)");
                errors.add(new StateReportValidationError(entity, field, error, message));
            }
            return null;
        }
    }

    /**
     * The Class RetrieveSchool.
     */
    public class RetrieveSchool implements FieldRetriever, FieldValidator {
        public static final String CALC_ID = "STD_SCHOOL";

        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private DataDictionaryField m_fieldSchoolNumber;

        /**
         * Instantiates a new retrieve school.
         */
        public RetrieveSchool() {
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        }

        /**
         * Gets the field value.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @return the field value
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            FLStateReportData flData = (FLStateReportData) data;
            SisStudent student = null;
            SisSchool school = null;
            if (entity.getBean() instanceof SisStudent) {
                student = (SisStudent) entity.getBean();
            } else if (entity.getBean() instanceof ConductIncident) {
                student = ((ConductIncident) entity.getBean()).getStudent();
            } else if (entity.getBean() instanceof ConductAction) {
                school = ((ConductAction) entity.getBean()).getSchool();
            }
            if (student != null) {
                StudentInfo info = getStudentHelper().getStudentInfo(student);
                school = info.getSchool(getSurveyPeriod().getSnapshotDate());
            }
            if (school != null) {
                value = flData.getFieldValue(school, m_fieldSchoolNumber);
            }
            return value;
        }

        /**
         * Gets the field validation.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @param value the value
         * @return the field validation
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            // TODO: add field validation

            return null;
        }
    }

    /**
     * Retriever for staff School Number, Primary/Home
     * http://www.fldoe.org/core/fileparse.php/12025/urlt/1516-217565.pdf
     *
     * @author Follett Software Company
     */
    public class RetrieveStaffPrimarySchoolNumber implements FieldRetriever {
        public static final String CALC_ID = "SCHOOL_NUMBER";

        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private DataDictionaryField m_fieldSchoolNumber;

        /**
         * Instantiates a new retrieve staff primary school number.
         */
        public RetrieveStaffPrimarySchoolNumber() {
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
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

            FLStateReportData flData = (FLStateReportData) data;
            FLStateReportEntity flEntity = (FLStateReportEntity) entity;
            SisStaff staff = (SisStaff) flEntity.getBean();

            StaffPosition primary = getStaffHelper().getPrimaryStaffPosition(staff);
            if (primary != null) {
                value = flData.getFieldValue(primary.getSchool(), m_fieldSchoolNumber);
            }
            return value;
        }
    }

    /**
     * Retriever for staff SSN.
     *
     * @author Follett Software Company
     */
    public class RetrieveStaffSsn implements FieldRetriever {
        public static final String CALC_ID = "SSN";

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
            FLStateReportEntity flEntity = (FLStateReportEntity) entity;
            SisStaff staff = (SisStaff) flEntity.getBean();
            StaffInfo info = getStaffHelper().getStaffInfo(staff);
            return info.getSSN();
        }
    }

    /**
     * The Class RetrieveWdisYear.
     */
    public class RetrieveWdisYear implements FieldRetriever {
        public static final String CALC_ID = "WDIS Year";

        private Map<String, Map<String, DataDictionaryField>> m_ddxAliasFields = new HashMap<>();

        private static final String ALIAS_WDIS_PRIOR_YEAR = "pgm-wdis-prior-year";
        private static final String ALIAS_WDIS_REPORTING_YEAR = "pgm-wdis-reporting-year";

        /**
         * Gets the field value.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @return the field value
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLStateReportData flData = (FLStateReportData) data;
            WdisEntity wdisEntity = (WdisEntity) entity;
            String value = null;
            if (wdisEntity.getProgram() != null) {
                String alias = FIELD_WDIS_REPORTING_YEAR.equals(field.getFieldId()) ? ALIAS_WDIS_REPORTING_YEAR
                        : ALIAS_WDIS_PRIOR_YEAR;
                String ddxOid = wdisEntity.getProgram().getExtendedDataDictionaryOid();
                Map<String, DataDictionaryField> aliasFields = m_ddxAliasFields.get(ddxOid);
                if (aliasFields == null) {
                    aliasFields = new HashMap<>();
                    m_ddxAliasFields.put(ddxOid, aliasFields);
                }

                DataDictionaryField fieldYear = aliasFields.get(alias);
                if (fieldYear == null) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                            wdisEntity.getProgram().getExtendedDataDictionary(),
                            flData.getBroker().getPersistenceKey());
                    fieldYear = flData.translateAliasToDictionaryField(dictionary, alias, true);
                    aliasFields.put(alias, fieldYear);
                }

                String year = (String) flData.getFieldValue(wdisEntity.getProgram(), fieldYear);
                value = getYearsFromYear(year);
            }

            if (value == null && FIELD_WDIS_REPORTING_YEAR.equals(field.getFieldId())) {
                value = getFiscalYear(Calendar.getInstance(), data.getCurrentContext());
            }
            return value;
        }
    }

    /**
     * The Class SchoolCalendarHelper.
     */
    public class SchoolCalendarHelper {
        private Map<String, SchoolCalendarInfo> m_schoolCalendarInfos = new HashMap();
        private Map<String, Collection<SchoolCalendar>> m_schoolCalendars;
        private Map<String, SchoolCalendar> m_mostCommonCalendar = new HashMap();
        private Comparator<SisSchoolCalendarDate> m_schoolCalendarDatesComparator;

        /**
         * Gets the most common calendar code.
         *
         * @param school the school
         * @return the most common calendar code
         */
        public SchoolCalendar getMostCommonCalendarCode(SisSchool school) {
            SchoolCalendar value = null;
            if (m_mostCommonCalendar.containsKey(school.getOid())) {
                value = m_mostCommonCalendar.get(school.getOid());
            } else {
                Collection<SchoolCalendar> calendars = getSchoolCalendars(school.getOid());
                if (calendars != null && !calendars.isEmpty()) {
                    value = calendars.iterator().next();
                    if (calendars.size() > 1) {
                        List<String> calendarCodes = new LinkedList();
                        for (SchoolCalendar calendar : calendars) {
                            calendarCodes.add(calendar.getCalendarId());
                        }

                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
                        criteria.addIn(SisStudent.COL_CALENDAR_CODE, calendarCodes);

                        String[] columns = new String[] {SisStudent.COL_CALENDAR_CODE, "count(*)"};

                        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
                        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
                        query.addOrderByDescending("count(*)");

                        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                        try {
                            while (iterator.hasNext()) {
                                Object[] row = (Object[]) iterator.next();
                                String calendarCode = (String) row[0];

                                if (!StringUtils.isEmpty(calendarCode)) {
                                    for (SchoolCalendar calendar : calendars) {
                                        if (calendarCode.equals(calendar.getCalendarId())) {
                                            value = calendar;
                                            break;
                                        }
                                    }
                                }
                            }
                        } finally {
                            iterator.close();
                        }
                    }
                }
                m_mostCommonCalendar.put(school.getOid(), value);
            }
            return value;
        }

        /**
         * Gets the school calendar dates comparator.
         *
         * @return the school calendar dates comparator
         */
        public Comparator<SisSchoolCalendarDate> getSchoolCalendarDatesComparator() {
            if (m_schoolCalendarDatesComparator == null) {
                m_schoolCalendarDatesComparator = new Comparator<SisSchoolCalendarDate>() {
                    @Override
                    public int compare(SisSchoolCalendarDate o1, SisSchoolCalendarDate o2) {
                        PlainDate date1 = o1.getDate();
                        PlainDate date2 = o2.getDate();
                        if (date1 == null) {
                            return -1;
                        } else if (date2 == null) {
                            return 1;
                        }
                        return date1.compareTo(date2);
                    }
                };
            }
            return m_schoolCalendarDatesComparator;
        }

        /**
         * Gets the school calendar info.
         *
         * @param school the school
         * @param calendarCode the calendar code
         * @param context
         * @return the school calendar info
         */
        public SchoolCalendarInfo getSchoolCalendarInfo(SisSchool school,
                                                        String calendarCode,
                                                        DistrictSchoolYearContext context) {
            String key = getSchoolCalendarInfoKey(context.getOid(), school.getOid(), calendarCode);
            SchoolCalendarInfo info = m_schoolCalendarInfos.get(key);
            if (info == null) {
                info = new SchoolCalendarInfo(context, school, calendarCode);
                m_schoolCalendarInfos.put(key, info);
            }
            return info;
        }

        /**
         * Gets the school calendars.
         *
         * @param schoolOid the school oid
         * @return the school calendars
         */
        public Collection<SchoolCalendar> getSchoolCalendars(String schoolOid) {
            return getSchoolCalendars(schoolOid, getCurrentContext());
        }

        /**
         * Gets the school calendars.
         *
         * @param schoolOid the school oid
         * @param context
         * @return the school calendars
         */
        public Collection<SchoolCalendar> getSchoolCalendars(String schoolOid, DistrictSchoolYearContext context) {
            if (m_schoolCalendars == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());
                BeanQuery query = new BeanQuery(SchoolCalendar.class, criteria);
                m_schoolCalendars = getBroker().getGroupedCollectionByQuery(query, SchoolCalendar.COL_SCHOOL_OID, 100);
            }
            return m_schoolCalendars.get(schoolOid);
        }

        /**
         * Gets the school calendar info key.
         *
         * @param ctxOid the ctx oid
         * @param sklOid the skl oid
         * @param calendarCode the calendar code
         * @return the school calendar info key
         */
        private String getSchoolCalendarInfoKey(String ctxOid, String sklOid, String calendarCode) {
            return SCHOOL_CALENDAR_KEY_DELIMITER + ctxOid +
                    SCHOOL_CALENDAR_KEY_DELIMITER + sklOid +
                    SCHOOL_CALENDAR_KEY_DELIMITER + calendarCode + SCHOOL_CALENDAR_KEY_DELIMITER;
        }
    }

    /**
     * The Class SchoolCalendarInfo.
     */
    public class SchoolCalendarInfo {
        private DistrictSchoolYearContext m_context;
        private SisSchool m_school;
        private String m_calendarCode;
        private Collection<SisSchoolCalendarDate> m_calendarDates;
        private ImmutableSet<PlainDate> m_inSessionDates;

        /**
         * Instantiates a new school calendar info.
         *
         * @param context the context
         * @param school the school
         * @param calendarCode the calendar code
         */
        public SchoolCalendarInfo(DistrictSchoolYearContext context, SisSchool school, String calendarCode) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, school.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            BeanQuery query = new BeanQuery(SchoolCalendarDate.class, criteria);
            query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
            m_calendarDates = getBroker().getCollectionByQuery(query);
        }

        /**
         * Gets the in session dates.
         *
         * @return the in session dates
         */
        public ImmutableSet<PlainDate> getInSessionDates() {
            if (m_inSessionDates == null) {
                List<PlainDate> dates = new ArrayList(m_calendarDates.size());
                for (SisSchoolCalendarDate calendarDate : m_calendarDates) {
                    if (calendarDate.getInSessionIndicator()) {
                        dates.add(calendarDate.getDate());
                    }
                }
                m_inSessionDates = ImmutableSet.copyOf(dates);
            }
            return m_inSessionDates;
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            return !m_calendarDates.isEmpty();
        }
    }

    /**
     * The Interface SurveyPeriod.
     */
    public interface SurveyPeriod {
        public String getCode();

        public PlainDate getDateCertain();

        public PlainDate getEndDate();

        public String getFLTermCode();

        public PlainDate getSnapshotDate();

        public PlainDate getStartDate();
    }

    /**
     * The Class SurveyPeriod.
     */
    public class FLSurveyPeriod implements SurveyPeriod {
        public static final String ORA_DDX_ID = "FL-ORA-SURVEY";

        private static final String ALIAS_CODE = "ora-survey-code";
        private static final String ALIAS_END_DATE = "ora-survey-end-date";
        private static final String ALIAS_SNAPSHOT_DATE = "ora-survey-snapshot-date";
        private static final String ALIAS_START_DATE = "ora-survey-start-date";
        private static final String ALIAS_YEAR = "ora-survey-year";

        private static final String ERROR_MSG_ALIAS =
                "Alias was not found for extended data dictionary " + ORA_DDX_ID + ",";
        private static final String ERROR_MSG_DDX = "Extended data dictionary was not found.";
        private static final String ERROR_MSG_ORA = "Survey period organization attribute was not found.";

        private String m_code;
        private PlainDate m_endDate;
        private PlainDate m_snapshotDate;
        private PlainDate m_startDate;

        /**
         * Instantiates a new survey period.
         *
         * @param code the code
         */
        public FLSurveyPeriod(String code) {
            m_code = code;

            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
            if (ddx == null) {
                addSetupError(ERROR_MSG_DDX, ORA_DDX_ID);
            } else {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                String fieldYear = translateAlias(dictionary, ALIAS_YEAR);
                String fieldCode = translateAlias(dictionary, ALIAS_CODE);
                String fieldSnapshotDate = translateAlias(dictionary, ALIAS_SNAPSHOT_DATE);
                String fieldStartDate = translateAlias(dictionary, ALIAS_START_DATE);
                String fieldEndDate = translateAlias(dictionary, ALIAS_END_DATE);

                X2Criteria criteria = new X2Criteria();

                criteria.addEqualTo(OrganizationAttributes.COL_EXTENDED_DATA_DICTIONARY_OID, ddx.getOid());
                criteria.addEqualTo(fieldYear, Integer.toString(getCurrentContext().getSchoolYear()));
                criteria.addEqualTo(fieldCode, code);

                QueryByCriteria query = new QueryByCriteria(OrganizationAttributes.class, criteria);
                OrganizationAttributes ora = (OrganizationAttributes) getBroker().getBeanByQuery(query);
                if (ora == null) {
                    addSetupError(ERROR_MSG_ORA, code);
                }

                if (getSetupErrors().size() == 0) {
                    DateAsStringConverter dateConverter = (DateAsStringConverter) ConverterFactory
                            .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
                    m_snapshotDate = (PlainDate) dateConverter
                            .parseSystemString((String) ora.getFieldValueByBeanPath(fieldSnapshotDate));

                    if (!FLStateReportData.SURVEY_PERIOD_1.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_2.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_3.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_4.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_6.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_F.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_G.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_W.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_X.equals(code) &&
                            !FLStateReportData.SURVEY_PERIOD_S.equals(code)) {
                        m_startDate = getCurrentContext().getStartDate();
                        m_endDate = getCurrentContext().getEndDate();
                    } else {
                        m_startDate = (PlainDate) dateConverter
                                .parseSystemString((String) ora.getFieldValueByBeanPath(fieldStartDate));
                        m_endDate = (PlainDate) dateConverter
                                .parseSystemString((String) ora.getFieldValueByBeanPath(fieldEndDate));
                    }
                }
            }
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        @Override
        public String getCode() {
            return m_code;
        }

        /**
         * Get the data certain for this period. This is currently set to the end date of the period
         * but may be redefined later.
         *
         * @return the date certain
         */
        @Override
        public PlainDate getDateCertain() {
            return m_endDate;
        }

        /**
         * Gets the end date.
         *
         * @return the end date
         */
        @Override
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the FL term code.
         *
         * @return the FL term code
         */
        @Override
        public String getFLTermCode() {
            String termCode = null;
            switch (getCode()) {
                case SURVEY_PERIOD_1:
                    termCode = SURVEY_TERM_CODE_PERIOD_1;
                    break;
                case SURVEY_PERIOD_2:
                    termCode = SURVEY_TERM_CODE_PERIOD_2;
                    break;
                case SURVEY_PERIOD_3:
                    termCode = SURVEY_TERM_CODE_PERIOD_3;
                    break;
                case SURVEY_PERIOD_4:
                    termCode = SURVEY_TERM_CODE_PERIOD_4;
                    break;
                default:
                    break;
            }
            return termCode;
        }

        /**
         * Gets the snapshot date.
         *
         * @return the snapshot date
         */
        @Override
        public PlainDate getSnapshotDate() {
            return m_snapshotDate;
        }

        /**
         * Gets the start date.
         *
         * @return the start date
         */
        @Override
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Translate alias.
         *
         * @param dictionary the dictionary
         * @param alias the alias
         * @return the string
         */
        private String translateAlias(DataDictionary dictionary, String alias) {
            String javaName = null;

            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                javaName = field.getJavaName();
            } else {
                addSetupError(ERROR_MSG_ALIAS, alias);
            }

            return javaName;
        }

    }

    protected static final String ALIAS_WDIS_AGE_INDICTOR = "all-mst-WdisAgeIndicator";
    /*
     * Constants for query criteria parameters from user input template.
     */
    protected static final String PARAM_SURVEY_PERIOD = "surveyPeriod";

    protected static final String SURVEY_PERIOD_1 = "1";
    protected static final String SURVEY_PERIOD_2 = "2";
    protected static final String SURVEY_PERIOD_3 = "3";
    protected static final String SURVEY_PERIOD_4 = "4";
    protected static final String SURVEY_PERIOD_5 = "5";
    protected static final String SURVEY_PERIOD_6 = "6";
    protected static final String SURVEY_PERIOD_7 = "7";
    protected static final String SURVEY_PERIOD_8 = "8";
    protected static final String SURVEY_PERIOD_9 = "9";
    protected static final String SURVEY_PERIOD_A = "A";
    protected static final String SURVEY_PERIOD_B = "B";
    protected static final String SURVEY_PERIOD_C = "C";
    protected static final String SURVEY_PERIOD_D = "D";
    protected static final String SURVEY_PERIOD_F = "F";
    protected static final String SURVEY_PERIOD_G = "G";
    protected static final String SURVEY_PERIOD_S = "S";
    protected static final String SURVEY_PERIOD_W = "W";
    protected static final String SURVEY_PERIOD_X = "X";

    protected static final String SURVEY_TERM_CODE_PERIOD_1 = "5";
    protected static final String SURVEY_TERM_CODE_PERIOD_2 = "1";
    protected static final String SURVEY_TERM_CODE_PERIOD_3 = "2";
    protected static final String SURVEY_TERM_CODE_PERIOD_4 = "4";

    protected static final List<String> SURVEY_PERIOD_VALID_CODES = Arrays.asList(FLStateReportData.SURVEY_PERIOD_1,
            FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_4,
            FLStateReportData.SURVEY_PERIOD_5, FLStateReportData.SURVEY_PERIOD_6, FLStateReportData.SURVEY_PERIOD_7,
            FLStateReportData.SURVEY_PERIOD_8, FLStateReportData.SURVEY_PERIOD_9, FLStateReportData.SURVEY_PERIOD_A,
            FLStateReportData.SURVEY_PERIOD_B, FLStateReportData.SURVEY_PERIOD_C, FLStateReportData.SURVEY_PERIOD_D,
            FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_G, FLStateReportData.SURVEY_PERIOD_S,
            FLStateReportData.SURVEY_PERIOD_W, FLStateReportData.SURVEY_PERIOD_X);

    protected static final String WDIS_MODE_ADULT_GENERAL_EDUCATION = "Adult Education";
    protected static final String WDIS_MODE_BOTH = "Both";
    protected static final String WDIS_MODE_POST_SECONDARY_CTE = "Post-Secondary CTE";

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    private static final String PATTERN_TABLE_PREFIX = "^[a-z]{2,3}-([a-z]{3})-.+";
    private static final String SCHOOL_CALENDAR_KEY_DELIMITER = "|";

    private Map<String, DataDictionaryField> m_aliasFields = new HashMap();
    private List<String> m_keysBeanPaths;
    private Pattern m_patternTablePrefix;
    private SchoolCalendarHelper m_schoolCalendarHelper;
    private FLStaffHelper m_staffHelper;
    private FLStudentHelper m_studentHelper;
    private SurveyPeriod m_surveyPeriod;
    private Set<String> m_uniqueKeys;

    /**
     * Gets the codes for state value.
     *
     * @param beanClass the bean class
     * @param columnName the column name
     * @param codes the codes
     * @return the codes for state value
     */
    public Set<String> getCodesForStateValue(Class beanClass, String columnName, Collection<String> codes) {
        Set<String> value = new HashSet();
        // make sure at least one value is included.
        value.add("__dummy__");

        ModelProperty prop = new ModelProperty(beanClass, columnName, getBroker().getPersistenceKey());
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(ReferenceCode.COL_STATE_CODE, codes);
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

            String[] columns = new String[] {ReferenceCode.COL_CODE};

            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    value.add(code);
                }
            } finally {
                iterator.close();
            }
        }
        return value;
    }

    /**
     * Gets the field value.
     *
     * @param bean the bean
     * @param field the field
     * @return the field value
     * @throws X2BaseException the x 2 base exception
     */
    public Object getFieldValue(X2BaseBean bean, DataDictionaryField field) throws X2BaseException {
        Object value = null;
        if (bean != null && field != null) {
            if (field.getDataTable().getClassName().equals(bean.getClass().getName())) {
                value = WebUtils.getProperty(bean, field.getJavaName());

                if (value instanceof String) {
                    if (field.isString()) {
                        String format = WebUtils.generateFormat(field,
                                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                        Converter baseConverter = ConverterFactory.getConverterForClass(
                                field.getEffectiveJavaType(),
                                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                field.isString(), format);
                        if (baseConverter instanceof SystemStringConverter) {
                            SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                            if (converter != null) {
                                value = converter.parseSystemString((String) value);
                            }
                        }
                    }
                }
                if (value instanceof String && !StringUtils.isEmpty((String) value) && field.hasReferenceTable()) {
                    if (field.getLength() > 50) { // process as D field with multiple values
                        List<String> codes = Arrays.asList(((String) value).split(","));
                        StringBuilder converted = new StringBuilder();
                        for (String code : codes) {
                            code = code.trim();
                            String stateCode = lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (!StringUtils.isEmpty(stateCode)) {
                                if (converted.length() > 0) {
                                    converted.append(",");
                                }
                                converted.append(stateCode);
                            }
                        }
                        value = converted.toString();
                    } else {
                        value = lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            } else {
                throw new X2RuntimeException(new UnsupportedOperationException(
                        "Bean class is " + bean.getClass() + " and field class is " + field.getDataTable().getClass()));
            }
        }
        return value;
    }

    /**
     * Method to get keys' java names for the current Export Format Definition.
     *
     * @return the keys bean paths
     */
    public List<String> getKeysBeanPaths() {
        if (m_keysBeanPaths == null) {
            String efdOid = getEfdOid();

            X2Criteria effCriteria = new X2Criteria();
            effCriteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, efdOid);
            effCriteria.addEqualTo(ExportFormatField.COL_KEY_IND, BooleanAsStringConverter.TRUE);

            String keyColumn = ExportFormatField.COL_NAME;
            QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, effCriteria);
            query.addOrderByAscending(ExportFormatField.COL_POSITION);
            m_keysBeanPaths = new LinkedList();
            for (Object obj : getBroker().getCollectionByQuery(query)) {
                ExportFormatField bean = (ExportFormatField) obj;
                m_keysBeanPaths.add((String) bean.getFieldValueByBeanPath(keyColumn));
            }
        }

        return m_keysBeanPaths;
    }

    /**
     * Gets the school calendar helper.
     *
     * @return the school calendar helper
     */
    public SchoolCalendarHelper getSchoolCalendarHelper() {
        if (m_schoolCalendarHelper == null) {
            m_schoolCalendarHelper = new SchoolCalendarHelper();
        }
        return m_schoolCalendarHelper;
    }

    /**
     * Gets the staff helper.
     *
     * @return the staff helper
     * @throws X2BaseException the x 2 base exception
     */
    public FLStaffHelper getStaffHelper() throws X2BaseException {
        if (m_staffHelper == null) {
            m_staffHelper = new FLStaffHelper(this);
        }
        return m_staffHelper;
    }

    /**
     * Gets the student helper.
     *
     * @return the student helper
     * @throws X2BaseException the x 2 base exception
     */
    public FLStudentHelper getStudentHelper() throws X2BaseException {
        if (m_studentHelper == null) {
            m_studentHelper = new FLStudentHelper(this);
        }
        return m_studentHelper;
    }


    /**
     * Gets the survey period.
     *
     * @return current Survey Period
     */
    public SurveyPeriod getSurveyPeriod() {
        return m_surveyPeriod;
    }

    /**
     * Gets the survey period code.
     *
     * @return one-character survey period code: [1-9, A-D, F, G, S, W, X]
     */
    public String getSurveyPeriodCode() {
        return getSurveyPeriod().getCode();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return the valid survey periods
     */
    public Collection<String> getValidSurveyPeriods() {
        return SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        ReferenceCode surveyPeriodBean = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class,
                (String) getParameter(PARAM_SURVEY_PERIOD));
        if (surveyPeriodBean == null) {
            addSetupError("Reference Codes", "Could not find the reference code for Survey Period");
            return;
        }
        m_surveyPeriod = new FLSurveyPeriod(surveyPeriodBean.getCode());
        if (!getValidSurveyPeriods().contains(getSurveyPeriodCode())) {
            addSetupError("Invalid Survey Period",
                    "Survey Period " + getSurveyPeriodCode() + " is invalid for this export");
        }
    }

    /**
     * Checks if row is unique.
     *
     * @param key the key
     * @return true, if is unique key
     */
    public boolean isUniqueKey(String key) {
        if (m_uniqueKeys == null) {
            m_uniqueKeys = new HashSet<>();
        }
        return m_uniqueKeys.contains(key) ? false : m_uniqueKeys.add(key);
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias the alias
     * @param required the required
     * @return String
     */
    public DataDictionaryField translateAliasToDictionaryField(String alias, boolean required) {
        return translateAliasToDictionaryField(getDataDictionary(), alias, required);
    }

    /**
     * Translates an alias from specified data dictionary into a Java bean path name.
     * An initialization error will be logged if the alias does not exist.
     *
     * @param dataDictionary the data dictionary
     * @param alias the alias
     * @param required the required
     * @return String
     */
    public DataDictionaryField translateAliasToDictionaryField(DataDictionary dataDictionary,
                                                               String alias,
                                                               boolean required) {
        DataDictionaryField field = null;

        if (dataDictionary == null || alias == null) {
            return field;
        }

        String aliasKey = (dataDictionary.getExtendedDictionary() != null
                && dataDictionary.getExtendedDictionary().getId() != null
                        ? dataDictionary.getExtendedDictionary().getId()
                        : "")
                + "_" + alias;

        if (m_aliasFields.containsKey(aliasKey)) {
            field = m_aliasFields.get(aliasKey);
        } else {
            String tablePrefix = null;

            if (m_patternTablePrefix == null || true) {
                m_patternTablePrefix = Pattern.compile(PATTERN_TABLE_PREFIX);
            }
            Matcher matcher = m_patternTablePrefix.matcher(alias);
            if (matcher.matches()) {
                tablePrefix = matcher.group(1);
            }

            field = dataDictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                if (!field.isEnabled()) {
                    addSetupError("Alias field is not enabled", alias);
                } else if (dataDictionary.getExtendedDictionary() == null &&
                        tablePrefix != null && !tablePrefix.equalsIgnoreCase(field.getTable().getObjectPrefix())) {
                    addSetupError("Alias field is defined on the wrong table", alias);
                }
            } else if (required) {
                AliasField aliasField = AliasField.findAliasField(alias);
                String aliasMsg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
                if (aliasField != null) {
                    aliasMsg = aliasField.getErrorMsg();
                }
                addSetupError(aliasMsg, alias);
            }
            m_aliasFields.put(aliasKey, field);
        }

        return field;
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias the alias
     * @param required the required
     * @return String
     */
    @Override
    public String translateAliasToJavaName(String alias, boolean required) {
        DataDictionaryField field = translateAliasToDictionaryField(alias, required);
        return field != null ? field.getJavaName() : null;
    }

    /**
     * Gets the years from year.
     *
     * @param year String
     * @return String
     */
    private String getYearsFromYear(String year) {
        String value = null;
        if (year != null) {
            String twoDecYear = year.substring(Math.max(year.length() - 2, 0));
            if (twoDecYear.matches("^\\d{2}$")) {
                String nextYear = String.valueOf((Integer.parseInt(twoDecYear) + 1));
                value = twoDecYear + nextYear.substring(Math.max(nextYear.length() - 2, 0));
            }
        }
        return value;
    }
}
