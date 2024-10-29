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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Field;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Operator;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationResult;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.KeyValuePair;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.ojb.broker.query.ReportQueryByCriteria;


/**
 * The Class FLWdisExportConfiguration.
 */
public class FLWdisExportConfiguration extends FLExportConfiguration {


    /**
     * The Enum FL_EXPORT.
     */
    public enum FL_EXPORT implements FLExport {
        AGE(FLWdisValidations.EXPORT_ID_SUFFIX_AGE, "F63417", PROCEDURE_ID_AGE, "EXP-FL-W-AGE",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]",
                                FIELD_SCHOOL_NUMBER_CIS),
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        ATR(FLWdisValidations.EXPORT_ID_SUFFIX_ATR, "F71124", PROCEDURE_ID_ATR, "EXP-FL-W-ATR",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]",
                                FIELD_SCHOOL_NUMBER_CIS),
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        CTE(FLWdisValidations.EXPORT_ID_SUFFIX_CTE, "F63442", PROCEDURE_ID_CTE, "EXP-FL-W-CTE",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]",
                                FIELD_SCHOOL_NUMBER_CIS),
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        SDI(FLWdisValidations.EXPORT_ID_SUFFIX_SDI, "F63422", PROCEDURE_ID_SDI, "EXP-FL-W-SDI",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]",
                                FIELD_SCHOOL_NUMBER_CIS),
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        SETS(FLWdisValidations.EXPORT_ID_SUFFIX_SETS, "F63427", PROCEDURE_ID_SETS, "EXP-FL-W-SETS",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]",
                                FIELD_SCHOOL_NUMBER_CIS),
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        SUP(FLWdisValidations.EXPORT_ID_SUFFIX_SUP, "F70871", PROCEDURE_ID_SUP, "EXP-FL-W-SUP",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA),
                        new FLRelationshipKey(SisOrganization.class, "localId", FIELD_STUDENT_NUM_ID_FLORIDA,
                                SisStudent.class,
                                "school.organization1")
                })),
        //
        TCRS(FLWdisValidations.EXPORT_ID_SUFFIX_TCRS, "F63432", PROCEDURE_ID_TCRS, "EXP-FL-W-TCRS",
                Arrays.asList(new FLRelationshipKey[] {}));

        private String m_code;
        private String m_exportId;
        private String m_fileNo;
        private HashMap<String, FLRelationshipKey> m_mapRelationshipByClassName;
        private String m_procedureId;
        private List<FLRelationshipKey> m_relationships;


        /**
         * Instantiates a new fl export.
         *
         * @param code String
         * @param fileNo String
         * @param procedureId String
         * @param exportId String
         * @param relationships List<FLRelationshipKey>
         */
        private FL_EXPORT(String code, String fileNo, String procedureId, String exportId,
                List<FLRelationshipKey> relationships) {
            m_code = code;
            m_fileNo = fileNo;
            m_procedureId = procedureId;
            m_exportId = exportId;
            m_relationships = relationships;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getCode()
         */
        @Override
        public String getCode() {
            return m_code;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getExportId()
         */
        @Override
        public String getExportId() {
            return m_exportId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getFileNo()
         */
        @Override
        public String getFileNo() {
            return m_fileNo;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getProcedureId()
         */
        @Override
        public String getProcedureId() {
            return m_procedureId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getRelationshipKeyByClassName(java.lang.String)
         */
        @Override
        public FLRelationshipKey getRelationshipKeyByClassName(String className) {
            if (m_mapRelationshipByClassName == null) {
                m_mapRelationshipByClassName = new HashMap();
                for (FLRelationshipKey relationship : m_relationships) {
                    m_mapRelationshipByClassName.put(relationship.getRelationshipClass().getName(), relationship);
                }
            }
            return m_mapRelationshipByClassName.get(className);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getValidationRules()
         */
        @Override
        public List<FLValidationRule> getValidationRules() {
            return FLWdisValidations.getValidationRules(m_code);
        }


        /**
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return "FL_EXPORT " + getProcedureId();
        }
    }


    /**
     * The Enum FL_SURVEY.
     */
    public enum FL_SURVEY implements FLSurvey {
        SURVEY_F("F", Arrays.asList(new FLExport[] {
                FL_EXPORT.AGE, FL_EXPORT.ATR, FL_EXPORT.CTE, FL_EXPORT.SDI, FL_EXPORT.SETS, FL_EXPORT.SUP,
                FL_EXPORT.TCRS
        })),
        //
        SURVEY_G("G", Arrays.asList(new FLExport[] {
                FL_EXPORT.AGE, FL_EXPORT.CTE, FL_EXPORT.SDI
        })),
        //
        SURVEY_W("W", Arrays.asList(new FLExport[] {
                FL_EXPORT.AGE, FL_EXPORT.ATR, FL_EXPORT.CTE, FL_EXPORT.SDI, FL_EXPORT.SETS, FL_EXPORT.SUP,
                FL_EXPORT.TCRS
        })),
        //
        SURVEY_X("X", Arrays.asList(new FLExport[] {
                FL_EXPORT.AGE, FL_EXPORT.CTE, FL_EXPORT.SDI
        })),
        //
        SURVEY_S("S", Arrays.asList(new FLExport[] {
                FL_EXPORT.AGE, FL_EXPORT.ATR, FL_EXPORT.CTE, FL_EXPORT.SDI, FL_EXPORT.SETS, FL_EXPORT.SUP,
                FL_EXPORT.TCRS
        }));
        private String m_code;
        private List<FLExport> m_exports;


        /**
         * Instantiates a new fl survey.
         *
         * @param code String
         * @param exports List<FLExport>
         */
        private FL_SURVEY(String code, List<FLExport> exports) {
            m_code = code;
            m_exports = exports;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLSurvey#getCode()
         */
        @Override
        public String getCode() {
            return m_code;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLSurvey#getExports()
         */
        @Override
        public List<FLExport> getExports() {
            return m_exports;
        }
    }


    /**
     * The Class FLWdisValidations.
     */
    private static class FLWdisValidations {


        /**
         * The Class RestrictionCompareDependCode.
         */
        public static class RestrictionCompareDependCode extends Restriction {
            private Map<String, String> m_codesDependencies;
            private Field m_comparableField;
            private Object m_comparableValue;
            private String m_fieldName;
            private Operator m_operator;
            private String m_refTableName;


            /**
             * Instantiates a new restriction compare depend code.
             *
             * @param operator Operator
             * @param field String
             * @param refTableName String
             * @param comparableValue Object
             */
            public RestrictionCompareDependCode(Operator operator, String field, String refTableName,
                    Object comparableValue) {
                m_operator = operator;
                m_fieldName = field;
                m_comparableValue = comparableValue;
                m_refTableName = refTableName;
            }

            /**
             * Instantiates a new restriction compare depend code.
             *
             * @param operator Operator
             * @param field String
             * @param refTableName String
             * @param comparableField
             */
            public RestrictionCompareDependCode(Operator operator, String field, String refTableName,
                    Field comparableField) {
                m_operator = operator;
                m_fieldName = field;
                m_comparableField = comparableField;
                m_refTableName = refTableName;
            }


            /**
             * The Class ValueDependentCodeConverter.
             */
            private class ValueDependentCodeConverter implements ValueAdjuster {


                /**
                 * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster#getAdjustedValue(java.lang.String,
                 *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration)
                 */
                @Override
                public String getAdjustedValue(String value, FLExportConfiguration helper) {
                    if (m_codesDependencies == null) {
                        m_codesDependencies = new HashMap<>();

                        X2Criteria refCodesCriteria = new X2Criteria();
                        refCodesCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER
                                + ReferenceTable.COL_USER_NAME, m_refTableName);
                        String[] attributes = {ReferenceCode.COL_CODE, ReferenceCode.COL_DEPENDENCY_CODE};
                        ReportQueryByCriteria query =
                                new ReportQueryByCriteria(ReferenceCode.class, attributes, refCodesCriteria);
                        try (ReportQueryIterator iterator = helper.getBroker().getReportQueryIteratorByQuery(query)) {
                            while (iterator.hasNext()) {
                                Object[] rows = (Object[]) iterator.next();
                                String programCode = (String) rows[0];
                                String totalHours = (String) rows[1];
                                m_codesDependencies.put(programCode, totalHours);
                            }
                        }
                    }
                    value = m_codesDependencies.get(value);
                    if (value == null) {
                        value = "";
                    }
                    return value;
                }
            }


            /**
             * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
             *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
             *      com.follett.fsc.core.k12.beans.ExportFormatRow)
             */
            @Override
            public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                        FLExport export,
                                                        ExportFormatRow row) {
                RestrictionCompare restriction = null;
                if (m_comparableValue != null) {
                    restriction = new RestrictionCompare(m_operator,
                            new Field(m_fieldName, new ValueDependentCodeConverter()), m_comparableValue);
                }
                if (m_comparableField != null) {
                    restriction = new RestrictionCompare(m_operator,
                            new Field(m_fieldName, new ValueDependentCodeConverter()), m_comparableField, String.class);
                }

                return restriction.getValidationResult(helper, export, row);
            }
        }


        /**
         * The Class RestrictionVerticalSumCompare.
         */
        public static class RestrictionVerticalSumCompare extends Restriction {

            ValueAdjuster m_adjuster = null;
            FLExport m_export = null;
            String m_fieldToGroup = null;
            String m_fieldToSum = null;
            Operator m_operator = null;
            Double m_comparableValue = null;


            /**
             * The Class AdjusterVertSumByField.
             */
            public class AdjusterVertSumByField implements ValueAdjuster {


                /**
                 * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster#getAdjustedValue(java.lang.String,
                 *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration)
                 */
                @Override
                public String getAdjustedValue(String fieldToGroupValue, FLExportConfiguration helper) {
                    LookupField lookupKey = helper.new LookupField();
                    LookupField lookupValue = helper.new LookupField();
                    lookupKey.add(m_fieldToGroup);
                    lookupValue.add(fieldToGroupValue);
                    Collection<ExportFormatRow> rows = helper.getExportFormatRows(m_export, lookupKey, lookupValue);
                    Double sum = Double.valueOf(0);
                    for (ExportFormatRow row : rows) {
                        String value = helper.getExportFormatRowFieldValue(row, m_export, m_fieldToSum);
                        Double parsedValue = Double.valueOf(value);
                        sum = Double.valueOf(sum.doubleValue() + parsedValue.doubleValue());
                    }
                    return sum.toString();
                }
            }


            /**
             * Instantiates a new restriction vertical sum compare.
             *
             * @param export FLExport
             * @param operator Operator
             * @param fieldToGroup String
             * @param fieldToSum String
             * @param comparableValue Double
             */
            public RestrictionVerticalSumCompare(FLExport export, Operator operator, String fieldToGroup,
                    String fieldToSum, Double comparableValue) {
                m_export = export;
                m_fieldToGroup = fieldToGroup;
                m_fieldToSum = fieldToSum;
                m_operator = operator;
                m_comparableValue = comparableValue;

                m_adjuster = new AdjusterVertSumByField();
            }


            /**
             * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
             *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
             *      com.follett.fsc.core.k12.beans.ExportFormatRow)
             */
            @Override
            public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                        FLExport export,
                                                        ExportFormatRow row) {
                RestrictionCompare restriction =
                        new RestrictionCompare(m_operator, new Field(m_fieldToGroup, m_adjuster), m_comparableValue);
                return restriction.getValidationResult(helper, export, row);
            }
        }


        /**
         * The Class RestrictionPriorYear.
         */
        public static class RestrictionPriorYear extends Restriction {


            /**
             * The Class ReportingYearAdjuster.
             */
            private static class ReportingYearAdjuster implements ValueAdjuster {
                private static final String PATTERN_REPORTING_YEAR = "\\d{4}";


                /**
                 * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster#getAdjustedValue(java.lang.String,
                 *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration)
                 */
                @Override
                public String getAdjustedValue(String value, FLExportConfiguration helper) {
                    if (value.matches(PATTERN_REPORTING_YEAR)) {
                        int mid = value.length() / 2;
                        String startYear = value.substring(0, mid);
                        String endYear = value.substring(mid);
                        String adjustedStartYear = String.valueOf(Integer.parseInt(startYear) - 1);
                        String adjustedEndYear = String.valueOf(Integer.parseInt(endYear) - 1);
                        value = adjustedStartYear + adjustedEndYear;
                    }
                    return value;
                }
            }


            /**
             * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
             *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
             *      com.follett.fsc.core.k12.beans.ExportFormatRow)
             */
            @Override
            public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                        FLExport export,
                                                        ExportFormatRow row) {
                RestrictionCompare restriction = new RestrictionCompare(Operator.EQUALS,
                        new Field(FIELD_PRIOR_YEAR),
                        new Field(FIELD_WDIS_REPORTING_YEAR, new ReportingYearAdjuster()), String.class);
                return restriction.getValidationResult(helper, export, row);
            }
        }

        public static final String EXPORT_ID_SUFFIX_AGE = "AGE";
        public static final String EXPORT_ID_SUFFIX_ATR = "ATR";
        public static final String EXPORT_ID_SUFFIX_CTE = "CTE";
        public static final String EXPORT_ID_SUFFIX_SDI = "SDI";
        public static final String EXPORT_ID_SUFFIX_SETS = "SETS";
        public static final String EXPORT_ID_SUFFIX_SUP = "SUP";

        public static final String EXPORT_ID_SUFFIX_TCRS = "TCRS";

        public static final String PATTERN_ALL_UNIQUE_LEFT_JUSTIFIED = "^(?:([1-9A-Za-z])(?!.*\\1))* *$";
        public static final String PATTERN_ALL_ZEROS = "^0+$";
        public static final String PATTERN_NOT_EMPTY = ".*[^ ].*";
        public static final String PATTERN_THREE_UNIQUE_PAIRS_LEFT_JUSTIFIED =
                "^(?! +\\S+)(.{2})((?!\\1).{2}| )((?!\\1|\\2).{2}| )$";

        public static final String REF_TABLE_NAME_COURSES_AHS = "FL Course Codes Adult High School Co-Enrolled";
        public static final String REF_TABLE_NAME_PSE = "FL Postsecondary School Of Enrollment";
        public static final String REF_TABLE_WDIS_COURSE_PROGRAM_CODES = "FL WDIS Course Program Codes";
        public static final String REF_TABLE_WDIS_IND_CERT_CODES = "FL WDIS Industry Certification Codes";
        public static final String REF_TABLE_WDIS_PROGRAM_CODES = "FL WDIS Program Codes";

        public static final Restriction m_isSurveyPeriodNotGX =
                Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^(?![GX]).{1}$");


        /**
         * Gets the validation rules.
         *
         * @param exportIdSuffix String
         * @return List
         */
        public static List<FLValidationRule> getValidationRules(String exportIdSuffix) {
            FLWdisValidations validations = new FLWdisValidations();
            List<FLValidationRule> rules = new ArrayList<>();
            for (RuleAssociation association : validations.getRuleAssociations()) {
                FLValidationRule rule = association.getRule(exportIdSuffix);
                if (rule != null) {
                    rules.add(rule);
                }
            }
            return rules;
        }


        /**
         * Gets the rule associations.
         *
         * @return List
         */
        private List<RuleAssociation> getRuleAssociations() {
            List<RuleAssociation> ruleAssociations = new ArrayList<>();
            RuleAssociation ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "3");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "3");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "3");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "4");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "3");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "3");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_STUDENT_NUM_ID_FLORIDA,
                    "^((?!77|78|79)[0-7][0-9][\\d]{8})|((?!000)[\\d]{9}[X])$",
                    "The first nine positions of Student Number Identifier, Florida must be numeric. "
                            + "The tenth position of Student Number Identifier, Florida must either be an \"X\" or numeric. "
                            + "If the tenth position of Student Number Identifier, Florida is numeric, "
                            + "the first two digits must be a valid district number in the range 01-76. "
                            + "If the tenth position of the Student Number Identifier, Florida is an 'X', "
                            + "the first three positions may not all be zeroes."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "4");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "4");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "5");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_SURVEY_PERIOD_CODE, "^[FWSGX]$",
                    "Survey Period Code must be F, W, S, G, or X and must be correct for the submission specified by the "
                            + "district."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "5");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "5");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "5");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "5");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "5");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "4");
            ruleAssociation.setProcessor(new ValidateWdisReportingYear());
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "20");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "20");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "8");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "20");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "11");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_TRANSACTION_CODE,
                    "^[ACD]$",
                    "The Transaction Code must be A, C, or D. For the original transmission, only A is valid. "
                            + "For subsequent batch/update submissions, if A is specified then the record must "
                            + "not already exist on the data base; if C or D is specified then the record must "
                            + "exist on the data base."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "7");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "7");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "3");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_SCHOOL_NUMBER_CIS,
                    "^(?!99|0000)[\\d]{4}$",
                    "School Number, Current Instruction/Service must be numeric in the range 0001 to 9899."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "8");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "1");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "1");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "6");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "1");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_DISTRICT_NUMBER_CIS,
                    "^(?!00|7[1,7-9])[0-7][0-9]$",
                    "District Number, Current Instruction/Service must be numeric and in the range 01-70 or 72-76 "
                            + "and must be correct for the district submitting the data."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "9");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "10");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "6");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_SECTION_NUMBER,
                    PATTERN_NOT_EMPTY,
                    "Section Number must not be all blanks."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "12");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "19");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "17");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "13");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_GRADE_LEVEL,
                    "^09|1[0-2]|3[0,1]$",
                    "Grade Level must be 09-12, 30 or 31."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "13");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^9900010$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^30|31$")),
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^9900099$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|10|11|12$"))),
                    "If CTE/Adult General Education Program Code is equal to 9900010, then Grade Level must be 30 or 31. "
                            + "If CTE/Adult General Education Program Code is equal to 9900099, then Grade Level must "
                            + "be 09-12."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "14");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(m_isSurveyPeriodNotGX)
                                    .testThen(Restriction.pattern(FIELD_POST_TEST_STATUS, "^[YNZ]$"))),
                    "Post Test Status must be equal to Y, N or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "16");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "40");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^9900099$"))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_DISABILITY_STUDENT,
                                    "^Z$"))),
                    "If CTE/Adult General Education Program Code is equal to 9900099, then CTE/Adult General Education, "
                            + "Disability, Student must be Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "17");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_CTE_AGE_DISABILITY_STUDENT,
                    "^[ABCINZ]$",
                    "The code for CTE/Adult General Education, Disability, Student must be A, B, C, I, N or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "18");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "14");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.equals(FIELD_SURVEY_PERIOD_CODE, "^[FWS]$"))
                            .testThen(Restriction.and(
                                    Restriction.byDateFormat(FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION),
                                    Restriction.byDateFormat(FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION))),
                            ValidationRule.testIf(Restriction.equals(FIELD_SURVEY_PERIOD_CODE, "^[GX]$"))
                                    .testThen(Restriction.and(
                                            Restriction.byDateFormat(FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION),
                                            Restriction.pattern(FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                                    PATTERN_ALL_ZEROS)))),
                    "If Survey is equal to F, W, or S then the Date of Entry, Program/Course/Section and the Date of Exit, "
                            + "Program/Course/Section must be a valid date in the format of MMDDYYYY. If Survey is equal to G or X, "
                            + "then the Date of Entry, Program/Course/Section must be a valid date in the format of MMDDYYYY, "
                            + "and the Date of Exit, Program/Course/Section must be equal to zeroes."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "19");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_ENROLLMENT_NOT_STATE_FUNDED,
                    "^[FISZ]$",
                    "Enrollment, Not State Funded must be F, I, S, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "21");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "13");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "22");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "14");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "10");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "7");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_FLORIDA_EDUCATION_IDENTIFIER,
                    "^FL(?!0{8}(0|1|20{3}))[\\d]{12}$",
                    "Florida Education Identifier (FLEID) is alphanumeric and must be entered "
                            + "as 'FL' in the first 2 positions followed by twelve numeric digits and "
                            + "must be greater than FL000000002000. No blanks or spaces are allowable."));
            ruleAssociations.add(ruleAssociation);

            // TODO finish when F61730 will be available
            // ruleAssociation = new RuleAssociation();
            // ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "22");
            // ruleAssociation.setProcessor(new ValidateRegularExpression(
            // FIELD_FLORIDA_EDUCATION_IDENTIFIER,
            // "",
            // "The CTE/Adult General Education Program Code must be an adult general education
            // "
            // + "program number from the CTE/Adult General Education Program Edit file "
            // + "(F61730)."));
            // m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "23");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "23");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^401$"))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^9{2}0{5}$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^402$"))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                            "^9{2}0{3}(10|99)$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^403$"))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                            "^9{2}0{2}130$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^404$"))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                            "^9{2}0{2}(040|050|051|300)$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^405$"))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                            "^S990001$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^409$"))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                            "^9{2}0{2}(090|100)$"))),
                    "The CTE/Adult General Education Program Code must be a valid code for the Cost Reporting Code."));
            ruleAssociations.add(ruleAssociation);

            // TODO finish when F61730 will be available
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "25");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(
                            FIELD_CTE_AGE_PROGRAM_CODE, "^(?!9{2}0{3}(10|99)).*$"))
                            .testThen(Restriction.pattern(FIELD_COURSE_NUMBER, "^\\S+$"))),
                    "Course Number must not contain blanks and must be a valid Course Number for the "
                            + "CTE/Adult General Education Program Code on the CTE/Adult General Education Program Edit "
                            + "file (F61730) unless the CTE/Adult General Education Program Code equals 9900010 or 9900099."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "26");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "26");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(
                            FIELD_SURVEY_PERIOD_CODE, "^[GX]$"))
                            .testThen(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$")),
                            ValidationRule.testIf(Restriction.pattern(
                                    FIELD_SURVEY_PERIOD_CODE, "^[FWS]$"))
                                    .testThen(new RestrictionPriorYear())),
                    "Prior Year must be numeric. If it is greater than zero, it must be a valid WDIS Reporting Year and must "
                            + "be one year prior to the current valid WDIS Reporting Year. If Survey = G or X, Prior Year must = 0000."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "27");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^40[1-59]$"))
                            .testThen(Restriction.and(
                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[DIJMNOPQRTWZ] *$"),
                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^[DIJMNOPQRTWZ]{0,1} *$"),
                                    Restriction.notEqualsFieldValue(FIELD_ADULT_FEE_STATUS_FIRST,
                                            FIELD_ADULT_FEE_STATUS_SECOND, String.class))),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^402$"))
                                    .testThen(
                                            Restriction.and(
                                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[C] *$"),
                                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND,
                                                            "^[C]{0,1} *$"),
                                                    Restriction.notEqualsFieldValue(FIELD_ADULT_FEE_STATUS_FIRST,
                                                            FIELD_ADULT_FEE_STATUS_SECOND, String.class)))),
                    "The Adult Fee Status (First) code must be left justified and must be a valid code for the Cost Reporting Code area "
                            + "as indicated below. The Adult Fee Status (Second) code must be left justified and must be a valid code for the "
                            + "Cost Reporting Code area and must not equal Adult Fee Status (First), or it must be blank."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "28");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[Z] *$"))
                            .testThen(Restriction.pattern(FIELD_ENROLLMENT_NOT_STATE_FUNDED, "^[FIS]$"))),
                    "If Adult Fee Status (First) is Z, then Enrollment, Not State Funded must be F, I, or S."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "29");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "21");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.uniqueValue(
                            FIELD_STUDENT_NUM_ID_FLORIDA,
                            FIELD_SURVEY_PERIOD_CODE,
                            FIELD_WDIS_REPORTING_YEAR,
                            FIELD_DISTRICT_NUMBER_CIS,
                            FIELD_SCHOOL_NUMBER_CIS,
                            FIELD_COURSE_NUMBER,
                            FIELD_SECTION_NUMBER,
                            FIELD_PRIOR_YEAR))),
                    "Each record must be unique based on Student Number Identifier, Florida; Survey Period Code; WDIS Reporting Year; "
                            + "District Number, Current Instruction/Service; School Number, Current Instruction/ Service; Course Number; "
                            + "Section Number; and Prior Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "30");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[Z] *$"))
                            .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^ *$"))),
                    "If Adult Fee Status (First) is Z, the Adult Fee Status (Second) must be blank."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "32");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{5}$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{4}[1-3]$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[BFHJ]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}10$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[KLMN]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}99$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[KLMNZ]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}90$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{3}90$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[Z]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}100$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{2}100$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[BFHJKLMNZ]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}130$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{2}13[1-6]$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[KLMN]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}40$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{3}40$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[1-6]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}50$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{3}50$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[DE]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}51$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{3}51$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[7]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}51$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{3}51$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[7]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}300$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^9{2}0{2}300$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[QRS]$"))),

                            ValidationRule.testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^S9{2}0{3}1$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_COURSE_NUMBER, "^S9{2}0{2}41$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^[KLMN]$")))),

                    "The Adult Educational Functioning Level, Initial code used must agree with the CTE/Adult General Education "
                            + "Program Code and the Course Number in the following table"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "33");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "45");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(Restriction.and(Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^[WS]$"),
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^W *$")))
                                    .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^[^Q] *$")),
                            ValidationRule
                                    .testIf(Restriction.and(Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^[WS]$"),
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^Q *$")))
                                    .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[^W] *$"))),
                    "If Survey = W or S and Adult Fee Status (First) is W, then Adult Fee Status (Second) must not be Q; "
                            + "and vice versa."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "34");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_CTE_AGE_PROGRAM_CODE,
                    "^(9{2}0{2}([0-2][0-9][0-9]|300))|S9{2}0{3}1$",
                    "The CTE/Adult General Education Program Code must be 9900000 -� 9900300 or S990001."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "39");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_COST_REPORTING_CODE, "^40[1-59]$",
                    "Cost Reporting Code must be 401-405, or 409."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "40");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "2");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, PATTERN_NOT_EMPTY))),
                    "The CTE/Adult General Education Completion Point Code must not be all blanks or spaces. "
                            + "Not Required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "41");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "32");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^.*Z.*$")))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^Z {5}$"))),
                    "If the CTE/Adult General Education Completion Point Code equals Z, then it must be "
                            + "left justified followed by five blanks. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "42");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ArrayUtils.addAll(getCompletionPointCodesRules(), new ValidationRule[] {
                                    ValidationRule
                                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                                    "^9{2}0{3}(10|99)$"))
                                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                                    PATTERN_THREE_UNIQUE_PAIRS_LEFT_JUSTIFIED)),
                                    ValidationRule
                                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                                    "^(?!9{2}0{3}(10|99)).*$"))
                                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                                    PATTERN_ALL_UNIQUE_LEFT_JUSTIFIED))
                            })),
                    "If the CTE/Adult General Education Program Code is not equal to 9900010 or 9900099, "
                            + "the CTE/Adult General Education Completion Point Code is a single byte that occurs "
                            + "six times. The value of each occurrence cannot be repeated except for blanks. "
                            + "The six byte field must be left justified with no embedded blanks, and must be valid for "
                            + "the CTE/Adult General Education Program Code reported for the student.\r\n"
                            + "If the CTE/Adult General Education Program Code equals 9900010 or 9900099, "
                            + "the CTE/Adult General Education Completion Point Code is two bytes that occur "
                            + "three times. The values of each occurrence cannot be repeated except for blanks. "
                            + "The six byte field must be left justified, with no embedded blanks, and must be "
                            + "valid for the CTE/Adult General Education Program Code reported for the student."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "43");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}9{0}(01|04|05|00|13)0$"),
                                    Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^(?!.*Z.*).*$")))
                            .testThen(Restriction
                                    .byDateFormat(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED)),
                            ValidationRule
                                    .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                            Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^.*Z.*$")))
                                    .testThen(Restriction.pattern(
                                            FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED,
                                            PATTERN_ALL_ZEROS))),
                    "If CTE/Adult General Education Program Code is equal to 9900010, 9900040, 9900050, 9900000, "
                            + "or 9900130, and the CTE/Adult General Education Completion Point Code is not Z, the Adult "
                            + "General Education Literacy Completion Date Earned must be a valid date in the format of "
                            + "MMDDYYYY, else it must be all zeroes. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "49");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}9{2}$")))
                            .testThen(
                                    Restriction.byReferenceCodes(REF_TABLE_NAME_COURSES_AHS, FIELD_COURSE_NUMBER))),
                    "If Prior Year is equal to zero and the CTE/Adult General Education Program Code is equal to 9900099, "
                            + "then the Course Number must be a valid course number on the list of approved courses for "
                            + "Adult High School Co-Enrolled for the current reporting year (see Appendix D)."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "50");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}(10|99)$"))
                            .testThen(Restriction.pattern(FIELD_COURSE_NUMBER, "(?!00|0100[0-2]|249999[1-9]|2[5-9])"
                                    + "^(?!7[0-8]|790|791000|798099[1-9]|798[1-9]|799)"
                                    + "(?!800[0-1]|970100[1-9]|97010[1-9]|9701[1-9]|970[2-9]|97[1-9]|9[8-9])"
                                    + "[0-27-9]\\d{6}$"))),
                    "If the CTE/Adult General Education Program Code equals 9900010 or 9900099, Course Number must "
                            + "be a valid number on the Course Code Directory file and in the range 0100300-2499990, "
                            + "7910010-7980990, or 8002000-9701000."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "51");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^[A-HZ]+ *$"))),
                    "Financial Assistance must be A-H, or Z, and must be left justified with trailing blanks. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "53");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^.*[A-H].*$")))
                            .testThen(Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^(?!.*Z.*).*$"))),
                    "If Financial Assistance equals A-H, the value Z cannot be reported on the same record. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "54");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "65");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.greaterThan(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                    Double.valueOf(0)))),
                    "WDIS Student Instructional Hours must be numeric and greater than zero."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "57");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{5}$"),
                                    Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^B$")))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^[A-HJKMNZ ]+$")),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{5}$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^F$")))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                            "^[BCDFGHKMNZ ]+$")),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{5}$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^H$")))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                            "^[CDGHMNZ ]+$")),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{5}$"),
                                            Restriction.pattern(FIELD_ADULT_ED_FUNC_LEVEL_INIT, "^J$")))
                                    .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                            "^[DHNZ ]+$"))),
                    "When the CTE/Adult General Education Program Code equals 9900000, the Adult Educational "
                            + "Functioning Level, Initial and CTE/Adult General Education Completion Point Code "
                            + "must conform to the following relationship"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "58");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}(10|40|50|99|00)$")))
                            .testThen(Restriction.greaterThanOrEquals(FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                    FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED))),
                    "If Prior Year = 0000 and Survey = S and CTE/Adult General Education Program Code is equal to "
                            + "9900010, 9900040, 9900050, 9900099, or 9900000, then the Adult General Education Literacy "
                            + "Completion Date Earned must not be greater than the Date of Exit, Program/Course/Section. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "70");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_TCRS,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_SCHOOL_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                    getWdisFieldsPair(FIELD_SECTION_NUMBER)))),
                    "If Prior Year = 0000, each WDIS Adult General Education Student Course record must have "
                            + "a matching WDIS Teacher Course record based on the key fields of District Number, "
                            + "Current Instruction/Service; School Number, Current Instruction/ Service; "
                            + "Survey Period Code; WDIS Reporting Year; Course Number; "
                            + "and Section Number. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "71");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "Each WDIS Adult General Education Student Course record must have a matching "
                            + "WDIS Student Demographic record based on District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; and WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "72");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SETS,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "Each WDIS Adult General Education Student Course must have a matching WDIS Student End "
                            + "of Term Status record based on District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code and WDIS Reporting Year. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "73");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SETS,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(Restriction.pattern(FIELD_AGE_WITHDRAWAL_REASON, "^[A-GN]$"),
                                            null)))),
                    "Each WDIS Adult General Education Student Course record must have a matching "
                            + "WDIS Student Demographic record based on District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; and WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "74");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}(10|99)$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_ADULT_HIGH_SCHOOL_DIPLOMA_OPTION, "^[AB]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900010 or 9900099, "
                            + "a matching WDIS Student Demographic record based on District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; and WDIS Reporting Year "
                            + "must exist with the Adult High School Diploma Option equal to A or B."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "75");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_AGE_EMPLOYMENT_BARRIERS, "^[CN]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900000, 9900010, "
                            + "9900040, 9900050 or 9900130, a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                            + "Survey Period Code; and WDIS Reporting Year must exist with the Adult General Education, "
                            + "Employment Barriers equal to C or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "76");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_AGE_EX_OFFENDER, "^[EN]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900000, 9900010, "
                            + "9900040, 9900050 or 9900130, a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                            + "Survey Period Code; and WDIS Reporting Year must exist with the Adult General Education, "
                            + "ExOffender equal to E or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "77");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_AGE_HOMELESS_INDIVIDUALS_OR_RUNAWAY_YOUTH,
                                                    "^[ABCDN]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900000, 9900010, "
                            + "9900040, 9900050 or 9900130, a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                            + "Survey Period Code; and WDIS Reporting Year must exist with the "
                            + "AGE Homeless Individuals or Runaway Youth equal to A, B, C, D or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "78");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_AGE_MIGRANT_AND_SEASONAL_FARMWORKER,
                                                    "^[ABN]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900000, 9900010, "
                            + "9900040, 9900050 or 9900130, a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                            + "Survey Period Code; and WDIS Reporting Year must exist with the "
                            + "Adult General Education, Migrant and Seasonal Farmworker equal to A, B or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "79");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, "^0000$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{2}(00|01|04|05|13)0$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    new KeyValuePair(
                                            Restriction.pattern(FIELD_EMPLOYMENT_STATUS,
                                                    "^[ENSU]$"),
                                            null)))),
                    "If Prior Year = 0000 and CTE/Adult General Education Program Code is equal to 9900000, 9900010, "
                            + "9900040, 9900050 or 9900130, a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                            + "Survey Period Code; and WDIS Reporting Year must exist with the "
                            + "Employment Status equal to E, N, S, or U."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "82");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^40[1-59]$"))
                            .testThen(new RestrictionVerticalSumCompare(FL_EXPORT.AGE, Operator.LESS_THAN_OR_EQUALS,
                                    FIELD_STUDENT_NUMBER_IDENTIFIER_ALIAS_FLORIDA,
                                    FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS, Double.valueOf(450)))),
                    "If Cost Reporting Code is 401-405, or 409 the sum of all WDIS Student Instructional Hours "
                            + "reported for the student within the survey must be 450 or less."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "85");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}(0|4|5)0$"),
                                    Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^(?!.*Z.*).*$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_ATR,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "If the CTE/Adult General Education Program Code is equal to 9900000, 9900040, "
                            + "or 9900050 and the CTE/Adult General Education Completion Point Code "
                            + "not equal to Z, then there must be at least one matching test record "
                            + "on the Adult Test Record format based on District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; and WDIS Reporting Year. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "86");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "86");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^F$")))
                            .testThen(Restriction.greaterThanOrEquals(FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION,
                                    getDate(3, 1, 2017))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$")))
                                    .testThen(Restriction.greaterThanOrEquals(
                                            FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION,
                                            getDate(7, 1, 2017))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$")))
                                    .testThen(Restriction.greaterThanOrEquals(
                                            FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION,
                                            getDate(0, 1, 2018)))),
                    "If Prior Year = 0000 and Survey = F, then the Date of Entry, Program/Course/Section cannot be "
                            + "before April 1, 2017. If Prior Year = 0000 and Survey = W, then the "
                            + "Date of Entry, Program/Course/Section cannot be before August 1, 2017. "
                            + "If Prior Year = 0000 and Survey = S, then the Date of Entry, Program/Course/Section "
                            + "cannot be before January 1, 2018."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "87");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "87");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^F$")))
                            .testThen(Restriction.lessThanOrEquals(FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                    getDate(8, 10, 2017))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$")))
                                    .testThen(Restriction.lessThanOrEquals(
                                            FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                            getDate(0, 31, 2018))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$")))
                                    .testThen(Restriction.lessThanOrEquals(
                                            FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                            getDate(6, 4, 2018)))),
                    "If Prior Year = 0000 and Survey = F, then the Date of Exit, Program/Course/Section "
                            + "cannot be after September 10, 2017. If Prior Year = 0000 and Survey = W, "
                            + "then the Date of Exit, Program/Course/Section cannot be after January 31, 2018. "
                            + "If Prior Year = 0000 and Survey = S, then the Date of Exit, Program/Course/Section "
                            + "cannot be after July 4, 2018."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "88");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^F$")))
                            .testThen(Restriction.lessThanOrEquals(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                    Double.valueOf(400))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$")))
                                    .testThen(Restriction.lessThanOrEquals(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                            Double.valueOf(650))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$")))
                                    .testThen(Restriction.lessThanOrEquals(
                                            FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                            Double.valueOf(700)))),
                    "If Prior Year = 0000 and Survey = F, the WDIS Student Instructional Hours cannot "
                            + "be greater than 400. If Prior Year = 0000 and Survey = W, the "
                            + "WDIS Student Instructional Hours cannot be greater than 650. "
                            + "If Prior Year = 0000 and Survey = S, the WDIS Student Instructional Hours "
                            + "cannot be greater than 700."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "89");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    getWdisFieldsPair(FIELD_GRADE_LEVEL)))),
                    "Grade Level on WDIS Adult General Education Student Course record does not match "
                            + "Grade Level on WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; "
                            + "and WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_AGE, "90");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$"),
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9{2}0{3}(00|10|40|50|99)$")))
                            .testThen(Restriction.lessThanOrEqualsFieldValue(
                                    FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED,
                                    FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION,
                                    Date.class))),
                    "If Prior Year = 0000 and Survey = W and CTE/Adult General Education Program Code is equal to "
                            + "9900010, 9900040, 9900050, 9900099, or 9900000, then the Adult General Education "
                            + "Literacy Completion Date Earned must not be greater than the Date of Exit, "
                            + "Program/Course/Section."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "1");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.uniqueValue(FIELD_DISTRICT_NUMBER_CIS, FIELD_SCHOOL_NUMBER_CIS,
                                    FIELD_STUDENT_NUM_ID_FLORIDA, FIELD_SURVEY_PERIOD_CODE,
                                    FIELD_WDIS_REPORTING_YEAR, FIELD_ADULT_TEST_NAME, FIELD_TEST_DATE,
                                    FIELD_ADULT_TEST_SUBJECT_CONTENT, FIELD_ADULT_TEST_LEVEL,
                                    FIELD_ADULT_TEST_FORM))),
                    "Each Adult General Education Test record must be unique based on "
                            + "District Number, Current Instruction/Services; School Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; Survey Period Code; WDIS Reporting Year; "
                            + "Adult Test Name; Test Date; Adult Test Subject Content;"
                            + " Adult Test Level; and Adult Test Form."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "4");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "4");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "3");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_SURVEY_PERIOD_CODE, "^[FWS]$",
                    "Survey Period Code must be F, W, or S and must be correct for the submission specified by the district."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "8");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_ADULT_TEST_NAME, "^(TAB|TBE|CAS|BSP|BSL)$",
                    "Adult Test Name must be TAB, TBE, CAS, BSP, or BSL."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "9");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_ADULT_TEST_SUBJECT_CONTENT, "^[MNORSW] $",
                    "Adult Test Subject Content must be M, N, O, R, S, or W."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "10");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^(BSL|BSP)$"))
                            .testThen(Restriction.pattern(FIELD_ADULT_TEST_FORM, "^([BCD]|NA) *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^CAS$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_FORM,
                                            "^(18[5-8]R|2[78]R|3[1-8]M|8[1-6]R|8[12]RX|98[1-6]L) *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^(TAB|TBE)$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_FORM,
                                            "^(1[0-2]|[9AB]) *$"))),
                    "Adult Test Form must be left justified with trailing blanks, and must be valid for the test reported."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "11");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^BSP$"))
                            .testThen(Restriction.pattern(FIELD_ADULT_TEST_LEVEL, "^NA *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^BSL$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_LEVEL, "^[BCD] *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^CAS$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_LEVEL, "^[ABCD] *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^TBE$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_LEVEL, "^[1-4] *$")),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_TEST_NAME, "^TAB$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_TEST_LEVEL, "^[ADELM] *$"))),
                    "Adult Test Level must be left justified with trailing blanks, and must be valid for the test reported."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "12");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_ADULT_TEST_SCORE, "^0*[\\d]+$",
                    "Adult Test Score must be numeric, right justified with leading zeroes."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_ATR, "14");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.and(Restriction.byDateFormat(FIELD_TEST_DATE),
                                    Restriction.lessThanOrEquals(FIELD_TEST_DATE, new Date())))),
                    "Test Date must be numeric, in the format MMDDYYYY, and a valid date that is not in the future."));
            ruleAssociations.add(ruleAssociation);

            // TODO when Master School Identification File will be available
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "7");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "2");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "18");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.pattern(FIELD_SCHOOL_NUMBER_CIS, "^(?!99|0000)[\\d]{4}$"))),
                    "The School Number, Current Instruction/Services must be numeric in the range 0001-9899 "
                            + "and it must exist on the Master School Identification File as a valid "
                            + "number in the District Number, Current Instruction/Services."));
            ruleAssociations.add(ruleAssociation);

            // TODO when F61730 will be available
            // ruleAssociation = new RuleAssociation();
            // ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "8");
            // ruleAssociation.setProcessor(new FLValidationRuleSet(
            // new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
            // .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
            // "^(?!99|0000)[\\d]{4}$"))),
            // "The CTE/Adult General Education Program Code must be a postsecondary Career and
            // Technical Education "
            // + "program number on the CTE/Adult General Education Program Edit file
            // (F61730)."));
            // m_ruleAssociations.add(ruleAssociation);

            // TODO when F61730 will be available
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "9");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_COURSE_NUMBER, "^[\\S]*$",
                    "Course Number must not contain blanks and must be a valid postsecondary CTE Course Number "
                            + "(other than \"\"local use only transfer\"\" courses) on the "
                            + "CTE/Adult General Education Program Edit file (F61730)."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "12");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX).testThen(
                            Restriction.pattern(FIELD_TOTAL_CLOCK_HOURS_EARNED_TOWARDS_AWARD, "^[\\d]+$"))),
                    "Total Clock Hours Earned Toward Award must be numeric. Not Required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "13");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.and(m_isSurveyPeriodNotGX,
                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^[VAP]$"))).testThen(
                                    Restriction.byDateFormat(FIELD_CTE_DATE_OF_PROGRAM_COMPLETION)),
                            ValidationRule.testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^(?![VAP]).*$"))).testThen(
                                            Restriction.pattern(FIELD_CTE_DATE_OF_PROGRAM_COMPLETION,
                                                    PATTERN_ALL_ZEROS))),
                    "If Full Program Completer is equal to V, A, or P then CTE Date of Program Completion "
                            + "must be a valid date in the format of MMDDYYYY, else it must be zeroes. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "15");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_RESIDENCY_FOR_TUITION_PURPOSES,
                    "^[FNDZ]$", "Residency for Tuition Purposes must be F, N, D, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "16");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.and(m_isSurveyPeriodNotGX,
                            Restriction.pattern(FIELD_COST_REPORTING_CODE, "^35[1-79]|364$")))
                            .testThen(Restriction.pattern(FIELD_FULL_TIME_STUDENT_INDICATOR, "^[FPS]$")),
                            ValidationRule.testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^3(41|7[12])$")))
                                    .testThen(Restriction.pattern(FIELD_FULL_TIME_STUDENT_INDICATOR, "^[Z]$"))),
                    "If Cost Reporting Code is equal to 351, 352, 353, 354, 355, 356, 357, 359, or 364 "
                            + "then Full Time Student Indicator* must be equal to F, P, or S. "
                            + "If Cost Reporting Code is equal to 341, 371, or 372 then "
                            + "Full Time Student Indicator* must be equal to Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "17");
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "10");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.byReferenceCodes(REF_TABLE_NAME_PSE,
                                    FIELD_POSTSECONDARY_SCHOOL_OF_ENROLLMENT))),
                    "The Postsecondary School of Enrollment must be a valid postsecondary school number "
                            + "listed in Appendix H for the District Number, Current Instruction/Services."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "18");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_CTE_AGE_DISABILITY_STUDENT, "^[ABCINZ]$",
                            "CTE/Adult General Education, Disability Student code must be A, B, C, I, N, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "24");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                            .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^Q$")),
                    ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^Q$"))
                            .testThen(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))),
                    "If the Dual Enrollment Indicator is equal to C, then the Adult Fee Status (First) "
                            + "must be equal to Q, and vice versa."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "25");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.or(
                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^A{2}|B{2}|C|{2}|H$"),
                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^A{2}|B{2}|C|{2}|H$")))
                            .testThen(Restriction.pattern(FIELD_RESIDENCY_FOR_TUITION_PURPOSES, "^N$"))),
                    "If the Adult Fee Status (First) OR the Adult Fee Status (Second) is equal to "
                            + "AA, BB, CC, or H, then the Residency for Tuition Purposes must be equal to N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "27");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_COST_REPORTING_CODE,
                    "^341|35[1-79]|364|37[12]$",
                    "Cost Reporting Code must equal 341, 351, 352, 353, 354, 355, 356, 357, 359, 364, 371, or 372."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "28");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^364$"))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^A020(4|6)08|H170(205|408|508|528|530|600|113|700)|M811058|W170(208|212)$")),
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE,
                                    "^A020(4|6)08|H170(205|408|508|528|530|600|113|700)|M811058|W170(208|212)$"))
                            .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^364$"))),
                    "If the Cost Reporting Code is 364, then CTE/Adult General Education Program Code must be a valid "
                            + "code for which Applied Technical Diploma may be reported (A020408, A020608, H170205, "
                            + "H170408, H170508, H170528, H170530, H170600, H170113, H170700, M811058, W170208, "
                            + "W170212) and vice versa."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "29");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_DISABILITY_STUDENT, "^Z$"))),
                    "If the Dual Enrollment Indicator is equal to C, then CTE/Adult General Education, Disability, "
                            + "Student must be Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "31");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                    .testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                    .testThen(Restriction.pattern(FIELD_CTE_DUAL_ENR_CRS_LOCATION,
                            "^[ABCD]$")),
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^[^C]$"))
                            .testThen(
                                    Restriction.pattern(FIELD_CTE_DUAL_ENR_CRS_LOCATION, "^[Z]$"))),
                    "If Dual Enrollment Indicator equals C, then CTE Dual Enrollment Course Location must be "
                            + "A, B, C or D else CTE Dual Enrollment Course Location must be Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "33");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    getCompletionPointCodesRules(Restriction.and(
                            m_isSurveyPeriodNotGX,
                            Restriction.pattern(FIELD_COST_REPORTING_CODE,
                                    "^(35[1-79]|364|37[12])$"),
                            Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE,
                                    "^(?!.*Z.*).*$")))),
                    "If the Cost Reporting Code is 351, 352, 353, 354, 355, 356, 357, 359, 364, 371, or 372, "
                            + "and the CTE/Adult General Education Completion Point Code is other than Z, "
                            + "then the reported codes must be left justified, with no embedded blanks, "
                            + "with no value repeated and valid for the CTE/Adult General Education Program Code. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "34");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX,
                                    new RestrictionCompareDependCode(Operator.LESS_THAN, FIELD_CTE_AGE_PROGRAM_CODE,
                                            REF_TABLE_WDIS_PROGRAM_CODES,
                                            Double.valueOf(450)))
                            .testThen(Restriction.pattern(FIELD_CTE_BASIC_SKILLS_EXAMINATION, "^G$")),
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX, Restriction.pattern(FIELD_COST_REPORTING_CODE, "^341$"))
                            .testThen(Restriction.pattern(FIELD_CTE_BASIC_SKILLS_EXAMINATION, "^Z$")),
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX,
                                    new RestrictionCompareDependCode(Operator.GREATER_THAN_OR_EQUALS,
                                            FIELD_CTE_AGE_PROGRAM_CODE,
                                            REF_TABLE_WDIS_PROGRAM_CODES,
                                            Double.valueOf(450)),
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^(?!341).*$"))
                            .testThen(Restriction.pattern(FIELD_CTE_BASIC_SKILLS_EXAMINATION, "^[ABCDFNPY]$"))),
                    "If the total program hours associated with the CTE Program Code are less than 450, then the "
                            + "CTE Basic Skills Examination code must equal G. If the Cost Reporting Code is "
                            + "equal to 341 then the CTE Basic Skills Examination code must be equal to Z. "
                            + "Else the CTE Basic Skills Examination code must equal A, B, C, D, F, N, P, or Y. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "35");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX, Restriction.pattern(FIELD_COST_REPORTING_CODE, "^341$"))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^.*Z.*$"))),
                    "If Cost Reporting Code is 341, CTE/Adult General Education Completion Point Code must = Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "36");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^37[12]$"))
                            .testThen(
                                    Restriction.and(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^\\S{6}[R]$"),
                                            Restriction.byReferenceCodes(REF_TABLE_WDIS_PROGRAM_CODES,
                                                    FIELD_CTE_AGE_PROGRAM_CODE)))),
                    "If Cost Reporting Code = 371 or 372, then the CTE/Adult General Education Program Code "
                            + "must be a valid code for which apprenticeship enrollment may be reported "
                            + "and the seventh character of the CTE/Adult General Education Program Code "
                            + "must = R."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "37");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_DUAL_ENROLLMENT_INDICATOR, "^[CZ]$",
                    "Dual Enrollment Indicator must be C or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "38");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))),
                    "If Dual Enrollment Indicator equals C, then Grade Level must be 09-12."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "39");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                            .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^35[1-79]|364$"))),
                    "If Dual Enrollment Indicator equals C, then Cost Reporting Code must "
                            + "be 351, 352, 353, 354, 355, 356, 357, 359, or 364."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "40");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction
                            .and(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^(?!350|370)3[56][0-9]$"),
                                    Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^Z$")))
                            .testThen(Restriction.pattern(FIELD_RESIDENCY_FOR_TUITION_PURPOSES, "^[^Z]$"))),
                    "If Cost Reporting Code is 351-369 and Dual Enrollment Indicator is equal to Z, "
                            + "the Residency for Tuition Purposes may not be equal to Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "41");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_DUAL_ENROLLMENT_INDICATOR, "^C$"))
                            .testThen(Restriction.pattern(FIELD_CTE_BASIC_SKILLS_EXAMINATION, "^[BCGNPY]$"))),
                    "If Dual Enrollment Indicator equals C, then CTE Basic Skills Exam must equal B, C, G, N, P, or Y"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "43");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^341$"))
                            .testThen(Restriction.and(
                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[DIRW] *$"),
                                    Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND, "^[DIRW]{0,1} *$"),
                                    Restriction.notEqualsFieldValue(FIELD_ADULT_FEE_STATUS_FIRST,
                                            FIELD_ADULT_FEE_STATUS_SECOND, String.class))),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^35[1-79]|364$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST,
                                                    "^([BDHIJMNOPQRTW]|AA|BB|CC) *$"),
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND,
                                                    "^([BDHIJMNOPQRTW]|AA|BB|CC){0,1} *$"),
                                            Restriction.notEqualsFieldValue(FIELD_ADULT_FEE_STATUS_FIRST,
                                                    FIELD_ADULT_FEE_STATUS_SECOND, String.class))),
                            ValidationRule.testIf(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^37[12]$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST,
                                                    "^[K] *$"),
                                            Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND,
                                                    "^ *$"))),
                            ValidationRule.testIf(Restriction.pattern(FIELD_ADULT_FEE_STATUS_FIRST, "^[DRW] *$"))
                                    .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_SECOND,
                                            "^(H|AA|BB|CC){0,1} *$"))),
                    "The Adult Fee Status (First) code must be left justified and must be a valid code for the "
                            + "Cost Reporting Code area as indicated below. The Adult Fee Status (Second) "
                            + "code must be left justified and must be a valid code for the Cost Reporting "
                            + "Code area and must not equal Adult Fee Status (First), or it must be blank. "
                            + "If the Adult Fee Status (First) code is D, R or W, the Adult Fee Status (Second) "
                            + "must be H, AA, BB, CC or blank and must be left justified."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "44");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(new RestrictionCompareDependCode(Operator.EQUALS, FIELD_COURSE_NUMBER,
                                    REF_TABLE_WDIS_COURSE_PROGRAM_CODES, new Field(FIELD_CTE_AGE_PROGRAM_CODE)))),
                    "The Course Number must be valid for the CTE/Adult General Education Program Code submitted."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "46");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$"),
                                    Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^V$")))
                            .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^35[1-79]$")),
                            ValidationRule
                                    .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$"),
                                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^A$")))
                                    .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^364$")),
                            ValidationRule
                                    .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$"),
                                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^P$")))
                                    .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^37[12]$"))),
                    "If Survey = S, then if Full Program Completer = V, Cost Reporting Code must be 351, 352, 353, "
                            + "354, 355, 356, 357 or 359; and if Full Program Completer = A, Cost Reporting Code "
                            + "must be 364; and if Full Program Completer = P, Cost Reporting Code must be 371 or 372. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "48");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^W1702(06|11)$"))
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CIS, "^08|35|41|55|58$"))),
                    "If the CTE/Adult General Education Program Code = W170206 or W170211, the "
                            + "District Number, Current Instruction/Service must be one of the "
                            + "following: 08, 35, 41, 55, or 58."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "49");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^W1702(05|13)$"))
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CIS, "^35|41|50|55|58|62$"))),
                    "If the CTE/Adult General Education Program Code = W170205 or W170213, the "
                            + "District Number, Current Instruction/Service must be one of the following: "
                            + "35, 41, 50, 55, 58, or 62."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "50");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^W170210$"))
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CIS, "^42$"))),
                    "If the CTE/Adult General Education Program Code = W170210, the "
                            + "District Number, Current Instruction/Service must be 42."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "52");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^37[12]$")))
                            .testThen(Restriction.pattern(FIELD_CTE_AGE_COMPLETION_POINT_CODE, "^[A-EZ ]*$"))),
                    "If the Cost Reporting Code is 371 or 372, then the CTE/Adult General Education "
                            + "Completion Point Codes must be in the range of A-E, Z or blank. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "56");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^E92010R$")))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^1[02]|3[01]$")),
                            ValidationRule
                                    .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                            Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^(?!E92010R).{6}R$")))
                                    .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$"))),
                    "If CTE Program Code is E92010R, then Grade Level must be 10-12, 30, or 31, else if the "
                            + "seventh byte of CTE Program Code is an R, Grade Level must equal 30 or 31. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "57");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^[A-HZ] *$"))),
                    "Financial Assistance must be A - H, or Z, and left justified with trailing blanks. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "58");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^.*Z.*$")))
                            .testThen(Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^Z *$"))),
                    "If Financial Assistance equals Z, all other positions must be blank. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "59");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^.*[A-H].*$")))
                            .testThen(Restriction.pattern(FIELD_FINANCIAL_ASSISTANCE_RECEIVED, "^(?!.*Z.*).*$"))),
                    "If Financial Assistance equals A - H, the value Z cannot be reported on the same record. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "67");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^[VAPNZ]$"))),
                    "Full Program Completer must be V, A, P, N, or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "68");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^[VA]$")))
                            .testThen(Restriction.pattern(FIELD_CTE_BASIC_SKILLS_EXAMINATION, "^[^NZ]$"))),
                    "If Full Program Completer is V or A, then CTE Basic Skills Exam must not equal N or Z. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "69");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^341$")))
                            .testThen(Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^Z$"))),
                    "If Cost Reporting Code is 341, Full Program Completer must equal Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "70");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS)))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_TCRS,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_SCHOOL_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                    getWdisFieldsPair(FIELD_SECTION_NUMBER)))),
                    "If Prior Year = 0000, each WDIS Career and Technical Education Student Course record must have a "
                            + "matching WDIS Teacher Course record based on the key fields of "
                            + "District Number, Current Instruction/Service; "
                            + "School Number, Current Instruction/Service; "
                            + "Survey Period Code; "
                            + "WDIS Reporting Year; "
                            + "Course Number; "
                            + "and Section Number. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "71");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "Each WDIS Career and Technical Education Student Course Schedule must have a matching "
                            + "WDIS Student Demographic Record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code; "
                            + "and WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "72");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SETS,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "Each WDIS Career and Technical Education Student Course must have a matching "
                            + "WDIS Student End of Term Status record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code and "
                            + "WDIS Reporting Year. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "82");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^372$")))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_CTE,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                    new KeyValuePair(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^371$"),
                                            null))),
                            ValidationRule
                                    .testIf(Restriction.and(m_isSurveyPeriodNotGX,
                                            Restriction.pattern(FIELD_COST_REPORTING_CODE, "^371$")))
                                    .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_CTE,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                            getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                            new KeyValuePair(
                                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^372$"),
                                                    null)))),
                    "If the Cost Reporting Code is 372, then there must be a matching "
                            + "WDIS Career and Technical Education Student Course Schedule record based on the elements "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code; "
                            + "WDIS Reporting Year; "
                            + "and Course Number, with a Cost Reporting Code of 371, and vice versa. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "83");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(
                                    Restriction.pattern(FIELD_COST_REPORTING_CODE, "^341|35[1-7]|359|364|371$"),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^F$")))
                            .testThen(new RestrictionVerticalSumCompare(FL_EXPORT.CTE, Operator.LESS_THAN_OR_EQUALS,
                                    FIELD_STUDENT_NUMBER_IDENTIFIER_ALIAS_FLORIDA,
                                    FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS, Double.valueOf(360))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_COST_REPORTING_CODE,
                                                    "^341|35[1-7]|359|364|371$"),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^[WS]$")))
                                    .testThen(new RestrictionVerticalSumCompare(FL_EXPORT.CTE,
                                            Operator.LESS_THAN_OR_EQUALS,
                                            FIELD_STUDENT_NUMBER_IDENTIFIER_ALIAS_FLORIDA,
                                            FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS, Double.valueOf(720)))),
                    "If Cost Reporting Code is 341, 351-357, 359, 364, or 371 and if Survey Period Code is F, "
                            + "then WDIS Student Instructional Hours must be 360 or less when summed for the "
                            + "survey, by student. If Reporting Code is 341, 351-357, 359, 364, or 371, and "
                            + "if Survey Period Code is W or S, then WDIS Student Instructional Hours must be "
                            + "720 or less when summed for the survey, by student. This edit encompasses all "
                            + "Career and Technical Education Student Course Schedule records that are sent "
                            + "for the student during the survey."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "84");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                    getWdisFieldsPair(FIELD_GRADE_LEVEL)))),
                    "Grade Level on WDIS Career and Technical Education Student Course Schedule record does "
                            + "not match Grade Level on WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code; "
                            + "and WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "85");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.and(Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                    Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^F$"),
                                    Restriction.greaterThan(FIELD_COST_REPORTING_CODE, Double.valueOf(350)),
                                    Restriction.lessThan(FIELD_COST_REPORTING_CODE, Double.valueOf(370))))
                            .testThen(Restriction.lessThanOrEquals(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                    Double.valueOf(450))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$"),
                                            Restriction.greaterThan(FIELD_COST_REPORTING_CODE, Double.valueOf(350)),
                                            Restriction.lessThan(FIELD_COST_REPORTING_CODE, Double.valueOf(370))))
                                    .testThen(Restriction.lessThanOrEquals(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                            Double.valueOf(600))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^S$"),
                                            Restriction.greaterThan(FIELD_COST_REPORTING_CODE, Double.valueOf(350)),
                                            Restriction.lessThan(FIELD_COST_REPORTING_CODE, Double.valueOf(370))))
                                    .testThen(Restriction.lessThanOrEquals(FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS,
                                            Double.valueOf(700)))),
                    "If Prior Year = 0000 and Survey Period Code = F and the Cost Reporting Code greater than "
                            + "350 and less than 370, the WDIS Student Instructional Hours cannot be greater "
                            + "than 450. "
                            + "If Prior Year = 0000 and Survey Period Code = W and the "
                            + "Cost Reporting Code greater than 350 and less than 370, the "
                            + "WDIS Student Instructional Hours cannot be greater than 600. "
                            + "If Prior Year = 0000 and Survey Period Code = S and the "
                            + "Cost Reporting Code greater than 350 and less than 370, "
                            + "the WDIS Student Instructional Hours cannot be greater than 700."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_CTE, "88");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$"),
                                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^V$")))
                                    .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^35[1-79]$")),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$"),
                                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^A$")))
                                    .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^364$")),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^W$"),
                                            Restriction.pattern(FIELD_FULL_PROGRAM_COMPLETER, "^P$")))
                                    .testThen(Restriction.pattern(FIELD_COST_REPORTING_CODE, "^37[12]$"))),
                    "If Prior Year = 0000 and Survey = W then if Full Program Completer = V, "
                            + "Cost Reporting Code must be 351, 352, 353, 354, 355, 356, 357 or 359; "
                            + "and if Full Program Completer = A, Cost Reporting Code must be 364; "
                            + "and if Full Program Completer = P, Cost Reporting Code must be 371 or 372."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "7");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_STUDENT_NUMBER_IDENTIFIER_ALIAS_FLORIDA,
                    "^((?!7[7-9])[0-7][\\d]{9})|([\\d]{9}[X])$",
                    "The first nine positions of Student Number Identifier - Alias, Florida must be numeric. "
                            + "The tenth position of Student Number Identifier - Alias, Florida must either "
                            + "be an \"\"X\"\" or numeric. If the tenth position of "
                            + "Student Number Identifier -Alias, Florida is numeric, the first two digits "
                            + "must be a valid district number in the range 01-76."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "9");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.uniqueValue(FIELD_DISTRICT_NUMBER_CIS,
                                    FIELD_STUDENT_NUM_ID_FLORIDA, FIELD_SURVEY_PERIOD_CODE,
                                    FIELD_WDIS_REPORTING_YEAR))),
                    "Each WDIS Student Demographic record must be unique based on "
                            + "District Number, Current Instruction/Services; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code and "
                            + "WDIS Reporting Year"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "10");
            ruleAssociation
                    .setProcessor(
                            new ValidateRegularExpression(FIELD_STUDENT_NAME_LEGAL,
                                    "^[\"',/\\.()\\-\\p{L}][\"',/\\.()\\-\\p{L} ]{16}.{3}.{12}.{10}$",
                                    "The Student Name, Legal: Last Name must not be blank (Z-fill is NOT allowed). "
                                            + "Allowable characters include double or single quotation marks, "
                                            + "commas, slashes, periods, parentheses, hyphens, and accent marks. "
                                            + "First character cannot be blank."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "11");
            ruleAssociation
                    .setProcessor(
                            new ValidateRegularExpression(FIELD_STUDENT_NAME_LEGAL,
                                    "^.{17}.{3}[\"',/\\.\\-\\p{L}][\"',/\\.\\-\\p{L} ]{11}[\"',/\\.()\\-\\p{L} ]{10}$",
                                    "The Student Name, Legal: First Name must not be blank (Z-fill is NOT allowed). "
                                            + "Allowable characters include double or single quotation marks, commas, "
                                            + "slashes, periods, hyphens and accent marks. "
                                            + "Student middle name/appendage may be blank but must not include non-displayable characters. "
                                            + "Allowable characters include double or single quotation marks, commas, "
                                            + "slashes, periods, parentheses, hyphens, and accent marks."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "12");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.byDateFormat(FIELD_BIRTH_DATE))),
                    "Birth Date must be numeric, a valid date, and in the format MMDDYYYY."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "13");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_GENDER, "^[MF]$", "Gender must be M or F."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "15");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_ENGLISH_LANGUAGE_LEARNERS_ADULT, "^[YN]$",
                            "English Language Learners code must be Y or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "16");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_RESIDENT_STATUS_STATE_COUNTY, "^[02-7AB]$",
                            "Resident Status, State/County code must be 0, A, B, 2, 3, 4, 5, 6, or 7."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "18");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_EXCEPTIONALITY_PRIMARY, "^[CF-MOPS-WZ]$",
                            "Exceptionality, Primary must be either C, F, G, H, I, J, K, L, M, O, P, S, T, U, V, W or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "19");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_CITIZENSHIP_STATUS, "^[ACPX]$",
                            "Citizenship must be A, C, P, or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "20");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_FIRST_TIME_STUDENT_INDICATOR, "^[YDZN]$",
                            "First-Time Student code must be Y, D, Z, or N."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "21");
            ruleAssociation
                    .setProcessor(new ValidateRegularExpression(FIELD_EMPLOYMENT_STATUS, "^[EUNSZ]$",
                            "Employment Status must be E, U, N, S, or Z"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "22");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.studentAgeInRange(FIELD_STUDENT_NUM_ID_FLORIDA,
                                    Integer.valueOf(12), null, true, true,
                                    new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                    "Birth Date must indicate an age of twelve years or older by the end of the term."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "24");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_DISPLACED_HOMEMAKER, "^[ABCDZ]$"))),
                    "Displaced Homemaker code must be A, B, C, D, or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "25");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_SINGLE_PARENT_AND_SINGLE_PREGNANT_WOMAN,
                                    "^[SWBZ]$"))),
                    "Single Parent and Single Pregnant Woman code must be S, W, B or Z . "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "27");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_OTHER_INSTITUTIONALIZED_ADULT,
                                    "^[AZ]$"))),
                    "Other Institutionalized Adult must be A or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "30");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_ETHNICITY, "^[YN]$")),
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.and(
                                    Restriction.pattern(FIELD_RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE, "^[YN]$"),
                                    Restriction.pattern(FIELD_RACE_ASIAN, "^[YN]$"),
                                    Restriction.pattern(FIELD_RACE_BLACK_OR_AFRICAN_AMERICAN, "^[YN]$"),
                                    Restriction.pattern(FIELD_RACE_NATIVE_HAWAIIAN_OR_OTH_PAC_ISLANDER, "^[YN]$"),
                                    Restriction.pattern(FIELD_RACE_WHITE, "^[YN]$"))),
                    ValidationRule
                            .testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.or(
                                    Restriction.pattern(FIELD_RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE, "^Y$"),
                                    Restriction.pattern(FIELD_RACE_ASIAN, "^Y$"),
                                    Restriction.pattern(FIELD_RACE_BLACK_OR_AFRICAN_AMERICAN, "^Y$"),
                                    Restriction.pattern(FIELD_RACE_NATIVE_HAWAIIAN_OR_OTH_PAC_ISLANDER, "^Y$"),
                                    Restriction.pattern(FIELD_RACE_WHITE, "^Y$")))),
                    "The value of the Ethnicity data element must be ‘Y’ or ‘N’. The value of the "
                            + "following data elements must be ‘Y’ or ‘N’, with the value of at "
                            + "least one of these data elements = ‘Y’: "
                            + "Ethnicity; "
                            + "Race: American Indian or Alaska Native; "
                            + "Race: Asian; "
                            + "Race: Black or African American; "
                            + "Race: Native Hawaiian or other Pacific Islander; "
                            + "Race: White."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "31");
            ruleAssociation.setProcessor(
                    new ValidateRegularExpression(FIELD_RESIDENCE_COUNTY, "^((?!6[89])[0-6][0-9])|99|00$",
                            "The Residence County code must be 01-67, 99, or 00."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "33");
            ruleAssociation.setProcessor(
                    new ValidateRegularExpression(FIELD_ORIGIN_OF_SCHOOLING_ADULT, "^[UNXZ]$",
                            "Origin of Schooling, Adult must be U, N, X., or Z"));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "34");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_AGE_EX_OFFENDER, "^[ENZ]$"))),
                    "Adult General Education, Ex-Offender must be E, N, or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "35");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_AGE_EMPLOYMENT_BARRIERS, "^[CNZ]$"))),
                    "Adult General Education, Employment Barriers must be C, N, or Z. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "36");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_HIGHEST_SCHOOL_GRADE_COMPLETED,
                                    "^((?!13|14|2[3-9])[0-2][0-9])|D1|G1|ZZ$"))),
                    "Highest School Grade Completed must be 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, "
                            + "11, 12, D1, G1, 15, 16, 17, 18, 19, 20, 21, 22, or ZZ. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "37");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_AGE_HOMELESS_INDIVIDUALS_OR_RUNAWAY_YOUTH,
                                    "^[ABCDNZ]$"))),
                    "Adult General Education Homeless Individuals or Runaway Youth must be "
                            + "A, B, C, D, N, or Z. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "38");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_AGE_MIGRANT_AND_SEASONAL_FARMWORKER,
                                    "^[ABNZ]$"))),
                    "Adult General Education, Migrant and Seasonal Farmworker must be A, B, N, or Z. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "40");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(m_isSurveyPeriodNotGX)
                            .testThen(Restriction.pattern(FIELD_MILITARY_STATUS, "^[ADENRVWYZX]$"))),
                    "Military Status must be A, D, E, N, R, V, W, Y, Z, or X. Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "41");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_ADULT_HIGH_SCHOOL_DIPLOMA_OPTION,
                    "^[ABZ]$", "Adult High School Diploma Option must be A, B, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "70");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^[FWSGX]$"))
                            .testThen(Restriction.or(
                                    Restriction.validateMatchInExport(PROCEDURE_ID_AGE,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)),
                                    Restriction.validateMatchInExport(PROCEDURE_ID_CTE,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR))))),
                    "For Survey Periods F, W, S, G, or X, each WDIS Student Demographic Record "
                            + "must have at least one matching WDIS Adult General Education Student Course "
                            + "record or WDIS Career and Technical Education Student Course record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code and "
                            + "WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "71");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_SURVEY_PERIOD_CODE, "^[FWS]$"))
                            .testThen(
                                    Restriction.validateMatchInExport(PROCEDURE_ID_SETS,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "For Survey Periods F, W, or S each WDIS Student Demographic Record "
                            + "must have a matching WDIS Student End of Term Status record based on "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code and "
                            + "WDIS Reporting Year. "
                            + "Not required for Survey G or X."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "80");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))
                            .testThen(Restriction.pattern(FIELD_RESIDENT_STATUS_STATE_COUNTY, "^[023AB]$")),
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$"))
                            .testThen(
                                    Restriction.pattern(FIELD_RESIDENT_STATUS_STATE_COUNTY, "^[4-7]$"))),
                    "If Grade Level is 09-12, then Resident Status, State/County must be 0, A, B, 2, or 3. "
                            + "If Grade Level is 30-31, then Resident Status, State/County must be 4, 5, 6, or 7."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SDI, "81");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.studentAgeInRange(FIELD_STUDENT_NUM_ID_FLORIDA, null,
                                    Integer.valueOf(90), false, true, new Date()))),
                    "Birth Date should not be reported as more than 90 years prior to the current date."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "7");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(
                                    Restriction.uniqueValue(FIELD_DISTRICT_NUMBER_CIS, FIELD_STUDENT_NUM_ID_FLORIDA,
                                            FIELD_SURVEY_PERIOD_CODE, FIELD_WDIS_REPORTING_YEAR,
                                            FIELD_PRIOR_YEAR, FIELD_GRADE_LEVEL)),
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.and(
                                    Restriction.validateNotMatchInExport(2, PROCEDURE_ID_SETS,
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA)),
                                    Restriction.validateNotMatchInExport(1, PROCEDURE_ID_SETS,
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            new KeyValuePair(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"),
                                                    null)),
                                    Restriction.validateMatchInExport(1, PROCEDURE_ID_SETS,
                                            getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                            new KeyValuePair(Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$"),
                                                    null))))),
                    "Each WDIS Student End of Term Status record must be unique based on the keys of "
                            + "District Number, Current Instruction/Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code; "
                            + "WDIS Reporting Year; "
                            + "Prior Year, and "
                            + "Grade Level. "
                            + "If more than one WDIS Student End of Term Status record is submitted for a student, "
                            + "only one record with Grade Level = 09-12 will be accepted and only one record "
                            + "with Grade Level = 30 or 31 will be accepted."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "8");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_DIPLOMA_TYPE,
                    "^W(0([67]|10|27|G[AD]|4[35]|5[24589]|D1|FT|FW|RW|X[LTW]))|ZZZ$",
                    "Diploma Type must be W06, W07, W10, W27, WGA, WGD, W43, W45, W52, W54, W55, W58, W59, "
                            + "WD1, WFT, WFW, WRW, WXL, WXT, WXW or ZZZ."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "9");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_CERTIFICATE_OF_COMPLETION_TYPE,
                    "^(W(08|8[AB]|09|44|5[36]))|ZZZ$",
                    "Certificate of Completion, Type must be W08, W8A, W8B, W09, W44, W53, W56 or ZZZ."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "21");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(
                            Restriction.pattern(FIELD_DIPLOMA_TYPE, "^W(0[67]|10|27|D1|G[AD]|F[TW]|RW|X[LTW])$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))),
                    "If Diploma Type is W06, W07, W10, W27, WD1, WGA, WGD, WFT, WFW, WRW, WXL, WXT, or "
                            + "WXW, Grade Level must be one of the grades 09-12."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "22");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(
                            Restriction.pattern(FIELD_CERTIFICATE_OF_COMPLETION_TYPE, "^W(0[89]|8[A-C])$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))),
                    "If Certificate of Completion, Type is W08, W8A, W8B, W8C, or W09, Grade Level must be one of the grades 09-12."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "23");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_DIPLOMA_TYPE, "^(?!ZZZ).*$"))
                            .testThen(Restriction.pattern(FIELD_CERTIFICATE_OF_COMPLETION_TYPE, "^ZZZ$")),
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CERTIFICATE_OF_COMPLETION_TYPE, "^ZZZ$"))
                            .testThen(Restriction.pattern(FIELD_DIPLOMA_TYPE, "^(?!ZZZ).*$"))),
                    "If Diploma Type is other than ZZZ, then Certificate of Completion, Type must be ZZZ and vice versa."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "26");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.pattern(FIELD_PRIOR_YEAR, "^(?!0000)\\d{4}$"))
                            .testThen(new RestrictionPriorYear())),
                    "Prior Year must be numeric. If it is greater than zero, it must be a valid WDIS Reporting Year and "
                            + "must be one year prior to the current valid WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "27");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_AGE_WITHDRAWAL_REASON, "^[A-GNZ]$",
                    "Adult General Education, Withdrawal Reason must be A, B, C, D, E, F, G, N, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "28");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_DIPLOMA_TYPE, "^(?!ZZZ).*$")))
                                    .testThen(Restriction.and(Restriction.byDateFormat(FIELD_AGE_DIPLOMA_DATE),
                                            Restriction.greaterThanOrEquals(FIELD_AGE_DIPLOMA_DATE,
                                                    new RuntimeParam(RuntimeParam.FISCAL_BEGIN_DATE)),
                                            Restriction.lessThanOrEquals(FIELD_AGE_DIPLOMA_DATE,
                                                    new RuntimeParam(RuntimeParam.FISCAL_END_DATE)))),
                            ValidationRule
                                    .testIf(Restriction.and(
                                            Restriction.pattern(FIELD_PRIOR_YEAR, PATTERN_ALL_ZEROS),
                                            Restriction.pattern(FIELD_DIPLOMA_TYPE, "^ZZZ$")))
                                    .testThen(Restriction.pattern(FIELD_AGE_DIPLOMA_DATE, PATTERN_ALL_ZEROS)),
                            ValidationRule
                                    .testIf(Restriction.pattern(FIELD_PRIOR_YEAR, "^(?!0000)\\d{4}$"))
                                    .testThen(Restriction.or(
                                            Restriction.byDateFormat(FIELD_AGE_DIPLOMA_DATE),
                                            Restriction.pattern(FIELD_AGE_DIPLOMA_DATE, PATTERN_ALL_ZEROS)))),
                    "If Prior Year is equal to zero and Diploma Type is not ZZZ, "
                            + "Adult General Education, Diploma Date must be a valid date in the format of MMDDYYYY "
                            + "and must be a date in the current reporting year. "
                            + "If Prior Year is equal to zero and Diploma Type equals ZZZ, "
                            + "Adult General Education, Diploma Date must be zero. "
                            + "If Prior Year is not equal to zero, Adult General Education, Diploma Date must "
                            + "be a valid date in the format of MMDDYYYY or it must be all zeroes."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "29");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.pattern(FIELD_DIPLOMA_TYPE, "^W(4[35]|5[24589])$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "3[01]"))),
                    "If Diploma Type is W43, W45, W52, W54, W55, W58 or W59, Grade Level must be one of the grades 30 or 31."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "30");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CERTIFICATE_OF_COMPLETION_TYPE, "^W(44|5[36])$"))
                            .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "30"))),
                    "If Certificate of Completion, Type is W44, W53, or W56 Grade Level must be 30."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "70");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.validateMatchInExport(PROCEDURE_ID_SDI,
                                    getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                    getWdisFieldsPair(FIELD_STUDENT_NUM_ID_FLORIDA),
                                    getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                    getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR)))),
                    "Each WDIS Student End of Term Status record must have a matching WDIS Student Demographic record based on "
                            + "District Number, Current Instruction/ Service; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code and "
                            + "WDIS Reporting Year."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SETS, "71");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_DIPLOMA_TYPE, "^W06$"))
                            .testThen(Restriction.pattern(FIELD_EXCEPTIONALITY_PRIMARY, "^[^CGHJKOPSVW].*$"))),
                    "If Diploma Type = W06, then Exceptionality, Primary may not be C, G, H, J, K, O, P, S, V or W."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "4");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_SURVEY_PERIOD_CODE, "^[FWS]$",
                    "Survey Period Code must be F, W, or S."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "9");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                            Restriction.uniqueValue(FIELD_DISTRICT_NUMBER_CIS, FIELD_STUDENT_NUM_ID_FLORIDA,
                                    FIELD_SURVEY_PERIOD_CODE, FIELD_WDIS_REPORTING_YEAR, FIELD_CTE_AGE_PROGRAM_CODE,
                                    FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                    FIELD_INDUSTRY_CERTIFICATION_OUTCOME))),
                    "Each Supplemental Information Format record must be unique based on "
                            + "District Number, Current Instruction/Services; "
                            + "Student Number Identifier, Florida; "
                            + "Survey Period Code; "
                            + "WDIS Reporting Year; "
                            + "CTE Program Code; "
                            + "Industry Certification Identifier, and "
                            + "Industry Certification Outcome."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "11");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_OUTCOME, "^P|F$")).testThen(
                                    Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                            "^(?!Z{8}| {8}).*$"))),
                    "If Industry Certification Outcome is equal to P or F, then "
                            + "Industry Certification Identifier must not be equal to ‘ZZZZZZZZ’ or blanks."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "12");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                    "^(?!Z{8}| {8}).*$"))
                            .testThen(Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_OUTCOME, "^P|F$"))),
                    "If Industry Certification Identifier is not equal to ‘ZZZZZZZZ’ or blanks then "
                            + "Industry Certification Outcome must be equal to P or F."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "13");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                    "^(?!Z{8}| {8}).*$"))
                            .testThen(Restriction.byReferenceCodes(REF_TABLE_WDIS_IND_CERT_CODES,
                                    FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER))),
                    "If Industry Certification Identifier is not equal to ‘ZZZZZZZZ’ or blanks, "
                            + "then it must be a valid Industry Certification Identifier."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "14");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(Restriction.pattern(FIELD_ADDITIONAL_HOURS_CREDITED, PATTERN_ALL_ZEROS))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                                    "^(?!Z{8}| {8}).*$"),
                                            Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_OUTCOME,
                                                    "^(?!Z| ).*$"))),
                            ValidationRule
                                    .testIf(Restriction.pattern(FIELD_ADDITIONAL_HOURS_CREDITED, "^(?!0{6}).*$"))
                                    .testThen(Restriction.and(
                                            Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
                                                    "^Z{8}| {8}$"),
                                            Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_OUTCOME,
                                                    "^Z| $")))),
                    "If Additional Hours Credited is equal to zero, then Industry Certification Identifier "
                            + "and Industry Certification Outcome must not be equal to ‘ZZZZZZZZ’ or blanks. "
                            + "If Additional Hours Credited is not equal to zero, then Industry Certification Identifier "
                            + "and Industry Certification Outcome must be equal to ‘ZZZZZZZZ’ or blanks"));
            ruleAssociations.add(ruleAssociation);

            // TODO multiple and potentially repeating identifiers should be stored for
            // different program codes
            // ruleAssociation = new RuleAssociation();
            // ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_SUP, "81");
            // ruleAssociation.setProcessor(new FLValidationRuleSet(
            // new RuleSet(
            // ValidationRule
            // .testIf(Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER,
            // "^(?!Z{8}| {8}).*$"))
            // .testThen(Restriction.and(
            // ,
            // Restriction.pattern(FIELD_INDUSTRY_CERTIFICATION_OUTCOME,
            // "^(?!Z| ).*$")))),
            // "If Industry Certification Identifier is not equal to ‘ZZZZZZZZ’ or blanks, "
            // + "then it must be a valid Industry Certification Identifier for the "
            // + "Career and Technical Education Program Code indicated, else it cannot be used
            // for "
            // + "federal Perkins reporting. "
            // + "(See Appendix ZZ for a crosswalk of valid Industry Certification Identifiers
            // to program codes)"));
            // m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "5");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.pattern(FIELD_COURSE_NUMBER,
                                    "^((?!00|0100[0-2]|249999[1-9]|2[5-9])"
                                            + "(?!7[0-8]|790|791000|798099[1-9]|798[1-9]|799)"
                                            + "(?!800[0-1]|900349[1-9]|9003[5-9]|900[4-9]|90[1-9]|9[1-9])"
                                            + "[0-27-9]\\d{6})|9900010$"))),
                    "Course Number must not contain blanks and must be a valid postsecondary course number "
                            + "on the CTE/Adult General Education Program Edit file (F61730) or a valid number on the "
                            + "Course Code Directory file in the range 0100300-2499990, 7910010-7980990, 8002000-9003490, "
                            + "and 9900010."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "7");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_FLORIDA_EDUCATION_IDENTIFIER,
                    "^FL\\d{12}$",
                    "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first "
                            + "2 positions followed by twelve numeric digits. No blanks or spaces are allowable."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "9");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_FACILITY_TYPE,
                    "^(?!2[1-9])[0-2][0-9]$",
                    "Facility Type code must be in the range 00 to 20."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "10");
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.and(
                                    Restriction.greaterThan(FIELD_WDIS_CLASS_LENGTH, Double.valueOf(0)),
                                    Restriction.lessThan(FIELD_WDIS_CLASS_LENGTH, Double.valueOf(901))))),
                            "WDIS Class Length must be greater than zero and less than 901 hours."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "12");
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.uniqueValue(
                                    FIELD_DISTRICT_NUMBER_CIS,
                                    FIELD_SCHOOL_NUMBER_CIS,
                                    FIELD_SURVEY_PERIOD_CODE,
                                    FIELD_WDIS_REPORTING_YEAR,
                                    FIELD_COURSE_NUMBER,
                                    FIELD_SECTION_NUMBER,
                                    FIELD_SOCIAL_SECURITY_NUMBER))),
                            "Each record must be unique based on the following key: "
                                    + "District Number, Current Instruction/Service; "
                                    + "School Number, Current Instruction/ Service; "
                                    + "Survey Period Code; "
                                    + "WDIS Reporting Year; "
                                    + "Course Number; "
                                    + "Section Number; "
                                    + "and Social Security Number."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "13");
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(
                            new RuleSet(
                                    ValidationRule
                                            .testIf(Restriction.pattern(FIELD_SOCIAL_SECURITY_NUMBER, "^CS.*$"))
                                            .testThen(Restriction.pattern(FIELD_SOCIAL_SECURITY_NUMBER,
                                                    "^CS\\d{7} $")),
                                    ValidationRule
                                            .testIf(Restriction.pattern(FIELD_SOCIAL_SECURITY_NUMBER, "^(?!CS).*$"))
                                            .testThen(Restriction.and(
                                                    Restriction.pattern(FIELD_SOCIAL_SECURITY_NUMBER,
                                                            "^\\d{9} |\\d{10}$"),
                                                    Restriction.greaterThan(FIELD_SOCIAL_SECURITY_NUMBER,
                                                            Double.valueOf(0))))),
                            "Social Security Number must be numeric and greater than zero, unless it is a "
                                    + "Staff Number Identifier and the first two positions are CS and the "
                                    + "last seven positions are numeric. Nine character SSNs should be left "
                                    + "justified with a trailing blank."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "14");
            ruleAssociation.setProcessor(new ValidateRegularExpression(
                    FIELD_DISTANCE_LEARNING_DELIVERY_INDICATOR,
                    "^[ABCXZ]$",
                    "The Distance Learning Delivery Indicator code must be A, B, C, X, or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "19");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_COURSE_NUMBER, "^EMS02(19|2[01])$"))
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CIS,
                                    "^35|41|5[58]$"))),
                    "If Course Number is equal to EMS0219, EMS0220, or EMS0221 District Number, Current Instruction/Service "
                            + "must be one of the following: 35, 41, 55, or 58."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "20");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_HOMELESS_ADULT_PROGRAM_INDICATOR,
                    "^[FZ]$", "Homeless Adult Program Indicator must be F or Z."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "21");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_STAFF_NUMBER_IDENTIFIER_LOCAL,
                    "^(?! )[1-9a-zA-Z ]*$",
                    "The Staff Number Identifier, Local may be any combination of letters, numbers and blanks. "
                            + "(All blanks are allowable.) It must be left-justified with trailing blanks."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "22");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_COURSE_NUMBER, "^RTE007[0-6]$"))
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CIS,
                                    "^42$"))),
                    "If Course Number is equal to RTE0070, RTE0071, RTE0072, RTE0073, RTE0074, RTE0075, or RTE0076 "
                            + "District Number, Current Instruction/Service must be 42."));
            ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(EXPORT_ID_SUFFIX_TCRS, "70");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.or(
                                    Restriction.validateMatchInExport(PROCEDURE_ID_CTE,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_SCHOOL_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                            getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                            getWdisFieldsPair(FIELD_SECTION_NUMBER)),
                                    Restriction.validateMatchInExport(PROCEDURE_ID_AGE,
                                            getWdisFieldsPair(FIELD_DISTRICT_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_SCHOOL_NUMBER_CIS),
                                            getWdisFieldsPair(FIELD_SURVEY_PERIOD_CODE),
                                            getWdisFieldsPair(FIELD_WDIS_REPORTING_YEAR),
                                            getWdisFieldsPair(FIELD_COURSE_NUMBER),
                                            getWdisFieldsPair(FIELD_SECTION_NUMBER))))),
                    "Each WDIS Teacher Course record must have either a matching WDIS Career and Technical Education Student Course "
                            + "record or a matching WDIS Adult General Education Student Course record based on "
                            + "District Number, Current Instruction/Service; "
                            + "School Number, Current Instruction/Service; "
                            + "Survey Period Code; "
                            + "WDIS Reporting Year; "
                            + "Course Number; "
                            + "and Section Number."));
            ruleAssociations.add(ruleAssociation);
            return ruleAssociations;
        }
    }


    /**
     * The Class RuleAssociation.
     */
    private static class RuleAssociation {
        private HashMap<String, String> m_associations = null;
        private FLValidationProcessor m_processor = null;


        /**
         * Adds the association.
         *
         * @param exportIdSuffix String
         * @param ruleNumber String
         */
        public void addAssociation(String exportIdSuffix, String ruleNumber) {
            if (m_associations == null) {
                m_associations = new HashMap<>();
            }
            m_associations.put(exportIdSuffix, ruleNumber);
        }


        /**
         * Gets the rule.
         *
         * @param exportIdSuffix String
         * @return FL validation rule
         */
        public FLValidationRule getRule(String exportIdSuffix) {
            FLValidationRule rule = null;
            if (exportIdSuffix != null) {
                for (Entry<String, String> association : m_associations.entrySet()) {
                    if (exportIdSuffix.equals(association.getKey())) {
                        rule = new FLValidationRule(exportIdSuffix, association.getValue(), m_processor);
                        break;
                    }
                }
            }
            return rule;
        }


        /**
         * Sets the processor.
         *
         * @param processor void
         */
        public void setProcessor(FLValidationProcessor processor) {
            m_processor = processor;
        }
    }


    /**
     * The Class ValidateWdisReportingYear.
     */
    public static class ValidateWdisReportingYear implements FLValidationProcessor {

        private static final String FIELD_NAME = "WDIS Reporting Year";
        private static final String FIELD_MSG =
                FIELD_NAME + " must be correct for the submission specified by the district.";


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            List<FLValidationError> errors = new LinkedList();
            String value = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME);
            if (value != null) {
                if (!value.equals(helper.getFiscalYear())) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME,
                            value, FIELD_MSG));
                }
            } else {
                errors.add(new FLValidationError(export, row, "Field not found", FIELD_NAME, "", FIELD_MSG));
            }
            return errors;
        }
    }

    public static final String FIELD_ADDITIONAL_HOURS_CREDITED = "Add Hrs Credited";
    public static final String FIELD_ADULT_ED_FUNC_LEVEL_INIT = "AE Func Level, Init";
    public static final String FIELD_ADULT_FEE_STATUS = "Adult Fee Status";
    public static final String FIELD_ADULT_FEE_STATUS_FIRST = FIELD_ADULT_FEE_STATUS + " 1st";
    public static final String FIELD_ADULT_FEE_STATUS_SECOND = FIELD_ADULT_FEE_STATUS + " 2nd";
    public static final String FIELD_ADULT_HIGH_SCHOOL_DIPLOMA_OPTION = "Diploma Option";
    public static final String FIELD_ADULT_RURAL_RESIDENT = "Rural Resident";
    public static final String FIELD_ADULT_TEST_FORM = "Test Form";
    public static final String FIELD_ADULT_TEST_LEVEL = "Test Level";
    public static final String FIELD_ADULT_TEST_NAME = "Test Name";
    public static final String FIELD_ADULT_TEST_SCORE = "Test Score";
    public static final String FIELD_ADULT_TEST_SUBJECT_CONTENT = "Test Subject Content";
    public static final String FIELD_AGE_DIPLOMA_DATE = "Diploma Date";
    public static final String FIELD_AGE_EMPLOYMENT_BARRIERS = "Empl Barriers";
    public static final String FIELD_AGE_EX_OFFENDER = "Ex-Offender";
    public static final String FIELD_AGE_HOMELESS_INDIVIDUALS_OR_RUNAWAY_YOUTH = "Hmls Indiv Rnw Yth";
    public static final String FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED = "Lit Compl Point Date";
    public static final String FIELD_AGE_MIGRANT_AND_SEASONAL_FARMWORKER = "Mgrt Ssnl Frmwrk";
    public static final String FIELD_AGE_WITHDRAWAL_REASON = "Wdrawal Reason";
    public static final String FIELD_BIRTH_DATE = "Birth Date";
    public static final String FIELD_CTE_DATE_OF_PROGRAM_COMPLETION = "Program Compl Date";
    public static final String FIELD_CAREER_PATHWAYS_STUDENT_PARTICIPANT = "Pathways Participant";
    public static final String FIELD_CERTIFICATE_OF_COMPLETION_TYPE = "Cert of Compl Type";
    public static final String FIELD_CITIZENSHIP_STATUS = "Citizenship Status";
    public static final String FIELD_COST_REPORTING_CODE = "Cost Reporting Code";
    public static final String FIELD_COURSE_NUMBER = "Course Number";
    public static final String FIELD_CTE_AGE_COMPLETION_POINT_CODE = "CTE-AGE Com Pnt Code";
    public static final String FIELD_CTE_AGE_DISABILITY_STUDENT = "CTE-AGE Disability";
    public static final String FIELD_CTE_AGE_PROGRAM_CODE = "CTE-AGE Program Code";
    public static final String FIELD_CTE_BASIC_SKILLS_EXAMINATION = "Basic Skills Exam";
    public static final String FIELD_CTE_DUAL_ENR_CRS_LOCATION = "Dual Enrollment Loc";
    public static final String FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION = "Date of Entry, P/C/S";
    public static final String FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION = "Date of Exit, P/C/S";
    public static final String FIELD_DIPLOMA_TYPE = "Diploma Type";
    public static final String FIELD_DISPLACED_HOMEMAKER = "Displaced Homemaker";
    public static final String FIELD_DISTANCE_LEARNING_DELIVERY_INDICATOR = "DLD Indicator";
    public static final String FIELD_DISTRICT_NUMBER_CIS = "District Number CIS";
    public static final String FIELD_DUAL_ENROLLMENT_INDICATOR = "Dual Enrollment Ind";
    public static final String FIELD_EMPLOYMENT_STATUS = "Employment Status";
    public static final String FIELD_ENGLISH_LANGUAGE_LEARNERS_ADULT = "ELL";
    public static final String FIELD_ENROLLMENT_NOT_STATE_FUNDED = "Enr Not State Funded";
    public static final String FIELD_ETHNICITY = "Ethnicity";
    public static final String FIELD_EXCEPTIONALITY_PRIMARY = "Exceptionality, Prim";
    public static final String FIELD_FACILITY_TYPE = "Facility Type";
    public static final String FIELD_FINANCIAL_ASSISTANCE_RECEIVED = "Fin Assist Received";
    public static final String FIELD_FIRST_TIME_STUDENT_INDICATOR = "First-Time Std Ind";
    public static final String FIELD_FLORIDA_EDUCATION_IDENTIFIER = "FL Education Id";
    public static final String FIELD_FULL_PROGRAM_COMPLETER = "Full Pgm Completer";
    public static final String FIELD_FULL_TIME_STUDENT_INDICATOR = "Full-time Std Ind";
    public static final String FIELD_GENDER = "Gender";
    public static final String FIELD_GRADE_LEVEL = "Grade Level";
    public static final String FIELD_HIGHEST_SCHOOL_GRADE_COMPLETED = "Highest School Grade";
    public static final String FIELD_HOMELESS_ADULT_PROGRAM_INDICATOR = "Homeless Pgm Ind";
    public static final String FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER = "Industry Cert ID";
    public static final String FIELD_INDUSTRY_CERTIFICATION_OUTCOME = "Industry Cert Out";
    public static final String FIELD_LEVEL_OF_SCHOOLING_ADULT = "Level of Schooling";
    public static final String FIELD_MILITARY_STATUS = "Military Status";
    public static final String FIELD_ORIGIN_OF_SCHOOLING_ADULT = "Origin of Schooling";
    public static final String FIELD_OTHER_INSTITUTIONALIZED_ADULT = "Other Inst-lized";
    public static final String FIELD_POSTSECONDARY_COURSE_STATUS = "Postsec Crs Status";
    public static final String FIELD_POSTSECONDARY_SCHOOL_OF_ENROLLMENT = "Postsec Skl of Enr";
    public static final String FIELD_POST_TEST_STATUS = "Post Test Status";
    public static final String FIELD_PRIOR_YEAR = "Prior Year";
    public static final String FIELD_RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE = "Race Native";
    public static final String FIELD_RACE_ASIAN = "Race Asian";
    public static final String FIELD_RACE_BLACK_OR_AFRICAN_AMERICAN = "Race Black";
    public static final String FIELD_RACE_NATIVE_HAWAIIAN_OR_OTH_PAC_ISLANDER = "Race Native Hawaiian";
    public static final String FIELD_RACE_WHITE = "Race White";
    public static final String FIELD_RESIDENCE_COUNTY = "Residence County";
    public static final String FIELD_RESIDENCY_FOR_TUITION_PURPOSES = "Resid Tuition Purpos";
    public static final String FIELD_RESIDENT_STATUS_STATE_COUNTY = "Resid Status St/Cnt";
    public static final String FIELD_SCHOOL_NUMBER_CIS = "School Number CIS";
    public static final String FIELD_SECTION_NUMBER = "Section Number";
    public static final String FIELD_SINGLE_PARENT_AND_SINGLE_PREGNANT_WOMAN = "Single Parent Preg W";
    public static final String FIELD_SOCIAL_SECURITY_NUMBER = "SSN";
    public static final String FIELD_STAFF_NUMBER_IDENTIFIER_LOCAL = "Staff Number ID";
    public static final String FIELD_STUDENT_NAME_LEGAL = "Student Name, Legal";
    public static final String FIELD_STUDENT_NUMBER_IDENTIFIER_ALIAS_FLORIDA = "Std Num ID Alias";
    public static final String FIELD_STUDENT_NUM_ID_FLORIDA = "Std Num ID FL";
    public static final String FIELD_SURVEY_PERIOD_CODE = "Survey Period";
    public static final String FIELD_TEST_DATE = "Test Date";
    public static final String FIELD_TOTAL_CLOCK_HOURS_EARNED_TOWARDS_AWARD = "Total Hrs Ernd Award";
    public static final String FIELD_TRANSACTION_CODE = "Transaction Code";
    public static final String FIELD_VETERAN_STUDENT_INDICATOR = "Veteran Std Ind";
    public static final String FIELD_WDIS_CLASS_LENGTH = "Class Length";
    public static final String FIELD_WDIS_REPORTING_YEAR = "WDIS Reporting Year";
    public static final String FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS = "WDIS Std Instr Hrs";

    public static final String PROCEDURE_ID_AGE = "EXPDATA-FL-W-AGE";
    public static final String PROCEDURE_ID_ATR = "EXPDATA-FL-W-ATR";
    public static final String PROCEDURE_ID_CTE = "EXPDATA-FL-W-CTE";
    public static final String PROCEDURE_ID_SDI = "EXPDATA-FL-W-SDI";
    public static final String PROCEDURE_ID_SETS = "EXPDATA-FL-W-SETS";
    public static final String PROCEDURE_ID_SUP = "EXPDATA-FL-W-SUP";
    public static final String PROCEDURE_ID_TCRS = "EXPDATA-FL-W-TCRS";



    /**
     * Instantiates a new FL wdis export configuration.
     *
     * @param currentContext DistrictSchoolYearContext
     * @param surveyCode String
     * @param broker X2Broker
     */
    public FLWdisExportConfiguration(DistrictSchoolYearContext currentContext, String surveyCode, X2Broker broker) {
        super(currentContext, surveyCode, broker);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getSurveyFromCode(java.lang.String)
     */
    @Override
    public FLSurvey getSurveyFromCode(String code) {
        FLSurvey match = null;
        for (FL_SURVEY survey : FL_SURVEY.values()) {
            if (code.contentEquals(survey.getCode())) {
                match = survey;
                break;
            }
        }
        return match;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getExports()
     */
    @Override
    protected FLExport[] getExports() {
        return FL_EXPORT.values();
    }


    /**
     * Gets the completion point codes rules.
     *
     * @param testIf Restriction[]
     * @return Validation rule[]
     */
    private static ValidationRule[] getCompletionPointCodesRules(Restriction... testIf) {
        if (testIf == null) {
            testIf = new Restriction[0];
        }
        return new ValidationRule[] {

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^99000(10|99)$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED,
                                        "^(L[A-H]|M[A-H]|S[A-H]|K[AB]|H[AB]|W[AB]|JA|GA|C[AB]|E[A-HJ-NPR-U])* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900130$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[VWXY]* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900300$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[ABC]* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900100$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[A] *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^S990001$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^D *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900000$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[ABCDEFGHJKMN]* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900040$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[ABCDEF]* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900050$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[BC]* *$")),

                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900051$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[A] *$")),
                ValidationRule
                        .testIf(ArrayUtils.addAll(testIf, new Restriction[] {
                                Restriction.pattern(FIELD_CTE_AGE_PROGRAM_CODE, "^9900090$")
                        }))
                        .testThen(Restriction
                                .pattern(FIELD_AGE_LITERACY_COMPLETION_POINT_CODE_DATE_EARNED, "^[A] *$"))};
    }


    /**
     * Gets the date.
     *
     * @param month int
     * @param day int
     * @param year int
     * @return Date
     */
    private static Date getDate(int month, int day, int year) {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MONTH, month);
        calendar.set(Calendar.DATE, day);
        calendar.set(Calendar.YEAR, year);

        return calendar.getTime();
    }


    /**
     * Gets the wdis fields pair.
     *
     * @param fieldName String
     * @return Key value pair
     */
    private static KeyValuePair getWdisFieldsPair(String fieldName) {
        return new KeyValuePair(fieldName, fieldName);
    }

}
