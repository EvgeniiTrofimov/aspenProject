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

import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionAnd;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateMatchInExport;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationResult;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;

/**
 * Prior school status/student attendance (ENR) export validation
 * Specification: http://www.fldoe.org/core/fileparse.php/15229/urlt/1617psssa.pdf
 */
public class FLPriorSchoolStatusValidation {

    private static class ValidateEnrMatchInEnrExport extends ValidateMatchInExport {

        private static final String DEFAULT_DATE_FORMAT_MASK = "MMddyyyy";
        private static final SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(DEFAULT_DATE_FORMAT_MASK);

        /**
         * Instantiates a new validate sdra match in except export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateEnrMatchInEnrExport(String procedure_id, KeyValuePair<String, String> keyFieldNamePair,
                KeyValuePair<Object, String>... relatedFieldNamePairs) {
            super(procedure_id, keyFieldNamePair, relatedFieldNamePairs);
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param currentExport FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateMatchInExport#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport currentExport,
                                                    ExportFormatRow row) {

            String entryDateValue = helper.getExportFormatRowFieldValue(row, currentExport, "Entry Date");
            try {
                PlainDate entryDate = new PlainDate(DEFAULT_DATE_FORMAT.parse(entryDateValue));
                for (KeyValuePair<Object, String> fieldNamePair : m_relatedFieldNamePairs) {
                    if (fieldNamePair.getKey() instanceof RestrictionAnd) {
                        RestrictionAnd restrictionAnd = (RestrictionAnd) fieldNamePair.getKey();
                        Restriction[] ands = restrictionAnd.getRestrictions();
                        if (ands.length > 1 && ands[1] instanceof RestrictionCompare) {
                            RestrictionCompare cmp = (RestrictionCompare) ands[1];
                            cmp.setComparableValue(entryDate);
                        }
                    }
                }
                return super.getValidationResult(helper, currentExport, row);
            } catch (Exception ex) {
                return ValidationResult.FieldNotValid("Entry Date", entryDateValue);
            }
        }
    }

    /**
     * Instantiates a new FL prior school status validation.
     */
    public FLPriorSchoolStatusValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("ENR", "1", new ValidateDistrictNumber("District Number, E")),

                new FLValidationRule("ENR", "2", new ValidateRegularExpression("School Number, E",
                        "^(?!(9001|3518)$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[8-9])$",
                        "The School Number, Current Enrollment must be alphanumeric and in the range 0001-9899, excluding 3518 and 9001, or it must be N998 or N999.")),

                new FLValidationRule("ENR", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number",
                                                "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be"
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be"
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is"
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an"
                                + "\"X\", the first three positions may not all be zeroes.")),

                new FLValidationRule("ENR", "4", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number Alias", "^\\d{9}(X|\\d)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number Alias", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number Alias",
                                                "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[89])\\d{8}$"))),
                        "The first nine positions of Student Number Identifier-Alias, Florida must be"
                                + "numeric. The tenth position of Student Number Identifier-Alias, Florida must"
                                + "either be an \"X\" or numeric. If the tenth position of Student Number Identifier-Alias,"
                                + "Florida is numeric, the first two digits must be a valid district number in the"
                                + "range 01-68, 71-75 or 78-79.")),

                new FLValidationRule("ENR", "5", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(Restriction.pattern("Student Name Legal", "^.*\\S.*$"),
                                        Restriction.pattern("Student Name Legal", "^(?!Z{3})"),
                                        Restriction.pattern("Student Name Legal", "^[\\p{L}\\s\\-'\"\\.,\\/]+$")))),
                        "The Student Name, Legal: Must not be blank (Z-fill is NOT allowed)."
                                + " Allowable characters include double or single quotation marks, commas,"
                                + " slashes, periods, parentheses, hyphens, accent marks, and spaces where appropriate. ")),

                new FLValidationRule("ENR", "6", new ValidateRegularExpression("Gender", "^M|F$",
                        "Gender code must be M or F.")),

                new FLValidationRule("ENR", "8", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.and(
                                        Restriction.byDateFormat("Birth Date"),
                                        Restriction.lessThanOrEquals("Birth Date",
                                                new RuntimeParam(RuntimeParam.PERIOD_END_DATE))))),
                        "Birth Date must be numeric, a valid date in the format MMDDYYYY, and"
                                + " must be less than or equal to the last day of the Survey Period.")),

                new FLValidationRule("ENR", "9", new ValidateRegularExpression("Grade Level", "^PK|KG|10|11|12|0[1-9]$",
                        "Grade Level code must be PK, KG, or 01-12.")),

                new FLValidationRule("ENR", "10", new ValidateRegularExpression("Survey Period", "^2|3|5$",
                        "Survey Period Code must be 2, 3 or 5 and must be correct for the submission specified by the district.")),

                new FLValidationRule("ENR", "11", new ValidateFiscalYear()),

                new FLValidationRule("ENR", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.or(
                                Restriction.equals("Survey Period", "2"),
                                Restriction.notEquals("Term", "Y"))).testThen(Restriction.pattern("Entry Code PK-12",
                                        "^E01|E02|E2A|E03|E3A|E04|E4A|E05|E09|R01|R02|R03$")),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^3|5$"),
                                Restriction.equals("Term", "Y")))
                                .testThen(Restriction.equals("Entry Code PK-12", "ZZZ"))),
                        "Entry (Re-entry) Code, PK-12 must be E01, E02, E2A, E03, E3A, E04, E4A,"
                                + " E05, E09, R01, R02, or R03 unless Survey Period Code is 3 or 5 and Term is Y, in"
                                + " which case the field should be Z-filled.")),

                new FLValidationRule("ENR", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                                Restriction.and(
                                        Restriction.byDateFormat("Entry Date"),
                                        Restriction.greaterThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^3|5$"),
                                Restriction.equals("Term", "Y")))
                                .testThen(Restriction.equals("Entry Date", "00000000")),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.equals("Survey Period", "5"),
                                Restriction.notEquals("Term", "Y"))).testThen(
                                        Restriction.and(
                                                Restriction.byDateFormat("Entry Date"),
                                                Restriction.greaterThanOrEquals("Entry Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                                Restriction.lessThanOrEquals("Entry Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "If Survey Period is 2 or 3, then Entry (Re-entry) Date must be numeric and"
                                + " must be a valid date that is greater than or equal to 07/01/**** and less than or"
                                + " equal to the last day of the survey week. If Survey Period Code is 3 or 5 and Term"
                                + " is Y then the field should be zero filled. If Survey Period Code is 5, then the Entry"
                                + " (Re-entry) Date must be numeric and must be a valid date in the range 7/01/**** to"
                                + " 8/31/****.")),

                new FLValidationRule("ENR", "14", new ValidateRegularExpression("Prior School County",
                        "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|99$",
                        "Prior School/Location: District/County must be numeric and in the range"
                                + " 01-68, 71-75 or 99.")),

                new FLValidationRule("ENR", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.or(
                                        Restriction.byAliasFldRefTable("all-enr-PriorSchoolState",
                                                "Prior School State"),
                                        Restriction.equals("Prior School State", "ZZ")))),
                        "Prior School/Location: State/Territory or Commonwealth must be a valid"
                                + " code as listed in Appendix H or Appendix Q of the DOE Information Database"
                                + " Requirements Volume I -- Automated Student Information System Manual or ZZ.")),

                new FLValidationRule("ENR", "16", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("Term", "Y")).testThen(
                                Restriction.byAliasFldRefTable("all-enr-PriorSchoolCountry",
                                        "Prior School Country")),
                        ValidationRule.testIf(Restriction.equals("Term", "Y")).testThen(
                                Restriction.equals("Prior School Country", "ZZ"))),
                        "Prior School/Location: Country must be a valid code as listed in Appendix"
                                + " G of the DOE Information Database Requirements Volume I -- Automated Student"
                                + " Information System Manual other than ZZ, unless the Term is Y, then Country"
                                + " should be Z-filled. ")),

                new FLValidationRule("ENR", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.or(
                                        Restriction.pattern("Withdrawal Code",
                                                "^DNE|W01|W02|W3A|W3B|W0[4-8]|W8A|W8B|W09|W1[0-3]|W15|W18|W2[1-7]|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$"),
                                        Restriction.and(Restriction.notEquals("Survey Period", "5"),
                                                Restriction.equals("Withdrawal Code", "ZZZ"))))),
                        "Withdrawal Code, PK-12 must be DNE, W01, W02, W3A, W3B, W04, W05, W06,"
                                + " W07, W08, W8A, W8B, W09, W10, W12, W13, W15, W18, W21, W22, W23, W24, W25,"
                                + " W26, W27, WFT, WFW, WRW, WGA, WGD, WXL, WXT, WXW, WD1 or WPO. It may"
                                + " also be ZZZ only for survey periods 2 and 3.")),

                new FLValidationRule("ENR", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.or(Restriction.equals("Survey Period", "5"),
                                Restriction.notEquals("Withdrawal Code", "ZZZ"))).testThen(
                                        Restriction.and(
                                                Restriction.byDateFormat("Withdrawal Date"),
                                                Restriction.greaterThanOrEquals("Withdrawal Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                                Restriction.lessThanOrEquals("Withdrawal Date",
                                                        new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                        ValidationRule.testIf(Restriction.and(Restriction.notEquals("Survey Period", "5"),
                                Restriction.equals("Withdrawal Code", "ZZZ"))).testThen(
                                        Restriction.equals("Withdrawal Date", "00000000"))),
                        "Withdrawal Date must be a valid date in the range of 07/01/**** to 08/31/****"
                                + " and must be less than or equal to the last day of the survey period, or must be"
                                + " zero-filled for survey periods 2 and 3 if the Withdrawal Code is ZZZ.")),

                new FLValidationRule("ENR", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                                Restriction.pattern("Survey Period", "^3|5$"))).testThen(
                                        Restriction.pattern("Days Present Annual",
                                                "^\\d+$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                                Restriction.equals("Days Present Annual",
                                        "000")),
                        ValidationRule.testIf(Restriction.and(Restriction.pattern("Survey Period", "^3|5$"),
                                Restriction.equals("Term", "Y"))).testThen(
                                        Restriction.equals("Days Present Annual", "000"))),
                        "Days Present, Annual must be zero-filled in Survey 2 and must be numeric"
                                + " for Surveys 3 and 5 unless Term is Y in which case it must be zero-filled. ")),

                new FLValidationRule("ENR", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                                Restriction.pattern("Survey Period", "^3|5$"))).testThen(
                                        Restriction.pattern("Days Absent Annual",
                                                "^\\d+$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2")).testThen(
                                Restriction.equals("Days Absent Annual",
                                        "000")),
                        ValidationRule.testIf(Restriction.and(Restriction.pattern("Survey Period", "^3|5$"),
                                Restriction.equals("Term", "Y"))).testThen(
                                        Restriction.equals("Days Absent Annual", "000"))),
                        "Days Absent, Annual must be zero-filled in Survey 2 and must be numeric"
                                + " for Surveys 3 and 5 unless Term is Y in which case it must be zero-filled. ")),

                new FLValidationRule("ENR", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                                Restriction.equals("Survey Period", "5"))).testThen(
                                        Restriction.pattern("Days Present Summer",
                                                "^\\d+$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                                Restriction.equals("Days Present Summer",
                                        "000")),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Term", "Y"))).testThen(
                                        Restriction.equals("Days Present Summer", "000"))),
                        "Days Present, Summer Terms must be zero-filled in surveys 2 and 3 and"
                                + " must be numeric for Survey 5 unless Term is Y in which case it must be zerofilled.")),

                new FLValidationRule("ENR", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.notEquals("Term", "Y"),
                                Restriction.equals("Survey Period", "5"))).testThen(
                                        Restriction.pattern("Days Absent Summer",
                                                "^\\d+$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                                Restriction.equals("Days Absent Summer",
                                        "000")),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Term", "Y"))).testThen(
                                        Restriction.equals("Days Absent Summer", "000"))),
                        "Days Absent, Summer Terms must be zero-filled in surveys 2 and 3 and"
                                + " must be numeric in survey 5 unless Term is Y in which case it must be zero-filled.")),

                // TODO cannot be resolved due to conflict between rules 12, 18 and 44 until client
                // data obtained
                // new FLValidationRule("ENR", "23", new FLValidationRuleSet(new RuleSet(
                // ValidationRule.testIf(Restriction.equals("Survey Period", "5")).testThen(
                // Restriction
                // .greaterThanOrEqualsFieldValue("Withdrawal Date", "Entry Date",
                // Date.class)),
                // ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(
                // Restriction.or(
                // Restriction
                // .greaterThanOrEqualsFieldValue("Withdrawal Date", "Entry Date",
                // Date.class),
                // Restriction.equals("Withdrawal Date", "00000000")))),
                // "For Survey 5, the Withdrawal Date must be greater than or equal to the"
                // + " Entry (Re-entry) Date. For Surveys 2 and 3, the Withdrawal Date must be
                // greater"
                // + " than or equal to the Entry (Re-entry) Date or it must be zero-filled. ")),

                new FLValidationRule("ENR", "24", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("District Number, E", "School Number, E",
                                        "Student Number", "Survey Period", "Fiscal Year", "Entry Date"))),
                        "Each Prior School Status/Student Attendance record must be unique based"
                                + " on the keys of District Number, Current Enrollment; School Number, Current"
                                + " Enrollment; Student Number Identifier, Florida; Survey Period Code; School Year,"
                                + " and Entry (Re-entry) Date.")),

                new FLValidationRule("ENR", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("School Number, E", "N999"))
                                .testThen(Restriction.byActiveSchool("School Number, E"))),
                        "School Number, Current Enrollment must exist on the Master School"
                                + " Identification File as a valid active school number for the District Number, Current"
                                + " Enrollment or it must be N999.")),

                new FLValidationRule("ENR", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E01"))
                                .testThen(Restriction.and(
                                        Restriction.equalsFieldValue("Prior School County", "District Number, E",
                                                String.class),
                                        Restriction.equals("Prior School State", "FL"),
                                        Restriction.equals("Prior School Country", "US")))),
                        "If the Entry Code is E01, then the Prior School/Location: District must be"
                                + " the same as the District Number, Current Enrollment, the Prior School/Location:"
                                + " State/Territory or Commonwealth must be FL, and the Prior School/Location:"
                                + " Country must be US.")),

                new FLValidationRule("ENR", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E02"))
                                .testThen(Restriction.and(
                                        Restriction.notEqualsFieldValue("Prior School County", "District Number, E",
                                                String.class),
                                        Restriction.notEquals("Prior School County", "99"))),
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E2A"))
                                .testThen(Restriction.equals("Prior School County", "99"))),
                        "If the Entry Code is:"
                                + " E02, then the Prior School/Location: District/County must not be 99 and"
                                + " must not be the same as the District Number, Current Enrollment."
                                + " E2A, then the Prior School/Location: District/County must be 99")),

                new FLValidationRule("ENR", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.equals("Prior School County", "99"),
                                Restriction.equals("Prior School State", "ZZ")))
                                .testThen(
                                        Restriction.notEquals("Prior School Country", "US")),
                        ValidationRule.testIf(Restriction.equals("Prior School Country", "US"))
                                .testThen(
                                        Restriction.or(
                                                Restriction.notEquals("Prior School County", "99"),
                                                Restriction.notEquals("Prior School State", "ZZ")))),
                        "If the Prior School/Location: District is 99 and the Prior School/Location:"
                                + " State/Territory or Commonwealth is ZZ, the Prior School/Location: Country may"
                                + " not be US and vice versa.")),

                new FLValidationRule("ENR", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Entry Code PK-12", "^R01|R02|R03$"))
                                .testThen(
                                        Restriction.equalsFieldValue("Prior School County", "District Number, E",
                                                String.class))),
                        "If the Re-Entry Code is R01, R02, or R03, the Prior School/Location: District"
                                + " must be the same as the District Number, Current Enrollment. ")),

                new FLValidationRule("ENR", "2A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E03"))
                                .testThen(Restriction.notEquals("Prior School County", "99")),
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E3A"))
                                .testThen(Restriction.equals("Prior School County", "99"))),
                        "If the Entry Code is:"
                                + " E03, then the Prior School/Location: District/County must not be 99."
                                + " E3A, then the Prior School/Location: District/County must be 99. ")),

                new FLValidationRule("ENR", "2B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E04"))
                                .testThen(Restriction.notEquals("Prior School County", "99")),
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E4A"))
                                .testThen(
                                        Restriction.equals("Prior School County", "99"))),
                        "If the Entry Code is:"
                                + " E04, then the Prior School/Location: District/County must not be 99."
                                + " E4A, then the Prior School/Location: District/County must be 99. ")),

                new FLValidationRule("ENR", "2C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E09"))
                                .testThen(Restriction.equals("Prior School State", "ZZ"))),
                        "If the Entry Code is E09, then the Prior School/Location: State/Territory or Commonwealth must be ZZ.")),

                new FLValidationRule("ENR", "2D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.byAliasFldRefTable("all-enr-PriorSchoolState",
                                "Prior School State")).testThen(
                                        Restriction.equals("Prior School Country", "US"))),
                        "If Prior School/Location: State/Territory or Commonwealth is any code"
                                + " listed in Appendix Q, then Prior School/Location: Country code must be US.")),

                new FLValidationRule("ENR", "2E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Educational Choice", "A")).testThen(
                                Restriction.pattern("District Number, Z", "^(0[1-9]|[1-5][0-9]|6[0-7])$")),
                        ValidationRule.testIf(Restriction.notEquals("Educational Choice", "A")).testThen(
                                Restriction.pattern("District Number, Z", "^00$"))),
                        "If Educational Choice code = A, then District Number, Zoned School must"
                                + "be 01-67. All other Educational Choice codes must be filled with zeroes.")),

                new FLValidationRule("ENR", "2F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Educational Choice", "A")).testThen(
                                Restriction.byActiveSchool("School Number, Z")),
                        ValidationRule.testIf(Restriction.notEquals("Educational Choice", "A")).testThen(
                                Restriction.pattern("School Number, Z", "^0000$"))),
                        "If Educational Choice code = A, then School Number, Zoned School must"
                                + "be an active school for the District Number, Zoned School on the Master School"
                                + "Identification file. All other Educational Choice codes must be filled with zeros.")),

                new FLValidationRule("ENR", "2G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.equals("Educational Choice", "A"),
                                Restriction.greaterThan("School Number, Z", Double.valueOf(0)))
                                .testThen(Restriction.greaterThan("District Number, Z", Double.valueOf(0))),
                        ValidationRule.testIf(Restriction.greaterThan("District Number, Z", Double.valueOf(0)))
                                .testThen(
                                        Restriction.greaterThan("School Number, Z", Double.valueOf(0)))),
                        "If Educational Choice code = A, and School Number, Zoned School is greater"
                                + "than 0000, then District Number, Zoned School must be greater than 00. If the District"
                                + "Number, Zoned School is greater than 00, then School Number, Zoned School must"
                                + "be greater than 0000.")),

                new FLValidationRule("ENR", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                .testThen(Restriction.equals("Term", "Z")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "3"))
                                .testThen(Restriction.pattern("Term", "^3|Y$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.pattern("Term", "^3|S|Y$"))),
                        "Term must be Z for survey 2, must be either 3 or Y for survey 3 and must be 3,"
                                + " S or Y for survey 5.")),

                new FLValidationRule("ENR", "31", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^2|3$"),
                                Restriction.equals("Withdrawal Code", "ZZZ")))
                                .testThen(
                                        Restriction.uniqueValue("Student Number", "Survey Period", "Withdrawal Code"))),
                        "No more than one record with ZZZ as Withdrawal Code, PK-12 may be"
                                + " submitted for survey periods 2 or 3. ")),

                new FLValidationRule("ENR", "32", new ValidateRegularExpression("Transaction Code", "^A|C|D$",
                        "The Transaction Code must be A, C or D. For the original transmission,"
                                + " only A is valid. For subsequent batch/update submissions, if A is specified then"
                                + " the record must not already exist on the database; if C or D is specified then the"
                                + " record must exist on the database.")),

                new FLValidationRule("ENR", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.equals("Entry Code PK-12", "E09"),
                                Restriction.notEquals("Term", "Y")))
                                .testThen(Restriction.notEquals("Prior School Country", "US")),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.notEquals("Prior School Country", "US"),
                                Restriction.notEquals("Term", "Y")))
                                .testThen(Restriction.equals("Entry Code PK-12", "E09"))),
                        "If the Prior School/Location: Country is not US, then the Entry Code must"
                                + " be E09 and vice versa, unless Term is Y.")),

                new FLValidationRule("ENR", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Entry Code PK-12", "^E0[1-5]|E2A|E3A|E4A|R0[1-3]$"))
                                .testThen(Restriction.equals("Prior School Country", "US"))),
                        "If Entry (Re-Entry) Code equals E01-E05, E2A, E3A, E4A, or R01-R03"
                                + " then Prior School/Location: Country must be US.")),

                new FLValidationRule("ENR", "35", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Withdrawal Code",
                                        "^W06|W07|W08|W8A|W8B|W09|W10|W27|WFT|WFW|WRW|WGA|WGD|WXL|WXT|WXW|WD1|WPO$"))
                                .testThen(Restriction.pattern("Grade Level", "^9|1[0-2]$"))),
                        "If Withdrawal Code, PK-12 is W06, W07, W08, W8A, W8B, W09, W10, W27,"
                                + " WFT, WFW, WRW, WGA, WGD, WXL, WXT, WXW, WD1 or WPO or then Grade"
                                + " Level must be 9-12. ")),

                new FLValidationRule("ENR", "36", new ValidateRegularExpression("Educational Choice", "^A|B|C|E|F|M|Z$",
                        "Educational Choice must be A, B, C, E, F, M or Z")),

                new FLValidationRule("ENR", "37", new ValidateRegularExpression("Disaster Affected", "^B|E|Q|Y|W|Z$",
                        "Disaster Affected Student code must be B, E, Q, Y, W or Z.")),

                new FLValidationRule("ENR", "38", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Prior School County", "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]$"))
                                .testThen(Restriction.equals("Prior School State", "FL"))),
                        "If the Prior School/Location: District/County is 01-68 or 71-75 then the Prior"
                                + " School/Location: State/Territory or Commonwealth must be FL.")),

                new FLValidationRule("ENR", "39",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "The Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),

                new FLValidationRule("ENR", "40", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),

                new FLValidationRule("ENR", "41", new ValidateRegularExpression("Offender Transfer", "^Y|N$",
                        "Student Offender Transfer must be Y or N.")),

                new FLValidationRule("ENR", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Prior School County", "99"))
                                .testThen(Restriction.notEquals("Prior School State", "FL"))),
                        "If the Prior School/Location: District/County is 99, then the Prior"
                                + " School/Location: State/Territory or Commonwealth must not be FL.")),

                new FLValidationRule("ENR", "43", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                .testThen(Restriction.equals("Days Absent Unexcuse", "000")),
                        ValidationRule.testIf(Restriction.notEquals("Survey Period", "2"))
                                .testThen(Restriction.or(
                                        Restriction.and(
                                                Restriction.equals("Term", "Y"),
                                                Restriction.equals("Days Absent Unexcuse", "000")),
                                        Restriction.pattern("Days Absent Unexcuse", "^\\d{3}$")))),
                        "Days Absent, Annual-Unexcused Not Related to Discipline must be zerofilled"
                                + " in survey 2 and must be numeric in surveys 3 and 5 unless Term is Y in"
                                + " which case it must be zero-filled.")),

                new FLValidationRule("ENR", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^3|5$"),
                                Restriction.equals("Withdrawal Code", "DNE")))
                                .testThen(Restriction.equals("Term", "Y"))),
                        "If Survey Period Code is 3 or 5 and Withdrawal Code is DNE, then Term must be Y.")),


                // TODO: implement this rule for ENR-45:
                // - School Number, Current Enrollment has a School Function Setting equal to
                // V and a Charter School Status not equal to Z on the Master School Identification
                // file
                new FLValidationRule("ENR", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("District Number, E", "^68|7[1-5]$"),
                                Restriction.pattern("School Number, E", "^7001|7004|7006|7023|3518|N998|N999$")))
                                .testThen(Restriction.equals("Educational Choice", "Z"))),
                        "The Educational Choice code must be Z for the following:"
                                + " - District Number, Current Enrollment is 68 or 71-75."
                                + " - School Number, Current Enrollment is 7001 (school district virtual"
                                + " instruction program - contracted provider) or 7004 (Florida Virtual School"
                                + " franchise) or 7006 (KG-12 virtual course offerings) or 7023 (school district"
                                + " virtual instruction program - district provider)."
                                + " - School Number, Current Enrollment is 3518 (McKay Scholarship)."
                                + " - School Number, Current Enrollment is N998 (Home Education)."
                                + " - School Number, Current Enrollment is N999 (private school)."
                                + " - School Number, Current Enrollment has a School Function Setting equal to"
                                + " V and a Charter School Status not equal to Z on the Master School Identification file.")),

                new FLValidationRule("ENR", "46", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.notEquals("Educational Choice", "C"))),
                        "If Grade Level = PK, Educational Choice must not be C. ")),

                new FLValidationRule("ENR", "47", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W05"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        null, true, false, "Withdrawal Date"))),
                        "If Withdrawal Code, PK-12 = W05 then age must be 16 or greater as of the Withdrawal Date.")),

                new FLValidationRule("ENR", "48", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W25"))
                                .testThen(Restriction.pattern("Grade Level", "^PK|KG$"))),
                        "If Withdrawal Code = W25 then Grade Level must be PK or KG.")),

                new FLValidationRule("ENR", "49", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W25"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", null,
                                        Integer.valueOf(6), false, false,
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "02", "01")))),
                        "If Withdrawal Code, PK-12 = W25 then age must be less than 6 years old as"
                                + " of February 1 of the current school year.")),

                new FLValidationRule("ENR", "4D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Prior School State", "FL"))
                                .testThen(Restriction.notEquals("Prior School County", "99")),
                        ValidationRule.testIf(Restriction.notEquals("Prior School State", "FL"))
                                .testThen(Restriction.equals("Prior School County", "99"))),
                        "If the Prior School/Location: State/Territory or Commonwealth is FL, then"
                                + " Prior School Location: District/County must not be 99. If the Prior"
                                + " School/Location: State/Territory or Commonwealth is not FL, then Prior School"
                                + " Location: District/County must be 99.")),

                new FLValidationRule("ENR", "4E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.or(
                                        Restriction.and(
                                                Restriction.equals("District Number, E", "35"),
                                                Restriction.equals("School Number, E", "0531")),
                                        Restriction.and(
                                                Restriction.equals("District Number, E", "55"),
                                                Restriction.equals("School Number, E", "0231")),
                                        Restriction.and(
                                                Restriction.equals("District Number, E", "64"),
                                                Restriction.equals("School Number, E", "7821"))))
                                .testThen(Restriction.pattern("Edicational Choice", "^B|Z$"))),
                        "If District Number, Current Enrollment is 35 and School Number, Current"
                                + " Enrollment is 0531 or if District Number, Current Enrollment is 55 and School"
                                + " Number, Current Enrollment is 0231 or if District Number, Current Enrollment is"
                                + " 64 and School Number, Current Enrollment is 7821; then Educational Choice"
                                + " code must be B or Z.")),

                // TODO: what is the Charter School in terms of out setup?
                new FLValidationRule("ENR", "4F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("School Number, E", "<CHARTER>"))
                                .testThen(Restriction.notEquals("Edicational Choice", "A"))),
                        "If the School Number, Current Enrollment is a Charter School (Charter"
                                + " School Status on the Master School ID table is any code other than Z), then"
                                + " Educational Choice code must not be A.")),

                new FLValidationRule("ENR", "4G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Term", "S")))
                                .testThen(Restriction
                                        .and(Restriction.greaterThan("Days Present Summer", Double.valueOf(0)),
                                                Restriction.equals("Days Present Annual", "000"),
                                                Restriction.equals("Days Absent Annual", "000"))),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Term", "3")))
                                .testThen(Restriction
                                        .and(Restriction.equals("Days Present Summer", "000"),
                                                Restriction.equals("Days Absent Summer", "000")))),
                        "If Survey Period is 5 and Term is S, then Days Present, Summer Terms"
                                + " must be greater than zero and both Days Present, Annual and Days Absent,"
                                + " Annual must be zero. If Survey Period is 5 and Term is 3, then both Days Present,"
                                + " Summer and Days Absent, Summer must be zero.")),

                new FLValidationRule("ENR", "4H", new ValidateRegularExpression("District Number, I/S",
                        "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service must be numeric, in the range" +
                                " 01-68, or 71-75.")),

                new FLValidationRule("ENR", "4I", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"))
                                .testThen(Restriction.equals("Habitual Truant", "Z")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.pattern("Habitual Truant", "^N|Y$"))),
                        "Habitual Truant code must be N, Y or Z. If Survey Period Code is 2 or 3, then"
                                + " Habitual Truant code must be Z. If Survey Period Code is 5, then Habitual Truant"
                                + " code must be N or Y.")),

                new FLValidationRule("ENR", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("District Number, I/S", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "For Survey Period code 5, each Prior School Status/Student Attendance"
                                + " record must have a matching Student Demographic record based on both the"
                                + " District Number, Current Instruction/Service and District Number, Current"
                                + " Enrollment (Student Demographic) and District Number, Current Enrollment"
                                + " (Prior School Status); Student Number Identifier, Florida; Survey Period Code and School Year.")),

                new FLValidationRule("ENR", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W21"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Action Code", "E"), null)))),
                        "If the Withdrawal Code, PK-12 is W21 on the Prior School Status/Student"
                                + " Attendance record, then at least one Discipline/Resultant Action code on the"
                                + " Student Discipline/Resultant Action records must be E."
                                + " The records should be matched on the following items: District Number, Current"
                                + " Enrollment; Student Number Identifier, Florida; Survey Period Code; and School Year.")),

                new FLValidationRule("ENR", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Entry Code PK-12", "^R01|R02|R03$"))
                                .testThen(new ValidateEnrMatchInEnrExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.and(
                                                        Restriction.pattern("Entry Code PK-12",
                                                                "^E0[1-5]|E2A|E3A|E4A|E09$"),
                                                        Restriction.lessThanOrEquals("Withdrawal Date", null)),
                                                null)))),
                        "If the Entry (Re-Entry) Code, PK-12 is R01, R02, or R03, then there must be"
                                + " a matching Prior School Status/Student Attendance record (based on District"
                                + " Number, Current Enrollment; Student Number Identifier, Florida; Survey Period"
                                + " Code and School Year) with an Entry (Re-Entry) Code, PK-12 of E01-E05, E2A,"
                                + " E3A, E4A, or E09 and a Withdrawal Date less than or equal to the Entry (Re-Entry) Date.")),

                new FLValidationRule("ENR", "71", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^3|5$"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days Present Annual", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Days Present Annual", Double.valueOf(180))))),
                        "For Survey Period Codes 3 and 5, Days Present, Annual must be in the range 0-180.")),

                new FLValidationRule("ENR", "72", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^3|5$"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days Absent Annual", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Days Absent Annual", Double.valueOf(180))))),
                        "For Survey Period Codes 3 and 5, Days Absent, Annual must be in the range 0-180.")),

                // TODO: find how to get information from MSID file
                new FLValidationRule("ENR", "73", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days Present Summer", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Days Present Summer", Double.valueOf(45))))),
                        "For Survey Period Code 5 if School Function/Setting on the Master School"
                                + " Identification (MSID) file for the School Number, Current Enrollment does not"
                                + " equal D, Days Present, Summer Terms must be in the range 0-45.")),

                // TODO: find how to get information from MSID file
                new FLValidationRule("ENR", "74", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days Absent Summer", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Days Absent Summer", Double.valueOf(45))))),
                        "For Survey Period Code 5 if School Function/Setting on the Master School"
                                + " Identification (MSID) file for the School Number, Current Enrollment does not"
                                + " equal D, Days Absent, Summer Terms must be in the range 0-45.")),

                new FLValidationRule("ENR", "75", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^3|5$"))
                                .testThen(Restriction.sumLessThanOrEquals(Double.valueOf(180), "Days Absent Annual",
                                        "Days Present Annual"))),
                        "For Survey Period Codes 3 and 5, the sum of Days Absent, Annual and"
                                + " Days Present, Annual must not be greater than 180.")),

                // TODO: find how to get information from MSID file
                new FLValidationRule("ENR", "76", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.sumLessThanOrEquals(Double.valueOf(45), "Days Absent Summer",
                                        "Days Present Summer"))),
                        "For Survey Period Code 5 if School Function/Setting for the School"
                                + " Number, Current Enrollment on the Master School Identification (MSID) file does"
                                + " not equal D, the sum of Days Absent, Summer Terms and Days Present, Summer"
                                + " Terms must not be greater than 45.")),

                new FLValidationRule("ENR", "77", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Entry Code PK-12", "E05"))
                                .testThen(Restriction.pattern("Grade Level", "^PK|KG$"))),
                        "If Entry Code is E05, grade must be either PK or KG.")),

                new FLValidationRule("ENR", "78", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days Absent Unexcuse", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Days Absent Unexcuse", Double.valueOf(180))))),
                        "For Survey Period Code 5, Days Absent, Annual Unexcused Not Related"
                                + " to Discipline must be in the range 0-180.")),

                new FLValidationRule("ENR", "79", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Code", "W26"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        null, true, false, "Withdrawal Date"))),
                        "If Withdrawal Code, PK-12 = W26 then age must be 16 or greater as of the Withdrawal Date.")),

                new FLValidationRule("ENR", "80", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^2|3$"),
                                Restriction.equals("Withdrawal Code", "W02")))
                                .testThen(new ValidateEnrMatchInEnrExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.and(
                                                        Restriction.equals("Entry Code PK-12", "R02"),
                                                        Restriction.greaterThanOrEquals("Withdrawal Date", null)),
                                                null)))),
                        "If Survey Period = 2 or 3 and if the Withdrawal Code is W02, then there"
                                + " must be a matching Prior School Status/Student Attendance record (based on"
                                + " District Number, Current Enrollment; Student Number Identifier, Florida; Survey"
                                + " Period Code and School Year) with an Entry (Re-Entry) Code, PK-12 of R02 and"
                                + " an Entry/Re-entry Date greater than or equal to the Withdrawal Date. ")),

                // TODO: find how to get information from MSID file
                new FLValidationRule("ENR", "81", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.sumLessThanOrEquals(Double.valueOf(70), "Days Absent Summer",
                                        "Days Present Summer"))),
                        "For Survey Period Code 5 if School Function/Setting for the School"
                                + " Number, Current Enrollment on the Master School Identification (MSID) file "
                                + " equals D, the sum of Days Absent, Summer Terms and Days Present, Summer"
                                + " Terms must not be greater than 70.")),

                new FLValidationRule("ENR", "82", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Withdrawal Code", "W21")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Withdrawal Reason", "W21"), null)))),
                        "If Survey is 5 and if the Withdrawal Code, PK-12 is W21 on the Prior School"
                                + " Status/Student Attendance record, then the Withdrawal Reason must be W21 on"
                                + " the Student End of Year Status record."
                                + " The records should be matched on the following items: District Number, Current"
                                + " Enrollment; Student Number Identifier, Florida; Survey Period Code; and School Year.")),

                /*
                 * TODO: implement aggregate rules #90-93
                 */

        });
    }
}
