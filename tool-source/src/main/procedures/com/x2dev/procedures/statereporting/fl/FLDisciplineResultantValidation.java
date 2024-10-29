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
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionOr;
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
 * Student Discipline/Resultant Action (SDRA) export validation
 * Specification: http://www.fldoe.org/core/fileparse.php/18496/urlt/1718sdra.pdf
 */
public class FLDisciplineResultantValidation {

    /**
     * The Class ValidateSdraMatchInExceptExport.
     */
    private static class ValidateSdraMatchInExceptExport extends ValidateMatchInExport {

        private static final String DEFAULT_DATE_FORMAT_MASK = "MMddyyyy";
        private static final SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(DEFAULT_DATE_FORMAT_MASK);

        /**
         * Instantiates a new validate sdra match in except export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateSdraMatchInExceptExport(String procedure_id, KeyValuePair<String, String> keyFieldNamePair,
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

            String incidentDateValue = helper.getExportFormatRowFieldValue(row, currentExport, "Incident Date");
            try {
                PlainDate incidentDate = new PlainDate(DEFAULT_DATE_FORMAT.parse(incidentDateValue));
                for (KeyValuePair<Object, String> fieldNamePair : m_relatedFieldNamePairs) {
                    if (fieldNamePair.getKey() instanceof RestrictionAnd) {
                        RestrictionAnd restrictionAnd = (RestrictionAnd) fieldNamePair.getKey();
                        Restriction[] ands = restrictionAnd.getRestrictions();
                        if (ands.length > 1 && ands[1] instanceof RestrictionOr) {
                            RestrictionOr restrictionOr = (RestrictionOr) ands[1];
                            Restriction[] ors = restrictionOr.getRestrictions();
                            if (ors.length > 1 && ors[1] instanceof RestrictionCompare) {
                                RestrictionCompare cmp = (RestrictionCompare) ors[1];
                                cmp.setComparableValue(incidentDate);
                            }
                        }
                        if (ands.length > 2 && ands[2] instanceof RestrictionCompare) {
                            RestrictionCompare cmp = (RestrictionCompare) ands[2];
                            cmp.setComparableValue(incidentDate);
                        }
                    }
                }
                return super.getValidationResult(helper, currentExport, row);
            } catch (Exception ex) {
                return ValidationResult.FieldNotValid("Incident Date", incidentDateValue);
            }
        }
    }

    /**
     * Instantiates a new FL discipline resultant validation.
     */
    public FLDisciplineResultantValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SDRA", "1", new ValidateDistrictNumber("District Number")),

                new FLValidationRule("SDRA", "2", new ValidateRegularExpression("School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[8-9])$",
                        "The School Number, Current Enrollment must be alphanumeric and in the range 0001-9899, excluding 9001, or it must be N998 or N999.")),

                new FLValidationRule("SDRA", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),

                new FLValidationRule("SDRA", "4", new ValidateRegularExpression("Survey Period", "^(2|3|5)$",
                        "Survey Period Code must be 2, 3 or 5 and must be correct for the submission specified by the district.")),

                new FLValidationRule("SDRA", "5", new ValidateFiscalYear()),

                new FLValidationRule("SDRA", "6", new ValidateRegularExpression(
                        "Action Code", "^[CEFHILMOPRSU]$",
                        "The Discipline/Resultant Action Code must be alphabetic and must be C,E,F,H,I,L,M,O,P,R,S or U.")),

                new FLValidationRule("SDRA", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("District Number",
                                        "Student Number", "Action Code", "Action School Number",
                                        "Incident Identifier", "Survey Period", "Fiscal Year"))),
                        "Each Student Discipline/Resultant Action record must be unique based on"
                                + " the keys of District Number, Current Enrollment; Student Number Identifier,"
                                + " Florida; Discipline/Resultant Action Code; School Number, Where"
                                + " Discipline/Resultant Action Occurred; Incident, Identifier; Survey Period Code"
                                + " and School Year.")),

                new FLValidationRule("SDRA", "9", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.and(
                                Restriction.pattern("Incident Identifier", "^[a-zA-Z0-9]*$"),
                                Restriction.notEquals("Incident Identifier", "^00000000$")))),
                        "Incident, Identifier must be alphanumeric, may not be zero, and must not contain blanks.")),

                new FLValidationRule("SDRA", "10", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.and(
                                Restriction.byDateFormat("Incident Date"),
                                Restriction.greaterThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                Restriction.lessThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")),
                                Restriction.lessThanOrEquals("Incident Date",
                                        new RuntimeParam(RuntimeParam.PERIOD_END_DATE))))),
                        "Incident, Date must be numeric, must be a valid date, in the range of"
                                + " 07/01/**** to 08/31/**** and not greater than the last day of the survey period.")),

                new FLValidationRule("SDRA", "11", new ValidateRegularExpression("Action Duration", "^\\d{3}$",
                        "The Duration, Discipline Action must be a three digit numeric code indicating length of Discipline action.")),

                new FLValidationRule("SDRA", "13", new ValidateRegularExpression("Hate Crime Involved", "^(Y|N)$",
                        "Student, Involved in Hate Crime code must be Y or N.")),

                new FLValidationRule("SDRA", "14", new ValidateRegularExpression("Use of Alcohol", "^(Y|N)$",
                        "Student, Use of Alcohol code must be Y or N.")),

                new FLValidationRule("SDRA", "15", new ValidateRegularExpression("Use of Drugs", "^(Y|N)$",
                        "Student, Use of Drugs code must be Y or N.")),

                new FLValidationRule("SDRA", "16", new ValidateRegularExpression("Weapon Use", "^(Y|N)$",
                        "Student, Weapon Use code must be Y or N.")),

                new FLValidationRule("SDRA", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "C")).testThen(
                                Restriction.equals("Action Duration", "000"))),
                        "If the Discipline/Resultant Action code is C, then Duration, Discipline Action must be zero.")),

                new FLValidationRule("SDRA", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "O")).testThen(
                                Restriction.pattern("Action Duration", "^010|00[1-9]$"))),
                        "If the Discipline/Resultant Action code is O, then Duration, Discipline Action must be greater than zero and less than or equal to 10.")),

                new FLValidationRule("SDRA", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "I")).testThen(
                                Restriction.pattern("Action Duration", "^010|00[1-9]$"))),
                        "If the Discipline/Resultant Action code is I, then Duration, Discipline Action must be greater than zero and less than or equal to 10.")),

                new FLValidationRule("SDRA", "20", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule
                                        .testIf(Restriction.and(Restriction.notEquals("School Number", "N998"),
                                                Restriction.notEquals("School Number", "N999")))
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, then it must"
                                + " exist on the Master School Identification File as a valid active school number in"
                                + " the district of enrollment. ")),

                new FLValidationRule("SDRA", "21", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byActiveSchool("Action School Number"))),
                        "The School Number, Where Discipline/Resultant Action Occurred must"
                                + " exist on the Master School Identification File as a valid active school number in"
                                + " the district of enrollment.")),

                new FLValidationRule("SDRA", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "S")).testThen(
                                Restriction.pattern("Action Duration", "^090|0[0-8][0-9]$"))),
                        "If the Discipline/Resultant Action code is S, the Duration, Discipline Action"
                                + " must be equal to or greater than zero and less than or equal to 90.")),

                new FLValidationRule("SDRA", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.byDateFormat("Birth Date"))),
                        "Birth Date must be numeric and a valid date in the format MMDDYYYY")),

                new FLValidationRule("SDRA", "24", new ValidateRegularExpression("Gender", "^(M|F)$",
                        "Gender code must be M or F.")),

                new FLValidationRule("SDRA", "26", new ValidateRegularExpression("ELL PK-12", "^(LY|LF|LP|LZ|ZZ)$",
                        "English Language Learners, PK-12 code must be LY, LF, LP, LZ, or ZZ")),

                new FLValidationRule("SDRA", "27",
                        new ValidateRegularExpression("Lunch Status", "^(0|1|3|4|C|D|E|F|N|R)$",
                                "Lunch Status Must be 0, 1, 3, 4, C, D, E, F, N or R.")),

                new FLValidationRule("SDRA", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction
                                .lessThanOrEquals("Birth Date", new RuntimeParam(RuntimeParam.DATE_CERTAIN)))),
                        "Birth Date must be less than or equal to the survey date.")),

                new FLValidationRule("SDRA", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$")).testThen(Restriction
                                .pattern("Grade Level", "^(PK|KG|10|11|12|0[1-9])$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5")).testThen(Restriction
                                .pattern("Grade Level", "^(PK|KG|10|11|12|0[1-9])$"))),
                        "If Survey Period Code = 2 or 3, Grade Level code must be PK, KG or 01-12."
                                + " If Survey Period Code = 5, Grade Level code must be PK, KG or 01-12. ")),

                new FLValidationRule("SDRA", "2A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "U")).testThen(
                                Restriction.pattern("Action Duration", "^(04[0-5]|0[1-3][0-9]|00[1-9])$"))),
                        "If the Discipline/Resultant Action code is U, the Duration, Discipline Action"
                                + " must be greater than zero and less than or equal to 45.")),

                // TODO: discuss actual age limits by grade level, they used in the STD export
                // validation as well
                new FLValidationRule("SDRA", "2B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(0),
                                        Integer.valueOf(9),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "5"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),

                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(9),
                                        Integer.valueOf(11),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE)))),
                        "There must be a valid association between the Grade Level listed below "
                                + "and the student's age. For Survey Periods 2 and 3, age will be determined as of "
                                + "Date Certain (Friday) of the survey period. For Survey Period 5, age will be "
                                + "determined as of June 30."
                                + "PK-not equal to 9+, KG-not equal to 0-3, 11+, 1-not equal to 0-4, 12+, 2-not equal to 0-4, 13+, 3-not equal to 0-4, 14+, "
                                + "4-not equal to 0-4, 15+, 5-not equal to 0-4, 16+, 6-not equal to 0-4, 18+, 7-not equal to 0-4, 19+, 8-not equal to 0-4, 20+, "
                                + "9-not equal to 0-4, 27+, 10-not equal to 0-4, 28+, 11-not equal to 0-4, 29+, 12-not equal to 0-4, 30+")),

                new FLValidationRule("SDRA", "2C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "H")).testThen(
                                Restriction.pattern("Action Duration", "^090|0[1-8][0-9]|00[1-9]$"))),
                        "If the Discipline/Resultant Action code is H, the Duration, Discipline Action"
                                + " must be greater than zero and less than or equal to 90.")),

                new FLValidationRule("SDRA", "2D", new ValidateRegularExpression("Student ID Local",
                        "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),

                new FLValidationRule("SDRA", "2F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("Action School Number",
                                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$"))),
                        "School Number, Where Discipline/Resultant Action Occurred must be "
                                + "numeric and a valid school number in the range 0001 to 9899, excluding 9001.")),

                new FLValidationRule("SDRA", "2G", new ValidateRegularExpression("Involved in Bullying", "^(Y|N)$",
                        "Student, Involved in Bullying must be Y or N.")),

                new FLValidationRule("SDRA", "2H", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("Incident School Num",
                                        "^(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$"))),
                        "School Number, Where Incident Occurred must be numeric in the range 0001-9899.")),

                new FLValidationRule("SDRA", "2I", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Incident School Num", "3518"))
                                        .testThen(Restriction.byActiveSchool("Incident School Num"))),
                        "School Number, Where Incident Occurred must exist on the Master School "
                                + "Identification File as a valid active school in the District Number, Current "
                                + "Enrollment, excluding 3518.")),

                new FLValidationRule("SDRA", "2J", new ValidateRegularExpression("Zero T Expulsions", "^(Y|N|Z)$",
                        "Zero-Tolerance: Expulsions code must be Y, N or Z.")),

                new FLValidationRule("SDRA", "2K", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Zero T Expulsions", "^(Y|N)$"))
                                        .testThen(Restriction.pattern("Action Code", "^(E|F|U)$")),
                                ValidationRule.testIf(Restriction.pattern("Action Code", "^(E|F|U)$"))
                                        .testThen(Restriction.pattern("Zero T Expulsions", "^(Y|N)$"))),
                        "If the Zero-Tolerance: Expulsions code is Y or N, then Discipline/Resultant "
                                + "Action Code must be E, F or U. If the Discipline/Resultant Action Code is E, F or"
                                + "U then the Zero-Tolerance: Expulsions code must be Y or N. ")),

                new FLValidationRule("SDRA", "2L", new ValidateRegularExpression("Fl Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),

                new FLValidationRule("SDRA", "31", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(?!N998|N999)\\S{4}$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!N998|N999)\\S{4}$")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "For surveys 2 and 3, each Student Discipline/Resultant Action record must "
                                + "have a matching Student Demographic record based on District Number, Current "
                                + "Enrollment; Student Number Identifier, Florida; Survey Period Code and School "
                                + "Year unless School Number/Current Enrollment is N998 or N999. For survey 5, "
                                + "each Student Discipline/Resultant Action record must have a matching Student "
                                + "Demographic and Student End of Year Status record based on District Number, "
                                + "Current Enrollment; Student Number Identifier, Florida; Survey Period Code and "
                                + "School Year unless School Number/Current Enrollment is N998 or N999.")),

                new FLValidationRule("SDRA", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Use of Drugs", "Y"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Drug Description", "^(M|N|O)$"),
                                                null)))),
                        "If the Student, Use of Drugs code is Y, then the Drug Description code on "
                                + "the matching Student Environmental Safety Incident Report (SESIR) format must "
                                + "be M, N or O. The records match is done using the District Number, Current "
                                + "Enrollment on the Student Discipline/Resultant Action format matched to the "
                                + "District Number, Reporting District on the SESIR format; along with matching "
                                + "School Year, Survey Period Code and Incident, Identifier. ")),

                new FLValidationRule("SDRA", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                        new KeyValuePair("Incident Date", "Incident, Date"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Weapon-Related", "^(1|2|3|4)$"), null)))
                                .testThen(Restriction.pattern("Weapon Use", "^(Y|N)$")),
                        ValidationRule.testIf(
                                Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                        new KeyValuePair("Incident Date", "Incident, Date"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Weapon-Related", "N"), null)))
                                .testThen(Restriction.equals("Weapon Use", "N"))),
                        "If Incident, Weapon-Related code on the School Environmental Safety"
                                + " Incident Report is 1, 2, 3 or 4, then Student, Weapon Use may be Y or N. If"
                                + " Incident, Weapon-Related code is N, then Student, Weapon Use code must be N."
                                + " The records should match on District Number, Current Enrollment on the Student"
                                + " Discipline Resultant Action record and District Number, Reporting District on the"
                                + " School Environmental Safety Incident Report record; Survey Period Code; School"
                                + " Year; Incident Identifier; and Incident Date.")),

                new FLValidationRule("SDRA", "35", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "E"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Withdrawal Code", "W21"), null)))),
                        "If Survey = 2 or 3 and if the Discipline/Resultant Action code is E, then"
                                + " Withdrawal Code, PK-12 on at least one Prior School Status/Student Attendance"
                                + " (PSS/SA) record must = W21. If Survey = 5 and if the Discipline/Resultant Action"
                                + " code is E, then the Withdrawal Code, PK-12 on at least one PSS/SA record must=W21."
                                + " The records should be matched on the following items: District Number, Current"
                                + " Enrollment; Student Number Identifier, Florida; Survey Period Code; and School Year.")),

                new FLValidationRule("SDRA", "36", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "E"))
                                .testThen(Restriction.validateNotMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.and(Restriction.equals("Exceptionality Other", "Z"),
                                                        Restriction.equals("Except Primary", "L")),
                                                null)))),
                        "If the Discipline/Resultant Action code is E, then there must not be a"
                                + " matching Exceptional Student record unless the Exceptionality, Primary = L"
                                + " (Gifted) and Exceptionality, Other = Z. The match should be done on District"
                                + " Number, Current Enrollment; Survey Period Code; School Year matched to Year"
                                + " and Student Number Identifier, Florida.")),

                new FLValidationRule("SDRA", "37", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Weapon Use", "Y"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                        new KeyValuePair("Incident Date", "Incident, Date"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Action School Number", "School Number"),
                                        new KeyValuePair(Restriction.notEquals("Weapon, Description", "Z"),
                                                null)))),
                        "If Student, Weapon Use on the Student Discipline Resultant Action format ="
                                + " Y, then Weapon, Description on the School Environmental Safety Incident Report"
                                + " format must not = Z."
                                + " The records should match on District Number, Current Enrollment on the Student"
                                + " Discipline Resultant Action record and District Number, Reporting District on the"
                                + " School Environmental Safety Incident Report record; School Number, Where"
                                + " Incident Occurred on the SESIR format matched to School Number, Where"
                                + " Discipline/Resultant Action Occurred on the SDRA format; Survey Period Code;"
                                + " School Year; Incident Identifier; and Incident Date. ")),

                new FLValidationRule("SDRA", "38", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "U"))
                                .testThen(new ValidateSdraMatchInExceptExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.and(
                                                        Restriction.or(
                                                                Restriction.notEquals("Exceptionality Other", "Z"),
                                                                Restriction.notEquals("Except Primary", "L")),

                                                        Restriction.or(
                                                                Restriction.equals("Dismissal Date", "000000"),
                                                                Restriction.greaterThan("Dismissal Date", null)),

                                                        Restriction.lessThanOrEquals("Placement Date", null)),
                                                null)))),
                        "If the Discipline/Resultant Action code is E, then there must not be a"
                                + " matching Exceptional Student record unless the Exceptionality, Primary = L"
                                + " (Gifted) and Exceptionality, Other = Z. The match should be done on District"
                                + " Number, Current Enrollment; Survey Period Code; School Year matched to Year"
                                + " and Student Number Identifier, Florida.")),

                new FLValidationRule("SDRA", "39", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "U"))
                                .testThen(Restriction.or(
                                        Restriction.equals("Use of Drugs", "Y"),
                                        Restriction.equals("Weapon Use", "Y"),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                                new KeyValuePair("Incident Date", "Incident, Date"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Action School Number", "School Number"),
                                                new KeyValuePair(Restriction.pattern("Injury-Related", "^A|B$"),
                                                        null))))),
                        "If the Discipline/Resultant Action Code is U, then one of the following must be true:"
                                + " a. Student, Use of Drugs must be Y; or"
                                + " b. Student, Weapon Use must be Y; or"
                                + " c. there is a matching School Environmental Safety Incident Report"
                                + " (SESIR) record with an Incident, Injury-Related code of A or B."
                                + " The records should match on District Number, Current Enrollment on the Student"
                                + " Discipline Resultant Action (SDRA) record and District Number, Reporting District"
                                + " on the SESIR record; School Number, Where Incident Occurred on the SESIR"
                                + " format matched to School Number, Where Discipline/Resultant Action Occurred"
                                + " on the SDRA format; Survey Period Code; School Year; Incident Identifier; and"
                                + " Incident Date.")),

                new FLValidationRule("SDRA", "3A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "S"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SESIR",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident, Identifier"),
                                        new KeyValuePair("Incident Date", "Incident, Date"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Action School Number", "School Number")))),
                        "If Discipline/Resultant Action Code on the Student Discipline/Resultant"
                                + " Action format = S, then there must be a matching record on the School"
                                + " Environmental Safety Incident Report (SESIR) format."
                                + " The records should match on District Number, Current Enrollment on the Student"
                                + " Discipline Resultant Action [SDRA] record and District Number, Reporting District"
                                + " on the School Environmental Safety Incident Report [SESIR] record; School"
                                + " Number, Where Incident Occurred on the SESIR format matched to School"
                                + " Number, Where Discipline/Resultant Action Occurred on the SDRA format;"
                                + " Survey Period Code; School Year; Incident Identifier; and Incident Date.")),

                new FLValidationRule("SDRA", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Action Code", "^(F|P)$"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThan("Action Duration", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Action Duration", Double.valueOf(210))))),
                        "If the Discipline/Resultant Action code is F or P, then Duration, Discipline"
                                + " Action must be greater than zero and less than or equal to 210.")),

                new FLValidationRule("SDRA", "41", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Action Code", "E"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThan("Action Duration", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("Action Duration", Double.valueOf(210))))),
                        "If the Discipline/Resultant Action code is E, then Duration, Discipline"
                                + " Action must be greater than zero and less than or equal to 210.")),

                new FLValidationRule("SDRA", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(5),
                                        Integer.valueOf(Integer.MAX_VALUE),
                                        true, false, new RuntimeParam(RuntimeParam.FISCAL_DATE, "09", "01")))),
                        "If Grade Level is KG-12, the student must be at least 5 years old on or"
                                + " before September 1 of the school year being reported.")),

                new FLValidationRule("SDRA", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Action Code", "H"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Incident Identifier", "Incident Identifier"),
                                        new KeyValuePair("Incident Date", "Incident Date"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Action School Number", "Action School Number"),
                                        new KeyValuePair(Restriction.pattern("Action Code", "^(E|F|P)$"), null)))),
                        "If Survey Period Code is equal to 5, and Discipline/Resultant Action code is"
                                + " H, then there must be a matching Student Discipline/Resultant Action record with"
                                + " code E, F, or P. The match should be done using District Number, Current"
                                + " Enrollment; Student Number Identifier, Florida; School Number, Where"
                                + " Discipline/Resultant Action Occurred; Incident, Identifier, Survey Period Code;"
                                + " Incident Date; and School Year.")),

                new FLValidationRule("SDRA", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("Action Code", "E"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Withdrawal Reason", "W21"), null)))),
                        "If Survey = 5 and if the Discipline/Resultant Action code is E, then the"
                                + " Withdrawal Reason on the Student End of Year Status record must = W21.")),

                new FLValidationRule("SDRA", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair(Restriction.pattern("Action Code", "^(E|F)$"), null)))),
                        "If Survey Period is 5, for each district there must be at least one record with"
                                + " a Discipline/Resultant Action Code of E or F (expelled from school with or without services).")),
        });
    }
}
