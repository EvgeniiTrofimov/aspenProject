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

import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;


/**
 * The Class FLStudentAssessmentValidation.
 */
public class FLStudentAssessmentValidation {

    /**
     * Instantiates a new FL student assessment validation.
     */
    public FLStudentAssessmentValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("ASM", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("ASM", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "The School Number, Current Enrollment must exist on the Master School "
                                + "Identification File as a valid active school in the district of enrollment.")),
                new FLValidationRule("ASM", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number", "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be "
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be "
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is "
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an "
                                + "\"X\", the first three positions may not all be zeroes.")),
                new FLValidationRule("ASM", "4", new ValidateRegularExpression(
                        "Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission "
                                + "specified by the district.")),
                new FLValidationRule("ASM", "5", new ValidateFiscalYear()),
                new FLValidationRule("ASM", "6", new ValidateRegularExpression(
                        "Test Name",
                        "^(APT|CAI|IBP)$",
                        "Test Name must be APT, CAI or IBP as listed on the Test Name Table in "
                                + "Appendix I of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual.")),
                new FLValidationRule("ASM", "7", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.and(Restriction.greaterThanOrEquals("Publication Year",
                                                Double.valueOf(1970)),
                                                Restriction.lessThanOrEquals("Publication Year",
                                                        Double.valueOf(Calendar.getInstance().get(Calendar.YEAR)))))),
                        "Test Publication Year must be numeric and a valid year in the range 1970 "
                                + "through the current year.")),
                new FLValidationRule("ASM", "8", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byDateFormat("MMddyyyy", "Test Date"))),
                        "Test Date must be numeric and a valid date that is not in the future.")),
                // TODO: Rule #9
                new FLValidationRule("ASM", "11", new ValidateRegularExpression("Test Score (1)",
                        "^[0\\d]{4}$",
                        "Test Score 1 must be numeric and right justified with leading zeros.")),
                new FLValidationRule("ASM", "13", new ValidateRegularExpression("Test Score (2)",
                        "^[0\\d]{4}$",
                        "Test Score 2 must be numeric and right justified with leading zeros.")),
                // Rule #14 skipped (Transaction Code)
                new FLValidationRule("ASM", "18", new ValidateRegularExpression("Test Level",
                        "\\S",
                        "Test Level must not be blank.")),
                new FLValidationRule("ASM", "19", new ValidateRegularExpression("Test Form",
                        "\\S",
                        "Test Form must not be blank.")),
                new FLValidationRule("ASM", "20", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Score Type (2)", "ZZ"))
                                        .testThen(Restriction.equals("Test Score (2)", "0000"))),
                        "If Test Score Type 2 is ZZ then Test Score 2 must be 0000.")),
                new FLValidationRule("ASM", "21", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number", "School Number",
                                                "Student Number", "Survey Period", "Fiscal Year", "Test Name",
                                                "Test Date", "Test Subject Content"))),
                        "Each Student Assessment record must be unique based on District "
                                + "Number, Current Enrollment; School Number, Current Enrollment; Student "
                                + "Number Identifier, Florida; Survey Period Code; School Year; Test Name, Test "
                                + "Date and Test Subject Content.")),
                new FLValidationRule("ASM", "22", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("ASM", "23", new ValidateRegularExpression("School Number",
                        "^(?!3518$)(?!9001$)([0-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to "
                                + "9899, excluding 3518 and 9001.")),
                new FLValidationRule("ASM", "24", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Test Name", "^(APT|CAI|IBP)$"))
                                        .testThen(Restriction.and(Restriction.equals("Test Score Type (1)", "SS"),
                                                Restriction.equals("Test Score Type (2)", "ZZ")))),
                        "Test Score Type 1 must be SS for APT, CAI or IBP, and Test Score Type 2 "
                                + "must be ZZ for APT, CAI or IBP.")),
                new FLValidationRule("ASM", "26", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                                        .testThen(Restriction.greaterThanOrEqualsYearOfDateField("Publication Year",
                                                "Test Date"))),
                        "If Test Name is CAI then Test Publication Year must be the year the data is "
                                + "being reported or the previous year.")),
                new FLValidationRule("ASM", "27", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                                        .testThen(Restriction.or(
                                                Restriction.equals("Test Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "01")),
                                                Restriction.equals("Test Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "02")),
                                                Restriction.equals("Test Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "11", "01")),
                                                Restriction.equals("Test Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "11", "02"))))),
                        "If Test Name is CAI then Test Date must be June 1 (0601), June 2 (0602) "
                                + "November 1 (1101) or November 2 (1102) of the school year being reported.")),
                new FLValidationRule("ASM", "28", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Test Name", "^(APT|CAI|IBP)$"))
                                        .testThen(Restriction.equals("Test Form", "Z"))),
                        "If Test Name is APT, CAI or IBP then Test Form must be Z.")),
                new FLValidationRule("ASM", "29", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                                        .testThen(Restriction.pattern("Test Level", "^(A|AS)$"))),
                        "If Test Name is CAI then Test Level must be A or AS.")),
                new FLValidationRule("ASM", "30", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "CAI"))
                                        .testThen(Restriction.and(
                                                Restriction.pattern("Test Score (1)", "^000[0-8]$"),
                                                Restriction.pattern("Test Score (2)", "^000[0-8]$")))),
                        "If Test Name is CAI then Test Score must be 0000, 0001, 0002, 0003, 0004,"
                                + "0005, 0006, 0007, or 0008.")),
                new FLValidationRule("ASM", "31", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "APT"))
                                        .testThen(Restriction.equals("Test Level", "ZZ"))),
                        "If Test Name is APT then Test Level must be ZZ.")),
                new FLValidationRule("ASM", "32", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "IBP"))
                                        .testThen(Restriction.pattern("Test Level", "^(HL|SL)$"))),
                        "If Test Name is IBP then Test Level must be HL or SL.")),
                new FLValidationRule("ASM", "33", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "APT"))
                                        .testThen(
                                                Restriction.pattern("Test Score (1)", "^000[1-5]$"))),
                        "If Test Name is APT then Test Score (1) must be 0001, 0002, 0003, 0004, or 0005.")),
                new FLValidationRule("ASM", "34", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Name", "IBP"))
                                        .testThen(Restriction.pattern("Test Score (1)", "^000[1-9]$"))),
                        "If Test Name is IBP then Test Score (1) must be 0001, 0002, 0003, 0004, 0005, 0006, 0007, 0008 or 0009.")),
                new FLValidationRule("ASM", "35", new ValidateRegularExpression("FL Education ID",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable.")),
                new FLValidationRule("ASM", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Student Assessment record must have a matching Student "
                                + "Demographic Information record based on District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; Survey Period Code and School Year.")),
        });
    }
}
