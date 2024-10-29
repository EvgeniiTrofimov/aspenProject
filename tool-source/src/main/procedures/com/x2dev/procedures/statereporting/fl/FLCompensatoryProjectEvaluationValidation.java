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
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.util.Arrays;
import java.util.List;

/**
 * The Class FLCompensatoryProjectEvaluationValidation.
 */
public class FLCompensatoryProjectEvaluationValidation {

    /**
     * Instantiates a new FL compensatory project evaluation validation.
     */
    public FLCompensatoryProjectEvaluationValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("CPE", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("CPE", "2", new ValidateRegularExpression(
                        "School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|9992|9993|999[5-7])$",
                        "School Number, Current Instruction/Service must be numeric in the range 0001 to 9899, excluding 9001, or it may be 9992, 9993, 9995, 9996 or 9997.")),
                new FLValidationRule("CPE", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number", "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be"
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be"
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is"
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an"
                                + "\"X\", the first three positions may not all be zeroes.")),
                new FLValidationRule("CPE", "4", new ValidateRegularExpression("Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("CPE", "5", new ValidateFiscalYear()),
                new FLValidationRule("CPE", "6",
                        new ValidateRegularExpression("Fed/St Project Type", "^[1258]$",
                                "Federal/State Project Type must be 1, 2, 5 or 8.")),
                new FLValidationRule("CPE", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "S"),
                                Restriction.pattern("Fed/St Project Type", "^[25]$"),
                                Restriction.equals("Support Services", "Z"))
                                .testThen(Restriction.notEquals("Fed/St Model", "00"))),
                        "If Term = S and Federal/State Project Type is 2 or 5, and Federal/State Project - Support Service = Z, "
                                + "then Federal/State Subject Area may not equal 0 (zero).")),
                new FLValidationRule("CPE", "8",
                        new ValidateRegularExpression("Migrant Priority", "^(Y|N|Z)$",
                                "Migrant Priority for Services must be Y, N or Z.")),
                new FLValidationRule("CPE", "9", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Fed/St Project Type", "^[25]$"))
                                .testThen(Restriction.notEquals("Migrant Priority", "Z"))),
                        "If Federal/State Project Type is 2 or 5 then the Migrant Priority for Services code must not equal Z.")),
                new FLValidationRule("CPE", "10",
                        new ValidateRegularExpression("Immigr Srv - Code L", "^(L|Z)$",
                                "Immigrant Student Services - Code L must be L or Z.")),
                new FLValidationRule("CPE", "11",
                        new ValidateRegularExpression("Immigr Srv - Code D", "^(D|Z)$",
                                "Immigrant Student Services - Code D must be D or Z.")),
                new FLValidationRule("CPE", "12",
                        new ValidateRegularExpression("Immigr Srv - Code F", "^(F|Z)$",
                                "Immigrant Student Services - Code F must be F or Z.")),
                new FLValidationRule("CPE", "13",
                        new ValidateRegularExpression("Immigr Srv - Code A", "^(A|Z)$",
                                "Immigrant Student Services - Code A must be A or Z.")),
                new FLValidationRule("CPE", "14",
                        new ValidateRegularExpression("Immigr Srv - Code B", "^(B|Z)$",
                                "Immigrant Student Services - Code B must be B or Z.")),
                new FLValidationRule("CPE", "15",
                        new ValidateRegularExpression("Immigr Srv - Code C", "^(C|Z)$",
                                "Immigrant Student Services - Code C must be C or Z.")),
                new FLValidationRule("CPE", "16",
                        new ValidateRegularExpression("Immigr Srv - Code M", "^(M|Z)$",
                                "Immigrant Student Services - Code M must be M or Z.")),
                // Skipped: Rule #17 (Transaction code)
                new FLValidationRule("CPE", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.uniqueValue("Student Number", "District Number",
                                        "School Number", "Survey Period", "Fiscal Year", "Fed/St Model",
                                        "Fed/St Project Type", "Term"))),
                        "Each Federal/State Compensatory Project Evaluation record must be unique based on the keys of District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; Student Number Identifier, Florida; Survey Period Code; School Year; Federal/State Project Type, "
                                + "Term and Federal/State Model.")),
                new FLValidationRule("CPE", "19",
                        new ValidateRegularExpression("Immigr Srv - Code O", "^(O|Z)$",
                                "Immigrant Student Services - Code O must be O or Z.")),
                new FLValidationRule("CPE", "20",
                        new ValidateRegularExpression("Term", "^(3|S)$", "Term must be 3 or S.")),
                new FLValidationRule("CPE", "21",
                        new ValidateRegularExpression("Immigr Srv - Code R", "^(R|Z)$",
                                "Immigrant Student Services - Code R must be R or Z.")),
                new FLValidationRule("CPE", "22",
                        new ValidateRegularExpression("Immigr Srv - Code S", "^(S|Z)$",
                                "Immigrant Student Services - Code S must be 9S or Z.")),
                new FLValidationRule("CPE", "23",
                        new ValidateRegularExpression("Immigr Srv - Code T", "^(T|Z)$",
                                "Immigrant Student Services - Code T must be T or Z.")),
                new FLValidationRule("CPE", "24",
                        new ValidateRegularExpression("Support Services", "^[ADHNORSTXZ]{4}$",
                                "Federal/State Project - Support Service must be A, D, H, N, O, R, S, T, X, or Z.")),
                new FLValidationRule("CPE", "25",
                        new ValidateRegularExpression("Referred Services", "^(Y|N|Z)$",
                                "Migrant Referred Services must be Y, N or Z.")),
                new FLValidationRule("CPE", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Fed/St Project Type", "^[25]$"))
                                .testThen(Restriction.notEquals("Referred Services", "Z"))),
                        "If Federal/State Project Type is 2 or 5 then the Migrant Referred Services code must not equal Z.")),
                new FLValidationRule("CPE", "27",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("CPE", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Fed/St Project Type", "^[25]$"),
                                Restriction.pattern("Fed/St Model", "^0[12]$"))
                                .testThen(Restriction.pattern("Service Provider", "^(T|Z)$")),
                        ValidationRule.testIf(Restriction.pattern("Fed/St Project Type", "^(?![25])\\d{1}$"),
                                Restriction.pattern("Fed/St Model", "^(?!(0[12]))\\d{2}$"))
                                .testThen(Restriction.equals("Service Provider", "Z"))),
                        "If Federal/State Project Type is 2 or 5 and Federal/State Subject Area is 1 or 2, "
                                + "then Federal/State Migrant Service Provider code must be T or Z. For all other Federal/State Types and Federal/State Subject Areas, "
                                + "the Federal/State Migrant Service Provider code must be Z.")),
                new FLValidationRule("CPE", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Fed/St Project Type", "1"))
                                .testThen(Restriction.notEquals("Fed/St Model", "00"))),
                        "If Federal/State Project Type is 1, then Federal/State Model code cannot be 00.")),
                new FLValidationRule("CPE", "2A",
                        new ValidateRegularExpression("Continuation", "^(A|B|C|Z)$",
                                "Migrant Continuation of Services must be A, B, C or Z.")),
                new FLValidationRule("CPE", "2B", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("CPE", "2C",
                        new ValidateRegularExpression("Fed/St Model", "^(0[019]|1[0-4])$",
                                "Federal/State Model must equal 00, 01, 09, 10, 11, 12, 13, 14.")),
                new FLValidationRule("CPE", "2D",
                        new ValidateRegularExpression("Service Provider", "^(T|Z)$",
                                "Federal/State Migrant Service Provider must be T or Z.")),
                new FLValidationRule("CPE", "2E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Fed/St Project Type", "8"))
                                .testThen(Restriction.equals("Fed/St Model", "14"))),
                        "If Federal/State Project Type equals 8, then Federal/State Model must equal 14.")),
                new FLValidationRule("CPE", "2F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Fed/St Model", "^(1[23])$"))
                                .testThen(Restriction.pattern("Service Provider", "^(T|Z)$")),
                        ValidationRule.testIf(Restriction.pattern("Fed/St Model", "^(?!(1[23]))\\d{2}$"))
                                .testThen(Restriction.equals("Service Provider", "Z"))),
                        "If Federal/State Model equals 12 or 13, then Federal/State Migrant Service Provider code must be T or Z. "
                                + "For all other Federal/State Models the Federal/State Migrant Service Provider must be Z.")),
                new FLValidationRule("CPE", "2G",
                        new ValidateRegularExpression("Subj Area - Other", "^(0|Z)$",
                                "Federal/State Subject Area - Other must be 0 or Z.")),
                new FLValidationRule("CPE", "2H",
                        new ValidateRegularExpression("Subj Area - R/L Arts", "^(1|Z)$",
                                "Federal/State Subject Area - Reading/Language Arts must be 1 or Z.")),
                new FLValidationRule("CPE", "2I",
                        new ValidateRegularExpression("Subj Area - Math", "^(2|Z)$",
                                "Federal/State Subject Area - Math must be 2 or Z.")),
                new FLValidationRule("CPE", "2J",
                        new ValidateRegularExpression("Subj Area - ESOL", "^(4|Z)$",
                                "Federal/State Subject Area - ESOL must be 4 or Z.")),
                new FLValidationRule("CPE", "2K",
                        new ValidateRegularExpression("Subj Area - MSTELO", "^(5|Z)$",
                                "Federal/State Subject Area - Multidisciplinary Studies/Tutoring/Extended Learning Opportunities must be 5 or Z.")),
                new FLValidationRule("CPE", "2L",
                        new ValidateRegularExpression("Subj Area - R Skills", "^(6|Z)$",
                                "Federal/State Subject Area - Readiness Skills must be 6 or Z.")),
                new FLValidationRule("CPE", "2M",
                        new ValidateRegularExpression("Subj Area - T Skills", "^(7|Z)$",
                                "Federal/State Subject Area - Transition Skills must be 7 or Z.")),
                new FLValidationRule("CPE", "2N",
                        new ValidateRegularExpression("Subj Area - Science", "^(8|Z)$",
                                "Federal/State Subject Area - Science must be 8 or Z.")),
                new FLValidationRule("CPE", "2O",
                        new ValidateRegularExpression("Subj Area - Soc Stud", "^(9|Z)$",
                                "Federal/State Subject Area - Social Studies must be 9 or Z.")),
                new FLValidationRule("CPE", "2P",
                        new ValidateRegularExpression("Subj Area - CTE/C Pr", "^(A|Z)$",
                                "Federal/State Subject Area - Career and Technical Education/Career Prep must be A or Z.")),
                new FLValidationRule("CPE", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(?!999[23]|999[56])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Instruction/Service is not 9992, 9993, 9995 or 9996, then it must exist on the "
                                + "Master School Identification File as a valid active school in the district of instruction.")),
                new FLValidationRule("CPE", "31", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Federal/State Compensatory Project Evaluation record must have a matching Student Demographic record based on "
                                + "District Number, Current Instruction/Service; Student Number Identifier, Florida; Survey Period Code and School Year."))
        });
    }
}
