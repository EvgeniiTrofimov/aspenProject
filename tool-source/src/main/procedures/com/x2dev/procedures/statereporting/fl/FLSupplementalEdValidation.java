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
import java.util.List;

/**
 * The Class FLSupplementalEdValidation.
 */
public class FLSupplementalEdValidation {

    /**
     * Instantiates a new FL supplemental education validation.
     */
    public FLSupplementalEdValidation() {

    }
    
    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("TITLEI", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("TITLEI", "2", new ValidateRegularExpression(
                        "School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N998)$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001 or it may be N998")),
                new FLValidationRule("TITLEI", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("TITLEI", "4", new ValidateRegularExpression(
                        "Survey Period", "^[1-6]|[89]$", "Survey Period Code must be 1, 2, 3, 4, 5, 6, 8 or 9.")),
                new FLValidationRule("TITLEI", "5",  new ValidateFiscalYear()),
                new FLValidationRule("TITLEI", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.uniqueValue("Student Number", "District Number", "School Number",
                                        "Survey Period", "Fiscal Year", "Service Provider", "Term"))),
                        "Each Title I Supplemental Educational Services record must be unique based on District Number, Current Enrollment; School Number, "
                                + "Current Enrollment; Student Number Identifier, Florida; Survey Period Code; Year; Title 1 Supplemental Educational Services "
                                + "Service Provider and Term.")),
                new FLValidationRule("TITLEI", "8", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "3"))
                                .testThen(Restriction.pattern("HOC  Science", "^999|180|1[0-7]\\d|00\\d|0\\d\\d$")),
                        ValidationRule.testIf(Restriction.equals("Term", "S"))
                                .testThen(Restriction.pattern("HOC  Science", "^999|090|0[0-8][0-9]$"))),
                        "If Term equals 3 then Title I Supplemental Educational Services Hours of Contact: Science must be numeric, equal to or greater than "
                                + "zero and equal to or less than 180 or it may be 999. If Term equals S then Title I Supplemental Educational Services Hours of Contact: "
                                + "Science must be numeric, equal to or greater than zero and equal to or less than 90 or it may be 999.")),
                new FLValidationRule("TITLEI", "11",
                        new ValidateRegularExpression("Term", "^3|S$", "Term must be 3 or S.")),
                new FLValidationRule("TITLEI", "12",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                //TODO Appendix T not defined
//                new FLValidationRule("TITLEI", "13", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
//                                .testThen(Restriction.byExtFldRefTable("FL-PGM-SES", "pgm-ses-service-provider",
//                                        "Service Provider"))),
//                        "The Title I Supplemental Educational Services - Service Provider must be a valid assigned identifier. Valid Title I Supplemental Educational Services "
//                        + " Service Provider assigned identifiers are specified in Appendix T of the DOE Information Data Base Requirements: Volume I "
//                        + " Automated Student Information manual.")),
                new FLValidationRule("TITLEI", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "School Number, Current Enrollment must exist on the Master School Identification File as a valid active school number "
                                + "for the District Number, Current, Enrollment.")),
                new FLValidationRule("TITLEI", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "3"))
                                .testThen(Restriction.pattern("HOC Read Lang Art",
                                        "^999|180|[0-1][0-7][0-9]$")),
                        ValidationRule.testIf(Restriction.equals("Term", "S"))
                                .testThen(Restriction.pattern("HOC Read Lang Art", "^999|090|0[0-8][0-9]$"))),
                        "If Term equals 3 then Title I Supplemental Educational Services Hours of Contact: Reading/Language Arts must be numeric, equal to or greater "
                                + "than zero and equal to or less than 180 or it may be 999. If Term equals S then Title I Supplemental Educational Services Hours of Contact: "
                                + "Reading/Language Arts must be numeric, equal to or greater than zero and equal to or less than 90 or it may be 999.")),
                new FLValidationRule("TITLEI", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "3"))
                                .testThen(Restriction.pattern("HOC Math", "^999|180|1[0-7]\\d|00\\d|0\\d\\d$")),
                        ValidationRule.testIf(Restriction.equals("Term", "S"))
                                .testThen(Restriction.pattern("HOC Math", "^999|090|0[0-8][0-9]$"))),
                        "If Term equals 3 then Title I Supplemental Educational Services Hours of Contact: Math must be numeric, equal to or greater than zero and equal "
                                + "to or less than 180 or it may be 999. If Term equals S then Title I Supplemental Educational Services Hours of Contact: Math must be numeric, "
                                + "equal to or greater than zero and equal to or less than 90 or it may be 999.")),
                new FLValidationRule("TITLEI", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.byActiveSchool("SES School"))),
                        "Title I Supplemental Educational Services School must exist on the Master School Identification File as a valid active school number for "
                                + "the District Number, Current, Enrollment.")),
                // TODO: Rule #24 Master School Identification (MSID) file
                new FLValidationRule("TITLEI", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.or(Restriction.notEquals("HOC  Science", "999"),
                                        Restriction.notEquals("HOC Math", "999"),
                                        Restriction.notEquals("HOC Read Lang Art", "999")))),
                        "One of the three elements, Title I Supplemental Educational Services - Hours of Contact: Reading/Language Arts, Title I Supplemental Educational Services"
                                + " - Hours of Contact: Math and Title I Supplemental Educational Services Hours of Contact: Science must be less than 999.")),
                new FLValidationRule("TITLEI", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("HOC  Science", "^999|000$"),
                                Restriction.pattern("HOC Read Lang Art", "^999|000$"),
                                Restriction.pattern("HOC Math", "^999|000$")))
                                .testThen(Restriction.equals("Begin Services Date", "00000000")),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("HOC  Science", "^(?!999|000)\\d{3}$"),
                                Restriction.pattern("HOC Read Lang Art", "^(?!999|000)\\d{3}$"),
                                Restriction.pattern("HOC Math", "^(?!999|000)\\d{3}$")))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Begin Services Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "01")),
                                        Restriction.lessThanOrEquals("Begin Services Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "If Title I Supplemental Educational Services - Hours of Contact: Reading Language Arts, Title I Supplemental Educational Services "
                                + "Hours of Contact: Math and Title I Supplemental Educational Services Hours of Contact: Science are all equal to zero and/or 999, "
                                + "then the Title I Begin Services Date must equal zero. On all other records, the Title I Supplemental Educational Services Begin "
                                + "Services Date must be numeric and a valid date greater than or equal to 06/01/**** and less than 08/31/****.")),
                new FLValidationRule("TITLEI", "27", new ValidateRegularExpression("Florida Education ID",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("TITLEI", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3|5$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code is 3, 9 or 5, the Title I Supplemental Educational Services record must have a matching Student Demographic record "
                                + "based on District Number, Current Enrollment; District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                                + "Survey Period Code and Year.")),
                new FLValidationRule("TITLEI", "53", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period equals 5, then each Title I Supplemental Educational Services record must have a matching Student End of Year Status "
                                + "record based on District Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code and Year."))
        });
    }
}
