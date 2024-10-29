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
 * The Class FLStudentTransportationValidation.
 */
public class FLStudentTransportationValidation {

    /**
     * Instantiates a new FL student transportation validation.
     */
    public FLStudentTransportationValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("TRN", "1", new ValidateDistrictNumber("District Number, I/S")),
                new FLValidationRule("TRN", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("TRN", "4", new ValidateRegularExpression("Survey Period", "^[1-4]$",
                        "Survey Period Code must be 1, 2, 3, or 4 and must be correct for the submission specified by the district.")),
                new FLValidationRule("TRN", "6", new ValidateFiscalYear()),
                new FLValidationRule("TRN", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.greaterThan("Days In Term", Double.valueOf(0)),
                                        Restriction.lessThan("Days In Term", Double.valueOf(100))))),
                        "Days in Term (for FTE Purposes) must be numeric, greater than zero and less than 100.")),
                new FLValidationRule("TRN", "8", new ValidateRegularExpression("Year-Round Indicator", "^[A-Z]$",
                        "Year-Round/Extended School Year FTE Indicator must be A or Z.")),
                new FLValidationRule("TRN", "9", new ValidateRegularExpression("Bus Number", "^(?!\\s{12}).{1,12}$",
                        "Bus Number must not contain all spaces (blanks).")),
                new FLValidationRule("TRN", "10", new ValidateRegularExpression("Bus Route Number", "^(?!\\s{15}).{1,15}$",
                        "Bus Route Number must not contain all spaces (blanks).")),
                new FLValidationRule("TRN", "11", new ValidateRegularExpression("Vehicle Category", "^[BEPG]$",
                        "Vehicle Category must be B, E, P, or G.")),
                new FLValidationRule("TRN", "12", new ValidateRegularExpression("Tran Memb Category", "^[FGLMN]$",
                        "Transportation Membership Category must be F, G, L, M, or N.")),
                new FLValidationRule("TRN", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("District Number, I/S",
                                        "Student Number", "Survey Period", "Fiscal Year",
                                        "Year-Round Indicator"))),
                        "Each Student Transportation record must be unique based on District Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Survey Period Code, Fiscal Year and Year-Round/Extended School Year FTE Indicator. "
                                + "-first record accepted, all other duplicate records rejected")),
                new FLValidationRule("TRN", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Vehicle Category", "B"))
                                .testThen(Restriction.notEquals("Bus Number", "ZZZZZZZZZZZZ"))),
                        "If Vehicle Category equals B, then Bus Number must not be all Zs.")),
                new FLValidationRule("TRN", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Vehicle Category", "B"))
                                .testThen(Restriction.notEquals("Bus Route Number", "ZZZZZZZZZZZZZZZ"))),
                        "If Vehicle Category equals B, then Bus Route Number must not be all Zs.")),
                new FLValidationRule("TRN", "16", new ValidateRegularExpression("District Number, E",
                        "^0[1-9]|[1-6][0-8]$",
                        "District Number, Current Enrollment must be numeric and in the range 01-68.")),
                new FLValidationRule("TRN", "17",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "The Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("TRN", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Tran Memb Category", "G"))
                                .testThen(Restriction.equals("Hazard Walk", "111111")),
                        ValidationRule.testIf(Restriction.pattern("Tran Memb Category", "^[FLMN]$"))
                                .testThen(Restriction.equals("Hazard Walk", "000000"))),
                        "If Transportation Membership Category is G then Hazardous Walking Code must be 111111, "
                                + "otherwise must be 000000 for all other Transportation Membership Categories (F, L, M N).")),
                new FLValidationRule("TRN", "21", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                // TODO Field logic should be implemented instead of default value as 90
                // new FLValidationRule("TRN", "22", new FLValidationRuleSet(new RuleSet(
                // ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                // .testThen(Restriction.lessThanOrEquals("Days In Term",
                // new RuntimeParam(RuntimeParam.DAYS_IN_SURVEY)))),
                // "If the Survey Period is 2 or 3 and the District Number, Current Enrollment
                // exists on the Less than 180 Days Regular School Year table, "
                // + "then the Days in Term (for FTE Purposes) should not exceed the days in the
                // survey reported on the Less than 180 Days Regular School Year table. "
                // + "Match on by Year and District Number, Current Enrollment.")),
                new FLValidationRule("TRN", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Tran Memb Category", "N"))
                                .testThen(Restriction.equals("Vehicle Category", "B"))),
                        "If Transportation Membership Category is N, then Vehicle Category must be B.")),
                new FLValidationRule("TRN", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Student Transportation record must have a matching Student Demographic record based on District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; Survey Period Code; and Fiscal Year.")),
                new FLValidationRule("TRN", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Tran Memb Category", "L"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Transportation Membership Category = L, then there must be a matching Exceptional Student record based on "
                                + "District Number, Current Enrollment; Student Number Identifier, Florida; Year and Survey Period Code.")),
                new FLValidationRule("TRN", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Tran Memb Category", "^(L|M)$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                                null)))),
                        "If Transportation Membership Category = L or M, then Grade Level on the matching Student Demographic Information record must be PK-12. "
                                + "The match should be made using the following elements: District Number, Current Enrollment; Student Number Identifier, Florida; "
                                + "Survey Period Code; and Year.")),
                new FLValidationRule("TRN", "53", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number, E", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.pattern("Grade Level", "^(0[7-9]|1[0-2])$"),
                                        null)))
                                .testThen(Restriction.equals("Hazard Walk", "000000"))),
                        "If Grade Level on the Demographic Information record is 07-12 then the Hazardous Walking Code must be 000000. "
                                + "The match is done using the following elements: District Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code; and Fiscal Year.")),
                // TODO Rule #90. The number of students for each school bus must be greater than 5.
        });
    }
}
