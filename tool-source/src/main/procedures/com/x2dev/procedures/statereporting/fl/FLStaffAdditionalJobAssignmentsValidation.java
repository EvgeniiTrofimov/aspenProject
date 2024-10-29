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
 * The Class FLStaffAdditionalJobAssignmentsValidation.
 */
public class FLStaffAdditionalJobAssignmentsValidation {


    /**
     * Instantiates a new FL staff additional job assignments validation.
     */
    public FLStaffAdditionalJobAssignmentsValidation() {
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SAJA", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("SAJA", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("SSN", "^.{9}.*$"))
                                        .testThen(Restriction.pattern("SSN", "^.{9}\\s$")),
                                ValidationRule.testIf(Restriction.pattern("Staff ID Local", "^(?!CS\\d{7}\\s{0,1}).{1,10}$"))
                                        .testThen(Restriction.and(
                                                Restriction.pattern("SSN", "^\\d{9}[\\s|\\d]{1}$"),
                                                Restriction.greaterThan("SSN", Double.valueOf(0)),
                                                Restriction.notEquals("SSN", "999999999")))),
                        "Social Security Number (SSN) must be numeric and greater than zero, "
                                + "excluding the value 999999999, unless it is a Staff Number Identifier and the first "
                                + "two positions are \"CS\" and the last seven positions are numeric. Nine-character "
                                + "SSN's should be left-justified, with a trailing blank.")),
                new FLValidationRule("SAJA", "3", new ValidateRegularExpression("Survey Period",
                        "^(2|3)$",
                        "Survey Period Code must be correct for the submission specified by the "
                                + "district and must be 2 or 3.")),
                new FLValidationRule("SAJA", "4", new ValidateFiscalYear()),
                new FLValidationRule("SAJA", "5", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "School Number, Primary/Home must exist on the Master School "
                                + "Identification File as a valid active school in the district of submission.")),
                // TODO: Rule #6
                new FLValidationRule("SAJA", "7", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.and(
                                                Restriction.pattern("Job Code FTE", "^\\d*$"),
                                                Restriction.lessThanOrEquals("Job Code FTE", Double.valueOf(100)))),
                                ValidationRule.testIf(Restriction.notEquals("Job Code FTE", Double.valueOf(0)))
                                        .testThen(Restriction.greaterThan("Job Code FTE", Double.valueOf(4)))),
                        "Job Code FTE must be numeric and less than or equal to 100. If Job Code "
                                + "FTE is not equal to zero, then it must be greater than 004.")),
                new FLValidationRule("SAJA", "8", new ValidateRegularExpression("Job Code Fund Source",
                        "^([BCEOGRSMNPQTU0]\\d{3}){3}$",
                        "Each of the three Job Code Fund Source codes must be one of the "
                                + "following: B, C, E, O, G, R, S, M, N, P, Q, T, U or zero.")),
                // TODO: Rule #9
                // Skipped: Rule #10 (Transaction code)
                new FLValidationRule("SAJA", "11", new ValidateRegularExpression("Qualified Paraprof.",
                        "^([A-D]|Z)$",
                        "Qualified Paraprofessional code must be A, B, C, D, or Z.")),
                new FLValidationRule("SAJA", "12", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Job Code Additional", "^5111[1-3]$"))
                                        .testThen(Restriction.pattern("Qualified Paraprof.", "^[A-D]$"))),
                        "Qualified Paraprofessional code must be A, B, C, or D for Job Codes 51111, "
                                + "51112 and 51113.")),
                new FLValidationRule("SAJA", "13", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Qualified Paraprof.", "^[A-D]$"))
                                        .testThen(Restriction.pattern("Job Code Additional", "^(51|52|53|54|55|59)\\S+$"))),
                        "If the Qualified Paraprofessional code is A, B, C, or D then the Job Code "
                                + "must begin with 51, 52, 53, 54, 55 or 59.")),
                // TODO: Rule #14
                new FLValidationRule("SAJA", "15", new ValidateRegularExpression("FL Education Id",
                        "^FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as "
                                + "\"FL\" in the first 2 positions followed by twelve numeric digits. No blanks or spaces "
                                + "are allowable.")),
                new FLValidationRule("SAJA", "30", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.notEquals("Job Code Fund Source", "000000000000"))),
                        "At least one of the three Job Code Fund Source codes must be nonzero.")),
                new FLValidationRule("SAJA", "31", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateJobCodeFundSource("Job Code Fund Source"))),
                        "Any one Job Code Fund Source code can appear only once on a Staff "
                                + "Additional Job Assignment record. For purposes of this edit, zero (used where "
                                + "there are fewer than three fund sources) is NOT treated as a Job Code Fund "
                                + "Source code.")),
                // TODO: Rule #32
                new FLValidationRule("SAJA", "33", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number", "SSN", "Survey Period",
                                                "Fiscal Year", "Job Code Additional"))),
                        "Each Staff Additional Job Assignment record must be unique based on "
                                + "District Number, Social Security Number, Survey Period Code, Fiscal Year, and "
                                + "Job Code, Additional.")),
                new FLValidationRule("SAJA", "34", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Job Code Additional", "^(51080|52080|53080|54080|55080|59080)$"))
                                        .testThen(Restriction.greaterThanOrEquals("Job Code FTE", Double.valueOf(0))),
                                ValidationRule.testIf(Restriction.pattern("Job Code Additional", "^(?!51080|52080|53080|54080|55080|59080).*$"))
                                        .testThen(Restriction.greaterThan("Job Code FTE", Double.valueOf(0)))),
                        "Job Code FTE may be equal to or greater than zero for substitute teachers "
                                + "(Job Code, Additional codes equal to 51080, 52080, 53080, 54080, 55080 or "
                                + "59080), but must be greater than zero for all other employees.")),
                new FLValidationRule("SAJA", "35", new ValidateRegularExpression("Staff ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "The Staff Number Identifier, Local may be any combination of letters, "
                                + "numbers and blanks. All blanks are not allowable. It must be left-justified with "
                                + "trailing blanks.")),
                new FLValidationRule("SAJA", "36", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.notEqualsFieldValue("Staff ID Local", "SSN", String.class))),
                        "The Staff Number Identifier, Local must not be identical to the Social Security Number.")),
                new FLValidationRule("SAJA", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STF",
                                        new KeyValuePair("SSN", "SSN"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Staff Additional Job Assignment record must have a matching Staff "
                                + "Demographic Information record based on District Number, Social Security "
                                + "Number, Survey Period Code, and Fiscal Year.")),
                // TODO: Rule #51 (Staff Payroll)
                // TODO: Rule #52 (Staff Payroll)
                new FLValidationRule("SAJA", "80", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Job Code Additional", "^(64021|64022|64023)$"))
                                        .testThen(Restriction.pattern("Job Code Fund Source", "^([RS].{3,11}|.{4}[RS].{3,7}|.{8}[RS].{3})$"))),
                        "If the Job Code, Additional is 64021, 64022 or 64023, then one of the Job "
                                + "Code Fund Source codes should be R or S.")),
        });
    }
}
