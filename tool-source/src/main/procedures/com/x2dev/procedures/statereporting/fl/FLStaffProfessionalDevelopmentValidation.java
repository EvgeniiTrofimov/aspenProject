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

import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * The Class FLStaffProfessionalDevelopmentValidation.
 */
public class FLStaffProfessionalDevelopmentValidation {

    /**
     * Generate date.
     *
     * @param month String
     * @param day String
     * @param year String
     * @return Date
     */
    protected static Date generateDate(String month, String day, String year) {
        Date date;

        try {
            SimpleDateFormat dateformat = new SimpleDateFormat("MMddyyyy");
            date = dateformat.parse(month + day + year);
        } catch (Exception e) {
            date = null;
        }

        return date;
    }

    /**
     * Instantiates a new FL staff professional development validation.
     */
    public FLStaffProfessionalDevelopmentValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SPD", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("SPD", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "School Number, Primary/Home must exist on the Master School "
                                + "Identification File as a valid active in the district of submission.")),
                new FLValidationRule("SPD", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("SSN", "^.{9}.*$"))
                                        .testThen(Restriction.pattern("SSN", "^.{9}\\s$")),
                                ValidationRule
                                        .testIf(Restriction.pattern("Staff ID Local", "^(?!CS\\d{7}\\s{0,1}).{1,10}$"))
                                        .testThen(Restriction.and(
                                                Restriction.pattern("SSN", "^\\d{1,10}$"),
                                                Restriction.greaterThan("SSN", Double.valueOf(0)),
                                                Restriction.notEquals("SSN", "999999999")))),
                        "Social Security Number (SSN) must be numeric and greater than zero, "
                                + "excluding the value 999999999, unless it is a Staff Number Identifier and the first "
                                + "two positions are \"CS\" and the last seven positions are numeric. Nine-character "
                                + "SSN's should be left-justified, with a trailing blank.")),
                new FLValidationRule("SPD", "4", new ValidateRegularExpression("Survey Period",
                        "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission "
                                + "specified by the district.")),
                new FLValidationRule("SPD", "5", new ValidateFiscalYear()),
                new FLValidationRule("SPD", "6", new ValidateRegularExpression("Learning Method",
                        "^(A|B|C|D|F|G|H|I|J|K)$",
                        "Professional Development, Learning Method must be A, B, C, D, F, G, H, I, J or K.")),
                new FLValidationRule("SPD", "7", new ValidateRegularExpression("Eval Method Staff",
                        "^[A-G]$",
                        "Professional Development, Evaluation Method, Staff must be A, B, C, D, E, F or G.")),
                new FLValidationRule("SPD", "8", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.and(
                                                Restriction.pattern("Participation Hours", "^\\d+$"),
                                                Restriction.greaterThan("Participation Hours", Double.valueOf(0))))),
                        "Professional Development, Participation Hours must be numeric, greater "
                                + "than zero (000) and contain no blanks.")),
                new FLValidationRule("SPD", "9", new ValidateRegularExpression("FL Education Id",
                        "^FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as "
                                + "\"FL\" in the first 2 positions followed by twelve numeric digits. No blanks or spaces "
                                + "are allowable.")),
                // Skipped: Rule #10 (Transaction code)
                new FLValidationRule("SPD", "11", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number", "SSN", "Survey Period",
                                                "Fiscal Year", "Component Number"))),
                        "Each Professional Development record must be unique based on District "
                                + "Number; Social Security Number; Survey Period Code; Fiscal Year and "
                                + "Professional Development, Component Number.")),
                new FLValidationRule("SPD", "12", new ValidateRegularExpression("Component Number",
                        "^[1-9](000|00[2-9]|01[0-7]|10[0-6]|20[0-9]|21[0-1]|30[0-8]|4[0-1][0-9]|42[0-4]|5[0-1][0-9]|520|60[0-2]|70[0-5]|80[0-5])(00[1-9]|[0-9][0-9][0-9])$",
                        "Position one of the Professional Development, Component Number must be "
                                + "1-9. Positions two, three and four must be 000, 002-017, 100-106, 200-211, 300-308, "
                                + "400-424, 500-520, 600-602, 700-705 or 800-805. Positions five, six and seven must "
                                + "be 001-999.")),
                new FLValidationRule("SPD", "13", new ValidateRegularExpression("Implementation Methd",
                        "^[M-T]$",
                        "Professional Development, Implementation Method must be M, N, O, P, Q, R, S or T.")),
                new FLValidationRule("SPD", "14", new ValidateRegularExpression("PD District Number",
                        "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|99)$",
                        "District Number, Where Professional Development Completed must be "
                                + "numeric in the range 01-68, 71-75 or 99.")),
                new FLValidationRule("SPD", "15", new ValidateRegularExpression("Primary Purpose",
                        "^[A-H]$",
                        "Professional Development Credits, Primary Purpose must be A, B, C, D, E, F, G or H.")),
                new FLValidationRule("SPD", "16", new ValidateRegularExpression("Eval Method Student",
                        "^([A-G]|Z)$",
                        "Professional Development, Evaluation Method, Student must be A, B, C, D, F, G or Z.")),
                new FLValidationRule("SPD", "17", new ValidateRegularExpression("Staff ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "The Staff Number Identifier, Local may be any combination of letters, "
                                + "numbers and blanks. All blanks are not allowable. It must be left-justified with "
                                + "trailing blanks.")),
                new FLValidationRule("SPD", "18", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.notEqualsFieldValue("Staff ID Local", "SSN",
                                                String.class))),
                        "The Staff Number Identifier, Local must not be identical to the Social Security Number.")),
                new FLValidationRule("SPD", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STF",
                                        new KeyValuePair("SSN", "SSN"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Professional Development record must have a matching Staff "
                                + "Demographic Information record based on District Number, Social Security Number, "
                                + "Survey Period Code and Fiscal Year.")),
        });
    }
}
