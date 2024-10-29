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
import java.util.regex.Pattern;


/**
 * The Class FLStudentAdditionalFundingValidation.
 */
public class FLStudentAdditionalFundingValidation {

    /**
     * Instantiates a new FL student additional funding validation.
     */
    public FLStudentAdditionalFundingValidation() {
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("FTE", "1", new ValidateDistrictNumber("District Number CIS")),
                new FLValidationRule("FTE", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("School Number IS",
                                                "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$"))),
                        "School Number, Current Instruction/Service must be numeric in the range "
                                + "0001 to 9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("FTE", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number FL", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number FL", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number FL", "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number FL", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number FL", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be "
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be "
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is "
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an "
                                + "\"X\", the first three positions may not all be zeroes.")),
                new FLValidationRule("FTE", "4", new ValidateRegularExpression(
                        "Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission"
                                + "specified by the district.")),
                new FLValidationRule("FTE", "5", new ValidateFiscalYear()),
                // Skipped: Rule #6 skipped (Transaction Code)
                new FLValidationRule("FTE", "7", new ValidateRegularExpression(
                        "FTE CEEB AP Test",
                        "^(0.00|0.16|0.32|0.48|0.64|0.80|0.96|1.12|1.28|1.44|1.60|1.76|1.92|2.08|2.24|2.40)$",
                        "FTE Earned, College Entrance Examination Board (CEEB) Advanced "
                                + "Placement Test must be numeric and must equal 0.00, 0.16, 0.32, 0.48, 0.64, 0.80, "
                                + "0.96, 1.12, 1.28, 1.44, 1.60, 1.76, 1.92, 2.08, 2.24 or 2.40.")),
                new FLValidationRule("FTE", "8", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("FTE", "9", new ValidateRegularExpression(
                        "FTE IB Diploma", "^(0.00|0.30)$",
                        "FTE Earned, International Baccalaureate Diploma must be 0.00 or 0.30.")),
                new FLValidationRule("FTE", "10", new ValidateRegularExpression(
                        "FTE IB Scope", "^(0.00|0.16|0.32|0.48|0.64|0.80|0.96|1.12)$",
                        "FTE Earned, International Baccalaureate Score must be numeric and must "
                                + "equal 0.00, 0.16, 0.32, 0.48, 0.64, 0.80, 0.96, or 1.12.")),
                new FLValidationRule("FTE", "11", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byActiveSchool("School Number IS"))),
                        "The School Number, Current Instruction/Service must exist on the Master "
                                + "School Identification File as a valid school number in the district of "
                                + "Instruction/Service.")),
                new FLValidationRule("FTE", "12", new ValidateRegularExpression(
                        "FTE AICE Diploma", "^(0.00|0.30)$",
                        "FTE Earned, Advance International Certificate of Education Diploma must be 0.00 or 0.30.")),
                new FLValidationRule("FTE", "13", new ValidateRegularExpression(
                        "FTE AICE Score",
                        "^(0.00|0.08|0.16|0.24|0.32|0.40|0.48|0.56|0.64|0.72|0.80|0.88|0.96|1.04|1.12)$",
                        "FTE Earned, Advanced International Certificate of Education Score must be "
                                + "0.00, 0.08, 0.16, 0.24, 0.32, 0.40, 0.48, 0.56, 0.64, 0.72, 0.80, 0.88, 0.96, 1.04, or 1.12.")),
                new FLValidationRule("FTE", "15", new ValidateRegularExpression(
                        "FTE Early Graduates", "^(0.0000|0.2500|0.5000)$",
                        "FTE Earned, Early Graduates must be numeric and must be equal to 0.0000, 0.2500, or 0.5000.")),
                new FLValidationRule("FTE", "16", new ValidateRegularExpression("Florida Education ID",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable.")),
                new FLValidationRule("FTE", "17", new ValidateRegularExpression("District Number CE",
                        "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5])$",
                        "District Number, Current Enrollment must be numeric and in the range 01-68 or 71-75.")),
                new FLValidationRule("FTE", "18", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number CIS", "School Number IS",
                                                "Student Number FL", "Survey Period", "Fiscal Year"))),
                        "Each Student Additional Funding record must be unique based on District "
                                + "Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Survey Period Code and School Year.")),
                new FLValidationRule("FTE", "19", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("FTE Early Graduates", Double.valueOf(0)))
                                        .testThen(Restriction.equalsFieldValue("District Number CE", "District Number CIS", String.class))),
                        "If FTE Earned, Early Graduates is greater than zero, then District Number, "
                                + "Current Enrollment must match District Number, Current "
                                + "Instruction/Service")),
                new FLValidationRule("FTE", "40", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("FTE Early Graduates", Double.valueOf(0)))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number FL", "Student Number"),
                                                new KeyValuePair("District Number CE", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^(W06|WFT|WRW)$"), "Diploma Type")))),
                        "If FTE Earned, Early graduates is greater than zero, then a Student End of "
                                + "Year Status record must exist and the Diploma Type on the Student End of Year "
                                + "Status record must be W06, WFT, or WRW. The records should match based on "
                                + "District Number, Current Enrollment; Student Number Identifier, Florida; Survey "
                                + "Period, and Year.")),
                new FLValidationRule("FTE", "41", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number FL", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, I/S"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Student Additional Funding record must have a matching Student "
                                + "Demographic record based on District Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Survey Period Code and School Year.")),
                // TODO: Rule #60
                // TODO: Rule #61
                // TODO: Rule #62
        });
    }
}
