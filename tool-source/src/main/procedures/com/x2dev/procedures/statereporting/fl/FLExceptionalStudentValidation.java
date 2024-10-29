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
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateExportsComparison;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidateExportsComparison.Operator;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The Class FLExceptionalStudentValidation.
 */
public class FLExceptionalStudentValidation {


    /**
     * Instantiates a new FL exceptional student validation.
     */
    public FLExceptionalStudentValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("EXCEPT", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("EXCEPT", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-5]$"))
                                        .testThen(Restriction.pattern("School Number",
                                                "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$"))),
                        "If Survey Period Code is 1-4 or 5, School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("EXCEPT", "3", new FLValidationRuleSet(
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
                new FLValidationRule("EXCEPT", "4", new ValidateRegularExpression(
                        "Survey Period", "^[1-5]$", "Survey Period Code must be 1, 2, 3, 4 or 5.")),
                new FLValidationRule("EXCEPT", "5", new ValidateFiscalYear()),
                // Rule #6 skipped (Transaction Code)
                new FLValidationRule("EXCEPT", "7", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                        .testThen(Restriction.uniqueValue("Student Number", "District Number",
                                                "Fiscal Year"))),
                        "Each Exceptional Student record must be unique based on the keys of District Number, "
                                + "Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Year.")),
                new FLValidationRule("EXCEPT", "8", new ValidateRegularExpression(
                        "Exceptionality Other", "^(([C-M]|O|P|[S-W])+|(Z))[Z]+$",
                        "Exceptionality, Other must be C-M, O, P, S-W, or Z.")),
                // TODO: Rule #9 "...within the fiscal year"
                new FLValidationRule("EXCEPT", "9", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                        Restriction.notEquals("Except Primary", "L"))
                                        .testThen(Restriction.byDateFormat("MMddyyyy", "Plan Date")),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                        Restriction.equals("Except Primary", "L"))
                                        .testThen(Restriction.byDateFormat("MMddyyyy", "Plan Date")),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.notEquals("Except Primary", "L"))
                                        .testThen(Restriction.or(Restriction.byDateFormat("MMddyyyy", "Plan Date"),
                                                Restriction.equals("Plan Date", "00000000")))),
                        "If Survey Period Code = 1, 2, 3 or 4, and if Exceptionality, Primary is not L, "
                                + "then the Exceptional Student Plan Date must be a valid date within the current or "
                                + "previous fiscal year. If Survey Period Code = 1, 2, 3 or 4, and if Exceptionality, "
                                + "Primary is L, then the Exceptional Student Plan Date must be a valid date within "
                                + "the current or four previous fiscal years. If Survey Period Code = 5, Exceptional "
                                + "Student Plan Date may either be zeroes or a valid date.")),
                new FLValidationRule("EXCEPT", "10", new ValidateRegularExpression(
                        "Except Primary", "^(C|[F-M]|[O-P]|[S-W]|Z)$",
                        "Exceptionality, Primary must be C, F-M, O-P, S-W, or Z.")),
                new FLValidationRule("EXCEPT", "11", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                        Restriction.notEquals("Except Primary", "L"),
                                        Restriction.notEquals("Exceptionality Other", "ZZZZZZZZZ"))
                                        .testThen(Restriction.greaterThan("Total School Week", Double.valueOf(0.0))),
                                ValidationRule.testIf(Restriction.notEquals("Survey Period", "2"))
                                        .testThen(Restriction.equals("Total School Week", "0000"))),
                        "If Survey Period Code = 2, Time, Total School Week must be numeric and "
                                + "must be greater than zero unless Exceptionality, Primary code is L and "
                                + "Exceptionality, Other is Z-filled, in which case Time, Total School Week may be "
                                + "zero. If survey does not equal 2, then, Time, Total School Week must be 0000.")),
                new FLValidationRule("EXCEPT", "12", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                        Restriction.or(Restriction.notEquals("Except Primary", "L"),
                                                Restriction.notEquals("Exceptionality Other", "LZZZZZZZZ")))
                                        .testThen(Restriction.greaterThanOrEquals("Non-Disabled Peers T",
                                                Double.valueOf(0.0))),
                                ValidationRule.testIf(Restriction.or(Restriction.notEquals("Survey Period", "2"),
                                        Restriction.and(Restriction.equals("Except Primary", "L"),
                                                Restriction.equals("Exceptionality Other", "ZZZZZZZZZ"))))
                                        .testThen(Restriction.equals("Non-Disabled Peers T", "0000"))),
                        "If Survey Period Code = 2 and there is a code other than L in either "
                                + "Exceptionality, Primary or Exceptionality, Other; then Time With Non-Disabled "
                                + "Peers must be numeric and must be greater than or equal to zero. If Survey "
                                + "Period Code is not 2 or if Exceptionality, Primary code is L and Exceptionality, "
                                + "Other is Z-filled; then Time With Non-Disabled Peers must be 0000.")),
                new FLValidationRule("EXCEPT", "13", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("EXCEPT", "14", new ValidateRegularExpression("Exceptionality Other",
                        "(^Z[Z]{8}$)|(^[^Z][\\s\\S]{8}$)",
                        "If the first character in the field for Exceptionality, Other is a Z, then all "
                                + "codes for the remaining eight fields must be Z.")),
                new FLValidationRule("EXCEPT", "15", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                        .testThen(Restriction.lessThanOrEqualsFieldValue("Non-Disabled Peers T",
                                                "Total School Week", Double.class))),
                        "If Survey Period Code = 2, Time With Non-Disabled Peers must be less than "
                                + "or equal to Time, Total School Week.")),
                new FLValidationRule("EXCEPT", "16", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Except Primary", "Z"))
                                        .testThen(Restriction.notContainsFieldValue("Exceptionality Other",
                                                "Except Primary"))),
                        "If Exceptionality, Primary is not Z; then Exceptionality, Other codes may not "
                                + "include the Exceptionality, Primary code.")),
                new FLValidationRule("EXCEPT", "17", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Exceptionality Other", "[C-M]|O|P|[S-W]"))
                                        .testThen(Restriction.pattern("Exceptionality Other",
                                                "^([C-M]|O|P|[S-W])+[Z]+$"))),
                        "If any Exceptionality, Other codes C-M, O, P or S-W are reported, the codes "
                                + "must be left-justified and the remaining fields Z-filled.")),
                new FLValidationRule("EXCEPT", "18", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                        .testThen(Restriction.pattern("IDEA Educational Env",
                                                "^[A|B|C|D|F|H|J|K|L|M|P|S|Z]$")),
                                ValidationRule.testIf(Restriction.notEquals("Survey Period", "2"))
                                        .testThen(Restriction.equals("IDEA Educational Env", "Z"))),
                        "For Survey Period Code = 2, Exceptional Student, IDEA Educational "
                                + "Environments must be A, B, C, D, F, H, J, K, L, M, P, S, or Z. For all other surveys "
                                + "this code must be Z.")),
                new FLValidationRule("EXCEPT", "19", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Except Primary", "L"),
                                        Restriction.equals("Exceptionality Other", "ZZZZZZZZZ"))
                                        .testThen(Restriction.equals("IDEA Educational Env",
                                                "Z"))),
                        "If the Exceptionality, Primary code = L, and the Exceptionality, Other code "
                                + "is Z-filled, then the Exceptional Student, IDEA Educational Environments code "
                                + "must be Z.")),
                new FLValidationRule("EXCEPT", "20", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(Restriction.notEquals("School Number", "N998"),
                                        Restriction.notEquals("School Number", "N999")))
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "If School Number, Current Enrollment is not N998 or N999, then the School "
                                + "Number, Current Enrollment must exist on the Master School Identification File "
                                + "as a valid active school number in the district of enrollment.")),
                new FLValidationRule("EXCEPT", "21", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]|5$"))
                                        .testThen(Restriction.or(Restriction.byDateFormat("MMddyyyy", "Dismissal Date"),
                                                Restriction.equals("Dismissal Date", "00000000")))),
                        "If Survey Period Code is 1-4 or 5, Exceptional Student, Dismissal Date must "
                                + "be numeric and must be a valid date or zeroes.")),
                new FLValidationRule("EXCEPT", "22", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"))
                                        .testThen(Restriction.pattern("Alt Assessment Adm", "^[D|P|Z]$")),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!2|3)[\\s\\S]$"))
                                        .testThen(Restriction.pattern("Alt Assessment Adm", "^Z$"))),
                        "If Survey Period Code = 2 or 3, Alternate Assessment Administered must be "
                                + "D, P or Z. For all other Survey Periods the Alternate Assessment Administered code "
                                + "must be Z.")),
                new FLValidationRule("EXCEPT", "23", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"),
                                        Restriction.equals("Except Primary", "L"),
                                        Restriction.equals("Exceptionality Other", "ZZZZZZZZZ"))
                                        .testThen(Restriction.pattern("Alt Assessment Adm", "^Z$"))),
                        "For Survey Period Code = 2 or 3, if the Exceptionality, Primary code = L, "
                                + "and the Exceptionality, Other code is Z-filled, then the Alternate Assessment "
                                + "Administered code must be Z.")),
                new FLValidationRule("EXCEPT", "26", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-5]$"))
                                        .testThen(Restriction.pattern("Gifted Eligibility", "^[A|B|Z]$"))),
                        "If Survey Period Code is 1-5 then Gifted Eligibility code must be A, B, or Z.")),
                new FLValidationRule("EXCEPT", "27", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("IDEA Educational Env", "^[C|D|F|H|P]$"))
                                        .testThen(
                                                Restriction.and(Restriction.pattern("Except Primary", "^((?!T|U).)+$"),
                                                        Restriction.pattern("Exceptionality Other", "^((?!T|U).)+$")))),
                        "If the Exceptional Student, IDEA Educational Environments code equals C, "
                                + "D, F, H, or P then the Exceptionality, Primary code and the Exceptionality, Other "
                                + "codes must not equal T or U.")),
                new FLValidationRule("EXCEPT", "28", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Gifted Eligibility", "^[A|B]$"))
                                        .testThen(Restriction.or(Restriction.equals("Except Primary", "L"),
                                                Restriction.equals("Exceptionality Other", "LZZZZZZZZ")))),
                        "If Gifted Eligibility = A or B; then Exceptionality, Primary or Exceptionality, "
                                + "Other must = L.")),
                new FLValidationRule("EXCEPT", "29", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(Restriction.equals("Except Primary", "U"),
                                        Restriction.equals("Exceptionality Other", "UZZZZZZZZ")))
                                        .testThen(Restriction.equals("IDEA Educational Env", "Z"))),
                        "If the Exceptionality, Primary code or the Exceptionality, Other code equals "
                                + "U, then the Exceptional Student, IDEA Educational Environments code must be Z.")),
                new FLValidationRule("EXCEPT", "2A", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Exceptionality Other", "ZZZZZZZZZ"))
                                        .testThen(Restriction.uniqueValue("Exceptionality Other"))),
                        "Exceptionality, Other must be unique other than Z.")),
                new FLValidationRule("EXCEPT", "2B", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2-3]$"),
                                        Restriction.notEquals("School Number", "N999"))
                                        .testThen(
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "4"),
                                        Restriction.notEquals("School Number", "N999"))
                                        .testThen(
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06",
                                                                        "30")))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[P|T]$"),
                                        Restriction.notEquals("School Number", "N999"))
                                        .testThen(
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08",
                                                                        "31")))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[R|E|I|N]$"))
                                        .testThen(Restriction.equals("Placement Date", "00000000")),
                                ValidationRule.testIf(Restriction.and(
                                        Restriction.or(Restriction.equals("Survey Period", "1"),
                                                Restriction.equals("School Number", "N999")),
                                        Restriction.notEquals("Placement Date", "00000000")))
                                        .testThen(Restriction.or(
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06",
                                                                        "30"))),
                                                Restriction.and(Restriction.byDateFormat("MMddyyyy", "Placement Date"),
                                                        Restriction.lessThanOrEquals("Placement Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08",
                                                                        "31")))))),
                        "For Survey Periods 2 and 3, if School Number, Current Enrollment is not "
                                + "N999, then Exceptional Student Placement Date must be a valid numeric date and "
                                + "less than or equal to the survey date. "
                                + "For Survey Period 4, if School Number, Current Enrollment is not N999, then "
                                + "Exceptional Student Placement Date must be a valid numeric date and less than "
                                + "or equal to June 30 of the current school year. "
                                + "For Survey 5, if Exceptional Student Placement Status = P or T and School "
                                + "Number, Current Enrollment is not N999, then Exceptional Student Placement "
                                + "Date must be a valid numeric date less than August 31 following the 180 day "
                                + "school year. For Survey 5, if Exceptional Student Placement Status = R, E, I, or N, "
                                + "Exceptional Student Placement Date must be zero-filled. "
                                + "If School Number, Current Enrollment is N999 or if Survey = 1 then the "
                                + "Exceptional Student Placement Date must either follow the rules above or it must "
                                + "be zero-filled")),
                new FLValidationRule("EXCEPT", "2C", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(
                                                Restriction.or(Restriction.byDateFormat("MMddyyyy", "Elig Determ Date"),
                                                        Restriction.equals("Elig Determ Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.equals("Placement Status", "T"))
                                        .testThen(
                                                Restriction.or(Restriction.byDateFormat("MMddyyyy", "Elig Determ Date"),
                                                        Restriction.lessThanOrEquals("Elig Determ Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")),
                                                        Restriction.equals("Elig Determ Date", "00000000"))),
                                ValidationRule.testIf(Restriction.notEquals("Survey Period", "5"),
                                        Restriction.equals("Placement Status", "T"))
                                        .testThen(
                                                Restriction.or(Restriction.byDateFormat("MMddyyyy", "Elig Determ Date"),
                                                        Restriction.lessThanOrEquals("Elig Determ Date",
                                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                        Restriction.equals("Elig Determ Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "1"),
                                        Restriction.equals("Placement Status", "P"))
                                        .testThen(
                                                Restriction.or(Restriction.byDateFormat("MMddyyyy", "Elig Determ Date"),
                                                        Restriction.lessThanOrEquals("Elig Determ Date",
                                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                        Restriction.equals("Elig Determ Date", "00000000"))),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2-4]$"),
                                        Restriction.equals("Placement Status", "P"))
                                        .testThen(Restriction.lessThanOrEquals("Elig Determ Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[R|E]$"))
                                        .testThen(Restriction.equals("Elig Determ Date", "00000000")),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[I|N|P]$"))
                                        .testThen(Restriction.and(
                                                Restriction.byDateFormat("MMddyyyy", "Elig Determ Date"),
                                                Restriction.lessThanOrEquals("Elig Determ Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "Exceptional Student Eligibility Determination Date must be numeric and "
                                + "either a valid date or all zeros. "
                                + "If Exceptional Student Placement Status = T, then Exceptional Student Eligibility "
                                + "Determination Date must be a valid date that is less than or equal to the survey "
                                + "date or it must be zero-filled. For Survey 5, use August 31 to represent the survey "
                                + "date. "
                                + "If Survey Period is 1 and Exceptional Student Placement Status = P, then "
                                + "Exceptional Student Eligibility Determination Date must be less than or equal to "
                                + "the survey date or zero-filled. "
                                + "If Survey Period is 2, 3 or 4 and Exceptional Student Placement Status = P, then "
                                + "Exceptional Student Eligibility Determination Date must be less than or equal to "
                                + "the survey date. "
                                + "If Survey Period = 5 and if Exceptional Student Placement Status = R or E then "
                                + "the Exceptional Student Eligibility Determination Date must be zero-filled. "
                                + "If Survey Period = 5 and if Exceptional Student Placement Status = I, N or P, then "
                                + "the Exceptional Student Eligibility Determinate Date must be a valid date less "
                                + "than or equal to August 31.")),
                new FLValidationRule("EXCEPT", "2D", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                        .testThen(Restriction.pattern("Placement Status",
                                                "^[P|T]$")),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.pattern("Placement Status",
                                                "^[R|E|P|I|N|T]$"))),
                        "If Survey Period Code is 1, 2, 3 or 4, then Exceptional Student Placement "
                                + "Status must be P or T. "
                                + "In Survey Period 5, Exceptional Student Placement Status must be R, E, P, I, N, or T.")),
                new FLValidationRule("EXCEPT", "2E", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[R|E|I|N]$"))
                                        .testThen(Restriction.equals("Except Primary",
                                                "Z"))),
                        "If Survey Period is 5 and if Exceptional Student Placement Status is R, E, I "
                                + "or N, then Exceptionality, Primary must be Z.")),
                new FLValidationRule("EXCEPT", "2F", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2-4]$"),
                                        Restriction.pattern("Placement Status", "^[E|P|I|N]$"))
                                        .testThen(Restriction.and(
                                                Restriction.byDateFormat("MMddyyyy", "Eval Completion Date"),
                                                Restriction.lessThanOrEquals("Eval Completion Date",
                                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN)))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.pattern("Placement Status", "^[E|P|I|N]$"))
                                        .testThen(Restriction.and(
                                                Restriction.byDateFormat("MMddyyyy", "Eval Completion Date"),
                                                Restriction.lessThan("Eval Completion Date",
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "1"),
                                        Restriction.pattern("Placement Status", "^[E|P|I|N]$"))
                                        .testThen(Restriction.or(Restriction.and(
                                                Restriction.byDateFormat("MMddyyyy", "Eval Completion Date"),
                                                Restriction.lessThanOrEquals("Eval Completion Date",
                                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                                                Restriction.equals("Eval Completion Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Placement Status", "R"))
                                        .testThen(Restriction.equals("Eval Completion Date", "00000000")),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                        Restriction.equals("Placement Status", "T"))
                                        .testThen(Restriction.or(Restriction.equals("Eval Completion Date", "00000000"),
                                                Restriction.and(
                                                        Restriction.byDateFormat("MMddyyyy", "Eval Completion Date"),
                                                        Restriction.lessThanOrEquals("Eval Completion Date",
                                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN))))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.equals("Placement Status", "T"))
                                        .testThen(Restriction.or(Restriction.equals("Eval Completion Date", "00000000"),
                                                Restriction.and(
                                                        Restriction.byDateFormat("MMddyyyy", "Eval Completion Date"),
                                                        Restriction.lessThan("Eval Completion Date",
                                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08",
                                                                        "31")))))),
                        "Evaluation Completion Date must be numeric. If Exceptional Student "
                                + "Placement Status = E, P, I or N and Survey Period does not equal 1, then "
                                + "Evaluation Completion Date must be a valid date and less than or equal to the "
                                + "survey date (for survey periods 2, 3 and 4) or less than August 31 following the "
                                + "180 day school year (for survey period 5). "
                                + "If Exceptional Student Placement Status = E, P, I or N and Survey Period is 1, then "
                                + "Evaluation Completion Date must be a valid date and less than or equal to the "
                                + "survey date or it must be zero-filled. "
                                + "If Exceptional Student Placement Status = R, then Evaluation Completion Date "
                                + "must be zero-filled. If Exceptional Student Placement Status = T, then Evaluation "
                                + "Completion Date must be zero-filled or a valid date that is less than or equal to "
                                + "the survey date (for survey periods 1, 2, 3 and 4) or less than August 31 following "
                                + "the 180 day school year (for survey period 5).")),
                new FLValidationRule("EXCEPT", "2G", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Date of Consent (EV)", "00000000"))
                                        .testThen(Restriction.lessThan("Date of Consent (EV)",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                        "Date of Consent for Evaluation must be a valid date and less than August "
                                + "31 at the start of the next school year or it must be zero filled.")),
                new FLValidationRule("EXCEPT", "2H", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.pattern("Referral Reason",
                                                "^[D|G|M|Z]$")),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                        .testThen(Restriction.equals("Referral Reason",
                                                "Z"))),
                        "If Survey Period Code is 5, Exceptional Student Referral Reason must be D, "
                                + "G, M or Z. If Survey Period Code is 1, 2, 3 or 4, Exceptional Student Referral "
                                + "Reason must be Z.")),
                new FLValidationRule("EXCEPT", "2I", new ValidateRegularExpression("60d Except/Extension",
                        "^(N|P|T|Y)$",
                        "Exceptional Student, 60-Day Exception/Extension must be N, P, T or Y.")),
                new FLValidationRule("EXCEPT", "2J", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.equals("Except Primary", "Z"))
                                        .testThen(Restriction.pattern("Placement Status",
                                                "^[R|E|I|N]$"))),
                        "If Exceptionality, Primary is Z and Survey Period is 5, then Exceptional "
                                + "Student Placement Status must be R, E, I or N.")),
                new FLValidationRule("EXCEPT", "2K", new ValidateRegularExpression("FL Education ID",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable.")),
                new FLValidationRule("EXCEPT", "2L", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                        .testThen(Restriction.notEquals("Except Primary",
                                                "Z"))),
                        "If Survey Period is 1, 2, 3, or 4, then Exceptionality, Primary must not be Z.")),
                new FLValidationRule("EXCEPT", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "For Survey Periods 1 - 4, each Exceptional Student record must have a "
                                + "matching Student Demographic record based on District Number, Current "
                                + "Enrollment; School Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code and Year. For Survey Period 5, each Exceptional "
                                + "Student record must have a matching Student Demographic record based on "
                                + "District Number, Current Enrollment; Student Number Identifier, Florida; Survey "
                                + "Period Code and Year.")),
                new FLValidationRule("EXCEPT", "31", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.equals("Survey Period", "2"),
                                        Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(3),
                                                Integer.valueOf(5), true, true),
                                        Restriction.or(
                                                Restriction.pattern("Except Primary", "^((?!L|Z).)*$"),
                                                Restriction.pattern("Exceptionality Other", "^((?!L|Z).)*$")))
                                        .testThen(Restriction.pattern("IDEA Educational Env",
                                                "^[A|B|J|K|L|M|S]$")),
                                ValidationRule.testIf(
                                        Restriction.equals("Survey Period", "2"),
                                        Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(3),
                                                Integer.valueOf(5), true, true),
                                        Restriction.or(
                                                Restriction.equals("Except Primary", "Z"),
                                                Restriction.equals("Exceptionality Other", "ZZZZZZZZZ")))
                                        .testThen(Restriction.equals("IDEA Educational Env", "Z"))),
                        "For Survey Period Code = 2, if the student is age 3-5 and if Exceptionality, "
                                + "Primary or Exceptionality, Other contains a code other than L or Z, then the "
                                + "Exceptional Student, IDEA Educational Environments code must be A, B, J, K, L, "
                                + "M or S. If Survey Period Code is 2 and if the student is age 3-5 and if "
                                + "Exceptionality, Primary equals L and Exceptionality, Other is all Z's, then "
                                + "Exceptional Student, IDEA Educational Environments code must be Z. Age is "
                                + "determined using the student'€™s Birth Date on the Student Demographic "
                                + "Information record and the Friday of Survey Week.")),
                new FLValidationRule("EXCEPT", "32", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                        Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(6),
                                                Integer.valueOf(21), true, true))
                                        .testThen(Restriction.pattern("IDEA Educational Env",
                                                "^[C|D|F|H|P|Z]$"))),
                        "For Survey Period Code = 2, if the student is age 6-21 then the Exceptional "
                                + "Student, IDEA Educational Environments code must be C, D, F, H, P or Z.")),
                new FLValidationRule("EXCEPT", "33", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                        Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(0),
                                                Integer.valueOf(2), true, true))
                                        .testThen(Restriction.equals("IDEA Educational Env", "Z"))),
                        "For Survey Period Code = 2, if the student is age 0-2, the Exceptional "
                                + "Student, IDEA Educational Environments code must be Z.")),
                new FLValidationRule("EXCEPT", "34", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair(Pattern.compile("^[PK]$"), "Grade Level")))
                                        .testThen(Restriction.and(Restriction.pattern("Except Primary", "^((?!L).)*$"),
                                                Restriction.pattern("Exceptionality Other", "^((?!L).)*$")))),
                        "If Grade Level = PK on the Student Demographic Information format, then "
                                + "neither Exceptionality, Primary nor Exceptionality, Other may equal L.")),
                // TODO: For rule #38 survey week was not used
                new FLValidationRule("EXCEPT", "38", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.pattern("Survey Period", "^[1-4]$"),
                                        Restriction.or(
                                                Restriction.equals("Except Primary", "U"),
                                                Restriction.equals("Exceptionality Other", "U")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair(Pattern.compile("^((?!A|B).)*$"),
                                                        "Year-Round Indicator")))
                                        .testThen(
                                                Restriction.studentAgeInRange("Student ID Local", null, Integer.valueOf(3),
                                                        false, false))),
                        "If Survey Period Code = 1-4 and if either Exceptionality, Primary is U or "
                                + "Exceptionality, Other is U, age must be less than three as of the first day of "
                                + "survey week; unless Year-Round/Extended School Year FTE Indicator = A or B.")),
                // TODO: For rule #39 survey week was not used
                new FLValidationRule("EXCEPT", "39", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.pattern("Survey Period", "^[1-4]$"),
                                        Restriction.or(
                                                Restriction.equals("Except Primary", "T"),
                                                Restriction.equals("Exceptionality Other", "T")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair(Pattern.compile("^((?!A|B).)*$"),
                                                        "Year-Round Indicator")))
                                        .testThen(
                                                Restriction.studentAgeInRange("Student ID Local", null, Integer.valueOf(6),
                                                        false, false))),
                        "If Survey Period Code = 1-4 and if either Exceptionality, Primary is T or "
                                + "Exceptionality, Other is T, age must be less than six as of the first day of survey "
                                + "week; unless Year-Round/Extended School Year FTE Indicator = A or B.")),
                new FLValidationRule("EXCEPT", "3A", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Placement Date", "00000000"))
                                        .testThen(Restriction.validateExportsComparison("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new ValidateExportsComparison.ComparableFields("Placement Date",
                                                        "Birth Date", Operator.GREATER_THAN, Date.class))),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number, E"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Exceptional Student Placement Date is not zeros, then Exceptional "
                                + "Student Placement Date must be greater than Birth Date on the Student "
                                + "Demographic Information record. For Survey Periods 1 - 4, the records should be "
                                + "matched to the Student Demographic Information record based on District "
                                + "Number, Current Enrollment; School Number, Current Enrollment; Student "
                                + "Number Identifier, Florida; Survey Period Code and Year. For Survey Period 5, the "
                                + "records should be matched to the Student Demographic Information record "
                                + "based on District Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code and Year.")),
                new FLValidationRule("EXCEPT", "3B", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Placement Status", "P"),
                                        Restriction.notEquals("Elig Determ Date", "00000000"))
                                        .testThen(Restriction.validateExportsComparison("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new ValidateExportsComparison.ComparableFields("Elig Determ Date",
                                                        "Birth Date", Operator.GREATER_THAN, Date.class)))),
                        "If Exceptional Student Placement Status = P and if Exceptional Student "
                                + "Eligibility Determination Date is not zero-filled, Exceptional Student Eligibility "
                                + "Determination Date must be greater than Birth Date. For Survey Periods 1 - 4, the "
                                + "records should be matched to the Student Demographic Information record "
                                + "based on District Number, Current Enrollment; School Number, Current "
                                + "Enrollment; Student Number Identifier, Florida; Survey Period Code and Year. "
                                + "For Survey Period 5, the records should be matched to the Student Demographic "
                                + "Information record based on District Number, Current Enrollment; Student "
                                + "Number Identifier, Florida; Survey Period Code and Year.")),
                new FLValidationRule("EXCEPT", "3C", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Placement Status", "P"),
                                        Restriction.notEquals("Eval Completion Date", "00000000"))
                                        .testThen(Restriction.validateExportsComparison("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new ValidateExportsComparison.ComparableFields("Eval Completion Date",
                                                        "Birth Date", Operator.GREATER_THAN, Date.class)))),
                        "If Exceptional Student Placement Status = P and Evaluation Completion "
                                + "Date is not zero-filled, then Evaluation Completion Date must be greater than "
                                + "Birth Date. For Survey Periods 1 - 4, the records should be matched to the "
                                + "Student Demographic Information record based on District Number, Current "
                                + "Enrollment; School Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code and Year. For Survey Period 5, the records should be "
                                + "matched to the Student Demographic Information record based on District "
                                + "Number, Current Enrollment; Student Number Identifier, Florida; Survey Period "
                                + "Code and Year.")),
                new FLValidationRule("EXCEPT", "41", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Placement Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Placement Date",
                                                "Elig Determ Date", Date.class))),
                        "If Exceptional Student Placement Date is not zeros then Exceptional "
                                + "Student Placement Date must be greater than or equal to Exceptional Student "
                                + "Eligibility Determination Date.")),
                new FLValidationRule("EXCEPT", "42", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Placement Status", "P"))
                                        .testThen(Restriction.lessThanOrEqualsFieldValue("Eval Completion Date",
                                                "Elig Determ Date", Date.class))),
                        "If Exceptional Student Placement Status = P, Evaluation Completion Date "
                                + "must be prior to (less than) or equal to Exceptional Student Eligibility "
                                + "Determination Date.")),
                new FLValidationRule("EXCEPT", "43", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.equals("Placement Status", "P"),
                                        Restriction.notEquals("Date of Consent (EV)", "00000000"))
                                        .testThen(Restriction.lessThanOrEqualsFieldValue("Date of Consent (EV)",
                                                "Eval Completion Date", Date.class))),
                        "If Exceptional Student Placement Status = P and Date of Consent for "
                                + "Evaluation is not zero-filled, then Date of Consent for Evaluation must be prior to "
                                + "(less than) or equal to Evaluation Completion Date.")),
                new FLValidationRule("EXCEPT", "44", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(
                                Restriction.notEquals("Dismissal Date", "00000000"))
                                .testThen(Restriction.pattern("Dismissal Date", "^0[7|8](0[1-9]|1[0-6])\\d{4}$"))),
                        "If Exceptional Student, Dismissal Date is a valid date, then it must be in the "
                                + "range 07/01/**** to 08/16/****.")),
                // TODO: Rule #80
        });
    }
}
