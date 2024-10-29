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
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateStudentNumber;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The Class FLEnglishLanguageLearnersValidation.
 */
public class FLEnglishLanguageLearnersValidation {

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
     * Instantiates a new FL english language learners validation.
     */
    public FLEnglishLanguageLearnersValidation() {
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("ELL", "1", new ValidateDistrictNumber("District Number CIS")),
                new FLValidationRule("ELL", "2", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3|5]$"))
                                        .testThen(Restriction.pattern("School Number",
                                                "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$"))),
                        "If Survey Period Code is 2, 3 or 5, School Number, Current Enrollment must "
                                + "be numeric in the range 0001 to 9899 (excluding 9001), N998 or N999.")),
                new FLValidationRule("ELL", "3", new ValidateStudentNumber("Student Number",
                        "The first nine positions of Student Number Identifier, Florida must be "
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be "
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is "
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an "
                                + "\"X\", the first three positions may not all be zeroes.")),
                new FLValidationRule("ELL", "4", new ValidateRegularExpression(
                        "Survey Period", "^(2|3|5)$",
                        "Survey Period Code must be 2, 3 or 5, and must be correct for the "
                                + "submission specified by the district.")),
                new FLValidationRule("ELL", "5", new ValidateFiscalYear()),
                new FLValidationRule("ELL", "6", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Entry Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Entry Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Entry Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Entry Date "
                                + "must be numeric and a valid date less than or equal to the survey date unless "
                                + "zero filled. If Survey Period Code is 5, English Language Learners: Entry Date "
                                + "must be numeric and a valid date less than or equal to June 30th unless zero "
                                + "filled.")),
                new FLValidationRule("ELL", "7", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.pattern("Basis of Entry", "^(A|R|L|T)$"))),
                        "If Survey Period Code is 2, 3 or 5, English Language Learners: Basis of "
                                + "Entry code must be A, R, L, or T.")),
                new FLValidationRule("ELL", "8", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Student Plan Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Student Plan Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Student Plan Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Student Plan Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Student Plan "
                                + "Date must be numeric and a valid date less than or equal to the survey date "
                                + "unless zero filled. If Survey Period Code is 5, English Language Learners: "
                                + "Student Plan Date must be numeric and a valid date less than or equal to June "
                                + "30th unless zero filled.")),
                new FLValidationRule("ELL", "9", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Classification Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Classification Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Classification Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Classification Date", "00000000")))),
                        "If Survey Period is 2 or 3, English Language Learners Classification Date "
                                + "must be numeric and a valid date less than or equal to the survey date unless "
                                + "zero filled. If Survey Period is 5, English Language Learners Classification Date "
                                + "must be numeric and a valid date less than or equal to June 30th unless zero "
                                + "filled.")),
                new FLValidationRule("ELL", "10", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Exit Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Exit Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Exit Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Exit Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Exit Date must "
                                + "be numeric and a valid date less than or equal to the survey date, unless zerofilled. "
                                + "If Survey Period Code is 5, English Language Learners: Exit Date must be "
                                + "numeric and a valid date less than or equal to June 30th unless zero filled.")),
                new FLValidationRule("ELL", "11", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Reevaluation Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Reevaluation Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Reevaluation Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Reevaluation Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, English Language Learners: Reevaluation"
                                + "Date must be numeric and a valid date less than or equal to the survey date "
                                + "unless zero filled. If Survey Period Code is 5, English Language Learners: "
                                + "Reevaluation Date must be numeric and a valid date less than or equal to June "
                                + "30th unless zero filled.")),
                new FLValidationRule("ELL", "12", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.pattern("Ext of Instruction", "^(Y|Z)$"))),
                        "If Survey Period Code is 2, 3 or 5, English Language Learners: Extension "
                                + "of Instruction must be Y or Z.")),
                new FLValidationRule("ELL", "13", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Reclass Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Reclass Date", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Reclass Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Reclass Date", "00000000")))),
                        "If Survey Period Code is 2 or 3, The English Language Learners: "
                                + "Reclassification Date must be numeric and a valid date less than or equal to the "
                                + "survey date unless zero filled. If Survey Period Code is 5, English Language "
                                + "Learners: Reclassification Date must be numeric and a valid date less than or "
                                + "equal to June 30th unless zero filled.")),
                new FLValidationRule("ELL", "14", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Listening",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Test Date Listening", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Listening",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Test Date Listening", "00000000")))),
                        "If Survey Period is 2 or 3, Test Date Listening must be numeric and a valid "
                                + "date, less than or equal to the survey date, unless zero filled. If Survey Period is "
                                + "5, Test Date Listening must be numeric and a valid date less than or equal to "
                                + "June 30th, unless zero-filled.")),
                new FLValidationRule("ELL", "15", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.pattern("Basis of Exit First", "^[A-J|L|R|Z]$"))),
                        "If Survey Period Code is 2, 3 or 5, English Language Learners: Basis of Exit "
                                + "(First) code must be A, B, C, D, E, F, G, H, I, J, L, R or Z.")),
                new FLValidationRule("ELL", "16", new ValidateRegularExpression(
                        "Fund Source", "^(E|Z)$", "Fund Source code must be E or Z.")),
                new FLValidationRule("ELL", "18", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Student Plan Date",
                                                "Entry Date", Date.class))),
                        "The English Language Learners: Student Plan Date must be a valid date "
                                + "and be greater than or equal to the English Language Learners: Entry Date.")),
                new FLValidationRule("ELL", "19", new ValidateRegularExpression(
                        "Prog Participation", "^(E|H|L|N|Z)$",
                        "English Language Learners: Program Participation code must be E, H, L, N or Z.")),
                new FLValidationRule("ELL", "1A", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                        .testThen(Restriction.pattern("Tier Placement", "^[A-D|Z]$")),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(3|5)$"))
                                        .testThen(Restriction.equals("Tier Placement", "Z"))),
                        "If Survey Period Code is 2, English Language Learners: Tier Placement "
                                + "code must be A, B, C, D or Z. If Survey Period Code is 3 or 5, then English "
                                + "Language Learners: Tier Placement code must be Z.")),
                new FLValidationRule("ELL", "20", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Reclass Date"))
                                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Reclass Date", "Exit Date",
                                                Date.class))),
                        "If there is a valid English Language Learners: Reclassification Date, then "
                                + "the English Language Learners: Reclassification Date must be greater than or "
                                + "equal to a valid English Language Learners: Exit Date.")),
                new FLValidationRule("ELL", "21", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number CIS", "Student Number",
                                                "Survey Period", "Fiscal Year"))),
                        "Each English Language Learners Student Information record must be "
                                + "unique based on the keys of District Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Survey Period Code; and School Year.")),
                new FLValidationRule("ELL", "22", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.greaterThanOrEquals("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction.or(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                "pgm-test-name-listening", "Test Name Listening"),
                                                Restriction.equals("Test Name Listening", "ZZZ"))),
                                ValidationRule.testIf(
                                        Restriction.lessThan("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction
                                                .or(Restriction.and(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                        "pgm-test-name-listening", "Test Name Listening"),
                                                        Restriction.pattern("Test Name Listening", "^(?!X).\\S\\S$")),
                                                        Restriction.equals("Test Name Listening", "ZZZ"))),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "68"))
                                        .testThen(Restriction.pattern("Test Name Listening", "^X\\S\\S$"))),
                        "If English Language Learners: Entry Date is September 1, 2005 or greater "
                                + "Test Name Listening must be a valid test on the Test Name Table in Appendix I "
                                + "with an indicator code of L (Listening) or Test Name Listening may be ZZZ. If "
                                + "English Language Learners: Entry Date is prior to September 1, 2005 Test Name "
                                + "Listening must be a valid test on the Test Name Table in Appendix I, other than "
                                + "X__, or Test Name Listening may be ZZZ. If District Number, Current Enrollment "
                                + "is 68, then Test Name Listening may be X__.")),
                new FLValidationRule("ELL", "23", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.or(
                                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-cont-listening",
                                                        "Test Cont Listening"),
                                                Restriction.equals("Test Cont Listening", "ZZ")))),
                        "The Test Subject Content Listening must be a valid code specified in "
                                + "Appendix L of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual or it may be ZZ.")),
                // Rule #24 skipped (Transaction Code)
                new FLValidationRule("ELL", "25", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(Restriction.notEquals("School Number", "N998"),
                                        Restriction.notEquals("School Number", "N999")))
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "The School Number, Current Enrollment must exist on the Master School "
                                + "Identification File as a valid active school in the district of enrollment, unless "
                                + "School Number, Current Enrollment is equal to N998 or N999.")),
                new FLValidationRule("ELL", "26", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Test Date Listening"))
                                        .testThen(Restriction.notEquals("Test Name Listening", "ZZZ"))),
                        "If the Test Date Listening is a valid date, then the Test Name Listening must "
                                + "be a valid code other than ZZZ.")),
                new FLValidationRule("ELL", "27", new ValidateRegularExpression("Test Sc T Listening",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Listening must be RS, SS, NP, AL or ZZ.")),
                new FLValidationRule("ELL", "28", new ValidateRegularExpression("Test Score Listening",
                        "^[0\\d]{4}$",
                        "Test Score Listening must be numeric and right justified with leading zeros.")),
                new FLValidationRule("ELL", "29", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Listening", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Listening", "ZZZ"),
                                                Restriction.equals("Test Sc T Listening", "ZZ"),
                                                Restriction.equals("Test Cont Listening", "ZZ"),
                                                Restriction.equals("Test Score Listening", "0000")))),
                        "If Test Date Listening is equal to zero, then Test Name Listening must be "
                                + "ZZZ, the Test Score Type Listening must be ZZ, the Test Subject Content "
                                + "Listening must be ZZ, and the Test Score Listening must be zero.")),
                new FLValidationRule("ELL", "2A", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.equals("Basis of Exit First", "A"))
                                        .testThen(Restriction.lessThan("Exit Date", generateDate("07", "01", "2009")))),
                        "If Survey Period Code is 2, 3 or 5 and if English Language Learners: Basis "
                                + "of Exit (First) is code A, then English Language Learners: Exit Date must be prior "
                                + "to July 1, 2009.")),
                new FLValidationRule("ELL", "2B", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.or(Restriction.pattern("Basis of Exit First", "^(F|G|R)$"),
                                                Restriction.pattern("Basis of Exit Second", "^(F|G|R)$")))
                                        .testThen(Restriction.lessThan("Exit Date", generateDate("07", "01", "2008"))),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.or(Restriction.pattern("Basis of Exit First", "^A$"),
                                                Restriction.pattern("Basis of Exit Second", "^A$")))
                                        .testThen(Restriction.lessThan("Exit Date", generateDate("07", "01", "2009"))),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.or(Restriction.pattern("Basis of Exit First", "^[B-E]$"),
                                                Restriction.pattern("Basis of Exit Second", "^[B-E]$")))
                                        .testThen(Restriction.lessThan("Exit Date", generateDate("04", "07", "2012")))),
                        "If Survey Period Code is 2, 3 or 5 and if either English Language Learners: "
                                + "Basis of Exit (First) or English Language Learners: Basis of Exit (Second) is code "
                                + "F, G or R, then English Language Learners: Exit Date must be prior to July 1, "
                                + "2008. "
                                + "If Survey Period Code is 2, 3 or 5 and if either English Language Learners: Basis "
                                + "of Exit (First) or English Language Learners: Basis of Exit (Second) is code A, "
                                + "then English Language Learners: Exit Date must be prior to July 1, 2009. "
                                + "If Survey Period Code is 2, 3 or 5 and if either English Language Learners: Basis "
                                + "of Exit (First) or English Language Learners: Basis of Exit (Second) is code B, C, "
                                + "D, or E, then English Language Learners: Exit Date must be prior to April 7, 2012.")),
                new FLValidationRule("ELL", "2C", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.notEquals("Basis of Exit First", "Z"))
                                        .testThen(Restriction.greaterThan("Exit Date", Double.valueOf(0)))),
                        "If Survey Period Code is 2, 3 or 5 and if English Language Learners: Basis "
                                + "of Exit (First) is not Z, then English Language Learners: Exit Date must be greater"
                                + "than 0.")),
                // Rule #2D Deleted
                // Rule #2E Deleted
                // Rule #2F Deleted
                // Rule #2G Deleted
                new FLValidationRule("ELL", "2H", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Basis of Entry", "A"),
                                        Restriction.equals("Exit Date", "00000000"))
                                        .testThen(Restriction.and(Restriction.notEquals("Test Name Listening", "ZZZ"),
                                                Restriction.notEquals("Test Name Speaking", "ZZZ")))),
                        "If English Language Learners: Basis of Entry is code A and If English "
                                + "Language Learners: Exit Date is 00000000 then the Test Name: Listening and "
                                + "Test Name: Speaking must be other than ZZZ.")),
                new FLValidationRule("ELL", "2I", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Basis of Entry", "R"),
                                        Restriction.equals("Exit Date", "00000000"))
                                        .testThen(Restriction.and(Restriction.notEquals("Test Name Reading", "ZZZ"),
                                                Restriction.notEquals("Test Name Writing", "ZZZ")))),
                        "If English Language Learners: Basis of Entry is code R and If English "
                                + "Language Learners: Exit Date is 00000000 then the Test Name: Reading and Test "
                                + "Name: Writing must be other than ZZZ.")),
                new FLValidationRule("ELL", "2J", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.and(Restriction.pattern("Test Form Listening", "\\S"),
                                                Restriction.pattern("Test Form Speaking", "\\S"),
                                                Restriction.pattern("Test Form Reading", "\\S"),
                                                Restriction.pattern("Test Form Writing", "\\S")))),
                        "If Survey Period Code is 2, 3 or 5, then Test Form must not be blank.")),
                new FLValidationRule("ELL", "2K", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.and(Restriction.pattern("Test Level Listening", "\\S"),
                                                Restriction.pattern("Test Level Speaking", "\\S"),
                                                Restriction.pattern("Test Level Reading", "\\S"),
                                                Restriction.pattern("Test Level Writing", "\\S")))),
                        "If Survey Period Code is 2, 3 or 5, then Test Form must not be blank.")),
                new FLValidationRule("ELL", "2L", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.or(Restriction.pattern("Basis of Exit First", "^[H-J]$"),
                                                Restriction.pattern("Basis of Exit Second", "^[H-J]$")))
                                        .testThen(Restriction.greaterThan("Exit Date",
                                                generateDate("04", "06", "2012")))),
                        "If Survey Period Code is 2, 3 or 5 and if either English Language Learners: "
                                + "Basis of Exit (First) or English Language Learners: Basis of Exit (Second) is code "
                                + "H, I or J, then English Language Learners: Exit Date must be after April 6, 2012.")),
                new FLValidationRule("ELL", "2M", new ValidateRegularExpression("District Number",
                        "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5])$",
                        "District Number, Current Enrollment must be numeric and in the range "
                                + "01-68 or 71-75.")),
                new FLValidationRule("ELL", "2O", new ValidateRegularExpression("Fl Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable.")),
                new FLValidationRule("ELL", "30", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Listening", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Sc T Listening", "ZZ"))),
                        "If Test Date Listening is greater than zero, then Test Score Type Listening "
                                + "must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "31", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Listening", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Cont Listening", "ZZ"))),
                        "If Test Date Listening is greater than zero, then Test Subject Content "
                                + "Listening must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "32", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Speaking",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Test Date Speaking", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Speaking",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Test Date Speaking", "00000000")))),
                        "If Survey Period is 2 or 3 Test Date Speaking must be numeric and a valid "
                                + "date, less than or equal to the survey date, unless zero filled. If Survey Period is "
                                + "5, Test Date Speaking must be numeric and a valid date, less than or equal to "
                                + "June 30th, unless zero filled.")),
                new FLValidationRule("ELL", "33", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.greaterThanOrEquals("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction.or(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                "pgm-test-name-speaking", "Test Name Speaking"),
                                                Restriction.equals("Test Name Speaking", "ZZZ"))),
                                ValidationRule.testIf(
                                        Restriction.lessThan("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction
                                                .or(Restriction.and(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                        "pgm-test-name-speaking", "Test Name Speaking"),
                                                        Restriction.pattern("Test Name Speaking", "^(?!X).\\S\\S$")),
                                                        Restriction.equals("Test Name Speaking", "ZZZ"))),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "68"))
                                        .testThen(Restriction.pattern("Test Name Listening", "^X\\S\\S$"))),
                        "If English Language Learners: Entry Date is September 1, 2005 or greater "
                                + "Test Name Listening must be a valid test on the Test Name Table in Appendix I "
                                + "with an indicator code of L (Listening) or Test Name Listening may be ZZZ. If "
                                + "English Language Learners: Entry Date is prior to September 1, 2005 Test Name "
                                + "Listening must be a valid test on the Test Name Table in Appendix I, other than "
                                + "X__, or Test Name Listening may be ZZZ. If District Number, Current Enrollment "
                                + "is 68, then Test Name Listening may be X__.")),
                new FLValidationRule("ELL", "34", new ValidateRegularExpression("Test Sc T Speaking",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Speaking must be RS, SS, NP, AL or ZZ.")),
                new FLValidationRule("ELL", "35", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.or(
                                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-cont-speaking",
                                                        "Test Cont Speaking"),
                                                Restriction.equals("Test Cont Speaking", "ZZ")))),
                        "The Test Subject Content Speaking must be a valid code specified in "
                                + "Appendix L of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual or it may be ZZ.")),
                new FLValidationRule("ELL", "36", new ValidateRegularExpression("Test Score Speaking",
                        "^[0\\d]{4}$",
                        "Test Score Speaking must be numeric and right justified with leading zeros.")),
                new FLValidationRule("ELL", "37", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Speaking", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Speaking", "ZZZ"),
                                                Restriction.equals("Test Sc T Speaking", "ZZ"),
                                                Restriction.equals("Test Cont Speaking", "ZZ"),
                                                Restriction.equals("Test Score Speaking", "0000")))),
                        "If Test Date Speaking is equal to zero, then Test Name Speaking must be "
                                + "ZZZ, the Test Score Type Speaking must be ZZ, the Test Subject Content "
                                + "Speaking must be ZZ, and the Test Score Speaking must be zero.")),
                new FLValidationRule("ELL", "38", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Test Date Speaking"))
                                        .testThen(Restriction.notEquals("Test Name Speaking", "ZZZ"))),
                        "If the Test Date Speaking is a valid date, then the Test Name Speaking must "
                                + "be a valid code other than ZZZ.")),
                new FLValidationRule("ELL", "39", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Speaking", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Sc T Speaking", "ZZ"))),
                        "If Test Date Speaking is greater than zero, then Test Score Type Speaking "
                                + "must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "40", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Speaking", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Cont Speaking", "ZZ"))),
                        "If Test Date Speaking is greater than zero, then Test Subject Content "
                                + "Speaking must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "41", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Reading",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Test Date Reading", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Reading",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Test Date Reading", "00000000")))),
                        "If Survey Period is 2 or 3, Test Date Reading must be numeric and a valid "
                                + "date, less than or equal to the survey date, unless zero filled. If Survey Period is "
                                + "5, Test Date Reading must be numeric and a valid date less than or equal to "
                                + "June 30th, unless zero-filled.")),
                new FLValidationRule("ELL", "42", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.greaterThanOrEquals("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction.or(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                "pgm-test-name-reading", "Test Name Reading"),
                                                Restriction.equals("Test Name Reading", "ZZZ"))),
                                ValidationRule.testIf(
                                        Restriction.lessThan("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction
                                                .or(Restriction.and(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                        "pgm-test-name-reading", "Test Name Reading"),
                                                        Restriction.pattern("Test Name Reading", "^(?!X).\\S\\S$")),
                                                        Restriction.equals("Test Name Reading", "ZZZ"))),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "68"))
                                        .testThen(Restriction.pattern("Test Name Reading", "^X\\S\\S$")),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "55"),
                                        Restriction.lessThan("Test Date Reading", generateDate("09", "01", "2010")))
                                        .testThen(Restriction.pattern("Test Name Reading", "^X\\S\\S$"))),
                        "If English Language Learners: Entry Date is September 1, 2005 or greater"
                                + "Test Name Reading must be an approved test listed on the Test Name Table in "
                                + "Appendix I with an indicator code of R (Reading) or Test Name Reading may be "
                                + "ZZZ. If English Language Learners: Entry Date is prior to September 1, 2005 Test "
                                + "Name Reading must be on the Test Name Table in Appendix I, other than X__, or "
                                + "Test Name Reading may be ZZZ. If District Number, Current Enrollment is 68, then "
                                + "Test Name Reading may be X__. If District Number, Current Enrollment is 55 and "
                                + "Test Date is prior to September 1, 2010, then Test Name Reading may be X__.")),
                new FLValidationRule("ELL", "43", new ValidateRegularExpression("Test Sc T Reading",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Reading must be RS, SS, NP, AL or ZZ.")),
                new FLValidationRule("ELL", "44", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.or(
                                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-cont-reading",
                                                        "Test Cont Reading"),
                                                Restriction.equals("Test Cont Reading", "ZZ")))),
                        "The Test Subject Content Reading must be a valid code specified in "
                                + "Appendix L of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual or it may be ZZ.")),
                new FLValidationRule("ELL", "45", new ValidateRegularExpression("Test Score Reading",
                        "^[0\\d]{4}$",
                        "Test Score Reading must be numeric and right justified with leading zeros.")),
                new FLValidationRule("ELL", "46", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Reading", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Reading", "ZZZ"),
                                                Restriction.equals("Test Sc T Reading", "ZZ"),
                                                Restriction.equals("Test Cont Reading", "ZZ"),
                                                Restriction.equals("Test Score Reading", "0000")))),
                        "If Test Date Reading is equal to zero, then Test Name Reading must be "
                                + "ZZZ, the Test Score Type Reading must be ZZ, the Test Subject Content "
                                + "Reading must be ZZ, and the Test Score Reading must be zero.")),
                new FLValidationRule("ELL", "47", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Test Date Reading"))
                                        .testThen(Restriction.notEquals("Test Name Reading", "ZZZ"))),
                        "If the Test Date Reading is a valid date, then the Test Name Reading must "
                                + "be a valid code other than ZZZ.")),
                new FLValidationRule("ELL", "48", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Reading", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Sc T Reading", "ZZ"))),
                        "If Test Date Reading is greater than zero, then Test Score Type Reading "
                                + "must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "49", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Reading", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Cont Reading", "ZZ"))),
                        "If Test Date Reading is greater than zero, then Test Subject Content "
                                + "Reading must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "50", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Writing",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                Restriction.equals("Test Date Writing", "00000000"))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                        .testThen(Restriction.or(Restriction.lessThanOrEquals("Test Date Writing",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30")),
                                                Restriction.equals("Test Date Writing", "00000000")))),
                        "If Survey Period is 2 or 3, Test Date Writing must be numeric and a valid "
                                + "date, less than or equal to the survey date, unless zero filled. If Survey Period is "
                                + "5, Test Date Writing must be numeric and a valid date less than or equal to "
                                + "June 30th, unless zero-filled.")),
                new FLValidationRule("ELL", "51", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.greaterThanOrEquals("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction.or(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                "pgm-test-name-writing", "Test Name Writing"),
                                                Restriction.equals("Test Name Writing", "ZZZ"))),
                                ValidationRule.testIf(
                                        Restriction.lessThan("Entry Date", generateDate("09", "01", "2015")))
                                        .testThen(Restriction
                                                .or(Restriction.and(Restriction.byExtFldRefTable("FL-PGM-ELL",
                                                        "pgm-test-name-writing", "Test Name Writing"),
                                                        Restriction.pattern("Test Name Writing", "^(?!X).\\S\\S$")),
                                                        Restriction.equals("Test Name Writing", "ZZZ"))),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "68"))
                                        .testThen(Restriction.pattern("Test Name Writing", "^X\\S\\S$")),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "50"),
                                        Restriction.greaterThanOrEquals("Test Date Writing",
                                                generateDate("07", "01", "2006")),
                                        Restriction.lessThanOrEquals("Test Date Writing",
                                                generateDate("06", "30", "2008")))
                                        .testThen(Restriction.pattern("Test Name Writing", "^X\\S\\S$")),
                                ValidationRule.testIf(Restriction.equals("District Number CIS", "55"),
                                        Restriction.lessThan("Test Date Writing", generateDate("09", "01", "2010")))
                                        .testThen(Restriction.pattern("Test Name Writing", "^X\\S\\S$"))),
                        "If English Language Learners: Entry Date is September 1, 2005 or greater "
                                + "Test Name Writing must be an approved test listed on the Test Name Table in "
                                + "Appendix I with an indicator code of W (Writing) or Test Name Writing may be "
                                + "ZZZ. If English Language Learners: Entry Date is prior to September 1, 2005, Test "
                                + "Name Writing must be on the Test Name Table in Appendix I, other than X__, or "
                                + "Test Name Writing may be ZZZ. If District Number is 68, then Test Name Writing "
                                + "may be X__. If District Number, Current Enrollment is 50 and Test Date: Writing is "
                                + "between 07/01/2006 and 06/30/2008, then Test Name: Writing may be X__. If "
                                + "District Number, Current Enrollment is 55 and Test Date is prior to September 1, "
                                + "2010, then Test Name Writing may be X__.")),
                new FLValidationRule("ELL", "52", new ValidateRegularExpression("Test Sc T Writing",
                        "^(RS|SS|NP|AL|ZZ)$",
                        "Test Score Type Writing must be RS, SS, NP, AL or ZZ.")),
                new FLValidationRule("ELL", "53", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.or(
                                                Restriction.byExtFldRefTable("FL-PGM-ELL", "pgm-test-subject-writing",
                                                        "Test Subject Writing"),
                                                Restriction.equals("Test Subject Writing", "ZZ")))),
                        "The Test Subject Content Writing must be a valid code specified in "
                                + "Appendix L of the DOE Information Data Base Requirements Volume I-Automated "
                                + "Student Information System manual or it may be ZZ.")),
                new FLValidationRule("ELL", "54", new ValidateRegularExpression("Test Score Writing",
                        "^[0\\d]{4}$",
                        "Test Score Writing must be numeric and right justified with leading zeros.")),
                new FLValidationRule("ELL", "55", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Test Date Writing", "00000000"))
                                        .testThen(Restriction.and(Restriction.equals("Test Name Writing", "ZZZ"),
                                                Restriction.equals("Test Sc T Writing", "ZZ"),
                                                Restriction.equals("Test Subject Writing", "ZZ"),
                                                Restriction.equals("Test Score Writing", "0000")))),
                        "If Test Date Writing is equal to zero, then Test Name Writing must be "
                                + "ZZZ, the Test Score Type Writing must be ZZ, the Test Subject Content "
                                + "Writing must be ZZ, and the Test Score Writing must be zero.")),
                new FLValidationRule("ELL", "56", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Test Date Writing"))
                                        .testThen(Restriction.notEquals("Test Name Writing", "ZZZ"))),
                        "If the Test Date Writing is a valid date, then the Test Name Writing must "
                                + "be a valid code other than ZZZ.")),
                new FLValidationRule("ELL", "57", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Writing", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Sc T Writing", "ZZ"))),
                        "If Test Date Writing is greater than zero, then Test Score Type Writing "
                                + "must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "58", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Test Date Writing", Double.valueOf(0)))
                                        .testThen(Restriction.notEquals("Test Subject Writing", "ZZ"))),
                        "If Test Date Writing is greater than zero, then Test Subject Content "
                                + "Writing must be a valid code other than ZZ.")),
                new FLValidationRule("ELL", "59", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("ELL", "62", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.or(
                                                Restriction.byDateFormat("MMddyyyy", "Reclass Exit Date"),
                                                Restriction.equals("Reclass Exit Date", "00000000")))),
                        "If Survey Period Code is 2, 3 or 5, then English Language Learners: "
                                + "Reclassification Exit Date must be numeric and a valid date, unless zero-filled.")),
                new FLValidationRule("ELL", "63", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Reclass Exit Date", Double.valueOf(0)))
                                        .testThen(Restriction.greaterThanOrEqualsFieldValue("Reclass Exit Date",
                                                "Reclass Date", Date.class))),
                        "If the English Language Learners: Reclassification Exit Date is greater than "
                                + "zero, then the English Language Learners: Reclassification Exit Date must be "
                                + "equal to or greater than the English Language Learners: Reclassification Date.")),
                new FLValidationRule("ELL", "64", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                        .testThen(Restriction.pattern("Basis of Exit Second", "^[B-J|L|Z]$"))),
                        "If Survey Period Code is 2, 3 or 5, English Language Learners: Basis of Exit "
                                + "(Second) code must be B, C, D, E, F, G, H, I, J, L or Z.")),
                new FLValidationRule("ELL", "65", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.notEquals("Basis of Exit Second", "Z"))
                                        .testThen(Restriction.notEquals("Basis of Exit First", "Z"))),
                        "If Survey Period Code is 2, 3 or 5 and English Language Learners: Basis of "
                                + "Exit (Second) code is not Z, then English Language Learners: Basis of Exit (First)"
                                + "code must not be Z.")),
                new FLValidationRule("ELL", "75", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number CIS", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Pattern.compile("^(LY|LP|LF)$"), "ELL")))),
                        "Each English Language Learners Student Information record must have a "
                                + "matching Student Demographic record based on District Number, Current "
                                + "Instruction/Service (Student Demographic) and District Number, Current "
                                + "Instruction/Service (English Language Learners Student Information); Student "
                                + "Number Identifier, Florida; Survey Period Code and School Year and with the "
                                + "English Language Learners code = LY, LP or LF.")),
                new FLValidationRule("ELL", "76", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.greaterThan("Exit Date", Double.valueOf(0)),
                                        Restriction.equals("Reclass Date", "00000000"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^LF$"), "ELL")))),
                        "If the English Language Learners: Exit Date is greater than zero, then the "
                                + "English Language Learners code must be LF, unless the English Language "
                                + "Learners: Reclassification Date is greater than zero.")),
                new FLValidationRule("ELL", "77", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.equals("Basis of Exit First", "Z"),
                                        Restriction.equals("Basis of Exit Second", "Z"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair(Pattern.compile("^(LP|LY)$"), "ELL"))),
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code is 2, 3 or 5; and if the English Language Learners: "
                                + "Basis of Exit (First) code is Z; and if the English Language Learners: Basis of "
                                + "Exit (Second) code is Z, then the English Language Learners, PK-12 code (on the "
                                + "Student Demographic Information record) must be LP or LY. The records should "
                                + "be matched on the following elements: District Number, Current "
                                + "Instruction/Service (Student Demographic) and District Number, Current "
                                + "Instruction/Service (English Language Learners Student Information); Student "
                                + "Number Identifier, Florida; Survey Period Code and School Year.")),
                new FLValidationRule("ELL", "79", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.byDateFormat("MMddyyyy", "Reclass Exit Date"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair(Pattern.compile("^LF$"), "ELL")))),
                        "If the English Language Learners: Reclassification Exit Date is a valid date, "
                                + "then the English Language Learners code must be LF.")),
                new FLValidationRule("ELL", "80", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^LY$"), "ELL"),
                                                new KeyValuePair(Pattern.compile("^(0[1-9]|10|1[1-2])$"),
                                                        "Grade Level")))
                                        .testThen(Restriction.pattern("Tier Placement", "^[A-D]$")),
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code = 2 and if English Language Learners, PK-12 on "
                                + "the Student Demographic Information record is LY and Grade Level = 1-12, then "
                                + "English Language Learners: Tier Placement code must be A, B, C or D. The "
                                + "match between the two records should be done by matching the District Number, "
                                + "Current Instruction/Service (Student Demographic Information) and District "
                                + "Number, Current Instruction/Service (English Language Learners Information); "
                                + "Student Number Identifier, Florida; Survey Period Code and School Year.")),
                new FLValidationRule("ELL", "82", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Classification Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Classification Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN)))),
                        "English Language Learners: Classification Date must be greater than or "
                                + "equal to the English Language Learners: Home Language Survey Date, unless "
                                + "the ELL: Classification Date is zero filled.")),
                new FLValidationRule("ELL", "83", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Pattern.compile("^(LY|LF)$"), "ELL")))
                                        .testThen(Restriction.notEquals("Entry Date", "00000000")),
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If English Language Learners, PK-12 on the Student Demographic "
                                + "Information record is LY or LF, then English Language Learners: Entry Date "
                                + "cannot be zero. The match between the two records should be done by matching "
                                + "the District Number, Current Instruction/Service (Student Demographic) and "
                                + "District Number, Current Instruction/Service (English Language Learners Student "
                                + "Information); Student Number Identifier, Florida; Survey Period Code and School "
                                + "Year.")),
                new FLValidationRule("ELL", "85", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number CIS", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period")))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair(Pattern.compile("^(KG|0[1-9]|10|1[1-2])$"),
                                                        "Grade Level"))),
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If there is a matching Student Demographic record, then the Grade Level "
                                + "must be K-12. The records should be matched using the District Number, Current "
                                + "Instruction/Service (Student Demographic) and District Number, Current "
                                + "Instruction/Service (English Language Learners Student Information); Student "
                                + "Number Identifier, Florida; Survey Period Code and School Year.")),
                new FLValidationRule("ELL", "86", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair(Pattern.compile("^LY$"), "ELL")),
                                        Restriction.pattern("Basis of Entry", "^(A|R|T)$"))
                                        .testThen(Restriction.or(Restriction.notEquals("Test Name Listening", "ZZZ"),
                                                Restriction.notEquals("Test Name Reading", "ZZZ"),
                                                Restriction.notEquals("Test Name Writing", "ZZZ"),
                                                Restriction.notEquals("Test Name Speaking", "ZZZ"))),
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"),
                                        Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number CIS", "District Number, E"),
                                                new KeyValuePair(Pattern.compile("^LF$"), "ELL")),
                                        Restriction.pattern("Basis of Exit First", "^(A|C|D|F|G|R)$"),
                                        Restriction.pattern("Basis of Exit Second", "^(A|C|D|F|G|R)$"))
                                        .testThen(Restriction.or(Restriction.notEquals("Test Name Listening", "ZZZ"),
                                                Restriction.notEquals("Test Name Reading", "ZZZ"),
                                                Restriction.notEquals("Test Name Writing", "ZZZ"),
                                                Restriction.notEquals("Test Name Speaking", "ZZZ")))),
                        "If Survey Period Code is 2, 3 or 5 and if English Language Learners, PK-12 "
                                + "code is LY and if the English Language Learners: Basis of Entry code is A, R or T, "
                                + "then at least one of the following must be other than ZZZ: Test Name: Listening; "
                                + "Test Name: Reading; Test Name: Writing; or Test Name: Speaking. "
                                + "If Survey Period Code is 2, 3 or 5 and if English Language Learners, PK-12 code is "
                                + "LF and if both the English Language Learners: Basis of Exit (First) and the English "
                                + "Language Learners: Basis of Exit (Second) codes are A, C, D, F, G or R then at "
                                + "least one of the following must be other than ZZZ: Test Name: Listening; Test "
                                + "Name: Reading; Test Name: Writing; or Test Name: Speaking.")),
                // TODO: Rule #91
                new FLValidationRule("ELL", "92", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                        Restriction.notEquals("Exit Date", "00000000"),
                                        Restriction.equals("Reclass Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Exit Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN_MINUS_2_YEARS))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.notEquals("Exit Date", "00000000"),
                                        Restriction.equals("Reclass Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Exit Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE_MINUS_2_YEARS, "08", "15")))),
                        "If Survey Period is 2 or 3, English Language Learners: Exit Date must be "
                                + "greater than or equal to two years prior to the Survey Date*, unless zero filled or "
                                + "unless English Language Learners: Reclassification Date is greater than zero. If "
                                + "Survey Period is 5, English Language Learners: Exit Date must be greater than or "
                                + "equal to two years prior to August 15th of the year in which the survey 5 data are "
                                + "due to the Department, unless zero filled or unless English Language Learners "
                                + "Reclassification Date is greater than zero.")),
                new FLValidationRule("ELL", "93", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                        Restriction.notEquals("Reclass Exit Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Reclass Exit Date",
                                                new RuntimeParam(RuntimeParam.DATE_CERTAIN_MINUS_2_YEARS))),
                                ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                        Restriction.notEquals("Reclass Exit Date", "00000000"))
                                        .testThen(Restriction.greaterThanOrEquals("Reclass Exit Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE_MINUS_2_YEARS, "08", "15")))),
                        "If Survey Period is 2 or 3, English Language Learners: Reclassification Exit "
                                + "Date must be greater than or equal to two years prior to the Survey Date*, unless "
                                + "zero filled. If Survey Period is 5, the English Language Learners Reclassification "
                                + "Exit Date must be greater than or equal to two years prior to August 15th of the "
                                + "year in which the survey 5 data is due to the Department, unless zero filled.")),
        });
    }
}
