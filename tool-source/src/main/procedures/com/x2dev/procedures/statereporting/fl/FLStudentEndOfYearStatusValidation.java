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
 * The Class FLStudentEndOfYearStatusValidation.
 */
public class FLStudentEndOfYearStatusValidation {

    /**
     * Instantiates a new FL student end of year status validation.
     */
    public FLStudentEndOfYearStatusValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SEYS", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("SEYS", "2", new ValidateRegularExpression(
                        "School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N998|N999)$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("SEYS", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "The first nine positions of Student Number Identifier, Florida must be numeric. The tenth position of Student Number Identifier, Florida "
                                + "must either be an X or numeric. If the tenth position of Student Number Identifier, Florida is numeric, the first two digits must be a "
                                + "valid district number in the range 01-75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an X, the first three "
                                + "positions may not all be zeroes.")),
                new FLValidationRule("SEYS", "4", new ValidateRegularExpression(
                        "Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("SEYS", "5", new ValidateFiscalYear()),
                // TODO only one record with Grade Level = 30 or 31 will be accepted
                new FLValidationRule("SEYS", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("Student Number", "District Number",
                                        "Survey Period", "Fiscal Year", "Grade Level"))),
                        "Each Student End of Year Status record must be unique based on the keys of District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; Survey Period Code; School Year; and Grade Level. "
                                + "If more than one Student End of Year Status record is submitted, only one record with Grade Level = PK-12 will be "
                                + "accepted and only one record with Grade Level = 30 or 31 will be accepted.")),
                new FLValidationRule("SEYS", "8",
                        new ValidateRegularExpression("Grade Promotion", "^(A|D|P|R|N|Z)$",
                                "Grade Promotion Status must be A, D, P, R, N or Z.")),
                new FLValidationRule("SEYS", "9",
                        new ValidateRegularExpression("Diploma Type",
                                "^(W06|W07|W10|WD1|WFW|WFT|WGD|WGA|WRW|WXL|WXT|WXW|W27|W43|W45|W52|W54|W55|W57|W58|ZZZ)$",
                                "Diploma Type must be W06, W07, W10, WD1, WFW, WFT, WGD, WGA, WRW, WXL, WXT, WXW, W27, W43, W45, W52, W54, W55, W57, W58 or ZZZ.")),
                new FLValidationRule("SEYS", "10",
                        new ValidateRegularExpression("Completion Cert Type", "^(W08|W8A|W8B|W09|ZZZ)$",
                                "Certificate of Completion, Type must be W08, W8A, W8B, W09 or ZZZ")),
                new FLValidationRule("SEYS", "11", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("District Number", "71"),
                                Restriction.pattern("School Number", "^0[567]00$"))
                                .testThen(Restriction.equals("Grade Promotion", "Z"))),
                        "If District Number, Current Enrollment equals 71, and School Number, Current Enrollment equals 0500, 0600, or 0700, "
                                + "then Grade Promotion Status must equal Z.")),
                new FLValidationRule("SEYS", "13",
                        new ValidateRegularExpression("Student Number", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("SEYS", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-8])$"))
                                .testThen(Restriction.equals("Year Entered 9 Grade", "00000000")),
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(09|1[0-2])$"))
                                .testThen(Restriction.greaterThan("Year Entered 9 Grade", Double.valueOf(0)))),
                        "If Grade Level is PK-08 then Year Entered Ninth Grade, Graduation Requirements Determination must be 00000000. "
                                + "If Grade Level is 09, 10, 11 or 12 then Year Entered Ninth Grade, Graduation Requirements Determination must be greater than zero.")),
                new FLValidationRule("SEYS", "16",
                        new ValidateRegularExpression("Withdrawal Reason",
                                "^(DNE|W0[1245]|W3[AB]|W1[2358]|W2[1-6]|WPO|ZZZ)$",
                                "Withdrawal Reason code must be DNE, W01, W02, W3A, W3B, W04, W05, W12, W13, W15, W18, W21-W26, WPO or ZZZ.")),
                new FLValidationRule("SEYS", "17",
                        new ValidateRegularExpression("Grade Level", "^(PK|KG|0[1-9]|1[0-2]|3[01])$",
                                "Grade Level code must be PK, KG, 01-12, 30 or 31.")),
                new FLValidationRule("SEYS", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "The School Number, Current Enrollment must exist on the Master School Identification File as a valid school number in the district of enrollment.")),
                new FLValidationRule("SEYS", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Drp Exit Option Test", "^(P|F)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(1[0-2]|3[01])$"))),
                        "If Dropout Prevention Performance-Based Exit Option Test Results is P or F, then Grade Level must be 10 or higher.")),
                new FLValidationRule("SEYS", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "DNE"))
                                .testThen(Restriction.pattern("Grade Promotion", "^(A|D|P|R)$"))),
                        "If Withdrawal Reason is DNE, then Grade Promotion Status must not be A, D, P, or R.")),
                new FLValidationRule("SEYS", "23",
                        new ValidateRegularExpression("Diploma Biliteracy", "^(B|G|S|Z)$",
                                "Diploma Biliteracy Seal Designation must be B, G, S or Z")),
                new FLValidationRule("SEYS", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.and(
                                                Restriction.greaterThanOrEquals("Grade Point Average", Double.valueOf(0)),
                                                Restriction.lessThanOrEquals("Grade Point Average", Double.valueOf(40000))),
                                        Restriction.equals("Grade Point Average", "99999")))),
                        "Grade Point Average State, Cumulative must be numeric and greater than or equal to zero but must not exceed 4.0000 or it may be 99999.")),
                new FLValidationRule("SEYS", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("District Number", "71"),
                                Restriction.and(
                                        Restriction.pattern("School Number", "^0[34]00$"),
                                        Restriction.equals("District Number", "71")))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")))),
                        ValidationRule.testIf(Restriction.and(Restriction.equals("District Number", "71"),
                                Restriction.pattern("School Number", "^0[567]00$")))
                                .testThen(Restriction.equals("Withdrawal Date", "00000000"))),
                        "If District Number, Current Enrollment is not 71 or"
                                + " If District Number, Current Enrollment is 71 and School Number, Current Enrollment is 0300 or 0400,"
                                + " then Withdrawal Date must be a valid date in the range of 07/01/**** to 08/31****."
                                + "If District Number, Current Enrollment is 71 and"
                                + " School Number, Current Enrollment is 0500, 0600 or 0700,"
                                + " then Withdrawal Date must be 00000000.")),
                new FLValidationRule("SEYS", "28", new ValidateRegularExpression("Florida Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("SEYS", "2P", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateYearEnteredNinthGrade("Year Entered 9 Grade",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "12", "31")))),
                        "Year Entered Ninth Grade, Graduation Requirements Determination must be numeric, "
                                + "contain no blanks and be a valid two-consecutive-years combination not in the future or it must be all zeros.")),
                new FLValidationRule("SEYS", "2R",
                        new ValidateRegularExpression("Online Course Exempt", "^(D|T|Z)$",
                                "Online Course Exempt must be D, T or Z.")),
                new FLValidationRule("SEYS", "32", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Diploma Type",
                                "^(W06|W07|W10|W27|WD1|WFT|WFW|WGA|WGD|WRW|WXL|WXT|WXW)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(09|1[0-2])$"))),
                        "If Diploma Type is W06, W07, W10, W27, WD1, WFT, WFW, WGA, WGD, WRW, WXL, WXT or WXW, Grade Level must be one of the grades 9-12.")),
                new FLValidationRule("SEYS", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Completion Cert Type", "^(W08|W8A|W09|W8B)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(09|1[0-2])$"))),
                        "If Certificate of Completion, Type is W08, W8A, W09, or W8B, Grade Level must be one of the grades 9-12.")),
                new FLValidationRule("SEYS", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("School Number", "0600"),
                                Restriction.notEquals("District Number", "71"))
                                .testThen(Restriction.or(
                                        Restriction.and(
                                                Restriction.notEquals("Diploma Type", "ZZZ"),
                                                Restriction.equals("Completion Cert Type", "ZZZ"),
                                                Restriction.equals("Withdrawal Reason", "ZZZ")),
                                        Restriction.and(
                                                Restriction.equals("Diploma Type", "ZZZ"),
                                                Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                                Restriction.equals("Withdrawal Reason", "ZZZ")),
                                        Restriction.and(
                                                Restriction.equals("Diploma Type", "ZZZ"),
                                                Restriction.equals("Completion Cert Type", "ZZZ"),
                                                Restriction.notEquals("Withdrawal Reason", "ZZZ"))))),
                        "Of the three data elements; Diploma Type, Certificate of Completion, Type, and Withdrawal Reason; one must be other than ZZZ, "
                                + "and only one may be other than ZZZ. This edit does not apply to records with School Number, Current Enrollment of 0600 in "
                                + "District Number, Current Enrollment of 71.")),
                new FLValidationRule("SEYS", "37",
                        new ValidateRegularExpression("Single Parent", "^(S|W|B|Z)$",
                                "Single Parent and Single Pregnant Woman code must be S, W, B or Z.")),
                new FLValidationRule("SEYS", "3C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Diploma Type", "^(W43|W45|W52|W54|W55|W57|W58)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(3[01])$"))),
                        "If Diploma Type is W43, W45, W52, W54, W55, W57 or W58, Grade Level must be 30 or 31.")),
                new FLValidationRule("SEYS", "3D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("Drp Exit Option Test", "^(P|F|Z)$")),
                        ValidationRule.testIf(Restriction.equals("Drp Exit Option Test", "P"))
                                .testThen(Restriction.pattern("Diploma Type", "^(W10|WGD|WGA)$")),
                        ValidationRule.testIf(Restriction.pattern("Diploma Type", "^(W10|WGD|WGA)$"))
                                .testThen(Restriction.equals("Drp Exit Option Test", "P"))),
                        "The Dropout Prevention Performance-Based Exit Option Test Results code must be P, F, or Z. "
                                + "If the Performance-Based Exit Option Test Results code is P, then Diploma Type must either be W10, WGD or WGA. "
                                + "If the Diploma Type code is W10, WGD or WGA then the Dropout Prevention Performance-Based Exit Option Test Results code must be P.")),
                new FLValidationRule("SEYS", "3F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "WPO"))
                                .testThen(Restriction.equals("Grade Level", "12"))),
                        "If Withdrawal Reason is WPO, Grade Level must be 12.")),
                new FLValidationRule("SEYS", "3I",
                        new ValidateRegularExpression("Grade Promotion GCE", "^[0-7]$",
                                "Grade Promotion Status: Good Cause Exemption must be 0 (zero), 1, 2, 3, 4, 5, 6 or 7.")),
                new FLValidationRule("SEYS", "3J", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "A"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.notEquals("Grade Promotion GCE", "0")),
                        ValidationRule.testIf(Restriction.greaterThan("Grade Promotion GCE", Double.valueOf(0)))
                                .testThen(Restriction.and(
                                        Restriction.equals("Grade Promotion", "A"),
                                        Restriction.equals("Grade Level", "3")))),
                        "If Grade Promotion Status equals A and Grade Level equals 3, Grade Promotion Status: Good Cause Exemption must not be 0 (zero); "
                                + "If Grade Promotion Status: Good Cause Exemption is greater than 0, then Grade Level must equal 3 and Grade Promotion Status must equal A.")),
                new FLValidationRule("SEYS", "3K", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "P"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.or(
                                        Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                        Restriction.equals("Withdrawal Reason", "WPO")))),
                        "If Grade Level equals 12 and Grade Promotion Status equals P, either Diploma, Type or Certificate of Completion, Type must not equal ZZZ "
                                + "or Withdrawal Reason must equal WPO.")),
                new FLValidationRule("SEYS", "3L", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Promotion", "^(R|N|Z)$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"))
                                .testThen(Restriction.and(
                                        Restriction.equals("Completion Cert Type", "ZZZ"),
                                        Restriction.equals("Diploma Type", "ZZZ")))),
                        "If Grade Level equals PK-12 and Grade Promotion Status = R, N or Z, then Diploma Type and Certificate Type must equal ZZZ.")),
                // TODO Rule 3P table of students who scored at Level 1 in Reading on the 3rd Grade
                // English Language Arts Florida Standards Assessment (ELA FSA)
                new FLValidationRule("SEYS", "3Q", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^3[01]$"))
                                .testThen(Restriction.equals("Grade Promotion", "Z"))),
                        "If Grade Level equals 30 or 31, then Grade Promotion Status must equal Z.")),
                new FLValidationRule("SEYS", "3R", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "A"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[01])$")),
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "D"))
                                .testThen(Restriction.equals("Grade Level", "12")),
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "P"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.pattern("Grade Promotion", "^(R|N)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "Z"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2]|3[01])$"))),
                        "There must be a valid association between the Grade Promotion Status listed below and the student's Grade Level."
                                + "Grade Promotion Status Grade Levels A-->KG-11 D-->12 P-->PK-12 R-->KG-12 N-->KG-12 Z-->PK-12, 30-31")),
                new FLValidationRule("SEYS", "3S", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.or(
                                        Restriction.notEquals("Completion Cert Type", "ZZZ"),
                                        Restriction.notEquals("Diploma Type", "ZZZ"),
                                        Restriction.equals("Withdrawal Reason", "WPO")))
                                .testThen(Restriction.equals("Grade Promotion", "P")),
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.equals("Diploma Type", "WD1"))
                                .testThen(Restriction.pattern("Grade Promotion", "^(P|D)$"))),
                        "For Grade Levels PK-12, if one of the elements Diploma Type or Certificate of Completion, Type "
                                + "is not equal to ZZZ, or if Withdrawal Reason is WPO, then Grade Promotion Status must be P. "
                                + "If Diploma Type equals WD1, then Grade Promotion Status must be P or D.")),
                new FLValidationRule("SEYS", "3T", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W05"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-5])$"))),
                        "If Withdrawal Reason is W05, Grade Level must not be PK-05.")),
                new FLValidationRule("SEYS", "3U", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W26"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2]|3[01])$"))),
                        "If Withdrawal Reason is W26, Grade Level must not be PK-5.")),
                new FLValidationRule("SEYS", "3V", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion", "D"))
                                .testThen(Restriction.or(
                                        Restriction.and(Restriction.equals("Grade Level", "12"),
                                                Restriction.pattern("Withdrawal Reason", "^(W0[12]|W3A)$")),
                                        Restriction.equals("Diploma Type", "WD1")))),
                        "If Grade Promotion Status = D, then Grade Level must equal 12 and "
                                + "Withdrawal Reason must be W01, W02 or W3A or Diploma Type must equal WD1.")),
                new FLValidationRule("SEYS", "3W", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W25"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG)$"))),
                        "If Withdrawal Reason is W25, Grade Level must be PK or KG.")),
                new FLValidationRule("SEYS", "3X",
                        new ValidateRegularExpression("Diploma Designation", "^(S|M|B|Z)$",
                                "Diploma Designation must be S, M, B or Z.")),
//                new FLValidationRule("SEYS", "41", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.alwaysTrue())
//                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
//                                        new KeyValuePair("Student Number", "Student Number"),
//                                        new KeyValuePair("District Number", "District Number, I/S"),
//                                        new KeyValuePair("District Number", "District Number, E"),
//                                        new KeyValuePair("Survey Period", "Survey Period"),
//                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
//                        "Each Student End of Year Status record must have a matching Student Demographic record "
//                                + "based on both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic); "
//                                + "District Number, Current Enrollment (Student End of Year Status); Student Number Identifier, Florida; Survey Period Code; and School Year.")),
                // TODO Date Entered United States School is less than two years from the Withdrawal Date
                new FLValidationRule("SEYS", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion GCE", "1"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.lessThan("Date Entered US Skl", new RuntimeParam(
                                                RuntimeParam.DATE_CERTAIN_PLUS_2_YEARS)), null)))),
                        "If Grade Promotion Status: Good Cause Exemption equals 1, then there must be a matching Student Demographic record where Date Entered United States School "
                                + "is less than two years from the Withdrawal Date. The records should match based on both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service (Student Demographic); District Number Current Enrollment (End of Year Status) Student Number Identifier, Florida; "
                                + "Survey Period, and School Year.")),
                new FLValidationRule("SEYS", "43", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Except Primary", "L"), null),
                                new KeyValuePair(Restriction.equals("Exceptionality Other", "ZZZZZZZZZ"),
                                        null)))
                                .testThen(Restriction.notEquals("Grade Promotion", "D"))),
                        "If Exceptionality, Primary is L and Exceptionality, Other is Z-filled, then the Grade Promotion Status must not be D. "
                                + "The records should match based on District Number, Current Enrollment; School Number, Current Enrollment; "
                                + "Student Number, Identifier; Survey, and Year.")),
                new FLValidationRule("SEYS", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateNotMatchInExport("EXPDATA-FL-EXCEPT",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.pattern("Diploma Type", "^(?!W[FW|RW|XW])\\S{3}$")),
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number"),
                                new KeyValuePair("School Number", "School Number"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Except Primary", "L"), null),
                                new KeyValuePair(Restriction.equals("Exceptionality Other",
                                        "ZZZZZZZZZ"), null)))
                                .testThen(Restriction.pattern("Diploma Type", "^(?!W[FW|RW|XW])\\S{3}$"))),
                        "If a Student End of Year Status record does not have a matching Exceptional Student record, based on District Number, Current Enrollment; "
                                + "School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Year "
                                + "or if Exceptionality, Primary is L and Exceptionality, Other is Z-filled, "
                                + "then the Diploma Type must not be WFW, WRW or WXW.")),
                new FLValidationRule("SEYS", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion GCE", "2"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.or(
                                                Restriction.pattern("Except Primary", "^(?!L|Z)\\S{1,9}$"),
                                                Restriction.pattern("Exceptionality Other", "^(?!L|Z)\\S{1,9}$")),
                                                null)))),
                        "If Grade Promotion Status: Good Cause Exemption equals 2; "
                                + "then there must be a matching Exceptional Student record where one of the two elements Exceptionality, Primary "
                                + "or Exceptionality, Other does not equal L or Z. The records should match based on District Number, Current Enrollment; "
                                + "School Number, Current Enrollment; Student Number, Identifier; Survey, and Year.")),
                new FLValidationRule("SEYS", "46", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Online Course Exempt", "D"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.or(
                                                Restriction.pattern("Except Primary", "^(?!L|Z)\\S{1,9}$"),
                                                Restriction.pattern("Exceptionality Other", "^(?!L|Z)\\S{1,9}$")),
                                                null)))),
                        "If the Online Course Exempt code is D, then there must be a matching Exceptional Student record (based on District Number, Current Enrollment; "
                                + "School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Year) with at least one Exceptionality, Primary "
                                + "or Exceptionality, Other code that is not L or Z.")),
                new FLValidationRule("SEYS", "47", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Year Entered 9 Grade",
                                "^201[4-9]201[5-9]|2[1-9]\\d{2}2[1-9]\\d{2}|20[2-9]\\d20[2-9]\\d$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.notEquals("Graduation Option", "4"), null)))),
                        "If Year Entered Ninth Grade, Graduation Requirements Determination is 2014-2015 or later, "
                                + "then Graduation Option on the Student Demographic record may not equal 4. "
                                + "The records should match based on both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic); "
                                + "District Number, Current Enrollment (End of Year Status), Student Number, Identifier, Florida; Survey Period Code, and Year.")),
                new FLValidationRule("SEYS", "49", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Promotion GCE", "5"))
                                .testThen(Restriction.or(
                                        Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.equals("Section 504 Eligible", "Y"),
                                                        null)),
                                        Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.or(
                                                        Restriction.pattern("Except Primary", "^(?!L|Z)\\S{1,9}$"),
                                                        Restriction.pattern("Exceptionality Other",
                                                                "^(?!L|Z)\\S{1,9}$")),
                                                        null))))),
                        "If Grade Promotion Status Good Cause Exemption equals 5, then there must either be a matching Federal/State Indicator record with Section 504 Eligible "
                                + "equal to Y, or there must be a matching Exceptional Student record with one of the two elements Exceptionality, Primary or Exceptionality, Other "
                                + "not equal to L or Z. The records should match based on District Number, Current Enrollment; School Number, Current Enrollment; "
                                + "Student Number, Identifier; Survey, and Year.")),
                new FLValidationRule("SEYS", "4A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("District Number", "District Number, E"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.pattern("Additional School Y", "^(S|F)$"), null)))
                                .testThen(Restriction.pattern("Grade Promotion", "^(P|D|N)$"))),
                        "If Additional School Year Student (on the Student Demographic Information record) = S or F, then Grade Promotion Status must be P, D, or N. "
                                + "The records should match based on both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic); "
                                + "District Number Current Enrollment (Student End of Year Status); Student Number Identifier, Florida; Survey Period Code and School Year.")),
                // TODO age must be less than 6 as of the Withdrawal Date
                new FLValidationRule("SEYS", "4B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W25"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.studentAgeInRange("Student ID Local", null, Integer.valueOf(6),
                                                        false, false,
                                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "09", "01")),
                                                null)))),
                        "If Withdrawal Reason = W25 then age must be less than 6 as of the Withdrawal Date. The records should match based on both "
                                + "District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic); "
                                + "District Number Current Enrollment (Student End of Year Status); Student Number Identifier, Florida; "
                                + "School Year and Survey Period Code.")),
                new FLValidationRule("SEYS", "4D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W21"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Withdrawal Code", "W21"), null)))),
                        "If Withdrawal Reason is W21 on the Student End of Year Status record, then there must be a matching Prior School Status/Student Attendance record "
                                + "with a Withdrawal Code, PK-12 of W21. The records should be matched on District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; School Year and Survey Period Code")),
                new FLValidationRule("SEYS", "4G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Diploma Type", "^(W07|W27)$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Except Primary",
                                                "^(C|G|H|J|K|O|P|S|V|W)$"), null)))),
                        "If Diploma Type = W07 or W27, then Exceptionality, Primary on the Exceptional Student record must be C, G, H, J, K, O, P, S, V or W. "
                                + "The match between the Student End of Year Status record and the Exceptional Student record should use the following fields: "
                                + "District Number, Current Enrollment; School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Year.")),
                new FLValidationRule("SEYS", "4H", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Drp Exit Option Test", "^(F|P)$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Graduation Option", "8"), null)))),
                        "If the Dropout Prevention: Performance-Based Exit Option Test Results code is F or P, then the Graduation Option on Student Demographic must equal 8. "
                                + "(The Student Demographic Information record is found by matching on both District Number, Current Enrollment and "
                                + "District Number, Current instruction/Service; Student Number Identifier, Florida; Survey Period Code and School Year.)")),
                new FLValidationRule("SEYS", "4I", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Diploma Type", "^(W10|WGA|WGD)$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Graduation Option", "8"), null)))),
                        "If the Diploma Type is W10, WGA or WGD (on the Student End of Year Status record), then Graduation Option on Student Demographic Information record must = 8. "
                                + "(The Student Demographic Information record is found by matching on both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service; Student Number Identifier, Florida; Survey Period Code and School Year.)")),
                new FLValidationRule("SEYS", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("District Number", "71"),
                                Restriction.pattern("Grade Level", "^(09|1[0-2])$"))
                                .testThen(Restriction.notEquals("Grade Point Average", "00000"))),
                        "If District Number, Current Enrollment is not 71 and Grade Level = 9-12, then Grade Point Average state, Cumulative must not equal 00000.")),
                new FLValidationRule("SEYS", "5A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Withdrawal Reason", "W21"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Action Code", "E"), null)))),
                        "If Withdrawal Reason is W21 on the Student End of Year Status record, then there must be a matching Student Discipline/Referral Action record "
                                + "with a Disciplinary/Referral Action code of E. The records should be matched on District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; School Year and Survey Period Code.")),
                // TODO Rule #80 less than 20 percent of records should be reported with Grade Point Average
                // State, Cumulative equal to 99999.
        });
    }
}
