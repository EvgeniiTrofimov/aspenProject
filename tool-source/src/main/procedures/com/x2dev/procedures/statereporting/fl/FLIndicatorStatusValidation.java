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
import java.util.Arrays;
import java.util.List;


/**
 * The Class FLIndicatorStatusValidation.
 */
public class FLIndicatorStatusValidation {


    /**
     * Instantiates a new FL indicator status validation.
     */
    public FLIndicatorStatusValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("FED", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("FED", "2", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("School Number",
                                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N998|N999)$"))),
                        "If Survey Period Code is 2, 3 or 5, School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, "
                                + "or it must be N998 or N999.")),
                new FLValidationRule("FED", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("FED", "4", new ValidateRegularExpression(
                        "Survey Period", "^(2|3|5)$",
                        "Survey Period Code must be 2, 3 or 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("FED", "5", new ValidateFiscalYear()),
                new FLValidationRule("FED", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.uniqueValue("Student Number", "District Number",
                                        "Survey Period", "Fiscal Year"))),
                        "Each Federal/State Indicator record must be unique based on the keys of District Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code; and Fiscal Year.")),
                new FLValidationRule("FED", "8", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "3"))
                                .testThen(Restriction.pattern("Fed Connected Ind", "^(A|B|C|Z)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|5)$"))
                                .testThen(Restriction.equals("Fed Connected Ind", "Z"))),
                        "For Survey Period Code 3, Federally Connected Student Indicator code must be A, B, C or Z. "
                                + "For Survey Period Codes 2 and 5 Federally Connected Student Indicator code must be Z.")),
                new FLValidationRule("FED", "9",
                        new ValidateRegularExpression("Medical Exemption", "^(A|B|C|D|E|Z)$",
                                "Medical Complexity Exemption code must be A, B, C, D, E or Z.")),
                new FLValidationRule("FED", "10",
                        new ValidateRegularExpression("Harassed Religion", "^(Y|N|Z)$",
                                "Bullied or Harassed - Religion code must be Y, N or Z.")),
                new FLValidationRule("FED", "11",
                        new ValidateRegularExpression("Harassed Orientation", "^(Y|N|Z)$",
                                "Bullied or Harassed - Sexual Orientation code must be Y, N or Z.")),
                new FLValidationRule("FED", "12",
                        new ValidateRegularExpression("Immigrant Student", "^(Y|N|Z)$",
                                "Immigrant Student code must be Y, N or Z.")),
                new FLValidationRule("FED", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Immigrant Student", "Z"))
                                .testThen(Restriction.equals("School Number", "3518"))),
                        "If Immigrant Student code is Z, then the School Number, Current Enrollment must be 3518.")),
                new FLValidationRule("FED", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.equals("Dropout Prevention", "Z")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Dropout Prevention", "^(A|D|E|J|N|P|R|U|W|Z)$"))),
                        "For Survey Period Codes 2 and 3, Dropout Prevention/Juvenile Justice Programs must be A, D, E, J, N, P, R, U, W or Z. "
                                + "For Survey Period Code 5, Dropout Prevention/Juvenile Justice Programs must be Z.")),
                new FLValidationRule("FED", "15",
                        new ValidateRegularExpression("Student Number", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("FED", "20",
                        new ValidateRegularExpression("Test Accommodations", "^(A|C|D|I|L|M|P|Q|R|S|T|U|V|X|Y|Z)$",
                                "Test Accommodations code must be A, C, D, I, L, M, P, Q, R, S, T, U, V, X, Y or Z.")),
                new FLValidationRule("FED", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("Immunization Status", "^[0-4]|8|W|X|Y$"))),
                        "For Survey Period Codes 2, 3 and 5, Immunization Status must be 0, 1, 2, 3, 4, 8, W, X or Y.")),
                new FLValidationRule("FED", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.or(
                                        Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "CAPE ID (2)"),
                                        Restriction.equals("CAPE ID (2)", "ZZZ")))),
                        "Career and Professional Academy Identifier (Second) must exist on Appendix Y of the DOE Information Data Base Requirements: Volume I "
                                + "Automated Student Information System Manual as a valid number for the District Number, Current Instruction/Service: "
                                + "CAPE (Second) or it must be ZZZ.")),
                new FLValidationRule("FED", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.notEquals("CAPE ID (1)", "ZZZ"),
                                Restriction.notEquals("CAPE ID (2)", "ZZZ")))
                                .testThen(Restriction.and(
                                        Restriction.equalsFieldValue("CAPE ID (1)", "CAPE ID (2)",
                                                String.class),
                                        Restriction.notEqualsFieldValue("CAPE District (1)", "CAPE District (2)",
                                                String.class)))),
                        "The combination of District Number, Current Instruction/Service: CAPE (First) and Career and Professional Academy Identifier (First) must not be "
                                + "the same as the combination of District Number, Current Instruction/Service: CAPE (Second) and Career and Professional Academy Identifier (Second) "
                                + "unless both Career and Professional Academy Identifiers are ZZZ.")),
                new FLValidationRule("FED", "24", new FLValidationRuleSet(new RuleSet(
                        ValidationRule
                                .testIf(Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "CAPE ID (2)"))
                                .testThen(Restriction.notEquals("CAPE ID (1)", "ZZZ"))),
                        "Career and Professional Academy Identifier (First) must not be ZZZ if Career and Professional Academy Identifier (Second) "
                                + "is entered with a valid number that exists on Appendix Y of the DOE Information Data Base Requirements: Volume I "
                                + "Automated Student Information System Manual.")),
                new FLValidationRule("FED", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("Homelessness Cause", "^(D|E|F|H|M|N|O|S|T|U|W|Z)$"))),
                        "If Survey Period Code is 2, 3 or 5, Homelessness Cause code must be D, E, F, H, M, N, O, S, T, U, W or Z.")),
                new FLValidationRule("FED", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Homeless Student", "^(A|B|D|E)$"))
                                .testThen(Restriction.pattern("Homelessness Cause", "^(D|E|F|H|M|N|O|S|T|U|W)$")),
                        ValidationRule.testIf(Restriction.equals("Homeless Student", "N"))
                                .testThen(Restriction.equals("Homelessness Cause", "Z"))),
                        "If Homeless Student, PK-12 code is A, B, D, E, Homelessness Cause must be D, E, F, H, M, N, O, S, T, U or W. "
                                + "If Homeless Student, PK-12 code is N, Homelessness Cause must be Z.")),
                new FLValidationRule("FED", "27", new ValidateRegularExpression(
                        "CAPE District (1)", "^0[0-9]|[1-6][0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service: CAPE (First) must be numeric, in the range 00-68 or 71-75.")),
                new FLValidationRule("FED", "28", new ValidateRegularExpression(
                        "CAPE District (2)", "^0[0-9]|[1-6][0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service: CAPE (Second) must be numeric, in the range 00-68 or 71-75.")),
                new FLValidationRule("FED", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("CAPE ID (1)", "ZZZ"))
                                .testThen(Restriction.notEquals("CAPE District (1)", "00")),
                        ValidationRule.testIf(Restriction.notEquals("CAPE District (1)", "00"))
                                .testThen(Restriction.notEquals("CAPE ID (1)", "ZZZ"))),
                        "If Career and Professional Academy Identifier (First) is not ZZZ then District Number, Current Instruction/Service: CAPE (First) must not be 00. "
                                + "If District Number, Current Instruction/Service: CAPE (First) is not 00 then Career and Professional Academy Identifier (First) must not be ZZZ.")),
                new FLValidationRule("FED", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("CAPE ID (2)", "ZZZ"))
                                .testThen(Restriction.notEquals("CAPE District (2)", "00")),
                        ValidationRule.testIf(Restriction.notEquals("CAPE District (2)", "00"))
                                .testThen(Restriction.notEquals("CAPE ID (2)", "ZZZ"))),
                        "If Career and Professional Academy Identifier (Second) is not ZZZ then District Number, Current Instruction/Service: CAPE (Second) must not be 00. "
                                + "If District Number, Current Instruction/Service: CAPE (Second) is not 00 then Career and Professional Academy Identifier (Second) must not be ZZZ.")),
                new FLValidationRule("FED", "31",
                        new ValidateRegularExpression("Arrest", "^(Y|N)$",
                                "School-Related Arrests code must be Y or N.")),
                new FLValidationRule("FED", "39", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("Section 504 Eligible", "^(Y|N|I|Z)$"))),
                        "For Survey Period Codes 2, 3 and 5, the Section 504 Eligible code must be Y, N, I or Z.")),
                new FLValidationRule("FED", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(?!N99[89])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, then it must exist on the Master School Identification File as "
                                + "a valid active school number in the district of enrollment.")),
                new FLValidationRule("FED", "41", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("Homeless Student", "^(A|B|D|E|N)$"))),
                        "For Survey Period Codes 2, 3 and 5, Homeless Student, PK-12 code must be A, B, D, E or N.")),
                new FLValidationRule("FED", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("Unaccompanied Youth", "^(Y|N|Z)$"))),
                        "For Survey Period Codes 2, 3 and 5, Homeless Unaccompanied Youth code must be Y, N, or Z.")),
                new FLValidationRule("FED", "43", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Homeless Student", "^(A|B|D|E)$"))
                                .testThen(Restriction.pattern("Unaccompanied Youth", "^(Y|N)$")),
                        ValidationRule.testIf(Restriction.equals("Homeless Student", "N"))
                                .testThen(Restriction.equals("Unaccompanied Youth", "Z"))),
                        "If Homeless Student, PK-12 code is A, B, D or E, Unaccompanied Youth must be Y or N. If Homeless Student, PK-12 code is N,"
                                + " Unaccompanied Youth must be Z.")),
                new FLValidationRule("FED", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.equals("Fund Source", "Z")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.pattern("Fund Source", "^(I|Z)$"))),
                        "For Survey Period Code 5, Fund Source code must be I or Z. For Survey Period Codes 2 or 3, Fund Source code must be Z.")),
                new FLValidationRule("FED", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.byExtFldRefTable("FL-PGM-CAPE", "pgm-cape-district", "CAPE ID (1)"),
                                        Restriction.equals("CAPE ID (1)", "ZZZ")))),
                        "Career and Professional Academy Identifier (First) must exist on Appendix Y of the DOE Information Data Base Requirements: Volume I "
                                + "Automated Student Information System Manual as a valid number for the District Number, Current Instruction/Service: "
                                + "CAPE (First) or it must be ZZZ.")),
                new FLValidationRule("FED", "46",
                        new ValidateRegularExpression("Military Family", "^(Y|N|Z)$",
                                "Military Family Student code must be Y, N or Z.")),
                new FLValidationRule("FED", "48", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|5)$"))
                                .testThen(Restriction.pattern("PE Waiver", "^(Y|N|Z)$"))),
                        "If Survey Period Code is 2, 3 or 5, then the Physical Education Waiver code must be Y, N or Z.")),
                new FLValidationRule("FED", "49",
                        new ValidateRegularExpression("Harassed Disability", "^(Y|N|Z)$",
                                "Bullied or Harassed - Disability code must be Y, N or Z.")),
                new FLValidationRule("FED", "4A",
                        new ValidateRegularExpression("Harassed Race", "^(Y|N|Z)$",
                                "Bullied or Harassed - Race code must be Y, N or Z.")),
                new FLValidationRule("FED", "4B",
                        new ValidateRegularExpression("Harassed Sex", "^(Y|N|Z)$",
                                "Bullied or Harassed - Sex code must be Y, N or Z.")),
                new FLValidationRule("FED", "4C", new ValidateRegularExpression("Fl Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("FED", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Federal/State Indicator Status record must have a matching Student Demographic record based on both District Number, "
                                + "Current Enrollment and District Number, Current Instruction/Service (Student Demographic) and District Number, "
                                + "Current Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Grade Level", "KG"), null)),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.pattern("Immunization Status", "^[0-4]|W|X|Y$"))),
                        "If Survey Period Code is 2 or 3 and Grade Level is KG on the matching Student Demographic Information record and if there is at least one "
                                + "matching Student Course Schedule record for the student, then the Immunization Status code must be 0, 1, 2, 3, 4, W, X or Y. "
                                + "The Student Demographic Information and Student Course Schedule matches are based on both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service (Student Demographic and Student Course Schedule) and District Number, Current Enrollment (Federal/State Indicator Status); "
                                + "Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("CAPE ID (1)", "ZZZ"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level",
                                                "^(0[6-9]|1[0-2])$"), null)))),
                        "If Career and Professional Academy Identifier (First) is not ZZZ then Grade Level on the matching Student Demographic Information record "
                                + "must be 06-12. The match is based on both District Number, Current Enrollment and District Number, Current Instruction/Service "
                                + "(Student Demographic) and District Number, Current Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; "
                                + "Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "59", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout Prevention", "D"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level",
                                                "^(?!PK|KG)\\S{2}$"), null)))),
                        "If Dropout Prevention/Juvenile Justice Programs code is D then Grade Level on the matching Student Demographic Information record "
                                + "must not equal PK or KG. The match is based on both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service (Student Demographic) and District Number, Current Enrollment (Federal/State Indicator Status); "
                                + "Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.equals("Grade Level", "PK"), null)))
                                .testThen(Restriction.equals("Military Family", "Z"))),
                        "If Grade Level on the Student Demographic Information record is PK, then Military Family Student code must be Z. "
                                + "The records should be matched using the following elements: District Number, Current Enrollment (Federal/State Indicator Status) "
                                + "and both District Number, Current Instruction/Service and District Number, Current Enrollment (Student Demographic Information); "
                                + "Fiscal Year/Year; Survey Period Code and Student Number Identifier, Florida.")),
                new FLValidationRule("FED", "61", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Grade Level", "07"), null)),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.pattern("Immunization Status", "^(0|[2-4]|8|W|X|Y)$"))),
                        "If Survey Period Code is 2 or 3 and Grade Level is 07 on the matching Student Demographic Information record and if there is at least one "
                                + "matching Student Course Schedule record for the student, then Immunization Status code must be 0, 2, 3, 4, 8, W, X or Y. "
                                + "The Student Demographic Information and Student Course Schedule matches are based on both District Number, Current Enrollment and "
                                + "District Number, Current Instruction/Service (Student Demographic and Student Course Schedule) and "
                                + "District Number, Current Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; "
                                + "Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "63", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.pattern("Grade Level", "^(09|1[0-2])$"), null)))
                                .testThen(Restriction.equals("PE Waiver", "Z")),
                        ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                new KeyValuePair("Student Number", "Student Number"),
                                new KeyValuePair("District Number", "District Number, I/S"),
                                new KeyValuePair("Survey Period", "Survey Period"),
                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                new KeyValuePair(Restriction.pattern("Grade Level", "^(0[1-8]|KG)$"), null)))
                                .testThen(Restriction.pattern("PE Waiver", "^(Y|N)$"))),
                        "If Grade Level equals PK or 9-12 on the matching Student Demographic Information record, then the Physical Education Waiver code must be Z. "
                                + "If Survey Period Code is 2, 3 or 5 and if Grade Level equals KG-8 on the matching Student Demographic Information record, "
                                + "then the Physical Education Waiver code must be Y or N. The match should be made using the following elements: District Number, Current Enrollment "
                                + "(Federal/State Indicator Status) and both District Number, Current Enrollment and District Number, Current Instruction/Service "
                                + "(Student Demographic Information); Survey Period Code; Year and Student Number Identifier, Florida.")),
                new FLValidationRule("FED", "64", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Immigrant Student", "Y"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Country of Birth",
                                                "^(?!US|PR)\\S{2}$"), null)))),
                        "If the Immigrant Student = Y, then the Country of Birth on the Student Demographic record must not be US or PR. The records should be matched on the "
                                + "following elements: District Number, Current Enrollment (Federal/State Indicator Status) and both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service (Student Demographic); Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "66", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout Prevention", "P"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level",
                                                "^(0[1-4]|KG)$"), null)))),
                        "If Dropout Prevention/Juvenile Justice Programs code is P, then Grade Level on the matching Student Demographic Information record must not be KG-04. "
                                + "The match is based on both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic) and District Number, "
                                + "Current Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "67", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.or(
                                                Restriction.pattern("Except Primary", "^(?!L|Z)\\S$"),
                                                Restriction.pattern("Exceptionality Other", "^(?!L|Z)\\S$")), null)))
                                .testThen(Restriction.equals("Section 504 Eligible", "Z"))),
                        "If Survey Period Code is 2 or 3 and there is a matching Exceptional Student record with an Exceptionality, Primary or Exceptionality, Other code "
                                + "other than L and Z; then the Section 504 Eligible code must be Z. The match is based on District Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Survey Period Code and Fiscal Year/Year.")),
                new FLValidationRule("FED", "81", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout Prevention", "P"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level",
                                                "^(05|06)$"), null)))),
                        "If Dropout Prevention/Juvenile Justice Programs code is P, then Grade Level on the matching Student Demographic Information record must not be 05 or 06. "
                                + "The match is based on both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic) and "
                                + "District Number, Current Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("FED", "82", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.or(
                                                Restriction.pattern("Except Primary", "^(?!L|Z)\\S$"),
                                                Restriction.pattern("Exceptionality Other", "^(?!L|Z)\\S$")), null)))
                                .testThen(Restriction.equals("Section 504 Eligible", "Z"))),
                        "If Survey Period Code is 5 and there is a matching Exceptional Student record with an Exceptionality, Primary or Exceptionality, Other code other than L and Z; "
                                + "then the Section 504 Eligible code must be Z. The match is based on District Number, Current Enrollment; Student Number Identifier, Florida; "
                                + "Survey Period Code and Fiscal Year/Year.")),
                // TODO Rule #91
        });
    }
}
