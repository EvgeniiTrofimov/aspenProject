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
 * The Class FLStudentCourseScheduleValidation.
 */
public class FLStudentCourseScheduleValidation {


    /**
     * Instantiates a new FL student course schedule validation.
     */
    public FLStudentCourseScheduleValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SSC", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("SSC", "2", new ValidateRegularExpression("School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N998|N999)$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("SSC", "3", new FLValidationRuleSet(
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
                new FLValidationRule("SSC", "4", new ValidateRegularExpression(
                        "Survey Period", "^[1-4]$",
                        "Survey Period Code must be 1, 2, 3, or 4 and it must be correct for the submission specified by the district.")),
                new FLValidationRule("SSC", "5", new ValidateFiscalYear()),
                // TODO schools where the Charter School Status is not Z and School Function/
                // Setting equals V on the Master School Identification file)
                new FLValidationRule("SSC", "7", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "1"))
                                .testThen(Restriction.or(
                                        Restriction.pattern("Instruct School",
                                                "^(?!7001|7004|7006|7023|9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|9996)$"),
                                        Restriction.pattern("Instruct School",
                                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$"))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"))
                                .testThen(Restriction.or(
                                        Restriction.pattern("Instruct School",
                                                "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|9996|N999)|"
                                                        + "(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$"),
                                        Restriction.equals("Course Number", "2222222"))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "4"))
                                .testThen(Restriction.or(
                                        Restriction.pattern("Instruct School",
                                                "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|9996)$"),
                                        Restriction.pattern("Instruct School",
                                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$")))),
                        "If Survey Period Code is 1, then School Number, Current Instruction/Service must be "
                                + "numeric in the range 0001-9899 or 9996 (excluding 7001, 7004, 7006, 7023, 9001; "
                                + "and excluding schools where the Charter School Status is not Z "
                                + "and School Function/ Setting equals V on the Master School Identification file), or "
                                + "School Number, Current Instruction/Service must be C901-C928, U970-U981, or P001-P999."
                                + "If Survey Period Code = 2 or 3, then School Number, Current Instruction/Service must be "
                                + "numeric in the range 0001-9899 (excluding 9001), 9996, C901-C928, U970-U981, P001-P999, or "
                                + "School Number, Current Instruction/Service may be a private school number."
                                + "If Survey Period Code is 4, then School Number, Current Instruction/Service must be "
                                + "numeric in the range 0001-9899 (excluding 9001), 9996, , or "
                                + "School Number, Current Instruction/Service must be C901-C928, U970-U981, or P001-P999.")),
                new FLValidationRule("SSC", "9", new ValidateRegularExpression(
                        "Course Number", "^\\S{1,7}$",
                        "Course Number must not contain blanks.")),
                new FLValidationRule("SSC", "10", new ValidateRegularExpression(
                        "Section Number", "^[0-9|A-Z|\\-$#&%/:]{1,5}$",
                        "Section Number must not be all blanks. "
                                + "Allowable characters are 0-9, A-Z, space, hyphen (-), dollar sign ($), pound sign (#), ampersand (&), percent (%), forward slash (/) and colon (:).")),
                new FLValidationRule("SSC", "11", new ValidateRegularExpression(
                        "Period Number", "^\\d{1,4}$",
                        "Period Number must be numeric and greater than or equal to zero.")),
                new FLValidationRule("SSC", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(0[1-9]|1[0-2])$"))
                                .testThen(Restriction.pattern("Course Number",
                                        "^(?!\\d{1,4}980|\\d{1,4}990)\\d{1,7}|0500980$")),
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Grade Level", "^(30|31)$"))
                                .testThen(Restriction.pattern("Course Number", "^(?![A-Z]0{6})\\d{1,7}$")),
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\S{1,3})\\S{1,4}$"))
                                .testThen(Restriction.pattern("Course Number", "^(?!00\\S{1,5})\\S{1,7}$"))),
                        "If the School Number Current Instruction/Service does not begin with P the Course Number must not be a local use only transfer"
                                + " course number unless Course Number = 0500980. If the School Number Current Instruction/Service does not begin with P "
                                + "then the Course Number must not begin with 00. Local use only transfer course numbers for Grade Levels 09-12 are numeric course numbers "
                                + "that end in 980 or 990. Local use only transfer course numbers for Grade Levels 30-31 are alphanumeric course numbers "
                                + "that begin with one alpha character and end with six zeroes.")),
                new FLValidationRule("SSC", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("Student Number", "Instruct District",
                                        "Survey Period", "Fiscal Year", "Instruct School", "Course Number",
                                        "Section Number", "Period Number", "Term"))),
                        "Each Student Course Schedule record must be unique based on Student Number Identifier, Florida; Survey Period Code; Fiscal Year; District Number, "
                                + "Current Instruction/Service; School Number, Current Instruction/Service; Course Number; Section Number; Period Number; and Term.")),
                new FLValidationRule("SSC", "15",
                        new ValidateRegularExpression("Dual Enrollment", "^(A|B|C|E|Z)$",
                                "Dual Enrollment Indicator must be A, B, C, E, or Z.")),
                // TODO Uncomment rule 16, need ref table Course Number
                /*new FLValidationRule("SSC", "16", new FLValidationRuleSet(new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern("Instruct School", "^(?!N999|(?!P000)P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "All alphanumeric Course Numbers must either be on the Course Code Directory or on the Statewide Course Numbering System file "
                                + "unless School Number, Current Instruction/Service equals N999 or P001-P999.")),*/
                // TODO Rule 17 Courses That Do Not Generate FTE file (F71424),
                new FLValidationRule("SSC", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^(0800300|8502000)$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "(102|103|112|113|254|255|999)"))),
                        "If Course Number equals 0800300 or 8502000, then FEFP Program Number must equal 102, 103, 112, 113, 254, 255, or 999.")),
                new FLValidationRule("SSC", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Instruct School", "N999"))
                                .testThen(Restriction.equals("Course Number", "2222222")),
                        ValidationRule.testIf(Restriction.equals("Course Number", "2222222"))
                                .testThen(Restriction.equals("Instruct School", "N999"))),
                        "If Course Number equals 2222222, then School Number, Current Instruction/Service must be a private school number and vice versa")),
                new FLValidationRule("SSC", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Dual Enrollment", "^(A|B|C|E)$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "(102|103|112|113)"))),
                        "FEFP Program Number must be 102, 103, 112 or 113 if Dual Enrollment Indicator code = A, B, C, or E.")),
                new FLValidationRule("SSC", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "A"))
                                .testThen(Restriction.pattern("Instruct School",
                                        "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$"))),
                        "If Dual Enrollment Indicator equals A, then School Number, Current Instruction/Service must be C901-C928, U970-U981 or P001-P999.")),
                new FLValidationRule("SSC", "22", new ValidateRegularExpression(
                        "Class Minutes", "^\\d{1,4}$",
                        "Class Minutes, Weekly must be numeric and greater than or equal to zero.")),
                new FLValidationRule("SSC", "23", new ValidateRegularExpression(
                        "FEFP Program Number", "^(10[1-3]|11[1-3]|130|25[45]|300|999)$",
                        "FEFP Program Number must be 101-103, 111-113, 130, 254-255, 300, or 999.")),
                new FLValidationRule("SSC", "24", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThan("FTE Reported Course", Double.valueOf(0)),
                                        Restriction.lessThanOrEquals("FTE Reported Course", Double.valueOf(1667)))),
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("FTE Reported Course", "^\\d{1,4}$"))),
                        "FTE Reported, Course must be numeric and greater than or equal to zero. If Period Number is 9800, FTE Reported, Course must be "
                                + "greater than zero and less than or equal to 1667")),
                new FLValidationRule("SSC", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.equals("Reading Intervention", "Z"))),
                        "If Grade Level on the Student Course Schedule record is PK, the Reading Intervention Component code must be Z.")),
                new FLValidationRule("SSC", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"))
                                .testThen(Restriction.pattern("Reading Intervention", "^(Y|N|Z)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1|4]$"))
                                .testThen(Restriction.equals("Reading Intervention", "Z"))),
                        "If Survey Period Code = 2 or 3, Reading Intervention Component code must be Y, N, or Z. If Survey Period Code = 1 or 4, "
                                + "Reading Intervention Component code must be Z.")),
                new FLValidationRule("SSC", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number",
                                "^(5100520|5100530|5100560|5100570|5100580|5100590)$"))
                                .testThen(Restriction.equals("Grade Level", "PK"))),
                        "If Course Number is 5100520, 5100530, 5100560, 5100570, 5100580 or 5100590 Grade Level must be PK")),
                new FLValidationRule("SSC", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Instruct School", "9996"))
                                .testThen(Restriction.equals("School Number", "N999"))),
                        "If School Number, Current Instruction/Service equals 9996, then School Number, Current Enrollment must be N999.")),
                new FLValidationRule("SSC", "29", new ValidateRegularExpression(
                        "Grade Level", "^(PK|KG|0[1-9]|1[0-2])$", "Grade Level must be PK, KG, or 01-12.")),
                new FLValidationRule("SSC", "2A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "A"),
                                Restriction.pattern("Instruct School", "^(?!(?!P000)P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.pattern("Course Number", "^[A-Z]\\d{1,6}$"))),
                        "If Dual Enrollment Indicator is A and if School Number, Current Instruction/Service is not P001-P999 then "
                                + "Course Number must start with an alphabetic character.")),
                new FLValidationRule("SSC", "2B", new ValidateRegularExpression(
                        "Course Grade",
                        "^(\\s{2}([A-D]|F|I|N|U|P|S|E|W|Z)|\\s(A[+-]|B[+-]|C[+-]|D[+-]|IP|WP|FL|NG|WF))$",
                        "The Course Grade code must be A+, A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F, I, IP, N, U, P, S, E, WP, FL, NG, W, WF or Z "
                                + "and must be right justified with leading blanks.")),
                // TODO School Function/Setting equals V on the Master School Identification file
                new FLValidationRule("SSC", "2C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "Z"),
                                Restriction.or(
                                        Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                        Restriction.equals("Instruct District", "71"),
                                        Restriction.and(
                                                Restriction.equals("Location of Student", "T"),
                                                Restriction.pattern("Instruct District", "^0[1-8]|[1-6][0-9]|7[2-5]$"),
                                                Restriction.pattern("Instruct School",
                                                        "^(?!7001|7004|7006|7023)\\S{1,4}$"))))
                                .testThen(Restriction.notEquals("Course Grade", "  Z"))),
                        "If Dual Enrollment Indicator = Z and School Number, Current Instruction/Service = 7001, 7004, 7006 or 7023;"
                                + "or District Number, Current Instruction/Service = 71; "
                                + "or if School Number, Current Instruction/Service has a Charter School Status is not Z "
                                + "and School Function/Setting equals V on the Master School Identification file,"
                                + "or Location of Student equals T; and District Number, Current Instruction equals 01-68 or 72-75; "
                                + "and School Number, Current Instruction not equal 7001, 7004, 7006, or 7023;"
                                + "then Course Grade may not equal Z. All other schools must equal Z")),
                // TODO Non-fundable Automotive Service Technology file (F71340)
                new FLValidationRule("SSC", "2D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("Course Number",
                                        "^(8709410|8709420|8709430|8709440|8709450|8709460|8709470|8709480|8709490|"
                                                + "8709491|8709492|8709493|9504110|9504120|9504130|9504140|9504150|9504160)$"))
                                .testThen(Restriction.equals("FTE Reported Course", "0000"))),
                        "If Survey Period Code is 1, 2, 3 or 4; and Course Number is 8709410, 8709420, 8709430, 8709440, 8709450, 8709460, 8709470, "
                                + "8709480, 8709490, 8709491, 8709492, 8709493, 9504110, 9504120, 9504130, 9504140, 9504150, or 9504160;"
                                + " and District Number, Current Instruction/Service and School Number, Current Instruction/Service exists on "
                                + "the Non-fundable Automotive Service Technology file (F71340),then FTE Reported, Course must be .0000.")),
                // TODO Rule 2E Master School Identification file
                // TODO School Function/Setting equals V on the Master School Identification file
                new FLValidationRule("SSC", "2F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "Z"),
                                Restriction.equals("Course Number", "2222222"),
                                Restriction.or(
                                        Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                        Restriction.equals("Instruct District", "71"),
                                        Restriction.and(
                                                Restriction.equals("Location of Student", "T"),
                                                Restriction.pattern("Instruct District",
                                                        "^(0[1-8]|[1-6][0-9]|7[2-5])$"),
                                                Restriction.pattern("Instruct School",
                                                        "^(?!7001|7004|7006|7023)\\S{1,4}$")),
                                        Restriction.pattern("Course Grade",
                                                "^(\\s{2}(F|I|N|U|W)|\\s(IP|WP|FL|NG|WF))$")))
                                .testThen(Restriction.equals("FTE Reported Course", "0000"))),
                        "If Dual Enrollment Indicator = Z, Course Number does not equal 2222222"
                                + " and School Number, Current Instruction/Service = 7001, 7004, 7006 or 7023;"
                                + " or District Number, Current Instruction/Service = 71,"
                                + " or if School Number, Current Instruction/Service has a Charter School Status is not Z and School Function/Setting equals V "
                                + "on the Master School Identification file,"
                                + " or Location of Student equals T; and District Number, Current Instruction equals 01-68 or 72-75; "
                                + "and School Number, Current Instruction not equal 7001, 7004, 7006, or 7023;"
                                + " and Course Grade is F, I, IP, N, U, WP, FL, NG, W, or WF;"
                                + "then FTE Reported, Course must be .0000.")),
                new FLValidationRule("SSC", "2G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Instruct School", "7006"))
                                .testThen(Restriction.byAliasFldRefTable("all-mst-OnlineProvider", "Online Provider")),
                        ValidationRule.testIf(Restriction.notEquals("Instruct School", "7006"))
                                .testThen(Restriction.equals("Online Provider", "ZZZ"))),
                        "If School Number, Current Instruction/Service is 7006, then the Online Course Provider code must be a valid code in Appendix GG. "
                                + "If School Number, Current Instruction/Service is not 7006, then the Online Course Provider code must be ZZZ.")),
                // TODO School Function/Setting equals V on the Master School Identification file
                new FLValidationRule("SSC", "2H", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "4"),
                                Restriction.or(
                                        Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                        Restriction.and(
                                                Restriction.equals("Instruct District", "71"),
                                                Restriction.pattern("Instruct School", "^(0300|0400)$")),
                                        Restriction.and(
                                                Restriction.equals("Location of Student", "T"),
                                                Restriction.pattern("Instruct District",
                                                        "^(0[1-8]|[1-6][0-9]|7[2-5])$"),
                                                Restriction.pattern("Instruct School",
                                                        "^(?!7001|7004|7006|7023)\\S{1,4}$"))))
                                .testThen(Restriction.notEquals("Course Grade", " IP"))),
                        "If Survey Period = 4"
                                + " and School Number, Current Instruction/Service = 7001, 7004, 7006 or 7023;"
                                + " or if District Number, Current Instruction/Service = 71 and School Number, Current Instruction/Service = 0300 or 0400;"
                                + " or School Number, Current Instruction/Service has a Charter School Status other than Z and School Function/Setting equals V "
                                + "on the Master School Identification file,"
                                + " or Location of Student equals T; and District Number, Current Instruction equals 01-68 or 72-75; and "
                                + "School Number, Current Instruction not equal 7001, 7004, 7006, or 7023;"
                                + "then Course Grade must not equal IP.")),
                // TODO School Function/Setting equals V on the Master School Identification file
                new FLValidationRule("SSC", "2I", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.or(
                                Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                Restriction.equals("Instruct District", "71"),
                                Restriction.and(
                                        Restriction.equals("Location of Student", "T"),
                                        Restriction.pattern("Instruct District", "^(0[1-8]|[1-6][0-9]|7[2-5])$"),
                                        Restriction.pattern("Instruct School",
                                                "^(?!7001|7004|7006|7023)\\S{1,4}$"))))
                                .testThen(Restriction.notEquals("Grade Level", "PK"))),
                        "If School Number, Current Instruction/Service = 7001, 7004, 7006 or 7023;"
                                + " or District Number, Current Instruction/Service = 71;"
                                + " or if School Number, Current Instruction/Service has a Charter School Status other than Z "
                                + "and School Function/Setting equals V on the Master School Identification file;"
                                + " or Location of Student equals T; and District Number, Current Instruction equals 01-68 or 72-75; "
                                + "and School Number, Current Instruction not equal 7001, 7004, 7006, or 7023;"
                                + "then Grade Level may not equal PK.")),
                new FLValidationRule("SSC", "2J", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "2222222"),
                                Restriction.equals("School Number", "3518"))
                                .testThen(Restriction.pattern("Survey Period", "^[2|3]$"))),
                        "If Course Number equals 2222222 and School Number, Current Enrollment equals 3518, then Survey Period Code must equal 2 or 3.")),
                // TODO Rule 2L Master School Identification file
                new FLValidationRule("SSC", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.equals("FEFP Program Number", "999"))
                                .testThen(Restriction.equals("FTE Reported Course", "0000"))),
                        "If Survey Period Code = 1-4 and FEFP Program Number is 999, then FTE Reported, Course must be zero.")),
                new FLValidationRule("SSC", "31", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.notEquals("FEFP Program Number", "999"),
                                Restriction.pattern("FEFP Program Number", "^(101|111)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-3])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.notEquals("FEFP Program Number", "999"),
                                Restriction.pattern("FEFP Program Number", "^(102|112)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[4-8])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.notEquals("FEFP Program Number", "999"),
                                Restriction.equals("FEFP Program Number", "130"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.notEquals("FEFP Program Number", "999"),
                                Restriction.pattern("FEFP Program Number", "^25[45]$"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.notEquals("FEFP Program Number", "999"),
                                Restriction.pattern("FEFP Program Number", "^(103|113|300)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(09|1[0-2])$"))),
                        "In surveys 1 through 4 if the FEFP Program Number is not 999, then there must be a valid association between the FEFP Program Number listed below "
                                + "and the student's Grade Level."
                                + "101,111 ---- >PK-03"
                                + "102,112 ---- >04-08"
                                + "130 -------- >KG-12"
                                + "254-255 ---- >PK-12"
                                + "103,113,300  >09-12")),
                new FLValidationRule("SSC", "32", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-6])$"),
                                Restriction.greaterThan("FTE Reported Course", Double.valueOf(0)))
                                .testThen(Restriction.pattern("Instruct School",
                                        "^(?!C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})\\S{1,4}$"))),
                        "If Survey Period Code is 1-4, Grade Level is less than 06 and FTE Reported, Course is greater than zero, "
                                + "then School Number, Current Instruction/Service must not be a college or university (C901-C928, U970-U981 or P001-P999).")),
                new FLValidationRule("SSC", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "5100580"))
                                .testThen(Restriction.pattern("Survey Period", "^[2|3]$"))),
                        "If Course Number is 5100580 then Survey Period code must be 2 or 3.")),
                new FLValidationRule("SSC", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "5100590"))
                                .testThen(Restriction.pattern("Survey Period", "^[1|4]$"))),
                        "If Course Number is 5100590 then Survey Period Code must be 1 or 4.")),
                new FLValidationRule("SSC", "35", new ValidateRegularExpression(
                        "Term", "^([1-9]|[B-O]|[S-X])$", "Term must be either 1-9, B-O or S-X.")),
                new FLValidationRule("SSC", "36", new ValidateRegularExpression(
                        "ELL Instruct Model", "^(E|S|I|C|O|T|Z)$",
                        "English Language Learners: Instructional Model must be E, S, I, C, O, T, or Z.")),
                new FLValidationRule("SSC", "37", new ValidateRegularExpression(
                        "Year-Round Indicator", "^(A|Z)$",
                        "Year-Round/Extended School Year FTE Indicator must be A or Z.")),
                new FLValidationRule("SSC", "38", new ValidateRegularExpression("Student ID Local",
                        "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                // TODO School Function/Setting equals V on the Master School Identification file
                new FLValidationRule("SSC", "39", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"),
                                Restriction.pattern("Dual Enrollment", "(?!B|C)\\S$"),
                                Restriction.or(
                                        Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                        Restriction.and(
                                                Restriction.equals("Instruct District", "71"),
                                                Restriction.pattern("Instruct School", "^(0300|0400)$")),
                                        Restriction.and(
                                                Restriction.equals("Location of Student", "T"),
                                                Restriction.pattern("Instruct District",
                                                        "^(0[1-8]|[1-6][0-9]|7[2-5])$"),
                                                Restriction.pattern("Instruct School",
                                                        "^(?!7001|7004|7006|7023)\\S{1,4}$"))))
                                .testThen(Restriction.equals("FTE Reported Course", "0000"))),
                        "If Survey Period is 2 or 3, and If Dual Enrollment Indicator is not B or C, and"
                                + " District Number, Current Instruction/Service = 71 and School Number, Current Instruction/Service = 0300 or 0400; or"
                                + " if School Number, Current Instruction/Service is"
                                + " 7001 (district virtual instruction program - contracted provider), or"
                                + " 7004 (franchise of Florida Virtual School), or"
                                + " 7006 (KG-12 virtual course offerings), or"
                                + " 7023 (district virtual instruction program - district provider), or"
                                + " Location of Student = T; and District Number, Current Instruction equals 01-68 or 72-75; "
                                + "and School Number Current Instruction not equal 7001, 7004, 7006 or 7023, "
                                + "then FTE Reported, Course must be 0000."
                                + "If Survey Period is 2 or 3 and "
                                + "The School Number, Current Instruction/Service has a School Function Setting of V on the Master School Identification file and"
                                + " The School Number, Current Instruction/Service has a Charter School Status not equal to Z on the Master School Identification file and"
                                + " Dual Enrollment Indicator is not B or C, "
                                + "then FTE Reported, Course must be 0000.")),
                new FLValidationRule("SSC", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(?!N99[89])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, then it must exist on the Master School Identification File "
                                + "as a valid active number in the District Number, Current Enrollment.")),
                // TODO add correct ref table for "all-crs-CTEProgramCode" (Career and Technical
                // Education/Adult
                // General Education Program Edit file (F61730))
                // new FLValidationRule("SSC", "41", new FLValidationRuleSet(new RuleSet(
                // ValidationRule.testIf(Restriction.alwaysTrue())
                // .testThen(Restriction.or(
                // Restriction.byAliasFldRefTable("all-crs-CTEProgramCode", "CTE Program Code"),
                // Restriction.equals("CTE Program Code", "0000000")))),
                // "The Career and Technical Education/Adult General Education Program Code must be
                // a program number from the "
                // + "Career and Technical Education/Adult General Education Program Edit file
                // (F61730) or must be zero-filled.")),
                new FLValidationRule("SSC", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.byActiveSchool("Instruct School"),
                                        Restriction.pattern("Instruct School",
                                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3}|9996)$"),
                                        Restriction.and(Restriction.pattern("Survey Period", "^[2|3]$"),
                                                Restriction.equals("Instruct School", "N999"))))),
                        "School Number, Current Instruction/Service must exist on the Master School Identification File as a valid active number in the "
                                + "District Number, Current Instruction/Service or it must be C901-C928, U970-U981, P001-P999, or 9996. "
                                + "If Survey Period Code = 2 or 3, then School Number, Current Instruction/Service may be a private school number anywhere in Florida. ")),
                // TODO Uncomment rule 43, need ref table Course Number
                /*new FLValidationRule("SSC", "43", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?!P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"),
                                Restriction.equals("Instruct School", "N999"))
                                .testThen(Restriction.equals("Course Number", "2222222"))),
                        "All numeric Course Numbers must be on the Course Code Directory file unless School Number, Current Instruction/Service begins with P. "
                                + "If Survey Period Code = 2, or 3 and School Number, Current Instruction/Service is a private school number, Course Number must be 2222222.")),*/
                new FLValidationRule("SSC", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(PK|KG|0[1-5])$"))
                                .testThen(Restriction.equals("Dual Enrollment", "Z"))),
                        "If Grade Level equals PK-05, then the Dual Enrollment Indicator must equal Z")),
                new FLValidationRule("SSC", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{1,6}$"),
                                Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"),
                                Restriction.pattern("Instruct School", "^(?!P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(102|103|112|113)$"))),
                        "If the Course Number begins with an alpha character and is on the Course Code Directory file, and the School Number, Current Instruction/Service "
                                + "does not begin with P, the FEFP Program Number must be 102, 103, 112 or 113.")),
                new FLValidationRule("SSC", "46", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.pattern("Period Number", "^([0-7]\\d|80)([0-7]\\d|80|88)$"),
                                        Restriction.validatePeriodNumber("Period Number"))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "4"))
                                .testThen(Restriction.equals("Period Number", "9800"))),
                        "The first two digits of the Period Number must be 00-80 while the last two digits must be 00-80, or 88 and be greater than or equal "
                                + "to the first two digits. For Survey 4, period 9800 may also be reported.")),
                new FLValidationRule("SSC", "47", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("Dual Enrollment", "Z"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2])$"))),
                        "If Dual Enrollment Indicator code does not equal Z, then Grade Level must equal 06-12.")),
                // TODO School Function/Setting equals T on the Master School Identification file
                new FLValidationRule("SSC", "48", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "E"))
                                .testThen(Restriction.pattern("Instruct School",
                                        "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$"))),
                        "If Dual Enrollment Indicator equals E, then School Number, Current Instruction/Service must be C901-C928, U970-U981 or P001-P999, "
                                + "or a school on the Master School Identification file that has School Function Setting equal to T.")),
                new FLValidationRule("SSC", "49", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.equals("Days Per Week", "0")),
                        ValidationRule.testIf(Restriction.notEquals("Period Number", "9800"))
                                .testThen(Restriction.pattern("Days Per Week", "^[1-7]$"))),
                        "Days Per Week must be 1-7. If Period Number is 9800, Days Per Week must equal zero.")),
                // TODO Rule 4A School Function/Setting equals D on the Master School Identification
                // file
                new FLValidationRule("SSC", "4B", new ValidateRegularExpression(
                        "Location of Student", "^(N|S|T|Z)$",
                        "Location of Student must be N, S, T or Z.")),
                new FLValidationRule("SSC", "4C", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("SSC", "4D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.and(
                                        Restriction.pattern("Course Number",
                                                "^(1200310|1200330|2000310|1206310|2100310)$"),
                                        Restriction.pattern("FEFP Program Number", "^(102|103)$")))),
                        "If Period Number is 9800, Course Number must be 1200310, 1200330, 2000310, 1206310 or 2100310 and FEFP Program Number must be 102 or 103.")),
                // TODO Rule 4E Virtual Online Course Provider Reference file - F71485
                new FLValidationRule("SSC", "4F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.and(
                                        Restriction.equals("Scheduled Monday", "N"),
                                        Restriction.equals("Scheduled Tuesday", "N"),
                                        Restriction.equals("Scheduled Wednesday", "N"),
                                        Restriction.equals("Scheduled Thursday", "N"),
                                        Restriction.equals("Scheduled Friday", "N"),
                                        Restriction.equals("Scheduled Saturday", "N")))),
                        "If Period Number is 9800, then all of the following must be N: Day of Week Scheduled, Monday, Day of Week Scheduled, Tuesday, "
                                + "Day of Week Scheduled, Wednesday, Day of Week Scheduled, Thursday, Day of Week Scheduled, Friday, Day of Week Scheduled, Saturday.")),
                new FLValidationRule("SSC", "4G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.equals("Scheduled Certain", "Z"))),
                        "If Period Number is 9800, Day of Week Scheduled, Date Certain must be Z")),
                // TODO School Function/Setting = V on the Master School Identification file,
                new FLValidationRule("SSC", "4H", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.or(
                                Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                Restriction.equals("Instruct District", "71"),
                                Restriction.and(
                                        Restriction.equals("Instruct School", "7079"),
                                        Restriction.equals("Instruct District", "50"))))
                                .testThen(Restriction.pattern("Location of Student", "^(N|S)$")),
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern("Location of Student", "^(T|Z)$"))),
                        "If School Number Current Instruction/Service"
                                + " equals 7001, 7004, 7006, or 7023;"
                                + " or if District Number Current Instruction/Service equals 71;"
                                + " or District Number, Current Instruction/Service=50 and School Number, Current Instruction/Service = 7079;"
                                + " or Charter School Status is not Z and School Function/Setting = V on the Master School Identification file, "
                                + "then Location of Student must be N or S. All other schools must be T or Z.")),
                new FLValidationRule("SSC", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "2400200"))
                                .testThen(Restriction.and(
                                        Restriction.equals("FEFP Program Number", "999"),
                                        Restriction.equals("FTE Reported Course", "0000")))),
                        "If Course Number is 2400200 (M/J Homeroom) then FEFP must equal 999 and FTE Reported, Course must be 0000.")),
                new FLValidationRule("SSC", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("Grade Level", "^(0[7-9]|1[0-2])$"))
                                .testThen(Restriction.greaterThan("Class Minutes", Double.valueOf(0))),
                        ValidationRule.testIf(Restriction.equals("Period Number", "9800"))
                                .testThen(Restriction.equals("Class Minutes", "0"))),
                        "If Survey Period = 1-4 and the student's Grade Level is 7-12, Class Minutes, Weekly must be greater than zero. "
                                + "If Period Number is 9800, Class Minutes, Weekly must be equal to zero.")),
                new FLValidationRule("SSC", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(N998|N999)$"))
                                .testThen(Restriction.equals("Dual Enrollment", "Z"))),
                        "If the School Number, Current Enrollment is N998 (home education) or N999 (private school), "
                                + "then Dual Enrollment Indicator must be Z.")),
                // TODO Career and Technical Education/Adult General Education Program Edit file (F61730)
                /*new FLValidationRule("SSC", "53", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number",
                                "^(?!1006300|2001310|2001340|2003310|2102360|2102365|2102370|3027010|3027020|2000350|2000360)[0-9]\\S{1,6}$"),
                                Restriction.notEquals("FEFP Program Number", "300"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-CTEProgramCode", "CTE Program Code")),
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]{3}0\\S{1,3}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-CTEProgramCode", "CTE Program Code")),
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{1,6}$"))
                                .testThen(Restriction.equalsFieldValue("CTE Program Code", "Course Number",
                                        String.class))),
                        "If Course Number begins with a number; and"
                                + " is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730); and"
                                + " is not 1006300, 2001310, 2001340, 2003310, 2102360, 2102365, 2102370, 3027010, 3027020, 2000350 or 2000360 "
                                + "with an FEFP Program Number other than 300;"
                                + " then the Career and Technical Education/Adult General Education Program Code must be a valid program number for the Course Number submitted, "
                                + "as listed in file F61730."
                                + "If Course Number begins with three alphabetic characters followed by a zero in the fourth position, and"
                                + " is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730);"
                                + " then the Career and Technical Education/Adult General Education Program Code must be a valid program number for the Course Number submitted, "
                                + "as listed in file F61730."
                                + "If Course Number begins with one alphabetic character, and"
                                + " is a course number from the Career and Technical Education/Adult General Education Program Edit file (F61730);"
                                + " then the Career and Technical Education/Adult General Education Program Code must be the same as the Course Number.")),*/
                new FLValidationRule("SSC", "54", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Virtual Instruction", "^(071|308|309|311)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.equals("Virtual Instruction", "301"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-5])$")),
                        ValidationRule.testIf(Restriction.pattern("Virtual Instruction", "^(302|313)$"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"))),
                        "If the Virtual Instruction Provider code is:"
                                + " 071 (Florida Virtual School) then Grade Level must be 06-12."
                                + " 301 (Florida Connections Academy, LLC) then Grade Level must be KG-05."
                                + " 302 (K12 Florida, LLC) then Grade Level must be KG-12."
                                + " 308 (Somerset Academy, Inc.) then Grade Level must be 06-12."
                                + " 309 (Edgenuity) then Grade Level must be 06-12."
                                + " 311 (Mater Virtual Academy) then Grade Level must be 06-12."
                                + " 313 (District VIP - Florida Connections Academy, LLC) then Grade Level must be KG-12.")),
                new FLValidationRule("SSC", "55", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct District", "^0[1-8]|[1-6][0-9]|7[2-5]$"),
                                Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$"),
                                Restriction.pattern("Term", "^(4|5|S)$"))
                                .testThen(Restriction.pattern("Survey Period", "^(1|4)$")),
                        ValidationRule.testIf(Restriction.equals("Instruct District", "71"),
                                Restriction.pattern("Instruct School", "^0[567]00$"),
                                Restriction.pattern("Term", "^(4|5|S)$"))
                                .testThen(Restriction.pattern("Survey Period", "^[1-4]$")),
                        ValidationRule.testIf(Restriction.and(
                                Restriction.equals("Instruct District", "71"),
                                Restriction.pattern("Instruct School", "^(0300|400)$")),
                                Restriction.and(
                                        Restriction.pattern("Instruct District", "^0[1-8]|[1-6][0-9]|7[2-5]$"),
                                        Restriction.pattern("Instruct School", "^(7001|7004|7006|7023)$")),
                                Restriction.pattern("Term", "^(4|5|S)$"))
                                .testThen(Restriction.equals("Survey Period", "4"))),
                        "If District Number, Current Instruction is 01-68, 72-75 and"
                                + " School Number, Current Instruction is not 7001, 7004, 7006, or 7023 and"
                                + " if Term equals 4, 5, or S, then Survey Period Code must be 1 or 4."
                                + "If District Number, Current Instruction is 71 and"
                                + " School Number, Current Instruction equals 0500, 0600 or 0700 and if Term equals 4, 5, or S,"
                                + "then Survey Period Code must be 1, 2, 3 or 4."
                                + "If District Number, Current Instruction is 71 and School Number, Current Instruction equals 0300 or 400,"
                                + " Or If District Number, Current Instruction is 01-68, 72-75 and if School Number, "
                                + "Current Instruction is 7001, 7004, 7006, or 7023 and if Term equals 4, 5 or S"
                                + "then Survey Period Code must be 4.")),
                new FLValidationRule("SSC", "56", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Grade Level", "PK"),
                                Restriction.pattern("FEFP Program Number", "^(?!111|25[45])\\S{1,3}$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(101|999)$"))),
                        "If Grade Level code is PK and the FEFP Program Number is not 111, 254 or 255, then FEFP Program Number must be 101 or 999.")),
                // TODO Rule 57 Virtual Instruction Provider code, Master School ID file School
                // Function Setting = V
                // TODO Rule 59 Master School Identification file
                new FLValidationRule("SSC", "5B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(0[6-9]|1[0-2])$"),
                                Restriction.pattern("Course Number", "^[A-Z]\\S{1,6}$"))
                                .testThen(Restriction.notEquals("Dual Enrollment", "Z"))),
                        "If Grade Level = 06-12 and Course Number contains an alpha character in the first position, then Dual Enrollment Indicator must be other than Z.")),
                new FLValidationRule("SSC", "5C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("FEFP Program Number", "130"),
                                Restriction.notEquals("Instruct School", "3518"))
                                .testThen(Restriction.pattern("ELL Instruct Model", "^(C|E|I|O|S|T)$"))),
                        "If FEFP Program Number is 130, English Language Learners: Instructional Model code must be C, E, I, O, S, or T "
                                + "unless School Number, Current Enrollment equals 3518.")),
                new FLValidationRule("SSC", "5D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Monday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Monday must be Y or N.")),
                new FLValidationRule("SSC", "5E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Tuesday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Tuesday must be Y or N.")),
                new FLValidationRule("SSC", "5F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Wednesday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Wednesday must be Y or N.")),
                new FLValidationRule("SSC", "5G", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Thursday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Thursday must be Y or N.")),
                new FLValidationRule("SSC", "5H", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Friday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Friday must be Y or N.")),
                new FLValidationRule("SSC", "5I", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Saturday", "^(Y|N)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Saturday must be Y or N.")),
                // TODO School Function Setting of V and a Charter School Status not equal to Z on
                // the Master School Identification file
                new FLValidationRule("SSC", "5K", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.pattern("Instruct School", "^(?!7001|7004|7006|7023)\\S{1,4}$"))
                                .testThen(Restriction.or(Restriction.equals("Scheduled Monday", "Y"),
                                        Restriction.equals("Scheduled Tuesday", "Y"),
                                        Restriction.equals("Scheduled Wednesday", "Y"),
                                        Restriction.equals("Scheduled Thursday", "Y"),
                                        Restriction.equals("Scheduled Friday", "Y"),
                                        Restriction.equals("Scheduled Saturday", "Y")))),
                        "If Survey Period Code is 2 or 3, and If School Number, Current Enrollment does not equal 3518; "
                                + "or the School Number, Current Instruction/Service is not 7001, 7004, 7006 or 7023; or the School Number, "
                                + "Current Instruction/Service has a School Function Setting of V and a Charter School Status not equal to Z "
                                + "on the Master School Identification file,"
                                + "then at least one of the following must be Y: Day of Week Scheduled, Monday; Day of Week Scheduled, Tuesday; Day of Week Scheduled, "
                                + "Wednesday; Day of Week Scheduled, Thursday; Day of Week Scheduled, Friday or Day of Week Scheduled, Saturday.")),
                new FLValidationRule("SSC", "5L", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("Scheduled Certain", "^(Y|N|Z)$"))),
                        "If Survey Period Code is 2 or 3, then Day of Week Scheduled, Date Certain must be Y, N or Z.")),
                new FLValidationRule("SSC", "5M", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.equals("Scheduled Friday", "Y"))
                                .testThen(Restriction.pattern("Scheduled Certain", "^(N|Z)$"))),
                        "If Survey Period code is 2 or 3 and Day of Week, Scheduled Friday is Y, then Day of Week Scheduled, Date Certain must be N or Z.")),
                new FLValidationRule("SSC", "5O", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "2222222"))
                                .testThen(Restriction.equals("School Number", "3518"))),
                        "If Course Number equals 2222222, then School Number, Current Enrollment must be 3518.")),
                new FLValidationRule("SSC", "5P", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Instruct District", "71"),
                                Restriction.pattern("Instruct School", "^0[567]00$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(10[1-3]|11[1-3]|300)$"))),
                        "If District Number, Current Instruction/Service is equal to 71 and School Number, Current Instruction/Service "
                                + "equals 0500, 0600, or 0700 then FEFP Program Number must equal 101, 102, 103, 111, 112, 113 or 300.")),
                new FLValidationRule("SSC", "5Q", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Instruct School", "7004"),
                                Restriction.notEquals("School Number", "7004"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(10[1-3]|11[1-3]|300)$"))),
                        "If School Number, Current Instruction/Service is equal to 7004 and School Number, Current Enrollment is "
                                + "not equal to 7004, then FEFP Program Number must be, 101, 102, 103, 111, 112, 113, or 300.")),
                new FLValidationRule("SSC", "5S", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^(5100580|5100590)$"))
                                .testThen(Restriction.and(
                                        Restriction.equals("FEFP Program Number", "999"),
                                        Restriction.equals("FTE Reported Course", "0000"),
                                        Restriction.equals("Grade Level", "PK")))),
                        "If Course Number is 5100580 or 5100590 then FEFP Program Number must be 999 and FTE Reported, Course must be 0000 and Grade Level must be PK.")),
                // TODO Appendix DD: Courses Eligible for ELL Weighted FTE
                new FLValidationRule("SSC", "5U", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.equals("FEFP Program Number", "130"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "If Survey Period Code is 2 or 3 and if School Number, Current Enrollment is not 3518 and if FEFP Program Number is 130, "
                                + "then the Course Number must be a course in Appendix DD.")),
                new FLValidationRule("SSC", "5V", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|4)$"),
                                Restriction.pattern("ELL Instruct Model", "^(S|C)$"))
                                .testThen(Restriction.pattern("Course Number",
                                        "^(12\\d{5}|20\\d{5}|21\\d{5}|82\\d{5})$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "1"),
                                Restriction.pattern("ELL Instruct Model", "^(S|C)$"))
                                .testThen(Restriction.pattern("Course Number",
                                        "^(12\\d{5}|20\\d{5}|21\\d{5}|82\\d{5})$"))),
                        "If Survey Period is 2, 3 or 4 and if English Language Learners: Instructional Model is S or C, then Course Number "
                                + "must be a Mathematics, Science, Social Studies or Computer Education course in Appendix DD."
                                + "If Survey Period is 1 and if English Language Learners: Instructional Model is S or C, then Course Number "
                                + "must be a Mathematics, Science, Social Studies or Computer Education course in the current or prior year in Appendix DD.")),
                new FLValidationRule("SSC", "5W", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3|4)$"),
                                Restriction.pattern("ELL Instruct Model", "^(E|I)$"))
                                .testThen(Restriction.pattern("Course Number", "^10\\d{5}|5010\\d{3}$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "1"),
                                Restriction.pattern("ELL Instruct Model", "^(E|I)$"))
                                .testThen(Restriction.pattern("Course Number", "^10\\d{5}|5010\\d{3}$"))),
                        "If Survey Period is 2, 3 or 4 and if English Language Learners: Instructional Model is E or I, then Course Number "
                                + "must be a Language Arts Course Number in Appendix DD."
                                + "If Survey Period is 1 and if English Language Learners: Instructional Model is E or I, then Course Number "
                                + "must be a Language Arts Course Number in the current or prior year in Appendix DD.")),
                new FLValidationRule("SSC", "5Y", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(7001|7023)$"),
                                Restriction.pattern("School Number", "^(?!7001|7023)\\S{1,4}$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(10[1-3]|11[1-3]|300)$"))),
                        "If School Number, Current Enrollment is not equal to 7001 or 7023 and if School Number, "
                                + "Current Instruction/Service is equal to 7001 or 7023, then FEFP Program Number must be 101, 102, 103, 111, 112, 113, or 300.")),
                new FLValidationRule("SSC", "5Z", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School",
                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3})$"))
                                .testThen(Restriction.notEquals("Dual Enrollment", "Z"))),
                        "If the School Number, Current Instruction/Service is C901-C928, U970-U981, or P001-P999, then the Dual Enrollment Indicator must be other than Z.")),
                new FLValidationRule("SSC", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?![1-4])\\d$"),
                                Restriction.notEquals("Year-Round Indicator", "A"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("Grade Level", "Grade Level")))),
                        "The student's Grade Level on the Student Course record must agree with the Grade Level on the Student Demographic record submitted "
                                + "by the district of instruction unless Year-Round/Extended School Year FTE Indicator on the Student Course record = A "
                                + "or if Survey Period Code = 1 or 4.")),
                new FLValidationRule("SSC", "61", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.equals("Grade Level", "PK"),
                                Restriction.greaterThan("FTE Reported Course", Double.valueOf(0)))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(1[01]1|25[45])$"))),
                        "During Surveys 1 through 4 if FTE Reported, Course is greater than zero and the student is less than 3 years old, "
                                + "then Grade Level must be PK and FEFP Program Number must be 101, 111 or 254-255.")),
                new FLValidationRule("SSC", "62", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.equals("Course Number", "2222222"),
                                Restriction.equals("School Number", "3518"))
                                .testThen(Restriction.lessThanOrEquals("FTE Reported Course", Double.valueOf(5000)))),
                        "If Survey Period Code equals 2 or 3 and Course Number equals 2222222, and School Number, Current Enrollment equals 3518,"
                                + "then FTE Reported Course must be equal to or less than 5000.")),
                new FLValidationRule("SSC", "66", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.notEquals("Period Number", "9800"),
                                Restriction.notEquals("Instruct School", "N999"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-MTC",
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Instruct District", "District Number"),
                                        new KeyValuePair("Instruct School", "School Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term")))),
                        "If Period Number is not 9800 and If School Number, Current Instruction/Service is not a private school number "
                                + "on a record with Survey Period Code = 2 or 3, Then each Student Course record must have a matching Teacher Course record "
                                + "based on the key fields of District Number, Current Instruction/Service; School Number, Current Instruction/ Service; "
                                + "Survey Period Code; Fiscal Year; Course Number; Section Number; Period Number; and Term.")),
                new FLValidationRule("SSC", "67", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Student Course Schedule must have a matching Student Demographic record based on District Number, Current Enrollment; "
                                + "School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; Fiscal Year; "
                                + "and District Number, Current Instruction/Service.")),
                new FLValidationRule("SSC", "69", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("FEFP Program Number", "^(11[1-3]|25[45])$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code = 1-4, each Student Course Schedule record with FEFP Program Number equal to 111-113 or 254-255 "
                                + "must have a matching Exceptional Student record based on District Number, Current Enrollment; School Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; Survey Period Code and Fiscal Year unless Survey Period Code = 2 or 3 and School Number, "
                                + "Current Instruction/Service is a private school number.")),
                new FLValidationRule("SSC", "72", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("FEFP Program Number", "130"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("ELL", "^(LP|LY)$"), null)))),
                        "If FEFP Program Number equals 130, then English Language Learners, PK-12 on the Student Demographic Information record "
                                + "(matched on District Number, Current Enrollment; School Number, Current Enrollment; Student Number Identifier, Florida;"
                                + " Survey Period Code; Fiscal Year; and District Number, Current Instruction/Service) must equal LY or LP.")),
                new FLValidationRule("SSC", "73", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.pattern("School Number", "^(?!3518|N99[89])\\S{1,4}$"),
                                Restriction.pattern("Instruct School", "^(?!0[567]00)\\S{1,4}$"),
                                Restriction.equals("Instruct District", "71"),
                                Restriction.or(
                                        Restriction.validateNotMatchInExport("EXPDATA-FL-ENR",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number", "District Number, E"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.equals("Withdrawal Code", "DNE"), null))))
                                .testThen(Restriction.equals("FTE Reported Course", ""))),
                        "For Surveys 2 and 3;"
                                + " if School Number, Current Enrollment is not 3518, N998, or N999; and"
                                + " if School Number, Current Instruction/Service is not 0500, 0600 or 0700 within District Number, Current Instruction/Service 71, and"
                                + " if FTE Reported, Course is greater than zero, and"
                                + " if the student has a Withdrawal Code, PK-12 of DNE on a Prior School Status/Student Attendance record, "
                                + "or if there is no matching Prior School Status/Student Attendance record,"
                                + "then FTE Reported, Course is set to NULL after the close of the state records processing cycle."
                                + "The match should be made using Year (School and Fiscal), Survey Period Code, District Number, Current Enrollment and "
                                + "Student Number Identifier, Florida. If the student has more than one Prior School Status/Student Attendance record, "
                                + "the match should be made to the record with the most recent Entry (Re-entry) Date.")),
                new FLValidationRule("SSC", "75", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "7650030"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(0),
                                                        Integer.valueOf(2), true, true,
                                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                null)))),
                        "If Course Number is 7650030 (Prekindergarten Disabilities: Age 0-2) then the student must be 0-2 years old. "
                                + "Age is determined using the student`s Birth Date on the Student Demographic Information record and the Friday of Survey Week. "
                                + "Match the Student Course Schedule record with the Student Demographic Information record using the following fields: "
                                + "District Number, Current Enrollment; School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; Fiscal Year; "
                                + "and District Number, Current Instruction/Service.")),
                new FLValidationRule("SSC", "80", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number", "^(?!P\\S{1,3})\\S{1,4}$"),
                                Restriction.pattern("Course Number", "^(?!8502000)[89]\\S{1,6}$"))
                                .testThen(Restriction.pattern("FEFP Program Number", "^(10[23]|11[23]|25[45]|300)$"))),
                        "If the School Number, Current Instruction/Service does not begin with P and the Course Number is a vocational secondary Course Number "
                                + "(begins with 8 or 9) except for Course Number 8502000, then the FEFP Program Number must be 300, 102, 103, 112-113 or 254-255.")),
                new FLValidationRule("SSC", "81", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.lessThanOrEquals("Class Minutes", Double.valueOf(2400)))),
                        "Class Minutes, Weekly must not be greater than 2400.")),
                new FLValidationRule("SSC", "82", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Number", "7650130"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("District Number", "District Number, E"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(3),
                                                        Integer.valueOf(5), true, true,
                                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN)),
                                                null)))),
                        "If Course Number is 7650130 then the student must be 3-5 years old. "
                                + "Age is determined using the student`s Birth Date on the Student Demographic Information record and the Friday of Survey Week. "
                                + "Match the Student Course Schedule record with the Student Demographic Information record using the following fields: "
                                + "District Number, Current Enrollment; School Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; "
                                + "Fiscal Year; and District Number, Current Instruction/Service.")),
                new FLValidationRule("SSC", "83", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("FEFP Program Number", "^(11[1-3]|25[45])$"))
                                .testThen(Restriction.validateNotMatchInExport(1, "EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("FEFP Program Number", "FEFP Program Number")))),
                        "Only one of the FEFP Program Numbers, 111-113 or 254-255, may be reported for a student in the same survey and Term.")),
                new FLValidationRule("SSC", "84", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dual Enrollment", "E"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Survey Period", "Survey Period")))
                                .testThen(Restriction.validateMatchInExport(2, "EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair(Restriction.equals("Dual Enrollment", "E"), null)))),
                        "If the Dual Enrollment Indicator equals E on one Student Course Schedule record reported for a student, "
                                + "then each Student Course Schedule record for that student must be coded with E in the Dual Enrollment Indicator field. "
                                + "Matching Student Course Schedule records for a student should be based on District Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Fiscal Year and Survey Period Code.")),
                new FLValidationRule("SSC", "96", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Reading Intervention", "Y"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Reading Intervention", "Reading Intervention")))
                                .testThen(Restriction.validateNotMatchInExport(2, "EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Reading Intervention", "Reading Intervention")))),
                        "For each student, no more than two Student Course Schedule records may be coded Y on the Reading Intervention Component element. "
                                + "Find all Student Course Schedule records for a student by matching on District Number, Current Instruction/Service; "
                                + "Student Number Identifier, Florida; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("SSC", "AA", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.pattern("Instruct District", "^0[1-9]|[1-6][0-9]|7[2-5]$"),
                                Restriction.pattern("Grade Level", "^(KG|0[1-5])$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct School", "Instruct School"),
                                        new KeyValuePair(Restriction.equals("Reading Intervention", "Y"), null)))),
                        "If Survey Period Code is 2 or 3 and district of instruction is 01-67, 69, or 72-75, and for any school that reported "
                                + "a student where Grade Level = KG - 05, then there must be at least one KG-05 student coded Y on the Reading Intervention Component element "
                                + "for each School Number, Current Instruction in MSID."))
        });
    }
}
