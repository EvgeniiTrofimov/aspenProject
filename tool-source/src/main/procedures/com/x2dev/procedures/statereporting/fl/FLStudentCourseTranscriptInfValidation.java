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
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.util.Arrays;
import java.util.List;

/**
 * The Class FLStudentCourseTranscriptInfValidation.
 */
public class FLStudentCourseTranscriptInfValidation {

    /**
     * Instantiates a new FL student course transcript Information validation.
     */
    public FLStudentCourseTranscriptInfValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SCTI", "1", new ValidateRegularExpression(
                        "Survey Period Code", "^5$",
                        "Survey Period Code must be 5 and must be correct for the"
                                + "submission specified by the district.")),
                new FLValidationRule("SCTI", "2", new ValidateDistrictNumber("District Number  CE")),
                new FLValidationRule("SCTI", "3", new ValidateRegularExpression(
                        "School Number CE",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to"
                                + "9899, excluding 9001")),
                new FLValidationRule("SCTI", "4", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.pattern("Student Number FL", "^\\d{9}(\\d|X)$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number FL", "^\\d{10}$"))
                                        .testThen(Restriction.pattern("Student Number FL", "^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$")),
                                ValidationRule.testIf(Restriction.pattern("Student Number FL", "^\\d{9}X$"))
                                        .testThen(Restriction.pattern("Student Number FL", "^(?!000).{10}$"))),
                        "The first nine positions of Student Number Identifier, Florida must be"
                                + "numeric. The tenth position of Student Number Identifier, Florida must either be"
                                + "an \"X\" or numeric. If the tenth position of Student Number Identifier, Florida is"
                                + "numeric, the first two digits must be a valid district number in the range 01-68, 71-"
                                + "75 or 78-79. If the tenth position of the Student Number Identifier, Florida is an"
                                + "\"X\", the first three positions may not all be zeroes.")),
                new FLValidationRule("SCTI", "5", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(Restriction.pattern("School Number WCrE",
                                "^000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N999$"))
                                .testThen(Restriction.pattern("District Number WCrE",
                                        "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[7-9]|99$")),
                                ValidationRule.testIf(Restriction.equals("School Number WCrE", "9900"))
                                        .testThen(Restriction.pattern("District Number WCrE", "^[A-W][K-Y]$")),
                                ValidationRule.testIf(Restriction.equals("School Number WCrE", "N997"))
                                        .testThen(Restriction.pattern("District Number WCrE", "^[A-Z][A-B]|AQ|WQ$"))),
                        "The District Number, Where Credit Earned must be numeric in the range of 01-68, 71-75 or 77-79 for Florida public school districts, "
                                + "99 for credit earned in Florida non-public schools, "
                                + "alpha codes AK-WY for credit earned out-of-state, "
                                + "or AA-ZB for credit earned out-of-country, or AQ to WQ for credit earned at United States Commonwealth and Territories. "
                                + "(See Appendices G, H and Q of the DOE Information Database Requirements: Volume I--Automated Student Information System Manual "
                                + "for valid Foreign Country, State codes and United States Commonwealth and Territories.).")),
                new FLValidationRule("SCTI", "6", new ValidateRegularExpression(
                        "School Number WCrE",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|"
                                + "C90[1-9]|C91[0-9]|C92[0-8]|U97[0-9]|U98[0-1]|P00[1-9]|P[0-9][1-9][0-9])$",
                        "The School Number, Where Credit Earned must be alphanumeric in the range of 0001-9899 for Florida public school district sites, "
                                + "N999 or in the range of 0001-9899 for credit earned in Florida PK-12 non-public schools, 9900 for credit earned in out-of-state schools, "
                                + "N998 for credit earned in home education, "
                                + "N997 for credit earned in an American school abroad, "
                                + "C901-C928 for Florida public community colleges, "
                                + "U970-U981 for Florida public state universities, or "
                                + "P001-P999 for eligible postsecondary nonpublic institutions.")),
                new FLValidationRule("SCTI", "7", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.lessThanOrEquals("School Year CT", new RuntimeParam(RuntimeParam.FISCAL_YEAR)))),
                        "School Year - Course Taken must be a valid school year, numeric, and less than or equal to the current school year.")),
                new FLValidationRule("SCTI", "8", new ValidateRegularExpression("Grade Level", "^(0[1-9]|1[0-2]|30)$",
                        "Grade Level must be 01-12 or 30.")),
                new FLValidationRule("SCTI", "9", new ValidateRegularExpression("Term", "^[1-9B-OS-Z]$",
                        "Term must be either 1-9, B-O or S-Z.")),
                new FLValidationRule("SCTI", "10", new ValidateRegularExpression("Course Number", "^\\S{1,7}$",
                        "Course Number must be alphanumeric and contain no blanks.")),
                new FLValidationRule("SCTI", "11", new ValidateRegularExpression("Course Sequen Number", ".*[^ ].*",
                        "The Course Sequence Number must be alphanumeric and contain no blanks. Allowable characters are 0-9, A-Z, hyphen, dollar sign, pound sign, and colon.")),
                new FLValidationRule("SCTI", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number WCrE", "^(?!P000|P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "All alphanumeric Course Numbers must either be on the Course Code Directory or on the Statewide Course Numbering System file "
                                + "unless School Number, Where Credit Earned equals P001-P999.")),
                new FLValidationRule("SCTI", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT",
                                "School Year RS", String.class))
                                .testThen(Restriction.pattern("Course State SAR",
                                        "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$"))),
                        "If School Year-Course Taken = School Year-Record Submission, then Course, State Subject Area Requirements must equal "
                                + "EN, MA, AH, WH, EC, AG, VO, PF, PE, EL, FL, PA, A1, BI, GE, EQ, SS, CE, EX, LA, LM, NC or SV.")),
                new FLValidationRule("SCTI", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT",
                                "School Year RS", String.class))
                                .testThen(Restriction.pattern("Course, Flag",
                                        "^([G-I]|[79NPTWX\\*\\s]){1,4}$"))),
                        "If School Year-Course Taken = School Year-Record Submission, then Course Flag must be 7, 9, G-I, N, P, T, W, X, * or blank.")),
                new FLValidationRule("SCTI", "15", new ValidateRegularExpression("Course, Flag",
                        "^(?!.*(7.*7|9.*9|G.*G|H.*H|I.*I|N.*N|P.*P|T.*T|W.*W|X.*X|\\*.*\\*).*).+$",
                        "For each Student Course Transcript Information record, no Course Flag, except blanks, may be repeated.")),
                new FLValidationRule("SCTI", "16", new ValidateRegularExpression("Credit Attempt Cours", "^\\d{1,3}$",
                        "Credit Attempted, Course must be numeric.")),
                new FLValidationRule("SCTI", "17", new ValidateRegularExpression("Credit Earned Course", "^\\d{1,3}$",
                        "Credit Earned, Course must be numeric.")),
                new FLValidationRule("SCTI", "18", new ValidateRegularExpression("Course Grade",
                        "^(\\s{2}([A-D]|F|I|N|U|P|S|E|W|Z)|\\s(A[+-]|B[+-]|C[+-]|D[+-]|IP|WP|FL|NG|WF))$",
                        "The Course Grade code must be A+, A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F, I, N, U, P, S, SB, T, E, WP, FL, NG, W or WF "
                                + "and must be right justified with leading blanks.")),
                new FLValidationRule("SCTI", "20", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("Student Number FL", "Survey Period Code",
                                        "District Number  CE", "School Number CE", "Course Number",
                                        "Course Sequen Number", "School Year CT", "Grade Level", "Term"))),
                        "Each Student Course Transcript Information record must be unique based on "
                                + "Survey Period Code; District Number, Current Enrollment; School Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; School Year - Course Taken; Grade Level; Term; "
                                + "Course Number; and Course Sequence Number.")),
                new FLValidationRule("SCTI", "21", new FLValidationRuleSet(
                        new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byActiveSchool("School Number CE"))),
                        "School Number, Current Enrollment must exist on the Master School Identification File as a valid active school number "
                                + "for the District Number, Current Enrollment.")),
                new FLValidationRule("SCTI", "23", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("SCTI", "24", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number WCrE",
                                "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|P000|P\\d{3})$"))
                                .testThen(Restriction.equals("District Number WCrE", "99"))),
                        "If School Number, Where Credit Earned equals C901-C928, U970-U981, or P001-P999, "
                                + "then District Number, Where Credit Earned must equal 99.")),
                new FLValidationRule("SCTI", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course Grade", "W"))
                                .testThen(Restriction.pattern("School Number WCrE",
                                        "^(C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|P000|P\\d{3}|9900|N999)$"))),
                        "If the Course Grade code equals W, then the School Number, Where Credit Earned must be U970-U981, C901-C928, P001-P999, 9900 or N999.")),
                new FLValidationRule("SCTI", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Numb Substit"))),
                        "Course Number, Substituted must be a course in the Course Code Directory or it must be blank.")),
                new FLValidationRule("SCTI", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$"))
                                .testThen(Restriction.equals("Course, Flag", "*"))),
                        "If the Course Number, Substituted is not blank, then at least one Course Flag must be *.")),
                new FLValidationRule("SCTI", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course, Flag", "*"))
                                .testThen(Restriction.pattern("Course Numb Substit", "^\\s*\\S+$"))),
                        "If any of the four Course Flags are *, then the Course Number, Substituted must not be blank.")),
                new FLValidationRule("SCTI", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equalsFieldValue("School Year CT", "School Year RS",
                                String.class))
                                .testThen(Restriction.pattern("Course Substit SSAR",
                                        "^(EN|MA|AH|WH|EC|AG|VO|PF|PE|EL|FL|PA|A1|BI|GE|EQ|SS|CE|EX|LA|LM|NC|SV)$")),
                        ValidationRule.testIf(Restriction.pattern("Course Numb Substit", "^\\s*$"))
                                .testThen(Restriction.pattern("Course Substit SSAR", "^\\s*$")),
                        ValidationRule.testIf(Restriction.pattern("Course Substit SSAR", "^\\s*$"))
                                .testThen(Restriction.pattern("Course Numb Substit", "^\\s*$"))),
                        "If School Year-Course Taken = School Year-Record Submission, then Course Substituted, State Subject Area Requirements must equal "
                                + "EN, MA, AH, WH, EC, AG, VO, PF, PE, EL, FL, PA, A1, BI, GE, EQ, SS, CE, EX, LA, LM, NC or SV or blank. If Course Number, Substituted is blank, "
                                + "Course Substituted, State Subject Area Requirements must be blank. If Course Substituted, State Subject Area Requirements is blank, "
                                + "then Course Number, Substituted must be blank.")),
                // TODO Rule #30 The School Year - Record Submission must be correct for the
                // submission specified by the district.
                new FLValidationRule("SCTI", "35", new ValidateRegularExpression("Online Course", "^(Y|N|I|J|O)$",
                        "Online Course must be Y, N, I, J or O.")),
                // TODO must not have a Sort Level of 1 (Basic Education PK-5) or 2 (Basic Education
                // 6-8) on the Course Code Directory file DPS.DISTRICT.K9.F62806.Yyyyy.
                new FLValidationRule("SCTI", "36", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number WCrE", "^(?!P\\S{1,3})\\S{1,4}$"))
                                .testThen(Restriction.pattern("Course Number",
                                        "^(?!5022000|22000([0-4]\\d|50)|22003([0-6]\\d|70))\\S{1,7}$"))),
                        "If School Number, Where Credit Earned does not begin with P, then Course Number must not equal 5022000, 2200000-2200050, or 2200300-2200370 (study hall) "
                                + "and must not have a Sort Level of 1 (Basic Education PK-5) or 2 (Basic Education 6-8) on the Course Code Directory file DPS.DISTRICT.K9.F62806.Yyyyy.")),
                new FLValidationRule("SCTI", "37", new ValidateRegularExpression("Florida Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("SCTI", "38", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Online Course", "^(J|O)$"))
                                .testThen(Restriction.equals("Course Number", "0200985"))),
                        "If Online Course equals J or O, then Course Number must equal 0200985.")),
                new FLValidationRule("SCTI", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("FEFP Program Number", "130"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number FL", "Student Number"),
                                        new KeyValuePair("District Number  CE", "District Number, E"),
                                        new KeyValuePair("Survey Period Code", "Survey Period"),
                                        new KeyValuePair("School Year RS", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("ELL", "^(LP|LY)$"), null)))),
                        "Each Student Course Transcript must have a matching Student Demographic Record based on District Number, Current Enrollment; "
                                + "Student Number Identifier, Florida; Survey Period Code and School Year - Record Submission matched to Year.")),
                new FLValidationRule("SCTI", "80", new ValidateRegularExpression(
                        "Grade Level", "^(0[6-9]|1[0-2]|30)$", "Grade Level must be in the range 06-12, or 30.")),
                new FLValidationRule("SCTI", "81", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(0[1-9])$"),
                                Restriction.greaterThan("Credit Earned Course", Double.valueOf(0)))
                                .testThen(Restriction.equals("Course, Flag", "9"))),
                        "If Grade Level is less than 09 and Credit Earned, Course is greater than zero, then Course Flag must be 9.")),
                new FLValidationRule("SCTI", "82", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course, Flag", "I"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SCTI",
                                        new KeyValuePair("Student Number FL", "Student Number FL"),
                                        new KeyValuePair("District Number  CE", "District Number  CE"),
                                        new KeyValuePair("School Number CE", "School Number CE"),
                                        new KeyValuePair("School Year RS", "School Year RS"),
                                        new KeyValuePair("Course State SAR", "Course State SAR"),
                                        new KeyValuePair(Restriction.equals("Course, Flag", "X"), null)))),
                        "For each Course Flag of I, there must be a Course Flag of X on another course record for the same District Number, "
                                + "Current Enrollment; School Number, Current Enrollment; Student Number Identifier, Florida; School Year - Record Submission and Course, "
                                + "State Subject Area Requirements.")),
                new FLValidationRule("SCTI", "83", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Course, Flag", "N"))
                                .testThen(Restriction.equals("Credit Earned Course", "0"))),
                        "If Course Flag is N then Credit Earned, Course must be zero.")),
                new FLValidationRule("SCTI", "84", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number",
                                "^(?!(79[1268]([0-3]|[5-7]|9)[0134][0-9]0)|"
                                        + "(8([1-5]|[7-9])00[41][0-3]0|8301650|90018[12]0|M899990|B079998))\\S{1,7}$"))
                                .testThen(Restriction.lessThanOrEquals("Credit Attempt Cours", Double.valueOf(100)))),
                        "If the Course Number is not one of those listed below, the Credit Attempted, Course must be less than or equal to 100."
                                + "Vocational Courses Offering Multiple Credit:"
                                + "8100410 8100100 8200420 M899990 B079998 8200410 8200100"
                                + "8300420 8300410 8301650 8400410 8400100 8500410 8500100"
                                + "8700400 8700100 8800100 8800410 8900410 8900100 9001820"
                                + "9001810 8300430"
                                + "Exceptional Education Courses Offering Multiple Credit:"
                                + "7910100 7921010 7962030 7963110 7967010 7980100"
                                + "7910110 7921330 7962040 7963120 7980010 7980110"
                                + "7910390 7960010 7963010 7963130 7980020 7980120"
                                + "7910400 7961010 7963030 7965010 7980030 7980130"
                                + "7912050 7961020 7963040 7965030 7980040 7980150"
                                + "7912340 7961030 7963050 7965040 7980050 7980190"
                                + "7915010 7961040 7963060 7966010 7980060"
                                + "7919010 7961050 7963070 7966020 7980070"
                                + "7920010 7962010 7963080 7966030 7980080"
                                + "7920050 7962020 7963090 7966040 7980090")),
                new FLValidationRule("SCTI", "85", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number",
                                "^(?!(79[1268]([0-3]|[5-7]|9)[0134][0-9]0)|"
                                        + "(8([1-5]|[7-9])00[41][0-3]0|8301650|90018[12]0|M899990|B079998))\\S{1,7}$"))
                                .testThen(Restriction.lessThanOrEquals("Credit Earned Course", Double.valueOf(100)))),
                        "If the Course Number is not one of those listed below, the Credit Earned, Course must be less than or equal to 100."
                                + "Vocational Courses Offering Multiple Credit:"
                                + "8100410 8100100 8200420 M899990 B079998 8200410 8200100"
                                + "8300420 8300410 8301650 8400410 8400100 8500410 8500100"
                                + "8700400 8700100 8800100 8800410 8900410 8900100 9001820"
                                + "9001810 8300430"
                                + "Exceptional Education Courses Offering Multiple Credit:"
                                + "7910100 7921010 7962030 7963110 7967010 7980100"
                                + "7910110 7921330 7962040 7963120 7980010 7980110"
                                + "7910390 7960010 7963010 7963130 7980020 7980120"
                                + "7910400 7961010 7963030 7965010 7980030 7980130"
                                + "7912050 7961020 7963040 7965030 7980040 7980150"
                                + "7912340 7961030 7963050 7965040 7980050 7980190"
                                + "7915010 7961040 7963060 7966010 7980060"
                                + "7919010 7961050 7963070 7966020 7980070"
                                + "7920010 7962010 7963080 7966030 7980080"
                                + "7920050 7962020 7963090 7966040 7980090")),
        });
    }
}
