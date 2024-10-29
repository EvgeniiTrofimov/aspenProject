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
 * The Class FLCTEStudentCourseScheduleValidation.
 */
public class FLCTEStudentCourseScheduleValidation {

    /**
     * Instantiates a new FLCTE student course schedule validation.
     */
    public FLCTEStudentCourseScheduleValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("CTESSC", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("CTESSC", "2", new ValidateRegularExpression(
                        "School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89])$",
                        "School Number, Current Enrollment must be numeric in the range 0001 to "
                                + "9899, excluding 9001, or it must be N998 or N999.")),
                new FLValidationRule("CTESSC", "3", new ValidateRegularExpression(
                        "Student Number",
                        "^(?!(0{3}\\d{6}X|\\d{9}[^\\dX]|((7[67]|[89][\\d])\\d{8})|.{1,9})$)",
                        "Student Number, must be numeric, tenth position must either be an X or numeric. "
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("CTESSC", "4", new ValidateRegularExpression(
                        "Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the "
                                + "submission specified by the district.")),
                new FLValidationRule("CTESSC", "5", new ValidateFiscalYear()),
                new FLValidationRule("CTESSC", "6", new ValidateRegularExpression("Instruct District",
                        "^(0[1-8]|[1-6][0-9]|7[1-5])$",
                        "District Number, Current Instruction/Service must be numeric in the range "
                                + "01-68 or 71-75 and must be correct for the district submitting the data.")),
                new FLValidationRule("CTESSC", "7", new ValidateRegularExpression(
                        "Instruct School",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|C90[1-9]|C91[0-9]|C92[0-8]|U97[0-9]|U98[0-1]|P00[1-9]|P[0-9][1-9][0-9])$",
                        "The School Number, Current Instruction/Service must be numeric in the "
                                + "range 0001-9899, excluding 9001, or it must be C901-C928, U970-U981, or P001-P999.")),
                // TODO: Rule #8 Career and Technical Education/Adult General Education Program file
                // TODO: Rule #9 Career and Technical Education/Adult General Education Program Edit file
                new FLValidationRule("CTESSC", "10", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.and(
                                                Restriction.pattern("Section Number", "^([0-9A-Z\\s-\\$£:]+)$"),
                                                Restriction.pattern("Section Number", ".*[^ ].*")))),
                        "Section Number must not be all blanks. Allowable characters are 0-9, A-Z, "
                                + "space, hyphen, dollar sign, pound sign and colon.")),
                new FLValidationRule("CTESSC", "11", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.greaterThanOrEquals("Period Number", Double.valueOf(0)))),
                        "Period Number must be numeric, and greater than or equal to zero.")),
                new FLValidationRule("CTESSC", "13", new ValidateRegularExpression("Term",
                        "^[1-9B-OS-Z]$",
                        "Term must be either 1-9, B-O or S-Z.")),
                new FLValidationRule("CTESSC", "16", new ValidateRegularExpression("Internship",
                        "^[Y|N]$",
                        "The code for Internship Participant must be Y or N.")),
                new FLValidationRule("CTESSC", "17", new ValidateRegularExpression("Student ID Local",
                        "^[A-Za-z0-9\\s]{10}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("CTESSC", "19", new ValidateRegularExpression("Grade Level",
                        "^(0[6-9]|1[0-2])$",
                        "Grade Level must be 06-12.")),
                // Rule #20 skipped (Transaction Code)
                new FLValidationRule("CTESSC", "21", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("Student Number", "Fiscal Year",
                                                "Instruct District", "Instruct School", "Course Number",
                                                "Section Number", "Period Number", "Term"))),
                        "Each record must be unique based on Student Number Identifier, Florida; "
                                + "School Year, District Number, Current Instruction/Service; School Number, "
                                + "Current Instruction/Service; Course Number; Section Number; Period Number "
                                + "and Term.")),
                new FLValidationRule("CTESSC", "25", new ValidateRegularExpression("Except CTE Course",
                        "^[E|S|M|Z]$",
                        "On the Career and Technical Student Course Schedule record, Exceptional "
                                + "Student Career and Technical Course Setting code must be one of the following: "
                                + "E, S, M or Z.")),
                new FLValidationRule("CTESSC", "29", new ValidateRegularExpression("CTE Completion Point",
                        "^[A-Z]{0,6}\\s{0,6}$",
                        "The Career and Technical/Adult General Education Completion Point Code "
                                + "must be A-Y, Z or blank and must be left justified with no embedded blanks.")),
                new FLValidationRule("CTESSC", "30", new ValidateRegularExpression("Mod Occup Comp Point",
                        "^[A-Z]$",
                        "The Modified Occupational Completion Point Code must be A-Y, or Z.")),
                new FLValidationRule("CTESSC", "31", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Mod Occup Comp Point", "Z"))
                                        .testThen(Restriction.equalsFieldValue("Mod Occup Comp Point",
                                                "CTE Completion Point", String.class))),
                        "If the Modified Occupational Completion Point is not Z, then it must be "
                                + "equal to the Career and Technical/Adult General Education Occupational "
                                + "Completion Point.")),
                new FLValidationRule("CTESSC", "32", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("CTE Completion Point", "Z"))
                                        .testThen(Restriction.pattern("CTE Completion Point", "^\\S{0,6}\\s{0,5}$"))),
                        "If the Career and Technical/Adult General Education Completion Point "
                                + "Code equals Z, then it must be left justified and followed by five blanks.")),
                // TODO: Rule #33 Career and Technical/Adult General Education Program Edit File
                new FLValidationRule("CTESSC", "36", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.notEquals("Mod Occup Comp Point", "Z"))
                                        .testThen(Restriction.equals("Except CTE Course", "E"))),
                        "If Modified Occupational Completion Point Code has any value other Z, "
                                + "then the Exceptional Student Career and Technical Course Setting must have a "
                                + "value of E.")),
                new FLValidationRule("CTESSC", "39", new ValidateRegularExpression("FEFP Program Number",
                        "^(102|103|112|113|130|254|255|300|999)$",
                        "FEFP Program Number must be 102, 103, 112, 113, 130, 254, 255, 300, or 999.")),
                new FLValidationRule("CTESSC", "40", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.notEquals("School Number", "N998"),
                                        Restriction.notEquals("School Number", "N999")))
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, then it must "
                                + "exist on the Master School Identification File as a valid number in the District "
                                + "Number, Current Enrollment.")),
                new FLValidationRule("CTESSC", "40", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.pattern("School Number",
                                                "^(?!N99[89])\\S{1,4}$"))
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not N998 or N999, "
                                + "then it must exist on the Master School Identification File as a valid number "
                                + "in the District Number, Current Enrollment.")),
                new FLValidationRule("CTESSC", "41", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(
                                        Restriction.pattern("Instruct School",
                                                "^(?!C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|P000|P\\d{3})\\S{1,4}$"))
                                        .testThen(Restriction.byActiveSchool("Instruct School"))),
                        "If the School Number, Current Instruction/Service is not C901-C928, U970-"
                                + "U981, or P001-P999, then it must exist as a valid active number in the District "
                                + "Number, Current Instruction/Service on the Master School Identification File.")),
                new FLValidationRule("CTESSC", "43", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.validatePeriodNumber("Period Number"))),
                        "The first two digits of Period Number must be 00-80 while the last two "
                                + "digits must be 00-80 or 88 and be greater than or equal to the first two digits.")),
                new FLValidationRule("CTESSC", "45", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Grade Level", "^[PK|KG|0[1-8]]$"))
                                        .testThen(Restriction.notEquals("FEFP Program Number", "300"))),
                        "If Grade Level is less than 09, then the FEFP Program Number must not be 300.")),
                new FLValidationRule("CTESSC", "4G", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable.")),
                new FLValidationRule("CTESSC", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Instruct School", "^(?![C|P|U]).+$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-CTETC",
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Instruct District", "District Number CIS"),
                                        new KeyValuePair("Instruct School", "School Number CIS"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("Term", "Term")))),
                        "If School Number, Current Instruction/Service does not begin with C, P and U, "
                                + "then each Career and Technical Education Student Course record must have a "
                                + "matching Career and Technical Education Teacher Course record based on the "
                                + "key fields of District Number, Current Instruction/Service; School Number, "
                                + "Current Instruction/Service; School Year; Course Number; Section Number; "
                                + "Period Number and Term.")),
                new FLValidationRule("CTESSC", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Career and Technical Student Course Schedule record must have a "
                                + "matching Student Demographic Information record based on District Number, "
                                + "Current Instruction; Student Number Identifier, Florida; Survey Period Code; and "
                                + "Year.")),
                new FLValidationRule("CTESSC", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("Instruct District", "District Number, I/S"),
                                        new KeyValuePair("Grade Level", "Grade Level")))),
                        "The student's Grade Level code on the Career and Technical Student "
                                + "Course record must agree with the Grade Level code on the Student "
                                + "Demographic record submitted by the district of instruction.")),
                new FLValidationRule("CTESSC", "63", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^(?!8502000|00000000)\\d+$"))
                                .testThen(
                                        Restriction.pattern("FEFP Program Number", "^(300|102|103|112|113|254|255)$"))),
                        "If the Course Number is not 8502000 or 00000000, then the FEFP Program "
                                + "Number must be 300, 102, 103, 112, 113, 254 or 255.")),
        });
    }
}
