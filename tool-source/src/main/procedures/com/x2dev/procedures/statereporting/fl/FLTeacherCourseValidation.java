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
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateSSN;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import java.util.Arrays;
import java.util.List;


/**
 * The Class FLTeacherCourseValidation.
 */
public class FLTeacherCourseValidation {

    /**
     * Instantiates a new FL teacher course validation.
     */
    public FLTeacherCourseValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("MTC", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("MTC", "2", new ValidateRegularExpression("School Number",
                        "^(?!9001|9996$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|"
                                + "C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3}|N999)$",
                        "School Number, Current Instruction/Service must be numeric in the range 0001 to 9899 (excluding 9001), "
                                + "9996 or it must be C901 to C928, U970 to U981, P001-P999 or N999.")),
                new FLValidationRule("MTC", "3", new ValidateRegularExpression(
                        "Survey Period", "^[1-4]$",
                        "Survey Period Code must be 1, 2, 3, or 4 and it must be correct for the submission specified by the district.")),
                new FLValidationRule("MTC", "4", new ValidateFiscalYear()),
                new FLValidationRule("MTC", "5", new ValidateRegularExpression(
                        "Course Number", "^\\S{1,7}$",
                        "Course Number must not contain blanks.")),
                new FLValidationRule("MTC", "6", new ValidateRegularExpression(
                        "Section Number", "^[0-9|A-Z|\\-$#&%/:]{1,5}$",
                        "Section Number must not be all blanks. "
                                + "Allowable characters are 0-9, A-Z, space, hyphen (-), dollar sign ($), pound sign (#), "
                                + "ampersand (&), percent (%), forward slash (/) and colon (:).")),
                new FLValidationRule("MTC", "7", new ValidateRegularExpression(
                        "Period Number", "^\\d{1,4}$",
                        "Period Number must be numeric and greater than or equal to zero.")),
                new FLValidationRule("MTC", "8", new ValidateRegularExpression(
                        "Certification Number", "^\\d{1,10}$",
                        "Florida Educators Certificate Number must be numeric with no embedded blanks.")),
                new FLValidationRule("MTC", "10", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("School Number", "District Number",
                                        "Survey Period", "Fiscal Year", "SSN", "Course Number",
                                        "Section Number", "Period Number", "Term"))),
                        "Each record must be unique based on the following keys: District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; Survey Period Code; Fiscal Year; Term; Course Number; Section Number; "
                                + "Period Number; and Social Security Number.")),
                new FLValidationRule("MTC", "11", new ValidateSSN()),
                new FLValidationRule("MTC", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("School Number", "7001"))
                                .testThen(Restriction.pattern("FL Educators Cert Id", "^9999999999|000\\d{7}$"))),
                        "If School Number, Current Instruction/Service = 7001, the Florida Educators Certificate Number must be a regular number "
                                + "assigned by the Certification Section of the Florida Department of Education in the following ranges: "
                                + "0000000001 - 0000999998 and 0001000000 -0009999999, and may not contain blanks.")),
                new FLValidationRule("MTC", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                .testThen(Restriction.pattern("Survey Indicator", "^(Y|N)$"))),
                        "If Survey Period Code is 1-4, the Term/Survey Indicator must be Y or N.")),
                new FLValidationRule("MTC", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.equals("Term", "3"))
                                .testThen(Restriction.equals("Survey Indicator", "Y"))),
                        "If Survey Period Code is 1-4 and Term is 3, the Term/Survey Indicator must be Y.")),
                new FLValidationRule("MTC", "16", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                Restriction.equals("Term", "1"))
                                .testThen(Restriction.equals("Survey Indicator", "Y"))),
                        "If Survey Period Code is 2 and Term is 1, the Term/Survey Indicator must be Y.")),
                new FLValidationRule("MTC", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "3"),
                                Restriction.equals("Term", "2"))
                                .testThen(Restriction.equals("Survey Indicator", "Y"))),
                        "If Survey Period Code is 3 and Term is 2, the Term/Survey Indicator must be Y.")),
                new FLValidationRule("MTC", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^51005[89]0$"))
                                .testThen(Restriction.equals("Certification Status", "V"))),
                        "If Course Number is 5100580 or 5100590, then Certification/Licensure/Qualification Status code must be V.")),
                new FLValidationRule("MTC", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^(?!51005[89]0)\\S{1,7}$"))
                                .testThen(Restriction.notEquals("Certification Status", "V"))),
                        "If Course Number does not equal 5100580 or 5100590, then Certification/Licensure/Qualification Status code must not be V.")),
                new FLValidationRule("MTC", "20", new ValidateRegularExpression("Facility Type", "^[01][0-9]|20$",
                        "Facility Type code must be in the range 00 to 20.")),
                new FLValidationRule("MTC", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Days In Term", Double.valueOf(0)),
                                        Restriction.lessThan("Days In Term", Double.valueOf(300))))),
                        "For Survey Periods 1-4, Days in Term (for FTE purposes) must be numeric, greater than zero and less than 300.")),
                new FLValidationRule("MTC", "23",
                        new ValidateRegularExpression("Certification Status", "^(A|B|H|I|O|M|S|N|V|P)$",
                                "Certification/Licensure/Qualification Status code must be A, B, H, I, O, M, S, N, V or P.")),
                new FLValidationRule("MTC", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Classroom Id Number",
                                        "^(\\d{5}\\w\\d{10}(\\d|\\w){5})|(\\d{5}O\\d{10}.{5})$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.pattern("Classroom Id Number",
                                        "^(.{0,20}[\\sZ0].{0,20})$"))),
                        "If Survey Period Code is 2 or 3, then the first five positions and positions 7-16 of Classroom Identification (FISH) "
                                + "Number must be numeric. Positions 17-21 may be alpha or numeric. If position 6 is O, positions 17-21 may be symbols. "
                                + "If Survey Period Code is 1 or 4, then each position of the Classroom Identification (FISH) Number must contain one of the following: "
                                + "blank (space), Z or zero.")),
                new FLValidationRule("MTC", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Classroom Id Number",
                                        "^(.{5}[A|B|C|F|O|S].{15})$"))),
                        "If Survey Period Code is 2 or 3, then position 6 of the Classroom Identification (FISH) Number must be A, B, C, F, O or S.")),
                new FLValidationRule("MTC", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Scheduling Method",
                                        "^(A|B|C|G|I|M|S|W)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.equals("Scheduling Method", "Z"))),
                        "If Survey Period Code is 2 or 3, then Scheduling Method must be A. B, C, G, I, M, S or W. "
                                + "If Survey Period Code is 1 or 4, then Scheduling Method must be Z.")),
                new FLValidationRule("MTC", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Classroom Id Number", "^(.{5}O.{15})$"))
                                .testThen(Restriction.notEquals("Facility Type", "00")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Classroom Id Number", "^(.{5}C.{15})$"))
                                .testThen(Restriction.equals("Facility Type", "19"))),
                        "If Survey Period Code is 2 or 3 and if position 6 of the Classroom Identification (FISH) Number is O, then Facility Type must not be 00. "
                                + "Survey Period Code is 2 or 3 and if position 6 of the Classroom Identification (FISH) number is C, then Facility Type must be 19.")),
                // TODO FLDOE FISH database
//                new FLValidationRule("MTC", "2A", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
//                                Restriction.pattern("Classroom Id Number", "^(.{5}[A|B|C|F|S].{15})$"))
//                                .testThen(Restriction.byAliasFldRefTable("all-rms-FacilityType", "Facility Type"))),
//                        "If Survey Period is 2 or 3, and If position 6 of the Classroom Identification (FISH) Number is A, B, C, F, or S the number "
//                                + "must be a valid FISH number on the FLDOE FISH database.")),
                new FLValidationRule("MTC", "31", new ValidateRegularExpression("Staff ID Local",
                        "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                        "The Staff Number Identifier, Local may be any combination of letters, numbers and blanks. All blanks are not allowable. "
                                + "It must be left-justified with trailing blanks.")),
                // TODO Course Number Class Size Core Course Indicator
                new FLValidationRule("MTC", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Scheduling Method", "^(C|I)$"),
                                Restriction.pattern("Period Number", "^(?!\\d{2}88)\\d{4}$"))
                                .testThen(Restriction.pattern("Team Teacher", "^[A-D]$"))),
                        "If Course Number Class Size Core Course Indicator on the Course Code Directory = Y, the last two digits of the Period Number are not 88, "
                                + "the Survey Period Code is 2 or 3, and Scheduling Method is C or I, then the Team Teacher Training code must be A-D.")),
                new FLValidationRule("MTC", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Team Teacher", "^([A-D]|Z)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.equals("Team Teacher", "Z"))),
                        "For Survey Period Codes 2 and 3, Team Teacher Training must be A-D or Z. "
                                + "For Survey Period Codes 1 and 4, Team Teacher Training must be Z.")),
                // TODO If not a Virtual Charter School (note: Virtual Charter Schools are
                // identified on MSID by Charter School Status not Z
                // and School Function Setting equal to V
                new FLValidationRule("MTC", "35", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("District Number", "^0[1-9]|[1-6][0-9]|7[1-5]$"),
                                Restriction.pattern("School Number", "^(?!7001|7004|7006|7023)\\S{4}$"),
                                Restriction.and(
                                        Restriction.equals("District Number", "50"),
                                        Restriction.pattern("School Number", "^(?!7079)\\S{4}$")))
                                .testThen(Restriction.notEquals("Facility Type", "20"))),
                        "If District Number, Current Instruction/Service is in the range 01 - 69 or 72 - 75 and"
                                + " if School Number, Current Instruction/Service is not 7001, 7006 7004, or 7023 and"
                                + " if School Number, Current Instruction/Service is not 7079 in District Number, Current Instruction/Service 50, and"
                                + " if the School Number, Current Instruction/Service is not one that has a School Function Setting of V and a Charter "
                                + "School Status not equal to Z on the Master School Identification file,"
                                + "then Facility Type must not equal 20.")),
                // TODO School Number, Current Instruction/Service is one for which
                // the School Function Setting = V and Charter School Status does not equal Z on the
                // Master School Identification file
                new FLValidationRule("MTC", "36", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("District Number", "71"))
                                .testThen(Restriction.notEquals("Facility Type", "20"))),
                        "If District Number, Current Instruction/Service is 71, or if the School Number, Current Instruction/Service is one for which "
                                + "the School Function Setting = V and Charter School Status does not equal Z on the Master School Identification file, "
                                + "then Facility type must equal 20.")),
                new FLValidationRule("MTC", "37", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"))
                                .testThen(Restriction.pattern("Fund Source", "^(Y|N)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[14]$"))
                                .testThen(Restriction.equals("Fund Source", "Z"))),
                        "If Survey Period Code is 2, or 3, then Fund Source: NCLB Title III must be coded Y or N. "
                                + "If Survey Period Code is 1 or 4, then Fund Source NCLB Title III must be Z.")),
                new FLValidationRule("MTC", "38", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("MTC", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(
                                        Restriction.byActiveSchool("School Number"),
                                        Restriction.pattern("School Number",
                                                "^C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|((?!P000)P\\d{3}|N999|9996)$")))),
                        "School Number, Current Instruction/Service must be valid and active on the Master School Identification File "
                                + "or must be C901 to C928, U970 to U981, P001-P999, 9996, or N999.")),
             // TODO reference table for courses
//                new FLValidationRule("MTC", "41", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.pattern("Course Number", "^\\d{1,7}$"),
//                                Restriction.pattern("School Number", "^(?!(?!P000)P\\d{3})\\S{1,4}$"))
//                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
//                        "All numeric Course Numbers must be on the Course Code Directory file unless School Number, Current Instruction/Service is P001-P999.")),
                new FLValidationRule("MTC", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.pattern("Period Number", "^([0-7]\\d|80)([0-7]\\d|80|88)$"),
                                        Restriction.validatePeriodNumber("Period Number")))),
                        "The first two digits of Period Number must be 00 to 80. The last two digits of Period Number must be 00 to 80 or 88 "
                                + "and must be greater than or equal to the first two digits.")),
                new FLValidationRule("MTC", "46", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "^[A-Z]\\S{1,6}$"),
                                Restriction.pattern("School Number", "^(?!N999|(?!P000)P\\d{3})\\S{1,4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-crs-StateId", "Course Number"))),
                        "All alphanumeric Course Numbers must be either on the Course Code Directory file or on the Statewide Course Numbering System file "
                                + "unless the School Number, Current Instruction/Service equals N999 or P001-P999.")),
                new FLValidationRule("MTC", "48", new ValidateRegularExpression("Primary Instructor", "^(Y|N)$",
                        "Primary Instructor Indicator code must be Y or N.")),
                new FLValidationRule("MTC", "49", new ValidateRegularExpression(
                        "Term", "^([1-9]|[B-O]|[S-X])$", "Term must be either 1-9, B-O or S-X.")),
                new FLValidationRule("MTC", "4A", new ValidateRegularExpression("Blended Learning", "^(Y|N)$",
                        "Blended Learning Course code must be Y or N.")),
                // TODO Charter School Status is not Z and School Function/ Setting equals V on the
                // Master School Identification file,
                new FLValidationRule("MTC", "4B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("District Number", "71"),
                                Restriction.pattern("School Number", "^(7001|7004|7006|7023|7079)$"))
                                .testThen(Restriction.equals("Blended Learning", "N")),
                        ValidationRule.testIf(Restriction.equals("District Number", "50"),
                                Restriction.equals("School Number", "7079"))
                                .testThen(Restriction.equals("Blended Learning", "N"))),
                        "If School Number, Current Instruction/Service = 7001, 7004, 7006 or 7023; or District Number, Current Instruction/Service =71; "
                                + "or District Number, Current Instruction/Service = 50 and School Number, Current Instruction/Service = 7079; "
                                + "or Charter School Status is not Z and School Function/ Setting equals V on the Master School Identification file, "
                                + "then Blended Learning Course must equal N.")),
//                new FLValidationRule("MTC", "51", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.alwaysTrue())
//                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
//                                        new KeyValuePair("Course Number", "Course Number"),
//                                        new KeyValuePair("Section Number", "Section Number"),
//                                        new KeyValuePair("Period Number", "Period Number"),
//                                        new KeyValuePair("District Number", "Instruct District"),
//                                        new KeyValuePair("School Number", "Instruct School"),
//                                        new KeyValuePair("Survey Period", "Survey Period"),
//                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
//                                        new KeyValuePair("Term", "Term")))),
//                        "Each Teacher Course Schedule record must have a matching Student Course Schedule record based on District Number, Current Instruction/Service; "
//                                + "School Number, Current Instruction/Service; Survey Period Code; Fiscal Year; Term; Course Number; Section Number and Period Number.")),
                // TODO Course Number Class Size Core Course Indicator
                // School of Instructions function setting on MSID does not equal D or V
                // Class Size Core Course Indicator on the Course Code Directory = Y
                new FLValidationRule("MTC", "53", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("District Number", "^(?!68|71)\\S{1,2}$"),
                                Restriction.pattern("School Number", "^(?!3518|9996|[A-Z]\\d{3})\\S{1,4}$"),
                                Restriction.equals("Blended Learning", "N"),
                                Restriction.pattern("Period Number", "^\\d{2}88$"),
                                Restriction.pattern("Facility Type", "^(0[0134]|1[1-9]|20)$"),
                                Restriction.and(
                                        Restriction.equals("Facility Type", "09"),
                                        Restriction.pattern("Course Number", "^\\d{1,7}$")),
                                Restriction.equals("Scheduling Method", "A"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Scheduling Method", "^(A|I)$"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("District Number", "^(?!68|71)\\S{1,2}$"),
                                Restriction.pattern("School Number", "^(?!3518|9996|[A-Z]\\d{3})\\S{1,4}$"),
                                Restriction.equals("Blended Learning", "N"),
                                Restriction.pattern("Period Number", "^\\d{2}88$"),
                                Restriction.pattern("Facility Type", "^(0[0134]|1[1-9]|20)$"),
                                Restriction.and(
                                        Restriction.equals("Facility Type", "09"),
                                        Restriction.pattern("Course Number", "^\\d{1,7}$")),
                                Restriction.pattern("Scheduling Method", "^(C|M)$"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Scheduling Method", "^(C|M)$"), null)))),
                        "If Survey Period Code equals 2 or 3 and"
                                + " District of Instruction is not = 68 or 71"
                                + " School of Instruction is not = 3518, 9996, or begins with an alpha character and"
                                + " School of Instructions function setting on MSID does not equal D or V (DJJ or Virtual) and"
                                + " Blended Learning Course = N and"
                                + " Period Number does not end in 88 and"
                                + " Facility Type = 00, 01, 03, 04, 11-20 and"
                                + " Facility Type = 09 with numeric course and"
                                + " Class Size Core Course Indicator on the Course Code Directory = Y,"
                                + "Then for each Teacher Course record with a Scheduling Method equal to A, all other Teacher Course records with the same "
                                + "District Number, Current Instruction Service; School Number, Current Instruction/Service; Period Number; Term; "
                                + "Classroom Identification Number; Survey Period Code and Fiscal Year must have the same Scheduling Method unless the Scheduling Method is I."
                                + "OR Then for each Teacher Course record with a Scheduling Method equal to C or M, at least one other Teacher Course record must exist "
                                + "with the same District Number, Current Instruction Service; School Number, Current Instruction/Service; Period Number; Term; "
                                + "Classroom Identification Number; Survey Period Code and Fiscal Year and a Scheduling Method of C or M.")),
                new FLValidationRule("MTC", "54", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Term", "6"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Term", "7"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "N"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Term", "7"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Term", "6"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "N"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, "
                                + "Current Instruction/Service; Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; "
                                + "Course Number; and Period Number the following conditions must be true:"
                                + " If Survey Period Code is 2 or 3, Term is 6 and Term/Survey Indicator is Y, then records with Term 7 must have a Term/Survey Indicator of N."
                                + " If Survey Period Code is 2 or 3, Term is 7 and Term/Survey Indicator is Y, then records with Term 6 must have a Term/Survey Indicator of N.")),
                new FLValidationRule("MTC", "55", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Term", "8"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Term", "9"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "N"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Term", "9"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Term", "8"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "N"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number; and Period Number the following conditions must be true:"
                                + " If Survey Period Code is 2 or 3, Term is 8 and Term/Survey Indicator is Y, then records with Term 9 must have a Term/Survey Indicator of N."
                                + " If Survey Period Code is 2 or 3, Term is 9 and Term/Survey Indicator is Y, then records with Term 8 must have a Term/Survey Indicator of N.")),
                new FLValidationRule("MTC", "56", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2"),
                                Restriction.pattern("Term", "^(J|K|L)$"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Term", "^(J|K|L)$"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "Y"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number; and Period Number the following condition must be true:"
                                + "If Survey Period Code is 2 and Term is J, K, or L, only one of these Terms may have a Term/Survey Indicator of Y.")),
                new FLValidationRule("MTC", "57", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "3"),
                                Restriction.pattern("Term", "^(M|N|O)$"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Term", "^(M|N|O)$"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "Y"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number and Period Number the following condition must be true:"
                                + "If Survey Period Code is 3 and Term is M, N, or O, only one of these Terms may have a Term/Survey Indicator of Y.")),
                new FLValidationRule("MTC", "58", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Term", "^(E|F|G|H|I)$"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Term", "^(E|F|G|H|I)$"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "Y"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number and Period Number the following condition must be true:"
                                + "If Survey Period Code is 2 or 3 and Term is E, F, G, H, or I, only one of these Terms may have a Term/Survey Indicator of Y.")),
                new FLValidationRule("MTC", "59", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Term", "^(B|C|D)$"),
                                Restriction.equals("Survey Indicator", "Y"))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Term", "^(B|C|D)$"), null),
                                        new KeyValuePair(Restriction.equals("Survey Indicator", "Y"), null)))),
                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number and Period Number the following condition must be true:"
                                + "If Survey Period Code is 2 or 3 and Term is B, C or D, only one of these Terms may have a Term/Survey Indicator of Y.")),
//                new FLValidationRule("MTC", "5A", new FLValidationRuleSet(new RuleSet(
//                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
//                                Restriction.pattern("Term", "^(T|U|V|W|X)$"),
//                                Restriction.equals("Survey Indicator", "Y"))
//                                .testThen(Restriction.validateMatchInExport(0, "EXPDATA-FL-MTC",
//                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
//                                        new KeyValuePair("Course Number", "Course Number"),
//                                        new KeyValuePair("Period Number", "Period Number"),
//                                        new KeyValuePair("District Number", "District Number"),
//                                        new KeyValuePair("School Number", "School Number"),
//                                        new KeyValuePair("Survey Period", "Survey Period"),
//                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
//                                        new KeyValuePair(Restriction.pattern("Term", "^(T|U|V|W|X)$"), null),
//                                        new KeyValuePair(Restriction.equals("Survey Indicator", "Y"), null)))),
//                        "For all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
//                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number; and Period Number the following condition must be true:"
//                                + "If Survey Period Code is 2 or 3 and Term is T, U, V, W or X, only one of these Terms may have a Term/Survey Indicator of Y.")),
                new FLValidationRule("MTC", "5B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term")))
                                .testThen(Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Indicator", "Survey Indicator")))),
                        "If Survey Period Code is 2 or 3, then all Teacher Course records that match on District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number; "
                                + "Period Number and Term must have the same Term/Survey Indicator.")),
                new FLValidationRule("MTC", "5C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term")))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair(Restriction.equals("Primary Instructor", "Y"), null)))),
                        "If Survey Period Code is 2 or 3, then at least one Primary Instructor Indicator Code must be Y for Teacher Course Schedule records "
                                + "that are matched on District Number, Current Instruction/Service; School Number, Current Instruction/Service; Survey Period Code; "
                                + "Fiscal Year; Term; Classroom Identification (FISH) Number and Period Number.")),
                new FLValidationRule("MTC", "5E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term")))
                                .testThen(Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term")))),
                        "If Survey Period Code is 2 or 3 and if Teacher Course records match on District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; Fiscal Year; Survey Period Code; Term; Course Number; Section Number; "
                                + "and Period Number; then Classroom Identification (FISH) Number must be the same.")),
                // TODO Charter School Status is not R, C, T or B and
                // School Function Setting is notV
                new FLValidationRule("MTC", "5F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("School Number",
                                        "^(?!C90[1-9]|C91\\d|C92[0-8]|U97\\d|U98[01]|(?!P000)P\\d{3}|7001|7006)\\S{1,4}$"),
                                Restriction.pattern("Course Number", "^(?![A-Z]\\S{1,6})\\S{1,6}$"),
                                Restriction.and(
                                        Restriction.pattern("District Number",
                                                "^(0[247]|1[2459]|2[0-8]|3[023489]|4[457]|54|6[2357]|75)$"),
                                        Restriction.pattern("School Number", "^(?!7004|7023)\\S{1,4}$")),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "Instruct District"),
                                        new KeyValuePair("School Number", "Instruct School"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair(Restriction.notEquals("Location of Student", "T"), null)))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STF",
                                        new KeyValuePair("Staff ID Local", "Staff ID Local"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair("SSN", "SSN")))),
                        "For Survey Periods 2 and 3,"
                                + " If the course is not a dual enrollment course at a postsecondary institution, and"
                                + " If School Number, Current Instruction/Service is not 7001 or 7006, and"
                                + " If School Number, Current Instruction/Service is not 7004 and 7023 in one of the following District Numbers, "
                                + "Current Instruction/Service: 02, 04, 07, 12, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 28, 30, "
                                + "32, 33, 34, 38, 39, 44, 45, 47, 54, 62, 63, 65, 67 and 75"
                                + " If Charter School Status is not R, C, T or B and School Function Setting is not V,"
                                + " If Location of Student code is not T on the Student Course Schedule record"
                                + "(matching on District Number, Current Instruction/Service; School Number"
                                + "Current Instruction/Service; Survey Period Code; Fiscal Year; Term; Course Number; Section Number and Period Number),"
                                + "then each Teacher Course Schedule record must have a matching Staff Demographic Information record based on District Number, "
                                + "Current Instruction/Service; Survey Period Code; Fiscal Year; Social Security Number and Staff Number Identifier, Local,"
                                + "Dual enrollment records are those with School Number, Current Instruction/Service of C901-C928, U970-U981 or P001-P999 "
                                + "and those with an alphanumeric course number at any School Number, Current Instruction/Service.")),
                new FLValidationRule("MTC", "63",
                        new ValidateRegularExpression("Certification Number", "^9999999999|000\\d{7}$",
                                "Florida Educators Certificate Number must be 0000000000 or in the range 0000000001 through 0000999998, "
                                        + "0001000000 through 0009999999, 0000999999 or 9999999999.")),
                new FLValidationRule("MTC", "64", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("School Number", "7001"))
                                .testThen(Restriction.pattern("FL Educators Cert Id", "^9999999999|000\\d{7}$"))),
                        "If the Florida Educators Certificate Number is less than 0009999999, but not 0000000000 or 0000999999 then the "
                                + "Florida Educators Certificate Number must be on the Florida Educators Certification file.")),
                new FLValidationRule("MTC", "65", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("Scheduling Method", "^(C|M)$"))
                                .testThen(Restriction.and(
                                        Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Scheduling Method", "Scheduling Method")),
                                        Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Scheduling Method", "Scheduling Method"),
                                                new KeyValuePair("SSN", "SSN"))))),
                        "If Survey Period Code equals 2 or 3, for each Teacher Course record with a Scheduling Method equal to C or M there "
                                + "must exist at least one other Teacher Course record with a different Social Security Number and the same Scheduling Method "
                                + "(at least 2 C, 2 M) based on District Number, Current Instruction/Service; School Number, Current Instruction/Service; Period Number; "
                                + "Classroom Identification/FISH Number; Survey Period Code and Fiscal Year.")),
                new FLValidationRule("MTC", "66", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.pattern("District Number", "^(?!71)\\S{1,2}$"),
                                Restriction.pattern("Period Number", "^\\d{2}88$"),
                                Restriction.equals("Scheduling Method", "I"))
                                .testThen(Restriction.and(
                                        Restriction.validateNotMatchInExport("EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("SSN", "SSN"),
                                                new KeyValuePair(
                                                        Restriction.pattern("Scheduling Method", "^(A|B|C|M|S|W)$"),
                                                        null)),
                                        Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(
                                                        Restriction.pattern("Scheduling Method", "^(A|B|C|M|S|W)$"),
                                                        null))))),
                        "If Survey Period Code equals 2 or 3 and District Number, Current Instruction/Service is not 71; "
                                + "for each Teacher Course record with a Scheduling Method equal to I and the end period is not equal to 88, "
                                + "there must exist at least one other Teacher Course record with a Scheduling Method equal to A, B, C, M, S or W "
                                + "based on District Number, Current Instruction/Service; School Number, Current Instruction/Service; Period Number; "
                                + "Classroom Identification/FISH Number; Survey Period Code and Fiscal Year, but with a different Social Security Number.")),
                new FLValidationRule("MTC", "67", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("Scheduling Method", "A"))
                                .testThen(Restriction.and(
                                        Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Course Number", "Course Number"),
                                                new KeyValuePair("Section Number", "Section Number"),
                                                new KeyValuePair("Scheduling Method", "Scheduling Method")),
                                        Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                                new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                                new KeyValuePair("Period Number", "Period Number"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair("Scheduling Method", "Scheduling Method"))))),
                        "If Survey Period Code equals 2 or 3, for each Teacher Course record with a Scheduling Method equal to A, "
                                + "there must exist at least one other Teacher Course record with a different Course/Section combination "
                                + "and the same Scheduling Method (at least 2 A), District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; Period Number; Classroom Identification/FISH Number; "
                                + "Survey Period Code and Fiscal Year.")),
                // TODO Course Number Class Size Core Course Indicator
                // School of Instructions function setting on MSID does not equal D or V
                // Class Size Core Course Indicator on the Course Code Directory = Y
                // sum of three years experience
                new FLValidationRule("MTC", "68", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.notEquals("District Number", "68"),
                                Restriction.pattern("School Number", "^(?!3518|9996|[A-Z]\\d{3})\\S{1,4}$"),
                                Restriction.equals("Blended Learning", "N"),
                                Restriction.pattern("Period Number", "^(?!\\d{2}88)\\d{4}$"),
                                Restriction.pattern("Facility Type", "^(0[0134]|1[1-9]|20)$"),
                                Restriction.equals("Scheduling Method", "C"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.pattern("Certification Status", "^(A|H|I|M|P|S|V)$"),
                                                null)))),
                        "If Survey Period Code is 2 or 3 and"
                                + " District of Instruction is not = 68 and"
                                + " School of Instruction is not = 3518, 9996, or begins with an alpha character and"
                                + " School of Instructions function setting on MSID does not equal D or V (DJJ or Virtual) and"
                                + " Blended Learning Course = N and"
                                + " Period Number does not end in 88 and"
                                + " Facility Type = 00, 01, 03, 04, 11-20 and"
                                + " Facility Type = 09 with a numeric course and"
                                + " Scheduling Method = C and"
                                + " Class Size Core Course Indicator on the Course Code Directory = Y, "
                                + "then for all Teacher Course records that match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; "
                                + "Fiscal Year; Survey Period Code; Classroom Identification (FISH) Number; Course Number; and Period Number and Scheduling Method C, "
                                + "the following conditions must be true:"
                                + " At least one Teacher Course record must have Certification/Licensure/Qualification Status equal to A, H, I, M, P, S or V"
                                + " At least one teacher in the group of Teacher Course records must have at least a sum of three years experience on the "
                                + "Staff Experience format where Experience Type equals F, N, P or S. Must match by District Number, SSN, Survey Period, Year"
                                + " Same teacher does not need to satisfy both conditions")),
                // TODO Course Number Class Size Core Course Indicator
                // School of Instructions function setting on MSID does not equal D or V
                // Class Size Core Course Indicator on the Course Code Directory = Y
                // sum of three years experience
                new FLValidationRule("MTC", "69", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.notEquals("District Number", "68"),
                                Restriction.pattern("School Number", "^(?!3518|9996|[A-Z]\\d{3})\\S{1,4}$"),
                                Restriction.equals("Blended Learning", "N"),
                                Restriction.pattern("Period Number", "^(?!\\d{2}88)\\d{4}$"),
                                Restriction.pattern("Facility Type", "^(0[0134]|1[1-9]|20)$"),
                                Restriction.equals("Scheduling Method", "I"),
                                Restriction.validateMatchInExport(2, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport(1, "EXPDATA-FL-MTC",
                                        new KeyValuePair("Classroom Id Number", "Classroom Id Number"),
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(
                                                Restriction.pattern("Certification Status", "^(A|H|I|M|P|S|V)$"),
                                                null)))),
                        "If Survey Period Code is 2 or 3 and"
                                + " District of Instruction is not = 68 and"
                                + " School of Instruction is not = 3518, 9996, or begins with an alpha character and"
                                + " School of Instructions function setting on MSID does not equal D or V (DJJ or Virtual) and"
                                + " Blended Learning Course = N and"
                                + " Period Number does not end in 88 and"
                                + " Facility Type = 00, 01, 03, 04, 11-20 and"
                                + " Facility Type = 09 with a numeric course and"
                                + " Scheduling Method = I and"
                                + " Class Size Core Course Indicator on the Course Code Directory = Y,"
                                + "then for all Teacher Course records that are also Class Size Core Course indicated with Y on the Course Code Directory that "
                                + "match on District Number, Current Instruction/Service; School Number, Current Instruction/Service; Fiscal Year; Survey Period Code; "
                                + "Classroom Identification (FISH) Number; and Period Number, the following conditions must be true:"
                                + " At least one Teacher Course record must have Certification/Licensure/Qualification Status equal to A, H, I, M, P, S or V"
                                + " At least one teacher in the group of Teacher Course records must have at least a sum of three years experience on the "
                                + "Staff Experience format where Experience Type equals F, N, P or S. Must match by District Number, SSN, Survey Period, Year"
                                + " Same teacher does not need to satisfy both conditions"))
        });
    }
}
