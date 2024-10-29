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
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * The Class FLStudentDemographicValidation.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentExtractValidation {


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

    DateAsStringConverter m_dateConverter;
    List<FLValidationRule> m_validationRules;

    /**
     * Instantiates a new FL student extract validation.
     */
    public FLStudentExtractValidation() {
        m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
        m_validationRules = new ArrayList();
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("STD", "2", new ValidateDistrictNumber("District Number, E")),
                new FLValidationRule("STD", "3", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]|9$"))
                                .testThen(Restriction.pattern("School Number",
                                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89]|999[237])$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.pattern("School Number",
                                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|N99[89]|999[237])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[68]$"))
                                .testThen(Restriction.pattern("School Number",
                                        "^(?!9001$)(?!3518$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$"))),
                        "If Survey Period Code is 1-4 or 9, School Number, Current Enrollment must be numeric in the range 0001 to 9899, excluding 9001 "
                                + "or it must be 9992, 9993, 9997, N998 or N999. If Survey Period Code is 5, School Number, Current Enrollment must be numeric "
                                + "in the range 0001 to 9899, excluding 9001 or it must be 9992, 9993, 9995, 9997, N998 or N999. "
                                + "If Survey Period Code is 6 or 8, School Number Current Enrollment must be numeric in the range 0001 to 9899, excluding 3518 and 9001.")),
                new FLValidationRule("STD", "4", new ValidateRegularExpression("Student Number",
                        "^(?!(00|69|7[067]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-68, 71-75 or 78-79."
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("STD", "5", new ValidateRegularExpression(
                        "Survey Period", "^[1-6]|[89]$", "Survey Period Code must be 1, 2, 3, 4, 5, 6, 8 or 9.")),
                new FLValidationRule("STD", "6", new ValidateFiscalYear()),
                new FLValidationRule("STD", "7", new ValidateRegularExpression("Student Number Alias",
                        "^(?!(00|69|7[067]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w| {10}$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-68, 71-75 or 78-79."
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("STD", "9", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.uniqueValue("Student Number", "District Number, I/S",
                                        "Fiscal Year"))),
                        "Each Student Demographic record must be unique based on District Number, "
                                + "Current Instruction/Service; Student Number Identifier, Florida; Survey Period Code and Year.")),
                new FLValidationRule("STD", "10", new ValidateRegularExpression("Student ID Local",
                        "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                        "Student Number Identifier, Local may be any combination of letters, numbers and blanks. "
                                + "(All blanks are allowable.) It must be left-justified with trailing blanks.")),
                new FLValidationRule("STD", "11", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(5|9)$"))
                                .testThen(Restriction.patternForFields(
                                        "^(000[0-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|A[0-9][0-9][0-9])$",
                                        "Institution Number 1", "Institution Number 2",
                                        "Institution Number 3")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-4]|7)$"))
                                .testThen(Restriction.patternForFields(
                                        "^0000$",
                                        "Institution Number 1", "Institution Number 2",
                                        "Institution Number 3"))),
                        "If Survey Period is 5 or 9, Institution Number, Neglected/Delinquent (First); Institution Number, Neglected/Delinquent (Second) "
                                + "and Institution Number, Neglected/Delinquent (Third) must be numeric in the range 0000 to 9899 "
                                + "or they must be a district assigned 3 digit number preceded by an A. "
                                + "If Survey Period is not 5 or 9, Institution Number, Neglected/Delinquent must be 0000. (This edit does "
                                + "not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "12", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("Institution Number 1", "0000"))
                                .testThen(Restriction.byExtFldRefTable("FL-PGM-NEGLECT", "pgm-institution",
                                        "Institution Number 1")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("Institution Number 2", "0000"))
                                .testThen(Restriction.byExtFldRefTable("FL-PGM-NEGLECT", "pgm-institution",
                                        "Institution Number 2")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("Institution Number 3", "0000"))
                                .testThen(Restriction.byExtFldRefTable("FL-PGM-NEGLECT", "pgm-institution",
                                        "Institution Number 3"))),
                        "If the Institution Number, Neglected/Delinquent (First); Institution Number, Neglected/Delinquent (Second) or Institution Number, "
                                + "Neglected/Delinquent (Third) is not 0000 then it must be a valid institution for neglected/delinquent children in the District of Enrollment."
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "13", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Survey Period", "^([1-4]|7)$"),
                                Restriction.equals("Institution Number 1", "0000"))
                                .testThen(Restriction.equals("Institution Number 2", "0000"))
                                .testThen(Restriction.equals("Institution Number 3", "0000")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-4]|7)$"),
                                Restriction.equals("Institution Number 2", "0000"))
                                .testThen(Restriction.equals("Institution Number 3", "0000"))),
                        "If Institution Number, Neglected/Delinquent (First) is 0000 then Institution Number, Neglected/Delinquent (Second) must also be 0000. "
                                + "If Institution Number, Neglected/Delinquent (First) or Institution Number, Neglected/Delinquent (Second) is 0000 "
                                + "then Institution Number, Neglected/Delinquent (Third) must be 0000. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "14", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.pattern("Grade Level", "^3[01]$")).testThen(
                                        Restriction.equals("School Number", "9997"))),
                        "If Survey Period is 2 or 3 and Grade Level is 30 or 31, then School Number, Current Enrollment must be 9997")),
                new FLValidationRule("STD", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.pattern("Migrant Status Term", "^[BDESTUVWX]$"))
                                .testThen(Restriction.and(Restriction.greaterThanOrEquals("Birth Date",
                                        (Date) m_dateConverter.parseSystemString("1995-09-01"), "MMddyyyy"),
                                        Restriction.lessThanOrEquals("Birth Date",
                                                (Date) m_dateConverter.parseSystemString("2018-08-31"), "MMddyyyy")))),
                        "If Migrant Status Term is B, D, E, S, T, U, V, W or X, then Birth Date must be 09011995 through 08312018 inclusive. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "16", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(1|4|5|9)$"))
                                .testThen(Restriction.equals("District Number Z", "00")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"))
                                .testThen(Restriction.pattern("District Number Z", "^([0-5][0-9]|6[0-7])$"))),
                        "If Survey Period Code = 1, 4, 5 or 9, then District Number, Zoned School must be filled with zeroes, If Survey Period Code = 2 "
                                + "or 3, then District Number, Zoned School must be 00-67.")),
                // TODO School Function/Setting is not D (DJJ) on MSID and Charter School Status is
                // Z (not Charter) on MSID
                new FLValidationRule("STD", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(1|4|5|9)$"))
                                .testThen(Restriction.equals("School Number Z", "0000")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)"),
                                Restriction.notEquals("District Number, I/S", "71"),
                                Restriction.notEquals("District Number, E", "68"),
                                Restriction.pattern("School Number",
                                        "^(?!3518|7001|7004|7006|7023)"
                                                + "(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$"),
                                Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"))
                                .testThen(Restriction.byActiveSchool("School Number Z"))),
                        "If Survey Period Code = 1, 4, 5 or 9, then School Number, Zoned School must be filled with zeroes."
                                + "If Survey Period Code = 2 or 3 and District of Instruction is not 71 and School of Enrollment is in the range 0001-9899 "
                                + "but not 3518, 7001, 7004, 7006, 7023 and School Function/Setting is not D (DJJ) on MSID and Charter School Status is Z (not Charter)"
                                + " on MSID and Grade Level code is KG-12 and Accountability ESE Center is Y or Primary Service Type is B (AlternativeEducation) "
                                + "on MSID and Neglected, Delinquent Status is not (D) or (N) on MSIDthen "
                                + "School Number, Zoned School must be an active school for the District Number, Zoned School on the Master School Identification file "
                                + "unless District of Instruction or District of Enrollment is 68 in which case School Number, Zoned School can also be zeroes. "
                                + "Any other School of Enrollment can also report zeros for School Number, Zoned School."
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(9),
                                        Integer.valueOf(11),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "5"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(9),
                                        Integer.valueOf(11),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "5"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_3))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(9),
                                        Integer.valueOf(11),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "6"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "6"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "PK"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(9),
                                        Integer.valueOf(11),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "KG"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(11),
                                        Integer.valueOf(12),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "1"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(12),
                                        Integer.valueOf(13),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "2"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(13),
                                        Integer.valueOf(14),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "3"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(14),
                                        Integer.valueOf(15),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "4"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(15),
                                        Integer.valueOf(16),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "^[89]$"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(16),
                                        Integer.valueOf(18),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "^[89]$"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(18),
                                        Integer.valueOf(19),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "7"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(19),
                                        Integer.valueOf(20),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "8"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(20),
                                        Integer.valueOf(27),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "9"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(27),
                                        Integer.valueOf(28),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "10"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(28),
                                        Integer.valueOf(29),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "11"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(29),
                                        Integer.valueOf(30),
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "^[89]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997)\\S{4}$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(30), null,
                                        true, false, new RuntimeParam(RuntimeParam.DATE_CERTAIN_OF_SURVEY_2)))),
                        "If School Number, Current Enrollment is not 3518, 9992, 9993, 9995 or 9997, then there must be a valid association between the Grade Level "
                                + "listed below and the student's age. "
                                + "For Survey Periods 1-4, age will be determined as of Date Certain (Friday) of the survey period. "
                                + "For Survey Period 5, age will be determined as of Date Certain of Survey 3. "
                                + "For Survey Period 6, age will be determined as of the Survey Due Date. "
                                + "For Survey Periods 8 and 9, age will be determined as of Date Certain of Survey 2."
                                + "PK-not equal to 9+, KG-not equal to 0-3, 11+, 1-not equal to 0-4, 12+, 2-not equal to 0-4, 13+, 3-not equal to 0-4, 14+, "
                                + "4-not equal to 0-4, 15+, 5-not equal to 0-4, 16+, 6-not equal to 0-4, 18+, 7-not equal to 0-4, 19+, 8-not equal to 0-4, 20+, "
                                + "9-not equal to 0-4, 27+, 10-not equal to 0-4, 28+, 11-not equal to 0-4, 29+, 12-not equal to 0-4, 30+")),
                new FLValidationRule("STD", "19", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-5]$"),
                                Restriction.pattern("ELL", "^(LY|LP)$"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.notEquals("Grade Level", "PK"),
                                Restriction.notEquals("Date Entered US Skl", "00000000"))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "Date Entered US Skl")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "9"))
                                .testThen(Restriction.equals("Date Entered US Skl", "00000000"))),
                        "If Survey Period Code is 1-4 or 5, and English Language Learners, PK-12 code = LY or LP, "
                                + "and School Number, Current Enrollment is not 3518, and Grade Level is not PK, "
                                + "then Date Entered United States School must be numeric and a valid date in the format MMDDYYYY. "
                                + "Date Entered United States School may be 00000000 for all others. "
                                + "For Survey Period 9, Date Entered United States School must be 00000000. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "1A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.pattern("Ethnicity", "^(Y|N)$"))),
                        "Ethnicity code must be Y or N. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "1B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.and(
                                        Restriction.pattern("Race American Native", "^(Y|N)$"),
                                        Restriction.pattern("Race Asian", "^(Y|N)$"),
                                        Restriction.pattern("Race Black", "^(Y|N)$"),
                                        Restriction.pattern("Race Pacific", "^(Y|N)$"),
                                        Restriction.pattern("Race White", "^(Y|N)$")))),
                        "Race: American Indian or Alaska Native, Race: Asian, Race: Black or African American, "
                                + "Race: Native Hawaiian or Other Pacific Islander, and Race: White codes must be Y or N. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "1C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.or(Restriction.equals("Race American Native", "Y"),
                                        Restriction.equals("Race American Native", "Y"),
                                        Restriction.equals("Race Asian", "Y"),
                                        Restriction.equals("Race Black", "Y"),
                                        Restriction.equals("Race Pacific", "Y"),
                                        Restriction.equals("Race White", "Y")))),
                        "At least one Race code (Race: American Indian or Alaska Native, Race: Asian, Race: Black or African American, "
                                + "Race: Native Hawaiian or Other Pacific Islander, or Race: White) must be Y. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "1D", new FLValidationRuleSet(new RuleSet(
                        // TODO implement field logic for period 8
                        // ValidationRule.testIf(Restriction.equals("Survey Period", "8"))
                        // .testThen(Restriction.pattern("Primary Instructor R",
                        // "^((?!000000000$)([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]))|(CS[0-9][0-9][0-9][0-9][0-9][0-9][0-9])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.equals("Primary Instructor R", "000000000"))),
                        "If Survey Period equals 8, then Primary Instructor Responsible, Reading code must be numeric and greater than zero or "
                                + "the first two positions must be <CS> and the last seven positions must be numeric. If Survey Period does not equal 8, "
                                + "then Primary Instructor Responsible, Reading code must be all zeroes (This edit does not apply to Survey Period 6.)")),
                new FLValidationRule("STD", "1E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("School Number", "7004"),
                                Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"))),
                        "If School Number, Current Enrollment is 7004 then Grade Level must be KG through 12. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "20-21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-6]|[89]$"))
                                .testThen(Restriction.and(Restriction.pattern("Student Name Legal",
                                        "^(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {0,}$|^(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {0,}$|^(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {1,}(?!Z{2,})[\\S]{1,} {0,}$"),
                                        Restriction.pattern("Student Name Legal", "^(?!Z{3})")))),
                        "The Student Name, Legal: First Name must not be blank (Z-fill is NOT allowed). Allowable characters include double "
                                + "or single quotation marks, commas, slashes, periods, hyphens and accent marks. "
                                + "Student middle name/appendage may be blank but must not include nondisplayable characters. "
                                + "Allowable characters include double or single quotation marks, commas, slashes, periods, parentheses, hyphens and accent marks.")),
                new FLValidationRule("STD", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "Birth Date"))),
                        "If School Number, Current Enrollment is not 9995, then Birth Date must be numeric and a valid date in the format MMDDYYYY")),
                new FLValidationRule("STD", "23", new ValidateRegularExpression(
                        "Gender Code", "^(M|F)$", "Gender code must be M or F")),
                new FLValidationRule("STD", "25", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("District Number, E", "72"),
                                Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "3518"))
                                .testThen(Restriction.pattern("Resident County", "^5[06]$"))),
                        "If District Number, Current Enrollment is 72 and if School Number, Current Enrollment = 3518, then Residence County code must "
                                + "be 50 or 56. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"),
                                Restriction.pattern("School Number Z", "^(?!0000$)[0-9][0-9][0-9][0-9]$"))
                                .testThen(Restriction.pattern("District Number Z", "^(?!00$)[0-9][0-9]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"),
                                Restriction.pattern("District Number Z", "^(?!00$)[0-9][0-9]$"))
                                .testThen(Restriction.pattern("School Number Z",
                                        "^(?!0000$)[0-9][0-9][0-9][0-9]$"))),
                        "If Survey Period Code = 2 or 3 and School Number, Zoned School is greater than 0000, then District Number, Zoned School must "
                                + "be greater than 00. If the District Number, Zoned School is greater than 00, then School Number, Zoned School must be "
                                + "greater than 0000. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)[1-5]|9$"))
                                .testThen(Restriction.pattern("ELL", "^(L[Y|F|P|Z]|ZZ)$"))),
                        "If Survey Period is 1, 2, 3, 4, 5, or 9, English Language Learners, PK-12 code must be LY, LF, LP, LZ, or ZZ. (This edit does "
                                + "not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(5|9)$"),
                                Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.pattern("Resident Status", "^([0AB]|[2-7])$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1-4]$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.pattern("Resident Status", "^[0AB23]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(2|3)$"),
                                Restriction.pattern("Grade Level", "^3[01]$"),
                                Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.pattern("Resident Status", "^[4-7]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "9995"))
                                .testThen(Restriction.equals("School Number", "Z"))),
                        "If Survey Period Code is 5 or 9 and School Number, Current Enrollment is not 9995, Resident Status, State/County "
                                + "code must be 0, A, B, 2, 3, 4, 5, 6 or 7. If Survey Period Code is 1-4, Grade Level is not 30 or 31, "
                                + "and School Number, Current Enrollment is not 9995, then Resident Status, State/County code must be 0, "
                                + "A, B, 2 or 3. If Survey Period Code is 2 or 3, Grade Level is 30 or 31, and School Number, Current "
                                + "Enrollment is not 9995, then Resident Status, State/County code must be 4, 5, 6 or 7. If School Number, "
                                + "Current Enrollment = 9995, then Resident Status State/County must be Z. (This edit does not apply to Survey "
                                + "Periods 6 and 8.)")),
                new FLValidationRule("STD", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1469]$"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "8"))
                                .testThen(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "9"),
                                Restriction.notEquals("Institution Number 1", "0000"))
                                .testThen(Restriction.equals("Grade Level", "^(PK|KG|0[1-9]|1[0-2]|30)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"))
                                .testThen(Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2]|3[01])$"))),
                        "If Survey Period Code = 1, 4, 6, or 9, Grade Level code must be PK, KG, or 01-12. If Survey Period Code = 8, then Grade Level "
                                + "must be KG or 01-12. If Survey Period Code = 9, then Grade Level may also = 30 if Institution Number, "
                                + "Neglected/Delinquent (First) does not =0000. If Survey Period Code = 2, 3 or 5, Grade Level code must "
                                + "be PK, KG, 01-12, 30 or 31.")),
                new FLValidationRule("STD", "30", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)^[1-5]|^9$"))
                                .testThen(Restriction.pattern("Agency Code", "^(A|C|Z)$"))),
                        "If Survey Period Code is 1-4, 5 or 9, Student Characteristic, Agency Programs code must be A, C or Z. (This edit does not apply "
                                + "to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "31", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-5]|9)$"),
                                Restriction.pattern("Grade Level", "^(09|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997|N998|N999)\\S{4}$"),
                                Restriction.equals("District Number, I/S", "71"),
                                Restriction.equals("District Number, E", "71"))
                                .testThen(Restriction.pattern("Graduation Option", "^([1-9]|A|B)$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-5]|9)$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9995|9997|N998|N999)\\S{4}$"))
                                .testThen(Restriction.equals("Graduation Option", "Z")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-5]|9)$"),
                                Restriction.equals("District Number, I/S", "71"),
                                Restriction.notEquals("District Number, E", "71"))
                                .testThen(Restriction.equals("Graduation Option", "Z")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^([1-5]|9)$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-8]|3[01])$"))
                                .testThen(Restriction.equals("Graduation Option", "Z"))),
                        "If Survey Period Code is 1-5 or 9; and If Grade Level equals 9 - 12; and If School Number, Current Enrollment does "
                                + "not equal 9992, 9993, 9995, 9997, N998, N999, or 3518; or If both District Number, Current Instruction/Service and "
                                + "District Number, Current Enrollment equal 71; then Graduation Option code must be 1, 2, 3, 4, 5, 6, 7, 8, 9, A or B. "
                                + "If School Number, Current Enrollment = 9992, 9993, 9995, 9997, N998, N999, 3518, or if District Number, Current Instruction/Service "
                                + "equals 71 and District Number, Current Enrollment does not equal 71, then Graduation Option code must be Z. "
                                + "If Grade Level equals PK-8, 30, or 31, then Graduation Option code must be Z.(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "32", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "3518"),
                                Restriction.equals("District Number, E", "73"))
                                .testThen(Restriction.pattern("Resident County", "^(06|37)$"))),
                        "If District Number, Current Enrollment is 73 and if School Number, Current Enrollment = 3518, then Residence County code must be 06 or 37. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "33", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("Migrant QAD", "00000000"))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "Migrant QAD"))),
                        "Qualifying Arrival Date (QAD) for Migrant Program Eligibility must be numeric and a valid date or must be all zeroes."
                                + " (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "34", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.pattern("Resident County", "^(?!00$)[0-5][0-9]|6[0-7]|99$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "9995"))
                                .testThen(Restriction.equals("Resident County", "00"))),
                        "If School Number, Current Enrollment is not 9995, then Residence County code must be 01-67 or 99. "
                                + "If School Number, Current Enrollment is 9995, then Residence County code must be 00. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "35", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.pattern("Lunch Status", "^[0134CDEFNR]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                                .testThen(Restriction.equals("Lunch Status", "Z"))),
                        "If School Number, Current Enrollment is not 9992, 9993, or 9995, then "
                                + "Lunch Status must be 0, 1, 3, 4, C, D, E, F, N or R. "
                                + "If School Number, Current Enrollment is 9992, 9993, or 9995, then Lunch Status must be Z. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "36", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"))
                                .testThen(Restriction.pattern("Migrant Status Term", "^[BDESTUVWXZ]$"))),
                        "Migrant Status Term code must be B, D, E, S, T, U, V, W, X or Z. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "37", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Migrant Status Term", "Z"))
                                .testThen(Restriction.equals("Migrant QAD", "00000000")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Migrant Status Term", "^[BDESTUVWX]$"))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "Migrant QAD"))),
                        "If Survey = 5 and Migrant Status Term is B, D, E, S, T, U, V, W or X, Qualifying Arrival Date (QAD) for Migrant Program "
                                + "Eligibility must be numeric and a valid date. If Migrant \n" +
                                "Status Term equal Z, then QAD must be all zeros. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "38", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Migrant Status Term", "^[BDESTUVWX]$"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Birth Date", generateDate("09", "01", "2013")),
                                        Restriction.lessThanOrEquals("Migrant QAD",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "If Migrant Status Term is B, D, E, S, T, U, V, W or X, then Qualifying Arrival Date (QAD) for Migrant Program Eligibility "
                                + "must be greater than or equal to 9/1/2013 and less than or equal to 08/31/2018. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "39", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Resident County", "99"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"))
                                .testThen(Restriction.pattern("Resident Status", "^(0|2)$"))),
                        "If Residence County code is 99 and Grade Level is PK-12, then Resident Status, State/County code must be 0 or 2. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number",
                                "^(?!9992|9993|9995|N998|N999)\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If the School Number, Current Enrollment is not 9992, 9993, 9995, N998 or N999, then it must exist on the Master School "
                                + "Identification File as a valid active school (active in the current school year) in the district of enrollment.")),
                new FLValidationRule("STD", "41", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("School Number", "9995"))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.PERIOD_END_DATE))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "9995"),
                                Restriction.equals("Migrant Status Term", "Z"))
                                .testThen(Restriction.equals("Birth Date", "00000000")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("School Number", "9995"),
                                Restriction.notEquals("Migrant Status Term", "Z"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Birth Date", generateDate("09", "01", "1995")),
                                        Restriction.lessThanOrEquals("Birth Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "If School Number, Current Enrollment is not 9995, then Birth Date must be less than or equal to the survey due date. "
                                + "If School Number, Current Enrollment is 9995; Survey Period Code is not 6 or 8 and Migrant Status Term is Z; "
                                + "then Birth Date must be 00000000. If School Number, Current Enrollment is 9995; Survey Period Code is not 6 or 8; "
                                + "and Migrant Status Term is not Z, then Birth Date must be 09011995 through 08312018 inclusive.")),
                new FLValidationRule("STD", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Resident County", "A"))
                                .testThen(Restriction.notEqualsFieldValue("District Number, E", "Resident County",
                                        String.class))),
                        "If Resident Status, State/County is A, then District Number, Current Enrollment and Residence County must not be the same. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "43", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-std-NativeLanguage", "Native Language")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                                .testThen(Restriction.equals("Native Language", "ZZ"))),
                        "If School Number, Current Enrollment is not 9992, 9993, or 9995, then Native Language, Student must be a valid language code, "
                                + "containing no blanks. If School Number, Current Enrollment is 9992, 9993, or 9995, then Native Language, Student code must be ZZ. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "44", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Resident Status", "3"),
                                Restriction.equals("District Number, E", "^(?!00$)[0-5][0-9]|6[0-7]$"))
                                .testThen(Restriction.notEqualsFieldValue("Resident County", "District Number, E",
                                        String.class))),
                        "If District Number, Current Enrollment is 01-67 and if Resident Status, State/County is 3, then District Number, Current Enrollment and "
                                + "Residence County must be the same. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "45", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.byAliasFldRefTable("all-std-PrimaryLanguage",
                                        "Primary Language")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                                .testThen(Restriction.equals("Primary Language", "ZZ"))),
                        "If School Number, Current Enrollment is not 9992, 9993, or 9995, then the Primary Language Spoken in Home code must be a valid language "
                                + "If School Number, Current Enrollment is 9992, 9993, or 9995, then the Primary Language Spoken in Home code must be ZZ. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "46", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.and(
                                        Restriction.byAliasFldRefTable("all-std-BirthCountry", "Country of Birth"),
                                        Restriction.notEquals("Country of Birth", "ZZ"))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Grade Level", "^3[01]$"))
                                .testThen(Restriction.or(
                                        Restriction.byAliasFldRefTable("all-std-BirthCountry", "Country of Birth"),
                                        Restriction.equals("Country of Birth", "ZZ"))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(9992|9993|9995)$"))
                                .testThen(Restriction.equals("Country of Birth", "ZZ"))),
                        "If Grade Level = PK-12 and if School Number, Current Enrollment is not 9992, 9993, or 9995, then Country of Birth must be a valid code "
                                + "as listed in Appendix G or Appendix Q of the DOE Information Database Requirements: Volume I -- Automated Student Information System Manual "
                                + "other than ZZ. If Grade Level = 30 or 31, then Country of Birth must be a valid code as listed in Appendix G or Appendix Q of the "
                                + "DOE Information Database Requirements: Volume I -- Automated Student Information System Manual including ZZ. "
                                + "If School Number, Current Enrollment is 9992, 9993, or 9995, then Country of Birth code must be ZZ. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "47", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Grade Level", "12"))
                                .testThen(Restriction.pattern("Additional School Y", "^[DSFZ]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.notEquals("Grade Level", "12"))
                                .testThen(Restriction.equals("Additional School Y", "Z"))),
                        "If Grade Level = 12, Additional School Year Student code must be D, S, F or Z. If Grade Level is not 12, then "
                                + "Additional School Year Student must be Z. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "48", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("Grade Level", "^3[01]$"),
                                Restriction.pattern("School Number", "^(9992|9993|9995|9997)$"))
                                .testThen(Restriction.equals("ELL Survey Date", "00000000")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "ELL Survey Date")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[13]$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"),
                                Restriction.or(Restriction.pattern("District Number, E", "^0[1-9]|[1-6][0-8]|7[1-5]$"),
                                        Restriction.and(Restriction.equals("District Number, E", "71"),
                                                Restriction.equals("District Number, I/S", "71"))))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[13]$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"),
                                Restriction.or(Restriction.pattern("District Number, E", "^0[1-9]|[1-6][0-9]|7[1-5]$"),
                                        Restriction.and(Restriction.notEquals("District Number, E", "71"),
                                                Restriction.equals("District Number, I/S", "71"))))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.DATE_CERTAIN_PLUS_90))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "4"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "06", "30"))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "9"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995|9997)\\S{4}$"))
                                .testThen(Restriction.lessThanOrEquals("Birth Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_DATE, "01", "01")))),
                        "If Grade Level = 30 or 31 or if School Number, Current Enrollment is 9992, 9993, 9995, or 9997 then "
                                + "English Language Learners: Home Language Survey Date must be zero-filled."
                                + "If School Number, Current Enrollment is not 9992, 9993, 9995, or 9997 and if Grade Level = PK-12, then "
                                + "English Language Learners: Home Language Survey Date must be a valid date and must meet the following criteria."
                                + "If District Number, Current Instruction is 01-68 or 72-75 or if both District Number, Current Enrollment and "
                                + "District Number, Current Instruction are 71; and Survey Period is 1-3, the date should be less than or equal "
                                + "to the date certain of survey week."
                                + "If District Number, Current Enrollment is not 71; District Number, Current Instruction is 71 and Survey Period is 1-3; "
                                + "then the date must be less than or equal to the date certain plus 90 calendar days."
                                + "For Survey Period 4, the date should be less than or equal to June 30 of the reporting year."
                                + "For Survey Period 5, the date should be less than or equal to August 31 of the reporting year."
                                + "For Survey Period 9, the date should be less than January 1 of the reporting year."
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "49", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.notEquals("Native Language", "ZZ")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^[9992|9993|9995]$"))
                                .testThen(Restriction.equals("Native Language", "ZZ"))),
                        "If Grade Level is PK-12 and School Number, Current Enrollment is not 9992, 9993, or 9995, then Native Language, Student must be other than ZZ. "
                                + "If School Number, Current Enrollment is 9992, 9993, or 9995, then Native Language, Student must be ZZ. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "4A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.equals("District Number, E", "71"),
                                Restriction.pattern("School Number", "^(0300|0400)$"))
                                .testThen(Restriction.notEquals("Resident County", "99"))),
                        "If District Number, Current Enrollment is 71 and School Number equals 0300 or 0400, then Residence County code must not equal 99. "
                                + "This edit does not apply to Survey Periods 6 and 8.")),
                new FLValidationRule("STD", "4B", new ValidateRegularExpression("FL Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                // TODO implement field logic for period 8
                // new FLValidationRule("STD", "51", new FLValidationRuleSet(new RuleSet(
                // ValidationRule.testIf(Restriction.equals("Survey Period", "8"))
                // .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STF",
                // new KeyValuePair("Primary Instructor R", "SSN"),
                // new KeyValuePair("District Number, E", "District Number"),
                // new KeyValuePair("School Number", "School Number"),
                // new KeyValuePair("Survey Period", "Survey Period"),
                // new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                // "If Survey = 8 there must be a matching Staff Demographic record based on the
                // following Student Demographic elements: "
                // + "District Number, Current Enrollment; School Number, Current Enrollment; Survey
                // Period Code; Year and Primary Instructor Responsible, "
                // + "Reading matched to Staff Demographic elements: District Number; School Number,
                // Primary/Home; Survey Period Code; Fiscal Year; "
                // + "Social Security Number (first nine positions).")),
                new FLValidationRule("STD", "52", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.equals("ELL", "LF"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair(Restriction.notEquals("Basis of Exit First", "Z"), null)))),
                        "If Survey Period Code is 2, 3 or 5, and If English Language Learners, PK-12 is LF and if there is a matching English Language Learners Information "
                                + "record and if there is a matching Student Course Schedule or Student End of Year Status record, then the English Language Learners: "
                                + "Basis of Exit (First) must not = Z. The match between the Student Demographic Information record and the English Language Learners Information "
                                + "record should be based on District Number, Current Instruction Service; Student Number Identifier, Florida; Survey Period Code and "
                                + "Fiscal/School Year. The match between the Student Demographic Information record and the Student Course Schedule record should be based on "
                                + "District Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Fiscal Year.The match between Student "
                                + "Demographic Information and Student End of Year Status record should be made based on District Number, Current Enrollment; "
                                + "Student Number, Identifier, Florida; Survey Period Code; and Year/School Year.")),
                new FLValidationRule("STD", "53", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("ELL", "^[LF|LY]$"),
                                Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code is 2, 3 or 5 and if English Language Learners, PK-12 is LY or LF and if Grade Level is KG-12 and if School Number, Current Enrollment "
                                + "is not 3518 and if there is a matching Student Course Schedule or Student End of Year Status record, then there must be a matching English Language "
                                + "Learners Information record based on District Number, Current Instruction Service; Student Number Identifier, Florida; Survey Period Code and Year. "
                                + "The match between Student Demographic and Student Course should be made based on District Number, Current Enrollment; Student Number Identifier, Florida; "
                                + "Survey Period Code; and Year/Fiscal Year. The match between Student Demographic and Student End of Year should be made based on District Number, "
                                + "Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Year/School Year. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "54", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[23]$"),
                                Restriction.equals("ELL", "LF"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.byDateFormat("MMddyyyy", "Exit Date"), null))),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.equals("ELL", "LF"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.equalsFieldValue("District Number, E", "District Number, I/S",
                                        String.class),
                                Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.byDateFormat("MMddyyyy", "Exit Date"), null)))),
                        "If the English Language Learners code = LF and if School Number, Current Enrollment is not 3518 and if Survey Period Code is 2 or 3 and if there "
                                + "is a matching Student Course Schedule record, then English Language Learners: Exit Date on the matching English Language Learners Information "
                                + "record must be a valid date. The match between the Student Demographic Information record and the Student Course Schedule record should be "
                                + "based on District Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Fiscal Year. If the English Language "
                                + "Learners code = LF and if School Number, Current Enrollment is not 3518 and if Survey Period Code is 5 and if there is a matching Student End of "
                                + "Year Status record and if District Number, Current Enrollment is equal to District Number, Current Instruction/Service, then English Language "
                                + "Learners: Exit Date on the matching English Language Learners Information record must be a valid date. The match between the Student Demographic "
                                + "Information record and the Student End of Year Status record should be based on District Number, Current Enrollment; Student Number Identifier,"
                                + " Florida; Survey Period Code; and Fiscal/School Year. The match between the Student Demographic Information record and the English Language "
                                + "Learners Information record should be based on District Number, Instruction/Service; Student Number Identifier, Florida; Survey Period Code "
                                + "and Fiscal/School Year.")),
                new FLValidationRule("STD", "55", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Grade Level", "Grade Level"),
                                        new KeyValuePair(Restriction.pattern("Diploma Type", "^[W07|W27]$"), null)))
                                .testThen(Restriction.pattern("Graduation Option", "^[4|7]$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Grade Level", "Grade Level"),
                                        new KeyValuePair(Restriction.pattern("Diploma Type", "^[W10|WGA|WGD]$"), null)))
                                .testThen(Restriction.equals("Graduation Option", "8"))),
                        "If Survey = 5 and Diploma Type on the Student End of Year Status record = W07 or W27, then Graduation Option code must equal 4 or 7; "
                                + "if Survey = 5 and Diploma Type = W10, WGA or WGD, then Graduation Option code must equal 8. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "56", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.equals("ELL", "LF"),
                                Restriction.notEquals("School Number", "3518"),
                                Restriction.or(
                                        Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                                Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.or(
                                                Restriction.and(
                                                        Restriction.equals("Reclass Exit Date", "00000000"),
                                                        Restriction.lessThanOrEquals("Exit Date", new RuntimeParam(
                                                                RuntimeParam.DATE_CERTAIN_PLUS_2_YEARS))),
                                                Restriction.and(
                                                        Restriction.notEquals("Reclass Exit Date", "00000000"),
                                                        Restriction.lessThanOrEquals("Reclass Exit Date",
                                                                new RuntimeParam(
                                                                        RuntimeParam.DATE_CERTAIN_PLUS_2_YEARS)))),
                                                null)))),
                        "If Survey Period Code is 2, 3 or 5 and if the English Language Learners, PK-12 code is LF and if School Number, Current Enrollment is not 3518 "
                                + "and if there is a matching Student Course Schedule or Student End of Year Status record then one of the following must be true for "
                                + "the English Language Learners Information record: English Language Learners: Reclassification Exit Date is all zeros and the English Language Learners: "
                                + "Exit Date is within two years of survey date or English Language Learners: Reclassification Exit Date is greater than zero and is within two years "
                                + "of survey date. Use the Survey Period 3 survey date for Survey Period 5 editing. The match between the Student Demographic record and the "
                                + "English Language Learners record should be based on District Number, Current Instruction/Service; Student Number Identifier, Florida; Survey "
                                + "Period Code; and Fiscal Year. If no matching English Language Learners Information record is found, then do not generate an error message. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "57", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("Graduation Option", "^[4|A|B]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number, E"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.notEquals("Term", "Y"), null)))
                                .testThen(Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Exceptionality Other",
                                                "^(?!T|U)(C|G|H|J|K|O|P|[S-W])$"), null)),
                                        Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.pattern("Except Primary",
                                                        "^(?!T|U)(C|G|H|J|K|O|P|[S-W])$"), null)))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"),
                                Restriction.pattern("Graduation Option", "^[4|A|B]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number, E"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Withdrawal Date", "^00000000$"), null)))
                                .testThen(Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Exceptionality Other",
                                                "^(?!T|U)(C|G|H|J|K|O|P|[S-W])$"), null)),
                                        Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.pattern("Except Primary",
                                                        "^(?!T|U)(C|G|H|J|K|O|P|[S-W])$"), null))))),
                        "If Survey Period Code is 5 and if Graduation Option code = 4, A or B, and if Prior School Status/Student Attendance Term does not equal Y, "
                                + "then Exceptionality, Primary or Exceptionality, Other on the Exceptional Student record must be reported with a code of C, G, H, J, K, O-P, "
                                + "S, V or W and these two fields must not contain T or U. If Survey Period Code is 2 or 3 and if Graduation Option code = 4, A or B, and if "
                                + "Withdrawal Date on the Prior School Status/Student Attendance record is 00000000; then Exceptionality, Primary or Exceptionality, Other "
                                + "on the Exceptional Student record must be reported with a code of C, G, H, J, K, O-P, S, V, or W and these two fields may not contain T or U. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "58", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[15]$"),
                                Restriction.equals("Graduation Option", "7"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Except Primary",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[1|5]$"),
                                Restriction.equals("Graduation Option", "7"),
                                Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Exceptionality Other",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null)))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Except Primary",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[234]$"),
                                Restriction.equals("Graduation Option", "7"),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Except Primary",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null))),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[234]$"),
                                Restriction.equals("Graduation Option", "7"),
                                Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Exceptionality Other",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null)))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Except Primary",
                                                "^(C|[F-K]|M|O|P|S|V|W)$"), null)))),
                        "If Survey Period Code is 1 or 5 and if Graduation Option code = 7, then Exceptionality, Primary on the Exceptional Student record must be "
                                + "C, F-K, M, O-P, S, V, or W. Exceptionality, Primary may also be L if Exceptionality, Other is C, F-K, M, O-P, S, V, or W. "
                                + "If Survey Period Code is 2, 3 or 4 and if there is a matching Student Course Schedule record and if Graduation Option code = 7, "
                                + "then Exceptionality, Primary on the Exceptional Student record must be C, F-K, M, O-P, S, V, or W. Exceptionality, Primary may also be L "
                                + "if Exceptionality, Other is C, F-K, M, O-P, S, V, or W. The match should use District Number, Current Enrollment; Student Number Identifier, "
                                + "Florida; Year/Fiscal Year and Survey Period Code. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "5A", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("ELL", "^[LY|LF]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.notEquals("Classification Date", "00000000"),
                                                null)))),
                        "If Survey Period Code is 2, 3 or 5 and If English Language Learners, PK-12 is LY or LF and if there is a matching English Language "
                                + "Learners Information record and if there is a matching Student Course Schedule or Student End of Year Status record, then the English "
                                + "Language Learners: Classification Date must not equal zero. The match between the Student Demographic Information record and the English "
                                + "Language Learners Information record should be based on District Number, Current Instruction/Service; Student Number Identifier, Florida; "
                                + "Survey Period Code and Fiscal/School Year. The match between the Student Demographic Information record and the Student Course Schedule "
                                + "record should be based on District Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and Fiscal Year. "
                                + "The match between the Student Demographic Information and the Student End of Year Status record should be made based on District Number, "
                                + "Current Enrollment; Student Number, Identifier, Florida; Survey Period Code; and Year/School Year.")),
                new FLValidationRule("STD", "5B", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("ELL", "^[LY|LF]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-SSC",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SEYS",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"))))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-ELL",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, I/S", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.notEquals("Basis of Entry", "Z"),
                                                null)))),
                        "If Survey Period Code is 2, 3 or 5 and if English Language Learners, PK-12 is LY or LF and if there is a matching English Language Learners "
                                + "Information record and if there is a matching Student Course Schedule or Student End of Year Status record, then the English Language Learners: "
                                + "Basis of Entry code on the English Language Learners Information record must not = Z. The match between the Student Demographic Information "
                                + "record and the English Language Learners Information record should be based on District Number, Current Instruction/Service; Student Number "
                                + "Identifier, Florida; Survey Period Code and Fiscal/School Year. The match between the Student Demographic Information record and the Student "
                                + "Course Schedule record should be based on District Number, Current Enrollment; Student Number Identifier, Florida; Survey Period Code; and "
                                + "Fiscal Year. The match between Student Demographic Information and Student End of Year Status record should be made based on District Number, "
                                + "Current Enrollment; Student Number, Identifier, Florida; Survey Period Code; and Year/School Year.")),
                new FLValidationRule("STD", "5C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[2|3]$"),
                                Restriction.notEquals("District Number, I/S", "71"),
                                Restriction.or(Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Except Primary", "M"), null)),
                                        Restriction.validateMatchInExport("EXPDATA-FL-EXCEPT",
                                                new KeyValuePair("Student Number", "Student Number"),
                                                new KeyValuePair("District Number, E", "District Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Restriction.equals("Exceptionality Other", "M"),
                                                        null))))
                                .testThen(Restriction.pattern("School Number",
                                        "^(?!3518|7001|7004|7006|7023)"
                                                + "(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$"))),
                        "If Survey Period code is 2 or 3 and if Exceptionality, Primary code = M or Exceptionality, Other code = M on the Exceptional Student record, "
                                + "and District of Instruction is not 71, then the School Number, Zoned School must be in the range 0001-9899 but not 3518, 7001, 7004, 7006, 7023. "
                                + "The match between the Student Demographic Information record and the Exceptional Student record should be based on District Number, "
                                + "Current Enrollment; Student Number Identifier, Florida; Survey Period Code and Fiscal Year. (This edit does not apply to Survey Periods 6 and 8)")),
                new FLValidationRule("STD", "5D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Immigrant Student", "Y"), null)))
                                .testThen(Restriction.byDateFormat("MMddyyyy", "Date Entered US Skl")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.equals("Grade Level", "PK"),
                                Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Immigrant Student", "Y"), null)))
                                .testThen(Restriction.equals("Date Entered US Skl", "00000000"))),
                        "If Survey Period Code is 2, 3, or 5, and Immigrant Student = Y on the Federal/State Indicator Status record, then Date Entered United States "
                                + "School must be numeric and a valid date in the format MMDDYYYY, unless Grade Level = PK then date must be 00000000. The match for Student "
                                + "Demographic and Federal/State Indicator Status records should be based on both District Number, Current Enrollment and District Number, "
                                + "Current Instruction/Service (Student Demographic) and District Number, Current Enrollment (Federal/State Indicator Status); Student Number "
                                + "Identifier, Florida; Survey Period Code and Fiscal Year. (This edit does not apply to Survey Periods 6 and 8.)")),
                // TODO 5E - National School Lunch Program (NSLP) Reference File
                new FLValidationRule("STD", "5F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.equals("Immigrant Student", "Y"), null)))
                                .testThen(Restriction.pattern("Country of Birth", "^((?!US|PR).)*$"))),
                        "If Survey Period Code is 2, 3, or 5, and Immigrant Student = Y on the Federal/State Indicator Status record, then Country of Birth must not "
                                + "be US or PR (per Appendix G or Appendix Q). The match for Student Demographic and Federal/State Indicator Status records should be based on "
                                + "both District Number, Current Enrollment and District Number, Current Instruction/Service (Student Demographic) and District Number, Current "
                                + "Enrollment (Federal/State Indicator Status); Student Number Identifier, Florida; Survey Period Code and Fiscal Year. (This edit does not apply "
                                + "to Survey Periods 6 and 8.)")),
                // TODO Rule 60
                new FLValidationRule("STD", "61", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("Graduation Option", "^[23]$"))
                                .testThen(Restriction.pattern("Grade Level", "^((?!9|10).)*$"))),
                        "For Survey Period Codes 2, 3 and 5, if Graduation Option code is 2 or 3, then Grade Level must not equal 9 or 10. "
                                + "(This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "63", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Grade Level", "^(KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9992|9993|9995)\\S{4}$"))
                                .testThen(Restriction.studentAgeInRange("Student ID Local", Integer.valueOf(5), null,
                                        true, false, new RuntimeParam(RuntimeParam.FISCAL_DATE, "09", "01")))),
                        "If Grade Level is KG-12 and School Number, Current Enrollment is not 9992, 9993, or 9995, the student must be at least 5 years old "
                                + "on or before September 1 of the school year being reported.")),
                new FLValidationRule("STD", "64", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^((?!6|8).)*$"),
                                Restriction.pattern("Grade Level", "^(PK|KG|0[1-9]|1[0-2])$"),
                                Restriction.pattern("School Number", "^(?!9995)\\S{4}$"))
                                .testThen(Restriction.pattern("Resident Status", "^[0|2|3|A|B]$")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^((?!6|8).)*$"),
                                Restriction.pattern("Grade Level", "^(30|31)$"),
                                Restriction.pattern("School Number", "^(?!9995)\\S{4}$"))
                                .testThen(Restriction.pattern("Resident Status", "^[4-7]$"))),
                        "If Grade Level is PK-12 and School Number, Enrollment is not 9995; then Resident Status, State/County must be 0, A, B, 2 or- 3. "
                                + "If Grade Level is 30-31, then Resident Status, State/County must be 4, 5, 6 or 7. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "68", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9994|9995|9997)\\S{4}$"),
                                Restriction.notEquals("District Number, I/S", "71"),
                                Restriction.pattern("Grade Level", "^((?!30|31).)*$"),
                                Restriction.validateMatchInExport("EXPDATA-FL-ENR",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number, E"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.notEquals("Withdrawal Code", "DNE"), null)))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code = 2, 3 or 5; School Number, Current Enrollment is not 3518, 9992, 9993, 9994, 9995 or 9997; District Number, "
                                + "Current Instruction/Service is not 71; District Number, Current Enrollment equals District Number, Current Instruction/Service; "
                                + "Withdrawal Code, PFtus/Student Attendance records is not DNE; and Grade Level is not 30 or 31, "
                                + "then each Student Demographic record must have a matching Federal/State Indicator Status record based on District Number, Current "
                                + "Enrollment; Student Number Identifier, Florida; Survey Period Code and Fiscal Year. (This edit does not apply to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "69", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^[235]$"),
                                Restriction.pattern("School Number", "^(?!3518|9992|9993|9994|9995|9997)\\S{4}$"),
                                Restriction.notEquals("Migrant Status Term", "X"),
                                Restriction.notEquals("District Number, I/S", "71"),
                                Restriction.pattern("Grade Level", "^((?!30|31).)*$"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-FED",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number, E", "District Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If Survey Period Code is 2, 3, or 5; School Number, Current Enrollment is not 3518, 9992, 9993, 9994, 9995, or 9997; Migrant Status Term is not X; "
                                + "District Number Current Instruction/Service is not 71 and Grade Level is not 30 or 31; then each Student Demographic record must have at least "
                                + "one matching Prior School Status/Student Attendance record based on District Number, Current Enrollment; Student Number Identifier, Florida; "
                                + "Survey Period Code, and School Year. (This edit does not apply to Survey Periods 6 and 8.)")),
                // TODO For each School Number, Current Enrollment
                new FLValidationRule("STD", "90", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(3518|9996|9997|N998|N999)$"))
                                .testThen(Restriction.pattern("Lunch Status", "^(0|2|3|A|B)$"))),
                        "For each School Number, Current Enrollment, at least one record must have a Lunch Status code not equal to 0 unless the School Number, Current "
                                + "Enrollment is 3518, 9996, 9997, N998, or N999. (The student did not apply for free or reduced price lunch). (This edit does not apply "
                                + "to Survey Periods 6 and 8.)")),
                new FLValidationRule("STD", "91", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^(?!6|8)\\d$"),
                                Restriction.pattern("School Number", "^(?!9995)\\S{4}$"))
                                .testThen(Restriction.pattern("Lunch Status", "^(0|2|3|A|B)$"))),
                        "For each School Number, Current Enrollment in the District Number, Current Enrollment either all records must have a Lunch Status code of 4 "
                                + "(Provision 2 School) or no records must have a Lunch Status code of 4. (This edit does not apply to Survey Periods 6 and 8.)"))
        });
    }
}
