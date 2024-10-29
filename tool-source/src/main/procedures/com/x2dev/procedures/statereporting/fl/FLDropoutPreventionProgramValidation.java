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
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * The Class FLDropoutPreventionProgramValidation.
 */
public class FLDropoutPreventionProgramValidation {

    DateAsStringConverter m_dateConverter;

    /**
     * Instantiates a new FL dropout prevention program validation.
     */
    public FLDropoutPreventionProgramValidation() {
        m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
    }

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("DRP", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("DRP", "2", new ValidateRegularExpression("School Number",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|999[2-5])$",
                        "The School Number, Current Enrollment must be alphanumeric and in the range 0001-9899, excluding 9001, or it may be 9992, 9993, 9994 or 9995.")),
                new FLValidationRule("DRP", "3", new ValidateRegularExpression("Student Number",
                        "^(?!(00|69|7[067]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-68, 71-75 or 78-79."
                                + "If the tenth position is an X, the first three positions may not all be zeroes.")),
                new FLValidationRule("DRP", "4", new ValidateRegularExpression("Survey Period", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("DRP", "5", new ValidateFiscalYear()),
                new FLValidationRule("DRP", "6", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Dropout/Juvenile Pgm", "^[URAENPDJW]$"))
                                .testThen(Restriction.and(Restriction.greaterThanOrEquals("Enrollment Date",
                                        (Date) m_dateConverter.parseSystemString("1989-07-01"), "MMddyyyy"),
                                        Restriction.lessThanOrEquals("Enrollment Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "If Dropout Prevention/Juvenile Justice Programs code is U, R, A, E, N, P D, J or W, then Dropout Prevention Program Enrollment Date must be numeric, "
                                + "a valid date, greater than or equal to 07/01/1989 and less than 08/31/****.")),
                new FLValidationRule("DRP", "7", new ValidateRegularExpression(
                        "Dropout/Juvenile Pgm", "^[URAENPDJW]$",
                        "Dropout Prevention/Juvenile Justice Programs must be A, D, E, J, N, P, R, U, or W.")),
                new FLValidationRule("DRP", "8", new ValidateRegularExpression("Dropout Part Length", "^\\d{3}$",
                        "Dropout Prevention Length of Prescribed Program must be numeric and greater than or equal to zero.")),
                new FLValidationRule("DRP", "9",
                        new ValidateRegularExpression("Fund Source", "^(D|Z)$", "Fund Source code must be D or Z.")),
                new FLValidationRule("DRP", "10", new ValidateRegularExpression(
                        "Progress Lev Math", "^[CDFGHZ]$", "Progress Level - Math code must be C, D, F, G, H, or Z.")),
                new FLValidationRule("DRP", "12", new ValidateRegularExpression(
                        "Progress Lev Reading", "^[CDFGHZ]$",
                        "Progress Level - Reading code must be C, D, F, G, H or Z.")),
                new FLValidationRule("DRP", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "S"))
                                .testThen(Restriction.pattern("Dropout Part Length", "^0([0-6]\\d|70)$"))),
                        "If Term equal S, then Dropout Prevention Length of Program Participation must be numeric, equal to or greater than zero "
                                + "and equal to or less than 70.")),
                new FLValidationRule("DRP", "16", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Withdrawal Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31"))))),
                        "Dropout Prevention Program Withdrawal Date must be numeric, a valid date, greater than or equal to 07/01/**** and less than or equal to 08/31/****.")),
                new FLValidationRule("DRP", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.greaterThanOrEqualsFieldValue("Withdrawal Date",
                                        "Enrollment Date", Date.class))),
                        "Dropout Prevention Program Withdrawal Date must be greater than or equal to Dropout Prevention Program Enrollment Date.")),
                new FLValidationRule("DRP", "24", new ValidateRegularExpression(
                        "District Number CIS", "^0[1-9]|[1-6][0-8]|7[1-5]$",
                        "District Number, Current Instruction/Service must be numeric and in the range 01-68 or 71-75 and must be correct for the district submitting the data.")),
                new FLValidationRule("DRP", "25", new ValidateRegularExpression("School Number CIS",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9]|999[2-5])$",
                        "The School Number, Current Instruction/Service must be alphanumeric and in the range 0001-9899, excluding 9001, or it may be 9992, 9993, 9994 or 9995.")),
                new FLValidationRule("DRP", "26",
                        new ValidateRegularExpression("Term", "^(3|S)$", "Term must be 3 or S.")),
                new FLValidationRule("DRP", "27", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number",
                                "^(?!999[2-5])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "If School Number, Current Enrollment is not 9992, 9993, 9994 or 9995, then the School Number, Current Enrollment must exist on the Master School "
                                + "Identification File as a valid active school number for the District Number, Current Enrollment.")),
                new FLValidationRule("DRP", "28", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("School Number CIS",
                                "^(?!999[2-5])\\S{4}$"))
                                .testThen(Restriction.byActiveSchool("School Number CIS"))),
                        "If School Number, Current Instruction/Service is not 9992, 9993, 9994 or 9995, then it must exist on the Master School Identification File "
                                + "as a valid active school in the district of instruction.")),
                new FLValidationRule("DRP", "29", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.uniqueValue("Student Number", "District Number CIS",
                                        "School Number CIS", "Survey Period", "Fiscal Year",
                                        "Dropout/Juvenile Pgm", "Term"))),
                        "Each Dropout Prevention Program Data record must be unique based on the keys of Student Number Identifier, Florida; "
                                + "Survey Period Code; School Year; Dropout Prevention/Juvenile Justice Programs; District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; and Term.")),
                new FLValidationRule("DRP", "35",
                        new ValidateRegularExpression("Student ID Local", "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                                "The Student Number Identifier, Local may be any combination of letters, numbers and blanks. (All blanks are allowable.) "
                                        + "It must be left-justified with trailing blanks.")),
                new FLValidationRule("DRP", "37", new ValidateRegularExpression("Pretest Out Math", "^[ABZ]$",
                        "Pretest Outcome - Math code must be A, B or Z.")),
                new FLValidationRule("DRP", "38", new ValidateRegularExpression("Pretest Out Reading", "^[ABZ]$",
                        "Pretest Outcome - Reading code must be A, B or Z.")),
                new FLValidationRule("DRP", "39", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Term", "3"))
                                .testThen(Restriction.pattern("Dropout Part Length", "^180|[0-1][0-7][0-9]$"))),
                        "If Term equal 3, then Dropout Prevention Length of Program Participation must be numeric, equal to or greater than zero and equal to "
                                + "or less than 180.")),
                new FLValidationRule("DRP", "40", new ValidateRegularExpression("Fl Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks, spaces or all zeros for the twelve numeric digits are allowable")),
                new FLValidationRule("DRP", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Dropout Prevention Program Data record must have a matching Student Demographic record based on District Number, "
                                + "Current Instruction/Service; Student Number Identifier, Florida; Survey Period Code; and School Year.")),
                new FLValidationRule("DRP", "51", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout/Juvenile Pgm", "D"))
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-STD",
                                        new KeyValuePair("Student Number", "Student Number"),
                                        new KeyValuePair("District Number", "District Number, I/S"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Restriction.pattern("Grade Level", "^(PK|KG)$"), null)))),
                        "If Dropout Prevention/Juvenile Justice Programs is D then Grade Level may not equal PK or KG.")),
                new FLValidationRule("DRP", "74", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout/Juvenile Pgm", "^[^DJNW]$"))
                                .testThen(Restriction.greaterThan("Dropout Pres Length", Double.valueOf("0")))),
                        "If the Dropout Prevention/Juvenile Justice Programs is not equal to D, J, N, or W, then the Dropout Prevention "
                                + "Length of Prescribed Program must be greater than zero.")),
                new FLValidationRule("DRP", "76", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout/Juvenile Pgm", "^[^AEPRU]$"),
                                Restriction.equals("Term", "S"))
                                .testThen(Restriction.lessThanOrEquals("Dropout Pres Length", Double.valueOf("70")))),
                        "If Dropout Prevention/Juvenile Justice Programs equals A, E, P, R, or U and if Term equals S, then the Dropout "
                                + "Prevention Length of Prescribed Program must be greater than zero and less than or equal to 70.")),
                new FLValidationRule("DRP", "78", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Dropout/Juvenile Pgm", "^[^AEPRU]$"),
                                Restriction.equals("Term", "3"))
                                .testThen(Restriction.and(
                                        Restriction.greaterThan("Dropout Pres Length", Double.valueOf("0")),
                                        Restriction.lessThanOrEquals("Dropout Pres Length", Double.valueOf("180"))))),
                        "If Dropout Prevention/Juvenile Justice Program code equals A, E, P, R, or U, and if Term equals 3, then Dropout "
                                + "Prevention Length of Prescribed Program must be greater than zero and less than or equal to 180.")),
        });
    }
}
