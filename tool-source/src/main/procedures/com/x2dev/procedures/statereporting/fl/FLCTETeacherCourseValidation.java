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
import com.x2dev.utils.converters.DateAsStringConverter;
import java.util.Arrays;
import java.util.List;

/**
 * The Class FLDropoutPreventionProgramValidation.
 */
public class FLCTETeacherCourseValidation {

    DateAsStringConverter m_dateConverter;

    /**
     * Instantiates a new FL dropout prevention program validation.
     */
    public FLCTETeacherCourseValidation() {

    }

    /**
     * Gets the validation rules.
     *
     * @return List
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("CTETC", "1", new ValidateDistrictNumber("District Number CIS")),
                new FLValidationRule("CTETC", "2", new ValidateRegularExpression("School Number CIS",
                        "^(?!9001$)(000[1-9]|00[1-9][0-9]|0[1-9][0-9][0-9]|[1-8][0-9][0-9][0-9]|9[0-8][0-9][0-9])$",
                        "School Number, Current Instruction/Service must be numeric in the range 0001 to 9899, excluding 9001.")),
                new FLValidationRule("CTETC", "3", new ValidateRegularExpression("Survey Period Code", "^5$",
                        "Survey Period Code must be 5 and must be correct for the submission specified by the district.")),
                new FLValidationRule("CTETC", "4", new ValidateFiscalYear()),
                // TODO: Rule #5 Career and Technical/Adult General Education Program Edit file
                new FLValidationRule("CTETC", "6", new ValidateRegularExpression("Section Number",
                        ".*[^ ].*",
                        "Section Number must not be all blanks. Allowable characters are 0-9, A-Z, space, hyphen, dollar sign, pound sign and colon.")),
                new FLValidationRule("CTETC", "7", new ValidateRegularExpression("Period Number",
                        "^(?!9999$)\\d{4}$",
                        "Period Number must be numeric, greater than or equal to zero, and may not be 9999.")),
                new FLValidationRule("CTETC", "8", new ValidateRegularExpression("Term", "^([1-9]|[B-O]|[S-Y])$",
                        "Term must be either 1-9, B-O, or S-Y.")),
                new FLValidationRule("CTETC", "9", new ValidateRegularExpression("Facility Type", "^([01][0-9]|20)$",
                        "Facility Type code must be in the range 00 to 20.")),
                new FLValidationRule("CTETC", "13", new ValidateRegularExpression("FL Educators Cert Id", "^\\d{10}$",
                        "Florida Educators Certificate Number must be numeric with no embedded blanks.")),
                new FLValidationRule("CTETC", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.uniqueValue("District Number CIS",
                                        "School Number CIS", "Fiscal Year", "Course Number",
                                        "Section Number", "Period Number", "SSN", "Term"))),
                        "Each record must be unique based on the following key: District Number, Current Instruction/Service; "
                                + "School Number, Current Instruction/Service; School Year; Course Number; Section Number; Period Number; "
                                + "Social Security Number and Term. -first record accepted, all other duplicate records rejected")),
                new FLValidationRule("CTETC", "16", new ValidateSSN()),
                new FLValidationRule("CTETC", "17", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "W170205"))
                                .testThen(Restriction.pattern("District Number CIS", "^(35|41|50|55|58|62)$"))),
                        "If Course Number = W170205 (EMT Basic), District Number, Current Instruction/Service must be "
                                + "one of the following: 35 41 50 55 58 62")),
                new FLValidationRule("CTETC", "18", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Course Number", "W170206"))
                                .testThen(Restriction.pattern("District Number CIS", "^(35|41|55|58)$"))),
                        "If Course Number = W170206 (Paramedic), District Number, Current Instruction/Service must be one"
                                + " of the following: 35 41 5 58")),
                new FLValidationRule("CTETC", "19",
                        new ValidateRegularExpression("Course Number", "^(?!M810015|M810016)\\S{7}$",
                                "Course Number cannot be = M810015 (Insurance Claims Adjustor) or M810016 (Insurance Customer Service Representative).")),
                new FLValidationRule("CTETC", "20", new ValidateRegularExpression("Staff ID local",
                        "^(?!\\s)[\\w\\d\\s]{1,}$|^\\s{1,}$",
                        "The Staff Number Identifier, Local may be any combination of letters, numbers and blanks. All blanks are not allowable. "
                                + "It must be left-justified with trailing blanks.")),
                // TODO If not a Virtual Charter School (note: Virtual Charter Schools are
                // identified on MSID by Charter School Status not Z
                // and School Function Setting equal to V
                new FLValidationRule("CTETC", "22", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("District Number CIS", "^0[1-9]|[1-6][0-9]|7[1-5]$"),
                                Restriction.pattern("School Number CIS", "^(?!7001|7004|7006|7023)\\S{4}$"),
                                Restriction.and(
                                        Restriction.equals("District Number CIS", "50"),
                                        Restriction.pattern("School Number CIS", "^(?!7079)\\S{4}$")))
                                .testThen(Restriction.notEquals("Facility Type", "20"))),
                        "If District Number, Current Instruction/Service is in the range 01 - 69 or 72 - 75 and"
                                + " if School Number, Current Instruction/Service is not 7001, or 7004, 7006, or 7023 and"
                                + " if School Number, Current Instruction/Service is not 7079 in District Number, Current Instruction/Service 50, and"
                                + " If not a Virtual Charter School (note: Virtual Charter Schools are identified on MSID by Charter School Status not Z "
                                + "and School Function Setting equal to V)"
                                + "then Facility Type must not equal 20.")),
                // TODO School Number, Current Instruction/Service is one for which
                // the School Function Setting = V and Charter School Status does not equal Z on the
                // Master School Identification file
                new FLValidationRule("CTETC", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("District Number CIS", "71"))
                                .testThen(Restriction.notEquals("Facility Type", "20"))),
                        "If District Number, Current Instruction/Service is 71, or if the School Number, Current Instruction/Service is one for which "
                                + "the School Function Setting = V and Charter School Status does not equal Z on the Master School Identification file, "
                                + "then Facility type must equal 20.")),
                new FLValidationRule("CTETC", "40", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byActiveSchool("School Number CIS"))),
                        "School Number, Current Instruction/Service must exist and be active on the Master School Identification File for the "
                                + "district reported in District Number, Current Instruction/Service.")),
                new FLValidationRule("CTETC", "42", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.pattern("Period Number", "^([0-7]\\d|80)([0-7]\\d|80|88)$"),
                                        Restriction.validatePeriodNumber("Period Number")))),
                        "The first two digits of Period Number must be 00 to 80. The last two digits of Period Number must be 00 to 80 or 88 "
                                + "and must be greater than or equal to the first two digits.")),                
                new FLValidationRule("CTETC", "43", new ValidateRegularExpression("Florida Education Id",
                        "^(?!FL0{12}$)FL\\d{12}$",
                        "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL in the first 2 positions followed by "
                                + "twelve numeric digits. No blanks or spaces are allowable.")),
                new FLValidationRule("CTETC", "60", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.validateMatchInExport("EXPDATA-FL-CTESSC",
                                        new KeyValuePair("Course Number", "Course Number"),
                                        new KeyValuePair("Section Number", "Section Number"),
                                        new KeyValuePair("Period Number", "Period Number"),
                                        new KeyValuePair("Term", "Term"),
                                        new KeyValuePair("School Number CIS", "School Number"),
                                        new KeyValuePair("District Number CIS", "District Number"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Career and Technical Teacher Course record must have a matching Career and Technical Student Course record based on "
                                + "District Number, Current Instruction/Service; School Number, Current Instruction/Service; School Year; Course Number; "
                                + "Section Number; Period Number and Term.")),
                new FLValidationRule("CTETC", "61", new ValidateRegularExpression("FL Educators Cert Id",
                        "^9999999999|000\\d{7}$",
                        "Florida Educators Certificate Number must be 0000000000 or in the range 0000000001 through 0000999998, 0001000000 through 0009999999, "
                                + "0000999999 or 9999999999.")),
        });
    }
}
