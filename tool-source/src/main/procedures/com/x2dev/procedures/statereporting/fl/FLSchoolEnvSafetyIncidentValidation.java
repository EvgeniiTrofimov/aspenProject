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
import java.util.regex.Pattern;

/**
 * The Class FLCTEStudentCourseScheduleValidation.
 */
public class FLSchoolEnvSafetyIncidentValidation {

    /**
     * Instantiates a new FLCTE student course schedule validation.
     */
    public FLSchoolEnvSafetyIncidentValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SESIR", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("SESIR", "2", new ValidateRegularExpression(
                        "School Number", "^(?!0000)([0-9][0-8][0-9][0-9]|98[0-9][0-9])$",
                        "School Number, Where Incident Occurred must be numeric in the range "
                                + "0001 - 9899.")),
                new FLValidationRule("SESIR", "3", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.byActiveSchool("School Number"))),
                        "School Number, Where Incident Occurred must exist on the Master School "
                                + "Identification File as a valid active school in the District Number, Reporting "
                                + "District.")),
                new FLValidationRule("SESIR", "4", new ValidateRegularExpression(
                        "Survey Period", "^[2|3|5]$",
                        "Survey Period Code must be 2, 3 or 5, and must be correct for the "
                                + "submission specified by the district.")),
                new FLValidationRule("SESIR", "5", new ValidateFiscalYear()),
                new FLValidationRule("SESIR", "6", new ValidateRegularExpression(
                        "Incident, Identifier", "^(?!00000000)\\S+$",
                        "Incident, Identifier must be alphanumeric, may not be zero, and must not "
                                + "contain blanks.")),
                new FLValidationRule("SESIR", "7", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.and(
                                        Restriction.byDateFormat("Incident, Date"),
                                        Restriction.greaterThanOrEquals("Incident, Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "07", "01")),
                                        Restriction.lessThanOrEquals("Incident, Date",
                                                new RuntimeParam(RuntimeParam.FISCAL_DATE, "08", "31")),
                                        Restriction.lessThanOrEquals("Incident, Date",
                                                new RuntimeParam(RuntimeParam.PERIOD_END_DATE))))),
                        "Incident, Date must be numeric, must be a valid date, in the range of "
                                + "07/01/**** to 08/31/**** and not greater than the last day of the survey period.")),
                new FLValidationRule("SESIR", "8", new ValidateRegularExpression(
                        "Incident, Location", "^[1-3]$",
                        "Incident, Location must be 1, 2 or 3.")),
                // TODO: Rule #9
                new FLValidationRule("SESIR", "10", new ValidateRegularExpression(
                        "Involvement Type", "^(S|N|B|U|Z)$",
                        "Incident, Involvement Type code must be S, N, B, U or Z.")),
                new FLValidationRule("SESIR", "11", new ValidateRegularExpression(
                        "Rep Law Enforcement", "^(Y|N)$",
                        "Incident, Reported to Law Enforcement code must be Y or N.")),
                new FLValidationRule("SESIR", "12", new ValidateRegularExpression(
                        "Gang-Related", "^(Y|N|Z)$",
                        "Incident, Gang-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "13", new ValidateRegularExpression(
                        "Alcohol-Related", "^(Y|N|Z)$",
                        "Incident, Alcohol-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "14", new ValidateRegularExpression(
                        "Drug-Related", "^(Y|N|Z)$",
                        "Incident, Drug-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "15", new ValidateRegularExpression(
                        "Hate Crime-Related", "^(Y|N|Z)$",
                        "Incident, Hate Crime-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "16", new ValidateRegularExpression(
                        "Weapon-Related", "^([1-4]|N|Z)$",
                        "Incident, Weapon-Related code must be 1, 2, 3, 4, N or Z.")),
                new FLValidationRule("SESIR", "17", new ValidateRegularExpression(
                        "Weapon, Description", "^(F|H|K|M|O|R|U|Z)$",
                        "Weapon, Description code must be F, H, K, M, O, R, U, or Z.")),
                new FLValidationRule("SESIR", "18", new ValidateRegularExpression(
                        "Incident, Context", "^[1-3]$",
                        "Incident, Context code must be 1, 2 or 3.")),
                new FLValidationRule("SESIR", "19", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[1-2]$"))
                                        .testThen(Restriction.pattern("Weapon, Description", "^(K|O|U)$")),
                                ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[3-4]$"))
                                        .testThen(Restriction.pattern("Weapon, Description", "^(F|H|M|R)$"))),
                        "If Incident, Weapon-Related code is 1 or 2, then Weapon, Description code "
                                + "must be K, O, or U. If Incident, Weapon-Related Code is 3 or 4, then Weapon, "
                                + "Description code must be F, H, M, or R.")),
                new FLValidationRule("SESIR", "1A", new ValidateRegularExpression(
                        "Hazing-Related", "^(Y|N|Z)$",
                        "Incident, Hazing-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "1B", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^HAZ$"))
                                        .testThen(Restriction.pattern("Hazing-Related", "^Y$"))),
                        "If Incident, Type code is HAZ, then Incident, Hazing-Related code must be Y.")),
                new FLValidationRule("SESIR", "1C", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^HOM$"))
                                        .testThen(Restriction.notEquals("Victims of Homicide", "ZZZZ"))),
                        "If Incident, Type code is equal to HOM, then Victims of Homicide code must not be all Zs.")),
                // Skipped: Rule #20 (Transaction Code)
                new FLValidationRule("SESIR", "21", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.alwaysTrue())
                                        .testThen(Restriction.uniqueValue("District Number", "School Number",
                                                "Survey Period", "Fiscal Year", "Incident, Identifier"))),
                        "Each School Environmental Safety Incident Report record must be unique "
                                + "based on District Number, Reporting District; School Number, Where Incident "
                                + "Occurred; Survey Period Code; School Year; and Incident, Identifier.")),
                new FLValidationRule("SESIR", "22", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^WPO$"))
                                        .testThen(Restriction.pattern("Weapon, Description", "^(F|H|K|M|O|R|U)$"))),
                        "If Incident, Type code is WPO, then Weapon, Description code must be F, "
                                + "H, K, M, O, R, or U.")),
                new FLValidationRule("SESIR", "23", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^WPO$"))
                                        .testThen(Restriction.pattern("Weapon-Related", "^[1-4]$"))),
                        "If Incident, Type code is WPO, then Incident, Weapon-Related code must be "
                                + "1, 2, 3 or 4.")),
                new FLValidationRule("SESIR", "24", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^ALC$"))
                                        .testThen(Restriction.pattern("Alcohol-Related", "^Y$"))),
                        "If Incident, Type code is ALC, then Incident, Alcohol-Related code must be Y.")),
                new FLValidationRule("SESIR", "25", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(DRD|DRU)$"))
                                        .testThen(Restriction.pattern("Drug-Related", "^Y$"))),
                        "If Incident, Type code is equal to DRD or DRU, then Incident, Drug-Related code must be Y.")),
                new FLValidationRule("SESIR", "26", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Weapon, Description", "^(K|O|U)$"))
                                        .testThen(Restriction.pattern("Weapon-Related", "^[1-2]$")),
                                ValidationRule.testIf(Restriction.pattern("Weapon, Description", "^(F|H|M|R)$"))
                                        .testThen(Restriction.pattern("Weapon-Related", "^[3-4]$"))),
                        "If Weapon, Description code is K, O, or U, then Incident, Weapon-Related "
                                + "code must be 1 or 2. If Weapon, Description code is F, H, M, or R, then Incident, "
                                + "Weapon-Related code must be 3 or 4.")),
                new FLValidationRule("SESIR", "27", new ValidateRegularExpression(
                        "Drug Description", "^(M|N|O|Z)$",
                        "Drug Description code must be M, N, O or Z.")),
                new FLValidationRule("SESIR", "28", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(BAT|SXB)$"))
                                        .testThen(Restriction.pattern("Injury-Related", "^(A|B)$"))),
                        "If Incident, Type is BAT or SXB, then Incident, Injury-Related must equal A or B.")),
                new FLValidationRule("SESIR", "29", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Drug Description", "^(M|N|O)$"))
                                        .testThen(Restriction.pattern("Drug-Related", "^Y$"))),
                        "If Drug Description code is M, N or O, then Incident, Drug-Related code must be Y.")),
                new FLValidationRule("SESIR", "30", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(DRD|DRU)$"))
                                        .testThen(Restriction.pattern("Drug Description", "^(M|N|O)$"))),
                        "If Incident, Type code is DRD or DRU, then Drug Description code must be M, N or O.")),
                new FLValidationRule("SESIR", "31", new ValidateRegularExpression(
                        "Bullying-Related", "^(Y|N|Z)$",
                        "Incident, Bullying-Related code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "32", new ValidateRegularExpression(
                        "Victims of Homicide", "^[E|F|O|S|Z]{4}$",
                        "Victims of Homicide code must be E, F, O, S, or Z.")),
                new FLValidationRule("SESIR", "33", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Drug-Related", "^Y$"))
                                        .testThen(Restriction.pattern("Drug Description", "^(M|N|O)$"))),
                        "If Incident, Drug-Related code is Y, then Drug Description code must be M, N or O.")),
                new FLValidationRule("SESIR", "34", new ValidateRegularExpression(
                        "Weapon, Discharged", "^(Y|N|Z)$",
                        "Weapon, Discharged code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "35", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Weapon, Description", "^(F|H|M|R)$"))
                                        .testThen(Restriction.pattern("Weapon, Discharged", "^(Y|N)$"))),
                        "If Weapon, Description code is F, H, M, or R, then Weapon, Discharged "
                                + "code must be Y or N.")),
                new FLValidationRule("SESIR", "36", new ValidateRegularExpression(
                        "Injury-Related", "^(A|B|Z)$",
                        "Incident, Injury-Related code must be A, B or Z.")),
                new FLValidationRule("SESIR", "37", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^HOM$"))
                                        .testThen(Restriction.pattern("Injury-Related", "^A$"))),
                        "If Incident, Type code is equal to HOM, then Incident, Injury-Related code "
                                + "must be A.")),
                new FLValidationRule("SESIR", "38", new ValidateRegularExpression(
                        "Basis-Religion", "^(Y|N|Z)$",
                        "Incident, Basis - Religion code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "39", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^BUL$"))
                                        .testThen(Restriction.pattern("Bullying-Related", "^Y$"))),
                        "If Incident, Type code is equal to BUL, then Incident, Bullying-Related code "
                                + "must be Y.")),
                new FLValidationRule("SESIR", "40", new ValidateRegularExpression(
                        "Basis-Sex Orientatio", "^(Y|N|Z)$",
                        "Incident, Basis - Sexual Orientation code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "41", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.pattern("Incident, Type", "^(BUL|HAR|SXH|TRE|UBL|UHR)$"),
                                        Restriction.pattern("Bullying-Related", "^Y$")))
                                        .testThen(Restriction.pattern("Basis-Religion", "^(Y|N)$"))),
                        "If Incident, Type is BUL, HAR, SXH, TRE, UBL, UHR or if Incident, BullyingRelated "
                                + "is Y, then Incident, Basis - Religion must be Y or N.")),
                new FLValidationRule("SESIR", "42", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Weapon-Related", "^[1-4]$"))
                                        .testThen(Restriction.pattern("Rep Law Enforcement", "^Y$"))),
                        "If Incident, Weapon-Related is 1, 2, 3 or 4, then Incident, Reported to Law "
                                + "Enforcement must equal Y.")),
                new FLValidationRule("SESIR", "43", new ValidateRegularExpression(
                        "Basis-Disability", "^(Y|N|Z)$",
                        "Incident, Basis - Disability code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "44", new ValidateRegularExpression(
                        "Basis-Race", "^(Y|N|Z)$",
                        "Incident, Basis - Race code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "45", new ValidateRegularExpression(
                        "Basis-Sex Orientatio", "^(Y|N|Z)$",
                        "Incident, Basis - Sex code must be Y, N or Z.")),
                new FLValidationRule("SESIR", "46", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.pattern("Incident, Type", "^(BUL|HAR|SXH|TRE|UBL|UHR)$"),
                                        Restriction.pattern("Bullying-Related", "^Y$")))
                                        .testThen(Restriction.pattern("Basis-Disability", "^(Y|N)$"))),
                        "If Incident, Type is BUL, HAR, SXH, TRE, UBL, UHR or if Incident, BullyingRelated "
                                + "is Y, then Incident, Basis - Disability must be Y or N.")),
                new FLValidationRule("SESIR", "47", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.pattern("Incident, Type", "^(BUL|HAR|SXH|TRE|UBL|UHR)$"),
                                        Restriction.pattern("Bullying-Related", "^Y$")))
                                        .testThen(Restriction.pattern("Basis-Race", "^(Y|N)$"))),
                        "If Incident, Type is BUL, HAR, SXH, TRE, UBL, UHR or if Incident, BullyingRelated "
                                + "is Y, then Incident, Basis - Race must be Y or N.")),
                new FLValidationRule("SESIR", "48", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.pattern("Incident, Type", "^(BUL|TRE|UBL|UHR)$"),
                                        Restriction.pattern("Bullying-Related", "^Y$")))
                                        .testThen(Restriction.pattern("Basis-Sex Orientatio", "^(Y|N)$")),
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^HAR$"))
                                        .testThen(Restriction.pattern("Basis-Sex Orientatio", "^N$")),
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^SXH$"))
                                        .testThen(Restriction.pattern("Basis-Sex Orientatio", "^Y$"))),
                        "If Incident, Type is BUL, TRE, UBL, UHR or Incident, Bullying-Related is Y,"
                                + "then Incident, Basis - Sex must be Y or N. If Incident, Type is HAR, then Incident, "
                                + "Basis - Sex must be N. If Incident, Type is SXH, then Incident, Basis - Sex must "
                                + "be Y.")),
                new FLValidationRule("SESIR", "49", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Alcohol-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Alcohol-Related code must be Z.")),
                new FLValidationRule("SESIR", "50", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Bullying-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Bullying-Related code must be Z.")),
                new FLValidationRule("SESIR", "51", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Drug-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Drug-Related code must be Z.")),
                new FLValidationRule("SESIR", "52", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Gang-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Gang-Related code must be Z.")),
                new FLValidationRule("SESIR", "53", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Hate Crime-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Hate Crime-Related code must be Z.")),
                new FLValidationRule("SESIR", "54", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Injury-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Injury-Related code must be Z.")),
                new FLValidationRule("SESIR", "55", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Involvement Type", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Involvement Type code must be Z.")),
                new FLValidationRule("SESIR", "56", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(UBL|UHR)$"))
                                        .testThen(Restriction.pattern("Weapon-Related", "^Z$"))),
                        "If Incident, Type is UBL or UHR then Incident, Weapon-Related code must be Z.")),
                new FLValidationRule("SESIR", "57", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(BAT|HOM|KID|SXB|WPO)$"))
                                        .testThen(Restriction.pattern("Rep Law Enforcement", "^Y$"))),
                        "If Incident, Type is BAT, HOM, KID, SXB, or WPO then Incident, Reported to"
                                + "Law Enforcement must equal Y.")),
                new FLValidationRule("SESIR", "58", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.or(
                                        Restriction.pattern("Incident, Type", "^(BUL|HAR|SXH|TRE|UBL|UHR)$"),
                                        Restriction.pattern("Bullying-Related", "^Y$")))
                                        .testThen(Restriction.pattern("Basis-Sex Orientatio", "^(Y|N)$"))),
                        "If Incident, Type is BUL, HAR, SXH, TRE, UBL, UHR or Incident, BullyingRelated "
                                + "is Y, then Incident, Basis - Sexual Orientation must be Y or N.")),
                // TODO: Rule #59
                new FLValidationRule("SESIR", "60", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Involvement Type", "^(S|B)$"))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                                new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                                new KeyValuePair("Incident, Date", "Incident Date"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "For each School Environmental Safety Incident Report (SESIR) record with "
                                + "an Incident, Involvement Type of S or B there must be at least one matching "
                                + "Student Discipline/Resultant Action (SDRA) record with a matching Incident "
                                + "Identifier and Incident Date. The match should be done using District Number, "
                                + "Reporting District on the SESIR format matched to District Number, Current "
                                + "Enrollment on the SDRA format and the following elements: School Number, "
                                + "Where Incident Occurred; Survey Period Code and School Year.")),
                new FLValidationRule("SESIR", "61", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Pattern.compile("^Y$"), "Use of Drugs")))
                                        .testThen(Restriction.pattern("Drug Description", "^(M|N|O)$"))),
                        "If the Student, Use of Drugs code on the matching Student Discipline "
                                + "/Resultant Action (SDRA) format is Y, then the Drug Description code must be M, "
                                + "N or O. The records match is done using the District Number, Current Enrollment "
                                + "on the SDRA format matched to the District Number, Reporting District on the "
                                + "SESIR format; School Number, Where Incident Occurred on the SESIR format "
                                + "matched to School Number, Where Discipline/Resultant Action Occurred on the "
                                + "SDRA format along with matching School Year, Survey Period Code and Incident, "
                                + "Identifier.")),
                new FLValidationRule("SESIR", "62", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                                new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^((?!C).)*$"), "Action Code")))),
                        "If there is a matching Disciplinary/Resultant Action record, then the "
                                + "Disciplinary/Resultant Action Code on the matching record must not equal C. "
                                + "The match should be based on District Number, Current Enrollment on the "
                                + "Student Discipline/Resultant Action (SDRA) format matched to the District "
                                + "Number, Reporting District on the School Environmental Safety Incident Report "
                                + "(SESIR) format; School Number, Where Incident Occurred on the SESIR format "
                                + "matched to School Number, Where Discipline/Resultant Action Occurred on the "
                                + "SDRA format along with matching School Year, Survey Period Code and Incident, "
                                + "Identifier.")),
                new FLValidationRule("SESIR", "63", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                        new KeyValuePair("Incident, Date", "Incident Date"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Pattern.compile("^N$"), "Weapon Use")))
                                        .testThen(Restriction.pattern("Weapon-Related", "^([1-4]|N|Z)$")),
                                ValidationRule.testIf(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                        new KeyValuePair("Incident, Date", "Incident Date"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                        new KeyValuePair(Pattern.compile("^Y$"), "Weapon Use")))
                                        .testThen(Restriction.pattern("Weapon-Related", "^[1-4]$"))),
                        "If Student, Weapon Use on the Student Discipline/Resultant Action (SDRA) "
                                + "format is N, then Incident, Weapon-Related on the School Environmental Safety "
                                + "Incident Report (SESIR) may be 1, 2, 3, 4, N or Z. If Student, Weapon Use on the "
                                + "SDRA is Y, then Incident, Weapon-Related code on the SESIR record must equal "
                                + "1, 2, 3 or 4. "
                                + "The records should match on District Number, Current Enrollment on the Student "
                                + "Discipline/Resultant Action record and District Number, Reporting District on the "
                                + "School Environmental Safety Incident Report record; School Number, Where "
                                + "Incident Occurred on the SESIR format matched to School Number, Where "
                                + "Discipline/Resultant Action Occurred on the SDRA format; Survey Period Code; "
                                + "School Year; Incident Identifier; and Incident Date.")),
                new FLValidationRule("SESIR", "64", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Involvement Type", "^(N|U)$"))
                                        .testThen(Restriction.validateNotMatchInExport("EXPDATA-FL-SDRA",
                                                new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "If the School Environmental Safety Incident Report (SESIR) record has an "
                                + "Incident, Involvement Type of N or U, then there must not be a matching Student "
                                + "Discipline/Resultant Action (SDRA) record. The match should be done using "
                                + "District Number, Reporting District on the SESIR format matched to District "
                                + "Number, Current Enrollment on the SDRA format; School Number, Where Incident "
                                + "Occurred on the SESIR format matched to School Number, Where "
                                + "Discipline/Resultant Action Occurred on the SDRA format; Survey Period Code; "
                                + "School Year and Incident Identifier.")),
                // TODO: Rule #80
                new FLValidationRule("SESIR", "90", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.equals("Incident, Type", "WPO"),
                                        Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                        new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                        new KeyValuePair("District Number", "District Number"),
                                        new KeyValuePair("School Number", "School Number"),
                                        new KeyValuePair("Survey Period", "Survey Period"),
                                        new KeyValuePair("Fiscal Year", "Fiscal Year")))
                                        .testThen(Restriction.validateMatchInExport("EXPDATA-FL-SDRA",
                                                new KeyValuePair("Incident, Identifier", "Incident Identifier"),
                                                new KeyValuePair("Incident, Date", "Incident Date"),
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("School Number", "School Number"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                new KeyValuePair(Pattern.compile("^((?!C).)*$"), "Action Code")))),
                        "If Incident, Type = WPO and if a matching Student Discipline/Resultant "
                                + "Action record exists, then one of the Disciplinary/Resultant Action codes on the "
                                + "Student Discipline/Resultant Action (SDRA) format must be E, F, P, H or U. The "
                                + "match should be done using District Number, Reporting District on the School "
                                + "Environmental Safety Incident Report (SESIR) format matched to District "
                                + "Number, Current Enrollment on the SDRA format; School Number, Where "
                                + "Incident Occurred on the SESIR matched to School Number, Where "
                                + "Discipline/Resultant Action Occurred on the SDRA format; along with School "
                                + "Year; Survey Period; Incident, Identifier and Incident, Date.")),
                // TODO: Rule #91
                new FLValidationRule("SESIR", "92", new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule.testIf(Restriction.pattern("Incident, Type", "^(BRK|DOC|DRD|DRU|ROB)$"))
                                        .testThen(Restriction.pattern("Rep Law Enforcement", "^Y$"))),
                        "If Incident, Type is BRK, DOC, DRD, DRU, or ROB, then Incident, Reported "
                                + "to Law Enforcement must equal Y.")),
        });
    }
}
