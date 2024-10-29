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

import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.Person;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationError;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateDistrictNumber;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateFiscalYear;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateSSN;
import com.x2dev.procedures.statereporting.fl.FLStaffExtractValidation.ValidateFlEducationId;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * Class configures validation rules for SXP export.
 * Specification: http://www.fldoe.org/core/fileparse.php/15228/urlt/1617se.pdf
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStaffExperienceValidation {

    private static final String FIELD_NAME_XP_LENGTH = "Experience Length";

    /**
     * The Class ValidateAge.
     */
    private class ValidateAge implements FLValidationProcessor {

        private int totalLength = 0;
        private FLExportConfiguration currentHelper = null;
        private String currentStaffSSN = null;

        /**
         * Gets the validation errors.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return List
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            // validation instance is loaded statically so old instance is mutated by each staff and
            // run, so we need determine if it's new staff/row to set totalLength to zero
            if (currentHelper == null || !currentHelper.equals(helper)) {
                currentHelper = helper;
                totalLength = 0;
            }
            String staffSSN = helper.getExportFormatRowFieldValue(row, export, "SSN");
            if (currentStaffSSN == null || !currentStaffSSN.equals(staffSSN)) {
                currentStaffSSN = staffSSN;
                totalLength = 0;
            }

            List<FLValidationError> errors = new LinkedList();
            boolean ok = false;
            String value = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME_XP_LENGTH);
            if (!StringUtils.isEmpty(value)) {
                try {
                    int length = Integer.parseInt(value);
                    String strBirthDate = FLStaffExtractValidation.getExportValue(helper, export, row, "EXPDATA-FL-STF",
                            "Birth Date",
                            new KeyValuePair("Staff ID Local", "Staff ID Local"));
                    if (!StringUtils.isEmpty(strBirthDate)) {
                        PlainDate birthDate =
                                new PlainDate(FLStaffExtractValidation.DEFAULT_DATE_FORMAT.parse(strBirthDate));
                        int age = Person.getAge(birthDate, null);
                        if (length <= age - 20) {
                            ok = true;
                            String xpType = helper.getExportFormatRowFieldValue(row, export, "Experience Type");
                            if ("F".equals(xpType) || "S".equals(xpType) || "P".equals(xpType) || "N".equals(xpType)) {
                                totalLength += length;
                                if (totalLength + 20 > age) {
                                    errors.add(
                                            new FLValidationError(export, row, "Incorrect value", FIELD_NAME_XP_LENGTH,
                                                    value,
                                                    "The sum of the values for Experience Types F, S, P and N must not be"
                                                            + " greater than the number computed when subtracting 20 from the calculated age"
                                                            + " (using Birth Date from the Staff Demographic Information record)."));
                                }
                            }
                        }
                    }
                } catch (NumberFormatException | ParseException ex) {
                    System.out.println(ex);
                }
            }
            if (!ok) {
                errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME_XP_LENGTH,
                        value,
                        FIELD_NAME_XP_LENGTH + " must not be greater than the number computed when"
                                + " subtracting 20 from the calculated age (using Birth Date from the Staff Demographic Information record)."));
            }
            return errors;
        }

    }

    /**
     * The Class ValidateExperienceTypes.
     */
    private static class ValidateExperienceTypes implements FLValidationProcessor {

        /**
         * Gets the validation errors.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return List
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            List<FLValidationError> errors = new LinkedList();
            String valueF = helper.getExportFormatRowFieldValue(row, export, "Experience Type");
            if ("F".equals(valueF)) {
                String lengthF = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME_XP_LENGTH);
                if (!StringUtils.isEmpty(lengthF)) {
                    int lf = Integer.parseInt(lengthF);
                    Collection<ExportFormatRow> exportRows =
                            FLStaffExtractValidation.getExportValues(helper, export, row, "EXPDATA-FL-SXP",
                                    new KeyValuePair("Staff ID Local", "Staff ID Local"));
                    if (exportRows != null) {
                        for (ExportFormatRow erow : exportRows) {
                            if ("D".equals(helper.getExportFormatRowFieldValue(erow, export, "Experience Type"))) {
                                String lengthD = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME_XP_LENGTH);
                                if (!StringUtils.isEmpty(lengthD)) {
                                    int ld = Integer.parseInt(lengthD);
                                    if (ld > lf) {
                                        errors.add(new FLValidationError(export, row,
                                                "Incorrect value", FIELD_NAME_XP_LENGTH,
                                                "Current values are: value(D)= " + lengthD + ", value(F)=" + lengthF,
                                                "The Experience Length for Experience Type code of F must be greater than"
                                                        + " or equal to the Experience Length for Experience Type code of D"));
                                    }
                                }
                            }
                        }
                    }
                }

            }
            return errors;
        }
    }

    /**
     * Gets the validation rules.
     *
     * @return List
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("SXP", "1", new ValidateDistrictNumber("District Number")),

                new FLValidationRule("SXP", "2", new ValidateSSN()),

                new FLValidationRule("SXP", "3", new ValidateRegularExpression("Survey Period",
                        "^[2|3]$",
                        "Survey Period Code must be correct for the submission specified by the district and must be 2 or 3")),

                new FLValidationRule("SXP", "4", new ValidateFiscalYear()),

                new FLValidationRule("SXP", "5", new ValidateRegularExpression("Experience Type",
                        "^A|C||D|F|M|N|P|S$",
                        "Experience Type code must be A, C, D, F, M, N, P, or S. ")),

                new FLValidationRule("SXP", "6", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern(FIELD_NAME_XP_LENGTH, "^\\d+$"))
                                .testThen(Restriction.lessThanOrEquals(FIELD_NAME_XP_LENGTH, Double.valueOf(75)))
                                .testThen(Restriction.greaterThanOrEquals(FIELD_NAME_XP_LENGTH, Double.valueOf(0)))),
                        "Experience Length must be numeric and be greater than or equal to zero and less than or equal to 75.")),

                new FLValidationRule("SXP", "7", new ValidateRegularExpression("Transaction Code", "^A|C|D$",
                        "The Transaction Code must be A, C or D.")),

                new FLValidationRule("SXP", "8", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.uniqueValue("District Number", "SSN", "Survey Period",
                                        "Fiscal Year", "Experience Type"))),
                        "Each Staff Experience record must be unique based on District Number, Social Security Number, Survey Period Code, Fiscal Year, and Experience Type code.")),

                new FLValidationRule("SXP", "9", new ValidateRegularExpression("Staff ID Local",
                        "[^-\\s].+$",
                        "The Staff Number Identifier, Local may be any combination of letters, numbers and blanks. All blanks are not allowable. It must be left-justified with trailing blanks.")),

                new FLValidationRule("SXP", "10", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.notEqualsFieldValue("Staff ID Local", "SSN", String.class))),
                        "Experience Length must be numeric and be greater than or equal to zero and less than or equal to 75.")),

                new FLValidationRule("SXP", "11", new ValidateFlEducationId()),

                new FLValidationRule("SXP", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(
                                        Restriction.validateMatchInExport("EXPDATA-FL-STF",
                                                new KeyValuePair("District Number", "District Number"),
                                                new KeyValuePair("SSN", "SSN"),
                                                new KeyValuePair("Survey Period", "Survey Period"),
                                                new KeyValuePair("Fiscal Year", "Fiscal Year")))),
                        "Each Staff Experience record must have a matching Staff Demographic Information record based on District Number, Social Security Number, Survey Period Code and Fiscal Year.")),

                new FLValidationRule("SXP", "21", new ValidateExperienceTypes()),
                new FLValidationRule("SXP", "22_23", new ValidateAge()),

                new FLValidationRule("SXP", "50", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.lessThanOrEquals(FIELD_NAME_XP_LENGTH, Double.valueOf(40)))),
                        "Experience Length must not be greater than 40.")),

        });
    }
}
