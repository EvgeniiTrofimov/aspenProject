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
import com.ibm.icu.util.Calendar;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.*;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class FLStaffValidation.
 *
 * Specification:
 * http://www.fldoe.org/core/fileparse.php/15228/urlt/1617di.pdf
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStaffExtractValidation {

    public static final String DEFAULT_DATE_FORMAT_MASK = "MMddyyyy";
    public static final SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(DEFAULT_DATE_FORMAT_MASK);

    private static final PlainDate ZERO_DATE = new PlainDate(new Date(0L));

    /**
     * The Class ValidateAbsentPresentDays.
     */
    public static class ValidateAbsentPresentDays implements FLValidationProcessor {
        private String[] m_fields;
        private String m_msg;

        /**
         * Instantiates a new validate absent present days.
         *
         * @param msg String
         * @param fields String[]
         */
        public ValidateAbsentPresentDays(String msg, String... fields) {
            m_msg = msg;
            m_fields = fields;
        }

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
            if (!helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_8) &&
                    !"71".equals(helper.getExportFormatRowFieldValue(row, export, "District Number"))) {
                if (m_fields != null && m_fields.length > 0) {
                    int daysTotal = 0;
                    for (String field : m_fields) {
                        String value = helper.getExportFormatRowFieldValue(row, export, field);
                        if (value != null) {
                            boolean correct = (helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_2) ||
                                    helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_3))
                                    && "000".equals(value);
                            if (!correct && helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_5)) {
                                try {
                                    int days = Integer.parseInt(value);
                                    correct = days == 999 || (days >= 0 && days <= 180);
                                    if (correct) {
                                        daysTotal += days;
                                    }
                                } catch (Exception ex) {
                                    System.out.println(ex);
                                }
                            }
                            if (!correct) {
                                errors.add(new FLValidationError(export, row, "Incorrect value", field,
                                        value, m_msg));
                            } else if (daysTotal > 180 && daysTotal / 999 != m_fields.length) {
                                errors.add(new FLValidationError(export, row, "Incorrect value", field,
                                        value,
                                        "The number of Days Present; Days Absent, Personal Leave; Days Absent, SickLeave; Days Absent, Temporary Duty Elsewhere; and Days Absent, Other added together must be in the range zero through 180 or all of these must be 999"));
                            }

                        } else {
                            errors.add(new FLValidationError(export, row, "Field not found", field, "",
                                    m_msg));
                        }
                    }
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidateAge.
     */
    public static class ValidateAge implements FLValidationProcessor {
        private static final String FIELD_NAME = "Birth Date";

        /**
         * Instantiates a new validate age.
         */
        public ValidateAge() {}

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
            if (!helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_8)) {
                PlainDate date = null;
                String value = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME);
                if (value != null) {
                    try {
                        date = new PlainDate(DEFAULT_DATE_FORMAT.parse(value));
                        int age = Person.getAge(date, null);
                        if (age < 16 || age > 75) {
                            date = null;
                        }
                    } catch (ParseException ex) {
                        errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME,
                                value, "Birth Date must be numeric and a valid date."));
                    }
                }
                if (date == null) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME,
                            value,
                            "Birth Date must be between the age range of 16 and 75 years old, inclusive"));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidateEmploymentDate.
     */
    public static class ValidateEmploymentDate implements FLValidationProcessor {

        private static final String MSG =
                "Employment Date must be numeric and a valid date which is " +
                        "prior to the current date unless Separation Date is prior to the Fiscal Year being " +
                        "reported, in which case, Employment Date may be all zeros";

        private boolean m_checkSeparation = true;
        private String m_fieldName;

        /**
         * Instantiates a new validate employment date.
         *
         * @param fieldName String
         */
        public ValidateEmploymentDate(String fieldName) {
            m_fieldName = fieldName;
        }

        /**
         * Instantiates a new validate employment date.
         *
         * @param fieldName String
         * @param checkSeparation boolean
         */
        public ValidateEmploymentDate(String fieldName, boolean checkSeparation) {
            m_fieldName = fieldName;
            m_checkSeparation = checkSeparation;
        }

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

            PlainDate current = FLStaffExtractValidation.parseDate(errors, m_fieldName, helper, export, row);
            if (current != null) {
                PlainDate separation =
                        FLStaffExtractValidation.parseDate(errors, "Separation Date", helper, export, row);
                if (m_checkSeparation && separation != null && separation.before(helper.getContext().getStartDate())) {
                    // may be zeros, not must be (rule 12, 13)
                    // if (!ZERO_DATE.equals(current)) {
                    // errors.add(
                    // new FLValidationError(export, row,
                    // "Incorrect value", m_fieldName, "Current value: "
                    // + helper.getExportFormatRowFieldValue(row, export, m_fieldName),
                    // MSG));
                    // }
                } else if (ZERO_DATE.equals(current) || current.after(new Date())) {
                    errors.add(
                            new FLValidationError(export, row,
                                    "Incorrect value", m_fieldName,
                                    helper.getExportFormatRowFieldValue(row, export, m_fieldName),
                                    MSG));
                } else if (separation != null && !ZERO_DATE.equals(separation) && separation.before(current)) { // rule
                                                                                                                // 51
                    errors.add(
                            new FLValidationError(export, row,
                                    "Incorrect value", m_fieldName,
                                    helper.getExportFormatRowFieldValue(row, export, m_fieldName),
                                    "If Separation Date is not zero then it must be greater than or equal to " +
                                            "Employment Date, Current Position; Employment Date, Continuous " +
                                            "Employment; and Employment Date, Original Position."));
                }
                if (separation != null && (ZERO_DATE.equals(separation)
                        || (separation.after(helper.getContext().getStartDate()) &&
                                separation.before(helper.getContext().getEndDate())))) {
                    if ("Employment Date CP".equals(m_fieldName)) {
                        PlainDate ce =
                                FLStaffExtractValidation.parseDate(errors, "Employment Date CE", helper, export, row);
                        PlainDate op =
                                FLStaffExtractValidation.parseDate(errors, "Employment Date OP", helper, export, row);
                        if (op != null && ce != null && (current.before(ce) || current.before(op))) {
                            new FLValidationError(export, row,
                                    "Incorrect value", m_fieldName,
                                    helper.getExportFormatRowFieldValue(row, export, m_fieldName),
                                    "If Separation Date is in the current Fiscal Year being reported or is zeroes, " +
                                            "then Employment Date, Current Position must be greater than or equal to " +
                                            "Employment Date, Continuous Employment and Employment Date, Original " +
                                            "Position.");
                        }
                    } else if ("Employment Date CE".equals(m_fieldName)) {
                        if (current
                                .before(FLStaffExtractValidation.parseDate(errors, "Employment Date OP", helper, export,
                                        row))) {

                            new FLValidationError(export, row,
                                    "Incorrect value", m_fieldName,
                                    helper.getExportFormatRowFieldValue(row, export, m_fieldName),
                                    "If Separation Date is in the current Fiscal Year being reported or is zeroes, " +
                                            "then Employment Date, Continuous Employment must be greater than or equal to "
                                            +
                                            "Employment Date, Original Position.");

                        }
                    }
                }
            } else {
                errors.add(new FLValidationError(export, row, "Field not found", m_fieldName, "", ""));
            }
            return errors;
        }
    }

    /**
     * The Class ValidateFlEducationId.
     */
    public static class ValidateFlEducationId extends ValidateRegularExpression {

        /**
         * Instantiates a new validate fl education id.
         */
        public ValidateFlEducationId() {
            super("FL Education Id", "^(?!FL0{12}$)FL\\d{12}$",
                    "Florida Education Identifier (FLEID) is alphanumeric and must be entered as FL " +
                            "in the first 2 positions followed by twelve numeric digits. No blanks or spaces are " +
                            "allowable.");
        }
    }

    /**
     * The Class ValidateParaprofessionalCode.
     */
    public static class ValidateParaprofessionalCode extends ValidateRegularExpression {

        /**
         * Instantiates a new validate paraprofessional code.
         */
        public ValidateParaprofessionalCode() {
            super("H Q Paraprofessional", "^A|B|C|D|Z$",
                    "Highly Qualified Paraprofessional code must be A, B, C, D, or Z.");
        }

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
            List<FLValidationError> errors = super.getValidationErrors(helper, export, row);
            if (errors.isEmpty()) {
                String value = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
                String jobCode = helper.getExportFormatRowFieldValue(row, export, "Job Code Primary");
                if (jobCode != null) {
                    if (jobCode != null && "Z".equals(value)
                            && !FLStateReportData.SURVEY_PERIOD_8.equals(helper.getSurveyCode()) &&
                            (jobCode.equals("51111") || jobCode.equals("51112") ||
                                    jobCode.equals("51113"))) {
                        errors.add(new FLValidationError(export, row, "Incorrect value", m_fieldName,
                                value,
                                "Highly Qualified Paraprofessional code must be A, B, C, or D for Job Codes 51111, 51112 and 51113."));
                    }
                    if (jobCode != null && !"Z".equals(value)
                            && !FLStateReportData.SURVEY_PERIOD_8.equals(helper.getSurveyCode()) &&
                            !jobCode.startsWith("51") && !jobCode.startsWith("52") && !jobCode.startsWith("53") &&
                            !jobCode.startsWith("54") && !jobCode.startsWith("55") && !jobCode.startsWith("59")) {
                        errors.add(new FLValidationError(export, row, "Incorrect value", m_fieldName,
                                value,
                                "If Survey Period is 2, 3 or 5 and the Highly Qualified Paraprofessional code is A, B, C, or D then the Job Code must begin with 51, 52, 53, 54, 55 or 59."));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatePersonnelEvaluationDetails.
     */
    public static class ValidatePersonnelEvaluationDetails implements FLValidationProcessor {
        private String m_fieldName;
        private String[] m_jobCodeExemptions;
        private int m_max;
        private int m_min;
        private String m_pattern;

        /**
         * Instantiates a new validate personnel evaluation details.
         *
         * @param fieldName String
         * @param jobCodeExemptions String[]
         * @param min int
         * @param max int
         */
        public ValidatePersonnelEvaluationDetails(String fieldName, String[] jobCodeExemptions, int min, int max) {
            this(fieldName, jobCodeExemptions, min, max, null);
        }

        /**
         * Instantiates a new validate personnel evaluation details.
         *
         * @param fieldName String
         * @param jobCodeExemptions String[]
         * @param pattern String
         */
        public ValidatePersonnelEvaluationDetails(String fieldName, String[] jobCodeExemptions, String pattern) {
            this(fieldName, jobCodeExemptions, -1, -1, pattern);
        }

        /**
         * Instantiates a new validate personnel evaluation details.
         *
         * @param fieldName String
         * @param jobCodeExemptions String[]
         * @param min int
         * @param max int
         * @param pattern String
         */
        public ValidatePersonnelEvaluationDetails(String fieldName, String[] jobCodeExemptions, int min, int max,
                String pattern) {
            m_fieldName = fieldName;
            m_jobCodeExemptions = jobCodeExemptions;
            m_min = min;
            m_max = max;
            m_pattern = pattern;
        }

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
            if (!FLStateReportData.SURVEY_PERIOD_8.equals(helper.getSurveyCode())) {
                String svalue = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
                if (StringUtils.isEmpty(svalue)) {
                    errors.add(new FLValidationError(export, row, "Field not found", m_fieldName, "", ""));
                } else {
                    boolean ok = false;
                    try {
                        int value = StringUtils.isEmpty(m_pattern) ? Integer.parseInt(svalue) : -1;
                        if ((FLStateReportData.SURVEY_PERIOD_2.equals(helper.getSurveyCode()) ||
                                FLStateReportData.SURVEY_PERIOD_3.equals(helper.getSurveyCode()))
                                && (value == 0 || "Z".equals(svalue))) {
                            ok = true;
                        } else if (FLStateReportData.SURVEY_PERIOD_5.equals(helper.getSurveyCode())) {
                            String jobCode = helper.getExportFormatRowFieldValue(row, export, "Job Code Primary");
                            String districtNumber = helper.getExportFormatRowFieldValue(row, export, "District Number");
                            if (!ArrayUtils.contains(m_jobCodeExemptions, jobCode) && !"68".equals(districtNumber)) {
                                if (StringUtils.isEmpty(m_pattern)) {
                                    String peCode =
                                            helper.getExportFormatRowFieldValue(row, export, "Personnel Evaluation");
                                    if ("H".equals(peCode) || "I".equals(peCode)) {
                                        ok = value == 0;
                                    } else {
                                        ok = value >= m_min && value <= m_max;
                                    }
                                } else {
                                    ok = svalue.matches(m_pattern);
                                }
                            } else {
                                ok = value == 0;
                            }
                        }
                    } catch (Exception ex) {
                        System.out.println(ex);
                    }
                    if (!ok) {
                        String jobCodes = StringUtils.join(m_jobCodeExemptions);
                        errors.add(new FLValidationError(export, row, "Incorrect value", m_fieldName,
                                svalue,
                                "If Survey Period Code = 5 and if the employee's Job Code places the employee " +
                                        "on lines xx-xx, inclusive, of the Public Schools Staff Survey - EEO-5, and if the Job "
                                        +
                                        " Code is not " + jobCodes + ", and if " +
                                        "the District Number is not 68, then the " + m_fieldName
                                        + "must be numeric, greater than or equal to " +
                                        m_min + " and less than or equal to " + m_max + ", " +
                                        "unless Personnel Evaluation code = H or I, then must be zero. For all other employees the "
                                        +
                                        "Personnel Evaluation, Instructional Practice Component must be zero. If Survey Period "
                                        +
                                        "Code = 2 or 3, then the " + m_fieldName + " must be zero."));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatePersonnelEvaluationExtra.
     */
    public static class ValidatePersonnelEvaluationExtra implements FLValidationProcessor {

        private static final String[] PK_JOB_CODES =
                new String[] {"52015", "55052"};

        private static final String[] SUBSTITUTE_JOB_CODES =
                new String[] {"51080", "52080", "53080", "54080", "55080", "59080"};

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
            String jobCode = helper.getExportFormatRowFieldValue(row, export, "Job Code Primary");
            String peCode = helper.getExportFormatRowFieldValue(row, export, "Personnel Evaluation");

            // 1#
            if (FLStateReportData.SURVEY_PERIOD_5.equals(helper.getSurveyCode())) {
                String districtNumber = helper.getExportFormatRowFieldValue(row, export, "District Number");
                if (!"73026".equals(jobCode) && !ArrayUtils.contains(PK_JOB_CODES, jobCode)
                        && !ArrayUtils.contains(SUBSTITUTE_JOB_CODES, jobCode) &&
                        !"68".equals(districtNumber) && peCode.matches("^[C-G]$")) {
                    int practice = parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Practice"));
                    int leadership = parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Leadership"));
                    if (!(leadership >= 33 && leadership <= 67) && !(practice >= 33 && practice <= 67)) {
                        errors.add(new FLValidationError(export, row,
                                "Incorrect value", "Personnel Evaluation Leadership",
                                String.valueOf(leadership),
                                "If Survey Period Code = 5, and the employee's Job Code places the employee on " +
                                        "lines 09-19 or 21-43, inclusive, of the Public Schools Staff Survey - EEO-5, and if the "
                                        +
                                        "Job Code is not 73026 (Registrar), or 52015 or 55052 (PK Teachers), or 51080, 52080, 53080, 54080, 55080 or 59080 "
                                        +
                                        "(Substitute Teachers), and if the District Number is not 68, and Personnel Evaluation "
                                        +
                                        "code is C-G then the Personnel Evaluation, Instructional Leadership Component or the "
                                        +
                                        "Personnel Evaluation, Instructional Practice Component must be greater than or equal "
                                        +
                                        "to 33 or less than or equal to 67."));
                    }
                }
            } else if (FLStateReportData.SURVEY_PERIOD_3.equals(helper.getSurveyCode())
                    && !ArrayUtils.contains(PK_JOB_CODES, jobCode)
                    && !ArrayUtils.contains(SUBSTITUTE_JOB_CODES, jobCode)) {
                // 1$
                String empType = helper.getExportFormatRowFieldValue(row, export, "Employee Type");
                if ("RF".equals(empType) || "RP".equals(empType) || "CF".equals(empType) || "CP".equals(empType)
                        || "TF".equals(empType)) {
                    PlainDate empDate = parseDate(errors, "Employment Date CP", helper, export, row);
                    Calendar calendar = Calendar.getInstance();
                    calendar.setTime(helper.getContext().getStartDate());
                    calendar.set(Calendar.MONTH, 6);
                    calendar.set(Calendar.DAY_OF_MONTH, 30);
                    if (empDate.after(calendar.getTime()) && !peCode.matches("^[C-I]$")) {
                        errors.add(new FLValidationError(export, row,
                                "Incorrect value", "Personnel Evaluation",
                                peCode,
                                "If Survey Period Code is 3 and if the employee's Job Code places the employee " +
                                        "on lines 21-33, inclusive, of the Public Schools Staff Survey - EEO-5, and if the Job "
                                        +
                                        "Code is not 52015 or 55052 (PK Teachers), 51080, 52080, 53080, 54080, 55080 or 59080 (Substitute Teachers), and if "
                                        +
                                        "Employee Type is RF, RP, CF, CP or TF, and if Employment Date, Current Position is on or after "
                                        +
                                        "July 1 of the current fiscal year, then the Personnel Evaluation code must be C-I. "));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatePersonnelEvaluationTotals.
     */
    public static class ValidatePersonnelEvaluationTotals implements FLValidationProcessor {

        private static final String[] PK_JOB_CODES =
                new String[] {"52015", "55052"};

        private static final String[] SUBSTITUTE_JOB_CODES =
                new String[] {"51080", "52080", "53080", "54080", "55080", "59080"};

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
            if (FLStateReportData.SURVEY_PERIOD_5.equals(helper.getSurveyCode())) {
                String jobCode = helper.getExportFormatRowFieldValue(row, export, "Job Code Primary");
                if (!"73026".equals(jobCode) && !ArrayUtils.contains(PK_JOB_CODES, jobCode)
                        && !ArrayUtils.contains(SUBSTITUTE_JOB_CODES, jobCode)) {
                    String districtNumber = helper.getExportFormatRowFieldValue(row, export, "District Number");
                    if (!"68".equals(districtNumber)) {
                        int practice = parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Practice"));
                        int leadership = parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Leadership"));
                        int responsibilities =
                                parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Responsibilities"));
                        int stdPerformance =
                                parseInt(helper.getExportFormatRowFieldValue(row, export, "PE Std Performance"));
                        if (leadership >= 33 && (leadership + responsibilities + stdPerformance) != 100) {
                            errors.add(new FLValidationError(export, row,
                                    "Incorrect value for ", "Personnel Evaluation Leadership",
                                    String.valueOf(leadership),
                                    "If Survey Period Code = 5 and if the employee's Job Code places the employee "
                                            +
                                            "on lines 09-19, inclusive, of the Public Schools Staff Survey - EEO-5, and if the Job "
                                            +
                                            "Code is not 73026 (Registrar), and if the District Number is not 68, and if the value "
                                            +
                                            "reported for this employee for the Personnel Evaluation, Instructional Leadership "
                                            +
                                            "Component is greater than or equal to 33, then the total of Personnel Evaluation, "
                                            +
                                            "Instructional Leadership Component; Personnel Evaluation, Professional and Job "
                                            +
                                            "Responsibilities Component and Personnel Evaluation, Student Performance "
                                            +
                                            "Component must be 100"));
                        }

                        if (practice >= 33 && (practice + responsibilities + stdPerformance) != 100) {
                            errors.add(new FLValidationError(export, row,
                                    "Incorrect value for ", "Personnel Evaluation Practice",
                                    String.valueOf(practice),
                                    "If Survey Period Code = 5 and if the employees Job Code places the " +
                                            "employee on lines 21-43, inclusive, of the Public Schools Staff Survey - EEO-5, "
                                            +
                                            "and if the Job Code is not 51080, 52080, 53080, 54080, 55080 or 59080 (Substitute "
                                            +
                                            "Teachers), and if the District Number is not 68, and if the value reported for this "
                                            +
                                            "employee for the Personnel Evaluation, Instructional Practice Component is "
                                            +
                                            "greater than or equal to 33, then the sum of Personnel Evaluation, Instructional "
                                            +
                                            "Practice Component; Personnel Evaluation, Professional and Job " +
                                            "Responsibilities Component and Personnel Evaluation, Student Performance "
                                            +
                                            "Component must be 100."));
                        }
                    }
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidateReadingEndorsement.
     */
    public static class ValidateReadingEndorsement implements FLValidationProcessor {

        private static final String FIELD_MSG = "Reading Endorsement should be one of C,G,N,P,R,Y or Z";
        private static final String FIELD_NAME_2011 = "R Endorsement 2011 ";
        private static final String[] VALID_VALUES = new String[] {"C", "G", "N", "P", "R", "Y", "Z"};

        private int m_index;

        /**
         * Instantiates a new validate reading endorsement.
         *
         * @param index int
         */
        public ValidateReadingEndorsement(int index) {
            m_index = index;
        }

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
            String fieldName2011 = FIELD_NAME_2011 + Integer.toString(m_index);
            String value = helper.getExportFormatRowFieldValue(row, export, fieldName2011);
            if (value != null) {
                if (!ArrayUtils.contains(VALID_VALUES, value)) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", fieldName2011,
                            value, FIELD_MSG));
                } else {
                    for (String code : new String[] {"Z", "P", "C", "R", "G"}) {
                        if (code.equals(value)) {
                            for (int i = 1; i <= 5; i++) {
                                if (!code.equals(helper.getExportFormatRowFieldValue(row, export,
                                        FIELD_NAME_2011 + i))) {
                                    errors.add(new FLValidationError(export, row,
                                            "Incorrect value", "Reading Endorsement",
                                            code,
                                            "If any of the eleven Reading Endorsement, Competency codes = "
                                                    + code
                                                    + ", then all the codes for Reading Endorsement, Competency must be "
                                                    + code));
                                }
                            }
                        }
                    }
                }
            } else if (!helper.getSurveyCode().equals(FLStateReportData.SURVEY_PERIOD_8)) {
                errors.add(new FLValidationError(export, row, "Field not found", fieldName2011, "", FIELD_MSG));
            }
            return errors;
        }
    }

    /**
     * The Class ValidateStaffNumberLocal.
     */
    public static class ValidateStaffNumberLocal implements FLValidationProcessor {
        private static final String FIELD_NAME = "Staff ID Local";

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
            String value = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME);
            if (StringUtils.isEmpty(value)) {
                errors.add(new FLValidationError(export, row, "Field not found", FIELD_NAME, "", ""));
            } else if (value.equals(helper.getExportFormatRowFieldValue(row, export, "SSN"))) {
                errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME,
                        value,
                        "The Staff Number Identifier, Local must not be identical to the Social Security Number."));
            }
            return errors;
        }

    }

    /**
     * Gets the export value.
     *
     * @param helper FLExportConfiguration
     * @param currentExport FL_EXPORT
     * @param row ExportFormatRow
     * @param procedure_id String
     * @param fieldName String
     * @param keyFieldNamePairs KeyValuePair<String,String>[]
     * @return String
     */
    public static String getExportValue(FLExportConfiguration helper,
                                        FLExport currentExport,
                                        ExportFormatRow row,
                                        String procedure_id,
                                        String fieldName,
                                        KeyValuePair<String, String>... keyFieldNamePairs) {
        String value = null;
        Collection<ExportFormatRow> erows =
                getExportValues(helper, currentExport, row, procedure_id, keyFieldNamePairs);
        if (erows != null && !erows.isEmpty()) {
            FLExport matchingExport = helper.getExportFromProcedureId(procedure_id);
            for (ExportFormatRow erow : erows) {
                value = helper.getExportFormatRowFieldValue(erow, matchingExport, fieldName);
                break;
            }
        }
        return value;
    }

    /**
     * Gets the export values.
     *
     * @param helper FLExportConfiguration
     * @param currentExport FL_EXPORT
     * @param row ExportFormatRow
     * @param procedure_id String
     * @param keyFieldNamePairs KeyValuePair<String,String>[]
     * @return Collection
     */
    public static Collection<ExportFormatRow> getExportValues(FLExportConfiguration helper,
                                                              FLExport currentExport,
                                                              ExportFormatRow row,
                                                              String procedure_id,
                                                              KeyValuePair<String, String>... keyFieldNamePairs) {
        FLExport matchingExport = helper.getExportFromProcedureId(procedure_id);

        LookupField key = helper.new LookupField();
        LookupField lookupValue = helper.new LookupField();

        for (KeyValuePair<String, String> pair : keyFieldNamePairs) {
            key.add(pair.getValue());
            lookupValue.add(helper.getExportFormatRowFieldValue(row, currentExport, pair.getKey()));
        }
        return helper.getExportFormatRows(matchingExport, key, lookupValue);
    }

    /**
     * Parses the int.
     *
     * @param sint String
     * @return int
     */
    public static int parseInt(String sint) {
        try {
            return Integer.parseInt(sint);
        } catch (NumberFormatException ex) {
            System.out.println(ex);
            return 0;
        }
    }

    /**
     * Instantiates a new FL staff extract validation.
     */
    public FLStaffExtractValidation() {}

    /**
     * Gets the validation rules.
     *
     * @return List<FLValidationRule>
     */
    public List<FLValidationRule> getValidationRules() {
        return Arrays.asList(new FLValidationRule[] {
                new FLValidationRule("STF", "1", new ValidateDistrictNumber("District Number")),
                new FLValidationRule("STF", "2", new ValidateSSN()),
                new FLValidationRule("STF", "3", new ValidateRegularExpression("Survey Period",
                        "^[2358]$",
                        "Survey Period Code must be correct for the submission specified by the district and must be 2, 3, 5 or 8")),
                new FLValidationRule("STF", "4", new ValidateFiscalYear()),

                new FLValidationRule("STF", "5", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byActiveSchool("School Number"))),
                        "School Number, Primary/Home must exist on the Master School Identification File as a valid active school in the district of submission.")),
                new FLValidationRule("STF", "6", new ValidateRegularExpression("Certificate Number",
                        "^(?!60{2}[2-4]0{6})([0-5][0-9]{9}|600[0-4][0-9]{6}|9999999999$)",
                        "Must be numeric, and in the range 0000000000 - 6001999999, 6002000001 - 6002999999, 6003000001 - 6003999999, 6004000001 - 6004999999 or 9999999999")),
                new FLValidationRule("STF", "7_8_25_26", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(Restriction.pattern("Employee Name", "^.*\\S.*$"),
                                        Restriction.pattern("Employee Name", "^[\\p{L}\\s\\-'\"\\.,\\/]+$")))),
                        "For the Employee Name, Legal; the Last Name cannot be blank. Allowable characters include double or single quotation marks, commas, slashes, periods, hyphens and accent marks.")),
                new FLValidationRule("STF", "11", new ValidateReadingEndorsement(1)),
                new FLValidationRule("STF", "1E", new ValidateReadingEndorsement(2)),
                new FLValidationRule("STF", "1F", new ValidateReadingEndorsement(3)),
                new FLValidationRule("STF", "1G", new ValidateReadingEndorsement(4)),
                new FLValidationRule("STF", "1H", new ValidateReadingEndorsement(5)),
                new FLValidationRule("STF", "2G_2O_2R_2S_2Z", new ValidateReadingEndorsement(5)),
                new FLValidationRule("STF", "10",
                        new ValidateRegularExpression("Gender", "^M|F$", "Gender should be M or F")),
                new FLValidationRule("STF", "28_29_2A_2B_2C_2D",
                        new ValidateAbsentPresentDays(
                                "If Survey Period Code is 2 or 3, then must be 000. If Survey Period Code is 5, " +
                                        "then must be numeric and less than or equal to 180 or it must be 999, unless "
                                        +
                                        "District Number is 71",
                                "Days Present", "Days Absent Personal", "Days Absent Sick", "Days Absent Duty",
                                "Absent Other")),

                new FLValidationRule("STF", "15", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.or(Restriction.equals("Separation Date", "00000000"),
                                        Restriction.byDateFormat("Separation Date")))),
                        "Separation Date must be numeric and a valid date which is prior to the current"
                                + " date, or it must be all zeros. This edit does not apply to Survey 8.")),

                new FLValidationRule("STF", "16", new ValidateRegularExpression("Separation Reason", "^[A-P]|Z$",
                        "Separation Reason code must be A-P or Z. This edit does not apply to Survey 8.")),

                // TODO: implement validation rule 1A
                new FLValidationRule("STF", "17", new ValidateRegularExpression("Job Code Primary", "^\\d+$",
                        "Job Code, Primary must equal one of the codes on the Job Code Assignments table.")),

                new FLValidationRule("STF", "1B",
                        new ValidateRegularExpression("Ethnicity", "^Y|N$", "Ethnicity should be Y or N")),

                new FLValidationRule("STF", "1C", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.patternForFields("^Y|N$",
                                        "Race American Native", "Race Asian", "Race Black", "Race Pacific",
                                        "Race White"))),
                        "Race: American Indian or Alaska Native; Race: Asian; Race: Black or African " +
                                "American; Race: Native Hawaiian or Other Pacific Islander, and Race: White must be Y or N.")),

                new FLValidationRule("STF", "1D", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.or(
                                        Restriction.equals("Race American Native", "Y"),
                                        Restriction.equals("Race Asian", "Y"),
                                        Restriction.equals("Race Black", "Y"),
                                        Restriction.equals("Race Pacific", "Y"),
                                        Restriction.equals("Race White", "Y")))),
                        "There must be a Y code for at least one of the Race data elements (Race: " +
                                "American Indian or Alaska Native, Race: Asian, Race: Black or African American, Race: "
                                +
                                "Native Hawaiian or Other Pacific Islander and Race: White).")),

                new FLValidationRule("STF", "18", new ValidateRegularExpression("Transaction Code", "^A|C|D$",
                        "The Transaction Code must be A, C or D.")),
                new FLValidationRule("STF", "24",
                        new ValidateRegularExpression("Employee Type", "^RF|RP|TF|TP|CF|CP|ST$",
                                "Employee Type code must be RF, RP, TF, TP, or ST.")),

                new FLValidationRule("STD", "9_32_57", new ValidateAge()),

                new FLValidationRule("STF", "27", new ValidateRegularExpression("Degree/Credential", "^C|A|B|M|S|D|Z$",
                        "The Degree/Credential Earned code must be C, A, B, M, S, D, or Z.")),

                // TODO: implement 2W rule, related to lines 44-54, inclusive, of the Public Schools
                // Staff Survey - EEO-5

                new FLValidationRule("STF", "12_51_52", new ValidateEmploymentDate("Employment Date CP")),
                new FLValidationRule("STF", "13_53", new ValidateEmploymentDate("Employment Date CE")),
                new FLValidationRule("STF", "14", new ValidateEmploymentDate("Employment Date OP", false)),

                new FLValidationRule("STF", "20", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Survey Period", "^2|3")).testThen(
                                        Restriction.equals("Separation Date", "00000000"))),
                        "If Survey Period Code is 2 or 3, then Separation Date must be zeros.")),
                new FLValidationRule("STF", "21", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(
                                Restriction.pattern("Survey Period", "^2|3")).testThen(
                                        Restriction.equals("Separation Reason", "Z"))),
                        "If Survey Period Code is 2 or 3, then Separation Reason code must be Z.")),

                new FLValidationRule("STF", "19",
                        new ValidateRegularExpression("Exempt Public Law", "^Y|Z$",
                                "Exempt from Public Records Law, Employee, must be Y or Z.")),

                // TODO: implement rule 2K which references to lines 21-33 of the Public Schools
                // Staff Survey (EEO-5)
                new FLValidationRule("STF", "2H_2I_2J", new ValidateParaprofessionalCode()),

                // TODO: implement rule 2Y related to lines 1-43, inclusive, of the Public Schools
                // Staff Survey
                new FLValidationRule("STF", "2X", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"))
                                .testThen(Restriction.pattern("School Principal Cer", "^Z$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.pattern("School Principal Cer", "^A|B|C|D|Z$"))),
                        "If Survey Period Code is 2 or 3, then School Principal certification Program code " +
                                "must be Z. If Survey Period Code is 5, then School Principal Certification Program " +
                                "must be A, B, C, D or Z.")),

                new FLValidationRule("STF", "1M", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                                .testThen(Restriction.equals("Supervising Educator", "Z")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"))
                                .testThen(Restriction.pattern("Supervising Educator", "^Y|N|Z$"))),
                        "Mentor/Supervising Educator code must be Y, N or Z. If Survey Period Code is " +
                                "5, Mentor/Supervising Educator code must be Z.")),

                // TODO: rule# 1O contains statement about lines 09-19 or 21-43, inclusive, of the
                // Public Schools Staff Survey - EEO-5, we should investigate and apply it to the
                // code beneath
                new FLValidationRule("STF", "1O", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.equals("Survey Period", "2"))
                                .testThen(Restriction.equals("Personnel Evaluation", "Z")),
                        ValidationRule.testIf(Restriction.pattern("Survey Period", "^3|5$"))
                                .testThen(Restriction.pattern("Personnel Evaluation", "^[C-I]|Z$")),
                        ValidationRule.testIf(Restriction.equals("Survey Period", "5"),
                                Restriction.pattern("Job Code Primary",
                                        "^((?!(73026|51080|52015|55052|52080|53080|54080|55080|59080)).)*$"))
                                .testThen(Restriction.notEquals("Personnel Evaluation", "Z"))),
                        "Personnel Evaluation code must be C-I or Z. If Survey Period Code = 2, then " +
                                "Personnel Evaluation code must be Z. If Survey Period Code = 5, and if the employee's "
                                +
                                "Job Code places the employee on lines 09-19 or 21-43, inclusive, of the Public Schools "
                                +
                                "Staff Survey - EEO-5, and if the Job Code is not 73026 (Registrar); or 52015 or 55052 "
                                +
                                "(PK Teachers); or 51080, 52080, "
                                +
                                "53080, 54080, 55080 or 59080 (Substitute Teachers) then the Personnel Evaluation code "
                                +
                                "must be C-I.")),
                new FLValidationRule("STF", "1P", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.notEquals("Survey Period", "8"),
                                Restriction.pattern("Job Code Primary",
                                        "^51080|52015|52080|53080|54080|55052|55080|59080|73026$"))
                                .testThen(Restriction.equals("Personnel Evaluation", "Z"))),
                        "If the employee's Job Code, Primary is 51080, 52015, 52080, 53080, 54080, 55052," +
                                "55080, 59080, or 73026, then the Personnel Evaluation code must be Z.")),

                // TODO: rules# 1Q, 1R, 1S, 1T, 1U, 1V, 1W, 1#, 1$ contains statement about lines of
                // the Public Schools Staff Survey - EEO-5, we should investigate and apply it to
                // the rows beneath
                new FLValidationRule("STF", "1Q", new ValidatePersonnelEvaluationDetails("PE Leadership",
                        new String[] {"73026"}, 33, 67)),
                new FLValidationRule("STF", "1R", new ValidatePersonnelEvaluationDetails("PE Practice",
                        new String[] {"51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"}, 33, 67)),
                new FLValidationRule("STF", "1S", new ValidatePersonnelEvaluationDetails("PE Responsibilities",
                        new String[] {"73026", "51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"},
                        0, 33)),
                new FLValidationRule("STF", "1T", new ValidatePersonnelEvaluationDetails("PE Std Performance",
                        new String[] {"73026", "51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"},
                        33, 67)),
                new FLValidationRule("STF", "1U", new ValidatePersonnelEvaluationDetails("Performance Measures",
                        new String[] {"73026", "51080", "52015", "52080", "53080", "54080", "55052", "55080", "59080"},
                        "^[B-K]$")),
                new FLValidationRule("STF", "1V_1W", new ValidatePersonnelEvaluationTotals()),
                new FLValidationRule("STF", "1#_1$", new ValidatePersonnelEvaluationExtra()),

                new FLValidationRule("STF", "1N", new ValidateFlEducationId()),
                new FLValidationRule("STF", "2T_2U", new ValidateStaffNumberLocal()),

                new FLValidationRule("STF", "23", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                                Restriction.uniqueValue("District Number", "SSN", "Survey Period", "Fiscal Year"))),
                        "Each Staff Demographic Information record must be unique based on District " +
                                "Number, Social Security Number, Survey Period Code, and Fiscal Year.")),

                new FLValidationRule("STF", "2E", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.notEquals("Survey Period", "8"),
                                Restriction.greaterThanOrEquals("Separation Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_BEGIN_DATE)),
                                Restriction.lessThanOrEquals("Separation Date",
                                        new RuntimeParam(RuntimeParam.FISCAL_END_DATE))))
                                .testThen(Restriction.notEquals("Separation Reason", "Z"))),
                        "If Separation Date falls within the Fiscal Year being reported, then Separation"
                                + " Reason code must not be Z. This edit does not apply to Survey 8.")),

                new FLValidationRule("STF", "2F", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.notEquals("Survey Period", "8"),
                                Restriction.notEquals("Separation Reason", "Z")))
                                .testThen(Restriction.notEquals("Separation Date", "00000000"))),
                        "If Separation Reason code is not Z, then Separation Date must be greater than"
                                + " zero. This edit does not apply to Survey 8.")),

                new FLValidationRule("STF", "2V", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.notEquals("Survey Period", "8"),
                                Restriction.pattern("Separation Reason", "^[A-P]$")))
                                .testThen(Restriction.pattern("Employee Type", "RF|RP"))),
                        "If Separation Reason code is A-P, then Employee type must be RF or RP. This"
                                + " edit does not apply to Survey 8.")),

                // TODO: implement rule #40 related to MSID file
                // TODO: implement rule #50 related to MSID file

                // TODO: implement rule #55 related to Public Schools Staff Survey (EEO-5)
                // TODO: implement rule #56 related to Public Schools Staff Survey (EEO-5)

                // TODO: rule 30, apply export IDS for: Staff Payroll, Fiscal Year Salaries,
                /*
                 * new FLValidationRule("STF", "30", new FLValidationRuleSet(new RuleSet(
                 * ValidationRule.testIf(Restriction.pattern("Survey Period", "^2|3$"))
                 * .testThen(Restriction.
                 * validateMatchInExport("EXPDATA-FL-<Staff Payroll Export ID>",
                 * new KeyValuePair("District Number", "District Number"),
                 * new KeyValuePair("SSN", "SSN"),
                 * new KeyValuePair("Survey Period", "Survey Period"),
                 * new KeyValuePair("Fiscal Year", "Fiscal Year"))),
                 *
                 * ValidationRule.testIf(Restriction.equals("Survey Period", "5"))
                 * .testThen(
                 * Restriction.or(
                 * Restriction.validateMatchInExport(
                 * "EXPDATA-FL-<Fiscal Year Salaries Export ID>",
                 * new KeyValuePair("District Number", "District Number"),
                 * new KeyValuePair("SSN", "SSN"),
                 * new KeyValuePair("Survey Period", "Survey Period"),
                 * new KeyValuePair("Fiscal Year", "Fiscal Year")),
                 * Restriction.validateMatchInExport(
                 * "EXPDATA-FL-<Fiscal Year Benefits Export ID>",
                 * new KeyValuePair("District Number", "District Number"),
                 * new KeyValuePair("SSN", "SSN"),
                 * new KeyValuePair("Survey Period", "Survey Period"),
                 * new KeyValuePair("Fiscal Year", "Fiscal Year"))))),
                 *
                 * "If Survey Period Code is 2 or 3, each Staff Demographic Information " +
                 * "record must have a matching Staff Payroll record based on District Number, " +
                 * "Social Security Number, Survey Period Code and Fiscal Year. If Survey Period " +
                 * "Code is 5, each Staff Demographic Information record must have a matching " +
                 * "Staff Fiscal Year Salaries, or Staff Fiscal Year Benefits format based on District "
                 * +
                 * "Number, Social Security number, Survey Period Code, and Fiscal Year.")),
                 */

                // TODO: implement cross-export rule #31
                // TODO: implement cross-export rule #32
                // TODO: implement cross-export rule #33
                // TODO: implement cross-export rule #34
                // TODO: implement cross-export rule #35
                // TODO: implement cross-export rule #36
                // TODO: implement cross-export rule #38

                new FLValidationRule("STF", "38", new FLValidationRuleSet(new RuleSet(
                        ValidationRule.testIf(Restriction.and(
                                Restriction.pattern("Survey Period", "^2|3$"),
                                Restriction.pattern("Job Code Primary",
                                        "^(?!.*(51080|52080|53080|54080|55080|59080)).*$"),
                                Restriction.pattern("Employee Type", "^RF|TF$")))
                                .testThen(
                                        Restriction.and(
                                                Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                        new KeyValuePair("District Number", "District Number"),
                                                        new KeyValuePair("SSN", "SSN"),
                                                        new KeyValuePair("Survey Period", "Survey Period"),
                                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                        new KeyValuePair(Pattern.compile("^C$"), "Experience Type")),
                                                Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                        new KeyValuePair("District Number", "District Number"),
                                                        new KeyValuePair("SSN", "SSN"),
                                                        new KeyValuePair("Survey Period", "Survey Period"),
                                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                        new KeyValuePair(Pattern.compile("^D$"), "Experience Type")),
                                                Restriction.validateMatchInExport("EXPDATA-FL-SXP",
                                                        new KeyValuePair("District Number", "District Number"),
                                                        new KeyValuePair("SSN", "SSN"),
                                                        new KeyValuePair("Survey Period", "Survey Period"),
                                                        new KeyValuePair("Fiscal Year", "Fiscal Year"),
                                                        new KeyValuePair(Pattern.compile("^F$"), "Experience Type"))))),
                        "For Survey Period Code 2 or 3, if the employee's Job Code, Primary on " +
                                "the Staff Demographic Information format places the employee on lines 21-33, " +
                                "inclusive, of the Public Schools Staff Survey - EEO-5, excluding substitute " +
                                "teachers (Job Code, Primary code equal to 51080, 52080, 53080, 54080, 55080 or " +
                                "59080) and the Employee Type is RF or TF, then the employee must have at least " +
                                "one Staff Experience record with an Experience Type of C, at least one record " +
                                "with an Experience Type of D and at least one record with an Experience Type " +
                                "of F . The following fields should be used in matching the records: District " +
                                "Number, Social Security Number, Survey Period Code and Fiscal Year. ")),

                // TODO: implement cross-export rule #54
                // TODO: implement cross-export rule #55
                // TODO: implement cross-export rule #59
        });
    }

    /**
     * Parses the date.
     *
     * @param errors List<FLValidationError>
     * @param fieldName String
     * @param helper FLExportConfiguration
     * @param export FL_EXPORT
     * @param row ExportFormatRow
     * @return PlainDate
     */
    protected static PlainDate parseDate(List<FLValidationError> errors,
                                         String fieldName,
                                         FLExportConfiguration helper,
                                         FLExport export,
                                         ExportFormatRow row) {
        PlainDate date = null;
        String value = helper.getExportFormatRowFieldValue(row, export, fieldName);
        if ("00000000".equals(value)) {
            date = ZERO_DATE;
        } else {
            try {
                date = new PlainDate(DEFAULT_DATE_FORMAT.parse(value));
            } catch (ParseException ex) {
                errors.add(new FLValidationError(export, row, "Incorrect value", fieldName,
                        value, ""));
            }
        }
        return date;
    }
}
