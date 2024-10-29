/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.x2dev.reports.bc.StudentAchievementDataExtractData.LIMITED_GRADES;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.reports.bc.StudentAchievementDataExtractData.EXPORT_FIELDS;
import com.x2dev.reports.bc.StudentAchievementDataExtractData.SADEReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "SADE" exception report.
 *
 * @author X2 Development Corporation
 */
public class StudentAchievementDataExceptionData extends ReportJavaSourceNet {

    /**
     * Rule type definition (based on subclasses).
     */
    enum RULE_TYPE {
        SCHOOL_ID_RULE(SchoolIDRule.class), PEN_RULE(PENRule.class), COURSE_CODE_RULE(
                CourseCodeRule.class), ACTIVE_DATE_RULE(ActiveDateRule.class), DUPLICATE_RECORD_RULE(
                        DuplicateRecordRule.class), SCHOOL_MARK_RULE(SchoolMarkRule.class), EQUIVALENCY_CHALLENGE_RULE(
                                EquivalencyChallengeRule.class), COMPLETION_DATE_RULE(
                                        CompletionDateRule.class), CREDITS_RULE(CreditsRule.class);

        /**
         * Instantiates a new rule type.
         *
         * @param ruleClass Class<? extends IRule>
         */
        private RULE_TYPE(Class<? extends IRule> ruleClass) {
            this.ruleClass = ruleClass;
        }

        private Class<? extends IRule> ruleClass;

        /**
         * Gets the rule class.
         *
         * @return the rule class
         */
        public Class<? extends IRule> getRuleClass() {
            return this.ruleClass;
        }
    }

    /**
     * The Enum EXCEPTION_TYPE.
     */
    enum EXCEPTION_TYPE {
        ERROR("E"), WARNING("W");

        private String m_exceptionTypeCode;

        /**
         * Instantiates a new exception type.
         *
         * @param exceptionTypeCode String
         */
        private EXCEPTION_TYPE(String exceptionTypeCode) {
            m_exceptionTypeCode = exceptionTypeCode;
        }

        /**
         * Gets the exception type code.
         *
         * @return String
         */
        public String getExceptionTypeCode() {
            return m_exceptionTypeCode;
        }
    }

    /**
     * Exceptions reported by report.
     */
    enum SADE_EXCEPTIONS {
        SCHOOL_ID_1A(EXCEPTION_TYPE.ERROR, "School id (MINCODE) must be included"), SCHOOL_ID_1B(EXCEPTION_TYPE.ERROR,
                "The school id (MINCODE) must be alphanumeric to a maximum of 8 digits"), PEN_2A(EXCEPTION_TYPE.ERROR,
                        "PEN must be included"), PEN_2B(EXCEPTION_TYPE.ERROR,
                                "The PEN must be numeric to a maximum of 10 digits"), COURSE_CODE_3B(
                                        EXCEPTION_TYPE.ERROR,
                                        "Course Code must be included"), COURSE_CODE_3C(EXCEPTION_TYPE.ERROR,
                                                "Maximum length of Course Code field value cannot exceed 8 characters"), ACTIVE_DATE_4A(
                                                        EXCEPTION_TYPE.ERROR,
                                                        "N/A"), ACTIVE_DATE_4C(EXCEPTION_TYPE.ERROR,
                                                                "Active date must be included"), ACTIVE_DATE_4E(
                                                                        EXCEPTION_TYPE.WARNING,
                                                                        "The Course does not have a Start Date. The school year start date has been substituted for the Active Date"), MANDATORY_FIELD(
                                                                                EXCEPTION_TYPE.ERROR,
                                                                                "%s must be included"), MAX_LENGTH(
                                                                                        EXCEPTION_TYPE.ERROR,
                                                                                        "The %s must be %s to a maximum of %d digits"),

        DUPLICATE_RECORDS(EXCEPTION_TYPE.ERROR, ""), SCHOOL_MARK_6A(EXCEPTION_TYPE.ERROR,
                "Invalid school mark type; must be either: 1, 2, 3 or 4"), SCHOOL_MARK_6B(EXCEPTION_TYPE.ERROR,
                        "Mark Type, Mark Value and Completion date must all be entered, or must all be blank"), SCHOOL_MARK_7B(
                                EXCEPTION_TYPE.ERROR,
                                "School mark not found in BCeSIS Enterprise Level Mark Scale"), SCHOOL_MARK_7D(
                                        EXCEPTION_TYPE.ERROR,
                                        "If school mark type is 2, then school mark value must be an integer from 0 to 100 inclusive"), SCHOOL_MARK_7E(
                                                EXCEPTION_TYPE.ERROR,
                                                "If school mark type is 3, then school mark value must be a valid value on the three-point scale"), SCHOOL_MARK_7F(
                                                        EXCEPTION_TYPE.ERROR,
                                                        "If school mark type is 4, then school mark value must be a valid value on the four-point scale"), SCHOOL_MARK_7G(
                                                                EXCEPTION_TYPE.ERROR,
                                                                "Mark Value must be a valid alphanumeric of maximum 3 digits"), SCHOOL_MARK_7I(
                                                                        EXCEPTION_TYPE.ERROR,
                                                                        "An �I� is not a final mark.  If the final school mark is not yet known, please remove this mark or ensure this mark is updated and included in the next submission.  Any �I� left uncorrected may be converted to an F"),

        EQUIVALENCY_CHALLENGE_8A(
                EXCEPTION_TYPE.ERROR,
                "The Equivalency/Challenge entered is not a valid value; valid values are E to indicate Equivalency, or C to indicate Challenge"), EQUIVALENCY_CHALLENGE_8B(
                        EXCEPTION_TYPE.ERROR, "Equivalency/Challenge must be a valid alphanumeric of 1 digit"),

        COMPLETION_DATE_9A(EXCEPTION_TYPE.ERROR,
                "The Completion Date entered must be greater than or equal to the active-date"), COMPLETION_DATE_9D(
                        EXCEPTION_TYPE.WARNING,
                        "Your Completion Date is past the end of the reporting period (and within the submission window)"), COMPLETION_DATE_9E(
                                EXCEPTION_TYPE.ERROR,
                                "The Completion Date cannot be later than the end of the submission window"), CREDITS_10B(
                                        EXCEPTION_TYPE.ERROR,
                                        "The potential credit value for a course must be blank, 1, 2, 3 or 4");

        private EXCEPTION_TYPE m_exceptionType;
        private String m_exceptionMessage;

        /**
         * Gets the exception type.
         *
         * @return exception type
         */
        public EXCEPTION_TYPE getExceptionType() {
            return m_exceptionType;
        }

        /**
         * Gets the exception message.
         *
         * @return String
         */
        public String getExceptionMessage() {
            return m_exceptionMessage;
        }

        /**
         * Instantiates a new sade exceptions.
         *
         * @param exceptionType EXCEPTION_TYPE
         * @param exceptionMessage String
         */
        private SADE_EXCEPTIONS(EXCEPTION_TYPE exceptionType, String exceptionMessage) {
            m_exceptionType = exceptionType;
            m_exceptionMessage = exceptionMessage;
        }
    }

    /**
     * Apply validation for ACTIVE DATE field.
     */
    class ActiveDateRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private PlainDate m_schoolYearStartDate = null;
        private SimpleDateFormat m_formatter = null;

        /**
         * Instantiates a new active date rule.
         *
         * @param field EXPORT_FIELDS
         * @param schoolYearStartDate PlainDate
         * @param formatter SimpleDateFormat
         */
        public ActiveDateRule(EXPORT_FIELDS field, PlainDate schoolYearStartDate, SimpleDateFormat formatter) {
            m_field = field;
            m_schoolYearStartDate = schoolYearStartDate;
            m_formatter = formatter;
        }

        /**
         * Validate.
         *
         * @param row Map<String,Object>
         * @see
         *      com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            String value = (String) row.get(m_field.getFieldId());

            try {
                if (isEmpty(value)) {
                    throwRow(row, SADE_EXCEPTIONS.ACTIVE_DATE_4C);
                } else if (m_formatter.parse(value).compareTo(m_schoolYearStartDate) == 0) {
                    throwRow(row, SADE_EXCEPTIONS.ACTIVE_DATE_4E);
                }
            } catch (Exception ex) {
                throwRow(row, SADE_EXCEPTIONS.ACTIVE_DATE_4A);
            }
        }
    }

    /**
     * Apply validation for COMPLETION DATE rule.
     */
    class CompletionDateRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private DateFormat m_formatter = null;
        private Collection<KeyValuePair<PlainDate, PlainDate>> m_dateRanges;

        /**
         * Instantiates a new completion date rule.
         *
         * @param field EXPORT_FIELDS
         */
        public CompletionDateRule(EXPORT_FIELDS field) {
            m_field = field;
        }

        /**
         * Sets the formatter.
         *
         * @param formatter void
         */
        public void setFormatter(DateFormat formatter) {
            m_formatter = formatter;
        }

        /**
         * Sets the submission ranges.
         *
         * @param ranges Collection<KeyValuePair<PlainDate,PlainDate>>
         */
        public void setSubmissionRanges(Collection<KeyValuePair<PlainDate, PlainDate>> ranges) {
            m_dateRanges = ranges;
        }

        /**
         * Validate.
         *
         * @param row Map<String,Object>
         * @see
         *      com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            checkActiveDate(row);
            checkSubmissionWindow(row);
        }

        /**
         * Validation rule - if completion date is not null it shuld be >= Active Date.
         *
         * @param row Map<String,Object>
         */
        private void checkActiveDate(Map<String, Object> row) {
            String activeString = (String) row.get(EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId());
            String completionString = (String) row.get(m_field.getFieldId());

            if (!isEmpty(activeString) && !isEmpty(completionString)) {
                try {
                    PlainDate activeDate = new PlainDate(m_formatter.parse(activeString));
                    PlainDate completionDate = new PlainDate(m_formatter.parse(completionString));

                    if (completionDate.before(activeDate)) {
                        throwRow(row, SADE_EXCEPTIONS.COMPLETION_DATE_9A);
                    }
                } catch (Exception ex) {
                    AppGlobals.getLog().warning("Exeption occurred checking active date: " + ex);
                }
            }
        }

        /**
         * Validates the completion date of the record against the submission date ranges.
         *
         * @param row Map<String,Object>
         */
        private void checkSubmissionWindow(Map<String, Object> row) {
            String completionString = (String) row.get(m_field.getFieldId());

            if (!isEmpty(completionString)) {
                try {
                    PlainDate completionDate = new PlainDate(m_formatter.parse(completionString));

                    boolean withinEndDate = false;
                    boolean withinStartDate = false;

                    for (KeyValuePair<PlainDate, PlainDate> dateRange : m_dateRanges) {
                        PlainDate submissionStart = dateRange.getKey();
                        PlainDate submissionEnd = dateRange.getValue();

                        if (!completionDate.after(submissionEnd)) {
                            withinEndDate = true;
                        }

                        if (!completionDate.before(submissionStart)) {
                            withinStartDate = true;
                        }
                    }

                    if (!withinEndDate) {
                        throwRow(row, SADE_EXCEPTIONS.COMPLETION_DATE_9D);
                    }

                    if (!withinStartDate) {
                        throwRow(row, SADE_EXCEPTIONS.COMPLETION_DATE_9E);
                    }

                } catch (Exception ex) {
                    AppGlobals.getLog().warning("Exeption occurred checking submission window: " + ex);
                }
            }
        }
    }

    /**
     * Apply validation for COURSE CODE rule.
     */
    class CourseCodeRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private int m_length;

        /**
         * Instantiates a new course code rule.
         *
         * @param field EXPORT_FIELDS
         * @param length int
         */
        public CourseCodeRule(EXPORT_FIELDS field, int length) {
            m_field = field;
            m_length = length;
        }

        /**
         * @see com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_field.getFieldId())) {
                String value = (String) row.get(m_field.getFieldId());

                if (isEmpty(value)) {
                    throwRow(row, SADE_EXCEPTIONS.COURSE_CODE_3B);
                } else if (isFieldLengthExceed(value, m_length)) {
                    throwRow(row, SADE_EXCEPTIONS.COURSE_CODE_3C);
                }
            }
        }
    }

    /**
     * Apply validation for CREDITS field.
     */
    class CreditsRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private List<Object> m_validValues = null;

        /**
         * Instantiates a new credits rule.
         *
         * @param field EXPORT_FIELDS
         * @param validValues List<Object>
         */
        public CreditsRule(EXPORT_FIELDS field, List<Object> validValues) {
            m_field = field;
            m_validValues = validValues;
        }

        /**
         * @see com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_field.getFieldId())) {
                String value = (String) row.get(m_field.getFieldId());

                if (value != null && !isValueExist(value, m_validValues)) {
                    throwRow(row, SADE_EXCEPTIONS.CREDITS_10B);
                }
            }
        }
    }

    /**
     * Apply validation for duplicate records.
     */
    class DuplicateRecordRule extends IRule {
        private EXPORT_FIELDS[] m_keyFields = null;

        Map<String, Map<String, Object>> identityMap = new HashMap<String, Map<String, Object>>();

        /**
         * Instantiates a new duplicate record rule.
         *
         * @param keyFields EXPORT_FIELDS[]
         */
        public DuplicateRecordRule(EXPORT_FIELDS[] keyFields) {
            m_keyFields = keyFields;
        }

        /**
         * Validation logic is following. We are storing hash value for key fields using
         * concatenation
         * Then we store pair <hash-key, row> in Map, and using new hash-key for identifying
         * identity for row
         *
         * @param row Map<String,Object>
         * @see com.x2dev.procedures.bc.SADEExceptionReportData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            StringBuilder identityKey = new StringBuilder();

            for (EXPORT_FIELDS field : m_keyFields) {
                identityKey.append(row.get(field.getFieldId()));
            }

            if (identityMap.containsKey(identityKey.toString())) {
                throwRow(row, SADE_EXCEPTIONS.DUPLICATE_RECORDS);
            }

            identityMap.put(identityKey.toString(), row);
        }
    }

    /**
     * Apply validation for EQUIVALENCY/CHALLENGE rule.
     */
    class EquivalencyChallengeRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        List<Object> m_equivalencyChallengeValidValues = Arrays.asList(new Object[] {"", "E", "C"});

        /**
         * Instantiates a new equivalency challenge rule.
         *
         * @param field EXPORT_FIELDS
         */
        public EquivalencyChallengeRule(EXPORT_FIELDS field) {
            m_field = field;
        }

        /**
         * Validate.
         *
         * @param row Map<String,Object>
         * @see
         *      com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_field.getFieldId())) {
                String value = (String) row.get(m_field.getFieldId());

                if (!isValueExist(value, m_equivalencyChallengeValidValues)) {
                    throwRow(row, SADE_EXCEPTIONS.EQUIVALENCY_CHALLENGE_8A);
                }

                if (isFieldLengthExceed(value, 1) || !isAlphanumeric(value)) {
                    throwRow(row, SADE_EXCEPTIONS.EQUIVALENCY_CHALLENGE_8B);
                }
            }
        }
    }

    /**
     * Apply vaildation for PEN field.
     */
    class PENRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private int m_length;

        /**
         * Instantiates a new PEN rule.
         *
         * @param field EXPORT_FIELDS
         * @param length int
         */
        public PENRule(EXPORT_FIELDS field, int length) {
            m_field = field;
            m_length = length;
        }

        /**
         * Validate.
         *
         * @param row Map<String,Object>
         * @see
         *      com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_field.getFieldId())) {
                String value = (String) row.get(m_field.getFieldId());

                if (isEmpty(value)) {
                    throwRow(row, SADE_EXCEPTIONS.PEN_2A);
                } else if (!isNumeric(value) || isFieldLengthExceed(value, m_length)) {
                    throwRow(row, SADE_EXCEPTIONS.PEN_2B);
                }
            }
        }
    }

    /**
     * Apply validation for SCHOOL ID field.
     */
    class SchoolIDRule extends IRule {
        private EXPORT_FIELDS m_field = null;
        private int m_length;

        /**
         * Instantiates a new school ID rule.
         *
         * @param field EXPORT_FIELDS
         * @param length int
         */
        public SchoolIDRule(EXPORT_FIELDS field, int length) {
            m_field = field;
            m_length = length;
        }

        /**
         * @see com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_field.getFieldId())) {
                String value = (String) row.get(m_field.getFieldId());

                if (isEmpty(value)) {
                    throwRow(row, SADE_EXCEPTIONS.SCHOOL_ID_1A);
                } else if (!isAlphanumeric(value) || isFieldLengthExceed(value, m_length)) {
                    throwRow(row, SADE_EXCEPTIONS.SCHOOL_ID_1B);
                }
            }
        }
    }

    /**
     * Apply validation for SCHOOL MARK TYPE and SCHOOL MARK VALUE fields.
     */
    class SchoolMarkRule extends IRule {
        private EXPORT_FIELDS m_schoolMarkTypeField = null;
        private EXPORT_FIELDS m_schoolMarkValueField = null;
        private EXPORT_FIELDS m_completionDateField = null;

        List<Object> m_schoolMarkTypeValidValues = Arrays.asList(new Object[] {"1", "2", "3", "4"});
        Map<String, ReferenceCode> m_gradeLevelMap = null;

        /**
         * Instantiates a new school mark rule.
         *
         * @param schoolMarkTypeField EXPORT_FIELDS
         * @param schoolMarkValueField EXPORT_FIELDS
         * @param completionDateField EXPORT_FIELDS
         */
        public SchoolMarkRule(EXPORT_FIELDS schoolMarkTypeField, EXPORT_FIELDS schoolMarkValueField,
                EXPORT_FIELDS completionDateField) {
            m_schoolMarkTypeField = schoolMarkTypeField;
            m_schoolMarkValueField = schoolMarkValueField;
            m_completionDateField = completionDateField;
        }

        /**
         * Sets the grade level map.
         *
         * @param gradeLevelMap Map<String,ReferenceCode>
         */
        public void setGradeLevelMap(Map<String, ReferenceCode> gradeLevelMap) {
            m_gradeLevelMap = gradeLevelMap;
        }

        /**
         * Validate.
         *
         * @param row Map<String,Object>
         * @see
         *      com.x2dev.reports.bc.StudentAchievementDataExceptionData.IRule#validate(java.util.Map)
         */
        @Override
        public void validate(Map<String, Object> row) {
            if (row.containsKey(m_schoolMarkTypeField.getFieldId()) &&
                    row.containsKey(m_schoolMarkValueField.getFieldId()) &&
                    row.containsKey(m_completionDateField.getFieldId())) {
                String schoolMarkType = (String) row.get(m_schoolMarkTypeField.getFieldId());
                String schoolMarkValue = (String) row.get(m_schoolMarkValueField.getFieldId());
                String completionDateValue = (String) row.get(m_completionDateField.getFieldId());

                if ((isEmpty(schoolMarkType) && !isEmpty(schoolMarkValue) && !isEmpty(completionDateValue)) ||
                        (isEmpty(schoolMarkType) && isEmpty(schoolMarkValue) && !isEmpty(completionDateValue)) ||
                        (isEmpty(schoolMarkType) && !isEmpty(schoolMarkValue) && isEmpty(completionDateValue)) ||
                        (!isEmpty(schoolMarkType) && isEmpty(schoolMarkValue) && !isEmpty(completionDateValue)) ||
                        (!isEmpty(schoolMarkType) && !isEmpty(schoolMarkValue) && isEmpty(completionDateValue)) ||
                        (!isEmpty(schoolMarkType) && isEmpty(schoolMarkValue) && isEmpty(completionDateValue))) {
                    throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_6B);
                } else if (!isEmpty(schoolMarkValue) && !isValueExist(schoolMarkType, m_schoolMarkTypeValidValues)) {
                    throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_6A);
                }

                if (!isEmpty(schoolMarkValue) && "1".equals(schoolMarkType)) {
                    if (m_gradeLevelMap != null && !m_gradeLevelMap.containsKey(schoolMarkValue)) {
                        throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7B);
                    }
                } else if (!isEmpty(schoolMarkValue) && "2".equals(schoolMarkType)) {
                    try {
                        int markValue = Integer.parseInt(schoolMarkValue);
                        if (markValue < 0 || markValue > 100) {
                            throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7D);
                        }
                    } catch (Exception ex) {
                        throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7D);
                    }
                } else if (!isEmpty(schoolMarkValue) && "3".equals(schoolMarkType)) {
                    try {
                        int markValue = Integer.parseInt(schoolMarkValue);
                        if (markValue < 1 || markValue > 3) {
                            throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7E);
                        }
                    } catch (Exception ex) {
                        throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7E);
                    }
                } else if (!isEmpty(schoolMarkValue) && "4".equals(schoolMarkType)) {
                    try {
                        int markValue = Integer.parseInt(schoolMarkValue);
                        if (markValue < 1 || markValue > 4) {
                            throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7F);
                        }
                    } catch (Exception ex) {
                        throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7F);
                    }
                }

                if (isFieldLengthExceed(schoolMarkValue, 3) && !isAlphanumeric(schoolMarkValue)) {
                    throwRow(row, SADE_EXCEPTIONS.SCHOOL_MARK_7G);
                }
            }
        }
    }

    /**
     * Abstract class with general methods for fields validation.
     */
    abstract class IRule {
        /*
         * All exceptions saved in those internal structure
         */
        private Map<EXCEPTION_TYPE, List<Map<String, Object>>> m_exceptionRows =
                new HashMap<StudentAchievementDataExceptionData.EXCEPTION_TYPE, List<Map<String, Object>>>();

        public static final String COL_EXCEPTION_TYPE = "colExceptionType";
        public static final String COL_EXCEPTION_MESSAGE = "colExceptionMessage";

        /**
         * Return true if exception appear and false otherwise.
         *
         * @param row Map<String,Object>
         */
        public abstract void validate(Map<String, Object> row);

        /**
         * Return exception list.
         *
         * @param exceptionType EXCEPTION_TYPE
         * @return List
         */
        public List<Map<String, Object>> getExceptionRowsByType(EXCEPTION_TYPE exceptionType) {
            return m_exceptionRows != null ? m_exceptionRows.get(exceptionType) : null;
        }

        /**
         * Returns all exception rows.
         *
         * @return List<Map<String, Object>>
         */
        public List<Map<String, Object>> getAllExceptionRows() {
            List<Map<String, Object>> allExceptionRows = new ArrayList<Map<String, Object>>();

            for (EXCEPTION_TYPE exceptionType : m_exceptionRows.keySet()) {
                allExceptionRows.addAll(m_exceptionRows.get(exceptionType));
            }

            return allExceptionRows;
        }

        /**
         * Set exception fields in a row and add them into exception list.
         *
         * @param rowTemp Map<String,Object>
         * @param exception SADE_EXCEPTIONS
         */
        protected void throwRow(Map<String, Object> rowTemp, SADE_EXCEPTIONS exception) {
            Map<String, Object> row = new HashMap<String, Object>();
            row.putAll(rowTemp);

            if (m_exceptionRows == null) {
                m_exceptionRows =
                        new HashMap<StudentAchievementDataExceptionData.EXCEPTION_TYPE, List<Map<String, Object>>>();
            }

            row.put(COL_EXCEPTION_TYPE, exception.getExceptionType().getExceptionTypeCode());
            row.put(COL_EXCEPTION_MESSAGE, exception.getExceptionMessage());

            if (!m_exceptionRows.containsKey(exception.getExceptionType())) {
                m_exceptionRows.put(exception.getExceptionType(), new ArrayList<Map<String, Object>>());
            }

            List<Map<String, Object>> exceptionRows = m_exceptionRows.get(exception.getExceptionType());
            exceptionRows.add(row);

            m_exceptionRows.put(exception.getExceptionType(), exceptionRows);
        }

        /**
         * Checks if the value is numeric.
         *
         * @param value String
         * @return boolean
         */
        protected boolean isNumeric(String value) {
            return value.matches("-?\\d+(\\.\\d+)?");
        }

        /**
         * Checks if the value is alpha or numeric.
         *
         * @param value String
         * @return boolean
         */
        protected boolean isAlphanumeric(String value) {
            return value.matches("[A-Za-z0-9]+");
        }

        /**
         * Checks if the value is empty. If the object is NULL it is considered empty. If the value
         * is a String then
         * empty string is also considered empty ("").
         *
         * @param value Object
         * @return boolean
         */
        protected boolean isEmpty(Object value) {
            boolean isEmpty = false;

            if (value == null) {
                isEmpty = true;
            } else if (value instanceof String && ((String) value).isEmpty()) {
                isEmpty = true;
            }

            return isEmpty;
        }

        /**
         * Returns true if the passed string is greater than the indicated length.
         *
         * @param value String
         * @param length int
         * @return boolean
         */
        protected boolean isFieldLengthExceed(String value, int length) {
            return (value != null && value.length() > length);
        }

        /**
         * Returns true if the passed value is included in the provided list.
         *
         * @param value Object
         * @param values List<Object>
         * @return boolean
         */
        protected boolean isValueExist(Object value, List<Object> values) {
            return values.contains(value);
        }
    }

    /**
     * Class encapsulate main validation logic - rules composition + exceptions returned.
     */
    public class Validator {
        List<IRule> m_rules = new ArrayList<IRule>();
        ReportDataGrid m_validationGrid = null;

        /**
         * Instantiates a new validator.
         *
         * @param grid ReportDataGrid
         */
        public Validator(ReportDataGrid grid) {
            m_validationGrid = grid;
        }

        /**
         * Validate.
         */
        public void validate() {
            for (Map<String, Object> row : m_validationGrid.getRows()) {
                for (IRule rule : m_rules) {
                    rule.validate(row);
                }
            }
        }

        /**
         * Adds the rule.
         *
         * @param rule IRule
         */
        public void addRule(IRule rule) {
            m_rules.add(rule);
        }

        /**
         * Gets the all exceptions for rules.
         *
         * @param ruleTypes RULE_TYPE[]
         * @return Report data grid
         */
        public ReportDataGrid getAllExceptionsForRules(RULE_TYPE[] ruleTypes) {
            ReportDataGrid grid = new ReportDataGrid();

            for (IRule rule : m_rules) {
                for (RULE_TYPE ruleType : ruleTypes) {
                    if (ruleType.getRuleClass().isInstance(rule)) {
                        List<Map<String, Object>> rows = rule.getAllExceptionRows();

                        if (rows != null && !rows.isEmpty()) {
                            grid.append(rule.getAllExceptionRows());
                        }
                    }
                }
            }

            return grid;
        }

        /**
         * Gets the exceptions for rules by type.
         *
         * @param ruleTypes RULE_TYPE[]
         * @param exceptionType EXCEPTION_TYPE
         * @return Report data grid
         */
        public ReportDataGrid getExceptionsForRulesByType(RULE_TYPE[] ruleTypes, EXCEPTION_TYPE exceptionType) {
            ReportDataGrid grid = new ReportDataGrid();

            for (IRule rule : m_rules) {
                for (RULE_TYPE ruleType : ruleTypes) {
                    if (ruleType.getRuleClass().isInstance(rule)) {
                        List<Map<String, Object>> rows = rule.getExceptionRowsByType(exceptionType);

                        if (rows != null && !rows.isEmpty()) {
                            grid.append(rule.getAllExceptionRows());
                        }
                    }
                }
            }

            return grid;
        }
    }

    /*
     * Columns
     */
    private static final String COL_SCHOOL = "school";
    private static final String COL_SUBREPORT_DATA = "subreportData";
    private static final String COL_SUBREPORT_FORMAT = "subreportFormat";
    private static final String COL_SUBREPORT_TYPE = "subreportType";

    /*
     * School Tallies
     */
    private static final String COL_DUPLICATE_RECORDS_STUDENT = "colDuplicatesStudent";
    private static final String COL_DUPLICATE_RECORDS_TOTAL = "colDuplicatesTotal";
    private static final String COL_ERROR_BASED_EXCEPTION_STUDENT = "colErrorBasedExceptionsStudent";
    private static final String COL_ERROR_BASED_EXCEPTION_TOTAL = "colErrorBasedExceptionsTotal";
    private static final String COL_INVALID_DATES_STUDENT = "colInvalidDatesStudent";
    private static final String COL_INVALID_DATES_TOTAL = "colInvalidDatesTotal";
    private static final String COL_WARNING_BASED_EXCEPTION_STUDENT = "colWarningBasedExceptionsStudent";
    private static final String COL_WARNING_BASED_EXCEPTION_TOTAL = "colWarningBasedExceptionsTotal";
    private static final String FIELD_CURRENT_END = "currentEndDate";
    private static final String FIELD_CURRENT_START = "currentStartDate";
    private static final String FIELD_PREVIOUS_END = "previousEndDate";
    private static final String FIELD_PREVIOUS_START = "previousStartDate";

    /*
     * Input parameters
     */
    private static final String PARAM_LIMIT_GRADES = "limitGrades";
    private static final String PARAM_PERIODS_INCLUDED = "includePeriod";
    private static final String PARAM_SORT_ORDER = "sortOrder";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    /*
     * Sub reports
     */
    private static final String SUBREPORT_ID_SADEE_EXCEPTIONS = "SADEER-E";
    private static final String SUBREPORT_ID_SADEE_DUPLICATE_RECORDS = "SADEER-DR";
    private static final String SUBREPORT_ID_SADEE_INVALID_DATES = "SADEER-ID";

    /*
     * Report parameters
     */
    private static final String PARAM_SCHOOL_COUNT_MAP = "schoolCountMap";
    private static final String PARAM_SCHOOL_DATES_MAP = "schoolDatesMap";

    /*
     * Members
     */
    private DistrictSchoolYearContext m_context;
    private SimpleDateFormat m_dateFormatter = new SimpleDateFormat("dd-MMM-yy");
    private boolean m_includePreviousDates;
    private Boolean m_limitGrades = Boolean.FALSE;
    private Collection<SisSchool> m_schools = null;
    private String[] m_sortOrder = null;

    private Report m_exceptionsSubreport;
    private Report m_duplicateRecordsSubreport;
    private Report m_invalidDatesSubreport;

    /*
     * General school settings
     */
    private Map<String, Map<String, Integer>> m_schoolCountsMap;
    private Map<String, Map<String, PlainDate>> m_schoolDatesMap;

    private String[] m_defaultSortOrder = new String[] {EXPORT_FIELDS.FIELD_PEN.getFieldId(),
            EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldId(),
            EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId(),
            EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER.getFieldId()};

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid exceptionReportGrid = new ReportDataGrid();

        for (SisSchool school : m_schools) {
            // Initialize school count map
            m_schoolCountsMap.put(school.getOid(), new HashMap<String, Integer>(32));
            m_schoolDatesMap.put(school.getOid(), new HashMap<String, PlainDate>(16));

            // Load SADE data for current school
            SADEReportDataGrid sadeExportGrid = getSADEGrid(school);
            Validator ruleEngine = getRuleEngine(sadeExportGrid);
            ruleEngine.validate();

            setSubreportData(ruleEngine, exceptionReportGrid, school, sadeExportGrid);
        }

        addParameter(PARAM_SCHOOL_COUNT_MAP, m_schoolCountsMap);
        addParameter(PARAM_SCHOOL_DATES_MAP, m_schoolDatesMap);

        exceptionReportGrid.beforeTop();
        return exceptionReportGrid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_context = getCurrentContext();
        m_schools = getSchools();

        m_schoolCountsMap = new HashMap<String, Map<String, Integer>>(m_schools.size());
        m_schoolDatesMap = new HashMap<String, Map<String, PlainDate>>(m_schools.size());

        String includedPeriods = (String) getParameter(PARAM_PERIODS_INCLUDED);
        if ("previous".equals(includedPeriods)) {
            m_includePreviousDates = true;
        } else {
            m_includePreviousDates = false;
        }

        if (getParameter(PARAM_LIMIT_GRADES) != null) {
            m_limitGrades = (Boolean) getParameter(PARAM_LIMIT_GRADES);
        }

        if (getParameter(PARAM_SORT_ORDER) != null) {
            m_sortOrder = getUserSortOrderAsStringArray((String) getParameter(PARAM_SORT_ORDER));
        } else {
            m_sortOrder = m_defaultSortOrder;
        }

        /*
         * Initialize subreport formats
         */
        m_exceptionsSubreport = ReportUtils.getReport(SUBREPORT_ID_SADEE_EXCEPTIONS, getBroker());
        m_duplicateRecordsSubreport = ReportUtils.getReport(SUBREPORT_ID_SADEE_DUPLICATE_RECORDS, getBroker());
        m_invalidDatesSubreport = ReportUtils.getReport(SUBREPORT_ID_SADEE_INVALID_DATES, getBroker());
    }

    /**
     * Iterate over the exceptions and add them in groups of 2500 to not go over the iReport
     * max-fill threshold.
     *
     *
     * @param grid ReportDataGrid
     * @param exceptions ReportDataGrid
     * @param format Report
     * @param type String
     * @param school SisSchool
     */
    private void appendExceptions(ReportDataGrid grid,
                                  ReportDataGrid exceptions,
                                  Report format,
                                  String type,
                                  SisSchool school) {
        AppGlobals.getLog().severe("SADE: Setting " + type + " exceptions. Size " + exceptions.rowCount());

        ReportDataGrid rowSubset = new ReportDataGrid();
        int count = 0;

        exceptions.beforeTop();
        while (exceptions.next()) {
            rowSubset.append(exceptions.getCurrentRow());
            count++;

            if (count % 1000 == 0 && exceptions.rowCount() > count) {
                AppGlobals.getLog().severe("SADE: Setting " + rowSubset.rowCount() + " exceptions.");
                rowSubset.beforeTop();

                grid.append();
                grid.set(COL_SUBREPORT_DATA, rowSubset);
                grid.set(COL_SUBREPORT_FORMAT, new ByteArrayInputStream(format.getCompiledFormat()));
                grid.set(COL_SUBREPORT_TYPE, type);
                grid.set(COL_SCHOOL, school);

                rowSubset = new ReportDataGrid();
                count = 0;
            }
        }

        AppGlobals.getLog().severe("SADE: Setting " + rowSubset.rowCount() + " exceptions.");
        rowSubset.beforeTop();

        grid.append();
        grid.set(COL_SUBREPORT_DATA, rowSubset);
        grid.set(COL_SUBREPORT_FORMAT, new ByteArrayInputStream(format.getCompiledFormat()));
        grid.set(COL_SUBREPORT_TYPE, type);
        grid.set(COL_SCHOOL, school);
    }

    /**
     * Determine the number of unique students in the grid.
     *
     * @param grid ReportDataGrid
     * @return int
     */
    private int calculateUniqueStudents(ReportDataGrid grid) {
        Set<String> uniqueStudentsSet = new HashSet<String>();

        for (Map<String, Object> row : grid.getRows()) {
            if (row.containsKey(EXPORT_FIELDS.FIELD_PEN.getFieldId())) {
                String penKey = (String) row.get(EXPORT_FIELDS.FIELD_PEN.getFieldId());

                if (penKey != null && !uniqueStudentsSet.contains(penKey)) {
                    uniqueStudentsSet.add(penKey);
                }
            }
        }

        return uniqueStudentsSet.size();
    }

    /**
     * Validator instantiation and configuration method.
     *
     * @param dataGrid SADEReportDataGrid
     * @return Validator
     */
    private Validator getRuleEngine(SADEReportDataGrid dataGrid) {
        Validator ruleValidator = new Validator(dataGrid);
        ruleValidator.addRule(new SchoolIDRule(EXPORT_FIELDS.FIELD_SCHOOL_ID, 8));
        ruleValidator.addRule(new PENRule(EXPORT_FIELDS.FIELD_PEN, 10));
        ruleValidator.addRule(new CourseCodeRule(EXPORT_FIELDS.FIELD_COURSE_CODE, 8));
        ruleValidator.addRule(
                new ActiveDateRule(EXPORT_FIELDS.FIELD_ACTIVE_DATE, m_context.getStartDate(), m_dateFormatter));
        ruleValidator.addRule(new DuplicateRecordRule(new EXPORT_FIELDS[] {EXPORT_FIELDS.FIELD_SCHOOL_ID,
                EXPORT_FIELDS.FIELD_PEN,
                EXPORT_FIELDS.FIELD_COURSE_CODE,
                EXPORT_FIELDS.FIELD_ACTIVE_DATE,
                EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER}));

        ruleValidator.addRule(new SchoolMarkRule(EXPORT_FIELDS.FIELD_SCHOOL_MARK_TYPE,
                EXPORT_FIELDS.FIELD_SCHOOL_MARK_VALUE, EXPORT_FIELDS.FIELD_COMPLETION_DATE));

        CompletionDateRule completionDateRule = new CompletionDateRule(EXPORT_FIELDS.FIELD_COMPLETION_DATE);
        completionDateRule.setFormatter(m_dateFormatter);

        /*
         * Set the date ranges for validation
         */
        Collection<KeyValuePair<PlainDate, PlainDate>> ranges = new LinkedList<KeyValuePair<PlainDate, PlainDate>>();
        KeyValuePair<PlainDate, PlainDate> current =
                new KeyValuePair<PlainDate, PlainDate>(dataGrid.getCurrentStartDate(), dataGrid.getCurrentEndDate());
        ranges.add(current);

        if (m_includePreviousDates) {
            KeyValuePair<PlainDate, PlainDate> previous = new KeyValuePair<PlainDate, PlainDate>(
                    dataGrid.getPreviousStartDate(), dataGrid.getPreviousEndDate());
            ranges.add(previous);
        }

        completionDateRule.setSubmissionRanges(ranges);

        ruleValidator.addRule(completionDateRule);
        ruleValidator.addRule(
                new CreditsRule(EXPORT_FIELDS.FIELD_CREDITS, Arrays.asList(new Object[] {"", "1", "2", "3", "4"})));

        return ruleValidator;
    }

    /**
     * Get SADE report grid.
     *
     * @param school SisSchool
     * @return SADE report data grid
     */
    private SADEReportDataGrid getSADEGrid(SisSchool school) {
        StudentContextReportHelper helper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
        SADEReportDataGrid sadeGrid = new StudentAchievementDataExtractData.SADEReportDataGrid(helper, getBroker());

        sadeGrid.initializeDateRanges(getOrganization());
        sadeGrid.setSchool(school);
        sadeGrid.setIncludePrevious(m_includePreviousDates);
        sadeGrid.setLimitGrades(m_limitGrades);
        sadeGrid.setLimitedGradeList(Arrays.asList(LIMITED_GRADES));
        sadeGrid.evaluateGrid();

        sadeGrid.sort(Arrays.asList(m_sortOrder), false);

        return sadeGrid;
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private Collection<SisSchool> getSchools() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
    }

    /**
     * Get all exception rows from validator and set appropriate data sections/fields in report and
     * set total
     * values for school.
     *
     * @param ruleEngine Validator
     * @param grid ReportDataGrid
     * @param school SisSchool
     * @param sadeExportGrid SADEReportDataGrid
     */
    private void setSubreportData(Validator ruleEngine,
                                  ReportDataGrid grid,
                                  SisSchool school,
                                  SADEReportDataGrid sadeExportGrid) {
        Map<String, Integer> schoolCounts = m_schoolCountsMap.get(school.getOid());
        Map<String, PlainDate> schoolDates = m_schoolDatesMap.get(school.getOid());

        /*
         * General exceptions
         */
        ReportDataGrid exceptionSubreportData = new ReportDataGrid();

        RULE_TYPE[] exceptionRules = new RULE_TYPE[] {RULE_TYPE.SCHOOL_ID_RULE,
                RULE_TYPE.PEN_RULE,
                RULE_TYPE.COURSE_CODE_RULE,
                RULE_TYPE.SCHOOL_MARK_RULE,
                RULE_TYPE.EQUIVALENCY_CHALLENGE_RULE,
                RULE_TYPE.CREDITS_RULE};

        RULE_TYPE[] duplicateRecordsRules = new RULE_TYPE[] {RULE_TYPE.DUPLICATE_RECORD_RULE};
        RULE_TYPE[] invalidDatesRules = new RULE_TYPE[] {RULE_TYPE.ACTIVE_DATE_RULE, RULE_TYPE.COMPLETION_DATE_RULE};

        ReportDataGrid exceptionsData = ruleEngine.getExceptionsForRulesByType(exceptionRules, EXCEPTION_TYPE.ERROR);
        ReportDataGrid warningData = ruleEngine.getExceptionsForRulesByType(exceptionRules, EXCEPTION_TYPE.WARNING);

        schoolCounts.put(COL_ERROR_BASED_EXCEPTION_STUDENT, Integer.valueOf(calculateUniqueStudents(exceptionsData)));
        schoolCounts.put(COL_ERROR_BASED_EXCEPTION_TOTAL, Integer.valueOf(exceptionsData.rowCount()));
        schoolCounts.put(COL_WARNING_BASED_EXCEPTION_STUDENT, Integer.valueOf(calculateUniqueStudents(warningData)));
        schoolCounts.put(COL_WARNING_BASED_EXCEPTION_TOTAL, Integer.valueOf(warningData.rowCount()));

        exceptionSubreportData.append(exceptionsData);
        exceptionSubreportData.append(warningData);

        appendExceptions(grid, exceptionSubreportData, m_exceptionsSubreport, "exceptions", school);

        /*
         * Duplicate records
         */
        ReportDataGrid duplciateRecordsSubreportData = ruleEngine.getAllExceptionsForRules(duplicateRecordsRules);

        schoolCounts.put(COL_DUPLICATE_RECORDS_STUDENT,
                Integer.valueOf(calculateUniqueStudents(duplciateRecordsSubreportData)));
        schoolCounts.put(COL_DUPLICATE_RECORDS_TOTAL, Integer.valueOf(duplciateRecordsSubreportData.rowCount()));

        appendExceptions(grid, duplciateRecordsSubreportData, m_duplicateRecordsSubreport, "duplicates", school);

        /*
         * Invalid dates
         */
        ReportDataGrid invalidDatesSubreportData = ruleEngine.getAllExceptionsForRules(invalidDatesRules);

        schoolCounts.put(COL_INVALID_DATES_STUDENT, Integer.valueOf(calculateUniqueStudents(invalidDatesSubreportData)));
        schoolCounts.put(COL_INVALID_DATES_TOTAL, Integer.valueOf(invalidDatesSubreportData.rowCount()));

        appendExceptions(grid, invalidDatesSubreportData, m_invalidDatesSubreport, "dates", school);

        /*
         * Additional reporting information
         */
        schoolDates.put(FIELD_CURRENT_END, sadeExportGrid.getCurrentEndDate());
        schoolDates.put(FIELD_CURRENT_START, sadeExportGrid.getCurrentStartDate());
        schoolDates.put(FIELD_PREVIOUS_END, sadeExportGrid.getPreviousEndDate());
        schoolDates.put(FIELD_PREVIOUS_START, sadeExportGrid.getPreviousStartDate());
    }
}
