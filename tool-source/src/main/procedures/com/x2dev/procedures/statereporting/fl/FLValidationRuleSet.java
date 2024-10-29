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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationError;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.LookupField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Field;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Field.ValueTransformation;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Operator;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLValidationRuleSet.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLValidationRuleSet implements FLValidationProcessor {

    /**
     * The Class RuleSet.
     */
    public static class RuleSet implements FLRuleSetValidationProcessor {
        private ValidationRule[] m_rules;
        private boolean m_skipIfValid = false;

        /**
         * Instantiates a new rule set.
         *
         * @param validationRules ValidationRule[]
         */
        public RuleSet(ValidationRule... validationRules) {
            this.m_rules = validationRules;
        }

        /**
         * Instantiates a new rule set.
         *
         * @param skipIfValid boolean
         * @param validationRules ValidationRule[]
         */
        public RuleSet(boolean skipIfValid, ValidationRule... validationRules) {
            this.m_rules = validationRules;
            m_skipIfValid = skipIfValid;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = ValidationResult.Valid();
            for (ValidationRule r : m_rules) {
                result = r.getValidationResult(helper, export, row);
                if (result.isSkipRule() || (m_skipIfValid && result.isOK())) {
                    continue;
                }
                break;
            }
            return result;
        }
    }

    /**
     * The Class ValidateExportsComparison.
     */
    public static class ValidateExportsComparison extends Restriction {

        /**
         * The Class ComparableFields.
         */
        public static class ComparableFields {
            private final String m_externalFieldName;
            private final String m_internalFieldName;
            private final Operator m_operator;
            private final Class<?> m_type;

            /**
             * Instantiates a new comparable fields.
             *
             * @param internalFieldName String
             * @param externalFieldName String
             * @param operator Operator
             * @param type Class<?>
             */
            public ComparableFields(String internalFieldName, String externalFieldName, Operator operator,
                    Class<?> type) {
                m_internalFieldName = internalFieldName;
                m_externalFieldName = externalFieldName;
                m_operator = operator;
                m_type = type;
            }

            /**
             * Gets the external field name.
             *
             * @return String
             */
            public String getExternalFieldName() {
                return m_externalFieldName;
            }

            /**
             * Gets the internal field name.
             *
             * @return String
             */
            public String getInternalFieldName() {
                return m_internalFieldName;
            }

            /**
             * Gets the operator.
             *
             * @return Operator
             */
            public Operator getOperator() {
                return m_operator;
            }

            /**
             * Gets the type.
             *
             * @return Class
             */
            public Class<?> getType() {
                return m_type;
            }
        }

        /**
         * The Enum Operator.
         */
        public static enum Operator {
            EQUALS, GREATER_THAN, GREATER_THAN_OR_EQUALS, LESS_THAN, LESS_THAN_OR_EQUALS, NOT_EQUALS
        }

        private String m_defaultDateFormat = "MMddyyyy";
        private KeyValuePair<String, String> m_keyFieldNamePair;
        private String m_procedure_id;

        private ComparableFields[] m_relatedFieldNamePairs;

        /**
         * Instantiates a new validate exports comparison.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs ComparableFields[]
         */
        public ValidateExportsComparison(String procedure_id, KeyValuePair<String, String> keyFieldNamePair,
                ComparableFields... relatedFieldNamePairs) {
            m_procedure_id = procedure_id;
            m_keyFieldNamePair = keyFieldNamePair;
            m_relatedFieldNamePairs = relatedFieldNamePairs;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param currentExport FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport currentExport,
                                                    ExportFormatRow row) {
            FLExport matchingExport = helper.getExportFromProcedureId(m_procedure_id);
            boolean isMatchFound = false;
            LookupField key = helper.new LookupField();
            LookupField lookupValue = helper.new LookupField();

            key.add(m_keyFieldNamePair.getValue());
            lookupValue.add(helper.getExportFormatRowFieldValue(row, currentExport, m_keyFieldNamePair.getKey()));
            Collection<ExportFormatRow> matchingExportRows =
                    helper.getExportFormatRows(matchingExport, key, lookupValue);

            String keyFieldName = m_keyFieldNamePair.getKey();
            String matchingKeyFieldName = m_keyFieldNamePair.getValue();
            String keyValue = helper.getExportFormatRowFieldValue(row, currentExport, keyFieldName);
            if (keyValue == null) {
                return ValidationResult.FieldNotFound(keyFieldName);
            }

            if (matchingExportRows != null) {
                for (ExportFormatRow matchingExportRow : matchingExportRows) {
                    String matchingKeyValue =
                            helper.getExportFormatRowFieldValue(matchingExportRow, matchingExport,
                                    matchingKeyFieldName);
                    if (matchingKeyValue == null) {
                        return ValidationResult.FieldNotFound(matchingKeyFieldName);
                    }

                    if (keyValue.contentEquals(matchingKeyValue)) {
                        isMatchFound = true;

                        try {
                            for (ComparableFields fieldNamePair : m_relatedFieldNamePairs) {
                                String value = helper.getExportFormatRowFieldValue(row, currentExport,
                                        fieldNamePair.getInternalFieldName());
                                if (value == null) {
                                    return ValidationResult.FieldNotFound(fieldNamePair.getInternalFieldName());
                                }
                                String comparableValue =
                                        helper.getExportFormatRowFieldValue(matchingExportRow, matchingExport,
                                                fieldNamePair.getExternalFieldName());
                                if (comparableValue == null) {
                                    return ValidationResult.FieldNotFound(fieldNamePair.getExternalFieldName());
                                }

                                // TODO: implement other types
                                if (fieldNamePair.getType() == Date.class) {
                                    SimpleDateFormat format = new SimpleDateFormat(m_defaultDateFormat);
                                    format.setLenient(false);


                                    Date _value = format.parse(value);
                                    Date _comparableValue = format.parse(comparableValue);

                                    switch (fieldNamePair.getOperator()) {
                                        // TODO: implement other operators
                                        case GREATER_THAN:
                                            if (!_value.after(_comparableValue)) {
                                                return ValidationResult
                                                        .FieldNotValid(fieldNamePair.getInternalFieldName(), value);
                                            }
                                            break;
                                        default:
                                            break;
                                    }

                                }
                            }
                        } catch (Exception e) {
                            return ValidationResult.InitError(null, null, e.getMessage());
                        }
                    }
                }
            }

            if (!isMatchFound) {
                return new ValidationResult(keyFieldName, keyValue, "", VALIDATION_STATUS.FIELD_NOT_VALID);
            }

            return super.getValidationResult(helper, currentExport, row);
        }
    }

    /**
     * The Class ValidateMatchInExport.
     */
    public static class ValidateMatchInExport extends Restriction {
        protected KeyValuePair<Object, String>[] m_relatedFieldNamePairs;
        private KeyValuePair<String, String> m_keyFieldNamePair;
        private int m_minNumberOfMatches = 0;

        private final boolean m_mustNotMatch;

        private String m_procedure_id;

        /**
         * Instantiates a new validate not match in export.
         *
         * @param mustNotMatch boolean
         * @param minNumberOfMatches
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateMatchInExport(boolean mustNotMatch, int minNumberOfMatches,
                String procedure_id,
                KeyValuePair<String, String> keyFieldNamePair,
                KeyValuePair<Object, String>... relatedFieldNamePairs) {
            m_procedure_id = procedure_id;
            m_keyFieldNamePair = keyFieldNamePair;
            m_relatedFieldNamePairs = relatedFieldNamePairs;

            m_mustNotMatch = mustNotMatch;
            m_minNumberOfMatches = minNumberOfMatches;
        }

        /**
         * Instantiates a new validate match at least minimum count in export.
         *
         * @param minNumberOfMatches int
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateMatchInExport(int minNumberOfMatches,
                String procedure_id,
                KeyValuePair<String, String> keyFieldNamePair,
                KeyValuePair<Object, String>... relatedFieldNamePairs) {
            m_procedure_id = procedure_id;
            m_keyFieldNamePair = keyFieldNamePair;
            m_relatedFieldNamePairs = relatedFieldNamePairs;

            m_mustNotMatch = false;
            m_minNumberOfMatches = minNumberOfMatches;
        }

        /**
         * Instantiates a new validate match in export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         */
        public ValidateMatchInExport(String procedure_id,
                KeyValuePair<String, String> keyFieldNamePair,
                KeyValuePair<Object, String>... relatedFieldNamePairs) {
            m_procedure_id = procedure_id;
            m_keyFieldNamePair = keyFieldNamePair;
            m_relatedFieldNamePairs = relatedFieldNamePairs;

            m_mustNotMatch = false;
            m_minNumberOfMatches = 1;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param currentExport FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport currentExport,
                                                    ExportFormatRow row) {

            FLExport matchingExport = helper.getExportFromProcedureId(m_procedure_id);
            int matchesFound = 0;
            LookupField key = helper.new LookupField();
            LookupField lookupValue = helper.new LookupField();

            key.add(m_keyFieldNamePair.getValue());
            lookupValue.add(helper.getExportFormatRowFieldValue(row, currentExport, m_keyFieldNamePair.getKey()));
            for (KeyValuePair<Object, String> pair : m_relatedFieldNamePairs) {
                if (pair.getKey() instanceof String) {
                    key.add(pair.getValue());
                    lookupValue.add(helper.getExportFormatRowFieldValue(row, currentExport, ((String) pair.getKey())));
                }
            }
            Collection<ExportFormatRow> matchingExportRows =
                    helper.getExportFormatRows(matchingExport, key, lookupValue);

            String keyFieldName = m_keyFieldNamePair.getKey();
            String matchingKeyFieldName = m_keyFieldNamePair.getValue();
            String keyValue = helper.getExportFormatRowFieldValue(row, currentExport, keyFieldName);
            if (keyValue == null) {
                return ValidationResult.FieldNotFound(keyFieldName);
            }

            if (matchingExportRows != null) {
                Iterator<ExportFormatRow> rowsIterator = matchingExportRows.iterator();
                while (rowsIterator.hasNext()) {
                    ExportFormatRow matchingExportRow = rowsIterator.next();
                    if (matchingExportRow.equals(row)) {
                        continue;
                    }
                    String matchingKeyValue =
                            helper.getExportFormatRowFieldValue(matchingExportRow, matchingExport,
                                    matchingKeyFieldName);
                    if (matchingKeyValue == null) {
                        return ValidationResult.FieldNotFound(matchingKeyFieldName);
                    }
                    if (keyValue.contentEquals(matchingKeyValue)) {
                        for (KeyValuePair<Object, String> fieldNamePair : m_relatedFieldNamePairs) {
                            if (fieldNamePair.getKey() instanceof String) {
                                String fieldName = fieldNamePair.getKey().toString();
                                String matchingFieldName = fieldNamePair.getValue();

                                String value = helper.getExportFormatRowFieldValue(row, currentExport, fieldName);
                                if (value == null) {
                                    return ValidationResult.FieldNotFound(fieldName);
                                }
                                String matchingValue =
                                        helper.getExportFormatRowFieldValue(matchingExportRow, matchingExport,
                                                matchingFieldName);
                                if (matchingValue == null) {
                                    return ValidationResult.FieldNotFound(matchingFieldName);
                                }

                                if (!value.contentEquals(matchingValue)) {
                                    return new ValidationResult(fieldName, value, "",
                                            VALIDATION_STATUS.FIELD_NOT_VALID);
                                }
                            } else if (fieldNamePair.getKey() instanceof Pattern) {
                                Pattern pattern = (Pattern) fieldNamePair.getKey();
                                String matchingFieldName = fieldNamePair.getValue();

                                String matchingValue =
                                        helper.getExportFormatRowFieldValue(matchingExportRow, matchingExport,
                                                matchingFieldName);
                                if (matchingValue == null) {
                                    return ValidationResult.FieldNotFound(matchingFieldName);
                                }

                                Matcher matcher = pattern.matcher(matchingValue);
                                if (!matcher.find() && !rowsIterator.hasNext()) {
                                    return new ValidationResult(matchingFieldName, matchingValue, "",
                                            VALIDATION_STATUS.FIELD_NOT_VALID);
                                }
                            } else if (fieldNamePair.getKey() instanceof Restriction) {
                                Restriction restriction = (Restriction) fieldNamePair.getKey();
                                ValidationResult validationResult =
                                        restriction.getValidationResult(helper, matchingExport, matchingExportRow);
                                if (validationResult.getStatus() != VALIDATION_STATUS.OK && !rowsIterator.hasNext()) {
                                    return validationResult;
                                }
                            }
                        }
                    }
                    break;
                }
                matchesFound++;
            }

            if (m_mustNotMatch && matchesFound > m_minNumberOfMatches) {
                return new ValidationResult(keyFieldName, keyValue, "", VALIDATION_STATUS.FIELD_NOT_VALID);
            }

            if (matchesFound < m_minNumberOfMatches) {
                return new ValidationResult(keyFieldName, keyValue, "", VALIDATION_STATUS.FIELD_NOT_VALID);
            }

            return super.getValidationResult(helper, currentExport, row);
        }
    }

    /**
     * The Interface FLRuleSetValidationProcessor.
     */
    static interface FLRuleSetValidationProcessor {

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         */
        ValidationResult getValidationResult(FLExportConfiguration helper, FLExport export, ExportFormatRow row);
    }

    /**
     * The Class Restriction.
     */
    static abstract class Restriction implements FLRuleSetValidationProcessor {

        /**
         * Always true.
         *
         * @return Restriction
         */
        public static Restriction alwaysTrue() {
            return new RestrictionTrue();
        }

        /**
         * Restriction And condition.
         *
         * @param restrictions Restriction[]
         * @return Restriction
         */
        public static Restriction and(Restriction... restrictions) {
            return new RestrictionAnd(restrictions);
        }

        /**
         * Check for value to be between two integers
         *
         * @param fieldName
         * @param from
         * @param to
         * @param includingFrom
         * @param includingTo
         * @return
         */
        public static Restriction between(String fieldName,
                                          int from,
                                          int to,
                                          boolean includingFrom,
                                          boolean includingTo) {
            return Restriction.and(
                    new RestrictionCompare(includingFrom ? Operator.GREATER_THAN_OR_EQUALS : Operator.GREATER_THAN,
                            fieldName, Double.valueOf(from)),
                    new RestrictionCompare(includingTo ? Operator.LESS_THAN_OR_EQUALS : Operator.LESS_THAN, fieldName,
                            Double.valueOf(to)));
        }

        /**
         * Restriction by active school.
         *
         * @param schoolNumberFieldName String
         * @return Restriction
         */
        public static Restriction byActiveSchool(String schoolNumberFieldName) {
            return new RestrictionByActiveSchool(schoolNumberFieldName);
        }

        /**
         * By alias fld ref table.
         *
         * @param referenceFieldAlias String
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction byAliasFldRefTable(String referenceFieldAlias, String... fieldNames) {
            return new RestrictionByAliasFieldReferenceTable(referenceFieldAlias, fieldNames);
        }

        /**
         * By date format.
         *
         * @param fieldName String
         * @return Restriction
         */
        public static Restriction byDateFormat(String fieldName) {
            return new RestrictionByDateFormat("MMddyyyy", fieldName);
        }

        /**
         * Restriction Pattern condition by date format.
         *
         * @param dateFormat String
         * @param fieldName String
         * @return Restriction
         */
        public static Restriction byDateFormat(String dateFormat, String fieldName) {
            return new RestrictionByDateFormat(dateFormat, fieldName);
        }

        /**
         * Restriction by extended field reference table.
         *
         * @param ddxId String
         * @param alias String
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction byExtFldRefTable(String ddxId, String alias, String... fieldNames) {
            return new RestrictionByExtendedFieldReferenceTable(ddxId, alias, fieldNames);
        }

        /**
         * By formatted reference codes of provided ref table.
         *
         * @param refTableName
         * @param formatId
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction byFormattedReferenceCodes(String refTableName,
                                                            String formatId,
                                                            String... fieldNames) {
            return new RestrictionByFormattedReferenceCodes(refTableName, formatId, fieldNames);
        }

        /**
         * By reference codes of provided ref table.
         *
         * @param refTableName
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction byReferenceCodes(String refTableName, String... fieldNames) {
            return new RestrictionByReferenceCodes(refTableName, fieldNames);
        }

        /**
         * Restriction Contains string condition.
         *
         * @param fieldName String
         * @param value String
         * @return Restriction
         */
        public static Restriction contains(String fieldName, String value) {
            return new RestrictionCompare(Operator.CONTAINS, fieldName, value);
        }

        /**
         * Restriction Contains field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @return Restriction
         */
        public static Restriction containsFieldValue(String fieldName, String comparableFieldName) {
            return new RestrictionCompare(Operator.CONTAINS, fieldName, new Field(comparableFieldName), String.class);
        }

        /**
         * Restriction Equals condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction equals(String fieldName, Object value) {
            return new RestrictionCompare(Operator.EQUALS, fieldName, value);
        }

        /**
         * Restriction Equals field value for fields condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction equalsFieldValue(String fieldName,
                                                   String comparableFieldName,
                                                   Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.EQUALS, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }

        /**
         * Grade level matches pattern.
         *
         * @param studentLocalIdFieldName String
         * @param expression String
         * @return Restriction
         */
        public static Restriction gradeLevelPattern(String studentLocalIdFieldName, String expression) {
            return new RestrictionGradeLevelPattern(studentLocalIdFieldName, expression);
        }

        /**
         * Restriction Greater Than condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction greaterThan(String fieldName, Object value) {
            return new RestrictionCompare(Operator.GREATER_THAN, fieldName, value);
        }

        /**
         * Restriction Greater Than field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction greaterThanFieldValue(String fieldName,
                                                        String comparableFieldName,
                                                        Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.GREATER_THAN, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }

        /**
         * Restriction Greater Than field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param dateFormat
         * @return Restriction
         */
        public static Restriction greaterThanFieldValue(String fieldName,
                                                        String comparableFieldName,
                                                        String dateFormat) {
            return new RestrictionCompare(Operator.GREATER_THAN, fieldName, new Field(comparableFieldName),
                    Date.class, dateFormat);
        }

        /**
         * Restriction Greater Than field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param dateFormat
         * @return Restriction
         */
        public static Restriction greaterThanOrEqualsFieldValue(String fieldName,
                                                                String comparableFieldName,
                                                                String dateFormat) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName, new Field(comparableFieldName),
                    Date.class, dateFormat);
        }

        /**
         * Greater than or equals.
         *
         * @param fieldName String
         * @param value Date
         * @param dateFormat String
         * @return Restriction
         */
        public static Restriction greaterThan(String fieldName, Date value, String dateFormat) {
            return new RestrictionCompare(Operator.GREATER_THAN, fieldName, value, dateFormat);
        }

        /**
         * Greater than or equals.
         *
         * @param fieldName String
         * @param value Date
         * @param dateFormat String
         * @return Restriction
         */
        public static Restriction greaterThanOrEquals(String fieldName, Date value, String dateFormat) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName, value, dateFormat);
        }

        /**
         * Restriction Greater Than Or Equals condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction greaterThanOrEquals(String fieldName, Object value) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName, value);
        }

        /**
         * Restriction Greater Than Or Equals condition.
         *
         * @param fieldName String
         * @param value Object
         * @param dateFormat
         * @return Restriction
         */
        public static Restriction greaterThanOrEquals(String fieldName, RuntimeParam value, String dateFormat) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName, value, dateFormat);
        }


        /**
         * Restriction Greater Than Or Equals field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction greaterThanOrEqualsFieldValue(String fieldName,
                                                                String comparableFieldName,
                                                                Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }

        /**
         * Greater than or equals year of date field.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @return Restriction
         */
        public static Restriction greaterThanOrEqualsYearOfDateField(String fieldName,
                                                                     String comparableFieldName) {
            return new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS, fieldName,
                    new Field(comparableFieldName, ValueTransformation.GET_LAST_4_SYMBOLS), Double.class);
        }

        /**
         * Validate job code fund source.
         *
         * @param fieldName String
         * @return Restriction
         */
        public static Restriction validateJobCodeFundSource(String fieldName) {
            return Restriction.or(
                    new RestrictionPattern("^0{12}$", fieldName),
                    new RestrictionPattern("^\\w.{3}00000000$", fieldName),
                    Restriction.and(
                            new RestrictionPattern("^\\w.{3}\\w.{3}0000$", fieldName),
                            new RestrictionCompare(Operator.NOT_EQUALS,
                                    new Field(fieldName, ValueTransformation.GET_FIRST_4_SYMBOLS),
                                    new Field(fieldName, ValueTransformation.GET_SECOND_4_SYMBOLS), String.class)),
                    Restriction.and(
                            new RestrictionPattern("^\\w.{3}\\w.{3}\\w.{3}$", fieldName),
                            new RestrictionCompare(Operator.NOT_EQUALS,
                                    new Field(fieldName, ValueTransformation.GET_FIRST_4_SYMBOLS),
                                    new Field(fieldName, ValueTransformation.GET_SECOND_4_SYMBOLS), String.class),
                            new RestrictionCompare(Operator.NOT_EQUALS,
                                    new Field(fieldName, ValueTransformation.GET_FIRST_4_SYMBOLS),
                                    new Field(fieldName, ValueTransformation.GET_LAST_4_SYMBOLS), String.class),
                            new RestrictionCompare(Operator.NOT_EQUALS,
                                    new Field(fieldName, ValueTransformation.GET_SECOND_4_SYMBOLS),
                                    new Field(fieldName, ValueTransformation.GET_LAST_4_SYMBOLS), String.class)));
        }

        /**
         * Restriction Less Than condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction lessThan(String fieldName, Object value) {
            return new RestrictionCompare(Operator.LESS_THAN, fieldName, value);
        }

        /**
         * Less than.
         *
         * @param fieldName String
         * @param value Date
         * @param dateFormat String
         * @return Restriction
         */
        public static Restriction lessThan(String fieldName, Date value, String dateFormat) {
            return new RestrictionCompare(Operator.LESS_THAN, fieldName, value, dateFormat);
        }

        /**
         * Less than.
         *
         * @param fieldName String
         * @param value Date
         * @param dateFormat String
         * @return Restriction
         */
        public static Restriction lessThan(String fieldName, RuntimeParam value, String dateFormat) {
            return new RestrictionCompare(Operator.LESS_THAN, fieldName, value, dateFormat);
        }

        /**
         * Restriction Less Than field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction lessThanFieldValue(String fieldName,
                                                     String comparableFieldName,
                                                     Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.LESS_THAN, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }

        /**
         * Less than or equals.
         *
         * @param fieldName String
         * @param value Date
         * @param dateFormat String
         * @return Restriction
         */
        public static Restriction lessThanOrEquals(String fieldName, Date value, String dateFormat) {
            return new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS, fieldName, value, dateFormat);
        }

        /**
         * Restriction Less Than Or Equals condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction lessThanOrEquals(String fieldName, Object value) {
            return new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS, fieldName, value);
        }

        /**
         * Restriction Less Than Or Equals field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction lessThanOrEqualsFieldValue(String fieldName,
                                                             String comparableFieldName,
                                                             Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }

        /**
         * Restriction Not Contains string condition.
         *
         * @param fieldName String
         * @param value String
         * @return Restriction
         */
        public static Restriction notContains(String fieldName, String value) {
            return new RestrictionCompare(Operator.NOT_CONTAINS, fieldName, value);
        }

        /**
         * Restriction Not Contains field value condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @return Restriction
         */
        public static Restriction notContainsFieldValue(String fieldName, String comparableFieldName) {
            return new RestrictionCompare(Operator.NOT_CONTAINS, fieldName, new Field(comparableFieldName),
                    String.class);
        }

        /**
         * Restriction Not Equals condition.
         *
         * @param fieldName String
         * @param value Object
         * @return Restriction
         */
        public static Restriction notEquals(String fieldName, Object value) {
            return new RestrictionCompare(Operator.NOT_EQUALS, fieldName, value);
        }

        /**
         * Restriction Not Equals field value for fields condition.
         *
         * @param fieldName String
         * @param comparableFieldName String
         * @param comparableFieldType Class<?>
         * @return Restriction
         */
        public static Restriction notEqualsFieldValue(String fieldName,
                                                      String comparableFieldName,
                                                      Class<?> comparableFieldType) {
            return new RestrictionCompare(Operator.NOT_EQUALS, fieldName, new Field(comparableFieldName),
                    comparableFieldType);
        }


        /**
         * Restriction Or condition.
         *
         * @param restrictions Restriction[]
         * @return Restriction
         */
        public static Restriction or(Restriction... restrictions) {
            return new RestrictionOr(restrictions);
        }

        /**
         * Restriction Pattern condition.
         *
         * @param fieldName String
         * @param pattern String
         * @return Restriction
         */
        public static Restriction pattern(String fieldName, String pattern) {
            return new RestrictionPattern(pattern, fieldName);
        }

        /**
         * Restriction Pattern condition for multiple fields.
         *
         * @param pattern String
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction patternForFields(String pattern, String... fieldNames) {
            return new RestrictionPattern(pattern, fieldNames);
        }

        /**
         * Restriction Pattern condition for all fields except from parameter fieldNames.
         *
         * @param pattern String
         * @param reverseFields
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction patternForFields(String pattern, boolean reverseFields, String... fieldNames) {
            return new RestrictionPattern(pattern, reverseFields, fieldNames);
        }

        /**
         * Student age in range.
         *
         * @param studentLocalIdFieldName String
         * @param fromAge Integer
         * @param toAge Integer
         * @param inclusiveFromAge boolean
         * @param inclusiveToAge boolean
         * @return Restriction
         */
        public static Restriction studentAgeInRange(String studentLocalIdFieldName,
                                                    Integer fromAge,
                                                    Integer toAge,
                                                    boolean inclusiveFromAge,
                                                    boolean inclusiveToAge) {
            return new RestrictionStudentAgeInRange(studentLocalIdFieldName, fromAge, toAge, inclusiveFromAge,
                    inclusiveToAge);
        }

        /**
         * Student age in range till date.
         *
         * @param studentLocalIdFieldName String
         * @param fromAge Integer
         * @param toAge Integer
         * @param inclusiveFromAge boolean
         * @param inclusiveToAge boolean
         * @param tillDate Date
         * @return Restriction
         */
        public static Restriction studentAgeInRange(String studentLocalIdFieldName,
                                                    Integer fromAge,
                                                    Integer toAge,
                                                    boolean inclusiveFromAge,
                                                    boolean inclusiveToAge,
                                                    Object tillDate) {
            return new RestrictionStudentAgeInRange(studentLocalIdFieldName, fromAge, toAge, inclusiveFromAge,
                    inclusiveToAge, tillDate);
        }

        /**
         * Restriction Sum Equals
         *
         * @param value Object
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction sumEquals(Double value, String... fieldNames) {
            return new RestrictionSumCompare(Operator.EQUALS, value, fieldNames);
        }

        /**
         * Restriction Sum Less Than
         *
         * @param value Object
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction sumGreaterThan(Double value, String... fieldNames) {
            return new RestrictionSumCompare(Operator.GREATER_THAN, value, fieldNames);
        }

        /**
         * Restriction Sum Less Than Or Equals condition.
         *
         * @param value Object
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction sumGreaterThanOrEquals(Double value, String... fieldNames) {
            return new RestrictionSumCompare(Operator.GREATER_THAN_OR_EQUALS, value, fieldNames);
        }

        /**
         * Restriction Sum Less Than
         *
         * @param value Object
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction sumLessThan(Double value, String... fieldNames) {
            return new RestrictionSumCompare(Operator.LESS_THAN, value, fieldNames);
        }

        /**
         * Restriction Sum Less Than Or Equals condition.
         *
         * @param value Object
         * @param fieldNames
         * @return Restriction
         */
        public static Restriction sumLessThanOrEquals(Double value, String... fieldNames) {
            return new RestrictionSumCompare(Operator.LESS_THAN_OR_EQUALS, value, fieldNames);
        }


        /**
         * Unique value.
         *
         * @param fieldNames String[]
         * @return Restriction
         */
        public static Restriction uniqueValue(String... fieldNames) {
            return new RestrictionUniqueValue(fieldNames);
        }

        /**
         * Validate exports comparison.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs ComparableFields
         * @return Restriction
         */
        public static Restriction validateExportsComparison(String procedure_id,
                                                            KeyValuePair<String, String> keyFieldNamePair,
                                                            ValidateExportsComparison.ComparableFields relatedFieldNamePairs) {
            return new ValidateExportsComparison(procedure_id, keyFieldNamePair, relatedFieldNamePairs);
        }

        /**
         * Validate match at least minimum count in export.
         *
         * @param minNumberOfMatches int
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         * @return Restriction
         */
        public static Restriction validateMatchInExport(int minNumberOfMatches,
                                                        String procedure_id,
                                                        KeyValuePair<String, String> keyFieldNamePair,
                                                        KeyValuePair<Object, String>... relatedFieldNamePairs) {
            return new ValidateMatchInExport(minNumberOfMatches, procedure_id, keyFieldNamePair, relatedFieldNamePairs);
        }

        /**
         * Validate match in export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         * @return Restriction
         */
        public static Restriction validateMatchInExport(String procedure_id,
                                                        KeyValuePair<String, String> keyFieldNamePair,
                                                        KeyValuePair<Object, String>... relatedFieldNamePairs) {
            return new ValidateMatchInExport(procedure_id, keyFieldNamePair, relatedFieldNamePairs);
        }

        /**
         * Validate not match in export.
         *
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         * @return Restriction
         */
        public static Restriction validateNotMatchInExport(String procedure_id,
                                                           KeyValuePair<String, String> keyFieldNamePair,
                                                           KeyValuePair<Object, String>... relatedFieldNamePairs) {
            return new ValidateMatchInExport(true, 0, procedure_id, keyFieldNamePair,
                    relatedFieldNamePairs);
        }

        /**
         * Validate not match in export.
         *
         * @param minNumberOfMatches int
         * @param procedure_id String
         * @param keyFieldNamePair KeyValuePair<String,String>
         * @param relatedFieldNamePairs KeyValuePair<Object,String>[]
         * @return Restriction
         */
        public static Restriction validateNotMatchInExport(int minNumberOfMatches,
                                                           String procedure_id,
                                                           KeyValuePair<String, String> keyFieldNamePair,
                                                           KeyValuePair<Object, String>... relatedFieldNamePairs) {
            return new ValidateMatchInExport(true, minNumberOfMatches, procedure_id, keyFieldNamePair,
                    relatedFieldNamePairs);
        }

        /**
         * Validate period number.
         *
         * @param periodNumberFieldName String
         * @return Restriction
         */
        public static Restriction validatePeriodNumber(String periodNumberFieldName) {
            return new ValidatePeriodNumber(periodNumberFieldName);
        }

        /**
         * Validate year entered ninth grade.
         *
         * @param yearEntered9GradeFieldName String
         * @param fiscalDate RuntimeParam
         * @return Restriction
         */
        public static Restriction validateYearEnteredNinthGrade(String yearEntered9GradeFieldName,
                                                                RuntimeParam fiscalDate) {
            return new ValidateYearEnteredNinthGrade(yearEntered9GradeFieldName, fiscalDate);
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            return ValidationResult.Valid();
        }
    }

    /**
     * The Class RestrictionAnd.
     */
    static class RestrictionAnd extends Restriction {
        private final Restriction[] m_restrictions;

        /**
         * Instantiates a new restriction and.
         *
         * @param restrictions Restriction[]
         */
        public RestrictionAnd(Restriction... restrictions) {
            this.m_restrictions = restrictions;
        }

        /**
         * Gets the restrictions.
         *
         * @return Restriction[]
         */
        public Restriction[] getRestrictions() {
            return m_restrictions;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);
            for (Restriction rp : m_restrictions) {
                result = rp.getValidationResult(helper, export, row);
                if (result.getStatus() != VALIDATION_STATUS.OK) {
                    break;
                }
            }
            return result;
        }
    }

    /**
     * The Class RestrictionByActiveSchool.
     */
    static class RestrictionByActiveSchool extends Restriction {
        private String m_fieldName;

        /**
         * Instantiates a new restriction by active school.
         *
         * @param schoolNumberFieldName String
         */
        public RestrictionByActiveSchool(String schoolNumberFieldName) {
            m_fieldName = schoolNumberFieldName;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {

            ValidationResult result = super.getValidationResult(helper, export, row);
            String schoolNumber = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
            if (schoolNumber == null) {
                result = ValidationResult.FieldNotFound(m_fieldName);
            } else {
                X2Criteria schoolCriteria = new X2Criteria();
                schoolCriteria.addEqualTo(SisSchool.COL_SCHOOL_ID, schoolNumber);
                schoolCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                SisSchool school = (SisSchool) helper.getBroker().getBeanByQuery(schoolQuery);
                if (school == null) {
                    result = ValidationResult.FieldNotValid(m_fieldName, schoolNumber);
                }
            }
            return result;
        }
    }

    /**
     * The Class RestrictionByAliasFieldReferenceTable.
     */
    static class RestrictionByAliasFieldReferenceTable extends Restriction {
        private String m_alias;
        private Set<String> m_codeSet;
        private String[] m_fieldNames;
        private String m_initError;
        private boolean m_initialized = false;

        /**
         * Instantiates a new restriction by alias field reference table.
         *
         * @param referenceFieldAlias String
         * @param fieldNames String[]
         */
        public RestrictionByAliasFieldReferenceTable(String referenceFieldAlias, String... fieldNames) {
            m_alias = referenceFieldAlias;
            m_fieldNames = fieldNames;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {

            ValidationResult result = super.getValidationResult(helper, export, row);
            initialize(helper);

            if (m_codeSet != null) {
                for (String fieldName : m_fieldNames) {
                    String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);
                    if (targetFieldValue == null) {
                        result = ValidationResult.FieldNotFound(fieldName);
                    } else {
                        if (!m_codeSet.contains(targetFieldValue)) {
                            result = ValidationResult.FieldNotValid(fieldName, targetFieldValue);
                        }
                    }
                }
            } else {
                result = ValidationResult.InitError(Arrays.toString(m_fieldNames), null, m_initError);
            }
            return result;
        }

        /**
         * Initialize.
         *
         * @param helper the helper
         */
        private void initialize(FLExportConfiguration helper) {
            if (!m_initialized) {
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(helper.getBroker().getPersistenceKey());
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(m_alias);
                if (field != null) {
                    if (!field.isEnabled()) {
                        m_initError = "Alias field " + m_alias + " is not enabled";
                    }
                    if (field.hasReferenceTable()) {
                        m_codeSet = new HashSet();
                        for (ReferenceCode item : field.getReferenceTable().getReferenceCodes(helper.getBroker())) {
                            m_codeSet.add(item.getStateCode());
                        }
                    } else {
                        m_initError = "Alias field " + m_alias + " has no reference table";
                    }
                } else {
                    m_initError =
                            "Alias field " + m_alias + " cannot be found";
                }
                m_initialized = true;
            }
        }
    }

    /**
     * The Class RestrictionByDateFormat.
     */
    static class RestrictionByDateFormat extends Restriction {
        private final String m_dateFormat;
        private final String m_fieldName;

        /**
         * Instantiates a new restriction by Date Format.
         *
         * @param dateFormat String
         * @param fieldName String
         */
        public RestrictionByDateFormat(String dateFormat, String fieldName) {
            this.m_fieldName = fieldName;
            this.m_dateFormat = dateFormat;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);
            String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
            if (targetFieldValue == null) {
                result = ValidationResult.FieldNotFound(m_fieldName);
            } else {
                SimpleDateFormat sdf = new SimpleDateFormat(m_dateFormat);
                sdf.setLenient(false);
                try {
                    sdf.parse(targetFieldValue);
                } catch (ParseException e) {
                    result = ValidationResult.FieldNotValid(m_fieldName, targetFieldValue);
                }
            }
            return result;
        }
    }

    /**
     * The Class RestrictionByExtendedFieldReferenceTable.
     */
    static class RestrictionByExtendedFieldReferenceTable extends Restriction {
        private String m_alias;
        private Set<String> m_codeSet;
        private String m_ddxId;
        private String[] m_fieldNames;
        private String m_initError;
        private boolean m_initialized = false;

        /**
         * Instantiates a new restriction by extended field reference table.
         *
         * @param ddxId String
         * @param alias String
         * @param fieldNames String[]
         */
        public RestrictionByExtendedFieldReferenceTable(String ddxId, String alias, String... fieldNames) {
            m_alias = alias;
            m_ddxId = ddxId;
            m_fieldNames = fieldNames;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {

            ValidationResult result = super.getValidationResult(helper, export, row);
            initialize(helper);

            if (m_codeSet != null) {
                for (String fieldName : m_fieldNames) {
                    String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);
                    if (targetFieldValue == null) {
                        result = ValidationResult.FieldNotFound(fieldName);
                    } else {
                        if (!m_codeSet.contains(targetFieldValue)) {
                            result = ValidationResult.FieldNotValid(fieldName, targetFieldValue);
                        }
                    }
                }
            } else {
                result = ValidationResult.InitError(Arrays.toString(m_fieldNames), null, m_initError);
            }
            return result;
        }

        /**
         * Initialize.
         *
         * @param helper the helper
         */
        private void initialize(FLExportConfiguration helper) {
            if (!m_initialized) {
                X2Criteria ddxCriteria = new X2Criteria();

                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, m_ddxId);

                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                ExtendedDataDictionary ddx = (ExtendedDataDictionary) helper.getBroker().getBeanByQuery(ddxQuery);
                if (ddx == null) {
                    m_initError = "Extended Dictionary " + m_ddxId + " could not be loaded";
                } else {
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(ddx, helper.getBroker().getPersistenceKey());
                    DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(m_alias);
                    if (field != null) {
                        if (!field.isEnabled()) {
                            m_initError = "Alias field " + m_alias + " for extended dictionary " + m_ddxId
                                    + " is not enabled";
                        }
                        if (field.hasReferenceTable()) {
                            m_codeSet = new HashSet();
                            for (ReferenceCode item : field.getReferenceTable().getReferenceCodes(helper.getBroker())) {
                                m_codeSet.add(item.getStateCode());
                            }
                        } else {
                            m_initError = "Alias field " + m_alias + " for extended dictionary " + m_ddxId
                                    + " has no reference table";
                        }
                    } else {
                        m_initError =
                                "Alias field " + m_alias + " for extended dictionary " + m_ddxId + " cannot be found";
                    }
                    m_initialized = true;
                }
            }
        }
    }

    /**
     * The Class RestrictionByFormattedReferenceCodes.
     */
    static class RestrictionByFormattedReferenceCodes extends Restriction {
        private Map<String, Set<String>> m_codeSets;
        private String[] m_fieldNames;
        private String m_formatId;
        private String m_initError;
        private boolean m_initialized = false;
        private String m_refTableName;

        /**
         * Instantiates a new restriction by formatted according to format of field (the field is
         * determined based on provided format procedure ID and field name) reference codes of
         * reference
         * table.
         *
         * @param refTableName String
         * @param formatId String
         * @param fieldName
         * @param formatFieldName String
         */
        public RestrictionByFormattedReferenceCodes(String refTableName, String formatId, String... fieldName) {
            m_refTableName = refTableName;
            m_fieldNames = fieldName;
            m_formatId = formatId;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {

            ValidationResult result = super.getValidationResult(helper, export, row);
            initialize(helper);

            if (m_codeSets != null) {
                for (String fieldName : m_fieldNames) {
                    Set<String> codes = m_codeSets.get(fieldName);
                    if (codes == null) {
                        return ValidationResult.InitError(fieldName, null, m_initError);
                    }
                    String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);
                    if (targetFieldValue == null) {
                        result = ValidationResult.FieldNotFound(fieldName);
                    } else {
                        if (!codes.contains(targetFieldValue)) {
                            result = ValidationResult.FieldNotValid(fieldName, targetFieldValue);
                        }
                    }
                }
            } else {
                result = ValidationResult.InitError(m_fieldNames[0], null, m_initError);
            }
            return result;
        }

        /**
         * Initialize.
         *
         * @param helper the helper
         */
        private void initialize(FLExportConfiguration helper) {
            if (!m_initialized) {
                X2Criteria refTableCriteria = new X2Criteria();
                refTableCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, m_refTableName);
                QueryByCriteria refTableQuery = new QueryByCriteria(ReferenceTable.class, refTableCriteria);
                ReferenceTable refTable = (ReferenceTable) helper.getBroker().getBeanByQuery(refTableQuery);

                if (refTable != null) {
                    m_codeSets = new HashMap<>();
                    for (String fieldName : m_fieldNames) {
                        Set<String> codes = m_codeSets.get(fieldName);
                        if (codes == null) {
                            codes = new HashSet<>();
                            m_codeSets.put(fieldName, codes);
                        }
                        for (ReferenceCode item : refTable.getReferenceCodes(helper.getBroker())) {
                            String formattedCode =
                                    helper.getFormattedValue(m_formatId, fieldName, item.getStateCode());
                            codes.add(formattedCode);
                        }
                    }
                } else {
                    m_initError =
                            "Reference table " + m_refTableName + " cannot be found";
                }
                m_initialized = true;
            }
        }
    }

    /**
     * The Class RestrictionByReferenceCodes.
     */
    static class RestrictionByReferenceCodes extends Restriction {
        private Set<String> m_codeSet;
        private String[] m_fieldNames;
        private String m_initError;
        private boolean m_initialized = false;
        private String m_refTableName;
        private ValueAdjuster m_valueAdjuster;

        /**
         * Instantiates a new restriction by reference table.
         *
         * @param refTableName
         * @param fieldNames String[]
         */
        public RestrictionByReferenceCodes(String refTableName, String... fieldNames) {
            m_refTableName = refTableName;
            m_fieldNames = fieldNames;
        }

        /**
         * Instantiates a new restriction by reference table using value adjuster.
         *
         * @param refTableName
         * @param valueAdjuster
         * @param fieldNames
         */
        public RestrictionByReferenceCodes(String refTableName, ValueAdjuster valueAdjuster, String... fieldNames) {
            m_refTableName = refTableName;
            m_fieldNames = fieldNames;
            m_valueAdjuster = valueAdjuster;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {

            if ("FL Faster Diseases".equals(m_refTableName) && m_fieldNames[0].equals("Dis Cond Type Code")) {
                System.out.println();
            }

            ValidationResult result = super.getValidationResult(helper, export, row);
            initialize(helper);

            if (m_codeSet != null) {
                for (String fieldName : m_fieldNames) {
                    String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);
                    if (targetFieldValue == null) {
                        result = ValidationResult.FieldNotFound(fieldName);
                    } else {
                        if (m_valueAdjuster != null) {
                            targetFieldValue = m_valueAdjuster.getAdjustedValue(targetFieldValue, helper);
                        }
                        if (!m_codeSet.contains(targetFieldValue)) {
                            result = ValidationResult.FieldNotValid(fieldName, targetFieldValue);
                        }
                    }
                }
            } else {
                result = ValidationResult.InitError(Arrays.toString(m_fieldNames), null, m_initError);
            }
            return result;
        }

        /**
         * Initialize.
         *
         * @param helper the helper
         */
        private void initialize(FLExportConfiguration helper) {
            if (!m_initialized) {
                X2Criteria refTableCriteria = new X2Criteria();
                refTableCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, m_refTableName);
                QueryByCriteria refTableQuery = new QueryByCriteria(ReferenceTable.class, refTableCriteria);
                ReferenceTable refTable = (ReferenceTable) helper.getBroker().getBeanByQuery(refTableQuery);

                if (refTable != null) {
                    m_codeSet = new HashSet();
                    for (ReferenceCode item : refTable.getReferenceCodes(helper.getBroker())) {
                        m_codeSet.add(item.getStateCode());
                    }
                } else {
                    m_initError =
                            "Reference table " + m_refTableName + " cannot be found";
                }
                m_initialized = true;
            }
        }
    }

    /**
     * The Class RestrictionCompare.
     */
    static class RestrictionCompare extends Restriction {

        /**
         * The Enum Operator.
         */
        public static enum Operator {
            CONTAINS, CONTENT_EQUALS, CONTENT_NOT_EQUALS, EQUALS, GREATER_THAN, GREATER_THAN_OR_EQUALS, LESS_THAN, LESS_THAN_OR_EQUALS, NOT_CONTAINS, NOT_EQUALS
        }

        /**
         * The Class Field.
         */
        static class Field {

            /**
             * The Enum ValueTransformation.
             */
            public static enum ValueTransformation {
                GET_FIRST_4_SYMBOLS, GET_SECOND_4_SYMBOLS, GET_LAST_4_SYMBOLS
            }

            private final String m_name;
            private final ValueAdjuster m_valueAdjuster;
            private final ValueTransformation m_valueTransformation;

            /**
             * Instantiates a new field.
             *
             * @param fieldName String
             */
            public Field(String fieldName) {
                m_name = fieldName;
                m_valueAdjuster = null;
                m_valueTransformation = null;
            }

            /**
             * Instantiates a new field.
             *
             * @param fieldName String
             * @param valueAdjuster
             */
            public Field(String fieldName, ValueAdjuster valueAdjuster) {
                m_name = fieldName;
                m_valueAdjuster = valueAdjuster;
                m_valueTransformation = null;
            }

            /**
             * Instantiates a new field.
             *
             * @param fieldName String
             * @param valueTransformation ValueTransformation
             */
            public Field(String fieldName, ValueTransformation valueTransformation) {
                m_name = fieldName;
                m_valueAdjuster = null;
                m_valueTransformation = valueTransformation;
            }

            public String getAdjustedValue(String value, FLExportConfiguration helper) {
                if (m_valueAdjuster != null) {
                    value = m_valueAdjuster.getAdjustedValue(value, helper);
                }

                return value;
            }

            /**
             * Gets the name.
             *
             * @return String
             */
            public String getName() {
                return m_name;
            }

            /**
             * Gets the transformated value.
             *
             * @param value String
             * @return String
             */
            public String getTransformatedValue(String value) {
                switch (m_valueTransformation) {
                    case GET_FIRST_4_SYMBOLS:
                        value = value.length() >= 4 ? value.substring(0, 4) : value;
                        break;
                    case GET_SECOND_4_SYMBOLS:
                        value = value.length() >= 8 ? value.substring(4, 8) : value;
                        break;
                    case GET_LAST_4_SYMBOLS:
                        value = value.length() >= 4 ? value.substring(value.length() - 4) : value;
                        break;
                    default:
                        break;
                }

                return value;
            }

            /**
             * Ge value transformation.
             *
             * @return ValueTransformation
             */
            public ValueTransformation geValueTransformation() {
                return m_valueTransformation;
            }
        }

        /**
         * The Enum Mode.
         */
        private enum Mode {
            MODE_COMPARE_WITH_DATE, MODE_COMPARE_WITH_DOUBLE, MODE_COMPARE_WITH_STRING
        }

        public interface ValueAdjuster {
            public String getAdjustedValue(String value, FLExportConfiguration helper);
        }

        private final Operator m_operator;
        private Class<?> m_comparableFieldType;

        private final Object m_targetField;
        private Object m_comparableValue;

        private final String m_dateFormat;
        private final String m_defaultDateFormat = "MMddyyyy";
        private final boolean m_ignoreCase;

        private Mode m_mode;

        /**
         * Instantiates a new restriction compare with some value of Date type.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableValue Date
         * @param dateFormat String
         */
        public RestrictionCompare(Operator operator, String fieldName, Date comparableValue, String dateFormat) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableValue;

            m_dateFormat = dateFormat;
            m_ignoreCase = false;
        }

        /**
         * Instantiates a new restriction compare with some value of Date type.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableValue Date
         * @param dateFormat String
         */
        public RestrictionCompare(Operator operator, String fieldName, RuntimeParam comparableValue,
                String dateFormat) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableValue;

            m_dateFormat = dateFormat;
            m_ignoreCase = false;
        }


        /**
         * Instantiates a new restriction compare with another field value.
         *
         * @param operator Operator
         * @param field Field
         * @param comparableField Field
         * @param comparableFieldType Class<?>
         */
        public RestrictionCompare(Operator operator, Field field, Field comparableField,
                Class<?> comparableFieldType) {
            m_operator = operator;
            m_targetField = field;
            m_comparableValue = comparableField;
            m_comparableFieldType = comparableFieldType;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = false;
        }

        /**
         * Instantiates a new restriction compare with another field value.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableField Field
         * @param comparableFieldType Class<?>
         */
        public RestrictionCompare(Operator operator, String fieldName, Field comparableField,
                Class<?> comparableFieldType) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableField;
            m_comparableFieldType = comparableFieldType;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = false;
        }


        /**
         * Instantiates a new restriction compare with another field value of Date type.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableField Field
         * @param comparableFieldType Class<Date>
         * @param dateFormat String
         */
        public RestrictionCompare(Operator operator, String fieldName, Field comparableField,
                Class<Date> comparableFieldType, String dateFormat) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableField;
            m_comparableFieldType = comparableFieldType;

            m_dateFormat = dateFormat;
            m_ignoreCase = false;
        }


        /**
         * Instantiates a new restriction compare with another field value of String type.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableField Field
         * @param comparableFieldType Class<String>
         * @param ignoreCase boolean
         */
        public RestrictionCompare(Operator operator, String fieldName, Field comparableField,
                Class<String> comparableFieldType, boolean ignoreCase) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableField;
            m_comparableFieldType = comparableFieldType;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = ignoreCase;
        }


        /**
         * Instantiates a new restriction compare with some value.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableValue Object
         */
        public RestrictionCompare(Operator operator, String fieldName, Object comparableValue) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableValue;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = false;
        }

        /**
         * Instantiates a new restriction compare with some value.
         *
         * @param operator Operator
         * @param field
         * @param comparableValue Object
         */
        public RestrictionCompare(Operator operator, Field field, Object comparableValue) {
            m_operator = operator;
            m_targetField = field;
            m_comparableValue = comparableValue;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = false;
        }

        /**
         * Instantiates a new restriction compare with some value of String type.
         *
         * @param operator Operator
         * @param fieldName String
         * @param comparableValue String
         * @param ignoreCase boolean
         */
        public RestrictionCompare(Operator operator, String fieldName, String comparableValue, boolean ignoreCase) {
            m_operator = operator;
            m_targetField = fieldName;
            m_comparableValue = comparableValue;

            m_dateFormat = m_defaultDateFormat;
            m_ignoreCase = ignoreCase;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            String targetFieldName = null;
            Object targetValue = null;
            try {
                // Define target value according to type
                if (m_targetField instanceof Field) {
                    Field targetField = (Field) m_targetField;

                    targetFieldName = targetField.getName();
                    targetValue = helper.getExportFormatRowFieldValue(row, export, targetField.getName());
                    targetValue = targetField.getAdjustedValue((String) targetValue, helper);
                    // Implement field value transformation
                    if (targetField.geValueTransformation() != null) {
                        targetValue = targetField.getTransformatedValue((String) targetValue);
                    }
                } else if (m_targetField instanceof String) {
                    targetFieldName = (String) m_targetField;
                    targetValue = helper.getExportFormatRowFieldValue(row, export, targetFieldName);
                }

                if (targetValue == null) {
                    return ValidationResult.FieldNotFound(targetFieldName);
                }


                Object comparableValue = m_comparableValue;

                // Extract value from runtime parameter
                if (comparableValue instanceof RuntimeParam) {
                    comparableValue = ((RuntimeParam) comparableValue).getRuntimeObject(helper);
                }

                // Define comparable value according to type
                if (comparableValue instanceof Field) {
                    Field comparableField = (Field) comparableValue;

                    comparableValue = helper.getExportFormatRowFieldValue(row, export, comparableField.getName());
                    if (comparableValue == null) {
                        return ValidationResult.FieldNotFound(comparableField.getName());
                    }

                    comparableValue = comparableField.getAdjustedValue((String) comparableValue, helper);
                    // Implement field value transformation
                    if (comparableField.geValueTransformation() != null) {
                        comparableValue = comparableField.getTransformatedValue((String) comparableValue);
                    }

                    if (m_comparableFieldType == Date.class) {
                        SimpleDateFormat format = new SimpleDateFormat(m_dateFormat);
                        format.setLenient(false);
                        comparableValue = format.parse(comparableValue.toString());
                    } else if (m_comparableFieldType == Double.class) {
                        comparableValue = Double.valueOf(Double.parseDouble(comparableValue.toString()));
                    } else if (m_comparableFieldType == String.class) {
                        comparableValue = comparableValue.toString();
                    } else {
                        return ValidationResult.InitError(comparableField.getName(), comparableValue.toString(),
                                "Unsupported comparable field " + comparableField.getName() + " value class type.");
                    }
                }

                // Define target value according to type
                if (comparableValue instanceof Date) {
                    m_mode = Mode.MODE_COMPARE_WITH_DATE;
                    String value = targetValue.toString();
                    if (value.contentEquals("00000000") || value.isEmpty()) {
                        return ValidationResult.FieldNotValid((String) m_targetField, value);
                    }
                    SimpleDateFormat format = new SimpleDateFormat(m_dateFormat);
                    format.setLenient(false);
                    targetValue = format.parse(value);
                } else if (comparableValue instanceof Double) {
                    m_mode = Mode.MODE_COMPARE_WITH_DOUBLE;

                    targetValue = Double.valueOf(Double.parseDouble(targetValue.toString()));
                } else if (comparableValue instanceof String) {
                    m_mode = Mode.MODE_COMPARE_WITH_STRING;

                    targetValue = targetValue.toString();
                    if (m_ignoreCase) {
                        targetValue = targetValue.toString().toLowerCase();
                        comparableValue = comparableValue.toString().toLowerCase();
                    }
                } else {
                    return ValidationResult.InitError(targetFieldName, String.valueOf(comparableValue),
                            "Unsupported comparable field value class type.");
                }


                // Target and comparable values comparison
                switch (m_operator) {
                    case EQUALS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (!((Date) targetValue).equals(comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (((Double) targetValue).doubleValue() != ((Double) comparableValue).doubleValue()) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_STRING:
                                if (!((String) targetValue).contentEquals((String) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case NOT_EQUALS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (((Date) targetValue).equals(comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (((Double) targetValue).doubleValue() == ((Double) comparableValue).doubleValue()) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_STRING:
                                if (((String) targetValue).contentEquals((String) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case CONTAINS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_STRING:
                                if (!((String) targetValue).contains((String) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case NOT_CONTAINS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_STRING:
                                if (((String) targetValue).contains((String) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case GREATER_THAN:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (!((Date) targetValue).after((Date) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (!(((Double) targetValue).doubleValue() > ((Double) comparableValue)
                                        .doubleValue())) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case GREATER_THAN_OR_EQUALS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (!(((Date) targetValue).after((Date) comparableValue)
                                        || ((Date) targetValue).equals(comparableValue))) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (!(((Double) targetValue).doubleValue() >= ((Double) comparableValue)
                                        .doubleValue())) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case LESS_THAN:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (!((Date) targetValue).before((Date) comparableValue)) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (!(((Double) targetValue).doubleValue() < ((Double) comparableValue)
                                        .doubleValue())) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    case LESS_THAN_OR_EQUALS:
                        switch (m_mode) {
                            case MODE_COMPARE_WITH_DATE:
                                if (!(((Date) targetValue).before((Date) comparableValue)
                                        || ((Date) targetValue).equals(comparableValue))) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            case MODE_COMPARE_WITH_DOUBLE:
                                if (!(((Double) targetValue).doubleValue() <= ((Double) comparableValue)
                                        .doubleValue())) {
                                    return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
                                }
                                break;
                            default:
                                break;
                        }
                        break;
                    default:
                        break;
                }
            } catch (Exception e) {
                return ValidationResult.FieldNotValid(targetFieldName, targetValue.toString());
            }

            return super.getValidationResult(helper, export, row);
        }

        /**
         * Sets the comparable value.
         *
         * @param value void
         */
        public void setComparableValue(Object value) {
            m_comparableValue = value;
        }
    }

    /**
     * The Class RestrictionGradeLevelPattern.
     */
    static class RestrictionGradeLevelPattern extends Restriction {
        private String m_fieldName;
        private Pattern m_pattern;

        /**
         * Instantiates a new restriction grade level equals.
         *
         * @param studentLocalIdFieldName String
         * @param expression Pattern
         */
        public RestrictionGradeLevelPattern(String studentLocalIdFieldName, String expression) {
            m_fieldName = studentLocalIdFieldName;
            m_pattern = Pattern.compile(expression);
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            String studentLocalId = helper.getExportFormatRowFieldValue(row, export, m_fieldName);

            if (studentLocalId == null) {
                return ValidationResult.FieldNotFound(m_fieldName);
            }
            studentLocalId = studentLocalId.trim();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStudent.COL_LOCAL_ID, studentLocalId);
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

            SisStudent student = (SisStudent) helper.getBroker().getBeanByQuery(query);
            if (student == null) {
                return ValidationResult.FieldNotFound(m_fieldName);
            }

            String gradeLevel = student.getGradeLevel();

            Matcher matcher = m_pattern.matcher(gradeLevel);
            if (!matcher.find()) {
                return ValidationResult.FieldNotValid(SisStudent.COL_GRADE_LEVEL, gradeLevel);
            }

            return super.getValidationResult(helper, export, row);
        }
    }

    /**
     * The Class RestrictionOr.
     */
    static class RestrictionOr extends Restriction {
        private final Restriction[] m_restrictions;

        /**
         * Instantiates a new restriction or.
         *
         * @param restrictions Restriction[]
         */
        public RestrictionOr(Restriction... restrictions) {
            this.m_restrictions = restrictions;
        }

        /**
         * Gets the restrictions.
         *
         * @return Restriction[]
         */
        public Restriction[] getRestrictions() {
            return m_restrictions;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);
            for (Restriction rp : m_restrictions) {
                result = rp.getValidationResult(helper, export, row);
                if (result.getStatus() == VALIDATION_STATUS.OK) {
                    break;
                }
            }
            return result;
        }
    }

    /**
     * The Class RestrictionPattern.
     */
    static class RestrictionPattern extends Restriction {
        private final String[] m_fieldNames;
        private final Pattern m_fieldPattern;
        private final boolean m_reverseFields;

        /**
         * Instantiates a new restriction pattern.
         *
         * @param pattern String
         * @param fieldNames String[]
         */
        public RestrictionPattern(String pattern, String... fieldNames) {
            this.m_fieldNames = fieldNames;
            this.m_fieldPattern = Pattern.compile(pattern);
            m_reverseFields = false;
        }

        /**
         * Instantiates a new restriction pattern.
         *
         * @param pattern String
         * @param reverseFields
         * @param fieldNames String[]
         */
        public RestrictionPattern(String pattern, boolean reverseFields, String... fieldNames) {
            this.m_fieldNames = fieldNames == null ? new String[0] : fieldNames;
            this.m_fieldPattern = Pattern.compile(pattern);
            m_reverseFields = reverseFields;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);
            List<String> fieldNames = new ArrayList<>();

            if (m_reverseFields) {
                ExportFormatDefinition efd = helper.getExportFormatDefinition(export);
                List<ExportFormatField> fieldsList = new ArrayList<>(efd.getFields());
                for (ExportFormatField field : fieldsList) {
                    fieldNames.add(field.getName());
                }
                fieldNames.removeAll(Arrays.asList(m_fieldNames));
            } else {
                fieldNames.addAll(Arrays.asList(m_fieldNames));
            }

            for (String fieldName : fieldNames) {
                String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);

                if (targetFieldValue == null) {
                    result = ValidationResult.FieldNotFound(fieldName);
                } else {
                    if (!m_fieldPattern.matcher(targetFieldValue).find()) {
                        result = ValidationResult.FieldNotValid(fieldName, targetFieldValue);
                    }
                }
            }
            return result;
        }
    }

    /**
     * The Class RestrictionStudentAgeInRange.
     */
    static class RestrictionStudentAgeInRange extends Restriction {
        private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("MMddyyyy");
        private String m_fieldName;
        private Integer m_fromAge;
        private boolean m_inclusiveFromAge;
        private boolean m_inclusiveToAge;
        private final Object m_tillDate;
        private Integer m_toAge;


        /**
         * Instantiates a new restriction student age in range.
         *
         * @param studentLocalIdFieldName String
         * @param fromAge Integer
         * @param toAge Integer
         * @param inclusiveFromAge boolean
         * @param inclusiveToAge boolean
         */
        public RestrictionStudentAgeInRange(String studentLocalIdFieldName, Integer fromAge, Integer toAge,
                boolean inclusiveFromAge, boolean inclusiveToAge) {
            m_fieldName = studentLocalIdFieldName;
            m_fromAge = fromAge;
            m_toAge = toAge;
            m_inclusiveFromAge = inclusiveFromAge;
            m_inclusiveToAge = inclusiveToAge;
            m_tillDate = new Date();
        }

        /**
         * Instantiates a new restriction student age in range till date.
         *
         * @param studentLocalIdFieldName String
         * @param fromAge Integer
         * @param toAge Integer
         * @param inclusiveFromAge boolean
         * @param inclusiveToAge boolean
         * @param tillDate Date
         */
        public RestrictionStudentAgeInRange(String studentLocalIdFieldName, Integer fromAge, Integer toAge,
                boolean inclusiveFromAge, boolean inclusiveToAge, Object tillDate) {
            m_fieldName = studentLocalIdFieldName;
            m_fromAge = fromAge;
            m_toAge = toAge;
            m_inclusiveFromAge = inclusiveFromAge;
            m_inclusiveToAge = inclusiveToAge;
            m_tillDate = tillDate;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            String studentLocalId = helper.getExportFormatRowFieldValue(row, export, m_fieldName);

            if (studentLocalId == null) {
                return ValidationResult.FieldNotFound(m_fieldName);
            }

            studentLocalId = studentLocalId.trim();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStudent.COL_LOCAL_ID, studentLocalId);
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

            SisStudent student = (SisStudent) helper.getBroker().getBeanByQuery(query);
            if (student == null) {
                return ValidationResult.FieldNotFound(m_fieldName);
            }

            PlainDate dob = student.getPerson().getDob();
            Object tillDateObj = m_tillDate;
            if (tillDateObj instanceof RuntimeParam) {
                tillDateObj = ((RuntimeParam) tillDateObj).getRuntimeObject(helper);
            } else if (tillDateObj instanceof String) {
                String sDate = helper.getExportFormatRowFieldValue(row, export, (String) tillDateObj);
                try {
                    tillDateObj = DATE_FORMAT.parse(sDate);
                } catch (ParseException e) {
                    return ValidationResult.FieldNotValid(m_fieldName, sDate);
                }
            }

            Date tillDate = (Date) tillDateObj;

            int studentAgeInYears = getDiffYears(dob, tillDate);
            String studentAgeString = Integer.toString(studentAgeInYears);

            if (m_toAge == null) {
                if (m_inclusiveFromAge) {
                    if (!(studentAgeInYears >= m_fromAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                } else {
                    if (!(studentAgeInYears > m_fromAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                }
            } else if (m_fromAge == null) {
                if (m_inclusiveToAge) {
                    if (!(studentAgeInYears <= m_toAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                } else {
                    if (!(studentAgeInYears < m_toAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                }
            } else {
                if (m_inclusiveFromAge) {
                    if (!(studentAgeInYears >= m_fromAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                } else {
                    if (!(studentAgeInYears > m_fromAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                }

                if (m_inclusiveToAge) {
                    if (!(studentAgeInYears <= m_toAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                } else {
                    if (!(studentAgeInYears < m_toAge.intValue())) {
                        return ValidationResult.FieldNotValid(m_fieldName, studentAgeString);
                    }
                }
            }

            return super.getValidationResult(helper, export, row);
        }

        /**
         * Gets the calendar.
         *
         * @param date Date
         * @return Calendar
         */
        private Calendar getCalendar(Date date) {
            Calendar cal = Calendar.getInstance(Locale.US);
            cal.setTime(date);
            return cal;
        }

        /**
         * Gets the diff years.
         *
         * @param first Date
         * @param last Date
         * @return int
         */
        private int getDiffYears(Date first, Date last) {
            Calendar a = getCalendar(first);
            Calendar b = getCalendar(last);
            int diff = b.get(Calendar.YEAR) - a.get(Calendar.YEAR);
            if (a.get(Calendar.MONTH) > b.get(Calendar.MONTH) ||
                    (a.get(Calendar.MONTH) == b.get(Calendar.MONTH) && a.get(Calendar.DATE) > b.get(Calendar.DATE))) {
                diff--;
            }
            return diff;
        }
    }

    /**
     * The Class RestrictionStudentAgeInRange.
     */
    static class RestrictionSumCompare extends Restriction {
        Double m_comparableValue;
        private String[] m_fieldNames;
        private Operator m_operator;

        /**
         * Instantiates a new sum restriction compare with some value of Double type.
         *
         * @param operator Operator
         * @param comparableValue Double
         * @param fieldNames String[]
         */
        public RestrictionSumCompare(Operator operator, Double comparableValue, String... fieldNames) {
            m_operator = operator;
            m_fieldNames = fieldNames;
            m_comparableValue = comparableValue;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            double sum = 0;
            String name = "";
            if (m_fieldNames != null && m_fieldNames.length > 0) {
                for (String fieldName : m_fieldNames) {

                    String sValue = helper.getExportFormatRowFieldValue(row, export, fieldName);

                    if (sValue == null) {
                        return ValidationResult.FieldNotFound(fieldName);
                    }
                    try {
                        sum += Double.parseDouble(sValue);
                    } catch (Exception ex) {
                        return ValidationResult.FieldNotValid(fieldName, sValue);
                    }

                    if (name.length() > 0) {
                        name += ",";
                    }
                    name += fieldName;
                }
            }
            if (name.length() == 0) {
                name = "(null)";
            }
            name = "Sum of " + name;
            if (m_comparableValue == null) {
                return ValidationResult.FieldNotValid(name, null);
            }
            boolean err = false;
            if (m_operator.equals(Operator.EQUALS)) {
                err = sum != m_comparableValue.doubleValue();
            } else if (m_operator.equals(Operator.GREATER_THAN_OR_EQUALS)) {
                err = sum < m_comparableValue.doubleValue();
            } else if (m_operator.equals(Operator.LESS_THAN_OR_EQUALS)) {
                err = sum > m_comparableValue.doubleValue();
            } else if (m_operator.equals(Operator.GREATER_THAN)) {
                err = sum <= m_comparableValue.doubleValue();
            } else if (m_operator.equals(Operator.LESS_THAN)) {
                err = sum >= m_comparableValue.doubleValue();
            } else {
                return ValidationResult.FieldNotValid(name, "Unsupported comparison operator for sum restruction");
            }
            return !err ? ValidationResult.Valid() : ValidationResult.FieldNotValid(name, Double.toString(sum));
        }
    }

    /**
     * The Class RestrictionTrue.
     */
    static class RestrictionTrue extends Restriction {

        /**
         * Instantiates a new restriction true.
         */
        public RestrictionTrue() {}

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            return super.getValidationResult(helper, export, row);
        }
    }

    /**
     * The Class RestrictionUniqueValue.
     */
    static class RestrictionUniqueValue extends Restriction {
        private final String[] m_fieldNames;


        /**
         * Instantiates a new restriction for unique value.
         *
         * @param fieldNames String[]
         */
        public RestrictionUniqueValue(String... fieldNames) {
            this.m_fieldNames = fieldNames;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);
            String currentValuesCombination = "";
            String currentFieldNamesCombination = "";
            for (String fieldName : m_fieldNames) {
                String targetFieldValue = helper.getExportFormatRowFieldValue(row, export, fieldName);

                if (targetFieldValue == null) {
                    result = ValidationResult.FieldNotFound(fieldName);
                } else {
                    currentValuesCombination = currentValuesCombination + " " + targetFieldValue;
                    currentFieldNamesCombination = currentFieldNamesCombination + " " + fieldName;
                }
            }
            if (!helper.getUniqueSet(this).add(currentValuesCombination)) {
                result = ValidationResult.FieldNotValid(currentFieldNamesCombination, currentValuesCombination);
            }
            return result;
        }
    }

    /**
     * The Class ValidatePeriodNumber.
     */
    static class ValidatePeriodNumber extends Restriction {
        private final String m_periodNumberFieldName;

        /**
         * Instantiates a new validate period number.
         *
         * @param periodNumberFieldName String
         */
        public ValidatePeriodNumber(String periodNumberFieldName) {
            this.m_periodNumberFieldName = periodNumberFieldName;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);

            String periodNumberFieldValue = helper.getExportFormatRowFieldValue(row, export, m_periodNumberFieldName);

            if (periodNumberFieldValue == null) {
                return ValidationResult.FieldNotFound(m_periodNumberFieldName);
            }

            try {
                Integer first2 = Integer.valueOf(periodNumberFieldValue.substring(0, 2));
                Integer second2 = Integer.valueOf(periodNumberFieldValue.substring(2));
                if (!(second2.intValue() >= first2.intValue())) {
                    return ValidationResult.FieldNotValid(m_periodNumberFieldName, periodNumberFieldValue);
                }
            } catch (NumberFormatException nfe) {
                return ValidationResult.FieldNotValid(m_periodNumberFieldName, periodNumberFieldValue);
            }

            return result;
        }
    }

    /**
     * The Class ValidateYearEnteredNinthGrade.
     */
    static class ValidateYearEnteredNinthGrade extends Restriction {
        private final RuntimeParam m_fiscalDate;
        private final String m_yearEntered9GradeFieldName;

        /**
         * Instantiates a new validate year entered ninth grade.
         *
         * @param yearEntered9GradeFieldName String
         * @param fiscalDate RuntimeParam
         */
        public ValidateYearEnteredNinthGrade(String yearEntered9GradeFieldName, RuntimeParam fiscalDate) {
            this.m_yearEntered9GradeFieldName = yearEntered9GradeFieldName;
            this.m_fiscalDate = fiscalDate;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = super.getValidationResult(helper, export, row);

            String m_yearEntered9GradeFieldValue =
                    helper.getExportFormatRowFieldValue(row, export, m_yearEntered9GradeFieldName);

            if (m_yearEntered9GradeFieldValue == null) {
                result = ValidationResult.FieldNotFound(m_yearEntered9GradeFieldName);
            } else {
                if (m_yearEntered9GradeFieldValue.contentEquals("00000000")) {
                    return result;
                }
                Calendar cal = Calendar.getInstance(Locale.US);
                cal.setTime((Date) m_fiscalDate.getRuntimeObject(helper));
                Integer fiscalYear = Integer.valueOf(cal.get(Calendar.YEAR));
                Integer firstYear = Integer.valueOf(m_yearEntered9GradeFieldValue.substring(0, 4));
                Integer secondYear = Integer.valueOf(m_yearEntered9GradeFieldValue.substring(4));
                if ((firstYear.intValue() >= secondYear.intValue())
                        || (secondYear.intValue() > fiscalYear.intValue())) {
                    return ValidationResult.FieldNotValid(m_yearEntered9GradeFieldName, m_yearEntered9GradeFieldValue);
                }
            }
            return result;
        }
    }

    /**
     * The Enum VALIDATION_STATUS.
     */
    enum VALIDATION_STATUS {
        FIELD_NOT_FOUND, FIELD_NOT_VALID, INIT_ERROR, OK
    }

    /**
     * The Class ValidationResult.
     */
    static class ValidationResult {
        /**
         * Validation Result is Field not found.
         *
         * @param fieldName String
         * @return ValidationResult
         */
        public static ValidationResult FieldNotFound(String fieldName) {
            return new ValidationResult(fieldName, null, null, VALIDATION_STATUS.FIELD_NOT_FOUND);
        }

        /**
         * Validation Result is Field not valid.
         *
         * @param fieldName String
         * @param currentValue String
         * @return ValidationResult
         */
        public static ValidationResult FieldNotValid(String fieldName, String currentValue) {
            return new ValidationResult(fieldName, currentValue, null, VALIDATION_STATUS.FIELD_NOT_VALID);
        }

        /**
         * Validation Result is Initialization error.
         *
         * @param field
         * @param value
         * @param initError String
         * @return ValidationResult
         */
        public static ValidationResult InitError(String field, String value, String initError) {
            return new ValidationResult(field, value, initError, VALIDATION_STATUS.INIT_ERROR);
        }

        /**
         * Validation Result is Valid.
         *
         * @return ValidationResult
         */
        public static ValidationResult Valid() {
            return new ValidationResult();
        }

        private final String m_fieldName;
        private final String m_fieldValue;
        private final String m_initError;
        private boolean m_skipRule;
        private final VALIDATION_STATUS m_status;

        /**
         * Instantiates a new validation result.
         */
        private ValidationResult() {
            this("", "", "", VALIDATION_STATUS.OK);
        }

        /**
         * Instantiates a new validation result.
         *
         * @param fieldName String
         * @param currentValue String
         * @param initError String
         * @param status VALIDATION_STATUS
         */
        private ValidationResult(String fieldName, String currentValue, String initError, VALIDATION_STATUS status) {
            this.m_fieldName = fieldName;
            this.m_fieldValue = currentValue;
            this.m_status = status;
            this.m_initError = initError;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the field value.
         *
         * @return String
         */
        public String getFieldValue() {
            return m_fieldValue;
        }

        /**
         * Gets the initialization error.
         *
         * @return String
         */
        public String getInitError() {
            return m_initError;
        }

        /**
         * Gets the status.
         *
         * @return validation status
         */
        public VALIDATION_STATUS getStatus() {
            return m_status;
        }

        /**
         * Checks if is field not found.
         *
         * @return true, if field not found
         */
        public boolean isFieldNotFound() {
            return getStatus() == VALIDATION_STATUS.FIELD_NOT_FOUND;
        }

        /**
         * Checks if is field not valid.
         *
         * @return true, if field not valid
         */
        public boolean isFieldNotValid() {
            return getStatus() == VALIDATION_STATUS.FIELD_NOT_VALID;
        }

        /**
         * Checks if there is initialization error.
         *
         * @return true, if there is initialization error
         */
        public boolean isInitError() {
            return getStatus() == VALIDATION_STATUS.INIT_ERROR;
        }

        /**
         * Checks if field validation status is valid.
         *
         * @return true, if field validation status is valid.
         */
        public boolean isOK() {
            return getStatus() == VALIDATION_STATUS.OK;
        }

        /**
         * Checks if rule will be skipped.
         *
         * @return true, if is skip rule
         */
        public boolean isSkipRule() {
            return m_skipRule;
        }

        /**
         * Sets the skip rule.
         *
         * @param skipRule void
         */
        public void setSkipRule(boolean skipRule) {
            this.m_skipRule = skipRule;
        }
    }

    /**
     * The Class ValidationRule.
     */
    static class ValidationRule implements FLRuleSetValidationProcessor {

        /**
         * Tests if Restrictions.
         *
         * @param restrictions Restriction[]
         * @return ValidationRule
         */
        public static ValidationRule testIf(Restriction... restrictions) {
            return new ValidationRule(restrictions);
        }

        private Restriction[] precondition;

        private Restriction ruleRestriction;

        /**
         * Instantiates a new validation rule.
         *
         * @param precondRestrictions Restriction[]
         */
        private ValidationRule(Restriction[] precondRestrictions) {
            precondition = precondRestrictions;
        }

        /**
         * Gets the validation result.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return Validation result
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            ValidationResult result = ValidationResult.Valid();
            for (Restriction r : precondition) {
                ValidationResult res = r.getValidationResult(helper, export, row);
                if (res.isFieldNotValid() || res.isFieldNotFound() || res.isInitError()) {
                    result.setSkipRule(true);
                    break;
                }
            }
            if (!result.isSkipRule()) {
                result = ruleRestriction.getValidationResult(helper, export, row);
            }
            return result;
        }

        /**
         * Tests then Restrictions.
         *
         * @param restriction Restriction
         * @return ValidationRule
         */
        public ValidationRule testThen(Restriction restriction) {
            ruleRestriction = restriction;
            return this;
        }
    }

    private String m_message;
    private RuleSet m_ruleSet;

    /**
     * Instantiates a new validate rule set.
     *
     * @param ruleSet RuleSet
     * @param message String
     */
    public FLValidationRuleSet(RuleSet ruleSet, String message) {
        m_ruleSet = ruleSet;
        m_message = message;
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
        ValidationResult result = m_ruleSet.getValidationResult(helper, export, row);
        if (result.getStatus() == VALIDATION_STATUS.FIELD_NOT_VALID) {
            errors.add(new FLValidationError(export, row, "Incorrect value", result.getFieldName(),
                    result.getFieldValue(), m_message));
        }
        if (result.getStatus() == VALIDATION_STATUS.FIELD_NOT_FOUND) {
            errors.add(new FLValidationError(export, row, "Field not found", result.getFieldName(), "", ""));
        }
        if (result.getStatus() == VALIDATION_STATUS.INIT_ERROR) {
            errors.add(new FLValidationError(export, row, "Initialization error", result.getFieldName(),
                    result.getFieldValue(), result.getInitError()));
        }
        return errors;
    }

}
