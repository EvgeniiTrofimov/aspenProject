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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.LookupField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.RuntimeParam;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FasterExport;
import com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.RuleAssociation;
import com.x2dev.procedures.statereporting.fl.FLFasterRecordsData.RetrieveTranscript;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Field;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Field.ValueTransformation;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.Operator;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationResult;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;


/**
 * The Class FLFasterValidations.
 */
public class FLFasterValidations {

    /**
     * The Class RestrictionByNumOfRecordType.
     */
    public static class RestrictionByNumOfRecordType extends Restriction {
        private int m_maxNum;
        private int m_minNum;
        private String m_recordType;


        /**
         * Instantiates a new restriction by num of record type.
         *
         * @param recordType String
         * @param num int
         */
        public RestrictionByNumOfRecordType(String recordType, int num) {
            m_maxNum = num;
            m_recordType = recordType;
            m_minNum = -1;
        }


        /**
         * Instantiates a new restriction by num of record type.
         *
         * @param recordType String
         * @param min int
         * @param max int
         */
        public RestrictionByNumOfRecordType(String recordType, int min, int max) {
            m_maxNum = min;
            m_maxNum = max;
            m_recordType = recordType;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            Collection<TransferObjectRecord> records = getRecords(helper, row);
            int counter = 0;
            for (TransferObjectRecord record : records) {
                if (record.getRecordType().equals(m_recordType)) {
                    counter++;
                }
                if (counter > m_maxNum || counter < m_minNum) {
                    return ValidationResult.FieldNotValid(FIELD_RECORD_TYPE, m_recordType);
                }
            }
            return ValidationResult.Valid();
        }


        /**
         * Gets the records.
         *
         * @param helper FLExportConfiguration
         * @param row ExportFormatRow
         * @return Collection
         */
        public Collection<TransferObjectRecord> getRecords(FLExportConfiguration helper, ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            return getTransferObjectRecords(validatedRecord);
        }
    }


    /**
     * The Class RestrictionByRecordTypes.
     */
    public static class RestrictionByRecordTypes extends Restriction {

        Collection<String> m_allowedRecordTypes = new ArrayList<>();


        /**
         * Instantiates a new restriction by record types.
         *
         * @param allowedRecordTypes Collection<String>
         */
        public RestrictionByRecordTypes(Collection<String> allowedRecordTypes) {
            m_allowedRecordTypes.addAll(allowedRecordTypes);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            StudentTransferObject studentTransfer = validatedRecord.getStudentTransfer();
            Collection<TransferObjectRecord> records = studentTransfer.getRecords();
            for (TransferObjectRecord record : records) {
                if (!m_allowedRecordTypes.contains(record.getRecordType())) {
                    return ValidationResult.FieldNotValid(FIELD_RECORD_TYPE, record.getRecordType());
                }
            }
            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionCompareByDependCode.
     */
    public static class RestrictionCompareByDependCode extends Restriction {
        private Map<String, String> m_codesDependencies;
        private String m_comparableFieldName;
        private String m_fieldName;
        private String m_refTableName;


        /**
         * Instantiates a new restriction compare by depend code.
         *
         * @param fieldName String
         * @param refTableName String
         * @param comparableFieldName String
         */
        public RestrictionCompareByDependCode(String fieldName, String refTableName,
                String comparableFieldName) {
            m_fieldName = fieldName;
            m_comparableFieldName = comparableFieldName;
            m_refTableName = refTableName;
        }


        /**
         * Gets the adjusted value.
         *
         * @param value String
         * @param helper FLExportConfiguration
         * @return String
         */
        public String getAdjustedValue(String value, FLExportConfiguration helper) {
            if (m_codesDependencies == null) {
                m_codesDependencies = new HashMap<>();

                X2Criteria refCodesCriteria = new X2Criteria();
                refCodesCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER
                        + ReferenceTable.COL_USER_NAME, m_refTableName);
                String[] attributes = {ReferenceCode.COL_CODE, ReferenceCode.COL_DEPENDENCY_CODE};
                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(ReferenceCode.class, attributes, refCodesCriteria);
                try (ReportQueryIterator iterator = helper.getBroker().getReportQueryIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        Object[] rows = (Object[]) iterator.next();
                        String code = (String) rows[0];
                        String dependencyCode = (String) rows[1];
                        m_codesDependencies.put(code, dependencyCode);
                    }
                }
            }
            return m_codesDependencies.get(value);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            String plainRow = validatedRecord.getPlainRow();
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            String validatedValue = fasterHelper.getPlainRowFieldValue(plainRow, export, m_fieldName);
            String dependencyCode = getAdjustedValue(validatedValue, helper);

            String comparableValue = fasterHelper.getPlainRowFieldValue(plainRow, export, m_comparableFieldName);

            if (!dependencyCode.equals(comparableValue)) {
                return ValidationResult.FieldNotValid(m_fieldName, validatedValue);
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionCompareRecordField.
     */
    public static class RestrictionCompareRecordField extends Restriction {

        private String m_comparableFieldName;
        private FLExport m_comparableExport;
        private String m_validatedFieldName;
        private ValueAdjuster m_comparableValueAdjuster;
        private ValueAdjuster m_validatedValueAdjuster;


        /**
         * Instantiates a new restriction compare record field.
         *
         * @param validatedFieldName String
         * @param comparableExport FLExport
         * @param comparableFieldName String
         */
        public RestrictionCompareRecordField(String validatedFieldName, FLExport comparableExport,
                String comparableFieldName) {
            m_comparableFieldName = comparableFieldName;
            m_comparableExport = comparableExport;
            m_validatedFieldName = validatedFieldName;
        }


        /**
         * Instantiates a new restriction compare record field.
         *
         * @param validatedFieldName String
         * @param validatedValueAdjuster ValueAdjuster
         * @param comparableExport FLExport
         * @param comparableFieldName String
         * @param comparableValueAdjuster ValueAdjuster
         */
        public RestrictionCompareRecordField(String validatedFieldName, ValueAdjuster validatedValueAdjuster,
                FLExport comparableExport, String comparableFieldName, ValueAdjuster comparableValueAdjuster) {
            this(validatedFieldName, comparableExport, comparableFieldName);
            m_comparableValueAdjuster = comparableValueAdjuster;
            m_validatedValueAdjuster = validatedValueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            StudentTransferObject studentTransfer = validatedRecord.getStudentTransfer();
            Collection<TransferObjectRecord> records = studentTransfer.getRecords();
            TransferObjectRecord comparableRecord = null;
            for (TransferObjectRecord currentRecord : records) {
                if (((FasterExport) m_comparableExport).getRecordType().equals(currentRecord.getRecordType())) {
                    comparableRecord = currentRecord;
                }
            }
            String validatedRecordValue =
                    fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, m_validatedFieldName);
            if (m_validatedValueAdjuster != null) {
                validatedRecordValue = m_validatedValueAdjuster.getAdjustedValue(validatedRecordValue, helper);
            }
            String comparableRecordValue = fasterHelper.getPlainRowFieldValue(comparableRecord.getPlainRow(),
                    m_comparableExport, m_comparableFieldName);
            if (m_comparableValueAdjuster != null) {
                comparableRecordValue = m_comparableValueAdjuster.getAdjustedValue(comparableRecordValue, helper);
            }
            if (!validatedRecordValue.equals(comparableRecordValue)) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedRecordValue);
            }
            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionCompareAdjustedFieldValue.
     *
     * @param <T> the generic type
     */
    public static abstract class RestrictionCompareAdjustedFieldValue<T extends Comparable> extends Restriction {
        private Comparator<T> m_defaultComparator;
        private int m_expectedResult;
        private String m_fieldName;
        private T m_valueToCompare;


        /**
         * Instantiates a new restriction compare adjusted field value.
         *
         * @param fieldName String
         * @param valueToCompare T
         * @param expectedResult int
         */
        public RestrictionCompareAdjustedFieldValue(String fieldName, T valueToCompare, int expectedResult) {
            m_fieldName = fieldName;
            m_valueToCompare = valueToCompare;
            m_expectedResult = expectedResult;

            m_defaultComparator = new Comparator<T>() {

                @Override
                public int compare(T o1, T o2) {
                    return o1.compareTo(o2);
                }

            };
        }


        /**
         * Gets the adjusted field value.
         *
         * @param helper FLExportConfiguration
         * @param fieldValue String
         * @return t
         */
        public abstract T getAdjustedFieldValue(FLExportConfiguration helper, String fieldValue);


        /**
         * Gets the comparator.
         *
         * @return Comparator
         */
        public Comparator<T> getComparator() {
            return m_defaultComparator;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            String fieldValue = fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, m_fieldName);
            T adjustedFieldValue = getAdjustedFieldValue(helper, fieldValue);
            return isValid(adjustedFieldValue) ? ValidationResult.Valid()
                    : ValidationResult.FieldNotValid(m_fieldName, fieldValue);
        }


        /**
         * Checks if is valid.
         *
         * @param adjustedFieldValue T
         * @return true, if is valid
         */
        public boolean isValid(T adjustedFieldValue) {
            return m_defaultComparator.compare(adjustedFieldValue, m_valueToCompare) == m_expectedResult;
        }
    }


    /**
     * The Class RestrictionCompareSchoolYearByDate.
     */
    class RestrictionCompareSchoolYearByDate extends RestrictionCompareAdjustedFieldValue<String> {


        /**
         * Instantiates a new restriction compare school year by date.
         *
         * @param fieldName String
         * @param valueToCompare String
         * @param expectedResult int
         */
        public RestrictionCompareSchoolYearByDate(String fieldName, String valueToCompare, int expectedResult) {
            super(fieldName, valueToCompare, expectedResult);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterValidations.RestrictionCompareAdjustedFieldValue#getAdjustedFieldValue(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      java.lang.String)
         */
        @Override
        public String getAdjustedFieldValue(FLExportConfiguration helper, String fieldValue) {
            SimpleDateFormat testDateFormat = new SimpleDateFormat("MMddyyyy");
            try {
                int schoolYear;
                Date testDate = testDateFormat.parse(fieldValue);
                Calendar cal = Calendar.getInstance();
                cal.setTime(testDate);
                int month = cal.get(Calendar.MONTH);
                if (month > Calendar.AUGUST) {
                    schoolYear = cal.get(Calendar.YEAR);
                } else {
                    schoolYear = cal.get(Calendar.YEAR) - 1;
                }
                String schoolYears =
                        schoolYear + String
                                .valueOf((Integer.valueOf(schoolYear).intValue() + 1));
                return schoolYears;
            } catch (ParseException e) {
                throw new X2RuntimeException();
            }
        }
    }


    /**
     * The Class RestrictionCrossRecordsEqualFields.
     */
    public static class RestrictionCrossRecordsEqualFields extends Restriction {
        private Map<String, String> m_equalFields = new HashMap<String, String>();
        private FL_EXPORT m_exportToCompare;


        /**
         * Instantiates a new restriction cross records equal fields.
         *
         * @param equalFields Map<String,String>
         * @param exportToCompare FL_EXPORT
         */
        public RestrictionCrossRecordsEqualFields(Map<String, String> equalFields, FL_EXPORT exportToCompare) {
            m_equalFields = equalFields;
            m_exportToCompare = exportToCompare;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            ArrayList<String> validatedValues = new ArrayList<String>();
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;

            for (TransferObjectRecord comparableRecord : findComparableRecords(helper, row)) {
                boolean allFieldsMatched = true;
                for (Entry<String, String> fields : m_equalFields.entrySet()) {
                    String validatedField = fields.getKey();
                    String comparableField = fields.getValue();

                    String validatedValue =
                            fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, validatedField);
                    String comparableValue =
                            fasterHelper.getPlainRowFieldValue(comparableRecord.getPlainRow(), m_exportToCompare,
                                    comparableField);

                    if (!validatedValue.equals(comparableValue)) {
                        allFieldsMatched = false;
                        break;
                    }
                }
                if (allFieldsMatched) {
                    return ValidationResult.Valid();
                }
            }

            for (String fieldName : m_equalFields.keySet()) {
                validatedValues
                        .add(fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, fieldName));
            }

            return ValidationResult.FieldNotValid(m_equalFields.keySet().toString(), validatedValues.toString());
        }


        /**
         * Find comparable records.
         *
         * @param helper FLExportConfiguration
         * @param row ExportFormatRow
         * @return List
         */
        public List<TransferObjectRecord> findComparableRecords(FLExportConfiguration helper, ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            StudentTransferObject studentTransfer = validatedRecord.getStudentTransfer();
            Collection<TransferObjectRecord> records = studentTransfer.getRecords();
            ArrayList<TransferObjectRecord> comparableRecords = new ArrayList<>();
            for (TransferObjectRecord record : records) {
                if (record.getRecordType().equals(m_exportToCompare.getRecordType())) {
                    comparableRecords.add(record);
                }
            }
            return comparableRecords;
        }
    }


    /**
     * The Class RestrictionFormattedString.
     */
    public static class RestrictionFormattedString extends Restriction {
        private String m_comparedFieldName;
        private String m_formatFieldName;
        private String m_formatId;
        private Operator m_operator;
        private Object m_value;


        /**
         * Instantiates a new restriction formatted string.
         *
         * @param operator Operator
         * @param comparedFieldName String
         * @param formatId String
         * @param formatFieldName String
         * @param value Object
         */
        public RestrictionFormattedString(Operator operator, String comparedFieldName, String formatId,
                String formatFieldName,
                Object value) {
            m_comparedFieldName = comparedFieldName;
            m_formatId = formatId;
            m_formatFieldName = formatFieldName;
            m_operator = operator;
            m_value = value;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            if (m_value instanceof RuntimeParam) {
                m_value = ((RuntimeParam) m_value).getRuntimeObject(helper);
            }
            String formattedValue = helper.getFormattedValue(m_formatId, m_formatFieldName, m_value.toString());
            RestrictionCompare restriction = new RestrictionCompare(m_operator,
                    m_comparedFieldName, formattedValue);
            return restriction.getValidationResult(helper, export, row);
        }
    }


    /**
     * The Class RestrictionForRecordType.
     */
    public static class RestrictionForRecordType extends Restriction {
        private FLExport m_validatedFasterExport;
        private Restriction m_restriction;


        /**
         * Instantiates a new restriction for record type.
         *
         * @param validatedFasterExport FLExport
         * @param restriction Restriction
         */
        public RestrictionForRecordType(FLExport validatedFasterExport, Restriction restriction) {
            m_validatedFasterExport = validatedFasterExport;
            m_restriction = restriction;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord recordRestrictionFor = findRecord(helper, row);
            if (recordRestrictionFor == null) {
                return ValidationResult.InitError(null, null, ERROR_INIT_CANNOT_FIND_RECORD);
            }
            return m_restriction.getValidationResult(helper, m_validatedFasterExport,
                    recordRestrictionFor.getExportFormatRow());
        }


        /**
         * Find record.
         *
         * @param helper FLExportConfiguration
         * @param row ExportFormatRow
         * @return TransferObjectRecord
         */
        public TransferObjectRecord findRecord(FLExportConfiguration helper, ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            List<TransferObjectRecord> records = validatedRecord.getStudentTransfer().getRecords();
            TransferObjectRecord recordRestrictionFor = null;
            for (TransferObjectRecord currentRecord : records) {
                if (currentRecord.getRecordType().equals(((FasterExport) m_validatedFasterExport).getRecordType())) {
                    recordRestrictionFor = currentRecord;
                }
            }
            return recordRestrictionFor;
        }
    }


    /**
     * The Class RestrictionForRecordsType.
     */
    public static class RestrictionForRecordsType extends Restriction {
        String m_recordsType;


        /**
         * Instantiates a new restriction for records type.
         *
         * @param recordsType String
         */
        public RestrictionForRecordsType(String recordsType) {
            if (!TransferObject.RECORDS_TYPE_INTERDISTRICT.equals(recordsType) &&
                    !TransferObject.RECORDS_TYPE_SECONDARY.equals(recordsType)) {
                throw new IllegalArgumentException("Unexpected records type: " + recordsType);
            }
            m_recordsType = recordsType;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            boolean valid = false;
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            if (validatedRecord != null) {
                TransferObject transferObject = validatedRecord.getTransferObject();
                if (transferObject != null && transferObject.getRecordsType() != null) {
                    valid = transferObject.getRecordsType().equals(m_recordsType);
                }
            }
            if (!valid) {
                return ValidationResult.InitError(null, null, m_recordsType);
            }
            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionHeaderMessageType.
     */
    public static class RestrictionHeaderMessageType extends Restriction {

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            String headerMessageType = null;
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            Collection<TransferObjectRecord> records = getTransferObjectRecords(validatedRecord);
            if (validatedRecord != null && records != null && !records.isEmpty()) {
                TransferObjectRecord header = records.iterator().next();
                FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
                FLExport[] exports = fasterHelper.getExports();
                if (header != null && exports != null && exports.length > 0) {
                    headerMessageType =
                            fasterHelper.getPlainRowFieldValue(header.getPlainRow(), exports[0], FIELD_MESSAGE_TYPE);
                }
            }
            return "S12".equals(headerMessageType)
                    ? ValidationResult.FieldNotValid(FIELD_MESSAGE_TYPE, headerMessageType)
                    : ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionHeaderRecordType.
     */
    public static class RestrictionHeaderRecordType extends Restriction {

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            Collection<TransferObjectRecord> records = getTransferObjectRecords(validatedRecord);
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            String validatedRecordType =
                    fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, FIELD_RECORD_TYPE);
            String requiredType = validatedRecordType.substring(0, 1);
            TransferObjectRecord header = records.iterator().next();
            String headerRecordType =
                    fasterHelper.getPlainRowFieldValue(header.getPlainRow(), export, FIELD_RECORD_TYPE);
            if (!headerRecordType.equals(requiredType + TransferObjectRecord.RECORD_TYPE_00)) {
                return ValidationResult.FieldNotValid(FIELD_RECORD_TYPE, validatedRecordType);
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionHeaderSectionBFilled.
     */
    public static class RestrictionHeaderSectionBFilled extends Restriction {

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord record = transferObjectHelper.getTransferObjectRecord(row);
            StudentTransferObject response = record.getStudentTransfer();
            StudentTransferObject request = transferObjectHelper.getRequestToReply(response, fasterHelper);
            if (request == null) {
                return ValidationResult.InitError(null, null, "Response Message Type S01-S17 should have Request");
            }
            TransferObjectRecord responseHeader = response.getRecords().iterator().next();
            TransferObjectRecord requestHeader = request.getRecords().iterator().next();

            for (Entry<String, String> pair : s_sectionAToBFields.entrySet()) {
                String value1 = fasterHelper.getPlainRowFieldValue(requestHeader.getPlainRow(), export,
                        pair.getKey());
                String value2 = fasterHelper.getPlainRowFieldValue(responseHeader.getPlainRow(), export,
                        pair.getValue());
                if (!value1.equals(value2)) {
                    return ValidationResult.FieldNotValid(pair.getValue(), value2);
                }
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionExternalResult.
     */
    public static abstract class RestrictionExternalResult extends Restriction {
        private Object m_expectedResult;
        private String m_validatedFieldName;
        private String m_validatedValue;
        private ValueAdjuster m_valueAdjuster;


        /**
         * Instantiates a new restriction external result.
         *
         * @param expectedResult Object
         * @param validatedFieldName String
         * @param valueAdjuster ValueAdjuster
         */
        public RestrictionExternalResult(Object expectedResult, String validatedFieldName,
                ValueAdjuster valueAdjuster) {
            m_expectedResult = expectedResult;
            m_validatedFieldName = validatedFieldName;
            m_valueAdjuster = valueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            String plainRow = transferObjectHelper.getPlainRow(row);
            m_validatedValue = fasterHelper.getPlainRowFieldValue(plainRow, export, m_validatedFieldName);
            if (m_valueAdjuster != null) {
                m_validatedValue = m_valueAdjuster.getAdjustedValue(m_validatedValue, fasterHelper);
            }

            Object validatedExternalResult = getValidatedExternalResult(fasterHelper, export, row);
            if (!validatedExternalResult.equals(m_expectedResult)) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, m_validatedValue);
            }
            return ValidationResult.Valid();
        }


        /**
         * Gets the validated value.
         *
         * @return String
         */
        public String getValidatedValue() {
            return m_validatedValue;
        }


        /**
         * Gets the validated external result.
         *
         * @param fasterHelper FLFasterExportConfiguration
         * @param export FLExport
         * @param row ExportFormatRow
         * @return Object
         */
        abstract Object getValidatedExternalResult(FLFasterExportConfiguration fasterHelper,
                                                   FLExport export,
                                                   ExportFormatRow row);
    }


    /**
     * The Class RestrictionImmunizationStatusDate.
     */
    public static class RestrictionImmunizationStatusDate extends Restriction {
        public static final String DATE_EXEMPT = "EXEMPT  ";
        public static final String DATE_FORMAT_FULL = "yyyyMMdd";
        public static final String DATE_FORMAT_L = "yyyy";

        private SimpleDateFormat m_fullDateFormat;
        private SimpleDateFormat m_lDateFormat;

        private String m_fieldName;


        /**
         * Instantiates a new restriction immunization status date.
         *
         * @param fieldName String
         */
        public RestrictionImmunizationStatusDate(String fieldName) {
            m_fullDateFormat = new SimpleDateFormat(DATE_FORMAT_FULL);
            m_fullDateFormat.setLenient(false);

            m_lDateFormat = new SimpleDateFormat(DATE_FORMAT_L);
            m_lDateFormat.setLenient(false);

            m_fieldName = fieldName;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterExport = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterExport.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            String plainRow = validatedRecord.getPlainRow();
            String statusDate = fasterExport.getPlainRowFieldValue(plainRow, export, m_fieldName);

            if (statusDate.startsWith("8") && statusDate.substring(2).equals("99999999")) {
                return ValidationResult.Valid();
            }

            String date = statusDate.substring(2);
            if (statusDate.startsWith("L") && date.substring(4).equals("0000")) {
                String year = date.substring(0, 4);
                try {
                    m_lDateFormat.parse(year);
                } catch (ParseException e) {
                    return ValidationResult.FieldNotValid(m_fieldName, statusDate);
                }
                return ValidationResult.Valid();
            }

            if (!date.equals(DATE_EXEMPT)) {
                try {
                    Date parsedDateObject = m_fullDateFormat.parse(date);
                    if (parsedDateObject.after(new Date())) {
                        return ValidationResult.FieldNotValid(m_fieldName, statusDate);
                    }
                } catch (ParseException e) {
                    return ValidationResult.FieldNotValid(m_fieldName, statusDate);
                }
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionPostReclassificationDate.
     */
    public static class RestrictionPostReclassificationDate extends Restriction {
        private String DATE_FORMAT = "yyyyMMdd";
        private SimpleDateFormat m_dateFormat;
        private Pattern m_emptyPattern;
        private String m_fieldNameNotBefore;
        private String m_firstLetter;
        private String m_validatedFieldName;


        /**
         * Instantiates a new restriction post reclassification date.
         *
         * @param fieldName String
         * @param firstLetter String
         * @param fieldNameNotBefore String
         */
        public RestrictionPostReclassificationDate(String fieldName, String firstLetter, String fieldNameNotBefore) {
            m_validatedFieldName = fieldName;
            m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
            m_dateFormat.setLenient(false);
            m_emptyPattern = Pattern.compile(PATTERN_EMPTY);
            m_firstLetter = firstLetter;
            m_fieldNameNotBefore = fieldNameNotBefore;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            String validatedValue =
                    fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, m_validatedFieldName);

            String validatedFirstLetter = validatedValue.substring(0, 1);
            if (!validatedFirstLetter.equals(m_firstLetter)) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedValue);
            }
            String dateValue = validatedValue.substring(1);
            Matcher matcher = m_emptyPattern.matcher(dateValue);
            if (matcher.find()) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedValue);
            }

            Date validatedDate = null;
            try {
                validatedDate = m_dateFormat.parse(dateValue);
            } catch (ParseException e) {
                throw new X2RuntimeException();
            }
            if (validatedDate.after(new Date())) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedValue);
            }

            String notBeforeDateValue =
                    fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, m_fieldNameNotBefore);
            matcher = m_emptyPattern.matcher(notBeforeDateValue);
            if (matcher.find()) {
                return ValidationResult.FieldNotValid(m_fieldNameNotBefore, notBeforeDateValue);
            }
            Date notBeforeDate = null;
            try {
                notBeforeDate = m_dateFormat.parse(notBeforeDateValue);
            } catch (ParseException e) {
                throw new X2RuntimeException();
            }

            if (validatedDate.before(notBeforeDate)) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedValue);
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionPreviousRecordType.
     */
    public static class RestrictionPreviousRecordType extends Restriction {

        public static final String RECORD_SEQUENCE = "Record Sequence";

        private boolean m_immediatelyFollows = true;
        private List<String> m_previousRecordType = new ArrayList<>();
        private boolean m_reverseRecordType = false;


        /**
         * Instantiates a new restriction previous record type.
         *
         * @param previoudRecordType String
         */
        public RestrictionPreviousRecordType(String previoudRecordType) {
            m_previousRecordType.add(previoudRecordType);
        }


        /**
         * Instantiates a new restriction previous record type.
         *
         * @param previoudRecordType String
         * @param immediatelyFollows boolean
         */
        public RestrictionPreviousRecordType(String previoudRecordType, boolean immediatelyFollows) {
            m_previousRecordType.add(previoudRecordType);
            m_immediatelyFollows = immediatelyFollows;
        }


        /**
         * Instantiates a new restriction previous record type.
         *
         * @param previoudRecordTypes List<String>
         */
        public RestrictionPreviousRecordType(List<String> previoudRecordTypes) {
            m_previousRecordType.addAll(previoudRecordTypes);
        }


        /**
         * Instantiates a new restriction previous record type.
         *
         * @param previoudRecordTypes List<String>
         * @param reverseRecordType boolean
         */
        public RestrictionPreviousRecordType(List<String> previoudRecordTypes, boolean reverseRecordType) {
            this(previoudRecordTypes);
            m_reverseRecordType = reverseRecordType;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            if (m_immediatelyFollows) {
                TransferObjectRecord previousRecord = getPreviousTransferObjectRecord(helper, row);
                if (!m_reverseRecordType && (previousRecord == null
                        || !m_previousRecordType.contains(previousRecord.getRecordType()))) {
                    return ValidationResult.FieldNotValid(RECORD_SEQUENCE, null);
                }
                if (m_reverseRecordType && previousRecord != null
                        && m_previousRecordType.contains(previousRecord.getRecordType())) {
                    return ValidationResult.FieldNotValid(RECORD_SEQUENCE, null);
                }
            } else {
                TransferObjectRecord previousRecord =
                        getPreviousTransferObjectRecordWithType(helper, row, m_previousRecordType.get(0));
                if (previousRecord == null) {
                    return ValidationResult.FieldNotValid(RECORD_SEQUENCE, null);
                }
            }

            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionPreviousSchoolYearRecord.
     */
    public static class RestrictionPreviousSchoolYearRecord extends Restriction {
        private static final String DATE_FORMAT_L = "yyyy";

        private static final String[] VALID_PREVIOUS_TYPES = new String[] {
                TransferObjectRecord.RECORD_TYPE_01, TransferObjectRecord.RECORD_TYPE_02,
                TransferObjectRecord.RECORD_TYPE_03, TransferObjectRecord.RECORD_TYPE_04};

        private SimpleDateFormat m_lDateFormat;


        /**
         * Instantiates a new restriction previous school year record.
         */
        public RestrictionPreviousSchoolYearRecord() {
            m_lDateFormat = new SimpleDateFormat(DATE_FORMAT_L);
            m_lDateFormat.setLenient(false);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            TransferObjectRecord previousRecord = getPreviousTransferObjectRecord(helper, row);
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            boolean valid = previousRecord != null;
            if (valid) {
                if (TransferObjectRecord.RECORD_TYPE_03.equals(previousRecord.getRecordType())) {
                    String previousYear = fasterHelper.getPlainRowFieldValue(previousRecord.getPlainRow(), export,
                            FIELD_SKL_YEAR_WITH_CENT);
                    String currentYear =
                            fasterHelper.getExportFormatRowFieldValue(row, export, FIELD_SKL_YEAR_WITH_CENT);
                    if (!StringUtils.isEmpty(previousYear) && !StringUtils.isEmpty(currentYear) &&
                            previousYear.length() > 4 && currentYear.length() > 4) {
                        try {
                            valid = m_lDateFormat.parse(previousYear.substring(0, 4))
                                    .compareTo(m_lDateFormat.parse(currentYear.substring(0, 4))) < 0;
                        } catch (ParseException ex) {
                            valid = false;
                        }
                    }
                }
            }
            if (valid) {
                valid = ArrayUtils.contains(VALID_PREVIOUS_TYPES, previousRecord.getRecordType());
            }
            if (valid && TransferObjectRecord.RECORD_TYPE_01.equals(previousRecord.getRecordType()) &&
                    TransferObject.RECORDS_TYPE_INTERDISTRICT
                            .equals(previousRecord.getTransferObject().getRecordsType())) {
                String gradeLevel =
                        fasterHelper.getPlainRowFieldValue(previousRecord.getPlainRow(), export, FIELD_GRADE_LEVEL);
                if (!"31".equals(gradeLevel) && !"30".equals(gradeLevel)) {
                    valid = Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, Integer.valueOf(21),
                            null, true, false).getValidationResult(fasterHelper, export, row).isOK();
                }
                if (valid) {
                    valid = !Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM,
                            START + PATTERN_BRIGHT_FUTURES + END)
                            .getValidationResult(fasterHelper, FL_EXPORT.RECORD00,
                                    previousRecord.getStudentTransfer().getRecords().get(0)
                                            .getExportFormatRow())
                            .isOK();
                }
            }
            if (!valid) {
                return ValidationResult.FieldNotValid(RestrictionPreviousRecordType.RECORD_SEQUENCE, null);
            }
            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionRecordMustFollow.
     */
    public static class RestrictionRecordMustFollow extends Restriction {

        Collection<String> m_recordTypesMustFollow = new ArrayList<>();


        /**
         * Instantiates a new restriction record must follow.
         *
         * @param recordTypesMustFollow Collection<String>
         */
        public RestrictionRecordMustFollow(Collection<String> recordTypesMustFollow) {
            m_recordTypesMustFollow.addAll(recordTypesMustFollow);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            StudentTransferObject studentTransfer = validatedRecord.getStudentTransfer();
            Collection<TransferObjectRecord> records = studentTransfer.getRecords();
            for (TransferObjectRecord record : records) {
                if (m_recordTypesMustFollow.contains(record.getRecordType())) {
                    return ValidationResult.Valid();
                }
            }

            return ValidationResult.FieldNotValid(FIELD_RECORD_TYPE, null);
        }
    }


    /**
     * The Class RestrictionSearchRecord.
     */
    public static abstract class RestrictionSearchRecord extends Restriction {
        private String m_comparableFieldName;
        private String m_validatedFieldName;


        /**
         * Instantiates a new restriction search record.
         *
         * @param validatedFieldName String
         * @param comparableFieldName String
         */
        public RestrictionSearchRecord(String validatedFieldName, String comparableFieldName) {
            m_comparableFieldName = comparableFieldName;
            m_validatedFieldName = validatedFieldName;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObjectHelper transferObjectHelper = fasterHelper.getTransferObjectHelper();
            TransferObjectRecord validatedRecord = transferObjectHelper.getTransferObjectRecord(row);
            String validatedFieldValue =
                    fasterHelper.getPlainRowFieldValue(validatedRecord.getPlainRow(), export, m_validatedFieldName);

            TransferObjectRecord comparableRecord = findRecord(helper, row);
            if (comparableRecord == null) {
                return ValidationResult.InitError(null, null, ERROR_INIT_CANNOT_FIND_RECORD);
            }
            String comparableFieldValue =
                    fasterHelper.getPlainRowFieldValue(comparableRecord.getPlainRow(),
                            FL_EXPORT.findExportByRecordType(comparableRecord.getRecordType()),
                            m_comparableFieldName);

            if (!validatedFieldValue.equals(comparableFieldValue)) {
                return ValidationResult.FieldNotValid(m_validatedFieldName, validatedFieldValue);
            }
            return ValidationResult.Valid();
        }


        /**
         * Find record.
         *
         * @param helper FLExportConfiguration
         * @param row ExportFormatRow
         * @return TransferObjectRecord
         */
        public abstract TransferObjectRecord findRecord(FLExportConfiguration helper, ExportFormatRow row);
    }


    /**
     * The Class RestrictionTransferObject.
     */
    public static class RestrictionTransferObject extends Restriction {
        private String m_alias;
        private String m_comparableValue;


        /**
         * Instantiates a new restriction transfer object.
         *
         * @param alias String
         * @param comparableValue String
         */
        public RestrictionTransferObject(String alias, String comparableValue) {
            m_alias = alias;
            m_comparableValue = comparableValue;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            FLFasterExportConfiguration fasterHelper = (FLFasterExportConfiguration) helper;
            TransferObject transferObject = fasterHelper.getTransferObject();
            String transferObjectValue = transferObject.getFieldValueByAlias(m_alias);
            if (!transferObjectValue.equals(m_comparableValue)) {
                return ValidationResult.FieldNotValid("Transfer Object: " + m_alias,
                        transferObjectValue);
            }
            return ValidationResult.Valid();
        }
    }


    /**
     * The Class RestrictionVerticalSumCompare.
     */
    public static class RestrictionVerticalSumCompare extends Restriction {

        ValueAdjuster m_adjuster = null;
        FLExport m_export = null;
        String m_fieldToGroup = null;
        String m_fieldToSum = null;
        Operator m_operator = null;
        Double m_comparableValue = null;


        /**
         * The Class AdjusterVertSumByField.
         */
        public class AdjusterVertSumByField implements ValueAdjuster {


            /**
             * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RestrictionCompare.ValueAdjuster#getAdjustedValue(java.lang.String,
             *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration)
             */
            @Override
            public String getAdjustedValue(String fieldToGroupValue, FLExportConfiguration helper) {
                LookupField lookupKey = helper.new LookupField();
                LookupField lookupValue = helper.new LookupField();
                lookupKey.add(m_fieldToGroup);
                lookupValue.add(fieldToGroupValue);
                Collection<ExportFormatRow> rows = helper.getExportFormatRows(m_export, lookupKey, lookupValue);
                Double sum = Double.valueOf(0);
                for (ExportFormatRow row : rows) {
                    String value = helper.getExportFormatRowFieldValue(row, m_export, m_fieldToSum);
                    Double parsedValue = Double.valueOf(value);
                    sum = Double.valueOf(sum.doubleValue() + parsedValue.doubleValue());
                }
                return sum.toString();
            }
        }


        /**
         * Instantiates a new restriction vertical sum compare.
         *
         * @param export FLExport
         * @param operator Operator
         * @param fieldToGroup String
         * @param fieldToSum String
         * @param comparableValue Double
         */
        public RestrictionVerticalSumCompare(FLExport export, Operator operator, String fieldToGroup,
                String fieldToSum, Double comparableValue) {
            m_export = export;
            m_fieldToGroup = fieldToGroup;
            m_fieldToSum = fieldToSum;
            m_operator = operator;
            m_comparableValue = comparableValue;

            m_adjuster = new AdjusterVertSumByField();
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction#getValidationResult(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public ValidationResult getValidationResult(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row) {
            RestrictionCompare restriction =
                    new RestrictionCompare(m_operator, new Field(m_fieldToGroup, m_adjuster), m_comparableValue);
            return restriction.getValidationResult(helper, export, row);
        }
    }

    private static final String ALIAS_CRS_DUAL_ENROLLMENT = "all-crs-CRDCDualEnroll";

    private static final String END = "$";

    private static final SimpleDateFormat ENR_DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");

    public static int EQUALS = 0;
    public static int GREATER = 1;
    public static int LESS = -1;

    private static final String ERROR_INIT_CANNOT_FIND_RECORD =
            "Cannot find record with specified parameters to validate";

    private static final String OR = "|";

    private static final int MAX_NUM_SCORES = 2;
    private static final int MAX_NUM_SUBJECT_CONTENT = 9;
    private static final int MAX_NUM_TEST = 6;
    private static final int MAX_NUM_TEST_PAS = 30;

    private static final String PATTERN_FROM_1_TO_8_ARE_UNIQUE = "^(?:(?:([1-8])|([^1-8]))(?!.*\\1))* *$";
    private static final String PATTERN_ALL_ZEROS = "^0+$";
    private static final String PATTERN_BRIGHT_FUTURES = "0{5}95";
    private static final String PATTERN_EMPTY = "^ +$";

    private static final String PATTERN_FROM_01_TO_76 = "^(?!00|77|78|79)[0-7][0-9]$";
    private static final String PATTERN_FROM_C901_TO_C928 = "C9(?!00|29)[0-2][0-9]";
    private static final String PATTERN_FROM_U970_TO_U980 = "U9(7[0-9]|80)";
    private static final String PATTERN_GRADE_LEVELS_PK_KG_FROM_01_TO_12_OR_23 =
            "PK" + OR + "KG" + OR + "0[1-9]" + OR + "1[0-2]" + OR + "23";
    private static final String PATTERN_NOT_ALL_ZEROS = ".*[^0].*";
    private static final String PATTERN_NOT_EMPTY = ".*[^ ].*";

    private static final String PATTERN_NUMERIC = "^[\\d]+$";

    private static final String PATTERN_SKL_NUM_CRED_EARNED_EXCL = "^(?!3[58]18).*$";

    private static final String REFERENCE_TABLE_NAME_ACADEMY_ID = "FL Cape Identifier";
    private static final String REFERENCE_TABLE_NAME_ACTIVITY_AWARDS = "FL Activity/Award Codes";
    private static final String REFERENCE_TABLE_NAME_BRIGHT_FUTURES_HS_COURSES = "FL Bright Futures HS Courses";
    private static final String REFERENCE_TABLE_NAME_CAN_MEX_SOUTH_AMERICAN_STATE_CODES =
            "FL Canadian Mexican and South American State Codes";
    private static final String REFERENCE_TABLE_NAME_COUNTRY_CODES = "FL Country Codes";
    private static final String REFERENCE_TABLE_NAME_COURSE_CODES = "FL Course Codes";
    private static final String REFERENCE_TABLE_NAME_DIPLOMA_TYPE = "FL Diploma Type";
    private static final String REFERENCE_TABLE_NAME_DISEASES_CODES = "FL Faster Diseases";
    private static final String REFERENCE_TABLE_NAME_DISTRICT_CODES = "FL FASTER Districts";
    private static final String REFERENCE_TABLE_NAME_IND_CERT = "FL Cape Industry Certification";
    private static final String REFERENCE_TABLE_NAME_IND_CERT_IDS = "FL Cape Industry Certification Id";
    private static final String REFERENCE_TABLE_NAME_LANG_CODES = "FL Language Code";
    private static final String REFERENCE_TABLE_NAME_MESSAGES_CODES = "FL FASTER Request/Response Codes";
    private static final String REFERENCE_TABLE_NAME_OUT_OF_STATE_HS_COURSES = "FL Out-of-state HS Courses";
    private static final String REFERENCE_TABLE_NAME_PART_DATE_QUALIFIER = "FL Participation Date Qualifier";
    private static final String REFERENCE_TABLE_NAME_PROC_TERMINOLOGY = "FL Faster Procedural Terminology";
    private static final String REFERENCE_TABLE_NAME_SCHOOL_CODES = "FL FASTER Schools";
    private static final String REFERENCE_TABLE_NAME_SPEEDE_EXPRESS_CODES =
            "FL FASTER SPEEDE/ExPRESS Institution ID Codes";
    private static final String REFERENCE_TABLE_NAME_STATE_CODES = "FL State Codes";
    private static final String REFERENCE_TABLE_NAME_STATE_TER_CODES = "FL State/Territory Codes";
    private static final String REFERENCE_TABLE_NAME_SUBJECT_AREA = "FL Course State Subject Area Requirements";
    private static final String REFERENCE_TABLE_NAME_SUBJECT_CONTENT_CODE = "FL Test Subject Content Code";
    private static final String REFERENCE_TABLE_NAME_TALENTED_20_HS_COURSES = "FL Talented 20 HS Courses";
    private static final String REFERENCE_TABLE_NAME_TEST_CODES = "FL Test Code";
    private static final String REFERENCE_TABLE_NAME_WDRAW_CODES = "FL Faster Wdrawal Codes";
    private static final String REFERENCE_TABLE_NAME_WDRAW_ADULTS_CODES = "FL Faster Wdrawal Adults Codes";

    private static final boolean SKIP_RULE_IF_VALID = true;

    private static final String START = "^";

    private static final Restriction s_addressedToMsix = new RestrictionForRecordType(FL_EXPORT.RECORD00,
            Restriction.and(
                    Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                            "^0{5}99$"),
                    Restriction.pattern(FIELD_ADDRESSED_SKL_NUM,
                            "^0{3}6$")));

    private static final List<String> s_fasterCreditsNeededSubjs = Arrays.asList(
            "ALGEBRA",
            "ALG 2",
            "AM GOV",
            "AM HIST",
            "BIOLOGY",
            "COMP ED",
            "ECONOM",
            "ELECT",
            "ENG",
            "ESE",
            "FOR LAN",
            "GEOMET",
            "LANG AR",
            "LMS",
            "MATH",
            "PERF FA",
            "PHYS ED",
            "PR ARTS",
            "SCIENCE",
            "SOC STU",
            "W HIST");

    private static final Map<String, String> s_fasterDateFormats = new HashMap<String, String>();

    static {
        s_fasterDateFormats.put("CM", "yyyyMM");
        s_fasterDateFormats.put("CY", "yyyy");
        s_fasterDateFormats.put("D8", "yyyyMMdd");
        s_fasterDateFormats.put("DB", "MMddyyyy");
        s_fasterDateFormats.put("RD4", "yyyy-yyyy");
        s_fasterDateFormats.put("RD5", "yyyyMM-yyyyMM");
        s_fasterDateFormats.put("RD8", "yyyyMMdd-yyyyMMdd");
    }

    private static final Map<String, String> s_fasterDateFormatsLengthChecker = new HashMap<String, String>();

    static {
        s_fasterDateFormatsLengthChecker.put("CM", "^[\\d]{6} +$");
        s_fasterDateFormatsLengthChecker.put("CY", "^[\\d]{4} +$");
        s_fasterDateFormatsLengthChecker.put("D8", "^[\\d]{8} +$");
        s_fasterDateFormatsLengthChecker.put("DB", "^[\\d]{8} +$");
        s_fasterDateFormatsLengthChecker.put("RD4", "^[\\d]{4}-[\\d]{4} +$");
        s_fasterDateFormatsLengthChecker.put("RD5", "^[\\d]{6}-[\\d]{6} +$");
        s_fasterDateFormatsLengthChecker.put("RD8", "[\\d]{8}-[\\d]{8} +$");
    }

    private static final Restriction s_notAddressedToMsix = new RestrictionForRecordType(FL_EXPORT.RECORD00,
            Restriction.or(
                    Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                            "^(?!0{5}99).*$"),
                    Restriction.pattern(FIELD_ADDRESSED_SKL_NUM,
                            "^(?!0{3}6).*$")));

    private static final RestrictionForRecordType s_sendingIsSpeedeExpress =
            new RestrictionForRecordType(FL_EXPORT.RECORD00,
                    Restriction.and(Restriction.pattern(FIELD_SENDING_DST_NUM, "^0{5}99$"),
                            Restriction.pattern(FIELD_SENDING_SKL_NUM, "^0{4}$"),
                            Restriction.pattern(FIELD_SENDING_INST_ID, PATTERN_NOT_EMPTY)));

    private static final RestrictionForRecordType s_sendingNotSpeedeExpress =
            new RestrictionForRecordType(FL_EXPORT.RECORD00,
                    Restriction.or(
                            Restriction.pattern(FIELD_SENDING_DST_NUM, "^(?!0{5}99).*$"),
                            Restriction.pattern(FIELD_SENDING_SKL_NUM, "^(?!0{4}).*$"),
                            Restriction.pattern(FIELD_SENDING_INST_ID, PATTERN_EMPTY)));

    private List<RuleAssociation> m_ruleAssociations = null;


    /**
     * Gets the rule associations.
     *
     * @param exportIdSuffix String
     * @return List
     */
    public List<RuleAssociation> getRuleAssociations(String exportIdSuffix) {
        List<RuleAssociation> rules = new ArrayList<>();
        if (m_ruleAssociations == null) {
            initializeRuleAssociations();
        }
        for (RuleAssociation association : m_ruleAssociations) {
            FLValidationRule rule = association.getRule(exportIdSuffix);
            if (rule != null) {
                rules.add(association);
            }
        }
        return rules;
    }


    /**
     * Gets the date.
     *
     * @param month int
     * @param day int
     * @param year int
     * @return Date
     */
    private static Date getDate(int month, int day, int year) {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MONTH, month);
        calendar.set(Calendar.DATE, day);
        calendar.set(Calendar.YEAR, year);

        return calendar.getTime();
    }


    /**
     * Gets the date.
     *
     * @param month int
     * @param day int
     * @return Date
     */
    private static Date getDate(int month, int day) {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MONTH, month);
        calendar.set(Calendar.DATE, day);

        return calendar.getTime();
    }


    /**
     * Gets the date with corrected year.
     *
     * @param date Date
     * @param yearsAdjusted int
     * @return Date
     */
    private static Date getDateWithCorrectedYear(Date date, int yearsAdjusted) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.YEAR, yearsAdjusted);

        return calendar.getTime();
    }


    /**
     * Gets the previous transfer object record.
     *
     * @param helper FLExportConfiguration
     * @param row ExportFormatRow
     * @return Transfer object record
     */
    private static TransferObjectRecord getPreviousTransferObjectRecord(FLExportConfiguration helper,
                                                                        ExportFormatRow row) {
        TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
        Collection<TransferObjectRecord> records = getTransferObjectRecords(validatedRecord);
        TransferObjectRecord previousRecord = null;
        if (validatedRecord != null && records != null) {
            for (TransferObjectRecord record : records) {
                if (record != null && !record.getRecordType().startsWith(TransferObjectRecord.RECORD_TYPE_99)) {
                    if (validatedRecord.getOid().equals(record.getOid())) {
                        break;
                    }
                    previousRecord = record;
                }
            }
        }
        return previousRecord;
    }


    /**
     * Gets the previous transfer object record with type.
     *
     * @param helper FLExportConfiguration
     * @param row ExportFormatRow
     * @param type String
     * @return Transfer object record
     */
    private static TransferObjectRecord getPreviousTransferObjectRecordWithType(FLExportConfiguration helper,
                                                                                ExportFormatRow row,
                                                                                String type) {
        TransferObjectRecord validatedRecord = getTransferObjectRecord(helper, row);
        Collection<TransferObjectRecord> records = getTransferObjectRecords(validatedRecord);
        TransferObjectRecord previousRecordWithType = null;
        if (validatedRecord != null && records != null) {
            for (TransferObjectRecord record : records) {
                if (record != null) {
                    if (validatedRecord.getOid().equals(record.getOid())) {
                        break;
                    }
                    if (record.getRecordType().equals(type)) {
                        previousRecordWithType = record;
                    }
                }
            }
        }
        return previousRecordWithType;
    }


    /**
     * Gets the transfer object record.
     *
     * @param helper FLExportConfiguration
     * @param row ExportFormatRow
     * @return Transfer object record
     */
    private static TransferObjectRecord getTransferObjectRecord(FLExportConfiguration helper, ExportFormatRow row) {
        TransferObjectRecord record = null;
        if (helper != null && row != null && helper instanceof FLFasterExportConfiguration) {
            TransferObjectHelper transferObjectHelper =
                    ((FLFasterExportConfiguration) helper).getTransferObjectHelper();
            if (transferObjectHelper != null) {
                record = transferObjectHelper.getTransferObjectRecord(row);
            }
        }
        return record;
    }


    /**
     * Gets the transfer object records.
     *
     * @param record TransferObjectRecord
     * @return Collection
     */
    private static Collection<TransferObjectRecord> getTransferObjectRecords(TransferObjectRecord record) {
        Collection<TransferObjectRecord> records = null;
        if (record != null) {
            StudentTransferObject studentTransfer = record.getStudentTransfer();
            if (studentTransfer != null) {
                records = studentTransfer.getRecords();
            }
        }
        return records;
    }


    /**
     * Initialize rule associations.
     */
    private void initializeRuleAssociations() {
        if (m_ruleAssociations == null) {
            m_ruleAssociations = new ArrayList<>();

            initializeRuleAssociations00();
            initializeRuleAssociations01();
            initializeRuleAssociations02();
            initializeRuleAssociations03();
            initializeRuleAssociations04();
            initializeRuleAssociations05();
            initializeRuleAssociations06();
            initializeRuleAssociations07();
            initializeRuleAssociations08();
            initializeRuleAssociations09();
            initializeRuleAssociations10();
            initializeRuleAssociations11();
            initializeRuleAssociations99ATV();
            initializeRuleAssociations99HC();
            initializeRuleAssociations99HS();
            initializeRuleAssociations99IMM();

            RuleAssociation ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "4.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_10, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_11, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "2.8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "2.8");
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(new RuleSet(ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.patternForFields("^[ -~]*$", true,
                                    FLFasterExportConfiguration.FIELD_FILLER_LOCAL_USE))),
                            "Non-displayable characters cannot be sent in data with the exception of Filler Reserved for Local Use"));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "11");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "7");
            ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_LAST_NAME, PATTERN_NOT_EMPTY,
                    "Last Name must not be blank."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "23.2");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99, "3");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "2.5");
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(
                            ValidationRule
                                    .testIf(Restriction.and(
                                            new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                    Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^0{5}99$")),
                                            new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                    Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^0{4}$"))))
                                    .testThen(Restriction.patternForFields("^[^~^`]+$", true))),
                    "If addressee is a SPEEDE/ExPRESS institution, the special characters tilde (~), caret (^), and grave accent (`) "
                            + "cannot be sent in the data."));
            m_ruleAssociations.add(ruleAssociation);



            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_10, "1");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_11, "2");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(new RestrictionHeaderRecordType())),
                    "If the previous header record's Record Type was not I00 (if this is a type I record) or S00 "
                            + "(if this is a Type S record), there is an error in this record's Record Type."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "2.5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "3");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(new RestrictionHeaderMessageType())),
                    "This record is invalid if it follows an I/S00 Header Record with a Message Type of S12."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "4");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "6");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(s_sendingNotSpeedeExpress)
                            .testThen(new RestrictionCompareRecordField(FIELD_DISTRICT_NUMBER_CE, null,
                                    FL_EXPORT.RECORD00,
                                    FIELD_SENDING_DST_NUM, new ValueAdjuster() {
                                        @Override
                                        public String getAdjustedValue(String value, FLExportConfiguration helper) {
                                            return value.substring(5);
                                        }
                                    })),
                    ValidationRule
                            .testIf(s_sendingIsSpeedeExpress)
                            .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CE, PATTERN_ALL_ZEROS))),
                    "District Number, Current, must be the same as that found in the Sending Institution (District/College) "
                            + "field on the last header record. If, however, the Sending Institution was a postsecondary or "
                            + "SPEEDE/ExPRESS institution, this field must be zero."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "5");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "7");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(s_sendingNotSpeedeExpress)
                            .testThen(new RestrictionCompareRecordField(FIELD_SCHOOL_NUMBER_CE, FL_EXPORT.RECORD00,
                                    FIELD_SENDING_SKL_NUM)),
                    ValidationRule.testIf(s_sendingIsSpeedeExpress)
                            .testThen(Restriction.pattern(FIELD_SCHOOL_NUMBER_CE, PATTERN_ALL_ZEROS))),
                    "School Number, Current, must be the same as that found in the Sending Institution (School/Campus) field "
                            + "on the last header record. If, however, the Sending Institution was a postsecondary or "
                            + "SPEEDE/ExPRESS institution, this field must be zero."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "6");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_09, "8");
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_11, "7");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule.testIf(Restriction.alwaysTrue())
                            .testThen(
                                    new RestrictionCompareRecordField(FIELD_STD_NUM_ID_FL, null, FL_EXPORT.RECORD00,
                                            FIELD_STD_NUM_ID_FL, new ValueAdjuster() {
                                                @Override
                                                public String getAdjustedValue(String value,
                                                                               FLExportConfiguration helper) {
                                                    value = value.trim();
                                                    return StringUtils.leftPad(value, 10, "0");
                                                }
                                            }))),
                    "Student Number Identifier, Florida , must be the same as the Student Number Identifier, Florida , "
                            + "from the last header record."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99, "1");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "1");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "1");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "1");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "1");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                    .testIf(Restriction.alwaysTrue())
                    .testThen(new RestrictionPreviousRecordType(TransferObjectRecord.RECORD_TYPE_00, false))),
                    "This record is out of sequence if there was no previous header record."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99, "2");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "2");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "2");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "2");
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "2");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                    .testIf(Restriction.alwaysTrue())
                    .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                            Restriction.pattern(FIELD_MESSAGE_TYPE,
                                    "^(?!(R0[1-9]|R1[0-2]|R9[56])|(B0[0-9]|B1[0-2])).*$")))),
                    "This record must not follow a header record that is a request "
                            + "(Message Type of R01-R12, R95, R96 B00-B12)."));
            m_ruleAssociations.add(ruleAssociation);
        }
    }


    /**
     * Initialize rule associations 00.
     */
    private void initializeRuleAssociations00() {
        String formatId = FL_EXPORT.RECORD00.getProcedureId();
        // we don't need postsecondary transcripts implementation
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "1");
        ruleAssociation.setProcessor(new ValidateRegularExpression(
                FLFasterExportConfiguration.FIELD_RECORD_TYPE,
                "^I00|S00$",
                "Record Type I00 is valid for school districts only. "
                        + "Record Types S00 and P00 may be used by both school districts and postsecondary/technical institutions. "
                        + "Record Type P00 is invalid if the addressed institution is 95 (Bright Futures)."));
        m_ruleAssociations.add(ruleAssociation);

        // we don't need postsecondary transcripts implementation
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "2.5");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM,
                                        "^0{5}99$"),
                                Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM,
                                        "^0{4}$")))
                        .testThen(Restriction.or(
                                Restriction.pattern(FLFasterExportConfiguration.FIELD_STD_NUM_ID_FL,
                                        PATTERN_NOT_EMPTY),
                                Restriction.pattern(FLFasterExportConfiguration.FIELD_INSTNAL_STD_NUM,
                                        PATTERN_NOT_EMPTY)))),
                        "If addressed institution code identifies a SPEEDE/ExPRESS institution, i.e., "
                                + "either 99 (with school code 0000) or 10002, then either a Primary Student Identifier or "
                                + "an Institutional Student Identifier is required. "));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.patternForFields("^[\\d]*$",
                                FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM,
                                FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM))),
                "Addressed Institution (all 11 characters, including school/campus number) cannot contain nonnumeric characters."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "4");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DISTRICT_CODES,
                                formatId,
                                FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM))),
                        "Addressed Institution (the first 7 characters) must be a valid institution that is participating in the Student Record Transfer System"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "5");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I00$"))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DISTRICT_CODES,
                                formatId,
                                FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM))),
                        "Addressed Institution must be a school district or \"99\" (MSIX Migrant Student Record) if Record Type is I00."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "5.5");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.byFormattedReferenceCodes(
                                        REFERENCE_TABLE_NAME_DISTRICT_CODES,
                                        formatId,
                                        FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM))
                                .testThen(Restriction.byFormattedReferenceCodes(
                                        REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                        formatId,
                                        FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM)),
                        ValidationRule
                                .testIf(Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM,
                                        "^0{4}$"))
                                .testThen(
                                        Restriction.or(
                                                Restriction.pattern(FLFasterExportConfiguration.FIELD_MESSAGE_TYPE,
                                                        "^S(0[1-9]|1[0-7])$"),
                                                Restriction.and(
                                                        Restriction.pattern(
                                                                FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM,
                                                                START + PATTERN_BRIGHT_FUTURES + END),
                                                        Restriction.pattern(
                                                                FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM,
                                                                "^0{4}$"))))),
                        "If the record is addressed to a school district (first 7 characters of Addressed Institution), "
                                + "then the school number (last 4 characters of Addressed Institution) must be a valid ID from "
                                + "the Florida Public Schools Master School ID File (see Appendix B). "
                                + "A school number of 0000 is valid only if Message Type is S01-S17 unless addressed to Talented Twenty, "
                                + "in which case school number must be 0000."));
        m_ruleAssociations.add(ruleAssociation);

        // 1.we don't need postsecondary institutions functionality, so ignore postsecondary
        // institutions requirements
        // 2.looks like there is a conflicting requirements for Talented Twenty: according
        // to the specification "A transcript addressed to Talented Twenty must have an
        // addressed institution of 95 with a school number that conforms to the school
        // number values required for Bright Futures" but rule 5.5 says it must be 0000.
        // At the same time rule 5.6 says "If the Addressed Institution is Bright Futures
        // District 95, then the first two characters must be numeric and represent a
        // year...". So exclusion for Talented Twenty is added for 5.6 for now.
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "5.6");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM,
                                                START + PATTERN_BRIGHT_FUTURES + END),
                                        Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM,
                                                "^(?!0{4})[\\d]{4}$"))) // exclusion for
                                                                        // Talented Twenty
                                .testThen(
                                        Restriction.pattern(FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM,
                                                "^0[4-9][78][01]$"))),
                        "If the record is addressed to a postsecondary institution (first 7 characters of Addressed Institution), "
                                + "then the campus number (last 4 characters of Addressed Institution) must be a valid "
                                + "campus ID (see Appendix C) or 0000. "
                                + "If the Addressed Institution is Bright Futures District 95, "
                                + "then the first two characters must be numeric and represent a year (without century) "
                                + "not less than 2004 nor greater than 2009, the third character must be either 7 or 8, "
                                + "and the fourth character must be 0 or 1."));
        m_ruleAssociations.add(ruleAssociation);

        // 5.7 is deleted

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.patternForFields("^[\\d]*$",
                                FLFasterExportConfiguration.FIELD_SENDING_DST_NUM,
                                FLFasterExportConfiguration.FIELD_SENDING_SKL_NUM))),
                "Sending Institution (all 11 characters, including school/campus number) cannot contain nonnumeric characters."));
        m_ruleAssociations.add(ruleAssociation);

        // it's unclear how to get agreement of other institution, so check only if it's the
        // user's institution id, i.e. district number
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "7");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(
                        new RuleSet(ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(
                                        new RestrictionFormattedString(
                                                Operator.EQUALS,
                                                FLFasterExportConfiguration.FIELD_SENDING_DST_NUM,
                                                FL_EXPORT.RECORD00.getProcedureId(),
                                                FLFasterExportConfiguration.FIELD_SENDING_DST_NUM,
                                                new RuntimeParam(RuntimeParam.DISTRICT_NUMBER)))),
                        "Sending Institution (the first 7 characters) must agree with the USER's institution-id, "
                                + "except when the user's id is one that is authorized to transmit data for other institutions. "
                                + "In this case, the Sending Institution would have to agree with one of the institution numbers "
                                + "associated with that user id number."));
        m_ruleAssociations.add(ruleAssociation);

        // 1.we don't need postsecondary institutions functionality, so ignore postsecondary
        // institutions requirements
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "7.5");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.byFormattedReferenceCodes(
                                        REFERENCE_TABLE_NAME_DISTRICT_CODES,
                                        formatId,
                                        FIELD_SENDING_DST_NUM))
                                .testThen(Restriction.byFormattedReferenceCodes(
                                        REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                        formatId,
                                        FIELD_SENDING_SKL_NUM)),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_SENDING_DST_NUM,
                                        START + PATTERN_BRIGHT_FUTURES + END))
                                .testThen(Restriction.pattern(FIELD_SENDING_SKL_NUM,
                                        "^(0[2-9]|1[0-2])[78][01]$"))),
                        "Sending Institution (the last 4 characters) must be a valid ID from the Florida Public Schools Master School ID File "
                                + "(see Appendix B) if the Sending Institution (the first 7 characters) is a school district; otherwise, "
                                + "it must be a valid campus number for this postsecondary institution (see Appendix C). In either case "
                                + "it may also be zero. "
                                + "If the Sending Institution is Bright Futures District 95, then the first two characters must be numeric and "
                                + "represent a year (without century) not less than 2002 nor greater than 2012, the third character must be "
                                + "either 7 or 8, and the fourth character must be 0 or 1."));
        m_ruleAssociations.add(ruleAssociation);

        // 7.6 is deleted

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "8");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_MESSAGES_CODES,
                                        formatId,
                                        FIELD_MESSAGE_TYPE)),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^S14$"))
                                .testThen(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I00).*$")),
                        ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern(FIELD_MESSAGE_TYPE,
                                        "^(?!R0[34]|S(0[5]|2[0-2]|3[0-2])).*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^Q20$"))
                                .testThen(Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                        START + PATTERN_BRIGHT_FUTURES + END)),
                        ValidationRule
                                .testIf(s_addressedToMsix)
                                .testThen(new RestrictionTransferObject(TransferObject.ALIAS_TNR_TYPE,
                                        TransferObject.TRANSFER_TYPE_RESPONSE))),
                        "Message Type must be R01-R12, R20, R30, R95 and R96, B00-B12, S01-S18, S20-S22, S30-S32, Q01-Q05, Q13, Q14, Q20. "
                                + "If Message Type is S14, Record Type cannot be I00. "
                                + "If the sending institution is a district, Message Types R03, R04, S05, S20-S22, and S30-S32 are invalid. "
                                + "Message Type X01 is only valid when sent by the FASTER system itself to indicate a failed attempt to send "
                                + "a student record. [NOTE: Rejected request records do not generate an X01 message.] "
                                + "Message type Q20 is only valid when the Addressed Institution is 95, Bright Futures. "
                                + "Requests cannot be sent to MSIX."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "8.5");
        ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_TEST_PROD_IND, "^T|P| $",
                "Test/Production Indicator must be T, P or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "9");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, PATTERN_NOT_EMPTY))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^[\\d]{9}.*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^[\\d]{9}[^XG].*$"))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL,
                                        "^(?!00|77)[0-7][0-9][\\d]{8}.*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^.{9}[X].*$"))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^(?!0{3}).*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^[9]{9}G$"))
                                .testThen(
                                        Restriction.and(
                                                new RestrictionTransferObject(TransferObject.ALIAS_TNR_TYPE,
                                                        TransferObject.TRANSFER_TYPE_RESPONSE),
                                                Restriction.pattern(FIELD_SENDING_DST_NUM, "^0{5}99$"))),
                        ValidationRule
                                .testIf(Restriction.and(
                                        new RestrictionTransferObject(TransferObject.ALIAS_TNR_TYPE,
                                                TransferObject.TRANSFER_TYPE_RESPONSE),
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^[IS]00$"),
                                        Restriction.pattern(FIELD_MESSAGE_TYPE,
                                                "^(?!S(0[2-4,89]|1[0,5-7])).*$")))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL, PATTERN_NOT_EMPTY))),
                        "If Student Number Identifier, Florida , is not blank, its first 9 characters must be numeric. "
                                + "If its tenth character does not contain the letter \"X\" or \"G\", its tenth character must "
                                + "be numeric and, in this case, its first 2 characters must be 01-76 or 78-79. "
                                + "If the tenth character does contain an \"X\", then the first three characters cannot be all zeroes. "
                                + "On a response, if the sending institution is (0000099) the value can also be '999999999G'. "
                                + "On a response, this field must not be blank if Record Type is I00 or S00 and Message Type is"
                                + " NOT one of S02, S03, S04, S08, S10, S15, S16 or S17."));
        m_ruleAssociations.add(ruleAssociation);

        // 1.we don't need postsecondary institutions functionality, so ignore postsecondary
        // institutions requirements
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "17");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_DATE_OF_BIRTH)),
                        ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.and(
                                        Restriction.greaterThan(FIELD_DATE_OF_BIRTH,
                                                getDate(8, 14, 1889),
                                                "yyyyMMdd"),
                                        Restriction.lessThan(FIELD_DATE_OF_BIRTH,
                                                getDateWithCorrectedYear(new Date(), -3),
                                                "yyyyMMdd")))),
                "Date of Birth must contain a valid date (CCYYMMDD) later than 9/14/1889 and earlier than 3 years before "
                        + "the current date. If Record Type is P00 and Message Type begins with either the letter S or the letter Q, "
                        + "then Date of Birth may also be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "18");
        ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_GENDER, "^F|M| $",
                "If Sex is not blank, it must be F or M."));
        m_ruleAssociations.add(ruleAssociation);

        // 1.we don't need postsecondary institutions functionality, so ignore postsecondary
        // institutions requirements
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction
                                        .and(Restriction.pattern(FIELD_RACIAL_ETHNIC_CAT, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(FIELD_RECORD_TYPE, "^S00$"),
                                                Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(0[2-48]|1[01])$")))
                                .testThen(Restriction.pattern(FIELD_RACIAL_ETHNIC_CAT, "^[WBHAMIOEZ]$")),
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_RACIAL_ETHNIC_CAT, PATTERN_NOT_EMPTY),
                                        Restriction.or(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S00).*$"),
                                                Restriction.pattern(FIELD_MESSAGE_TYPE,
                                                        "^(?!S(0[2-48]|1[01])).*$"))))
                                .testThen(Restriction.pattern(FIELD_RACIAL_ETHNIC_CAT, "^[WBHAMI]$"))),
                "If Racial/Ethnic is not blank, it must be W, B, H, A, M or I. If Record Type is P00, -OR- Record Type is S00 "
                        + "and Message Type is S02-S04, S08, S10 or S11, then codes O, E and Z are also valid."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.patternForFields("^Y|N| $",
                                        FIELD_ETHNICITY,
                                        FIELD_RACE_AMERICAALASKAINDIAN,
                                        FIELD_RACE_ASIAN,
                                        FIELD_RACE_AFRICAN_BLACK,
                                        FIELD_RACE_PACIFIC_ISLANDER,
                                        FIELD_RACE_WHITE))),
                "If Ethnicity is not blank, it must be Y or N.\n" +
                        "Race: If American Indian or Alaska Native is not blank, it must be Y or N.\n" +
                        "Race: If Asian is not blank, it must be Y or N.\n" +
                        "Race: If Black or African American is not blank, it must be Y or N.\n" +
                        "Race: If Native Hawaiian or Other Pacific Islander is not blank, it must be Y or N.\n"
                        +
                        "Race: If White is not blank, it must be Y or N."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19.3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(s_sendingIsSpeedeExpress)
                                .testThen(Restriction.pattern(FIELD_ID_TYPE,
                                        "^28|30|4[89A]|5[067]|C0|F8|LR|MV$"))),
                "If the sending institution is a SPEEDE/ExPRESS institution and the Unique Institutional Identifier "
                        + "is not blank, then the Type of Unique Institutional Identifier cannot be blank and must have a value "
                        + "of 28, 30, 48, 49, 4A, 50, 56, 57, C0, F8, LR, or MV."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19.4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(s_sendingIsSpeedeExpress)
                                .testThen(Restriction.pattern(FIELD_SENDING_INST_ID, PATTERN_NOT_EMPTY))),
                "If the sending institution is a SPEEDE/ExPRESS institution and the Type of Unique Institutional "
                        + "Identifier is not blank, then the Unique Institutional Identifier cannot be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_HS_GRADUATION_DATE, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(FIELD_HS_GRADUATION_DATE, PATTERN_NOT_ALL_ZEROS)))
                                .testThen(Restriction.and(
                                        Restriction.byDateFormat("MMyyyy", FIELD_HS_GRADUATION_DATE),
                                        Restriction.lessThan(FIELD_HS_GRADUATION_DATE, new Date(), "MMyyyy")))),
                "If High School Graduation Date is not blank or zeroes, it must be numeric and a valid date "
                        + "of the form MMCCYY which is not in the future."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "19.7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(0[1-9]|1[0-7])$"))
                                .testThen(new RestrictionHeaderSectionBFilled())),
                "If Message Type is S01 - S17 then Section B (bytes 371 - 605) must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        // We don't need postsecondary transfers
        //
        // 19.9)
        //
        // If Message Type is R01 - R12, R20 or R30, and Record Type is P00 the record must
        // be
        // addressed to a postsecondary institution.

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "20");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE,
                                        "^S(0[1579]|1[34])|Q(0[1-4]|1[34])$"))
                                .testThen(new RestrictionRecordMustFollow(Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_01,
                                        TransferObjectRecord.RECORD_TYPE_02,
                                        TransferObjectRecord.RECORD_TYPE_03,
                                        TransferObjectRecord.RECORD_TYPE_04,
                                        TransferObjectRecord.RECORD_TYPE_05,
                                        TransferObjectRecord.RECORD_TYPE_06,
                                        TransferObjectRecord.RECORD_TYPE_08,
                                        TransferObjectRecord.RECORD_TYPE_09,
                                        TransferObjectRecord.RECORD_TYPE_10,
                                        TransferObjectRecord.RECORD_TYPE_11)))),
                "If Message Type is S01, S05, S07, S09, S13, S14, Q01-Q04, Q13 or Q14, then at least one "
                        + "record must follow this Header Record in this transcript which is neither a comment record "
                        + "(I/S07 or P04) nor a pass-through record (G99)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "21");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE,
                                        "^S(0[2-48]|1[01])$"))
                                .testThen(Restriction.and(
                                        new RestrictionByRecordTypes(
                                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_00,
                                                        TransferObjectRecord.RECORD_TYPE_07,
                                                        TransferObjectRecord.RECORD_TYPE_99,
                                                        TransferObjectRecord.RECORD_TYPE_99ATV,
                                                        TransferObjectRecord.RECORD_TYPE_99HS,
                                                        TransferObjectRecord.RECORD_TYPE_99HC,
                                                        TransferObjectRecord.RECORD_TYPE_99IMM)),
                                        new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_07,
                                                3)))),
                "If Message Type is S02-S04, S08, S10 or S11, then only comment records (up to 3 I/S07 or any number of P04 records) "
                        + "or pass-through records (any number of G99 records) can follow this Header Record in this transcript."));
        m_ruleAssociations.add(ruleAssociation);

        // 21.5 edit is deleted

        // We don't have MSID file, so just restrict by school codes reference table
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "21.7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^R(0[1-9]|1[0-2])$"))
                                .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                        formatId,
                                        FIELD_SENDING_SKL_NUM))),
                "If Message Type is R01-R12 and sending institution is a district, then the school number must be either 9100 "
                        + "(Human Resources Center) or be flagged as active, future school or temporarily inactive on the MSID file."));
        m_ruleAssociations.add(ruleAssociation);

        // Sending institution cannot be 95, Bright Futures?
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "21.8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^R9[56]$"))
                                .testThen(Restriction.pattern(FIELD_SENDING_DST_NUM,
                                        START + PATTERN_BRIGHT_FUTURES + END))),
                "If Message Type is R95 or R96, the sending institution must be 95, Bright Futures."));
        m_ruleAssociations.add(ruleAssociation);

        // Sending institution cannot be 95, Bright Futures Office?
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "21.9");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^B(0[0-9]|1[0-2])$"))
                                .testThen(Restriction.pattern(FIELD_SENDING_DST_NUM,
                                        START + PATTERN_BRIGHT_FUTURES + END))),
                "If Message Type is B00-B12 then sending institution must be the Bright Futures Office "
                        + "(Institution ID 0000095)."));
        m_ruleAssociations.add(ruleAssociation);

        // 1.we don't need postsecondary institutions functionality, so ignore postsecondary
        // institutions requirements
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "23");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.or(
                                        Restriction.and(
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^0{5}99$"),
                                                Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^0{4}$")),
                                        s_sendingIsSpeedeExpress))
                                .testThen(
                                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SPEEDE_EXPRESS_CODES,
                                                formatId,
                                                FIELD_SPEEDE_EXPRESS_INSTITUTION_ID)),
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.or(
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^(?!0{5}99).*$"),
                                                Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^(?!0{4}).*$")),
                                        s_sendingNotSpeedeExpress))
                                .testThen(Restriction.pattern(FIELD_SPEEDE_EXPRESS_INSTITUTION_ID,
                                        PATTERN_EMPTY))),
                "If Addressed or Sending Institution is 00000990000, then the Part A SPEEDE/ExPRESS Institution ID, "
                        + "item 16b, must be a valid PK-12 institution participating in the SPEEDE/ExPRESS national electronic "
                        + "transcript system; otherwise, it must be blank. "
                        + "If Addressed or Sending Institution is 00100020000, then the Part A SPEEDE/ExPRESS Institution ID, "
                        + "item 16b, must be a valid postsecondary institution participating in the SPEEDE/ExPRESS national electronic "
                        + "transcript system; otherwise, it must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "25");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^0{5}99$"),
                                        Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^0{4}$"),
                                        Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(0[1-9]|1[0-7])$")))
                                .testThen(Restriction.pattern(FIELD_TRANSACTION_ID_B, PATTERN_NOT_EMPTY))),
                "If addressed institution code identifies a SPEEDE/ExPRESS institution (i.e., either 99 or 10002) "
                        + "and the transaction is a response to a request (Message Types S01 through S17), "
                        + "then the Part B SPEEDE/ExPRESS Transaction Control ID, item 32a, must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "26");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^0{5}99$"),
                                        Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^0{4}$"),
                                        Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(0[1-9]|1[0-7])$")))
                                .testThen(Restriction.pattern(FIELD_SPEEDE_EXPRESS_INSTITUTION_ID_B,
                                        PATTERN_NOT_EMPTY))),
                "If addressed institution code identifies a SPEEDE/ExPRESS institution (i.e., either 99 or 10002) "
                        + "and the transaction is a response to a request (Message Types S01 through S17), then the Part B "
                        + "SPEEDE/ExPRESS Institution ID, item 32b, must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "27");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(0[1-9]|1[0-7])$"))
                                .testThen(Restriction.pattern(FIELD_TRANSMIT_DATE_B, PATTERN_NOT_EMPTY))),
                "If the transaction is a response to a request (Message Types S01 through S17), then the Part B Post Date, "
                        + "Post Time, Sequence Number, item 32c, must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "28");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                START + PATTERN_BRIGHT_FUTURES + END),
                                        Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^0{4}$")))
                                .testThen(Restriction.pattern(FIELD_RECORD_TYPE, "^S00|I00$"))),
                "Record type must be I00 or S00 for Talented Twenty."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "29");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                START + PATTERN_BRIGHT_FUTURES + END),
                                        Restriction.pattern(FIELD_ADDRESSED_SKL_NUM, "^(?!0{4}).*$")))
                                .testThen(Restriction.pattern(FIELD_RECORD_TYPE, "^S00|I00$"))),
                "Record type must be I00 or S00 for Bright Futures."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "30");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^S14$"))
                                .testThen(Restriction.pattern(FIELD_RECORD_TYPE, "^S00$"))),
                "Record type must be S00 for msg S14-TEACHER CERT."));
        m_ruleAssociations.add(ruleAssociation);

        // Rule 31: Header record missing on first transcript.- is not clear how to
        // implement
        // if needed at all.

        // Rule 32: Addressee can receive header records only - balance ignored. (This
        // applies only to addressee's with status = H). - is not clear how to implement

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "33");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(s_addressedToMsix)
                                .testThen(Restriction.pattern(FIELD_RECORD_TYPE, "^I00$"))),
                "Record type must be I00 for MSIX (Migrant Students) Addressees."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "34");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(s_addressedToMsix)
                                .testThen(Restriction.pattern(FIELD_MESSAGE_TYPE, "^Q01$"))),
                "Message type must be Q01 for transcripts addressed to MSIX."));
        m_ruleAssociations.add(ruleAssociation);

        // Rule 35: Record Type must be 'P00' for Cost of Attendance record (Message Type
        // 'S20', 'S21', 'S22') or Enrollment record (Message Type 'S30', 'S31', S32). -
        // postsecondary is not needed

        // Rule 36: If Message Type is S20 or S30, then at least one record must follow this
        // Header Record in this transcript. - postsecondary is not needed

        // Rule 37: If Message Type is S20 then only a P20 record type can follow this
        // Header
        // Record in this transcript. - postsecondary is not needed

        // Rule 38: If Message Type is S30 then only a P02 or a P03 record type can follow
        // this Header Record in this transcript. - postsecondary is not needed

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "39");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^S(18|2[12]|3[12])$"))
                                .testThen(Restriction.and(
                                        new RestrictionByRecordTypes(
                                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_00))))),
                "If Message Type is S18, S21, S22, S31 or S32, then no additional record types can follow this "
                        + "Header Record for this student in this transcript."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_00, "40");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MESSAGE_TYPE, "^R20$"))
                                .testThen(Restriction.byDateFormat("yyyyMM", FIELD_TERM_DESIGNATOR))),
                "Term Designator must be a valid year and month in the form CCYYMM. "
                        + "It is to be used only when the Message Type Code is equal to 'R20'."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 01.
     */
    private void initializeRuleAssociations01() {
        String formatId = FL_EXPORT.RECORD01.getProcedureId();
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(TransferObjectRecord.RECORD_TYPE_00))),
                "If the previous record was not a header record, this record is out of sequence."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "6.5");
        ruleAssociation.setProcessor(
                new FLValidationRuleSet(new RuleSet(
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^.*\\S.*$"))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^[\\d]{9}.*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^[\\d]{9}[^X].*$"))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL,
                                        "^(?!00|77)[0-7][0-9][\\d]{8}.*$")),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^.{9}[X].*$"))
                                .testThen(Restriction.pattern(FIELD_STD_NUM_ID_FL, "^(?!0{3}).*$"))),
                        "If not blank, the first 9 characters of Student Number Identifier-Alias, Florida , must be numeric. "
                                + "If its tenth character does not contain the letter \"X\", its tenth character must be numeric and, "
                                + "in this case, its first 2 characters must be 01-76 or 78-79. If the tenth character does contain an 'X', "
                                + "then the first three characters cannot be all zeroes."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO: what is addressee is MSIX ?
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "11");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(s_notAddressedToMsix)
                                .testThen(Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_DATE_OF_BIRTH),
                                        Restriction.greaterThan(FIELD_DATE_OF_BIRTH,
                                                getDate(8, 14, 1889),
                                                "yyyyMMdd"),
                                        Restriction.lessThan(FIELD_DATE_OF_BIRTH,
                                                getDateWithCorrectedYear(new Date(), -3), "yyyyMMdd")))),
                "Birth date must contain a valid date (CCYYMMDD) later than 9/14/1889 and earlier than 3 years before the current date, unless the addressee is MSIX."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "12");
        ruleAssociation.setProcessor(new ValidateRegularExpression(FIELD_GENDER, "^F|M$",
                "Sex must be M or F"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "13");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.or(
                                        Restriction.lessThan(FIELD_DIPLOMA_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.lessThan(FIELD_WDRAWAL_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.lessThan(FIELD_CERT_OF_COMPL_DATE,
                                                getDate(8, 1, 2009))))
                                .testThen(Restriction.pattern(FIELD_RACIAL_ETHNIC_CAT, "^W|B|H|A|I|M$"))),
                "If the Diploma Date -or- Withdrawal Date -or- Certificate of Completion Date is less than 08/01/2009, "
                        + "then the Racial/Ethnic Category must be W, B, H, A, I or M."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "13.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.or(
                                        Restriction.greaterThanOrEquals(FIELD_DIPLOMA_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.greaterThanOrEquals(FIELD_WDRAWAL_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.greaterThanOrEquals(FIELD_CERT_OF_COMPL_DATE,
                                                getDate(8, 1, 2009))))
                                .testThen(Restriction.patternForFields("^Y|N$",
                                        FIELD_ETHNICITY,
                                        FIELD_RACE_AMERICAALASKAINDIAN,
                                        FIELD_RACE_ASIAN,
                                        FIELD_RACE_AFRICAN_BLACK,
                                        FIELD_RACE_PACIFIC_ISLANDER,
                                        FIELD_RACE_WHITE))),
                "If the Diploma Date -or- Withdrawal Date -or- Certificate of Completion Date is not before "
                        + "08/01/2009 (new student), then Ethnicity must be Y or N. "
                        + "Each of the associated Race Code fields must contain a valid value of either 'Y' or 'N'. "
                        + "None of the five Race Code fields can be spaces.\n" +
                        "Race: American Indian or Alaska Native must be Y or N.\n" +
                        "Race: Asian must be Y or N.\n" +
                        "Race: Black or African American must be Y or N.\n" +
                        "Race: Native Hawaiian or Other Pacific Islander must be Y or N.\n" +
                        "Race: White must be Y or N."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "13.2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.or(
                                        Restriction.greaterThanOrEquals(FIELD_DIPLOMA_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.greaterThanOrEquals(FIELD_WDRAWAL_DATE,
                                                getDate(8, 1, 2009)),
                                        Restriction.greaterThanOrEquals(FIELD_CERT_OF_COMPL_DATE,
                                                getDate(8, 1, 2009))))
                                .testThen(Restriction.or(
                                        Restriction.equals(FIELD_ETHNICITY, "Y"),
                                        Restriction.equals(FIELD_RACE_AMERICAALASKAINDIAN, "Y"),
                                        Restriction.equals(FIELD_RACE_ASIAN, "Y"),
                                        Restriction.equals(FIELD_RACE_AFRICAN_BLACK, "Y"),
                                        Restriction.equals(FIELD_RACE_PACIFIC_ISLANDER, "Y"),
                                        Restriction.equals(FIELD_RACE_WHITE, "Y")))),
                "If the Diploma Date or Withdrawal Date or Certification of Completion Date is not before "
                        + "08/01/2009 (new student), then at least one race code must be Y."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "14");
        ruleAssociation.setProcessor("Migrant Status Term must be Z if Migrant Continuation of Services is A, B or C.",
                Restriction.pattern(FIELD_MIGRANT_CONTINUATION, "^(A|B|C)$"),
                Restriction.equals(FIELD_MIGRANT_STATUS_TERM, "Z"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "14.5");
        ruleAssociation.setProcessor("If Migrant Status Term is not blank it must be B, D, E, S, T, U, V, W, X or Z "
                + "(not edited if Record Type is S01). Starting with 2013-2014 school year "
                + "Migrant Status Term is required (cannot be blank) if being sent to "
                + "the MSIX Federal Migrant Program",
                ValidationRule.testIf(Restriction.and(
                        new RestrictionForRecordsType(TransferObject.RECORDS_TYPE_INTERDISTRICT),
                        Restriction.pattern(FIELD_MIGRANT_STATUS_TERM, PATTERN_NOT_EMPTY))).testThen(
                                Restriction.pattern(FIELD_MIGRANT_STATUS_TERM, "^(B|D|E|S|T|U|V|W|X|Z)$")),
                ValidationRule.testIf(s_addressedToMsix)
                        .testThen(Restriction.pattern(FIELD_MIGRANT_STATUS_TERM, PATTERN_NOT_EMPTY)));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "15");
        ruleAssociation.setProcessor("If Migrant Status Term field is 'Z', then Priority For Services "
                + "must also be 'Z'.",
                Restriction.equals(FIELD_MIGRANT_STATUS_TERM, "Z"),
                Restriction.equals(FIELD_MIGRANT_PRIORITY, "Z"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "16");
        ruleAssociation.setProcessor("If Migrant Status Term field is not 'Z' and not blank, "
                + "then Priority For Services must be either 'Y' or 'N'.",
                Restriction.pattern(FIELD_MIGRANT_STATUS_TERM, PATTERN_NOT_EMPTY),
                Restriction.or(
                        Restriction.equals(FIELD_MIGRANT_STATUS_TERM, "Z"),
                        Restriction.pattern(FIELD_MIGRANT_PRIORITY, "^(Y|N)$")));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "17");
        ruleAssociation.setProcessor("The Seal of Biliteracy designation must be G (gold), S (silver) "
                + "or B (both). Use Z or blank if not applicable.",
                Restriction.pattern(FIELD_BILITER_SEAL_DESIGN, "^(G|S|B|Z| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18");
        ruleAssociation.setProcessor("Grade Level must be PK, KG, 01 - 12, 23, 30 or 31",
                Restriction.pattern(FIELD_GRADE_LEVEL, "^(PK|KG|0[1-9]|1[0-2]|23|30|31)$"));
        m_ruleAssociations.add(ruleAssociation);

        Calendar currentMonthAndYear = Calendar.getInstance();
        currentMonthAndYear.set(currentMonthAndYear.get(Calendar.YEAR), currentMonthAndYear.get(Calendar.MONTH), 1);
        currentMonthAndYear.add(Calendar.MONTH, 1);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.1");
        ruleAssociation.setProcessor("If Communications, Date Passed for Graduation Purposes is not blank, "
                + "it must be a valid date (MMCCYY) not later than the current month and year.",
                Restriction.pattern(FIELD_GRAD_PURP_DATE_COMM, PATTERN_NOT_EMPTY),
                Restriction.and(
                        Restriction.byDateFormat(FIELD_GRAD_PURP_DATE_COMM),
                        Restriction.lessThan(FIELD_GRAD_PURP_DATE_COMM, currentMonthAndYear.getTime())));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.2");
        ruleAssociation.setProcessor("If Mathematics, Date Passed for Graduation Purposes is not blank, "
                + "it must be a valid date (MMCCYY) not later than the current month and year.",
                Restriction.pattern(FIELD_GRAD_PURP_DATE_MATH, PATTERN_NOT_EMPTY),
                Restriction.and(
                        Restriction.byDateFormat(FIELD_GRAD_PURP_DATE_MATH),
                        Restriction.lessThan(FIELD_GRAD_PURP_DATE_MATH, currentMonthAndYear.getTime())));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.3");
        ruleAssociation.setProcessor("Graduation Option must be 1, 4, 5, 6, 7, 8, A, B, N, Z or blank "
                + "for students who graduated after 2004. If the student graduated in 2014 or after, code 9 is valid. "
                + "Code 5 and 6 cannot be used after school year 2014-2015.If the student graduated in 2004 or before, "
                + "a code of 2 or 3 is also valid.",
                ValidationRule
                        .testIf(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.and(
                                                Restriction.greaterThan(FIELD_HS_GRADUATION_DATE,
                                                        getDate(Calendar.AUGUST, 1, 2004), "MMyyyy"),
                                                Restriction.lessThan(FIELD_HS_GRADUATION_DATE,
                                                        getDate(Calendar.AUGUST, 1, 2013), "MMyyyy"))))
                        .testThen(Restriction.pattern(FIELD_GRADUATION_OPTION, "^(1|[4-8]|A|B|N|Z| )$")),

                ValidationRule.testIf(
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.and(
                                        Restriction.greaterThan(FIELD_HS_GRADUATION_DATE,
                                                getDate(Calendar.AUGUST, 1, 2013), "MMyyyy"),
                                        Restriction.lessThan(FIELD_HS_GRADUATION_DATE,
                                                getDate(Calendar.AUGUST, 1, 2014), "MMyyyy"))))
                        .testThen(Restriction.pattern(FIELD_GRADUATION_OPTION, "^(1|[4-9]|A|B|N|Z| )$")),

                ValidationRule.testIf(
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.greaterThan(FIELD_HS_GRADUATION_DATE,
                                        getDate(Calendar.AUGUST, 1, 2014), "MMyyyy")))
                        .testThen(Restriction.pattern(FIELD_GRADUATION_OPTION, "^(1|4|[7-9]|A|B|N|Z| )$")),

                ValidationRule.testIf(
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.lessThan(FIELD_HS_GRADUATION_DATE,
                                        getDate(Calendar.AUGUST, 1, 2004), "MMyyyy")))
                        .testThen(Restriction.pattern(FIELD_GRADUATION_OPTION, "^([1-8]|A|B|N|Z| )$")));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(null, Arrays.asList(TransferObject.RECORDS_TYPE_INTERDISTRICT));
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.4");
        ruleAssociation.setProcessor("Minimum Exceptional Student Education Performance Standards Mastered "
                + "for Graduation Purposes, Date, must be a valid date (MMDDCCYY) not later than "
                + "the current date, or blank (not edited if Record Type is S01).",
                Restriction.or(
                        Restriction.pattern(FIELD_MIN_EXCEPT_DATE, PATTERN_EMPTY),
                        Restriction.and(
                                Restriction.byDateFormat(FIELD_MIN_EXCEPT_DATE),
                                Restriction.lessThan(FIELD_MIN_EXCEPT_DATE, Calendar.getInstance().getTime()))));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.5");
        ruleAssociation.setProcessor("National Merit Scholar must be S, F, Z or blank.",
                Restriction.pattern(FIELD_NTL_MERIT_SCHOLAR, "^(S|F|Z| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.6");
        ruleAssociation.setProcessor("National Achievement Scholar must be S, F, Z or blank.",
                Restriction.pattern(FIELD_NTL_ACHV_SCHOLAR, "^(S|F|Z| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.65");
        ruleAssociation.setProcessor("National Hispanic Scholar must be S, Z or blank.",
                Restriction.pattern(FIELD_NTL_HISPANIC_SCHOLAR, "^(S|Z| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.7");
        ruleAssociation.setProcessor("Migrant Summer Term must be S or blank.",
                Restriction.pattern(FIELD_MIGRANT_SUMMER_TERM, "^(S| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "18.8");
        ruleAssociation.setProcessor("Migrant Annual Term must be 3 or blank.",
                Restriction.pattern(FIELD_MIGRANT_ANNUAL_TERM, "^(3| )$"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "19");
        ruleAssociation.setProcessor("Street Address must not be blank.",
                Restriction.pattern(FIELD_ADDRESS_STREET, PATTERN_NOT_EMPTY));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "21");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_ADDRESS_CITY, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_ADDRESS_STATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_ADDRESS_ZIP, PATTERN_NOT_EMPTY)))),
                "City, State, Zip must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "21.3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)))
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_ADDRESS_CITY, PATTERN_NOT_EMPTY)))),
                "City cannot be blank if addressee is Bright Futures."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "21.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ADDRESS_STATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                formatId,
                                FIELD_ADDRESS_STATE))),
                "If State is not blank then it must be a valid state code."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "23");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DOB_VERIFICATION, "^[1-9T ]$"))),
                "Birth Date Verification must be 1 - 9, T or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "25");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.patternForFields("^[PGOASN ]$",
                                        FIELD_PARENT_GUARD_CODE_F,
                                        FIELD_PARENT_GUARD_CODE_M)))),
                "Female Parent/Guardian Code and Male Parent/Guardian Code must be P, G, O, A, S, N or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "25.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_PARENT_GUARD_CODE, "^[PGOASNFM ]$"))),
                "Parent/Guardian Code must be P, G, O, A, S, N, F, M or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "27");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S01).*$"),
                                s_notAddressedToMsix))
                        .testThen(Restriction.pattern(FIELD_HLTH_EXAM_SKL_ENTRY, "^[YTRNV]$"))),
                "Health Examination must be Y, T, R, N or V (not edited if Record Type is S01 or if addressee is Migrants/MSIX)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "27.2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S01).*$"))
                        .testThen(Restriction.pattern(FIELD_CRIT_HLTH_INFO_IND, "^[YN ]$"))),
                "Critical/Chronic Health Information - 911 Medical Alert must be Y, N or blank (not edited if Record Type is S01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "27.6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S01).*$"))
                        .testThen(Restriction.pattern(FIELD_HEARING_PROBLEMS, "^H[YTN]|ZZ| +$"))),
                "Screening for Hearing Problems code must be HY, HT, HN, ZZ or blank (not edited if Record Type is S01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "27.8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S01).*$"))
                        .testThen(Restriction.pattern(FIELD_VISION_PROBLEM, "^V[YTN]|ZZ| +$"))),
                "Screening for Vision Problems code must be VY, VT , VN, ZZ or blank (not edited if Record Type is S01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "31");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_STD_SCHOOL_NAME, PATTERN_NOT_EMPTY))),
                "School Name must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "32");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_STD_SCHOOL_ADDRESS_1, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_STD_SCHOOL_ADDRESS_2, PATTERN_NOT_EMPTY)))),
                "School Address must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "33");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_STD_SKL_PHONE_NUMBER, PATTERN_NOT_EMPTY))),
                "School Phone Number must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "36");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_WDRAWAL_CODE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_GRADE_LEVEL, "^(?!3[01]).*$")))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_WDRAW_CODES,
                                formatId, FIELD_WDRAWAL_CODE)),
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_WDRAWAL_CODE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_WDRAW_ADULTS_CODES,
                                formatId,
                                FIELD_WDRAWAL_CODE))),
                "Withdrawal Code must be a valid code listed in Appendix A of the "
                        + "Education Information & Accountability Services Student data base or blanks. "
                        + "(This field is edited for both record types I01 and S01). "
                        + "Adult code must be a valid code listed in Appendix B of the Education Information & "
                        + "Accountability Services Student data base or blanks."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "37");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_WDRAWAL_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_WDRAWAL_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(Restriction.byDateFormat("MMddyyyy", FIELD_WDRAWAL_DATE)),
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_WDRAWAL_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_WDRAWAL_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(Restriction.lessThan(FIELD_WDRAWAL_DATE,
                                new RuntimeParam(RuntimeParam.DATE_WITH_CURRENT_YEAR_PLUS_YEARS,
                                        getDate(Calendar.SEPTEMBER, 1), Integer.valueOf(2)),
                                "MMddyyyy"))),
                "If Withdrawal Date is not blanks or all zeroes, then it must be a valid date (MMDDCCYY). "
                        + "The Withdrawal Date can be a date in the future only if the Withdrawal date is before September "
                        + "1st of the upcoming school year. (This field is edited for both record types I01 and S01)."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "39");
            List<ValidationRule> validationRules01_39 = new ArrayList<ValidationRule>();
            for (String subject : s_fasterCreditsNeededSubjs) {
                String fieldName = FIELD_CREDS_NEEDED + subject;
                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(fieldName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(fieldName, "^[\\d]+$"));
                validationRules01_39.add(rule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules01_39.toArray(new ValidationRule[0])),
                    "If Credits Needed, Local Subject Area Requirements is not blank, it must be numeric and"
                            + "greater than or equal to zero."));
            m_ruleAssociations.add(ruleAssociation);
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "40");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_CREDS_NEEDED + "ALG 2", PATTERN_NOT_ALL_ZEROS),
                                Restriction.pattern(FIELD_CREDS_NEEDED + "ALG 2", PATTERN_NOT_EMPTY)))
                        .testThen(Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, "^20102011|20112012$"))),
                "Credits Needed, Local Subject Area Requirements - ALGEBRA II is only valid when year entered ninth grade is 2010-2011 or 2011-2012."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "43");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"))
                        .testThen(Restriction.pattern(FIELD_CLASS_RANK_NUM_POS, "^ +|[\\d]+$"))),
                "Class Rank, Number, must be blank or numeric and greater than or equal to zero (not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "44");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"))
                        .testThen(Restriction.pattern(FIELD_CLASS_RANK_PERCENT, "^ +|[\\d]+$"))),
                "Class Rank, Percentile, must be blank or numeric and greater than or equal to zero (not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "46");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"))
                        .testThen(Restriction.pattern(FIELD_GPA_DISTRICT, "^ +|[\\d]+$"))),
                "If District GPA is not blank, it must be numeric and greater than or equal to zero (not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "46.2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_MESSAGE_TYPE, "^Q20$"))))
                        .testThen(Restriction.pattern(FIELD_GPA_DST_CUMULATIVE, "^[\\d]+$"))),
                "If the addressed institution is 95 (Bright Futures) and the Message Type is Q20, then "
                        + "Grade Point Average District, Cumulative must be numeric and greater than zero."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "46.4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_MESSAGE_TYPE, "^Q20$"))))
                        .testThen(Restriction.pattern(FIELD_GPA_STATE_CUMULATIVE, "^(?!0{5}|[4-9])[\\d]+|40{4}$"))),
                "If the addressed institution is 95 (Bright Futures) and Message Type is Q20, then "
                        + "Grade Point Average State, Cumulative must be numeric and greater than zero "
                        + "and less than or equal to 4.0."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "47");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))
                        .testThen(Restriction.pattern(FIELD_GPA_STATE, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GPA_STATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_GPA_STATE, "^(?![4-9])[\\d]+|40{4}$"))),
                "If Grade Level is 09 - 12 then State GPA must not be blank. If State GPA is not blank, "
                        + "it must be numeric and greater than or equal to zero, and less than or equal to 4.0."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "48");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.or(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES))),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("MMddyyyy", FIELD_DIPLOMA_DATE),
                                Restriction.lessThanOrEquals(FIELD_DIPLOMA_DATE, new Date(), "MMddyyyy"))),
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.or(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES))),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_NOT_ALL_ZEROS),
                                s_sendingIsSpeedeExpress))
                        .testThen(Restriction.or(
                                Restriction.byDateFormat("0000yyyy", FIELD_DIPLOMA_DATE),
                                Restriction.byDateFormat("00ddyyyy", FIELD_DIPLOMA_DATE),
                                Restriction.byDateFormat("MM00yyyy", FIELD_DIPLOMA_DATE),
                                Restriction.byDateFormat("MMddyyyy", FIELD_DIPLOMA_DATE)))),
                "If Diploma Date is not blank or all zeroes, it must be a valid date (MMDDCCYY) not later than "
                        + "the current date (not edited if Record Type is I01 and addressed institution is not Bright Futures) "
                        + "unless the sending institution is a SPEEDE/ExPRESS institution, in which case the day and/or month "
                        + "can be zeroes."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "49");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_DIPLOMA_TYPE, PATTERN_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DIPLOMA_TYPE,
                                formatId,
                                FIELD_DIPLOMA_TYPE))),
                "If Diploma Date is blank, Diploma Type must be blank. Otherwise, it must be a valid diploma type code "
                        + "(found in Diploma Type)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "50");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_NOT_ALL_ZEROS),
                                        s_sendingNotSpeedeExpress))
                        .testThen(Restriction.byDateFormat("MMddyyyy", FIELD_CERT_OF_COMPL_DATE)),
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_NOT_ALL_ZEROS)),
                                s_sendingIsSpeedeExpress)
                        .testThen(Restriction.and(
                                Restriction.or(
                                        Restriction.byDateFormat("MMddyyyy", FIELD_CERT_OF_COMPL_DATE),
                                        Restriction.byDateFormat("00ddyyyy", FIELD_CERT_OF_COMPL_DATE),
                                        Restriction.byDateFormat("MM00yyyy", FIELD_CERT_OF_COMPL_DATE),
                                        Restriction.byDateFormat("0000yyyy", FIELD_CERT_OF_COMPL_DATE)))),
                ValidationRule
                        .testIf(Restriction.byDateFormat("MMddyyyy", FIELD_CERT_OF_COMPL_DATE))
                        .testThen(Restriction.lessThanOrEquals(FIELD_CERT_OF_COMPL_DATE, new Date(), "MMddyyyy")),
                ValidationRule
                        .testIf(Restriction.byDateFormat("00ddyyyy", FIELD_CERT_OF_COMPL_DATE))
                        .testThen(Restriction.lessThanOrEquals(FIELD_CERT_OF_COMPL_DATE, new Date(), "00ddyyyy")),
                ValidationRule
                        .testIf(Restriction.byDateFormat("MM00yyyy", FIELD_CERT_OF_COMPL_DATE))
                        .testThen(Restriction.lessThanOrEquals(FIELD_CERT_OF_COMPL_DATE, new Date(), "MM00yyyy")),
                ValidationRule
                        .testIf(Restriction.byDateFormat("0000yyyy", FIELD_CERT_OF_COMPL_DATE))
                        .testThen(Restriction.lessThanOrEquals(FIELD_CERT_OF_COMPL_DATE, new Date(), "0000yyyy"))),
                "If Certificate of Completion Date is not blank or all zeroes, it must be a valid date (MMDDCCYY) not later "
                        + "than the current date (not edited if Record Type is I01) unless the sending institution is a SPEEDE/ExPRESS "
                        + "institution, in which case the day and/or month can be zeroes."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "51");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, PATTERN_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_CERT_OF_COMPL_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, "^W(0[89]|8[ABC]|44|53|)|ZZZ$"))),
                "If Certificate of Completion Date is blank, Certificate of Completion Type must be blank. "
                        + "Otherwise, Certificate of Completion Type must be W08, W8A, W8B, W09, W44, W53, W8B, W8C or ZZZ "
                        + "(not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "51.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        Restriction.or(
                                                Restriction.pattern(FIELD_DIPLOMA_TYPE, "^(?!ZZZ).*$"),
                                                Restriction.pattern(FIELD_DIPLOMA_TYPE, PATTERN_NOT_EMPTY))))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, "^ZZZ$"),
                                Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, PATTERN_EMPTY))),
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                        Restriction.or(
                                                Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, "^(?!ZZZ).*$"),
                                                Restriction.pattern(FIELD_CERT_OF_COMPL_TYPE, PATTERN_NOT_EMPTY))))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_DIPLOMA_TYPE, "^ZZZ$"),
                                Restriction.pattern(FIELD_DIPLOMA_TYPE, PATTERN_EMPTY)))),
                "Diploma Type and Certificate of Completion Type are mutually exclusive. That is, if Diploma Type is not ZZZ or "
                        + "blank then Certificate of Completion Type must be ZZZ or blank. "
                        + "If Certificate of Completion Type is not ZZZ or blank then Diploma Type must be ZZZ or blank "
                        + "(not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "52");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_RECORD_TYPE, "^(?!I01).*$"),
                                Restriction.pattern(FIELD_CLASS_RANK_EFF_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_CLASS_RANK_EFF_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("MMddyyyy", FIELD_CLASS_RANK_EFF_DATE),
                                Restriction.lessThanOrEquals(FIELD_CLASS_RANK_EFF_DATE, new Date(), "MMddyyyy")))),
                "If Class Rank Effective Date is not blank or zeroes, it must be numeric and a valid date of the form MMDDCCYY "
                        + "which is not in the future (not edited if Record Type is I01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "53");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_NATIVE_LANGUAGE, PATTERN_NOT_EMPTY))
                        .testThen(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_LANG_CODES,
                                        formatId,
                                        FIELD_NATIVE_LANGUAGE))),
                "If Native Language, Student is not blank, it must be a valid language code as found in Appendix N of the "
                        + "Education Information & Accountability Services Student data base."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "54");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_HOME_LANG_SURV_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_HOME_LANG_SURV_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("yyyyMMdd", FIELD_HOME_LANG_SURV_DATE),
                                Restriction.lessThanOrEquals(FIELD_HOME_LANG_SURV_DATE, new Date(), "yyyyMMdd"))),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HOME_LANG_SURV_DATE, PATTERN_ALL_ZEROS))
                        .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$"))),
                "If LEP Home Language Survey Date is not blank, it must be a valid date (CCYYMMDD) not later than the current date. "
                        + "If Grade Level is 30 or 31 LEP Home Language Survey Date may also be zero."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "55");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COUNTRY_OF_BIRTH, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_COUNTRY_CODES,
                                formatId,
                                FIELD_COUNTRY_OF_BIRTH)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COUNTRY_OF_BIRTH, "^ZZ$"))
                        .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$"))),
                "Country of Birth must be a valid foreign country code. "
                        + "(See Appendix G of the Education Information & Accountability Services Student data base.) \n"
                        +
                        "Code ZZ is valid only for grade levels 30 and 31. \n"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "56");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_PRIMARY_LANGUAGE, PATTERN_NOT_EMPTY))
                        .testThen(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_LANG_CODES,
                                        formatId,
                                        FIELD_PRIMARY_LANGUAGE))),
                "If Parent/Guardian Primary Home Language is not blank, it must be a valid language code as found in "
                        + "Appendix N of the Education Information & Accountability Services Student data base."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "57");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRAD_REQ_BASIS, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.or(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                        formatId,
                                        FIELD_GRAD_REQ_BASIS),
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_COUNTRY_CODES,
                                        formatId,
                                        FIELD_GRAD_REQ_BASIS),
                                Restriction.pattern(FIELD_GRAD_REQ_BASIS, "^EX$")))),
                "Basis for Graduation must be a valid state, territory or foreign country code, EX or blank. "
                        + "(See Appendix G and Appendix Q of the Education Information & Accountability Services Student data base.)"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "59");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COMVOL_SERVICE_HRS, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_COMVOL_SERVICE_HRS, "^[\\d]+$"))),
                "If Community Service Hours is not blank, it must be numeric and greater than or equal to zero "
                        + "(not edited if Record Type is S01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "60");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_EVEN_START_FAMIL_LIT, "^[YNZ]$"))),
                "Even Start Family Literacy Program Participation must be Y, N or Z."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "61");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_FL_FIRST_START_PGM, "^[YNZ]$"))),
                "Florida First Start Program Participation must be Y, N or Z."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "63");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_PRIOR_TO_KINDERGART, PATTERN_EMPTY))),
                "This field was discontinued by the Student Information System in July 2012. "
                        + "The valid values at that time (C,F,D,H,I,L, M,N,O,P,S,T,V and Z) will continue to be allowed for transcript "
                        + "data collected prior to 2012-2013."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "64");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DIFF_DIPLOMA, "^[1Z]$"))),
                "Differentiated Diploma must be 1 or Z."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "65");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_IB_DIPLOMA, "^[YNZ]$")),
                ValidationRule
                        .testIf(Restriction.or(
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_EMPTY),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_ALL_ZEROS)))
                        .testThen(Restriction.pattern(FIELD_IB_DIPLOMA, "^[^Y]$"))),
                "International Baccalaureate Diploma must be Y, N or Z. "
                        + "If Diploma Date is blanks or all zeroes, International Baccalaureate Diploma cannot be Y."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO how to determine if school is valid for IB program?
        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "65.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IB_DIPLOMA, "^Y$"))
                        .testThen(Restriction.alwaysTrue())),
                "If International Baccalaureate Diploma is Y, the sending institution must be a valid school for the IB program."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "66");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COLLEGE_READY_DIPLOM, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_COLLEGE_READY_DIPLOM, "^[YNZ]$"))),
                "College Ready Diploma must be Y, N, Z , or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "67");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_AIC_PROGRAM_COMPLETE, "^[YNZ]$"))),
                "Advanced International Certificate Program must be Y, N or Z."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO how to determine if school is valid for AIC program?
        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "67.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_AIC_PROGRAM_COMPLETE, "^Y$"))
                        .testThen(Restriction.alwaysTrue())),
                "If Advanced International Certificate Program is Y, the sending institution must be a valid "
                        + "school for the AIC program."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "68");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^(?!S01).*$"))
                        .testThen(Restriction.pattern(FIELD_DST_COM_VOL_REQ_MET, "^[YNZ]$"))),
                "Community Service Requirement Met must be Y, N or Z (not edited if Record Type is S01)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "69");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)))
                        .testThen(Restriction.pattern(FIELD_EARLY_ADM_STUDENT, "^[YZ ]$"))),
                "If Addressed Institution is 95 (Bright Futures), then Early Admission Student must be Y, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "70");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^12$"))
                        .testThen(Restriction.pattern(FIELD_ADDL_SKL_YEAR, "^[DFSYZ ]$"))),
                "If Grade Level is 12, Additional School Year Student code must be D, F, S, Y, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "71");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_AICE_DIPLOMA, "^[YNZ ]$")),
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_EMPTY),
                                Restriction.pattern(FIELD_DIPLOMA_DATE, PATTERN_ALL_ZEROS)))
                        .testThen(Restriction.pattern(FIELD_AICE_DIPLOMA, "^[^Y]$"))),
                "Advanced International Certificate of Education Diploma must be Y, N, Z or blank. "
                        + "If Diploma Date is blanks or all zeroes, Advanced International Certificate of "
                        + "Education Diploma cannot be Y."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "72");
            List<ValidationRule> validationRules01_72 = new ArrayList<ValidationRule>();
            for (String subject : s_fasterCreditsNeededSubjs) {
                String fieldName = FIELD_CREDS_NEEDED + subject;
                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(fieldName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(fieldName, "^[\\d]+$"));
                validationRules01_72.add(rule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules01_72.toArray(new ValidationRule[0])),
                    "Local Subject area must be numeric or blank."));
            m_ruleAssociations.add(ruleAssociation);
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "73");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                s_addressedToMsix,
                                Restriction.pattern(FIELD_COUNTRY_OF_BIRTH,
                                        "^MX|CC|AE|BL|BR|CI|CL|EC|FA|FN|FG|PX|PE|SX|UY|VE$")))
                        .testThen(new RestrictionCompareByDependCode(FIELD_MIGRANT_BIRTH_STATE,
                                REFERENCE_TABLE_NAME_CAN_MEX_SOUTH_AMERICAN_STATE_CODES, FIELD_COUNTRY_OF_BIRTH)),
                ValidationRule
                        .testIf(Restriction.and(
                                s_addressedToMsix,
                                Restriction.pattern(FIELD_COUNTRY_OF_BIRTH, "^US$")))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                formatId,
                                FIELD_MIGRANT_BIRTH_STATE)),
                ValidationRule
                        .testIf(Restriction.and(
                                s_addressedToMsix,
                                Restriction.pattern(FIELD_MIGRANT_BIRTH_STATE, PATTERN_EMPTY)))
                        .testThen(Restriction.pattern(FIELD_COUNTRY_OF_BIRTH,
                                "^(?!US|MX|CC|AE|BL|BR|CI|CL|EC|FA|FN|FG|PX|PE|SX|UY|VE).*$"))),
                "Migrant Birth State - when Addressed Institution is MSIX (990006): \n" +
                        "If Country of Birth is Mexico (MX), Canada (CC) or one of the South American country codes, "
                        + "Migrant Birth State must be a valid code for the country indicated as found on Appendix BB of "
                        + "the Education Information & Accountability Services Student data base. \n"
                        +
                        "If Country of Birth is the United States (US), Migrant Birth State must be a valid U.S. state code as "
                        + "found on Appendix H of the Education Information & Accountability Services Student data base. \n"
                        +
                        "If Country of Birth is not US, MX, CC or one of the South American country codes -or- "
                        + "the Migrant Birth State is not known, Migrant Birth State may be or spaces."));
        m_ruleAssociations.add(ruleAssociation);

        // cannot be received from MSIX System if validations only for created transcripts?
        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "74");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MSIX_ID, "^[\\d]+ *$"))),
                "If a Migrant Identification Number is provided, it must be numeric and match the Institutional Student Number received"
                        + " for the Migrant student from the Federal MSIX System. If not provided, this field should be spaces."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "75");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(s_addressedToMsix)
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("MMddyyyy", FIELD_MIGRANT_ENR_DATE),
                                Restriction.lessThanOrEquals(FIELD_MIGRANT_ENR_DATE, new Date(), "MMddyyyy"),
                                Restriction.greaterThanOrEquals(FIELD_MIGRANT_ENR_DATE,
                                        new RuntimeParam(RuntimeParam.DATE_WITH_CURRENT_YEAR_PLUS_YEARS,
                                                getDate(com.ibm.icu.util.Calendar.SEPTEMBER, 1)),
                                        "MMddyyyy")))),
                "Beginning with 2013-2014 academic year, the MEP Enrollment Date is required if the transcript is being sent to the "
                        + "Federal MSIX System. "
                        + "It must not be blank or zeros, but must be numeric and a valid date which is not in the future "
                        + "and in the current school year."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "76");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.or(
                                Restriction.byDateFormat("yyyyyyyy", FIELD_YEAR_ENTERED_9_GRADE),
                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_EMPTY))),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))
                        .testThen(Restriction.byDateFormat("yyyyyyyy", FIELD_YEAR_ENTERED_9_GRADE))),
                "Year Entered Ninth Grade, Graduation Requirements Determination must be a valid year (biennium) or can be blank. "
                        + "It should be reported in the form CCYYCCYY. "
                        + "Beginning with the 2012-2013 school year, this field is required for all public school students "
                        + "enrolled in Grade Level 9 through 12."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "79");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_PHYS_EDU_WAIVER, "^[YNZ]$"))),
                "Physical Education Waiver must be Y, N or Z."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "80");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MULTIPLE_BIRTH_STD, "^[YNZ]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MULTIPLE_BIRTH_STD, "^[YNZ ]$"))),
                "Multiple Birth Student must be Y, N or Z for Migrant students. "
                        + "For non-migrant students, this field can be a space."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "81");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_CONTINUATION, "^[ABC]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_CONTINUATION, "^Z$"))),
                "Migrant Continuation of Services must be A, B or C for migrant students. "
                        + "Non-migrant students should be Z, and not blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "82");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_PRIORITY, "^[YN]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MSIX_ID, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_PRIORITY, "^Z$"))),
                "Migrant Priority For Services must be Y or N for migrant students. "
                        + "Non-migrant students should be Z, and not blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "83");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_ADULT_FEE_STATUS_1ST, "^[C-GI-KM-X] |AA|BB|CC|  $"))),
                "Adult Fee Status must be C-G, I-K, M-X, AA, BB OR CC. This field can be blank."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO RULE #84: MSIX Identification Number (MSIX ID) cannot be blank and must be valid for
        // students having been previously assigned an MSIX ID by the Federal MSIX System and the
        // MSIX ID returned to the district.

        // TODO RULE #84.1: MSIX Identification Number (MSIX ID) must be blank if the student has
        // been sent to the Federal MSIX System but no MSIX ID has been assigned to the student. .
        // This applies to students previously sent but have yet to be issued a MSIX ID.

        // TODO RULE #84.2: MSIX Identification Number (MSIX ID) must be blank if the student is
        // being sent to the Federal MSIX System by the district for the first time. . This applies
        // to students having never been sent to the Federal MSIX System by the district in a
        // previous submission.

        // TODO RULE #85: The student has been previously deactivated in the MSIX system. Reject-
        // Upon deactivation of the student in the MSIX system, a migrant status of M06 was provided
        // to the sending district via FASTER Response. Districts should update the students record
        // to indicate they are no longer served (see Item #14 on the FASTER format I01, Migrant
        // Status Term) by the Migrant Education Program (MEP) and should not be included in
        // subsequent transmissions to the MSIX system. Contact the State MEP Administrator if you
        // have further questions regarding this situation.

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "86");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RESID_TUITION_PURPOS, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_RESID_TUITION_PURPOS, "^[DFNZ]$"))),
                "If present, Residency for Tuition Purposes must be D, F, N or Z."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "87");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"))
                        .testThen(Restriction.pattern(FIELD_ONLINE_COURSE_EXEMPT, "^[DTZ]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, "^(?!09|1[0-2]).*$"))
                        .testThen(Restriction.pattern(FIELD_ONLINE_COURSE_EXEMPT, "^[DTZ ]$"))),
                "Beginning in the 2013-2014 school year, Online Course Exempt must be D, T or Z for students "
                        + "in grades 9 - 12. This field may be blank for all other students."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "88");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20092010"))
                        .testThen(Restriction.pattern(FIELD_DIPLOMA_DESIGNATION, "^[SMBZ]$")),
                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_YEAR_ENTERED_9_GRADE, "20092010"))
                        .testThen(Restriction.pattern(FIELD_DIPLOMA_DESIGNATION, "^[SMBZ ]$"))),
                "Diploma Designation must be S, M, B or Z starting with Year Entered Ninth Grade 20092010 or later. "
                        + "This field may be blank prior to Year Entered Ninth Grade 20092010."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "89");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_MET_ALG_1, "^[YNEZ]$")),
                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_MET_ALG_1, "^ $"))),
                "Algebra I Assessment Met must be Y, N, E or Z starting with Year Entered Ninth Grade 20102011 and after. "
                        + "This field must be blank prior to Year Entered Ninth Grade 20102011."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "90");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_GEOMET, "^[YE ]$")),
                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_GEOMET, "^ $"))),
                "Geometry Assessment Passed must be Y, E or spaces starting with Year Entered Ninth Grade 20102011 and after. "
                        + "This field must be blank prior to Year Entered Ninth Grade 20102011."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "91");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_BIOLOGY, "^[YE ]$")),
                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_BIOLOGY, "^ $"))),
                "Biology Assessment Passed must be Y, E or spaces starting with Year Entered Ninth Grade 20102011 and after. "
                        + "This field must be blank prior to Year Entered Ninth Grade 20102011."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "92");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_USHISTORY, "^[YE ]$")),
                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_YEAR_ENTERED_9_GRADE, "20102011"))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_USHISTORY, "^ $"))),
                "US History Assessment Passed must be Y, E or spaces starting with Year Entered Ninth Grade 20102011 and after. "
                        + "This field must be blank prior to Year Entered Ninth Grade 20102011."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "93");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.lessThan(FIELD_HS_GRADUATION_DATE, getDate(Calendar.SEPTEMBER, 1, 2017),
                                        "MMyyyy")))
                        .testThen(Restriction.pattern(FIELD_ASM_PASSED_ALG_2, "^[YE ]$"))),
                "Algebra II Assessment Passed must be Y, E or spaces starting with Year Entered Ninth Grade 20102011 and after. "
                        + "This field may be blank prior to Year Entered Ninth Grade 20102011. \n" +
                        "As of 7/1/2017, this field will no longer be edited for students graduating in "
                        + "school year 2017-2018 or later."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_01, "94");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_ASM_MET_ELA, "^[YNZ ]$"))),
                "English/Language Arts Assessment Met must be Y, N, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 02.
     */
    private void initializeRuleAssociations02() {
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(TransferObjectRecord.RECORD_TYPE_01))),
                "This record is out of sequence if it follows a record with a type other than I01/S01."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.or(
                        new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")),
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, Integer.valueOf(21), null,
                                        true,
                                        false)),
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^0{5}99$"))))
                        .testThen(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, "^[ 0-8]$")),
                ValidationRule.testIf(Restriction.and(
                        new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                Restriction.pattern(FIELD_GRADE_LEVEL, "^(?!3[01]).*$")),
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, null, Integer.valueOf(21),
                                        false,
                                        false)),
                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, "^(?!0{5}99).*$"))))
                        .testThen(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, "^[0-8]$"))),
                "If the student is in Grade Level 30 or 31 (adult) or is 21 years of age or older, or "
                        + "the addressee is MSIX or SPEEDE/ExPRESS, then Immunization Status may be blank. "
                        + "Otherwise Immunization Status must be 0 - 8."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule.testIf(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, "^[013-57]$"))
                        .testThen(Restriction.pattern(FIELD_VAC_CERT_EXP_DATE, PATTERN_ALL_ZEROS)),
                ValidationRule.testIf(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, "^[26]$"))
                        .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_VAC_CERT_EXP_DATE)),
                ValidationRule.testIf(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, "^6$"))
                        .testThen(
                                Restriction.lessThan(FIELD_VAC_CERT_EXP_DATE, getDate(Calendar.JULY, 15, 1993),
                                        "yyyyMMdd")),
                ValidationRule.testIf(Restriction.pattern(FIELD_IMMUNIZATION_STATUS, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_VAC_CERT_EXP_DATE, PATTERN_EMPTY))),
                "If Immunization Status is 0,1, 3 - 5 or 7 Vaccine Certification Expiration Date must be all zeroes. "
                        + "If Immunization Status is 2 or 6, Vaccine Certification Expiration Date must be a valid date (CCYYMMDD). "
                        + "If Immunization Status is 6, Vaccine Certification Expiration Date must be prior to July 15, 1993. "
                        + "If Immunization Status is blank, then Vaccine Certificate Expiration Date must also be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "9");
        List<ValidationRule> validationRules02_9 = new ArrayList<>();
        for (int i = 1; i < 41; i++) {
            String fieldName = FIELD_STATUS_DATE + i;
            ValidationRule currentValidationRule = ValidationRule
                    .testIf(Restriction.pattern(fieldName, "^[^ ].*$"))
                    .testThen(Restriction.pattern(fieldName, "^[A-M89].*$"));
            validationRules02_9.add(currentValidationRule);
        }
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                validationRules02_9.toArray(new ValidationRule[40])),
                "If the first characters of Vaccine Status-1 through Vaccine Status-40 are not blank, "
                        + "they must be the letters A through M or the numbers 8 or 9."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "10");
        List<ValidationRule> validationRules02_10 = new ArrayList<>();
        for (int i = 1; i < 41; i++) {
            String fieldName = FIELD_STATUS_DATE + i;

            ValidationRule currentValidationRule = ValidationRule
                    .testIf(Restriction.pattern(fieldName, "^[A-M8].*$"))
                    .testThen(new RestrictionImmunizationStatusDate(fieldName));
            validationRules02_10.add(currentValidationRule);

            currentValidationRule = ValidationRule
                    .testIf(Restriction.pattern(fieldName, "^9.*$"))
                    .testThen(Restriction.pattern(fieldName, "^9.EXEMPT  $"));
            validationRules02_10.add(currentValidationRule);
        }
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                validationRules02_10.toArray(new ValidationRule[40])),
                "If the first characters of Vaccine Status-1 through Vaccine Status-40 are one of the "
                        + "letters A through M or the number 8, their corresponding Vaccine Dates must be "
                        + "valid dates (CCYYMMDD) not later than the current date or the word \"EXEMPT\" followed by two spaces. "
                        + "If, instead, the first character is the number 9, Vaccine Date must be the word \"EXEMPT\" followed by two "
                        + "spaces. Also, Vaccine Date may be all 9's if the first character of Vaccine Status is the number 8. "
                        + "Also, Vaccine Date may be a valid century and year followed by four zeroes for month and day if "
                        + "Vaccine Status is L."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_02, "11");
        List<ValidationRule> validationRules02_11 = new ArrayList<>();
        for (int i = 1; i < 41; i++) {
            String fieldName = FIELD_STATUS_DATE + i;

            ValidationRule currentValidationRule = ValidationRule
                    .testIf(Restriction.pattern(fieldName, "^([L89].*)|(..EXEMPT  )$"))
                    .testThen(Restriction.pattern(fieldName, "^.[0-9].*$"));
            validationRules02_11.add(currentValidationRule);

            currentValidationRule = ValidationRule
                    .testIf(Restriction.pattern(fieldName, "^(?!([L89].*)|(..EXEMPT))[^ ].*$"))
                    .testThen(Restriction.pattern(fieldName, "^.[1-9].*$"));
            validationRules02_11.add(currentValidationRule);
        }
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                validationRules02_11.toArray(new ValidationRule[40])),
                "If the first characters of Vaccine Status-1 through Vaccine Status-40 are not blank, "
                        + "their corresponding Vaccine Dosages must be numeric and between 1 and 9, unless "
                        + "the first character of Vaccine Status is \"L\" or one of the numbers 8 or 9, "
                        + "in which case Vaccine Dosage may also be zero or characters 3-10 are \"EXEMPT\" "
                        + "in which case the Vaccine Dosage may also be zero."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 03.
     */
    private void initializeRuleAssociations03() {
        String formatId = FL_EXPORT.RECORD03.getProcedureId();
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousSchoolYearRecord())),
                "This record is out of sequence if it immediately follows:\n" +
                        "a) an I03/S03 record with a School Year greater than or equal to this record's School Year;\n"
                        +
                        "b) a record with a type greater than I04/S04;\n" +
                        "c) a type I00/S00 record; or\n" +
                        "d) an I01 unless:\n" +
                        "    i) the student is not classified in an adult grade level (30 or 31),\n" +
                        "    ii) the student is age 21 years or older (Birth Date less than or equal to transmission date minus 21 years),\n"
                        +
                        "    iii) Former edit deleted.\n" +
                        "    iv) Addressee not Bright Futures (95)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                new RestrictionCompare(Operator.EQUALS,
                                        new Field(FIELD_SKL_YEAR_WITH_CENT, ValueTransformation.GET_FIRST_4_SYMBOLS),
                                        new Field(FIELD_SKL_YEAR_WITH_CENT, new ValueAdjuster() {
                                            @Override
                                            public String getAdjustedValue(String value, FLExportConfiguration helper) {
                                                if (value != null && value.length() == 8) {
                                                    value = value.substring(4);
                                                    value = String.valueOf((Integer.parseInt(value) - 1));
                                                }
                                                return value;
                                            }
                                        }),
                                        String.class),
                                new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS,
                                        new Field(FIELD_SKL_YEAR_WITH_CENT, ValueTransformation.GET_SECOND_4_SYMBOLS),
                                        String.valueOf(Calendar.getInstance().get(Calendar.YEAR)))

                        ))),
                "School Year With Century must be numeric, represent consecutive years "
                        + "(eg., 19831984 for the 1983-1984 school year), and not be in the future."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I03$"))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_DAYS_ABSENT_ANNUAL, PATTERN_EMPTY),
                                new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS,
                                        FIELD_DAYS_ABSENT_ANNUAL, Double.valueOf(0))))),
                "Days Absent, Annual, must be blank or numeric and greater than or "
                        + "equal to zero (not edited if Record Type is S03)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "9");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I03$"))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_DAYS_PRESENT_ANNUAL, PATTERN_EMPTY),
                                new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS,
                                        FIELD_DAYS_PRESENT_ANNUAL, Double.valueOf(0))))),
                "Days Present, Annual, must be blank or numeric and greater than or "
                        + "equal to zero (not edited if Record Type is S03)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "10");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I03$"))
                        .testThen(Restriction.pattern(FIELD_GRADE_PROMO_STATUS, "^A|D|P|R|N|Z| $"))),
                "Grade Promotion Status must be A, D, P, R, N, Z or blank (not edited if Record Type is S03)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "12");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I03$"))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_DAYS_ABSENT_SUMMER, PATTERN_EMPTY),
                                Restriction.and(
                                        new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS,
                                                FIELD_DAYS_ABSENT_SUMMER, Double.valueOf(0)),
                                        new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS,
                                                FIELD_DAYS_ABSENT_SUMMER, Double.valueOf(90)))))),
                "Days Absent, Summer Terms, must be blank or numeric greater than or equal to zero, and "
                        + "less than or equal to 90 (not edited if Record Type is S03)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "13");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^I03$"))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_DAYS_PRESENT_SUMMER, PATTERN_EMPTY),
                                Restriction.and(
                                        new RestrictionCompare(Operator.GREATER_THAN_OR_EQUALS,
                                                FIELD_DAYS_PRESENT_SUMMER, Double.valueOf(0)),
                                        new RestrictionCompare(Operator.LESS_THAN_OR_EQUALS,
                                                FIELD_DAYS_PRESENT_SUMMER, Double.valueOf(90)))))),
                "Days  Present, Summer Terms, must be blank or numeric greater than or equal to zero, and "
                        + "less than or equal to 90 (not edited if Record Type is S03)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "14");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID,
                        ValidationRule
                                .testIf(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20122013"))
                                .testThen(
                                        Restriction.pattern(FIELD_MIGRANT_ENR_DATE, PATTERN_NOT_EMPTY)),
                        ValidationRule
                                .testIf(Restriction.pattern(FIELD_MIGRANT_ENR_DATE, PATTERN_NOT_EMPTY))
                                .testThen(Restriction.and(
                                        Restriction.byDateFormat(FIELD_MIGRANT_ENR_DATE),
                                        Restriction.lessThanOrEquals(FIELD_MIGRANT_ENR_DATE,
                                                Calendar.getInstance().getTime())))),
                "If academic year (School Year With Century) is 20122013 or later, Migrant Enrollment Date "
                        + "is required. It must be a valid date and not in the future. It is not required "
                        + "for academic years prior to 20122013. However, if present, it must be a "
                        + "valid date and not in the future."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "14.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MIGRANT_ENR_DATE, PATTERN_NOT_EMPTY))
                        .testThen(new RestrictionExternalResult(Boolean.TRUE, FIELD_MIGRANT_ENR_DATE, null) {

                            @Override
                            Object getValidatedExternalResult(FLFasterExportConfiguration fasterHelper,
                                                              FLExport export,
                                                              ExportFormatRow row) {
                                try {
                                    PlainDate enrDate = new PlainDate(ENR_DATE_FORMAT.parse(getValidatedValue()));
                                    String currentYear =
                                            fasterHelper.getExportFormatRowFieldValue(row, export,
                                                    FIELD_SKL_YEAR_WITH_CENT).substring(0, 4);
                                    X2Criteria criteria = new X2Criteria();
                                    criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, currentYear);
                                    DistrictSchoolYearContext context =
                                            (DistrictSchoolYearContext) fasterHelper.getBroker().getBeanByQuery(
                                                    new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
                                    return enrDate.compareTo(context.getStartDate()) >= 0 &&
                                            enrDate.compareTo(context.getEndDate()) <= 0
                                                    ? Boolean.TRUE
                                                    : Boolean.FALSE;
                                } catch (ParseException e) {
                                    e.printStackTrace();
                                }
                                return Boolean.FALSE;
                            }
                        })),
                "If academic year (School Year With Century) is 20122013 or later, Migrant Enrollment date is required. "
                        + "It must fall within the academic school year reported. It is not required for academic years prior to "
                        + "20122013. However, if present, it must fall within the academic school year reported."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "15");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_MIGRANT_WDRAWAL_DATE, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_MIGRANT_WDRAWAL_DATE, PATTERN_NOT_ALL_ZEROS)))
                        .testThen(
                                Restriction.and(
                                        Restriction.byDateFormat(FIELD_MIGRANT_WDRAWAL_DATE),
                                        new RestrictionCompare(Operator.GREATER_THAN,
                                                FIELD_MIGRANT_WDRAWAL_DATE, FIELD_MIGRANT_ENR_DATE)))),
                "Migrant Withdrawal Date must be a valid date, must be greater than the "
                        + " Migrant Enrollment Date associated with this enrollment, may be zeros or may be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "16");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20122013"))
                        .testThen(
                                Restriction.and(
                                        Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, PATTERN_NOT_EMPTY))),

                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_NOT_EMPTY),
                                s_notAddressedToMsix))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_FROM_01_TO_76),
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, "99|00|"),
                                // TODO have not ideas what AA-ZP codes are (see spec for field 14c)
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_CODES,
                                        formatId,
                                        FIELD_MIGRANT_DST_ATTENDED))),

                ValidationRule
                        .testIf(s_addressedToMsix)
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_FROM_01_TO_76),
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, "FL"))),

                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, PATTERN_NOT_EMPTY),
                                s_notAddressedToMsix))
                        .testThen(Restriction.and(
                                Restriction.or(
                                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                formatId,
                                                FIELD_MIGRANT_SKL_ATTENDED),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^0[56]00$"),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                PATTERN_FROM_C901_TO_C928),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                PATTERN_FROM_U970_TO_U980),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^999[2-3]$"),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^N99[7-9]$"),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^9900$")),
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!3818).*$"))),

                ValidationRule
                        .testIf(s_addressedToMsix)
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!N99[7-9]).*$"),
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!9900).*$"))),

                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^0[56]00$"))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, "^71$")),

                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                        PATTERN_FROM_C901_TO_C928),
                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                        PATTERN_FROM_U970_TO_U980)))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_FROM_01_TO_76),
                                Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, "FL"))),

                ValidationRule
                        .testIf(Restriction.lessThan(FIELD_SKL_YEAR_WITH_CENT, "20132014"))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!999[23]).*$")),

                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.greaterThan(FIELD_SKL_YEAR_WITH_CENT, "20132014"),
                                s_addressedToMsix))
                        .testThen(Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!N998).*$"))),

                "If academic year (School Year With Century) is 20122013 or later, Migrant District Attended and "
                        + "Migrant School Attended are required. Data entered for these fields must meet "
                        + "the business rules for a valid district/school number combination for transcripts addressed to the "
                        + "MSIX System as defined on the School Year format. These fields are not required if academic year "
                        + "is prior to 20122013."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO we don't have "Attachments to Format T4"
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "17");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, PATTERN_FROM_01_TO_76))
                        .testThen(
                                Restriction.and(
                                        Restriction.or(
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                        PATTERN_FROM_C901_TO_C928),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                        PATTERN_FROM_U970_TO_U980),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^N99[7-8]$"),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^999[2-3]$"),
                                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                        formatId,
                                                        FIELD_MIGRANT_SKL_ATTENDED)),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^(?!3818).*$")))),

                "If Migrant District Attended = 01-76, then Migrant School Attended must be one of the following:\n" +
                        "C901-C928  Public State or Community College\n" +
                        "U970-U980  Public University\n" +
                        "Pnnn       A valid Florida Private Postsecondary school\n" +
                        "N998       Home Schooled during a period student was identified as a Migrant Student (not valid for academic years after 20132014).\n"
                        +
                        "N997       Migrant Non-attender or Out-of-School Youth.\n" +
                        "9992-9993  State Private Interdistrict Institution Migrant School Attended not valid for academic years prior 20132014 (not on the MSID Public School File.)\n"
                        +
                        "nnnn       A valid Public PK-12 school number on the MSID Public School File (not 3818)"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO we don't have "Attachments to Format T4"
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_03, "18");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_MIGRANT_DST_ATTENDED, "FL"))
                        .testThen(
                                Restriction.and(
                                        Restriction.or(
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                        PATTERN_FROM_C901_TO_C928),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                        PATTERN_FROM_U970_TO_U980),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^999[7-8]$"),
                                                Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED, "^999[2-3]$"),
                                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                        formatId,
                                                        FIELD_MIGRANT_SKL_ATTENDED)),
                                        Restriction.pattern(FIELD_MIGRANT_SKL_ATTENDED,
                                                PATTERN_SKL_NUM_CRED_EARNED_EXCL)))),
                "If Migrant District Attended = FL, then Migrant School Attended must be one of the following:\n" +
                        "\n" +
                        "C901-C928  Florida Public State or Community College\n" +
                        "U970-U980  Florida Public University\n" +
                        "Pnnn       A valid Florida Private Postsecondary school\n" +
                        "9998       Home Schooled during a period student was identified as a Migrant Student (not valid for academic years after 20132014).\n"
                        +
                        "9997       Migrant Non-attender or Out-of-School Youth.\n" +
                        "9992-9993  State Private Interdistrict Institution Migrant School Attended not valid for academic years prior 20132014 (not on the MSID Public School File).\n"
                        +
                        "nnnn       A valid Public PK-12 school number on the MSID Public School File (not 3518 or 3818)\n"
                        +
                        "nnnn       A valid Private school number on the MSID Private School File. "));
        m_ruleAssociations.add(ruleAssociation);

    }


    /**
     * Initialize rule associations 04.
     */
    private void initializeRuleAssociations04() {
        String formatId = FL_EXPORT.RECORD04.getProcedureId();
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                new RestrictionPreviousRecordType(Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_03, TransferObjectRecord.RECORD_TYPE_04)),
                                new RestrictionSearchRecord(FIELD_SKL_YEAR_WITH_CENT, FIELD_SKL_YEAR_WITH_CENT) {
                                    @Override
                                    public TransferObjectRecord findRecord(FLExportConfiguration helper,
                                                                           ExportFormatRow row) {
                                        return getPreviousTransferObjectRecord(helper, row);
                                    }
                                }))),
                "This record is out of sequence if the immediately preceding record was NOT\n"
                        +
                        "a) an I03/S03 record with the same School Year; or \n"
                        +
                        "b) an I04/S04 record with the same School Year.\n"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(s_notAddressedToMsix)
                        .testThen(Restriction
                                .or(
                                        Restriction.pattern(FIELD_DST_NUM_CRED_EARNED,
                                                START +
                                                        PATTERN_FROM_01_TO_76
                                                        + OR +
                                                        "99"
                                                        + END),
                                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                                formatId,
                                                FIELD_DST_NUM_CRED_EARNED),
                                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_COUNTRY_CODES,
                                                formatId,
                                                FIELD_DST_NUM_CRED_EARNED),
                                        Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, PATTERN_EMPTY)))),
                "For transcripts not addressed to MSIX: District Number, Where Credit Earned, must be 01 - 76, 99, "
                        + "a valid U.S. state code (AL - WY), a valid foreign country code (AA - ZZ) or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "7.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(s_addressedToMsix)
                        .testThen(Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, "^(?!00|7[7-9])[0-7][0-9]|FL$"))),
                "For transcripts addressed to MSIX (Migrant): District Number, Where Credit Earned, must be 01 - 76,or 'FL'."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                s_notAddressedToMsix,
                                Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, PATTERN_FROM_01_TO_76)))
                        .testThen(
                                Restriction.and(
                                        Restriction.or(
                                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                        formatId,
                                                        FIELD_SKL_NUM_EARNED_TAKEN),
                                                Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                        START +
                                                                PATTERN_ALL_ZEROS
                                                                + OR +
                                                                "N99[7-9]"
                                                                + OR +
                                                                PATTERN_FROM_C901_TO_C928
                                                                + OR +
                                                                PATTERN_FROM_U970_TO_U980
                                                                + OR +
                                                                PATTERN_EMPTY
                                                                + END)),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                PATTERN_SKL_NUM_CRED_EARNED_EXCL)))),
                "For transcripts not addressed to MSIX (Migrant): If District Number, Where Credit Earned, is 01 - 76, "
                        + "School Number, Where Credit Earned, must be a valid ID from the Florida Public Schools Master School ID File "
                        + "(see Appendix B). It may also be zero, N997, N998, N999, C901-C928, U970-U980 or blanks. "
                        + "However, school numbers 3518 and 3818 are invalid."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        s_notAddressedToMsix,
                                        Restriction.or(
                                                Restriction.byFormattedReferenceCodes(
                                                        REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                                        formatId,
                                                        FIELD_DST_NUM_CRED_EARNED),
                                                Restriction.byFormattedReferenceCodes(
                                                        REFERENCE_TABLE_NAME_COUNTRY_CODES,
                                                        formatId,
                                                        FIELD_DST_NUM_CRED_EARNED),
                                                Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, PATTERN_EMPTY))))
                        .testThen(
                                Restriction.or(
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN, PATTERN_NUMERIC),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN, "^N99[7-9]$"),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN, PATTERN_EMPTY)))),
                "For transcripts not addressed to MSIX (Migrant): If District Number, Where Credit Earned, is "
                        + "a valid U.S. state code (AL - WY) or a valid foreign country code (AA - ZZ) or blank, "
                        + "School Number, Where Credit Earned, must be numeric and greater than or equal to zero or "
                        + "be equal to N997, N998, N999, 9900 or blanks."));
        m_ruleAssociations.add(ruleAssociation);

        // TODO we don't have "Attachments to Format T4"
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        s_notAddressedToMsix,
                                        Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, "^99$")))
                        .testThen(
                                Restriction.or(
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                START +
                                                        PATTERN_FROM_C901_TO_C928
                                                        + OR +
                                                        PATTERN_FROM_U970_TO_U980
                                                        + OR +
                                                        "N99[7-9]"
                                                        + OR +
                                                        PATTERN_EMPTY
                                                        + END)))),
                "For transcripts not addressed to MSIX (Migrant): \n" +
                        "If District Number, Where Credit Earned is 99, School Number, Where Credit Earned, "
                        + "may be any of the following:\n" +
                        "C901-C928 Florida public community college (see Attachment 2 to Format T4 in Appendix I) "
                        + "U970-U980 Florida public university (see Attachment 2 to Format T4 in Appendix I) "
                        + "Pnnn Florida non-public institution (where \"nnn\" is one of the codes found in Attachment 3 to Format T4 in Appendix I) "
                        + "N997, N998, N999 or blanks"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                s_addressedToMsix)
                        .testThen(
                                Restriction.and(
                                        Restriction.or(
                                                Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                        START +
                                                                PATTERN_FROM_C901_TO_C928
                                                                + OR +
                                                                PATTERN_FROM_U970_TO_U980
                                                                + OR +
                                                                "999[78]" +
                                                                END),
                                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                        formatId,
                                                        FIELD_SKL_NUM_EARNED_TAKEN)),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                PATTERN_SKL_NUM_CRED_EARNED_EXCL)))),
                "For transcripts addressed to MSIX (Migrant): School Number, Where Credit Earned may be any of the following:\n"
                        +
                        "C901-C928 Florida Public State or Community College "
                        + "U970-U980 Florida Public University "
                        + "9998 Home Schooled during a period student was identified as a Migrant Student "
                        + "(not valid for academic years after 20132014). "
                        + "9997 Migrant Non-attender or Out-of-School Youth. "
                        + "Pnnn A valid Florida Private Postsecondary school "
                        + "nnnn A valid Public PK-12 school number or Private school number on the MSID Public School File "
                        + "(not 3518 or 3818)"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO we don't have MSID private/public school files, so for just check school codes
        // reference table
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        s_addressedToMsix,
                                        Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, "^FL$"),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN, PATTERN_NUMERIC)))
                        .testThen(
                                Restriction.and(
                                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                formatId,
                                                FIELD_SKL_NUM_EARNED_TAKEN),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                PATTERN_SKL_NUM_CRED_EARNED_EXCL)))),
                "For transcripts addressed to MSIX (Migrant):  If District Number, Where Credit Earned = 'FL' "
                        + "and the School Number Where Credit Earned is numeric, then School Number, Where Credit Earned "
                        + "must be:\n" +
                        "nnnn A valid Private school number on the MSID Private School File " +
                        "nnnn A valid Public PK-12 school number on the MSID Public School File (not 3518 or 3818)"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "8.6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(
                                Restriction.and(
                                        s_addressedToMsix,
                                        Restriction.pattern(FIELD_DST_NUM_CRED_EARNED, PATTERN_FROM_01_TO_76)))
                        .testThen(
                                Restriction.and(
                                        Restriction.or(
                                                Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                        START +
                                                                PATTERN_FROM_C901_TO_C928
                                                                + OR +
                                                                PATTERN_FROM_U970_TO_U980
                                                                + OR +
                                                                "999[78]"
                                                                + END),
                                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
                                                        formatId,
                                                        FIELD_SKL_NUM_EARNED_TAKEN)),
                                        Restriction.pattern(FIELD_SKL_NUM_EARNED_TAKEN,
                                                PATTERN_SKL_NUM_CRED_EARNED_EXCL)))),
                "For transcripts addressed to MSIX (Migrant):  "
                        + "If District Number, Where Credit Earned = 01 - 76, School Number, Where Credit Earned "
                        + "must be one of the following:\n" +
                        "C901-C928 Florida Public State or Community College "
                        + "U970-U980 Florida Public University "
                        + "9998 Home Schooled during a period student was identified as a Migrant Student (not valid for academic years after 20132014). "
                        + "9997 Migrant Non-attender or Out-of-School Youth. "
                        + "Pnnn A valid Florida Private Postsecondary school "
                        + "nnnn Public PK-12 school number on the MSID Public School File "
                        + "(not 3518 or 3818)"));
        m_ruleAssociations.add(ruleAssociation);

        // Rule 9: School Name, Credit, is not edited.

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "10");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionSearchRecord(FIELD_SKL_YEAR_WITH_CENT, FIELD_SKL_YEAR_WITH_CENT) {
                            @Override
                            public TransferObjectRecord findRecord(FLExportConfiguration helper,
                                                                   ExportFormatRow row) {
                                return getPreviousTransferObjectRecordWithType(helper, row,
                                        TransferObjectRecord.RECORD_TYPE_03);
                            }
                        })),
                "School Year With Century must be the same as the School Year With Century from the last Student School Year record."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "11");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD03,
                                Restriction.lessThan(FIELD_SKL_YEAR_WITH_CENT, "19881989")) {
                            @Override
                            public TransferObjectRecord findRecord(FLExportConfiguration helper,
                                                                   ExportFormatRow row) {
                                return getPreviousTransferObjectRecordWithType(helper, row,
                                        TransferObjectRecord.RECORD_TYPE_03);
                            }
                        })
                        .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, START +
                                PATTERN_GRADE_LEVELS_PK_KG_FROM_01_TO_12_OR_23
                                + OR +
                                "1[3-7]"
                                + OR +
                                "2[0-24]"
                                + END)),
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD03,
                                Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "19881989")) {
                            @Override
                            public TransferObjectRecord findRecord(FLExportConfiguration helper,
                                                                   ExportFormatRow row) {
                                return getPreviousTransferObjectRecordWithType(helper, row,
                                        TransferObjectRecord.RECORD_TYPE_03);
                            }
                        })
                        .testThen(Restriction.pattern(FIELD_GRADE_LEVEL, START +
                                PATTERN_GRADE_LEVELS_PK_KG_FROM_01_TO_12_OR_23
                                + OR +
                                "3[01]"
                                + END))),
                "Grade Level must be PK, KG, 01 - 12 or 23. "
                        + "For School Years beginning with 1988-1989, Grade Level may also be 30 or 31. "
                        + "For School Years prior to 1988-1989, Grade Level may also be 13 - 17, 20 - 22, and 24."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "12");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_TERM, "^[1-9B-OR-Y]$", "Term must be 1 - 9, B - O or R - Y."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "13");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_COURSE_NUMBER, PATTERN_NOT_EMPTY,
                        "Course Number must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "14");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_COURSE_TITLE_ABBREV, PATTERN_NOT_EMPTY,
                        "Course Title must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "15");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule.testIf(
                        Restriction.or(
                                Restriction.pattern(FIELD_GRADE_LEVEL, START + "09|1[0-2]" + END),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + ".*9.*" + END),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.or(
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                        START + PATTERN_BRIGHT_FUTURES + END),
                                                Restriction.pattern(FIELD_ADDRESSED_SKL_NUM,
                                                        START + "9900" + END)))))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SUBJECT_AREA,
                                formatId,
                                FIELD_CRS_STATE_SUBJ_REQS)),
                ValidationRule.testIf(
                        Restriction.and(
                                Restriction.pattern(FIELD_GRADE_LEVEL,
                                        START + wo("09|1[0-2]") + END),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + wo(".*9.*") + END),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.and(
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                        START + wo(PATTERN_BRIGHT_FUTURES) + END),
                                                Restriction.pattern(FIELD_ADDRESSED_SKL_NUM,
                                                        START + wo("9900") + END)))))
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, PATTERN_EMPTY),
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SUBJECT_AREA,
                                        formatId,
                                        FIELD_CRS_STATE_SUBJ_REQS))),
                ValidationRule
                        .testIf(
                                Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS,
                                        START + "PH|CH|S3" + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "20132014"))),
                "The Subject Area must be A1, A2, BI, GE, CH, PH, EQ, S3, EN, MA, SC, AH, WH, EC, AG, VO, SV, PF, LM, "
                        + "PE, EX, EL, FL, LA, SS, NC, CE or PA. "
                        + "This field may also be blank if Grade Level is not 09 - 12 and there is no Course Flag of 9 "
                        + "and the addressee is not Bright Futures, Talented Twenty or Out-of-State. "
                        + "If entered, the Subject Area Substitute field must be one of the valid codes listed above. "
                        + "The Subject Area codes PH, CH and S3 are only valid for school year 2013-14."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("E") + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^19992000$"))),
                "Course Flags with a value of E are valid only through the 1999-2000 academic school year (School Year with Century)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16a");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("S") + END))
                        .testThen(Restriction.and(
                                Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "19821983"),
                                Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "19861987")))),
                "Course Flags with a value of S are valid only if the academic school year (School Year With Century) is between "
                        + "1982-1983 and 1986-1987."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16b");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.and(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE,
                                                        PATTERN_NOT_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("1") + END)))
                        .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                Restriction.lessThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20082009"))),
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.or(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("1") + END)))
                        .testThen(Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20112012"))),
                "Course Flags with a value of 1 are valid for those students who entered the ninth grade in 2008-2009 or before. "
                        + "If the Year Entered Ninth Grade is not available, then the academic school year (School Year with Century) "
                        + "must be 2011-2012 or before."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16c");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("6|@|#|%") + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^20062007$"))),
                "Course Flags with a value of 6, @, #, and % are valid only through the 2006-2007 academic school year "
                        + "(School Year with Century)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16d");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("B|M|D") + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^20062007$"))),
                "Course Flags with a value of B, M or D are valid only through the 2006-2007 academic school year "
                        + "(School Year with Century)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16e");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.and(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE,
                                                        PATTERN_NOT_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("A|F|K") + END)))
                        .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                Restriction.lessThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20092010"))),
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.or(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("A|F|K") + END)))
                        .testThen(Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20122013"))),
                "Course Flags with a value of A, F, and K are valid only for those students who entered the ninth grade "
                        + "in 2009-2010 or before. If the Year Entered Ninth Grade is not available, then the academic school year "
                        + "(School Year with Century) must be 2012-2013 or before."));

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16f");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.and(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE,
                                                        PATTERN_NOT_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("O|Q|&") + END)))
                        .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                Restriction.lessThanOrEquals(FIELD_YEAR_ENTERED_9_GRADE, "20062007"))),
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.or(
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_EMPTY),
                                                Restriction.pattern(FIELD_YEAR_ENTERED_9_GRADE, PATTERN_ALL_ZEROS))),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("A|F|K") + END)))
                        .testThen(Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20092010"))),
                "Course Flags with a value of O, Q, and & are valid only for those students who entered the ninth grade "
                        + "in 2006-2007 or before. If the Year Entered Ninth Grade is not available, then the academic school year "
                        + "(School Year with Century) must be 20092010 or before."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16g");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("0|R|[$]") + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^20122013$"))),
                "Course Flags with a value of 0 (zero) R and $ are valid only through the 2012-2013 academic school year "
                        + "(School Year with Century)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16h");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("5") + END))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^19961997$"))),
                "Course Flags with a value of 5 are valid only through the 1996-1997 academic school year (School Year with Century)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16i");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_COURSE_FLAG, PATTERN_FROM_1_TO_8_ARE_UNIQUE))),
                "Course Flag values 1-8 are mutually exclusive of each other. That is, if one of the course flags is '1', "
                        + "then none of the remaining course flags can be '1', but can be 2 through 8, etc."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "16j");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("2|3|4|8") + END))
                        .testThen(Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "^20132014$"))),
                "Course Flag values 2, 3, 4 and 8 are obsolete after the academic school year 2013-2014."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "17");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                START + wo(PATTERN_BRIGHT_FUTURES) + END)),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("P") + END)))
                        .testThen(Restriction.pattern(FIELD_CREDIT_ATTEMPTED_CRS, "^000|050|100| +$")),
                ValidationRule
                        .testIf(Restriction.or(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                START + PATTERN_BRIGHT_FUTURES + END)),
                                Restriction.pattern(FIELD_COURSE_FLAG, START + wo(regexContains("P")) + END)))
                        .testThen(Restriction.pattern(FIELD_CREDIT_ATTEMPTED_CRS, "^000|050|100$"))),
                "Credit Attempted, Course, must be numeric and not less than zero. "
                        + "It may also be blank, unless addressed to Bright Futures or Talented Twenty, "
                        + "and having a course flag of \"P\" (in progress)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "18");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_CREDIT_EARNED_CRS, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_CREDIT_EARNED_CRS, "^000|050|100$"))),
                "If Credit Earned, Course, is not blank, it must be numeric and not less than zero."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "18.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, START + PATTERN_BRIGHT_FUTURES + END)))
                        .testThen(Restriction.pattern(FIELD_CREDIT_EARNED_CRS, PATTERN_NOT_EMPTY))),
                "If Addressed Institution is 95, Bright Futures, Credit Earned must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "19");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_GRADE_LEVEL, START + wo("PK|KG|0[1-8]") + END))
                        .testThen(Restriction.pattern(FIELD_COURSE_GRADE,
                                START + "([ABCD][+-]?|[FINUPSEWT]|W[PF]|FL|NG| +) *" + END)),
                ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_GRADE_LEVEL, START + wo("PK|KG|0[1-8]") + END),
                                Restriction.greaterThanOrEquals(FIELD_CRS_STATE_SUBJ_REQS, "^A2$"),
                                Restriction.pattern(FIELD_COURSE_GRADE, "^T *$")))
                        .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_HS_GRADUATION_DATE, "^[\\d]{2}201[67]$")))),
                "Course Grade must be one of A+, A, A-, B+, B, B-, C+, C, C-, D+, D, D-, F, I, N, U, P, S, E, WP, FL, "
                        + "NG, WF, W or blank (left justified). "
                        // TODO implement after clarification how to determined if it's course
                        // where the student has passed AP, EOC or CLEP exam
                        + "For school year 2016-2017 and later, a course grade of T will be accepted for relevant courses where "
                        + "the student has passed an AP, EOC or CLEP exam. "
                        + "For relevant Algebra II courses (subject area A2), a course grade of T is only accepted for students "
                        + "graduating in school year 2016-2017. "
                        // TODO implement after clarification how to determined if student is
                        // eligible for Florida Seal of Biliteracy Program
                        + "For school year 2016-2017 and later, a course grade of SB will be accepted for relevant courses where "
                        + "the student is eligible for the Florida Seal of Biliteracy Program. "
                        + "For grades PK - 8, Course Grade is not edited."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "19.1");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_GRADE, "^W *$"))
                        .testThen(new RestrictionExternalResult(Boolean.TRUE, FIELD_COURSE_NUMBER,
                                new ValueAdjuster() {
                                    @Override
                                    public String getAdjustedValue(String value, FLExportConfiguration helper) {
                                        return value.trim();
                                    }
                                }) {

                            @Override
                            Object getValidatedExternalResult(FLFasterExportConfiguration fasterHelper,
                                                              FLExport export,
                                                              ExportFormatRow row) {
                                DataDictionary dataDictionary = DataDictionary
                                        .getDistrictDictionary(fasterHelper.getBroker().getPersistenceKey());
                                DataDictionaryField crsStateIdField = dataDictionary
                                        .findDataDictionaryFieldByAlias(RetrieveTranscript.ALIAS_CRS_STATE_ID);
                                String crsStateIdJavaName = crsStateIdField.getJavaName();
                                X2Criteria courseCriteria = new X2Criteria();
                                courseCriteria.addEqualTo(crsStateIdJavaName, getValidatedValue());
                                Course course = (Course) fasterHelper.getBroker()
                                        .getBeanByQuery(new QueryByCriteria(Course.class, courseCriteria));
                                Object courseDualEnrIndicator =
                                        course.getFieldValueByAlias(ALIAS_CRS_DUAL_ENROLLMENT);
                                return Boolean
                                        .valueOf(BooleanAsStringConverter.TRUE.equals(courseDualEnrIndicator));
                            }
                        })),
                "If Course Grade is equal to W, then Course Number must be a dual enrollment course."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "19.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule
                        .testIf(Restriction.and(Restriction.pattern(FIELD_COURSE_GRADE, PATTERN_EMPTY),
                                Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$")))
                        .testThen(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("P") + END))),
                "If Course Grade is blank and Grade Level is 09-12, then Course Flag must contain a \"P\"."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "19.6");
        ruleAssociation
                .setProcessor(new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule
                                        .testIf(Restriction.and(
                                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM,
                                                                PATTERN_BRIGHT_FUTURES)),
                                                Restriction.or(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"),
                                                        Restriction.pattern(FIELD_COURSE_FLAG,
                                                                START + regexContains("9") + END))))
                                        .testThen(Restriction.pattern(FIELD_COURSE_GRADE,
                                                START + wo("I|W[PF]|FL") + END))),
                        "If Addressed Institution is 95, Bright Futures, and Grade Level is 9-12 or Course Flag is 9, "
                                + "then Course Grade must not be I, WP, FL and WF."));
        m_ruleAssociations.add(ruleAssociation);

        // ignore postsecondary requirements
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "19.7");
        ruleAssociation
                .setProcessor(new FLValidationRuleSet(
                        new RuleSet(ValidationRule
                                .testIf(Restriction.and(Restriction.pattern(FIELD_COURSE_NUMBER, "^.[\\d]{2}.*$"),
                                        Restriction.or(Restriction.pattern(FIELD_GRADE_LEVEL, "^09|1[0-2]$"),
                                                Restriction.and(Restriction.lessThan(FIELD_GRADE_LEVEL, "09"),
                                                        Restriction.pattern(FIELD_COURSE_FLAG,
                                                                START + regexContains("9") + END))),
                                        Restriction.greaterThan(FIELD_CREDIT_ATTEMPTED_CRS, "0"),
                                        Restriction.greaterThan(FIELD_SKL_YEAR_WITH_CENT, "19931994")))
                                .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_COURSE_CODES,
                                        formatId,
                                        FIELD_COURSE_NUMBER))),
                        "If bytes 2 and 3 of Course Number are numeric, "
                                + "Grade Level is 9-12 or Grade Level is less than 9 and there is a Course Flag of 9, "
                                + "Credit Attempted is greater than 0, " + "and School Year is greater than 19931994, "
                                + "then the Course Number must be found on the DOE Course Code Directory. "
                                + "If the Course Number is alphanumeric, then the Course Number must be on the "
                                + "Postsecondary Course Code Directory."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(null, Arrays.asList(TransferObject.RECORDS_TYPE_INTERDISTRICT));
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "20");
        ruleAssociation
                .setProcessor(new FLValidationRuleSet(
                        new RuleSet(
                                ValidationRule
                                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG,
                                                START + wo(regexContains("P")) + END))
                                        .testThen(Restriction.pattern(FIELD_CRS_IN_PROGRESS_HRS, PATTERN_ALL_ZEROS)),
                                ValidationRule
                                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG,
                                                START + regexContains("P") + END))
                                        .testThen(Restriction.pattern(FIELD_CRS_IN_PROGRESS_HRS, "^[\\d]+$"))),
                        "Course In Progress Hours must equal zero if there is no P in the Course Flag field. "
                                + "Otherwise, Course In Progress Hours must be numeric and greater than or equal to zero. "
                                + "It may also be blank (not edited if Record Type is S04)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(null, Arrays.asList(TransferObject.RECORDS_TYPE_INTERDISTRICT));
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "21");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_CRS_ABSENCES, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_CRS_ABSENCES, "^[\\d]+$"))),
                "If Course Absences is not blank, it must be numeric and greater than or equal to zero "
                        + "(not edited if Record Type is S04)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "23");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                .testThen(new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_04, 40) {


                    @Override
                    public Collection<TransferObjectRecord> getRecords(FLExportConfiguration helper,
                                                                       ExportFormatRow row) {
                        Collection<TransferObjectRecord> records = super.getRecords(helper, row);
                        TransferObjectRecord previousSchoolYearRecord = getPreviousTransferObjectRecordWithType(
                                helper, row, TransferObjectRecord.RECORD_TYPE_03);
                        Collection<TransferObjectRecord> transcriptsToCount = new ArrayList<>();
                        if (previousSchoolYearRecord != null) {
                            boolean isRecordsAdding = false;
                            for (TransferObjectRecord record : records) {
                                if (isRecordsAdding
                                        && !record.getRecordType()
                                                .equals(TransferObjectRecord.RECORD_TYPE_04)) {
                                    isRecordsAdding = false;
                                }
                                if (isRecordsAdding) {
                                    transcriptsToCount.add(record);
                                }
                                if (previousSchoolYearRecord.getOid().equals(record.getOid())) {
                                    isRecordsAdding = true;
                                }
                            }
                        }
                        return transcriptsToCount;
                    }

                })), "No more than 40 Student Course records may follow any single Student School Year record."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "25");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(ValidationRule
                                        .testIf(Restriction.and(Restriction
                                                .pattern(FIELD_TERM_START_DATE, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(FIELD_TERM_START_DATE, PATTERN_NOT_ALL_ZEROS)))
                                        .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_TERM_START_DATE))),
                                "If Term Start Date is not blank or all zeroes, it must be a valid date (CCYYMMDD). -OS Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        // Session start date? Let's suppose it is term start date.
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "26");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(
                                        ValidationRule
                                                .testIf(Restriction.and(
                                                        Restriction.pattern(FIELD_TERM_END_DATE, PATTERN_NOT_EMPTY),
                                                        Restriction.pattern(FIELD_TERM_END_DATE,
                                                                PATTERN_NOT_ALL_ZEROS)))
                                                .testThen(Restriction.and(
                                                        Restriction.byDateFormat("yyyyMMdd", FIELD_TERM_START_DATE),
                                                        Restriction
                                                                .greaterThanFieldValue(FIELD_TERM_END_DATE,
                                                                        FIELD_TERM_START_DATE, "yyyyMMdd")))),
                                "If Term End Date is not blank or all zeroes, it must be a valid date (CCYYMMDD) and greater than "
                                        + "Session Start Date. -OS Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "27");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_SKL_CRED_CODE_TYPE, "^7[1-8]|C[BS]| +$"))),
                "SPEEDE/ExPRESS School Number Where Credit Earned Code Type must be 71-78, CB, CS, or blank. -OS Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO we don't have mapping from School Number Where Credit Earned Code to School Number
        // Where Credit Earned Code Type
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "28");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue()).testThen(Restriction.alwaysTrue())),
                "SPEEDE/ExPRESS School Number Where Credit Earned Code must be valid according to the code specified in "
                        + "SPEEDE/ExPRESS School Number Where Credit Earned Code Type. -OS Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "29");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(
                                        ValidationRule.testIf(Restriction.pattern(FIELD_SKL_CRED_CODE, PATTERN_EMPTY))
                                                .testThen(Restriction.pattern(FIELD_SKL_CRED_CODE_TYPE, PATTERN_EMPTY)),
                                        ValidationRule
                                                .testIf(Restriction.pattern(FIELD_SKL_CRED_CODE_TYPE,
                                                        PATTERN_EMPTY))
                                                .testThen(Restriction.pattern(FIELD_SKL_CRED_CODE, PATTERN_EMPTY))),
                                "If either SPEEDE/ExPRESS School Number Where Credit Earned Code Type or SPEEDE/ExPRESS School Number "
                                        + "Where Credit Earned Code is blank then the other must also be blank. -OS Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO fix reference tables codes when will be available
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "30");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(ValidationRule
                                        .testIf(Restriction.byFormattedReferenceCodes(
                                                REFERENCE_TABLE_NAME_BRIGHT_FUTURES_HS_COURSES, formatId,
                                                FIELD_COURSE_NUMBER))
                                        .testThen(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, PATTERN_NOT_EMPTY))),
                                "Blank subject area invalid for Bright Futures high school courses"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO fix reference tables codes when will be available
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "31");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(ValidationRule
                                        .testIf(Restriction.byFormattedReferenceCodes(
                                                REFERENCE_TABLE_NAME_TALENTED_20_HS_COURSES, formatId,
                                                FIELD_COURSE_NUMBER))
                                        .testThen(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, PATTERN_NOT_EMPTY))),
                                "Blank subject area invalid for Talented 20 high school courses"));
        m_ruleAssociations.add(ruleAssociation);

        // TODO fix reference tables codes when will be available
        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "32");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(ValidationRule
                                        .testIf(Restriction.byFormattedReferenceCodes(
                                                REFERENCE_TABLE_NAME_OUT_OF_STATE_HS_COURSES, formatId,
                                                FIELD_COURSE_NUMBER))
                                        .testThen(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, PATTERN_NOT_EMPTY))),
                                "Blank subject area invalid for out-of-state high school courses"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "33");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(SKIP_RULE_IF_VALID, ValidationRule
                        .testIf(Restriction.and(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20132014"),
                                Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20152016")))
                        .testThen(Restriction.pattern(FIELD_ONLINE_CRS_INDICATOR, "^Y|N$")),
                        ValidationRule.testIf(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "^20162017$"))
                                .testThen(Restriction.pattern(FIELD_ONLINE_CRS_INDICATOR, "^Y|N|J|O|I$")),
                        ValidationRule.testIf(Restriction.pattern(FIELD_ONLINE_CRS_INDICATOR, "^J|O$"))
                                .testThen(Restriction.pattern(FIELD_COURSE_NUMBER, "^0200985$")),
                        ValidationRule.testIf(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "^20172018$"))
                                .testThen(Restriction.pattern(FIELD_ONLINE_CRS_INDICATOR, "^[^O]$"))),
                "Online course indicator is not edited when the academic year is 2012-2013 or earlier. "
                        + "For academic years 2013-2014 through 2015-2016 it must contain \"Y\" or \"N\" for all course records. "
                        + "Beginning with the 2016-2017 school year, this item must contain \"Y\", \"N\", \"J\", \"O\" or \"I\" "
                        + "for all course records. "
                        + "Codes \"J\" and \"O\" are valid only for Course Number 0200985. "
                        + "Beginning with the 2017-2018 school year, code \"O\" is obsolete and this item must contain \"Y\", \"N\", "
                        + "\"J\", or \"I\" for all course records. "
                        + "Code \"J\" is still valid only for Course Number 0200985."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "34");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule
                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG, START + regexContains("[*]") + END))
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_SBT_CRS_NUMBER, "^0800300$"),
                                Restriction.pattern(FIELD_COURSE_NUMBER, "^18023[01]0$"),
                                Restriction.greaterThan(FIELD_SKL_YEAR_WITH_CENT, "20072008")))),
                "If a Course Flag has a value of \"*\" (asterisk), then the Course Number, Substitution should be 0800300, "
                        + "and the Course Number should be either 1802300 or 1802310. "
                        + "This flag is valid starting with School Year With Century 2007-2008 and after."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "35");
        ruleAssociation
                .setProcessor(new FLValidationRuleSet(
                        new RuleSet(ValidationRule
                                .testIf(Restriction.alwaysTrue())
                                .testThen(Restriction.pattern(FIELD_COURSE_FLAG,
                                        START + wo(regexContains("[+]")) + END))),
                        "The Course Flag value \"+\" is no longer a valid flag and has been removed from processing completely. Reject-"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "37");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DISTRICT_NUMBER_CE,
                                START + PATTERN_FROM_01_TO_76 + OR + "99" + END))),
                "District Number, Current Enrollment must be equal to 01-76 or 99."));
        m_ruleAssociations.add(ruleAssociation);

        // rule # 38: Course Assessment Status, from spec **NOTE: This item is now obsolete. It will
        // not be used nor edited

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "42");
        ruleAssociation
                .setProcessor(
                        new FLValidationRuleSet(
                                new RuleSet(ValidationRule
                                        .testIf(Restriction.pattern(FIELD_COURSE_FLAG,
                                                START + regexContains("O|Q|&") + END))
                                        .testThen(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, "^LM$"))),
                                "Course Flags of \"O\", \"Q\", and \"&\" are substitutes for Life Management Skills, "
                                        + "and are only valid with a subject area code of \"LM\"."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "43");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, "^SS$"))
                        .testThen(Restriction.pattern(FIELD_SKL_YEAR_WITH_CENT, "^20132014$"))),
                "Subject area SS (Social Studies) can only be used through school year 2013-2014."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_04, "44");
        ruleAssociation.setProcessor(new FLValidationRuleSet(
                new RuleSet(ValidationRule.testIf(Restriction.pattern(FIELD_CRS_STATE_SUBJ_REQS, "^SV$"))
                        .testThen(Restriction.and(Restriction.greaterThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20072008"),
                                Restriction.lessThanOrEquals(FIELD_SKL_YEAR_WITH_CENT, "20122013")))),
                "Subject area SV (Half Science and Half Career and Technical Education) can only be used "
                        + "between school years 2007-2008 and 2012-2013."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 05.
     */
    private void initializeRuleAssociations05() {
        String formatId = FL_EXPORT.RECORD05.getProcedureId();
        // looks like there is an error in "a record with a type less than I05/S05". Let's
        // suppose it should be \"a record with a type greater than I04/S04\"
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(
                                Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_01,
                                        TransferObjectRecord.RECORD_TYPE_02,
                                        TransferObjectRecord.RECORD_TYPE_03,
                                        TransferObjectRecord.RECORD_TYPE_04))),
                ValidationRule
                        .testIf(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_01)))
                        .testThen(Restriction.or(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, null,
                                                Integer.valueOf(21),
                                                false,
                                                false))))),
                "This record is out of sequence if it immediately follows\n" +
                        "a) a record with a type greater than I04/S04; \n" +
                        "b) an I00/S00 record; \n" +
                        "c) an I01 and the student out of sequence is not classified in an adult grade level (30 or 31), "
                        + "is age 21 years or older (Birth Date less than or equal to transmission date minus 21 years).\n"
                        + "Note that any intervening generic pass-through records (G99) are ignored in making sequence tests."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "11");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]|ZZ$"))),
                "English Language Learner, if not blank, must be LY, LF, LP, LZ or ZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "12");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^LY$"))
                        .testThen(Restriction.pattern(FIELD_ELL_ENTRY_BASIS, "^[ARLT]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^LP$"))
                        .testThen(Restriction.pattern(FIELD_ELL_ENTRY_BASIS, "^[ARLTZ ]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^ZZ|  $"))
                        .testThen(Restriction.pattern(FIELD_ELL_ENTRY_BASIS, "^ |Z$"))),
                "If English Language Learner is LY or LP then English Language Learner Basis of Entry must be A, R, L or T. "
                        + "If English Language Learner is LP, then English Language Learner Basis of Entry may also be blank or Z. "
                        + "If English Language Learner is Z or blank, then English Language Learner Basis of Entry must be blank or ZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "13");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YF]$"))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_ENTRY_DATE),
                                Restriction.lessThanOrEquals(FIELD_ELL_ENTRY_DATE, new Date(), "yyyyMMdd"))),
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.or(
                                Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_ENTRY_DATE),
                                Restriction.pattern(FIELD_ELL_ENTRY_DATE, PATTERN_EMPTY)))),
                "If English Language Learner is LY or LF, then English Language Learner Entry Date must be a valid date (CCYYMMDD) "
                        + "not later than the current date. Otherwise this date can be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "14");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_CLASSIF_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_CLASSIF_DATE, new Date(),
                                                "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_CLASSIF_DATE, "^0+| +$")))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Classification Date must be "
                        + "a valid date (CCYYMMDD) not later than the current date, all zeroes, or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "15");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_STD_PLAN_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_STD_PLAN_DATE, new Date(),
                                                "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_STD_PLAN_DATE, PATTERN_EMPTY)))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Student Plan Date must be a valid date "
                        + "(CCYYMMDD) not later than the current date or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "16");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_REEVAL_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_REEVAL_DATE, new Date(),
                                                "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_REEVAL_DATE, "^0+| +$")))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Re-evaluation Date must be a valid date "
                        + "(CCYYMMDD) not later than the current date, all zeroes, or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "17");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.pattern(FIELD_ELL_INSTRUCT_EXT, "^Y|Z| $"))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Extension of Instruction must be "
                        + "Y, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "18");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_EXIT_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_EXIT_DATE, new Date(), "yyyyMMdd"),
                                        Restriction.greaterThanOrEqualsFieldValue(FIELD_ELL_EXIT_DATE,
                                                FIELD_ELL_ENTRY_DATE,
                                                "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_EXIT_DATE, "^0+| +$")))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Exit Date must be a "
                        + "valid date (CCYYMMDD) not later than the current date and not before "
                        + "English Language Learner Entry Date, all zeroes or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "18.3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_RECLAS_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_RECLAS_DATE, new Date(),
                                                "yyyyMMdd"),
                                        Restriction.greaterThanOrEqualsFieldValue(FIELD_ELL_RECLAS_DATE,
                                                FIELD_ELL_EXIT_DATE, "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_RECLAS_DATE, "^0+| +$")))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Reclassification Date "
                        + "must be a valid date (CCYYMMDD) not later than the current date and not before "
                        + "Limited English Proficient Exit Date, all zeroes or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "18.6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                Restriction.and(
                                        Restriction.byDateFormat("yyyyMMdd", FIELD_ELL_RECLAS_EXIT_DATE),
                                        Restriction.lessThanOrEquals(FIELD_ELL_RECLAS_EXIT_DATE, new Date(),
                                                "yyyyMMdd"),
                                        Restriction.greaterThanOrEqualsFieldValue(FIELD_ELL_RECLAS_EXIT_DATE,
                                                FIELD_ELL_RECLAS_DATE, "yyyyMMdd")),
                                Restriction.pattern(FIELD_ELL_RECLAS_EXIT_DATE, "^0+| +$")))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Reclassification Exit Date "
                        + "must be a valid date (CCYYMMDD) not later than the current date and not before "
                        + "Limited English Proficient Reclassification Date, all zeroes or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "19");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                new RestrictionPostReclassificationDate(FIELD_ELL_REPORT_CARD_1, "A",
                                        FIELD_ELL_EXIT_DATE),
                                Restriction.pattern(FIELD_ELL_REPORT_CARD_1, "^ +$")))),
                "If English Language Learner is LY, LF, LP or LZ then "
                        + "English Language Learner Post Reclassification Date First Report Card "
                        + "must be the letter \"A\" followed by a valid date (CCYYMMDD) not later than the current date and "
                        + "not before English Language Learner Exit Date or the entire field must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "20");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                new RestrictionPostReclassificationDate(FIELD_ELL_SEMIANN_REVIEW + 1, "B",
                                        FIELD_ELL_EXIT_DATE),
                                Restriction.pattern(FIELD_ELL_SEMIANN_REVIEW + 1, "^ +$")))),
                "If English Language Learner is LY, LF, LP or LZ then "
                        + "English Language Learner Post Reclassification Date First Semi-Annual Review "
                        + "must be the letter \"B\" followed by a valid date (CCYYMMDD) not later than the current date and "
                        + "not before English Language Learner Exit Date or the entire field must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "21");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                new RestrictionPostReclassificationDate(FIELD_ELL_SEMIANN_REVIEW + 2, "C",
                                        FIELD_ELL_EXIT_DATE),
                                Restriction.pattern(FIELD_ELL_SEMIANN_REVIEW + 2, "^ +$")))),
                "If English Language Learner is LY, LF, LP or LZ then "
                        + "English Language Learner Post Reclassification Date Second Semi-Annual Review "
                        + "must be the letter \"C\" followed by a valid date (CCYYMMDD) not later than the current date and "
                        + "not before English Language Learner Exit Date or the entire field must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "22");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.or(
                                new RestrictionPostReclassificationDate(FIELD_ELL_YEAR_2_END, "D",
                                        FIELD_ELL_RECLAS_DATE),
                                Restriction.pattern(FIELD_ELL_YEAR_2_END, "^ +$")))),
                "If English Language Learner is LY, LF, LP or LZ then "
                        + "English Language Learner Post Reclassification Date End of Second Year "
                        + "must be the letter \"D\" followed by a valid date (CCYYMMDD) not later than the current date and "
                        + "not before English Language Learner Reclassification Date or the entire field must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "22.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.pattern(FIELD_ELL_EXIT_BASIS + 1, "^[A-GRLZ ]$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^ZZ$"))
                        .testThen(Restriction.pattern(FIELD_ELL_EXIT_BASIS + 2, PATTERN_EMPTY))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner First Basis of Exit "
                        + "must be A-G, R, L, Z or blank. "
                        + "If English Language Learner is ZZ or blank, then English Language Learner First Basis of Exit must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "22.6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_ELL_PK_12, "^L[YFPZ]$"))
                        .testThen(Restriction.pattern(FIELD_ELL_EXIT_BASIS + 2, "^[B-GZ ]$"))),
                "If English Language Learner is LY, LF, LP or LZ then English Language Learner Second Basis of Exit must be B-G, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "22.8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_RECORD_TYPE, "^S05$"))
                        .testThen(Restriction.patternForFields(PATTERN_EMPTY,
                                FIELD_DRP_PROGRAM_CODE + 1,
                                FIELD_DRP_PROGRAM_CODE + 2,
                                FIELD_DRP_PROGRAM_CODE + 3,
                                FIELD_DRP_OUTCOMES + 1,
                                FIELD_DRP_OUTCOMES + 2,
                                FIELD_DRP_OUTCOMES + 3,
                                FIELD_DRP_PLACEMNT_REASONS + 1,
                                FIELD_DRP_PLACEMNT_REASONS + 2,
                                FIELD_DRP_PLACEMNT_REASONS + 3))),
                "If the record type is S05, then all occurrences of the Dropout Prevention data elements (items 22-33.5) must be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "23");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.patternForFields("^[URAEPDNWZ ]$",
                                FIELD_DRP_PROGRAM_CODE + 1,
                                FIELD_DRP_PROGRAM_CODE + 2,
                                FIELD_DRP_PROGRAM_CODE + 3))),
                "Dropout Prevention/Juvenile Justice Programs must be U, R, A, E, P, D, N, W, Z, or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "24");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 1, "^[ABCN ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 2, "^[ABCN ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 3, "^[ABCN ]{5}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is U, then "
                        + "Dropout Prevention/Juvenile Justice Placement Reasons must be A, B, C, N or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "25");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 1, "^[HIJT ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 2, "^[HIJT ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 3, "^[HIJT ]{5}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is A, "
                        + "then Dropout Prevention/Juvenile Justice Placement Reasons must be H, I, J, T or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "26");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 1, "^[EFG ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 2, "^[EFG ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 3, "^[EFG ]{5}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is P, then "
                        + "Dropout Prevention/Juvenile Justice Placement Reasons must be E, F, G or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "28");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^[DZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 1, "^[Z ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^[DZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 2, "^[Z ]{5}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^[DZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_PLACEMNT_REASONS + 3, "^[Z ]{5}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is D, Z, then "
                        + "Dropout Prevention/Juvenile Justice Placement Reasons must be Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "31");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 1, "^[YDNZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 2, "^[YDNZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^U$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 3, "^[YDNZ ]{3}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is U then "
                        + "Dropout Prevention/Juvenile Justice Prevention Outcomes must be Y, D, N, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "32");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 1, "^[YNPZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 2, "^[YNPZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^P$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 3, "^[YNPZ ]{3}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is P then "
                        + "Dropout Prevention/Juvenile Justice Prevention Outcomes must be Y, N, P, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "33");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 1, "^[ESRNT ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 2, "^[ESRNT ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^A$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 3, "^[ESRNT ]{3}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is A, then "
                        + "Dropout Prevention/Juvenile Justice Outcomes must be E, S, R, N, T or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "35");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^[DH]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 1, "^[ATVWXYZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^[DH]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 2, "^[ATVWXYZ ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^[DH]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 3, "^[ATVWXYZ ]{3}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is or D or H, then "
                        + "Dropout Prevention/Juvenile Justice Outcomes must be A, T, V, W, X, Y, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "37");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 1, "^[RENZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 1, "^[Z ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 2, "^[RENZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 2, "^[Z ]{3}$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DRP_PROGRAM_CODE + 3, "^[RENZ]$"))
                        .testThen(Restriction.pattern(FIELD_DRP_OUTCOMES + 3, "^[Z ]{3}$"))),
                "If Dropout Prevention/Juvenile Justice Programs is R, E, N or Z, then "
                        + "Dropout Prevention/Juvenile Justice Outcomes must be Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "39");

            String fieldNamePrefix = FIELD_CTE_INDUS_CERT_ID;
            String refTableName = REFERENCE_TABLE_NAME_IND_CERT_IDS;
            int numOfFields = 4;

            ValidationRule[] validationRules = new ValidationRule[numOfFields];
            int index = 0;
            for (ValidationRule validationRule : validationRules) {
                if (validationRule == null) {
                    validationRules[index] = ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.or(
                                    Restriction.patternForFields(PATTERN_EMPTY,
                                            fieldNamePrefix + (index + 1)),
                                    Restriction.byFormattedReferenceCodes(refTableName,
                                            formatId,
                                            fieldNamePrefix + (index + 1))));
                }
                index++;
            }

            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules),
                    "Industry Certification Identifier must be blank or a valid value found in "
                            + "the Student Information System Appendix Z."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "40");

            String fieldNamePrefix = FIELD_CTE_INDUS_CERT_OUT;
            String refTableName = REFERENCE_TABLE_NAME_IND_CERT;
            int numOfFields = 4;

            ValidationRule[] validationRules = new ValidationRule[numOfFields];
            int index = 0;
            for (ValidationRule validationRule : validationRules) {
                if (validationRule == null) {
                    validationRules[index] = ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.or(
                                    Restriction.patternForFields(PATTERN_EMPTY,
                                            fieldNamePrefix + (index + 1)),
                                    Restriction.byFormattedReferenceCodes(refTableName,
                                            formatId,
                                            fieldNamePrefix + (index + 1))));
                }
                index++;
            }

            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules),
                    "Industry Certification Outcome must be blank or a valid value found in the "
                            + "Student Information System Industry Certification Outcome."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "41");

            String fieldNamePrefix = FIELD_CTE_ACADEMY_ID;
            String refTableName = REFERENCE_TABLE_NAME_ACADEMY_ID;
            int numOfFields = 4;

            ValidationRule[] validationRules = new ValidationRule[numOfFields];
            int index = 0;
            for (ValidationRule validationRule : validationRules) {
                if (validationRule == null) {
                    validationRules[index] = ValidationRule
                            .testIf(Restriction.alwaysTrue())
                            .testThen(Restriction.or(
                                    Restriction.patternForFields(PATTERN_EMPTY,
                                            fieldNamePrefix + (index + 1)),
                                    Restriction.byFormattedReferenceCodes(refTableName,
                                            formatId,
                                            fieldNamePrefix + (index + 1))));
                }
                index++;
            }

            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    validationRules),
                    "Career and Professional Academy Identifier must be either blank or a valid value found on Appendix Y of the "
                            + "Education Information & Accountability Services Database Manual."));
            m_ruleAssociations.add(ruleAssociation);
        }


        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_05, "42");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.or(
                                Restriction.patternForFields("^[ATVZ ]$",
                                        FIELD_CTE_FULL_PGM_COMPL + 1,
                                        FIELD_CTE_FULL_PGM_COMPL + 2,
                                        FIELD_CTE_FULL_PGM_COMPL + 3,
                                        FIELD_CTE_FULL_PGM_COMPL + 4)))),
                "Credential Awarded must be equal to A, T, V, Z or blank."));
        m_ruleAssociations.add(ruleAssociation);

        // RULE 43 AS OF 7/1/2017 ARE NOT EDITED
    }


    /**
     * Initialize rule associations 06.
     */
    private void initializeRuleAssociations06() {
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(
                                Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_00,
                                        TransferObjectRecord.RECORD_TYPE_01,
                                        TransferObjectRecord.RECORD_TYPE_05))),
                ValidationRule
                        .testIf(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_01)))
                        .testThen(Restriction.or(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, null,
                                                Integer.valueOf(21),
                                                false,
                                                false))))),
                "This record is out of sequence if it immediately follows:\n" +
                        "a) a record with a type less than I05;\n" +
                        "b) a record with a type greater than I05;\n" +
                        "c) an I00; or\n" +
                        "c) an I01 and the student is not classified in an adult grade level (30 or 31), is age 21 years or older (Birth Date less than or equal to transmission date minus 21 years).\n"
                        +
                        "Note that any intervening generic pass-through records (G99) are ignored in making sequence tests."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_EXCEPT_PLAN_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat(FIELD_EXCEPT_PLAN_DATE),
                                Restriction.lessThanOrEquals(FIELD_EXCEPT_PLAN_DATE,
                                        Calendar.getInstance().getTime())))),
                "If Exceptional Student Plan Date is not blank, it must be a valid date not "
                        + "later than the current date (MMDDCCYY)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "9");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_EXCEPT_PRIMARY, START + "C" + OR + "[F-M]" + OR +
                        "O|P" + OR + "[S-W]" + OR + "Z" + OR + PATTERN_EMPTY + END,
                        "Exceptionality Primary must be C, F – M, O, P, S – W, Z or blank. "));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "10");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_FEFP_PGM_NUM, START + "1[0-1][1-3]" + OR + "25[4-5]" + OR +
                        "130" + OR + "300" + OR + "99[8-9]" + OR + PATTERN_EMPTY + END,
                        "If FEFP Program Number is not blank, it must be 101-103, 111-113, "
                                + "254, 255, 130, 300, 998, and 999."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "16");
        ruleAssociation.setProcessor(
                new ValidateRegularExpression(FIELD_SECTION_504_ELIGIBLE, START + "Y|N|I|Z" +
                        OR + PATTERN_EMPTY + END, "Section 504 Eligible must be Y, N, I, Z or blank. "));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "18.7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_EVAL_REEVAL_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat(FIELD_EVAL_REEVAL_DATE)))),
                "If Exceptional Student Current Evaluation/Re-evaluation Date is not blank, "
                        + "it must be a valid date (MMDDCCYY)."));
        m_ruleAssociations.add(ruleAssociation);

        for (int i = 1; i <= 10; i++) {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "20");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_PLACEMENT_DATE + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.and(
                                    Restriction.byDateFormat(FIELD_PLACEMENT_DATE + i),
                                    Restriction.lessThanOrEquals(FIELD_PLACEMENT_DATE + i,
                                            Calendar.getInstance().getTime())))),
                    "If Exceptional Student Placement Date is not blank, it must be a valid date "
                            + "(MMDDCCYY) not later than the current date."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "21");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_ELIG_DETERM_DATE + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.and(
                                    Restriction.byDateFormat(FIELD_ELIG_DETERM_DATE + i),
                                    Restriction.lessThanOrEquals(FIELD_ELIG_DETERM_DATE + i,
                                            Calendar.getInstance().getTime())))),
                    "If Exceptional Student Eligibility Determination Date is not blank, "
                            + "it must be a valid date (MMDDCCYY) not later than the current date."));
            m_ruleAssociations.add(ruleAssociation);


            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "22");
            ruleAssociation.setProcessor(
                    new ValidateRegularExpression(FIELD_PLACEMENT_STATUS + i, START + "R|P|I|N|TZ" +
                            OR + PATTERN_EMPTY + END,
                            "Exceptional Student Placement Status must be R, P, I, N, T, Z or blank"));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "23");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_REFERRAL_DATE + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.and(
                                    Restriction.byDateFormat(FIELD_REFERRAL_DATE + i),
                                    Restriction.lessThanOrEquals(FIELD_REFERRAL_DATE + i,
                                            Calendar.getInstance().getTime())))),
                    "If Referral Date is not blank it must be a valid date (MMDDCCYY) "
                            + "not later than the current date."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "24");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_DISMISSAL_DATE + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.and(
                                    Restriction.byDateFormat(FIELD_DISMISSAL_DATE + i),
                                    Restriction.lessThanOrEquals(FIELD_DISMISSAL_DATE + i,
                                            Calendar.getInstance().getTime())))),
                    "If Exceptional Student Dismissal Date is not blank, it must be a valid date "
                            + "(MMDDCCYY) not later than the current date."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "25");
            ruleAssociation.setProcessor(
                    new ValidateRegularExpression(FIELD_EXCEPTIONALITY + i, START + "C" + OR + "[F-M]" + OR +
                            "O|P" + OR + "[S-W]" + OR + "Z" + OR + PATTERN_EMPTY + END,
                            "Exceptionality must be C, F - M, O, P, S - W, Z or blank. "));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "26");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_EVAL_COMPL_DATE + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.byDateFormat(FIELD_EVAL_COMPL_DATE + i))),
                    "If Evaluation Completion Date is not blank, it must be a valid date (MMDDCCYY)."));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_06, "27");
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                    ValidationRule
                            .testIf(Restriction.pattern(FIELD_CONSENT_DATE_EV + i, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.byDateFormat(FIELD_CONSENT_DATE_EV + i))),
                    "If Date of Consent for Evaluation is not blank, it must be a valid date (MMDDCCYY)."));
            m_ruleAssociations.add(ruleAssociation);
        }
    }


    /**
     * Initialize rule associations 07.
     */
    private void initializeRuleAssociations07() {
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "1.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_07, 5))),
                "A maximum of five I07/S07 records are allowed per transcript."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_07, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(
                                Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_00,
                                        TransferObjectRecord.RECORD_TYPE_01,
                                        TransferObjectRecord.RECORD_TYPE_02,
                                        TransferObjectRecord.RECORD_TYPE_03,
                                        TransferObjectRecord.RECORD_TYPE_04,
                                        TransferObjectRecord.RECORD_TYPE_05,
                                        TransferObjectRecord.RECORD_TYPE_06,
                                        TransferObjectRecord.RECORD_TYPE_07))),
                ValidationRule
                        .testIf(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_00)))
                        .testThen(new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                Restriction.pattern(FIELD_MESSAGE_TYPE, "^S0[2-468]|1[01]$"))),
                ValidationRule
                        .testIf(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_01)))
                        .testThen(Restriction.or(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, null,
                                                Integer.valueOf(21),
                                                false,
                                                false))))),
                "This record is out of sequence if it immediately follows:\n" +
                        "a) a record with a type greater than I07/S07; \n" +
                        "b) an I00/S00 whose Message Type was not S02 - S04, S06, S08, S10 or S11; \n" +
                        "c) an I01 and the student is not classified in an adult grade level (30 or 31), is age 21 years or older "
                        + "(Birth Date less than or equal to transmission date minus 21 years)."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 08.
     */
    private void initializeRuleAssociations08() {
        RuleAssociation ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "1.5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_08, 9))),
                "A maximum of nine I08/S08 records are allowed per transcript."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_01)))
                        .testThen(Restriction.or(
                                new RestrictionForRecordType(FL_EXPORT.RECORD01,
                                        Restriction.pattern(FIELD_GRADE_LEVEL, "^3[01]$")),
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.studentAgeInRange(FIELD_STD_NUM_ID_FL, null,
                                                Integer.valueOf(21),
                                                false,
                                                false))))),
                "This record is out of sequence if it immediately follows:\n" +
                        "a) an I01 and the student is not classified in an adult grade level (30 or 31), \n" +
                        "b) the student is age 21 years or older (Birth Date less than or equal to transmission date minus 21 years) \n"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 1, PATTERN_NOT_EMPTY))),
                "Test Block 1 cannot be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation();
        ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 2, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 3, PATTERN_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 3, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 4, PATTERN_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 4, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 5, PATTERN_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 5, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_TEST_GRADE_LEVEL + 6, PATTERN_EMPTY))),
                "A non-blank Test Block cannot follow a blank Test Block."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "9");
            List<ValidationRule> validationRules08_9 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String fieldNameTestDate = FIELD_TEST_DATE + testNum;
                String fieldNameTestGradeLevel = FIELD_TEST_GRADE_LEVEL + testNum;

                ValidationRule currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                Restriction.byDateFormat("MMddyyyy", fieldNameTestDate),
                                new RestrictionCompareSchoolYearByDate(fieldNameTestDate, "19881989", LESS)))
                        .testThen(Restriction.pattern(fieldNameTestGradeLevel, START +
                                PATTERN_GRADE_LEVELS_PK_KG_FROM_01_TO_12_OR_23
                                + OR +
                                "3[01]"
                                + OR +
                                "1[3-7]"
                                + OR +
                                "2[0-24]"
                                + END));
                validationRules08_9.add(currentValidationRule);

                currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                Restriction.byDateFormat("MMddyyyy", fieldNameTestDate),
                                Restriction.or(
                                        new RestrictionCompareSchoolYearByDate(fieldNameTestDate, "19881989", GREATER),
                                        new RestrictionCompareSchoolYearByDate(fieldNameTestDate, "19881989", EQUALS))))
                        .testThen(Restriction.pattern(fieldNameTestGradeLevel, START +
                                PATTERN_GRADE_LEVELS_PK_KG_FROM_01_TO_12_OR_23
                                + OR +
                                "3[01]"
                                + END));
                validationRules08_9.add(currentValidationRule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(
                    new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_9.toArray(new ValidationRule[12])),
                    "Grade Level must be PK, KG, 01-12, 23, 30 or 31. For School Years prior to 1988-1989, "
                            + "Grade Level may also be 13 - 17, 20 -22, and 24."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "10");
            List<ValidationRule> validationRules08_10 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String fieldNameTestDate = FIELD_TEST_DATE + testNum;

                ValidationRule currentValidationRule = ValidationRule
                        .testIf(Restriction.pattern(fieldNameTestDate, PATTERN_NOT_EMPTY))
                        .testThen(
                                Restriction.and(
                                        Restriction.byDateFormat("MMddyyyy", fieldNameTestDate),
                                        Restriction.lessThanOrEquals(fieldNameTestDate, new Date(), "MMddyyyy")));
                validationRules08_10.add(currentValidationRule);
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_10.toArray(new ValidationRule[6])),
                            "Test Date must be a valid date (MMDDCCYY) not later than the current date."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "11");
            List<ValidationRule> validationRules08_11 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String fieldNameTestName = FIELD_TEST_NAME + testNum;

                ValidationRule currentValidationRule = ValidationRule
                        .testIf(Restriction.pattern(fieldNameTestName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_TEST_CODES,
                                fieldNameTestName));
                validationRules08_11.add(currentValidationRule);
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_11.toArray(new ValidationRule[6])),
                            "Test Name must be valid code from Test Name Table"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "11.5");
            List<ValidationRule> validationRules08_11_5 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String fieldNameTestDate = FIELD_TEST_DATE + testNum;
                String fieldNameTestName = FIELD_TEST_NAME + testNum;

                ValidationRule currentValidationRule = ValidationRule
                        .testIf(Restriction.greaterThanOrEquals(fieldNameTestDate, getDate(Calendar.MARCH, 1, 2005),
                                "MMddyyyy"))
                        .testThen(Restriction.pattern(fieldNameTestName, START + wo("SAT|SA1") + END));
                validationRules08_11_5.add(currentValidationRule);
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_11_5.toArray(new ValidationRule[6])),
                            "Test Names SAT and SA1 are invalid for test dates on or after 3/01/2005. "
                                    + "SAT tests taken after this date must be coded with test name SA3."));
            m_ruleAssociations.add(ruleAssociation);
        }


        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "14");
            List<ValidationRule> validationRules08_14 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectNum = 2; subjectNum <= MAX_NUM_SUBJECT_CONTENT; subjectNum++) {
                    String subjectContentFieldName = FIELD_TEST_SC + testNum + subjectNum;
                    String prevSubjectContentFieldName = FIELD_TEST_SC + testNum + (subjectNum - 1);
                    ValidationRule currentValidationRule = ValidationRule
                            .testIf(Restriction.pattern(prevSubjectContentFieldName, PATTERN_EMPTY))
                            .testThen(Restriction.pattern(subjectContentFieldName, PATTERN_EMPTY));
                    validationRules08_14.add(currentValidationRule);
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_14.toArray(new ValidationRule[0])),
                            "Within each Test Block, a non-blank Test Subject Content Block cannot follow a blank Test Subject Content Block."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "15");
            List<ValidationRule> validationRules08_15 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testNameFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectNum = 1; subjectNum <= MAX_NUM_SUBJECT_CONTENT; subjectNum++) {
                    String subjectContentFieldName = FIELD_TEST_SC + testNum + subjectNum;
                    ValidationRule currentValidationRule = ValidationRule
                            .testIf(Restriction.pattern(testNameFieldName, PATTERN_NOT_EMPTY))
                            .testThen(Restriction.and(
                                    Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_TEST_CODES,
                                            testNameFieldName),
                                    new RestrictionCompareByDependCode(subjectContentFieldName,
                                            REFERENCE_TABLE_NAME_SUBJECT_CONTENT_CODE, testNameFieldName)));
                    validationRules08_15.add(currentValidationRule);
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_15.toArray(new ValidationRule[0])),
                            "The Test Name must be valid and the Test Subject Content must be valid for the Test Name "
                                    + "(see Appendix L in the Student Information System)"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16");
            List<ValidationRule> validationRules08_16 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.pattern(scoreTypeFieldName, PATTERN_NOT_EMPTY))
                                .testThen(Restriction.pattern(scoreTypeFieldName,
                                        "^AL|DS|F[RS]|G[ELV]|L[SX]|N[CPS]|OS|P[FI]|RS|S[LS]$"));
                        validationRules08_16.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16.toArray(new ValidationRule[0])),
                            "Test Score Type must be one of RS, SS, NP, NC, GE, LS, LX, NS, FS, PI, AL, PF, DS, OS, FR, GV, GL, SL"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16.5");
            List<ValidationRule> validationRules08_16_5 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectNum = 1; subjectNum <= MAX_NUM_SUBJECT_CONTENT; subjectNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.pattern(scoreTypeFieldName, "^PF$"))
                                .testThen(Restriction.pattern(scoreFieldName, "^0{3}[12]$"));
                        validationRules08_16_5.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16_5.toArray(new ValidationRule[0])),
                            "If Test Score Type is PF, then Test Score must be 0001 or 0002."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16.6");
            List<ValidationRule> validationRules08_16_6 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.pattern(scoreTypeFieldName, "^OS$"))
                                .testThen(Restriction.pattern(testFieldName, "^ESI|RFS|WSS|DIB$"));
                        validationRules08_16_6.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16_6.toArray(new ValidationRule[0])),
                            "If Test Score Type is OS, then Test Name must be ESI, RFS, WSS or DIB."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16.7");
            List<ValidationRule> validationRules08_16_7 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testFieldName, "^ESI$"),
                                        Restriction.pattern(scoreTypeFieldName, "^OS$")))
                                .testThen(Restriction.pattern(scoreFieldName, "^ +R|G|N$"));
                        validationRules08_16_7.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16_7.toArray(new ValidationRule[0])),
                            "If Test Name is ESI and Test Score Type is OS, then Test Score must be R, G, or N "
                                    + "(right-justified blank-filled)."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16.8");
            List<ValidationRule> validationRules08_16_8 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testFieldName, "^RFS$"),
                                        Restriction.pattern(scoreTypeFieldName, "^OS$")))
                                .testThen(Restriction.pattern(scoreFieldName, "^ +E|P|C$"));
                        validationRules08_16_8.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16_8.toArray(new ValidationRule[0])),
                            "If Test Name is RFS and Test Score Type is OS, then Test Score must be E, P, or C "
                                    + "(right-justified blank-filled)."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "16.9");
            List<ValidationRule> validationRules08_16_9 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testFieldName, "^WSS$"),
                                        Restriction.pattern(scoreTypeFieldName, "^OS$")))
                                .testThen(Restriction.pattern(scoreFieldName, "^ +N|I|P$"));
                        validationRules08_16_9.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_16_9.toArray(new ValidationRule[0])),
                            "If Test Name is WSS and Test Score Type is OS, then Test Score must be N, I, or P "
                                    + "(right-justified blank-filled)."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "17");
            List<ValidationRule> validationRules08_17 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(scoreFieldName, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(scoreTypeFieldName, START + wo("OS") + END)))
                                .testThen(Restriction.pattern(scoreFieldName, "^[\\d]+$"));
                        validationRules08_17.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_17.toArray(new ValidationRule[0])),
                            "If Test Score is not blank and Score Type is not OS, then Test Score must be right "
                                    + "justified and contain no blank, alphabetic or special characters."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "17.5");
            List<ValidationRule> validationRules08_17_5 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + testScoreNum;
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;

                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(
                                        Restriction.and(
                                                Restriction.pattern(scoreFieldName, PATTERN_NOT_EMPTY),
                                                Restriction.pattern(scoreFieldName, PATTERN_NOT_ALL_ZEROS)))
                                .testThen(Restriction.pattern(scoreTypeFieldName, PATTERN_NOT_EMPTY));
                        validationRules08_17_5.add(currentValidationRule);

                        currentValidationRule = ValidationRule
                                .testIf(Restriction.pattern(scoreTypeFieldName, PATTERN_NOT_EMPTY))
                                .testThen(Restriction.and(
                                        Restriction.pattern(scoreFieldName, PATTERN_NOT_EMPTY),
                                        Restriction.pattern(scoreFieldName, PATTERN_NOT_ALL_ZEROS)));
                        validationRules08_17_5.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_17_5.toArray(new ValidationRule[0])),
                            "If Test Score is not blank, then its associated Test Score Type must not be blank or vice versa."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "18");
            List<ValidationRule> validationRules08_18 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testNameFieldName = FIELD_TEST_NAME + testNum;
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String scoreFieldName = FIELD_TEST_S + testNum + subjectContentNum + testScoreNum;

                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                        Restriction.pattern(testNameFieldName, "^SAT$")))
                                .testThen(Restriction.pattern(scoreFieldName, "^0[2-8][\\d]{2}$"));
                        validationRules08_18.add(currentValidationRule);

                        currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                        Restriction.pattern(testNameFieldName, "^ACT$")))
                                .testThen(Restriction.pattern(scoreFieldName, "^(?!00(00|3[7-9]))00[0-3][0-9]$"));
                        validationRules08_18.add(currentValidationRule);

                        currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                                Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                        Restriction.pattern(testNameFieldName, "^CPT$")))
                                .testThen(Restriction.pattern(scoreFieldName,
                                        "^(?!01[3-9]|012[1-9]|0000)0[01][0-9][0-9]$"));
                        validationRules08_18.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_18.toArray(new ValidationRule[0])),
                            "If Addressed Institution is 95, Bright Futures, then Test Scores for SAT, ACT, and CPT "
                                    + "must be in valid ranges for scaled score types as follows: \n"
                                    + "1) Valid scaled scores for SAT subtests are 200-800. \n"
                                    + "2) Valid scaled scores for ACT subtests are 1-36. \n"
                                    + "3) Valid scaled scores for CPT subtests are 1-120.\n"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "19");
            List<ValidationRule> validationRules08_19 = new ArrayList<>();

            ArrayList<Restriction> verbalRestrictions = new ArrayList<>();
            ArrayList<Restriction> math73Restrictions = new ArrayList<>();
            ArrayList<Restriction> math95Restrictions = new ArrayList<>();
            ArrayList<Restriction> crRestrictions = new ArrayList<>();
            ArrayList<Restriction> reading92Restrictions = new ArrayList<>();
            ArrayList<Restriction> englishRestrictions = new ArrayList<>();
            ArrayList<Restriction> readingRcRestrictions = new ArrayList<>();
            ArrayList<Restriction> skRestrictions = new ArrayList<>();
            ArrayList<Restriction> eaRestrictions = new ArrayList<>();

            ArrayList<String> testNames = new ArrayList<>();
            ArrayList<String> testDates = new ArrayList<>();

            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                String testNameFieldName = FIELD_TEST_NAME + testNum;
                String testDateFieldName = FIELD_TEST_DATE + testNum;
                testNames.add(testNameFieldName);
                testDates.add(testDateFieldName);
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    for (int testScoreNum = 1; testScoreNum <= MAX_NUM_SCORES; testScoreNum++) {
                        String subjectContentFieldName = FIELD_TEST_SC + testNum + subjectContentNum;
                        verbalRestrictions.add(Restriction.pattern(subjectContentFieldName, "^70$"));
                        math73Restrictions.add(Restriction.pattern(subjectContentFieldName, "^73$"));
                        math95Restrictions.add(Restriction.pattern(subjectContentFieldName, "^95$"));
                        crRestrictions.add(Restriction.pattern(subjectContentFieldName, "^CR$"));
                        reading92Restrictions.add(Restriction.pattern(subjectContentFieldName, "^92$"));
                        englishRestrictions.add(Restriction.pattern(subjectContentFieldName, "^94$"));
                        readingRcRestrictions.add(Restriction.pattern(subjectContentFieldName, "^RC$"));
                        skRestrictions.add(Restriction.pattern(subjectContentFieldName, "^SK$"));
                        eaRestrictions.add(Restriction.pattern(subjectContentFieldName, "^EA$"));
                    }
                }
            }
            int index = 0;
            Date firstOfMarch2005 = getDate(Calendar.MARCH, 1, 2005);
            for (String testName : testNames) {
                String testDate = testDates.get(index);

                ValidationRule currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                Restriction.pattern(testName, "^SAT$"),
                                Restriction.lessThan(testDate, firstOfMarch2005, "MMddyyyy")))
                        .testThen(Restriction.and(
                                Restriction.or(verbalRestrictions.toArray(new Restriction[0])),
                                Restriction.or(math73Restrictions.toArray(new Restriction[0]))));
                validationRules08_19.add(currentValidationRule);

                currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                Restriction.pattern(testName, "^SAT$"),
                                Restriction.greaterThanOrEquals(testDate, firstOfMarch2005, "MMddyyyy")))
                        .testThen(Restriction.and(
                                Restriction.or(crRestrictions.toArray(new Restriction[0])),
                                Restriction.or(math73Restrictions.toArray(new Restriction[0]))));
                validationRules08_19.add(currentValidationRule);

                currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                Restriction.pattern(testName, "^ACT$")))
                        .testThen(Restriction.and(
                                Restriction.or(reading92Restrictions.toArray(new Restriction[0])),
                                Restriction.or(englishRestrictions.toArray(new Restriction[0])),
                                Restriction.or(math95Restrictions.toArray(new Restriction[0]))));
                validationRules08_19.add(currentValidationRule);

                currentValidationRule = ValidationRule
                        .testIf(Restriction.and(
                                new RestrictionForRecordType(FL_EXPORT.RECORD00,
                                        Restriction.pattern(FIELD_ADDRESSED_DST_NUM, PATTERN_BRIGHT_FUTURES)),
                                Restriction.pattern(testName, "^CPT$")))
                        .testThen(Restriction.and(
                                Restriction.or(readingRcRestrictions.toArray(new Restriction[0])),
                                Restriction.or(skRestrictions.toArray(new Restriction[0])),
                                Restriction.or(eaRestrictions.toArray(new Restriction[0]))));
                validationRules08_19.add(currentValidationRule);

                index++;
            }

            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_19.toArray(new ValidationRule[0])),
                            "If Addressed Institution is 95, Bright Futures, then "
                                    + "SAT must have Verbal (70) and Math (73) scores for tests taken before March 2005 or "
                                    + "Critical Reading (CR) and Math (73) scores for March 2005 forward; "
                                    + "ACT must at least have Reading (92), English (94) and Math (95) scores; "
                                    + "and CPT must at least have Reading (RC), Sentence Skills (SK), and Elementary Algebra (EA) scores."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "20");
            List<ValidationRule> validationRules08_20 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    ValidationRule currentValidationRule = ValidationRule
                            .testIf(Restriction.pattern(testNameFieldName, "^FCB$"))
                            .testThen(Restriction.pattern(FIELD_TEST_ST + testNum + subjectContentNum + 1, "^PF$"));
                    validationRules08_20.add(currentValidationRule);
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_20.toArray(new ValidationRule[0])),
                            "If Test Name is FCB, then Test Score Type 1 must be PF."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "21");
            List<ValidationRule> validationRules08_21 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    for (int scoreNum = 1; scoreNum <= MAX_NUM_SCORES; scoreNum++) {
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.pattern(testNameFieldName, "^EB1|ECA|ECG|EAH$"))
                                .testThen(Restriction.pattern(FIELD_TEST_ST + testNum + subjectContentNum + scoreNum,
                                        "^RC|PI|SS|SC|TS$"));
                        validationRules08_21.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_21.toArray(new ValidationRule[0])),
                            "If Test Name is EB1 (Biology EOC), ECA (Algebra I EOC), ECG (Geometry EOC), or EAH (U.S. History EOC), "
                                    + "then Test Score Type must be RC, PI, SS, SC, or TS."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "22");
            List<ValidationRule> validationRules08_22 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    String testDateFieldName = FIELD_TEST_DATE + testNum;
                    for (int scoreNum = 1; scoreNum <= MAX_NUM_SCORES; scoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + scoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testNameFieldName, "^ECA$"),
                                        Restriction.pattern(scoreTypeFieldName, "^TS$")))
                                .testThen(
                                        new RestrictionCompareSchoolYearByDate(testDateFieldName, "20102011", EQUALS));
                        validationRules08_22.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_22.toArray(new ValidationRule[0])),
                            "If Test Name is ECA, then Test Score Type TS is valid only if the Test Date falls within "
                                    + "the 2010-2011 school year."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "23");
            List<ValidationRule> validationRules08_23 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    String testDateFieldName = FIELD_TEST_DATE + testNum;
                    for (int scoreNum = 1; scoreNum <= MAX_NUM_SCORES; scoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + scoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testNameFieldName, "^EB1|ECG$"),
                                        Restriction.pattern(scoreTypeFieldName, "^TS$")))
                                .testThen(
                                        new RestrictionCompareSchoolYearByDate(testDateFieldName, "20112012", EQUALS));
                        validationRules08_23.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_23.toArray(new ValidationRule[0])),
                            "If Test Name is EB1 or ECG, Test Score Type TS is valid only if Test Date falls "
                                    + "within the 2011-2012 school year."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation();
            ruleAssociation.addAssociation(TransferObjectRecord.RECORD_TYPE_08, "24");
            List<ValidationRule> validationRules08_24 = new ArrayList<>();
            for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                for (int subjectContentNum = 1; subjectContentNum <= MAX_NUM_SUBJECT_CONTENT; subjectContentNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    String testDateFieldName = FIELD_TEST_DATE + testNum;
                    for (int scoreNum = 1; scoreNum <= MAX_NUM_SCORES; scoreNum++) {
                        String scoreTypeFieldName = FIELD_TEST_ST + testNum + subjectContentNum + scoreNum;
                        ValidationRule currentValidationRule = ValidationRule
                                .testIf(Restriction.and(
                                        Restriction.pattern(testNameFieldName, "^EAH$"),
                                        Restriction.pattern(scoreTypeFieldName, "^TS$")))
                                .testThen(
                                        new RestrictionCompareSchoolYearByDate(testDateFieldName, "20122013", EQUALS));
                        validationRules08_24.add(currentValidationRule);
                    }
                }
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules08_24.toArray(new ValidationRule[0])),
                            "If Test Name is EAH, then Test Score Type TS is valid on if the Test Date falls "
                                    + "within the 2012-2013 school year."));
            m_ruleAssociations.add(ruleAssociation);
        }
    }


    /**
     * Initialize rule associations 09.
     */
    private void initializeRuleAssociations09() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(
                                Arrays.asList(TransferObjectRecord.RECORD_TYPE_01,
                                        TransferObjectRecord.RECORD_TYPE_02,
                                        TransferObjectRecord.RECORD_TYPE_03,
                                        TransferObjectRecord.RECORD_TYPE_04,
                                        TransferObjectRecord.RECORD_TYPE_05,
                                        TransferObjectRecord.RECORD_TYPE_06,
                                        TransferObjectRecord.RECORD_TYPE_07,
                                        TransferObjectRecord.RECORD_TYPE_08)))),
                "This record is out of sequence if it immediately follows a record with a type greater than I08"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "9");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.or(
                                Restriction.pattern(FIELD_INCIDENT_TYPE,
                                        "^(ALC|ARS|BAT|BRK|BUL|DOC|DRD|DRU|FIT|HAR|HOM|KID|OMC|ROB|STL|SXB|SXH|SXO|TBC|TRE|TRS|UBL|UHR|VAN|WPO|ZZZ)$"),
                                Restriction.and(
                                        Restriction.pattern(FIELD_INCIDENT_TYPE, "^(DRG|MVT)$"),
                                        new RestrictionCompareSchoolYearByDate(FIELD_INCIDENT_DATE, "20052006",
                                                LESS)),
                                Restriction.and(
                                        Restriction.pattern(FIELD_INCIDENT_TYPE, "^(HAZ|PHA|SXA)$"),
                                        new RestrictionCompareSchoolYearByDate(FIELD_INCIDENT_DATE, "20142015",
                                                GREATER))))),
                "Incident Type must be ALC, ARS, BAT, BRK, BUL, DOC, DRD, DRU, FIT, HAR, HOM, KID, OMC, ROB, STL, SXB, SXH, "
                        + "SXO, TBC, TRE, TRS, UBL, UHR, VAN, WPO or ZZZ. DRG and MVT may still be used for "
                        + "records prior to 2005-2006. BHA may be used for records prior to 2010-2011. "
                        + "HAZ, PHA, and SXA are valid as of 2014-2015."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "10");
        ruleAssociation.setProcessor("Incident Date must be a valid date (MMDDCCYY) not later than the current date.",
                Restriction.and(
                        Restriction.byDateFormat(FIELD_INCIDENT_DATE),
                        Restriction.lessThanOrEquals(FIELD_INCIDENT_DATE,
                                Calendar.getInstance().getTime())));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "11");
        ruleAssociation.setProcessor("The first character of Discipline/Resultant Action Code must be "
                + "C, D, E, F, H, I, O, P, S, L, M, R or U. A and D may still be used for records prior to 20052006.",
                Restriction.or(
                        Restriction.and(
                                Restriction.or(
                                        Restriction.pattern(FIELD_INCIDENT_DATE, PATTERN_EMPTY),
                                        new RestrictionCompareSchoolYearByDate(FIELD_INCIDENT_DATE, "20042005",
                                                GREATER)),
                                Restriction.pattern(FIELD_ACTION_CODE, "^(C|D|E|F|H|I|O|P|S|L|M|R|U).*$")),
                        Restriction.and(
                                Restriction.pattern(FIELD_INCIDENT_DATE, PATTERN_NOT_EMPTY),
                                new RestrictionCompareSchoolYearByDate(FIELD_INCIDENT_DATE, "20052006",
                                        LESS),
                                Restriction.pattern(FIELD_ACTION_CODE, "^(A|D).*$"))));
        m_ruleAssociations.add(ruleAssociation);

        // TODO action code is 1 character field, so the rule doesn't make sense
        // ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "12");
        // ruleAssociation.setProcessor("The Second through Fifth characters of Discipline/Resultant
        // Action Code "
        // + "must represent a valid school on the MSID file. ",
        // Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_SCHOOL_CODES,
        // new ValueAdjuster() {
        // @Override
        // public String getAdjustedValue(String value, FLExportConfiguration helper) {
        // return value != null && value.length() >= 5 ? value.substring(2, 5) : value;
        // }
        // }, FIELD_ACTION_CODE));
        // m_ruleAssociations.add(ruleAssociation);

        Calendar nowPlusOneYear = Calendar.getInstance();
        nowPlusOneYear.add(Calendar.YEAR, 1);
        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "13");
        ruleAssociation.setProcessor("Discipline/Resultant Action Date must be a valid date (MMDDCCYY), "
                + "not more than one year in the future.",
                Restriction.and(Restriction.byDateFormat(FIELD_ACTION_DATE),
                        Restriction.lessThanOrEquals(FIELD_ACTION_DATE, nowPlusOneYear.getTime())));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "14");
        ruleAssociation.setProcessor("If not blank, Discipline/Resultant Duration must be numeric.",
                Restriction.pattern(FIELD_ACTION_DURATION, PATTERN_NOT_EMPTY),
                Restriction.pattern(FIELD_ACTION_DURATION, PATTERN_NUMERIC));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "15");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is S, then "
                + "the Discipline/Duration must be > 0 and <= 90.",
                Restriction.equals(FIELD_ACTION_CODE, "C"),
                Restriction.between(FIELD_ACTION_DURATION, 0, 90, false, true));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "16");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is C, L, M or R "
                + "then the Discipline/Duration must be = 0.",
                Restriction.pattern(FIELD_ACTION_CODE, "^(C|L|M|R)$"),
                Restriction.equals(FIELD_ACTION_DURATION, Double.valueOf(0)));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "17");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is E, F or P, "
                + " then the Discipline/Duration must be > 0 and <= 210. ",
                Restriction.pattern(FIELD_ACTION_CODE, "^(E|F|P)$"),
                Restriction.between(FIELD_ACTION_DURATION, 0, 210, false, true));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "18");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is I or O, "
                + "then the Discipline/Duration must be > 0 and <= 10.",
                Restriction.pattern(FIELD_ACTION_CODE, "^(I|O)$"),
                Restriction.between(FIELD_ACTION_DURATION, 0, 10, false, true));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "19");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is U, "
                + "then the Discipline/Duration must be > 0 and <= 45.",
                Restriction.equals(FIELD_ACTION_CODE, "U"),
                Restriction.between(FIELD_ACTION_DURATION, 0, 45, false, true));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_09, "20");
        ruleAssociation.setProcessor("If the Discipline/Resultant Action Code is H, "
                + "then the Discipline/Duration must be > 0 and <= 90.",
                Restriction.equals(FIELD_ACTION_CODE, "H"),
                Restriction.between(FIELD_ACTION_DURATION, 0, 90, false, true));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 10.
     */
    private void initializeRuleAssociations10() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "2");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionPreviousRecordType(
                                Arrays.asList(
                                        TransferObjectRecord.RECORD_TYPE_08,
                                        TransferObjectRecord.RECORD_TYPE_09)))),
                "This record is out of sequence if it immediately follows a record with a type not I08 or I09"));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "3");
            List<ValidationRule> validationRules10_03 = new ArrayList<>();
            for (int testPasNum = 1; testPasNum <= MAX_NUM_TEST_PAS; testPasNum++) {
                String testPasNameFieldName = FIELD_TEST_NAME + testPasNum;
                String testPasDateFieldName = FIELD_TEST_DATE + testPasNum;
                String testPasGradeLevelFieldName = FIELD_TEST_GRADE_LEVEL + testPasNum;
                String testPasSubjectContentFieldName = FIELD_TEST_SC + testPasNum;

                List<Restriction> orRestrictions = new ArrayList<>();

                for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    String testDateFieldName = FIELD_TEST_DATE + testNum;
                    String testGradeLevelFieldName = FIELD_TEST_GRADE_LEVEL + testNum;

                    for (int testScNum = 1; testScNum <= MAX_NUM_SUBJECT_CONTENT; testScNum++) {
                        String testSubjectContentFieldName = FIELD_TEST_SC + testNum + testScNum;

                        Map<String, String> equalFields = new HashMap<String, String>();
                        equalFields.put(testPasNameFieldName, testNameFieldName);
                        equalFields.put(testPasDateFieldName, testDateFieldName);
                        equalFields.put(testPasGradeLevelFieldName, testGradeLevelFieldName);
                        equalFields.put(testPasSubjectContentFieldName, testSubjectContentFieldName);

                        orRestrictions.add(new RestrictionCrossRecordsEqualFields(equalFields, FL_EXPORT.RECORD08));
                    }
                }

                validationRules10_03.add(ValidationRule
                        .testIf(Restriction.pattern(testPasNameFieldName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.or(orRestrictions.toArray(new Restriction[0]))));
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules10_03.toArray(new ValidationRule[0])),
                            "Must have associated I08 Test record(s)"));
            m_ruleAssociations.add(ruleAssociation);
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_10, 1))),
                "There is more than one I10 record"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_TEST_NAME + 1, PATTERN_NOT_EMPTY))),
                "Test block # 1 is empty"));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "6");
            List<ValidationRule> validationRules10_06 = new ArrayList<>();
            for (int testNum = 2; testNum < MAX_NUM_TEST_PAS; testNum++) {
                String previousTestNameField = FIELD_TEST_NAME + (testNum - 1);
                String testNameField = FIELD_TEST_NAME + testNum;

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(previousTestNameField, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(testNameField, PATTERN_EMPTY));

                validationRules10_06.add(rule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules10_06.toArray(new ValidationRule[0])),
                    "Test block is preceded by an empty test block"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "7");
            List<ValidationRule> validationRules10_07 = new ArrayList<>();
            for (int testPasNum = 1; testPasNum <= MAX_NUM_TEST_PAS; testPasNum++) {
                String testPasNameFieldName = FIELD_TEST_NAME + testPasNum;
                String testPasDateFieldName = FIELD_TEST_DATE + testPasNum;
                String testPasGradeLevelFieldName = FIELD_TEST_GRADE_LEVEL + testPasNum;

                List<Restriction> orRestrictions = new ArrayList<>();

                for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                    String testNameFieldName = FIELD_TEST_NAME + testNum;
                    String testDateFieldName = FIELD_TEST_DATE + testNum;
                    String testGradeLevelFieldName = FIELD_TEST_GRADE_LEVEL + testNum;

                    Map<String, String> equalFields = new HashMap<String, String>();
                    equalFields.put(testPasNameFieldName, testNameFieldName);
                    equalFields.put(testPasDateFieldName, testDateFieldName);
                    equalFields.put(testPasGradeLevelFieldName, testGradeLevelFieldName);

                    orRestrictions.add(new RestrictionCrossRecordsEqualFields(equalFields, FL_EXPORT.RECORD08));
                }

                validationRules10_07.add(ValidationRule
                        .testIf(Restriction.pattern(testPasNameFieldName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.or(orRestrictions.toArray(new Restriction[0]))));
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules10_07.toArray(new ValidationRule[0])),
                            "Test name/grade/date has no matching IS08 test"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "8");
            List<ValidationRule> validationRules10_08 = new ArrayList<>();
            for (int testPasNum = 1; testPasNum <= MAX_NUM_TEST_PAS; testPasNum++) {
                String testPasSubjectContentFieldName = FIELD_TEST_SC + testPasNum;

                List<Restriction> orRestrictions = new ArrayList<>();

                for (int testNum = 1; testNum <= MAX_NUM_TEST; testNum++) {
                    for (int testScNum = 1; testScNum <= MAX_NUM_SUBJECT_CONTENT; testScNum++) {
                        String testSubjectContentFieldName = FIELD_TEST_SC + testNum + testScNum;

                        Map<String, String> equalFields = new HashMap<String, String>();
                        equalFields.put(testPasSubjectContentFieldName, testSubjectContentFieldName);
                        orRestrictions.add(new RestrictionCrossRecordsEqualFields(equalFields, FL_EXPORT.RECORD08));
                    }
                }

                validationRules10_08.add(ValidationRule
                        .testIf(Restriction.pattern(testPasSubjectContentFieldName, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.or(orRestrictions.toArray(new Restriction[0]))));
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules10_08.toArray(new ValidationRule[0])),
                            "Subject content has no matching IS08 subject content"));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_10, "9");
            List<ValidationRule> validationRules10_09 = new ArrayList<>();
            for (int testPasNum = 1; testPasNum <= MAX_NUM_TEST_PAS; testPasNum++) {
                String testPasNameField = FIELD_TEST_NAME + testPasNum;
                String testPasNumberField = FIELD_TEST_PAS_NUM + testPasNum;

                validationRules10_09.add(ValidationRule
                        .testIf(Restriction.pattern(testPasNumberField, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(testPasNameField, PATTERN_EMPTY)));
            }
            ruleAssociation
                    .setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                            validationRules10_09.toArray(new ValidationRule[0])),
                            "PAS Number is blank within a test block that is not blank"));
            m_ruleAssociations.add(ruleAssociation);
        }

        // TODO implement rule 10 after will be clear how to determine if test is FCAT SSS Reading
        // and SSS Mathematics subtest, so each FCAT SSS Reading and SSS Mathematics subtest taken
        // after 1/1/2008 must have related I10 record with PAS Code
    }


    /**
     * Initialize rule associations 11.
     */
    private void initializeRuleAssociations11() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "1");
        ruleAssociation.setProcessor(
                "If Addressed Institution is the MSIX Federal Migrant Program then this record is required. "
                        + "Only one I11 record can be sent per transcript. The first iteration of "
                        + "Qualifying Arrival Information must be provided. This should correlate to the current "
                        + "Qualifying Arrival Information for the student.",
                ValidationRule.testIf(Restriction.alwaysTrue()).testThen(
                        new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_11, 1)),
                ValidationRule.testIf(s_addressedToMsix).testThen(
                        new RestrictionByNumOfRecordType(TransferObjectRecord.RECORD_TYPE_11, 1, 1)));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "5");
        ruleAssociation.setProcessor(
                "District Number, Current Enrollment must be the same as that found in the sending "
                        + "institution field on the last Header record. ",
                new RestrictionCompareRecordField(FIELD_DISTRICT_NUMBER_CE, null,
                        FL_EXPORT.RECORD00,
                        FIELD_SENDING_DST_NUM, new ValueAdjuster() {
                            @Override
                            public String getAdjustedValue(String value, FLExportConfiguration helper) {
                                return value.substring(5);
                            }
                        }));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "6");
        ruleAssociation.setProcessor(
                "School Number, Current Enrollment must be the same as that found "
                        + "in the sending institution field on the last Header record.",
                new RestrictionCompareRecordField(FIELD_SCHOOL_NUMBER_CE, FL_EXPORT.RECORD00,
                        FIELD_SENDING_SKL_NUM));
        m_ruleAssociations.add(ruleAssociation);

        for (int i = 1; i <= 10; i++) {
            String qadFieldName = FIELD_QA_DATE + Integer.toString(i);
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "8");
            ruleAssociation.setProcessor("Qualifying Arrival Date (QAD) must be a valid date "
                    + "(MMDDCCYY) not later than the current date.",
                    Restriction.and(
                            Restriction.byDateFormat(qadFieldName),
                            Restriction.lessThanOrEquals(qadFieldName, Calendar.getInstance().getTime())));
            m_ruleAssociations.add(ruleAssociation);

            String qadFromStateFieldName = FIELD_QA_FROM_STATE + Integer.toString(i);
            String qadFromCountryFieldName = FIELD_QA_FROM_COUNTRY + Integer.toString(i);
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "10");
            ruleAssociation.setProcessor("Qualifying Arrival Date (QAD) From State field may be blank or 'ZZ'. "
                    + "Otherwise, if the QAD From Country is the United States, it must be a valid US state "
                    + "code as found in Appendix H.; If the QAD From Country is Canada, Mexico or in South America, "
                    + "it must be a valid state code as found in Appendix BB PLEASE NOTE: Puerto Rico (PR) "
                    + "is the only applicable U.S. Territory/Commonwealth the Federal MSIX system will accept.",

                    ValidationRule.testIf(Restriction.equals(qadFromCountryFieldName, "US"))
                            .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_STATE_TER_CODES,
                                    qadFromStateFieldName)),
                    ValidationRule
                            .testIf(Restriction.pattern(qadFromCountryFieldName,
                                    "^(AE|BZ|BR|CC|CI|CL|CS|EC|ES|GT|GY|HO|MX|NU|PN|PX|PE|SX|UY|VE)$"))
                            .testThen(Restriction.byFormattedReferenceCodes(
                                    REFERENCE_TABLE_NAME_CAN_MEX_SOUTH_AMERICAN_STATE_CODES,
                                    qadFromStateFieldName)),
                    ValidationRule
                            .testIf(Restriction.and(Restriction.equals(qadFromCountryFieldName, "US"),
                                    s_addressedToMsix))
                            .testThen(Restriction.equals(qadFromStateFieldName, "PR")));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "11");
            ruleAssociation.setProcessor("Qualifying Arrival Date (QAD) from Country may be blank or 'ZZ'. "
                    + "Otherwise it must be a valid country code from Appendix G.",
                    Restriction.or(Restriction.pattern(qadFromCountryFieldName, PATTERN_EMPTY),
                            Restriction.equals(qadFromCountryFieldName, "ZZ"),
                            Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_COUNTRY_CODES,
                                    qadFromStateFieldName)));
            m_ruleAssociations.add(ruleAssociation);

            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "12");
            ruleAssociation.setProcessor("Qualifying Arrival Date (QAD) to City must not be blank.",
                    Restriction.pattern(FIELD_QA_TO_CITY + Integer.toString(i), PATTERN_NOT_EMPTY));
            m_ruleAssociations.add(ruleAssociation);
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_11, "14");
        ruleAssociation.setProcessor(
                "Migrant Residency Date must be a valid date (MMDDCCYY) not later than the current date.",
                Restriction.and(
                        Restriction.byDateFormat(FIELD_MIGRANT_RESID_DATE),
                        Restriction.lessThanOrEquals(FIELD_MIGRANT_RESID_DATE, Calendar.getInstance().getTime())));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 99 ATV.
     */
    private void initializeRuleAssociations99ATV() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                .testIf(Restriction.alwaysTrue())
                .testThen(Restriction.pattern(FIELD_TYPE_CODE_QUALIFIER, "^S[AB]$"))),
                "Activity/Award Type Code Qualifier must be SA or SB."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TYPE_CODE_QUALIFIER, "^SA$"))
                        .testThen(Restriction.and(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_ACTIVITY_AWARDS,
                                        FIELD_TYPE_CODE),
                                Restriction.pattern(FIELD_TYPE_CODE, "^(!?M).*$"))),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TYPE_CODE_QUALIFIER, "^SB$"))
                        .testThen(Restriction.pattern(FIELD_TYPE_CODE, "^M0[1-7]|ZZZ$"))),
                "If Activity/Award Type Code Qualifier is SA, Activity/Award Type Code must be "
                        + "A01-A35, AA1, AA2, AZZ, B01-B04, BZZ, C01-C07, C0Z, CA1, CB1, CC1-CC3, CZZ, D01-D04, DZZ, "
                        + "E01-E26, EA1, EA2, EZZ, F01-F03, FZZ, G01-G07, GZZ, H01-H06, HZZ, I01-I09, IZZ, J01-J15, JZZ, "
                        + "K01-K04, KZZ, L01-L03, LZZ or ZZZ. "
                        + "If Activity/Award Type Code Qualifier is SB, Activity/Award Type Code must be M01-M07 or ZZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TIME_QUALIFYING_CODE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_TIME_QUALIFYING_CODE,
                                "^52|8[RS]|D[AW]|H[RT]|M[JO]|S[12]|UN|WK|YR|Z[ABCZ]$"))),
                "If Code Qualifying Time Involved in Activity is not blank, it must be "
                        + "52, 8R, 8S, DA, DW, HR, HT, MJ, MO, S1, S2, UN, WK, YR, ZA, ZB, ZC or ZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_TIME_QUALIFYING_CODE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_INVOLVED_TIME, "^[\\d]+$"))),
                "If Code Qualifying Time Involved in Activity is not blank, then Time Involved in Activity cannot be blank, "
                        + "and must be numeric."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "9");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_PARTICIPATION_LVL, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_INVOLVED_TIME,
                                "^0[K1-9]|1[0-2]|AD|E[LM]|HG|I[FN]|MS|P[0-5K]|SS|UN|V[RS]$"))),
                "If Level of Participation in Activity is not blank, then it must be "
                        + "0K, 01-12, AD, EL, EM, HG, IF, IN, MS, P0-P5, PK, SS, UN, VR or VS."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "10");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_STD_WAS_PAID, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_STD_WAS_PAID, "^Y|N$"))),
                "If Student Was Paid to Participate is not blank, it must be Y or N."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "11");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_SCHOOL_SPONSORED, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_SCHOOL_SPONSORED, "^Y|N$"))),
                "If School Sponsored Activity is not blank, it must be Y or N."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "12");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_STD_WAS_RECRUITED, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_STD_WAS_RECRUITED, "^Y|N$"))),
                "If Student Was Recruited is not blank, it must be Y or N."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "13");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_QUALIFIER_1, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PART_DATE_QUALIFIER,
                                FIELD_DATE_QUALIFIER_1))),
                "If Participation Date Qualifier 1 is not blank, it must be "
                        + "007, 036, 043, 050, 055, 102, 103, 196, 197, 198, 237, 270, 275, 336, "
                        + "337, 467, 574, 576 or ZZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "14");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_QUALIFIER_1, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_DATE_FORMAT_1, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_DATE_FORMAT_1,
                                        START + regexContains("C[MY]|D[8B]|RD[458]") + END)))),
                "If Participation Date Qualifier 1 is not blank, then Participation Date Format 1 cannot be blank, "
                        + "and must be CM, CY, D8, DB, RD4, RD5 or RD8."));
        m_ruleAssociations.add(ruleAssociation);


        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "15");
            m_ruleAssociations.add(ruleAssociation);
            ArrayList<ValidationRule> validationRules99ATV_15 = new ArrayList<>();
            for (Entry<String, String> dateFormats : s_fasterDateFormats.entrySet()) {
                String fasterDateFormat = dateFormats.getKey();
                String dateFormat = dateFormats.getValue();

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_1, START + regexContains(fasterDateFormat) + END))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat(dateFormat, FIELD_DATE_1),
                                Restriction.lessThanOrEquals(FIELD_DATE_1, new Date(), dateFormat)));
                validationRules99ATV_15.add(rule);
            }
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(new RuleSet(validationRules99ATV_15.toArray(new ValidationRule[0])),
                            "If Participation Date Format 1 is not blank, then Participation Date 1 cannot be blank, "
                                    + "and must be a valid date, not in future."));
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "16");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_QUALIFIER_2, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PART_DATE_QUALIFIER,
                                FIELD_DATE_QUALIFIER_2))),
                "If Participation Date Qualifier 2 is not blank, it must be 007, 036, 043, 050, 055, "
                        + "102, 103, 196, 197, 198, 237, 270, 275, 336, 337, 467, 574, 576 or ZZZ."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "17");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_QUALIFIER_2, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.and(
                                Restriction.pattern(FIELD_DATE_FORMAT_2, PATTERN_NOT_EMPTY),
                                Restriction.pattern(FIELD_DATE_FORMAT_2,
                                        START + regexContains("C[MY]|D[8B]|RD[458]") + END)))),
                "If Participation Date Qualifier 2 is not blank, then Participation Date Format 2 cannot be blank, "
                        + "and must be CM, CY, D8, DB, RD4, RD5 or RD8."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "18");
            m_ruleAssociations.add(ruleAssociation);
            ArrayList<ValidationRule> validationRules99ATV_18 = new ArrayList<>();
            for (Entry<String, String> dateFormats : s_fasterDateFormats.entrySet()) {
                String fasterDateFormat = dateFormats.getKey();
                String dateFormat = dateFormats.getValue();

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_2, START + regexContains(fasterDateFormat) + END))
                        .testThen(Restriction.and(
                                Restriction.byDateFormat(dateFormat, FIELD_DATE_2),
                                Restriction.lessThanOrEquals(FIELD_DATE_2, new Date(), dateFormat)));
                validationRules99ATV_18.add(rule);
            }
            ruleAssociation.setProcessor(
                    new FLValidationRuleSet(new RuleSet(validationRules99ATV_18.toArray(new ValidationRule[0])),
                            "If Participation Date Format 2 is not blank, then Participation Date 2 cannot be blank, "
                                    + "and must be a valid date, not in future."));
        }

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "19");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.and(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PART_DATE_QUALIFIER,
                                        FIELD_DATE_QUALIFIER_1),
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PART_DATE_QUALIFIER,
                                        FIELD_DATE_QUALIFIER_2)))),
                "Invalid Participation Date Qualifier"));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "20");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_1, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_DATE_FORMAT_1, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_2, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_DATE_FORMAT_2, PATTERN_NOT_EMPTY))),
                "Participation Date Format x required if Participation Date Qualifier is not blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "21");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DATE_FORMAT_1,
                                START + regexContains("C[MY]|D[8B]|RD[458]") + END)),
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DATE_FORMAT_2,
                                START + regexContains("C[MY]|D[8B]|RD[458]") + END))),
                "Invalid Participation Date Format. Must= CM, CY, D8, DB, RD4, RD5 or RD8."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "22");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_1, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_DATE_1, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_2, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_DATE_2, PATTERN_NOT_EMPTY))),
                "Participation Date x required if participation Date Format if not blank."));
        m_ruleAssociations.add(ruleAssociation);

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "23");
            ArrayList<ValidationRule> validationRules99ATV_23 = new ArrayList<>();
            for (Entry<String, String> dateFormats : s_fasterDateFormats.entrySet()) {
                String fasterDateFormat = dateFormats.getKey();
                String dateFormat = dateFormats.getValue();

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_1, START + regexContains(fasterDateFormat) + END))
                        .testThen(Restriction.byDateFormat(dateFormat, FIELD_DATE_1));
                validationRules99ATV_23.add(rule);

                rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_2, START + regexContains(fasterDateFormat) + END))
                        .testThen(Restriction.byDateFormat(dateFormat, FIELD_DATE_2));
                validationRules99ATV_23.add(rule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules99ATV_23.toArray(new ValidationRule[0])),
                    "Participation Date x invalid for specified format."));
        }

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "24");
            ArrayList<ValidationRule> validationRules99ATV_24 = new ArrayList<ValidationRule>();
            for (Entry<String, String> dateFormatPattern : s_fasterDateFormatsLengthChecker.entrySet()) {
                String dateFormat = dateFormatPattern.getKey();
                String pattern = dateFormatPattern.getValue();

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_1, START + regexContains(dateFormat) + END))
                        .testThen(Restriction.pattern(FIELD_DATE_1, pattern));
                validationRules99ATV_24.add(rule);

                rule = ValidationRule
                        .testIf(Restriction.pattern(FIELD_DATE_FORMAT_2, START + regexContains(dateFormat) + END))
                        .testThen(Restriction.pattern(FIELD_DATE_2, pattern));
                validationRules99ATV_24.add(rule);
            }

            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules99ATV_24.toArray(new ValidationRule[0])),
                    "Length of Participation Date invalid for specified format."));
            m_ruleAssociations.add(ruleAssociation);
        }

        {
            ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99ATV, "25");
            ArrayList<ValidationRule> validationRules99ATV_25 = new ArrayList<>();
            for (Entry<String, String> dateFormats : s_fasterDateFormats.entrySet()) {
                String fasterDateFormat = dateFormats.getKey();
                String dateFormat = dateFormats.getValue();

                ValidationRule rule = ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_DATE_FORMAT_1, START + regexContains(fasterDateFormat) + END),
                                Restriction.byDateFormat(dateFormat, FIELD_DATE_1)))
                        .testThen(
                                Restriction.lessThanOrEquals(FIELD_DATE_1, new Date(), dateFormat));
                validationRules99ATV_25.add(rule);

                rule = ValidationRule
                        .testIf(Restriction.and(
                                Restriction.pattern(FIELD_DATE_FORMAT_2, START + regexContains(fasterDateFormat) + END),
                                Restriction.byDateFormat(dateFormat, FIELD_DATE_2)))
                        .testThen(
                                Restriction.lessThanOrEquals(FIELD_DATE_2, new Date(), dateFormat));
                validationRules99ATV_25.add(rule);
            }
            ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(SKIP_RULE_IF_VALID,
                    validationRules99ATV_25.toArray(new ValidationRule[0])),
                    "Participation Date x cannot be in the future."));
            m_ruleAssociations.add(ruleAssociation);
        }
    }


    /**
     * Initialize rule associations 99 HC.
     */
    private void initializeRuleAssociations99HC() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                .testIf(Restriction.alwaysTrue())
                .testThen(Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DISEASES_CODES,
                        FIELD_DIS_COND_TYPE_CODE))),
                "Disease Condition Type Code must be a valid International Classification of Diseases (ICD) Code "
                        + "(Attachment 1 to format G99HS)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                .testIf(Restriction.alwaysTrue())
                .testThen(
                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PROC_TERMINOLOGY,
                                FIELD_MED_TREATMENT_TYPE))),
                "Medical Treatment Type Code must be a valid Current Procedural Terminology (CPT) Code "
                        + "(Attachment 2 to format G99HS) or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_1ST_ENC_DATE, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, PATTERN_NOT_EMPTY))),
                "If either Format for Date of First Encounter or Date of First Encounter is not blank then "
                        + "the other must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, "^C[MY]|D[8B]| +$"))),
                "Format for Date of First Encounter must be CM, CY, D8, DB or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.alwaysTrue()),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, START + regexContains("CM") + END))
                        .testThen(Restriction.byDateFormat("yyyyMM", FIELD_1ST_ENC_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, START + regexContains("CY") + END))
                        .testThen(Restriction.byDateFormat("yyyy", FIELD_1ST_ENC_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, START + regexContains("D8") + END))
                        .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_1ST_ENC_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_1ST_ENC_DATE_FORMAT, START + regexContains("DB") + END))
                        .testThen(Restriction.byDateFormat("MMddyyyy", FIELD_1ST_ENC_DATE))),
                "Date of First Encounter must be blank or formatted according to the code specified in "
                        + "Format for Date of First Encounter."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HC, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_DIS_COND_RESOLUTION, "^N|U|Y| +$"))),
                "Resolution of Disease or Condition must be N, U, Y or blank."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 99 HS.
     */
    private void initializeRuleAssociations99HS() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                .testIf(Restriction.alwaysTrue())
                .testThen(Restriction.or(
                        Restriction.and(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DISEASES_CODES,
                                        FIELD_HLTH_SCR_TYPE_CODE),
                                Restriction.pattern(FIELD_HLTH_SCR_TYPE_CODE, "^V[7|8].*$")),
                        Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PROC_TERMINOLOGY,
                                FIELD_HLTH_SCR_TYPE_CODE)))),
                "Health Screening Type Code must be a valid International Classification of Diseases (ICD) Code in the range "
                        + "V70 through V82.9 (Attachment 1 to format G99HS) or a valid Current Procedural Terminology (CPT) Code "
                        + "(Attachment 2 to format G99HS)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_HLTH_SCR_DATE, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, PATTERN_NOT_EMPTY))),
                "If either Format for Date of Health Screening or Date of Health Screening is not blank then the other must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, "^C[MY]|D[8B]| +$"))),
                "Format for Date of Health Screening must be CM, CY, D8, DB or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.alwaysTrue()),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, START + regexContains("CM") + END))
                        .testThen(Restriction.byDateFormat("yyyyMM", FIELD_HLTH_SCR_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, START + regexContains("CY") + END))
                        .testThen(Restriction.byDateFormat("yyyy", FIELD_HLTH_SCR_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, START + regexContains("D8") + END))
                        .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_HLTH_SCR_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_HLTH_SCR_DATE_FORMAT, START + regexContains("DB") + END))
                        .testThen(Restriction.byDateFormat("MMddyyyy", FIELD_HLTH_SCR_DATE))),
                "Date of Health Screening must be blank or formatted according to the code specified in Format for "
                        + "Date of Health Screening."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99HS, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_HLTH_SCR_RESULTS, "^ABN|NOR|B33| +$"))),
                "Results of Health Screening must be ABN, NOR, B33 or blank."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Initialize rule associations 99 IMM.
     */
    private void initializeRuleAssociations99IMM() {
        RuleAssociation ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "3");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(ValidationRule
                .testIf(Restriction.alwaysTrue())
                .testThen(Restriction.or(
                        Restriction.and(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_DISEASES_CODES,
                                        FIELD_IMM_TYPE_CODE),
                                Restriction.pattern(FIELD_IMM_TYPE_CODE, "^V0[3|7].*$")),
                        Restriction.and(
                                Restriction.byFormattedReferenceCodes(REFERENCE_TABLE_NAME_PROC_TERMINOLOGY,
                                        FIELD_IMM_TYPE_CODE),
                                Restriction.pattern(FIELD_IMM_TYPE_CODE, "^907(0[1-8]|12|18|28|49).*$"))))),
                "Immunization Type Code must be a valid International Classification of Diseases (ICD) Code "
                        + "in the range V03 through V07.9 (Attachment 1 to format G99HS) or a valid Current Procedural Terminology (CPT) "
                        + "Code in the range 90701 through 90749 (Attachment 2 to format G99HS)."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "4");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE_FORMAT, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_IMM_DATE, PATTERN_NOT_EMPTY)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_IMM_DATE_FORMAT, PATTERN_NOT_EMPTY))),
                "If either Format for Date of Immunization or Date of Immunization is not blank then the other must not be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "5");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_IMM_DATE_FORMAT, "^C[MY]|D[8B]| +$"))),
                "Format for Date of Immunization must be CM, CY, D8, DB or blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "6");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.alwaysTrue()),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE_FORMAT, START + regexContains("CM") + END))
                        .testThen(Restriction.byDateFormat("yyyyMM", FIELD_IMM_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE_FORMAT, START + regexContains("CY") + END))
                        .testThen(Restriction.byDateFormat("yyyy", FIELD_IMM_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE_FORMAT, START + regexContains("D8") + END))
                        .testThen(Restriction.byDateFormat("yyyyMMdd", FIELD_IMM_DATE)),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE_FORMAT, START + regexContains("DB") + END))
                        .testThen(Restriction.byDateFormat("MMddyyyy", FIELD_IMM_DATE))),
                "Date of Immunization must be blank or formatted according to the code specified in Format for "
                        + "Date of Immunization."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "7");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE, PATTERN_EMPTY))
                        .testThen(Restriction.pattern(FIELD_IMM_STATUS_CODE, "^0[1-9]|1[0-4]| +$")),
                ValidationRule
                        .testIf(Restriction.pattern(FIELD_IMM_DATE, PATTERN_NOT_EMPTY))
                        .testThen(Restriction.pattern(FIELD_IMM_STATUS_CODE, "^0[1-9]|1[0-4]$"))),
                "Immunization Status Code must be numeric and in the range 01-14. If Date of Immunization is blank, "
                        + "the Immunization Status Code may also be blank."));
        m_ruleAssociations.add(ruleAssociation);

        ruleAssociation = new RuleAssociation(TransferObjectRecord.RECORD_TYPE_99IMM, "8");
        ruleAssociation.setProcessor(new FLValidationRuleSet(new RuleSet(
                ValidationRule
                        .testIf(Restriction.alwaysTrue())
                        .testThen(Restriction.pattern(FIELD_IMM_SOURCE_CODE, "^CQ|H[CR]|IR|MG|PY|ST| +$"))),
                "Immunization Record Source Code must be CQ, HC, HR, IR, MG, PY, ST or blank."));
        m_ruleAssociations.add(ruleAssociation);
    }


    /**
     * Regex contains.
     *
     * @param pattern String
     * @return String
     */
    private static String regexContains(String pattern) {
        return ".*" + pattern + ".*";
    }


    /**
     * Wo.
     *
     * @param regex String
     * @return String
     */
    private static String wo(String regex) {
        return "(?!" + regex + ").*";
    }
}
