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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.ExportResultsComparisonManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNComparisonManager.
 */
public class TNComparisonManager extends ExportResultsComparisonManager {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * This is a wrapper class for ExportFormatRow classes. The purpose of this class is to
     * override the equals and compareTo method so that we can compare only the fields
     * that are needed for the Difference Engine.
     *
     * The equals method above is overridden from super class to compare all fields.
     * The main difference from super is not to compare fields with writebackIndicator=true.
     */
    protected class TNComparableExportFormatRow extends ComparableExportFormatRow {

        private ExportFormatRow m_tnResultRow;

        /**
         * Instantiates a new TN comparable export format row.
         *
         * @param resultRow ExportFormatRow
         */
        public TNComparableExportFormatRow(ExportFormatRow resultRow) {
            super(resultRow);
            this.m_tnResultRow = resultRow;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object.equals(Object obj)
         */
        @SuppressWarnings("synthetic-access")
        @Override
        public boolean equals(Object obj) {
            boolean isEqual = false;

            if (obj == null) {
                isEqual = false;
            } else if (this == obj) {
                isEqual = true;
            } else if (getClass() != obj.getClass()) {
                isEqual = false;
            } else {
                TNComparableExportFormatRow other = (TNComparableExportFormatRow) obj;
                if (m_tnResultRow == null) {
                    if (other.m_tnResultRow != null) {
                        isEqual = false;
                    }
                } else if (!m_tnResultRow.equals(other.m_tnResultRow)) {
                    /*
                     * Compare all export related data fields except the field that
                     * represents the change indicator column.
                     */
                    Set<String> keys = m_exportFormatMapping.keySet();
                    for (String key : keys) {
                        if (!key.equals(m_changeIndicatorColumnName)) {
                            DataFieldConfig field = m_exportFormatMapping.get(key);

                            if (!m_exportFieldsMapping.get(key).getWritebackInd()) {
                                String oldValue = (String) m_tnResultRow
                                        .getFieldValueByBeanPath(field.getDataField().getJavaName());
                                String newValue = (String) other.m_tnResultRow
                                        .getFieldValueByBeanPath(field.getDataField().getJavaName());

                                if (oldValue != null && newValue != null && oldValue.equals(newValue)) {
                                    isEqual = true;
                                    continue;
                                }

                                isEqual = false;
                                break;
                            }
                        }
                    }
                }
            }

            return isEqual;
        }

    }

    private static final String RECORD_TYPE = "RECORD TYPE";

    private X2Broker m_broker;
    private Map<String, ExportFormatField> m_exportFieldsMapping;
    private Set<String> m_commentList;
    private int m_grandTotal;
    private int m_restagingD;
    private int m_restagingN;
    private int m_restagingTotal;
    private Map<String, String> m_statisticByRecordType;
    private Set<String> m_uniqueKeys;

    /**
     * Instantiates a new TN comparison manager.
     */
    public TNComparisonManager() {}

    /**
     * Gets the comment list.
     *
     * @return the m_commentList
     */
    public Set<String> getCommentList() {
        if (m_commentList == null) {
            m_commentList = new HashSet<String>();
        }
        if (m_statisticByRecordType != null) {
            m_commentList.addAll(m_statisticByRecordType.values());
        }

        return m_commentList;
    }

    /**
     * Gets the grand total.
     *
     * @return the m_grandTotal
     */
    public int getGrandTotal() {
        return m_grandTotal;
    }

    /**
     * Initialize broker.
     *
     * @param broker X2Broker
     */
    public void initializeBroker(X2Broker broker) {
        m_broker = broker;
    }

    /**
     * Sets the restaging counts.
     *
     * @param restagingRows ArrayList<ExportFormatRow>
     * @param recordsType String
     * @param restExportFormatFields Collection<ExportFormatField>
     */
    public void setRestagingCounts(ArrayList<ExportFormatRow> restagingRows,
                                   String recordsType,
                                   Collection<ExportFormatField> restExportFormatFields) {
        buildCommentForRestagingRows(restagingRows,
                recordsType,
                restExportFormatFields);
    }

    /**
     * @see com.follett.fsc.core.k12.business.ExportResultsComparisonManager#compareResult(com.follett.fsc.core.k12.beans.ExportFormatResult,
     *      com.follett.fsc.core.k12.beans.ExportFormatResult)
     */
    /*
     * This is an exact copy from default method without the error for same result
     *
     * @see
     * com.follett.fsc.core.k12.business.ExportResultsComparisonManager#compareResult(com.follett.
     * fsc.core.k12.beans.ExportFormatResult, com.follett.fsc.core.k12.beans.ExportFormatResult)
     */
    @Override
    protected List<ExportFormatRow> compareResult(ExportFormatResult oldResult, ExportFormatResult newResult) {
        String recordType = oldResult.getDefinition().getName().substring(0, 6);
        List<ExportFormatRow> finalRowsList = new ArrayList<ExportFormatRow>();

        // Identify the change indicator column
        DataFieldConfig dataFieldConfig = m_exportFormatMapping.get(m_changeIndicatorColumnName);

        if (dataFieldConfig != null) {
            String indicatorField = dataFieldConfig.getDataField().getJavaName();
            /**
             * Setting the header and trailer.
             */
            setHeader(newResult.getHeading());
            setTrailer(newResult.getTrailer());

            // Now starting the compare
            QueryIterator oldResultIterator = getSortedResultRows(oldResult);
            QueryIterator newResultIterator = getSortedResultRows(newResult);

            /*
             * Compare each record in the export format row. The below ComparableExportFormatRow
             * class is a
             * wrapper to the ExportFormatRow class so that the equals and compareTo method can be
             * overridden.
             */
            TNComparableExportFormatRow oldRecord = null;
            TNComparableExportFormatRow newRecord = null;

            if (oldResultIterator != null && newResultIterator != null) {
                while (true) {
                    if ((!oldResultIterator.hasNext() && !newResultIterator.hasNext()) &&
                            (oldRecord == null && newRecord == null)) {
                        break;
                    } else if (oldRecord != null && newRecord != null && oldRecord.equals(newRecord)) {
                        // The equals method above is overridden in
                        // ComparableExportFormatRow to compare all fields

                        oldRecord = gotoNextRecord(oldResultIterator);
                        newRecord = gotoNextRecord(newResultIterator);
                    } else if (oldRecord != null && newRecord != null && !oldRecord.equals(newRecord)) {
                        // The equals method above is overridden in
                        // ComparableExportFormatRow to compare all fields

                        /*
                         * Compare old and new record and save to DB. The compare method below is
                         * overridden in
                         * ComparableExportFormatRow to compare only fields that have unique key
                         * indicator.
                         */
                        int comparison = oldRecord.compareTo(newRecord);
                        if (comparison > 0) {
                            ExportFormatRow newRow = (ExportFormatRow) newRecord.getResultRow().copyBean();
                            setInsertedRecord(finalRowsList, newRow, indicatorField, m_insertedRecordIndicator);

                            newRecord = gotoNextRecord(newResultIterator);
                        } else if (comparison < 0) {
                            ExportFormatRow oldRow = (ExportFormatRow) oldRecord.getResultRow().copyBean();
                            setDeletedRecord(finalRowsList, oldRow, indicatorField, m_deletedRecordIndicator);

                            oldRecord = gotoNextRecord(oldResultIterator);
                        } else {
                            ExportFormatRow oldRow = (ExportFormatRow) oldRecord.getResultRow().copyBean();
                            ExportFormatRow newRow = (ExportFormatRow) newRecord.getResultRow().copyBean();
                            setChangedRecord(finalRowsList, oldRow, newRow, indicatorField, m_updatedRecordIndicator);

                            oldRecord = gotoNextRecord(oldResultIterator);
                            newRecord = gotoNextRecord(newResultIterator);
                        }
                    } else if (oldRecord != null && newRecord == null) {
                        /*
                         * This case will happen only when newRecord is null and oldRecord is not
                         * null
                         * i.e. if the old result set's last record comes after the new result set's
                         * last record.
                         */
                        ExportFormatRow oldRow = (ExportFormatRow) oldRecord.getResultRow().copyBean();
                        setDeletedRecord(finalRowsList, oldRow, indicatorField, m_deletedRecordIndicator);

                        oldRecord = gotoNextRecord(oldResultIterator);
                    } else if (oldRecord == null && newRecord != null) {
                        /*
                         * This case will happen only when oldRecord is null and newRecord is not
                         * null
                         * i.e. if the new result set's last record comes after the old result set's
                         * last record.
                         */
                        ExportFormatRow newRow = (ExportFormatRow) newRecord.getResultRow().copyBean();
                        setInsertedRecord(finalRowsList, newRow, indicatorField, m_insertedRecordIndicator);

                        newRecord = gotoNextRecord(newResultIterator);
                    } else if (oldRecord == null && newRecord == null) {
                        // This case will happen only during the beginning.
                        oldRecord = gotoNextRecord(oldResultIterator);
                        newRecord = gotoNextRecord(newResultIterator);
                    }
                }
            }
        } else {
            m_errorList.add("Comparison Indicator Column name is invalid.");
        }

        if (oldResult.getOid().equals(newResult.getOid())) {
            m_errorList.add("Export Format Result1 and Export Format Result2 are same report. Result Set is empty.");
        }

        Collection<ExportFormatField> exportFormatFields =
                TNExportResultsHelper.getExportFormatField(newResult, m_broker);

        buildCommentForFinalRows(finalRowsList, recordType, exportFormatFields);

        return finalRowsList;
    }

    /**
     * This method builds a map of all the field names and DataFieldConfigs for a given export
     * format result.
     * Also populate helper map of all ExportFormatFields
     *
     * @param result ExportFormatResult
     * @return Map<String, DataFieldConfig>
     */
    @Override
    protected Map<String, DataFieldConfig> getExportFormatMapping(ExportFormatResult result) {
        Map<String, DataFieldConfig> exportFormatMapping = new LinkedHashMap<String, DataFieldConfig>();
        m_exportFieldsMapping = new LinkedHashMap<String, ExportFormatField>();
        X2Criteria criteria = getExportFormatField(result, false);
        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);

        QueryIterator iterator = m_broker.getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExportFormatField field = (ExportFormatField) iterator.next();
                DataFieldConfig dataField = field.getDataFieldConfig();

                exportFormatMapping.put(field.getName(), dataField);
                m_exportFieldsMapping.put(field.getName(), field);
            }
        } finally {
            iterator.close();
        }

        return exportFormatMapping;
    }

    /**
     * This method advances the given Export Format Result's iterator pointer to the next row.
     *
     * @param iterator QueryIterator
     * @return TNComparableExportFormatRow
     */
    @Override
    protected TNComparableExportFormatRow gotoNextRecord(QueryIterator iterator) {
        TNComparableExportFormatRow nextRecord = null;

        if (iterator.hasNext()) {
            nextRecord = new TNComparableExportFormatRow((ExportFormatRow) iterator.next());
        }

        return nextRecord;
    }

    /**
     * This method count all changed records and build comment for them.
     *
     * @param finalRowsList List of ExportFormatRow
     * @param recordType String
     * @param exportFormatFields Collection<ExportFormatField>
     */
    private void buildCommentForFinalRows(List<ExportFormatRow> finalRowsList,
                                          String recordType,
                                          Collection<ExportFormatField> exportFormatFields) {
        String comment = "";

        int total = 0;
        int totalE = 0;
        int totalD = 0;
        int totalN = 0;

        if (m_commentList == null) {
            m_commentList = new HashSet<String>();
        }
        if (m_statisticByRecordType == null) {
            m_statisticByRecordType = new HashMap<String, String>();
        }
        for (ExportFormatRow formatRow : finalRowsList) {
            if (formatRow.getDefinition() != null) {
                String recordTypeBeanPath = getRecorTypeBeanPath(formatRow.getDefinition());

                if (!StringUtils.isEmpty(recordTypeBeanPath)) {
                    String recordTypeValue = (String) formatRow.getFieldValueByBeanPath(recordTypeBeanPath);

                    if ("N".equals(recordTypeValue) &&
                            (m_uniqueKeys == null
                                    || !m_uniqueKeys.contains(getUniqueKey(exportFormatFields, formatRow)))) {
                        totalN += 1;
                        total += 1;
                        m_grandTotal += 1;
                    } else if ("E".equals(recordTypeValue)) {
                        totalE += 1;
                        total += 1;
                        m_grandTotal += 1;
                    } else if ("D".equals(recordTypeValue) &&
                            (m_uniqueKeys == null
                                    || !m_uniqueKeys.contains(getUniqueKey(exportFormatFields, formatRow)))) {
                        totalD += 1;
                        total += 1;
                        m_grandTotal += 1;
                    }

                }
            }
        }

        comment = "Total Records with Record Type = " + recordType + " - " + String.valueOf(total + m_restagingTotal)
                + ". Type E - " + totalE +
                ", Type D - " + (totalD + m_restagingD) + ", Type N - " + (totalN + m_restagingN) + ".";

        m_statisticByRecordType.put(recordType, comment);
    }

    /**
     * This method count restaging rows.
     *
     * @param restagingRows Collection<ExportFormatRow>
     * @param recordsType String
     * @param restExportFormatFields Collection<ExportFormatField>
     */
    private void buildCommentForRestagingRows(Collection<ExportFormatRow> restagingRows,
                                              String recordsType,
                                              Collection<ExportFormatField> restExportFormatFields) {
        if (m_uniqueKeys == null) {
            m_uniqueKeys = new HashSet<String>();
        }

        int totalD = 0;
        int totalN = 0;

        if (restagingRows != null) {
            for (ExportFormatRow formatRow : restagingRows) {
                if (formatRow.getDefinition() != null) {
                    String recordTypeBeanPath = getRecorTypeBeanPath(formatRow.getDefinition());

                    if (!StringUtils.isEmpty(recordTypeBeanPath)) {
                        String recordTypeValue = (String) formatRow.getFieldValueByBeanPath(recordTypeBeanPath);

                        if ("N".equals(recordTypeValue)) {
                            totalN += 1;
                            m_grandTotal += 1;

                            m_uniqueKeys.add(getUniqueKey(restExportFormatFields, formatRow));
                        } else if ("D".equals(recordTypeValue)) {
                            totalD += 1;
                            m_grandTotal += 1;

                            m_uniqueKeys.add(getUniqueKey(restExportFormatFields, formatRow));
                        }
                    }
                }
            }
        }

        m_restagingD = totalD;
        m_restagingN = totalN;
        m_restagingTotal = totalD + totalN;

        /*
         * Needed to report statistic in comment if there are only restaging results.
         * This method will be recalled and statistic for record type will be overwritten if there
         * are final rows.
         */
        buildCommentForFinalRows(Collections.EMPTY_LIST, recordsType, null);
    }

    /**
     * Looking for the RECORD TYPE bean path.
     *
     * @param efd ExportFormatDefinition
     * @return String
     */
    private String getRecorTypeBeanPath(ExportFormatDefinition efd) {
        String beanPath = null;

        Collection<ExportFormatField> effList = efd.getFields();

        for (ExportFormatField eff : effList) {
            if (RECORD_TYPE.equals(eff.getName())) {
                beanPath = eff.getDataFieldConfig().getDataField().getJavaName();
                break;
            }
        }

        return beanPath;
    }

    /**
     * Looking for unique key for the row.
     *
     * @param exportFormatFields Collection<ExportFormatField>
     * @param row ExportFormatRow
     * @return String
     */
    private String getUniqueKey(Collection<ExportFormatField> exportFormatFields, ExportFormatRow row) {
        StringBuilder uniqueRowKey = new StringBuilder();
        for (ExportFormatField exportFormatField : exportFormatFields) {
            String fieldValue = null;
            DataFieldConfig config = exportFormatField.getDataFieldConfig();
            if (config != null) {
                fieldValue = (String) row.getFieldValueByBeanPath(config.getDataField().getJavaName());
            }

            /*
             * If the value requires padding, pad it and trim it to field max length.
             */
            fieldValue = ExportFormatManager.doPadding(fieldValue == null ? "" : fieldValue,
                    (exportFormatField.getPaddingDirection() == 0
                            ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                            : exportFormatField.getPaddingDirection()),
                    exportFormatField.getPaddingChar(),
                    exportFormatField.getMaximumLength());

            uniqueRowKey.append(fieldValue);
        }

        return uniqueRowKey.toString();
    }

}
