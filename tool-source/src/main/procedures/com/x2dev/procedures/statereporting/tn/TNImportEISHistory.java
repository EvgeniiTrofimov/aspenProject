/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Helper class to process the data rows from the TN EIS files.
 *
 * @author Follett Software Company
 */
public class TNImportEISHistory {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Class members.
     */
    private X2Broker m_broker;
    private PlainDate m_date;
    private Map<String, LinkedList<ExportFormatDefinition>> m_efdMap;
    private ArrayList<String> m_efdOids;
    private Map<String, LinkedList<ExportFormatField>> m_effMap;
    private Map<String, LinkedList<ExportFormatResult>> m_efrMap;
    private Map<String, Long> m_efrLatestTimeStamp = new HashMap();
    private Organization m_organization;
    private Map<String, String> m_parameters;
    private String m_recordId;
    private int m_schoolYear;

    /**
     * Other constants
     */
    private static final String REGEX_EFR_NAME = "TN %%% %%% Uploads for %%%%";
    private static final String REGEX_PRC_NAME = "EFD_\\d{5}";

    /**
     * Instantiates a new TN import EIS history.
     *
     * @param organization Organization
     * @param broker X2Broker
     * @param parameters Map
     */
    public TNImportEISHistory(Organization organization, X2Broker broker, Map parameters) {
        if (broker == null) {
            throw new X2RuntimeException();
        }
        m_organization = organization;
        m_broker = broker;
        m_parameters = new HashMap<String, String>();
        m_parameters.putAll(parameters);
        populateEFDefinitionsMap();
        populateEFFiledsMap();
        populateEFResultsMap();
    }

    /**
     * Set date from the values of first line of operated file.
     *
     * @param firstLine void
     */
    public void setDate(String firstLine) {
        if (!StringUtils.isEmpty(firstLine)) {
            firstLine = firstLine.trim();
            if (firstLine.endsWith(".EIS") && firstLine.length() > 14) {
                String month = firstLine.substring(14, 16);
                String day = firstLine.substring(16, 18);
                String year = firstLine.substring(10, 14);

                Calendar cal = Calendar.getInstance();

                if (month.matches("\\d{2}") && day.matches("\\d{2}") && year.matches("\\d{4}")) {
                    cal.set(Integer.valueOf(year).intValue(), Integer.valueOf(month).intValue() - 1,
                            Integer.valueOf(day).intValue());
                } else {
                    cal.setTimeInMillis(System.currentTimeMillis());
                }

                Date date = cal.getTime();
                if (date != null) {
                    m_date = new PlainDate(date);
                }
            }
        }


    }

    /**
     * The main methods to update all information from records.
     *
     * @param record String
     * @return int
     * @throws CloneNotSupportedException exception
     */
    public int updateEISHistory(String record) throws CloneNotSupportedException {
        int addToRecordsCount = 0;
        if (!StringUtils.isEmpty(record)) {
            if (record.trim().length() > 5 && record.substring(0, 5).matches("\\d{5}")) {
                String previousRecordId = m_recordId;
                m_recordId = record.substring(0, 3);
                String recordVer = record.substring(3, 5);

                String key = "EFD_" + recordVer + m_recordId;

                String prcId = m_parameters.get(key);

                if (!StringUtils.isEmpty(prcId)) {
                    LinkedList efds = m_efdMap.get(prcId);
                    if (efds == null) {
                        throw new IllegalStateException("key=" + key + " prcId=" + prcId);
                    }
                    if (efds.size() > 0) {
                        ExportFormatDefinition exFormatDefinition = (ExportFormatDefinition) efds.getFirst();
                        ArrayList<ExportFormatField> fields = getExFormatFieldsByEFD(exFormatDefinition);
                        // set school year based on the first data row found for each export format
                        // definition
                        if (StringUtils.isEmpty(previousRecordId) || !previousRecordId.equals(m_recordId)) {
                            initializeSchoolYear(record, fields);
                        }

                        String efrName = "TN " + m_recordId + " " + recordVer + " Uploads for " + m_schoolYear;
                        ExportFormatResult exFormatResult = null;
                        ExportFormatRow exFormatRow = null;
                        if (exFormatDefinition != null) {
                            exFormatResult = getEfrByName(efrName, exFormatDefinition);

                            if (exFormatResult != null) {
                                exFormatRow = addNewExportFormatRow(exFormatDefinition, exFormatResult);
                            } else {
                                exFormatResult = X2BaseBean.newInstance(ExportFormatResult.class,
                                        getBroker().getPersistenceKey());

                                exFormatResult.setOrganization1Oid(m_organization.getOid());
                                exFormatResult.setDefinitionOid(exFormatDefinition.getOid());
                                exFormatResult.setName(efrName);
                                exFormatResult.setRunDate(System.currentTimeMillis());
                                getBroker().saveBeanForced(exFormatResult);
                                LinkedList<ExportFormatResult> list = m_efrMap.get(exFormatResult.getName());
                                if (list == null) {
                                    list = new LinkedList<ExportFormatResult>();
                                    m_efrMap.put(exFormatResult.getName(), list);
                                }
                                list.add(exFormatResult);

                                exFormatRow = addNewExportFormatRow(exFormatDefinition, exFormatResult);

                            }
                        }
                        setRemainingValuesToRow(record, exFormatRow, fields);
                        getBroker().saveBeanForced(exFormatRow);
                        addToRecordsCount = 1;
                    }
                }
            }
        }

        return addToRecordsCount;
    }

    /**
     * Adds new ExportFromatRow based on ExportFormatDefinition ExportFromatResult Oids.
     *
     * @param exFormatDef ExportFormatDefinition
     * @param exFormatResult ExportFormatResult
     * @return ExportFormatRow
     */
    private ExportFormatRow addNewExportFormatRow(ExportFormatDefinition exFormatDef,
                                                  ExportFormatResult exFormatResult) {
        ExportFormatRow exFormatRow = null;
        if (exFormatDef != null && exFormatResult != null && exFormatResult.getOid() != null) {
            exFormatRow = X2BaseBean.newInstance(ExportFormatRow.class,
                    getBroker().getPersistenceKey());

            exFormatRow.setDefinitionOid(exFormatDef.getOid());
            exFormatRow.setResultOid(exFormatResult.getOid());
            exFormatRow.setEntryDate(getNextEntryTimestamp(exFormatResult));
        }

        return exFormatRow;
    }

    /**
     * Returns the StateReportData object used by this helper instance.
     *
     * @return StateReportData
     */
    private X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Returns ExportFormatResult by name from map.
     *
     * @param efrName String
     * @param exFormatDef ExportFormatDefinition
     * @return ExportFormatResult
     */
    private ExportFormatResult getEfrByName(String efrName, ExportFormatDefinition exFormatDef) {
        ExportFormatResult efrToReturn = null;
        Collection<ExportFormatResult> efrs = m_efrMap.get(efrName);

        if (efrs != null) {
            for (ExportFormatResult efr : efrs) {
                if (efr.getDefinitionOid().equals(exFormatDef.getOid())) {
                    efrToReturn = efr;
                    break;
                }
            }
        }

        return efrToReturn;
    }

    /**
     * Look for the all ExportFormatField for the given ExportFormatDefinition.
     *
     * @param exFormatDef ExportFormatDefinition
     * @return Array list
     */
    private ArrayList<ExportFormatField> getExFormatFieldsByEFD(ExportFormatDefinition exFormatDef) {
        ArrayList<ExportFormatField> fieldsList = new ArrayList<ExportFormatField>();
        if (exFormatDef != null) {
            fieldsList.addAll(m_effMap.get(exFormatDef.getOid()));
        }

        Collections.sort(fieldsList, new Comparator<ExportFormatField>() {
            @Override
            public int compare(ExportFormatField eff1, ExportFormatField eff2) {

                return eff1.getPosition() - eff2.getPosition();
            }
        });
        return fieldsList;
    }

    /**
     * Query ExportFromatRow based on ExportFromatResult Oid and entry date. </br>
     * Order by descending by entry date and get first of list.
     *
     * @param efResult ExportFormatResult
     * @return long
     */
    private long getNextEntryTimestamp(ExportFormatResult efResult) {
        Long latest = m_efrLatestTimeStamp.get(efResult.getOid());
        if (latest == null) {
            long today = m_date.getTime();
            Calendar cal = Calendar.getInstance();
            cal.setTime(m_date);
            cal.add(Calendar.DATE, 1);
            long tomorrow = cal.getTimeInMillis();

            X2Criteria efwCriteria = new X2Criteria();
            efwCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, efResult.getOid());
            efwCriteria.addGreaterOrEqualThan(ExportFormatRow.COL_ENTRY_DATE, Long.valueOf(today));
            efwCriteria.addLessThan(ExportFormatRow.COL_ENTRY_DATE, Long.valueOf(tomorrow));

            QueryByCriteria efwQuery = new QueryByCriteria(ExportFormatRow.class, efwCriteria);
            efwQuery.addOrderByDescending(ExportFormatRow.COL_ENTRY_DATE);
            ExportFormatRow efw = (ExportFormatRow) getBroker().getBeanByQuery(efwQuery);
            latest = efw == null ? Long.valueOf(today) : Long.valueOf(efw.getLastModifiedTime());
        }
        Long next = Long.valueOf(latest.longValue() + 1);
        m_efrLatestTimeStamp.put(efResult.getOid(), next);
        return next.longValue();
    }

    /**
     * Initialize school year based on record.
     *
     * @param record String
     * @param fields ArrayList<ExportFormatField>
     */
    private void initializeSchoolYear(String record, ArrayList<ExportFormatField> fields) {
        Collections.sort(fields, new Comparator<ExportFormatField>() {
            @Override
            public int compare(ExportFormatField o1, ExportFormatField o2) {
                return Integer.valueOf(o1.getPosition()).compareTo(Integer.valueOf(o2.getPosition()));
            }
        });
        int schoolYearOffset = 0;
        for (ExportFormatField field : fields) {
            if (field.getName().equalsIgnoreCase("SCHOOL YEAR")) {
                break;
            }
            schoolYearOffset += field.getMaximumLength();
        }
        m_schoolYear = Integer.parseInt(record.substring(schoolYearOffset, schoolYearOffset + 4)) + 1;
    }

    /**
     * Populate map of ExportFormatDefinitions keyed on procedure id.
     */
    private void populateEFDefinitionsMap() {
        ArrayList<String> prcIds = new ArrayList<String>();
        for (Map.Entry<String, String> entry : m_parameters.entrySet()) {
            if (entry.getKey().matches(REGEX_PRC_NAME)) {
                prcIds.add(entry.getValue());
            }
        }
        X2Criteria efdCriteria = new X2Criteria();
        efdCriteria.addIn(ExportFormatDefinition.COL_PROCEDURE_ID, prcIds);
        QueryByCriteria efdQuery = new QueryByCriteria(ExportFormatDefinition.class, efdCriteria);
        m_efdMap = getBroker().getGroupedCollectionByQuery(efdQuery,
                ExportFormatDefinition.COL_PROCEDURE_ID, 500);

    }

    /**
     * Iterate all ExportFromatDefinition map and returns array of oids.
     */
    private void populateEfdOidsCollection() {
        m_efdOids = new ArrayList<String>();
        for (Map.Entry<String, LinkedList<ExportFormatDefinition>> entry : m_efdMap.entrySet()) {
            for (ExportFormatDefinition efd : entry.getValue()) {
                m_efdOids.add(efd.getOid());
            }
        }
    }

    /**
     * Populates ExportFormatField map keyed on ExportFormatDefinition Oids.
     */
    private void populateEFFiledsMap() {
        X2Criteria effCriteria = new X2Criteria();

        if (m_efdOids == null) {
            populateEfdOidsCollection();
        }
        effCriteria.addIn(ExportFormatField.COL_DEFINITION_OID, m_efdOids);
        QueryByCriteria effQuery = new QueryByCriteria(ExportFormatField.class, effCriteria);
        m_effMap = getBroker().getGroupedCollectionByQuery(effQuery, ExportFormatField.COL_DEFINITION_OID, 1024);
    }

    /**
     * Populate ExportFormatResult map keyed on name.
     */
    private void populateEFResultsMap() {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addLikeIgnoreCase(ExportFormatResult.COL_NAME, REGEX_EFR_NAME);
        if (m_efdMap == null) {
            populateEFDefinitionsMap();
        }

        populateEfdOidsCollection();

        efrCriteria.addIn(ExportFormatResult.COL_DEFINITION_OID, m_efdOids);
        QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        m_efrMap = getBroker().getGroupedCollectionByQuery(efrQuery, ExportFormatResult.COL_NAME, 1024);
    }

    /**
     * Copy the remaining values from the input row to the ExportFormatRow.
     *
     * @param record String
     * @param row ExportFormatRow
     * @param fields ArrayList of ExportFormatField
     */
    private void setRemainingValuesToRow(String record, ExportFormatRow row, ArrayList<ExportFormatField> fields) {
        if (row != null && fields != null && !fields.isEmpty()) {
            int startPosition = 0;
            int endPosition = 0;
            for (ExportFormatField field : fields) {
                endPosition += field.getMaximumLength();

                String valueToPut = null;
                if (record.length() >= endPosition) {
                    valueToPut = record.substring(startPosition, endPosition);
                    String saveField = null;
                    if (field.getDataFieldConfig() != null && field.getDataFieldConfig().getDataField() != null) {
                        saveField = field.getDataFieldConfig().getDataField().getJavaName();
                    }

                    if (!StringUtils.isEmpty(saveField) && !StringUtils.isEmpty(valueToPut)) {
                        try {
                            WebUtils.setProperty(row, saveField, valueToPut);
                        } catch (X2BaseException e) {
                            // Ignore: the value was not saved, probably an
                            // invalid field name.
                        }
                    }
                }

                startPosition = endPosition;

            }
        }
    }
}
