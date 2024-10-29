/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This report is used to report the count of compliance, non-compliance, conditional, waived and
 * any 4 other exempt reasons for all
 * health immunization definitions. These are grouped by gradelevel.
 *
 * @author Follett Software Company
 */
public class StudentImmunizationCompliance extends ReportJavaSourceNet {
    /**
     * Report Fields and Params
     */

    private static final String FIELD_COUNT_COMPLIANCE = "complianceCount";
    private static final String FIELD_COUNT_CONDITIONAL = "conditionalCount";
    private static final String FIELD_COUNT_NON_COMPLIANCE = "nonComplianceCount";
    private static final String FIELD_COUNT_REASON = "reason";
    private static final String FIELD_COUNT_WAIVED = "waivedCount";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_SERIES_ID = "seriesId";
    private static final String PARAM_REASON = "REASON";
    private static final String PARAM_SCHOOL_YEAR = "SCHOOL_YEAR";

    /**
     * Other Constants and Parameters
     */
    private static final String ALIAS_SERIES_EXEMPT_REASON = "SERIES EXEMPT REASON";
    private static final String DATA_FIELD_GRADE = "Grade Level";
    private static final String DATA_FIELD_SCHOOL = "School Name";
    private static final String EMPTY_STRING = "";
    private static final int MAX_EXEMPT_REASONS = 4;
    private static final String PROCEDURE_ID = "procedureId";
    private static final String STATUS_COMPLIANT = "COMPLIANT";
    private static final String STATUS_CONDITIONAL = "CONDITIONAL";
    private static final String STATUS_NON_COMPLIANT = "NOT-COMPLIANT";
    private static final String STATUS_WAIVED = "WAIVED";
    private static final String WAIVED_STATUS_SEPARATOR = "-";

    protected String[] m_immunizationExemptReasons;
    protected StateReportData m_reportData;

    private Map<String, Integer> m_dataFieldIndexesMap = new HashMap<String, Integer>();
    private ReportDataGrid m_dataGridImmunization;
    private Map<String, Map<String, ComplianceCountBySeries>> m_immunizationComplianceCountByGradeLvl =
            new TreeMap<String, Map<String, ComplianceCountBySeries>>();
    private String m_immunizationSeriesExemptReasons;
    private Collection<StateReportValidationError> m_initErrors = null;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet.gatherData() throws Exception
     */
    @Override
    protected Object gatherData() throws Exception {
        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in Student Immunization Report";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }
            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

        /*
         * Build the entire report grid.
         */
        if (m_reportData != null && m_reportData.open()) {
            try {
                StateReportEntity entity = null;

                // Eexempt reasons
                m_immunizationSeriesExemptReasons =
                        m_reportData.translateAliasToJavaName(ALIAS_SERIES_EXEMPT_REASON, false);
                if (m_immunizationSeriesExemptReasons != null) {
                    DataDictionaryField exemptReasons = m_reportData
                            .getDataDictionaryField(HealthImmunizationSeries.class, m_immunizationSeriesExemptReasons);
                    if (exemptReasons != null) {
                        Criteria refCodeCriteria =
                                ReferenceManager.getCodesCriteria(exemptReasons.getReferenceTableOid(),
                                        getOwnableCriteria(),
                                        true,
                                        true,
                                        false,
                                        getBroker().getPersistenceKey());

                        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
                        refCodeQuery.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);
                        refCodeQuery.addOrderByAscending(ReferenceCode.COL_CODE);

                        Map<String, ReferenceCode> immunizationExemptReasons =
                                getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 8);
                        if (immunizationExemptReasons != null) {
                            m_immunizationExemptReasons = new String[immunizationExemptReasons.size()];
                            (immunizationExemptReasons.keySet()).toArray(m_immunizationExemptReasons);
                        }
                    }
                }

                int fieldCount = m_reportData.getFieldCount();
                initializeFieldPositions(fieldCount);
                while ((entity = m_reportData.next()) != null) {
                    String[] entityFieldValues = entity.getFieldValues();
                    String gradeLevel = entityFieldValues[m_dataFieldIndexesMap.get(DATA_FIELD_GRADE).intValue()];

                    for (int currentFieldIndex = (m_dataFieldIndexesMap.get(DATA_FIELD_SCHOOL).intValue()
                            + 1); currentFieldIndex < fieldCount; currentFieldIndex++) {
                        FieldDefinition fDef = m_reportData.getFieldDefinition(currentFieldIndex);
                        ComplianceCountBySeries complianceCounter = getComplianceCounter(fDef.getFieldId(), gradeLevel);
                        calculateComplianceCount(complianceCounter, entityFieldValues[currentFieldIndex]);
                    }
                }
            } finally {
                m_reportData.close();
            }
        }

        buildReportGrids();
        addParameter(PARAM_SCHOOL_YEAR, Integer.toString(getCurrentContext().getSchoolYear()));

        return m_dataGridImmunization;
    }

    /**
     * This method builds the entire report grid.
     */
    private void buildReportGrids() {
        m_dataGridImmunization = new ReportDataGrid();
        Set<String> sortedGradeKeys = m_immunizationComplianceCountByGradeLvl.keySet();

        /*
         * Retrieving series for each grade level
         */
        for (String gradeKey : sortedGradeKeys) {

            Map<String, ComplianceCountBySeries> gradeLevelMap = m_immunizationComplianceCountByGradeLvl.get(gradeKey);
            Set<String> sortedSeriesIdKeys = gradeLevelMap.keySet();

            // Flag for building the dynamic header.
            boolean headerBuilt = false;
            for (String seriesId : sortedSeriesIdKeys) {
                ComplianceCountBySeries compliance = gradeLevelMap.get(seriesId);
                m_dataGridImmunization.append();
                m_dataGridImmunization.set(FIELD_GRADE_LEVEL, gradeKey);
                m_dataGridImmunization.set(FIELD_SERIES_ID, seriesId);
                m_dataGridImmunization.set(FIELD_COUNT_COMPLIANCE, Integer.toString(compliance.getCompliance()));
                m_dataGridImmunization.set(FIELD_COUNT_NON_COMPLIANCE, Integer.toString(compliance.getNonCompliance()));
                m_dataGridImmunization.set(FIELD_COUNT_CONDITIONAL, Integer.toString(compliance.getConditional()));
                m_dataGridImmunization.set(FIELD_COUNT_WAIVED, Integer.toString(compliance.getWaived()));

                Map<String, Integer> waivedReasonMap = compliance.getWaivedReasonMap();
                Set<String> reasons = waivedReasonMap.keySet();
                if (!headerBuilt) { // Building the dynamic header
                    int reasonNumber = 1;
                    for (String reason : reasons) {
                        if (reasonNumber > MAX_EXEMPT_REASONS) {
                            break;
                        }
                        addParameter(PARAM_REASON + Integer.toString(reasonNumber), reason);
                        reasonNumber++;
                    }

                    while (reasonNumber <= MAX_EXEMPT_REASONS) // 4 is selected because we display
                                                               // count for only 4 exempt reasons
                    {
                        addParameter(PARAM_REASON + Integer.toString(reasonNumber), EMPTY_STRING);
                        reasonNumber++;
                    }
                    headerBuilt = true;
                }
                int reasonNumber = 1;
                for (String reason : reasons) {
                    if (reasonNumber > MAX_EXEMPT_REASONS) {
                        break;
                    }
                    m_dataGridImmunization.set(FIELD_COUNT_REASON + reasonNumber,
                            waivedReasonMap.get(reason).toString());
                    reasonNumber++;
                }

                while (reasonNumber <= MAX_EXEMPT_REASONS) // 4 is selected because we display count
                                                           // for only 4 exempt reasons
                {
                    m_dataGridImmunization.set(FIELD_COUNT_REASON + reasonNumber, EMPTY_STRING);
                    reasonNumber++;
                }
            }
        }

        m_dataGridImmunization.beforeTop();
    }

    /**
     * This method calculates the count of compliance, non-compliance, waived, conditional, and any
     * other exempt reasons.
     *
     * @param complianceCounter ComplianceCountBySeries
     * @param fieldValue String
     */
    private void calculateComplianceCount(ComplianceCountBySeries complianceCounter, String fieldValue) {
        if (STATUS_CONDITIONAL.equalsIgnoreCase(fieldValue)) {
            complianceCounter.setConditional(complianceCounter.getConditional() + 1);
        } else if (STATUS_COMPLIANT.equals(fieldValue)) {
            complianceCounter.setCompliance(complianceCounter.getCompliance() + 1);
        } else if (STATUS_NON_COMPLIANT.equals(fieldValue)) {
            complianceCounter.setNonCompliance(complianceCounter.getNonCompliance() + 1);
        } else // Status is waived of some kind
        {
            if (STATUS_WAIVED.equals(fieldValue)) {
                complianceCounter.setWaived(complianceCounter.getWaived() + 1);
            } else {
                String[] waived = fieldValue.split(WAIVED_STATUS_SEPARATOR);
                if (waived != null && waived.length == 2) {
                    String exemptReason = waived[1];
                    Map<String, Integer> waivedReasonMap = complianceCounter.getWaivedReasonMap();
                    if (waivedReasonMap.keySet().contains(exemptReason)) {
                        waivedReasonMap.put(exemptReason,
                                Integer.valueOf((waivedReasonMap.get(exemptReason).intValue()) + 1));
                    }
                }
            }
        }
    }

    /**
     * This method creates the ComplianceCountBySeries objects and sets it up in the map to
     * categorize it by gradelevel.
     *
     * @param definition String
     * @param gradeLevel String
     * @return ComplianteCountBySeries
     */
    private ComplianceCountBySeries getComplianceCounter(String definition, String gradeLevel) {
        Map<String, ComplianceCountBySeries> immunizationComplianceCountMap = null;

        if (!m_immunizationComplianceCountByGradeLvl.containsKey(gradeLevel)) {
            immunizationComplianceCountMap = new TreeMap<String, ComplianceCountBySeries>();
            immunizationComplianceCountMap.put(definition, new ComplianceCountBySeries());
            m_immunizationComplianceCountByGradeLvl.put(gradeLevel, immunizationComplianceCountMap);
        } else {
            immunizationComplianceCountMap = m_immunizationComplianceCountByGradeLvl.get(gradeLevel);
            if (!immunizationComplianceCountMap.containsKey(definition)) {
                immunizationComplianceCountMap.put(definition, new ComplianceCountBySeries());
            }
        }

        immunizationComplianceCountMap = m_immunizationComplianceCountByGradeLvl.get(gradeLevel);

        return immunizationComplianceCountMap.get(definition);
    }

    /**
     * This method loads a map with the field positions for the desired fields.
     *
     * @param fieldCount int
     */
    private void initializeFieldPositions(int fieldCount) {
        for (int currentFieldIndex = 0; currentFieldIndex < fieldCount; currentFieldIndex++) {
            String fieldId = m_reportData.getFieldDefinition(currentFieldIndex).getFieldId();
            if (DATA_FIELD_GRADE.equals(fieldId)) {
                m_dataFieldIndexesMap.put(DATA_FIELD_GRADE, Integer.valueOf(currentFieldIndex));
            } else if (DATA_FIELD_SCHOOL.equals(fieldId)) {
                m_dataFieldIndexesMap.put(DATA_FIELD_SCHOOL, Integer.valueOf(currentFieldIndex));
                break;
            }
        }
    }

    /**
     * This class keeps track of compliance count by series.
     */
    protected class ComplianceCountBySeries {
        int m_compliance;
        int m_conditional;
        int m_nonCompliance;
        int m_waived;
        Map<String, Integer> m_waivedReasonMap = new HashMap<String, Integer>();

        /**
         * Default constructor.
         */
        public ComplianceCountBySeries() {
            if (m_immunizationExemptReasons != null) {
                int length = m_immunizationExemptReasons.length;

                for (int index = 0; (index < length) && (index < 4); index++) {
                    m_waivedReasonMap.put(m_immunizationExemptReasons[index], Integer.valueOf(0));
                }
            }
        }

        /**
         * Returns the complaince count.
         * 
         * @return int
         */
        public int getCompliance() {
            return m_compliance;
        }

        /**
         * Returns the conditional count.
         * 
         * @return int
         */
        public int getConditional() {
            return m_conditional;
        }

        /**
         * Returns the non-compliance count.
         * 
         * @return int
         */
        public int getNonCompliance() {
            return m_nonCompliance;
        }

        /**
         * Gets the waived count.
         * 
         * @return int
         */
        public int getWaived() {
            return m_waived;
        }

        /**
         * Returns the waived reasons map.
         * 
         * @return Map<String, Integer>
         */
        public Map<String, Integer> getWaivedReasonMap() {
            return m_waivedReasonMap;
        }

        /**
         * Sets the compliance count.
         *
         * @param compliance void
         */
        public void setCompliance(int compliance) {
            m_compliance = compliance;
        }

        /**
         * Sets the conditional count.
         *
         * @param conditional void
         */
        public void setConditional(int conditional) {
            m_conditional = conditional;
        }

        /**
         * Sets the non-compliance count.
         *
         * @param nonCompliance void
         */
        public void setNonCompliance(int nonCompliance) {
            m_nonCompliance = nonCompliance;
        }

        /**
         * Sets the waived count.
         *
         * @param waived void
         */
        public void setWaived(int waived) {
            m_waived = waived;
        }

        /**
         * Sets the waived reasons map.
         *
         * @param waivedReasonMap Map<String,Integer>
         */
        public void setWaivedReasonMap(Map<String, Integer> waivedReasonMap) {
            m_waivedReasonMap = waivedReasonMap;
        }
    }
}
