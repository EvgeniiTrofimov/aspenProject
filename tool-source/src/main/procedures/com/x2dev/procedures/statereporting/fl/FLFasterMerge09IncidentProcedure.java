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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_ACTION_DST_NUMBER;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_ACTION_SKL_NUM;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_INCIDENT_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_INCIDENT_TYPE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge09IncidentProcedure.
 */
public class FLFasterMerge09IncidentProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        INCIDENT_TYPE(FIELD_INCIDENT_TYPE, new FieldMerger("all-cnd-IncidentType")),
        //
        INCIDENT_DATE(FIELD_INCIDENT_DATE, new FieldMerger("incidentDate"),
                new ValueAdjusterDate("MMddyyyy")),
        //
        ACTION_DST_NUMBER(FIELD_ACTION_DST_NUMBER, new FieldMerger("all-cnd-FasterDstNum")),
        //
        ACTION_SKL_NUM(FIELD_ACTION_SKL_NUM, new FieldMerger("all-cnd-FasterSklNum"));

        private FieldMerger m_fieldMerger = null;
        private String m_fieldName = null;
        private ValueAdjuster m_valueAdjuster = null;


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger) {
            m_fieldName = fieldName;
            m_fieldMerger = fieldMerger;
        }


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         * @param valueAdjuster ValueAdjuster
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger,
                ValueAdjuster valueAdjuster) {
            this(fieldName, fieldMerger);
            m_valueAdjuster = valueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getFieldName()
         */
        @Override
        public String getFieldName() {
            return m_fieldName;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getMerger()
         */
        @Override
        public FieldMerger getMerger() {
            return m_fieldMerger;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getValueAdjuster()
         */
        @Override
        public ValueAdjuster getValueAdjuster() {
            return m_valueAdjuster;
        }
    }

    private static final String SCHOOL_ASSIGN_TO_NAME = "Archive School";

    private SisSchool m_archiveSchool = null;
    private Map<String, ConductIncident> m_incidents = new HashMap<>();


    /**
     * Gets the archive school.
     *
     * @return Sis school
     */
    private SisSchool getArchiveSchool() {
        if (m_archiveSchool == null) {
            X2Criteria archiveSchoolCriteria = new X2Criteria();
            archiveSchoolCriteria.addEqualTo(SisSchool.COL_NAME, SCHOOL_ASSIGN_TO_NAME);
            BeanQuery archiveQuery = new BeanQuery(SisSchool.class, archiveSchoolCriteria);
            m_archiveSchool = (SisSchool) getBroker().getBeanByQuery(archiveQuery);
            if (m_archiveSchool == null) {
                throw new X2RuntimeException();
            }
        }
        return m_archiveSchool;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getIncidentKey();
        ConductIncident incident = m_incidents.get(key);
        if (incident == null) {
            incident = getNewIncident();
            m_incidents.put(key, incident);
        }

        return incident;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return ConductIncident.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getCurrentPlainRow()
     */
    @Override
    protected String getCurrentPlainRow() {
        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadIncidents();
    }


    /**
     * Gets the incident key.
     *
     * @return String
     */
    private String getIncidentKey() {
        String incidentType = getImportingFieldInfo(FIELD_INCIDENT_TYPE).getValue();
        Object adjustedIncidentDate = getAdjustedValue(FIELD_INCIDENT_DATE);
        String dstNumber = getImportingFieldInfo(FIELD_ACTION_DST_NUMBER).getValue();
        String sklNumber = getImportingFieldInfo(FIELD_ACTION_SKL_NUM).getValue();

        StringBuilder key = new StringBuilder();

        return key.append(incidentType).append(adjustedIncidentDate).append(dstNumber).append(sklNumber).toString();
    }


    /**
     * Gets the incident key.
     *
     * @param incident ConductIncident
     * @return String
     */
    private String getIncidentKey(ConductIncident incident) {
        String incidentType =
                (String) incident
                        .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_INCIDENT_TYPE).getBeanPath());
        String incidentDate = incident.getIncidentDate().toString();
        String dstNumber =
                (String) incident
                        .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_ACTION_DST_NUMBER).getBeanPath());
        String sklNumber =
                (String) incident
                        .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_ACTION_SKL_NUM).getBeanPath());

        StringBuilder key = new StringBuilder();
        return key.append(incidentType).append(incidentDate).append(dstNumber).append(sklNumber).toString();
    }


    /**
     * Gets the new incident.
     *
     * @return Conduct incident
     */
    private ConductIncident getNewIncident() {
        ConductIncident incident = X2BaseBean.newInstance(ConductIncident.class, getBroker().getPersistenceKey());
        incident.setFieldValueByBeanPath(ConductIncident.COL_STUDENT_OID, getStudent().getOid());
        incident.setFieldValueByBeanPath(ConductIncident.COL_SCHOOL_OID, getArchiveSchool().getOid());
        return incident;
    }


    /**
     * Load incidents.
     */
    private void loadIncidents() {
        X2Criteria cndCriteria = new X2Criteria();
        cndCriteria.addEqualTo(ConductIncident.COL_STUDENT_OID, getStudent().getOid());
        cndCriteria.addNotEmpty(getImportingFieldInfo(FIELD_ACTION_DST_NUMBER).getBeanPath(),
                getBroker().getPersistenceKey());
        cndCriteria.addNotEmpty(getImportingFieldInfo(FIELD_ACTION_SKL_NUM).getBeanPath(),
                getBroker().getPersistenceKey());

        QueryByCriteria cndQuery = new QueryByCriteria(ConductIncident.class, cndCriteria);
        Collection<ConductIncident> incidents = getBroker().getCollectionByQuery(cndQuery);
        if (incidents != null) {
            for (ConductIncident incident : incidents) {
                m_incidents.put(getIncidentKey(incident), incident);
            }
        }
    }
}
