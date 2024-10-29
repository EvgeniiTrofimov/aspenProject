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

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MedicaidRosterData.
 */
public class MedicaidRosterData extends StateReportData {
    public static final String ALIAS_DOE_11 = "DOE 11";
    public static final String ALIAS_DOE_12 = "DOE 12";
    public static final String ALIAS_DOE_32 = "DOE 32";
    public static final String ALIAS_DOE_34 = "DOE 34";

    protected DataDictionary m_districtDictionary;

    private Map<String, String> m_reportReasons;
    private Map<String, String> m_enrollStatuses;

    /**
     * The Class MedicaidRosterEntity.
     */
    public static class MedicaidRosterEntity extends StateReportEntity {
        MedicaidRosterData m_data;

        /**
         * Instantiates a new MA entity.
         */
        public MedicaidRosterEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (MedicaidRosterData) data;
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Class to retrieve assessment date.
     */
    protected class RetrieveDOE implements FieldRetriever {

        protected static final String RETRIEVER_ID = "MA-SIF-SPED-PLACEMNT";

        /**
         * Instantiates a new MA retriever.
         */
        public RetrieveDOE() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String returnValue = "";

            switch ((String) field.getParameter()) {
                case ALIAS_DOE_11:
                    returnValue = m_reportReasons.get(student.getFieldValueByAlias(ALIAS_DOE_11));
                    break;
                case ALIAS_DOE_12:
                    returnValue = m_enrollStatuses.get(student.getFieldValueByAlias(ALIAS_DOE_12));
                    break;
                case ALIAS_DOE_32:
                    returnValue = (String) student.getFieldValueByAlias(ALIAS_DOE_32);
                    break;
                case ALIAS_DOE_34:
                    returnValue = (String) student.getFieldValueByAlias(ALIAS_DOE_34);
            }
            return returnValue;
        }
    }

    protected static final String PARAM_INCLUDE_STUDENTS = "includeStudents";

    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    private QueryByCriteria m_criteria;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);

        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        m_reportReasons = loadReferenceTable(ALIAS_DOE_11);
        m_enrollStatuses = loadReferenceTable(ALIAS_DOE_12);

        setEntityClass(MedicaidRosterEntity.class);

        m_criteria = m_helper.getStudentQuery(true);

        if (getParameter(PARAM_INCLUDE_STUDENTS) != null
                && ((Boolean) getParameter(PARAM_INCLUDE_STUDENTS)).booleanValue()) {
            m_criteria.getCriteria().addNotNull(SisStudent.COL_STATE_ID);
        }

        // Set the query to be used for student selection.
        setQuery(m_criteria);

        // Build a map of calculations/retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDOE.RETRIEVER_ID, new RetrieveDOE());
        super.addCalcs(calcs);
    }

    /**
     * Loads the codes from the reference table to memory
     */
    private Map<String, String> loadReferenceTable(String fieldAlias) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(fieldAlias);
        Map<String, String> refMap = new HashMap<String, String>();

        if (dictionaryField.hasReferenceTable()) {
            String referenceTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            QueryIterator refCodes = getBroker().getIteratorByQuery(query);
            try {
                while (refCodes.hasNext()) {
                    ReferenceCode refCode = (ReferenceCode) refCodes.next();
                    refMap.put(refCode.getCode(), refCode.getStateCode());
                }
            } finally {
                refCodes.close();
            }
        }

        return refMap;
    }
}
