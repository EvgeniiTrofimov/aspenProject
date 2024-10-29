/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: Student Address export.
 * This class implements the data export for MD Student Address export.
 *
 * @author X2 Development Corporation
 */
public class MDStudentAddressData extends MDStudentReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD Student Address export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class MDStudentAddressEntity extends MDStudentReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MDStudentAddressEntity() {
            // public no argument constructor for dynamic instantiation.
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

        /**
         * @see com.x2dev.procedures.statereporting.md.MDStudentReportData.MDStudentReportEntity#getSchool(com.x2dev.procedures.statereporting.md.MDStudentReportData)
         */
        @Override
        public SisSchool getSchool(MDStudentReportData data) {
            return ((SisStudent) getBean()).getSchool();
        }
    }

    /**
     * Retriever for ID Record.
     *
     */
    protected class RetrieveRecordIdNumber implements FieldRetriever {

        protected static final String CALC_ID = "ADDR-ID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field) throws X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MDStudentAddressData mdData = (MDStudentAddressData) data;
            int currentId = mdData.m_recordId.intValue();
            currentId += 1;
            mdData.m_recordId = Integer.valueOf(currentId);
            return String.format("%06d", currentId);
        }
    }

    /**
     * Class members.
     */
    protected StudentHistoryHelper m_helper;
    protected Integer m_recordId = Integer.valueOf(0);

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        // Load initialization data
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        super.initialize();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(MDStudentAddressEntity.class);
            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveRecordIdNumber.CALC_ID, new RetrieveRecordIdNumber());
            super.addCalcs(calcs);

        }
    }

    /**
     * Sets the parameters.
     *
     * @param parameters Map<String,Object>
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#setParameters(java.util.Map)
     */
    @Override
    public void setParameters(Map<String, Object> parameters) {
        super.setParameters(parameters);
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    @Override
    protected Criteria getStudentCriteria() {
        return m_helper.getStudentCriteria();
    }

    /**
     * Gets the student query.
     *
     * @return Query by criteria
     */
    @Override
    protected QueryByCriteria getStudentQuery() {
        return m_helper.getStudentQuery(false);
    }

}
