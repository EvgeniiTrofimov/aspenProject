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


package com.x2dev.procedures.destiny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export format procedure to export Staff information to Destiny.
 *
 * @author X2 Development Corporation
 */
public class DestinyStaffPatron extends StateReportData {
    private static final String SCHOOL_PARAMETER = "SCHOOL";
    private static final String BEANOID_PARAMETER = "BEANOID";

    private static final String USER1_CALC_ID = "USER1";
    private static final String USER2_CALC_ID = "USER2";
    private static final String USER3_CALC_ID = "USER3";
    private static final String USER4_CALC_ID = "USER4";
    private static final String USER5_CALC_ID = "USER5";
    private static final String USERNAME_CALC_ID = "USERNAME";

    /**
     * The Class DestinyStaffPatronEntity.
     */
    public static class DestinyStaffPatronEntity extends StateReportEntity {
        DestinyStaffPatron m_staffPatron = null;

        /**
         * Instantiates a new destiny staff patron entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public DestinyStaffPatronEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_staffPatron = (DestinyStaffPatron) data;
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

    protected String m_activeStaffCode;
    protected String m_beanOid;
    protected Map<String, String> m_usernames;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Class getBeanClass() {
        return SisStaff.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "StaffPatron";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Returns the user defined field 1.
     */
    protected class RetrieveUser1 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            /*
             * Define custom code in any of the User1-5 field retrievers to retrieve special
             * values related to the student.
             */

            return value;
        }
    }

    /**
     * Returns the user defined field 2.
     */
    protected class RetrieveUser2 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            return value;
        }
    }

    /**
     * Returns the user defined field 3.
     */
    protected class RetrieveUser3 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            return value;
        }
    }

    /**
     * Returns the user defined field 4.
     */
    protected class RetrieveUser4 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            return value;
        }
    }

    /**
     * Returns the user defined field 5.
     */
    protected class RetrieveUser5 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            return value;
        }
    }

    /**
     * Returns the user name.
     */
    protected class RetrieveUsername implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStaff staff = (SisStaff) entity.getBean();

            String value = m_usernames.get(staff.getPersonOid());

            return value;
        }
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @param school School
     * @return Criteria
     */
    private Criteria getStaffCriteria(School school) {
        /*
         * Currently two conditions. Active staff who are in the school requested.
         */
        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addEqualTo(Staff.COL_STATUS, m_activeStaffCode);
        if (school != null) {
            primaryCriteria.addEqualTo(Staff.COL_SCHOOL_OID, school.getOid());
        }
        if (!StringUtils.isEmpty(m_beanOid)) {
            primaryCriteria.addEqualTo(X2BaseBean.COL_OID, m_beanOid);
        }
        List<FieldDefinition> fieldDefinitions = getFieldDefinitions();

        for (FieldDefinition field : fieldDefinitions) {
            if ("DistrictId".equals(field.getFieldId())) {
                primaryCriteria.addNotNull(field.getBeanPath());
                break;
            }
        }
        return primaryCriteria;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void initialize() {

        // Load initialization data
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            School school = null;

            if (getParameter(SCHOOL_PARAMETER) != null) {
                school = (School) getBroker().getBeanByOid(School.class, (String) getParameter(SCHOOL_PARAMETER));
            }

            if (getParameter(BEANOID_PARAMETER) != null) {
                m_beanOid = (String) getParameter(BEANOID_PARAMETER);
            }
            /*
             * Build query object that will be used to retrieve export students.
             */
            QueryByCriteria staffQuery = new QueryByCriteria(Staff.class, getStaffCriteria(school));

            loadUsernames(getStaffCriteria(school));

            // Set the query to be used for selection.
            setQuery(staffQuery);
            setEntityClass(DestinyStaffPatronEntity.class);

            // Add any retrievers or validators.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(USER1_CALC_ID, new RetrieveUser1());
            calcs.put(USER2_CALC_ID, new RetrieveUser2());
            calcs.put(USER3_CALC_ID, new RetrieveUser3());
            calcs.put(USER4_CALC_ID, new RetrieveUser4());
            calcs.put(USER5_CALC_ID, new RetrieveUser5());
            calcs.put(USERNAME_CALC_ID, new RetrieveUsername());
            super.addCalcs(calcs);
        }
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeStaffCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
    }

    /**
     * Loads a map of login names for students keyed to the person OID.
     *
     * @param staffCriteria Criteria
     */
    private void loadUsernames(Criteria staffCriteria) {
        m_usernames = new HashMap<String, String>(2048);

        Criteria criteria = new Criteria();
        criteria.addIn(SisUser.COL_PERSON_OID, new SubQuery(SisStaff.class, SisStaff.COL_PERSON_OID, staffCriteria));

        QueryByCriteria query = new QueryByCriteria(SisUser.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisUser user = (SisUser) iterator.next();

                m_usernames.put(user.getPersonOid(), user.getLoginName());
            }
        } finally {
            iterator.close();
        }
    }
}