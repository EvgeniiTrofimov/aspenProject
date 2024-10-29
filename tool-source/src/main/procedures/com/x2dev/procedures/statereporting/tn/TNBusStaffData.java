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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolAttribute;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for TN 016 Bus Staff Export.
 * Entity represents whole organization, and each entity row it is a district.
 */
public class TNBusStaffData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * The Class TNBusStaffEntity.
     */
    public static class TNBusStaffEntity extends TNStateReportEntity {
        private Organization m_org;
        private SchoolAttribute m_sklAtt;

        /**
         * Gets the attributes.
         *
         * @return School attribute
         */
        public SchoolAttribute getAttributes() {
            return m_sklAtt;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            return m_org.getName() + " " + m_sklAtt.getContext().getContextId();
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

            m_org = (Organization) bean;
            TNBusStaffData tnData = (TNBusStaffData) data;

            m_sklAtt = tnData.getDistAttributes(m_org.getOid());

            if (m_sklAtt == null) {
                setRowCount(0);
            }
            tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     * Field Retriever for getting values from reference code of the corresponding bus.
     */
    public class RetrieveBusStaffInfo implements FieldRetriever {
        private final String PARAM_BUS_ASSISST = "BUS_ASSIST";
        private final String PARAM_BUS_GARAGE = "BUS_GARAGE";
        private final String PARAM_CON_DRIVERS = "CON_DRIVERS";
        private final String PARAM_EMPL_RIVERS = "EMPL_RIVERS";
        private final String PARAM_FULL_TIME = "FULL_TIME";
        private final String PARAM_MECH_MAINTEN = "MECH_MAINTEN";
        private final String PARAM_REG_DRIVERS = "REG_DRIVERS";
        private final String PARAM_SCHOOLYEAR = "SCHOOLYEAR";
        private final String PARAM_SUB_DRIVERS = "SUB_DRIVERS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            TNBusStaffEntity tnEntity = (TNBusStaffEntity) entity;
            SchoolAttribute sklAtt = tnEntity.getAttributes();
            Object value = null;
            if (PARAM_BUS_ASSISST.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldBusAssist);
            } else if (PARAM_BUS_GARAGE.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldBusGarage);
            } else if (PARAM_CON_DRIVERS.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldContract);
            } else if (PARAM_EMPL_RIVERS.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldEmplBusRivers);
            } else if (PARAM_FULL_TIME.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldFullTimeMechanics);
            } else if (PARAM_MECH_MAINTEN.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldMaintainNonBus);
            } else if (PARAM_REG_DRIVERS.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldRegDrivers);
            } else if (PARAM_SCHOOLYEAR.equals(param)) {
                value = Integer.valueOf(sklAtt.getContext().getSchoolYear() - 1);
            } else if (PARAM_SUB_DRIVERS.equals(param)) {
                value = data.getPropertyAsJavaType(sklAtt, m_fieldSubsDrivers);
            }

            return value;
        }

        /**
         * Integer value.
         *
         * @param data StateReportData
         * @param refCode ReferenceCode
         * @param beanPath String
         * @return Integer
         * @throws X2BaseException exception
         */
        Integer integerValue(StateReportData data, ReferenceCode refCode, String beanPath) throws X2BaseException {
            Integer value = Integer.valueOf(0);
            Object rowValue = data.getPropertyAsJavaType(refCode, beanPath);
            if (rowValue instanceof Number) {
                value = Integer.valueOf(((Number) rowValue).intValue());
            }
            return value;
        }
    }

    // Aliases
    private static final String ALIAS_BUS_ASSISTANTS = "DOE BUS ASSISTANTS SKA";
    private static final String ALIAS_BUS_GARAGE_AND_MAINT = "DOE GARAGE MAINTENANCE SKA";
    private static final String ALIAS_CONTRACT_BUS_DRIVERS = "DOE CONTRACT DRIVER CDL SKA";
    private static final String ALIAS_EMPL_BUS_RIVERS = "DOE EMPLOYED DRIVERS CDL SKA";
    private static final String ALIAS_FULL_TIME_MECHANICS = "DOE FULL TIME MECHANICS SKA";
    private static final String ALIAS_MAINTAIN_NON_BUS_SKL = "DOE MAINTAIN NON BUS SKA";
    private static final String ALIAS_REGULAR_DRIVERS = "DOE REGULAR DRIVERS SKA";
    private static final String ALIAS_SUBSTITUTE_DRIVERS = "DOE SUBSTITUTE DRIVERS SKA";

    protected String m_schoolYear;
    protected String m_selectedContextOid;

    protected String m_fieldBusAssist;
    protected String m_fieldBusGarage;
    protected String m_fieldContract;
    protected String m_fieldEmplBusRivers;
    protected String m_fieldFullTimeMechanics;
    protected String m_fieldMaintainNonBus;
    protected String m_fieldRegDrivers;
    protected String m_fieldSubsDrivers;

    private Map<String, SchoolAttribute> m_orgAttMap;

    /**
     * Gets the dist attributes.
     *
     * @param oid String
     * @return School attribute
     */
    public SchoolAttribute getDistAttributes(String oid) {
        return m_orgAttMap.get(oid);
    }

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_selectedContextOid = (String) getParameter(ToolInput.CONTEXT_OID_PARAM);

        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        // build the query for students to report.
        Criteria criteria = new Criteria();
        QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

        setQuery(query);
        setEntityClass(TNBusStaffEntity.class);

        initializeOrgAttMap();

        // Add any necessary FieldRetrievers and FieldValidators
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("BUS_STF_CALC", new RetrieveBusStaffInfo());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldValidator>();

        super.addValidators(validators);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldBusAssist = translateAliasToJavaName(ALIAS_BUS_ASSISTANTS, true);
        m_fieldBusGarage = translateAliasToJavaName(ALIAS_BUS_GARAGE_AND_MAINT, true);
        m_fieldContract = translateAliasToJavaName(ALIAS_CONTRACT_BUS_DRIVERS, true);
        m_fieldEmplBusRivers = translateAliasToJavaName(ALIAS_EMPL_BUS_RIVERS, true);
        m_fieldFullTimeMechanics = translateAliasToJavaName(ALIAS_FULL_TIME_MECHANICS, true);
        m_fieldMaintainNonBus = translateAliasToJavaName(ALIAS_MAINTAIN_NON_BUS_SKL, true);
        m_fieldRegDrivers = translateAliasToJavaName(ALIAS_REGULAR_DRIVERS, true);
        m_fieldSubsDrivers = translateAliasToJavaName(ALIAS_SUBSTITUTE_DRIVERS, true);
    }

    /**
     * Initialize org att map.
     */
    private void initializeOrgAttMap() {
        X2Criteria attCriteria = new X2Criteria();
        attCriteria.addEqualTo(SchoolAttribute.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_fieldStateSchoolId, "9999");
        attCriteria.addEqualTo(SchoolAttribute.COL_CONTEXT_OID, m_selectedContextOid);
        QueryByCriteria attQuery = new QueryByCriteria(SchoolAttribute.class, attCriteria);

        m_orgAttMap = getBroker().getMapByQuery(attQuery, SchoolAttribute.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ORGANIZATION1 + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, 16);
    }
}
