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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Bus data migration procedure.
 */
public class BusDataMigrationProcedure extends ProcedureJavaSource {

    private static final String DDX_ID_RCD = "REF-BUS";
    private static final String DDX_ID_UDD = "TN-UDD-BUS";

    private static final int INITIAL_MAP_SIZE = 1024;

    private String[] m_aliases = {
            TNBusData.ALIAS_ADVERTISING,
            TNBusData.ALIAS_AIR_CONDITIONING,
            TNBusData.ALIAS_BUS_DISABLED,
            TNBusData.ALIAS_BUS_DESCRIPTION,
            TNBusData.ALIAS_BUS_TYPE,
            TNBusData.ALIAS_BUS_SERVICE_BEGIN_DATE,
            TNBusData.ALIAS_BUS_SERVICE_END_DATE,
            TNBusData.ALIAS_CHAIR_LIFTS,
            TNBusData.ALIAS_COMMUNICATION,
            TNBusData.ALIAS_DAILY_MILES_AM,
            TNBusData.ALIAS_FATAL_OFF_BOARD,
            TNBusData.ALIAS_FATAL_ON_BOARD,
            TNBusData.ALIAS_FUEL,
            TNBusData.ALIAS_GPS,
            TNBusData.ALIAS_HOSPITALIZED,
            TNBusData.ALIAS_INSPECTED,
            TNBusData.ALIAS_PD_ACCIDENTS,
            TNBusData.ALIAS_PI_ACCIDENTS,
            TNBusData.ALIAS_PRIVATE,
            TNBusData.ALIAS_RESTRAINTS,
            TNBusData.ALIAS_SPECIALLY_EQUIPPED,
            TNBusData.ALIAS_SURVEILLANCE,
            TNBusData.ALIAS_TOTAL_ACCIDENT,
            TNBusData.ALIAS_TOTAL_TREATED,
            TNBusData.ALIAS_YEAR_BEGAN,
            TNBusData.ALIAS_ESCS,
            TNBusData.ALIAS_SAFETY_COMPLAINTS,
            TNBusData.ALIAS_BUS_NUM
    };

    private ExtendedDataDictionary m_ddxRcd;
    private ExtendedDataDictionary m_ddxUdd;

    private Map<String, String> m_fieldsRcd;
    private Map<String, String> m_fieldsUdd;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("Import Bus data from RCD-based to UDD-based extension table");

        loadDictionaries();

        Map<String, UserDefinedTableD> busesUdd = getBusesUdd();

        for (ReferenceCode busRcd : getBusesRcd()) {
            String busNumber = busRcd.getStateCode();
            if (!StringUtils.isEmpty(busNumber)) {
                UserDefinedTableD busUdd = getUddBusByNumber(busesUdd, busNumber);
                busUdd.setFieldValueByBeanPath(m_fieldsUdd.get(TNBusData.ALIAS_BUS_DESCRIPTION),
                        StringUtils.truncate(busRcd.getDescription(), 50));
                busUdd.setFieldValueByBeanPath(m_fieldsUdd.get(TNBusData.ALIAS_BUS_DISABLED),
                        busRcd.getDisabledIndicator() ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE);
                copyFields(busRcd, busUdd);
                logErrors(busNumber, getBroker().saveBean(busUdd));
            }
        }
        logMessage("Import complete");
    }

    /**
     * Copy fields.
     *
     * @param beanRcd X2BaseBean
     * @param beanUdd X2BaseBean
     */
    private void copyFields(X2BaseBean beanRcd, X2BaseBean beanUdd) {
        for (String alias : m_aliases) {
            String beanPathRcd = m_fieldsRcd.get(alias);
            if (beanPathRcd != null) {
                Object value = beanRcd.getFieldValueByBeanPath(beanPathRcd);
                String beanPathUdd = m_fieldsUdd.get(alias);
                beanUdd.setFieldValueByBeanPath(beanPathUdd, value);
            }
        }
    }

    /**
     * Load buses from Reference Codes.
     *
     * @return Collection
     */
    private Collection<ReferenceCode> getBusesRcd() {
        return m_ddxRcd.getReferenceCodes(getBroker());
    }

    /**
     * Load existing buses from UDD table.
     *
     * @return Map
     */
    private Map<String, UserDefinedTableD> getBusesUdd() {

        X2Criteria uddCriteria = new X2Criteria();

        uddCriteria.addEqualTo(UserDefinedTableD.COL_EXTENDED_DATA_DICTIONARY_OID, m_ddxUdd.getOid());

        QueryByCriteria uddQuery = new QueryByCriteria(UserDefinedTableD.class, uddCriteria);

        getBroker().deleteByQuery(uddQuery);

        String busNumberBeanPath = m_fieldsUdd.get(TNBusData.ALIAS_BUS_NUM);
        return getBroker().getMapByQuery(uddQuery, busNumberBeanPath, INITIAL_MAP_SIZE);
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        X2Criteria ddxCriteria = new X2Criteria();

        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        return getBroker().getBeanByQuery(ddxQuery);
    }

    /**
     * Gets the udd bus by number.
     *
     * @param buses Map<String,UserDefinedTableD>
     * @param busNumber String
     * @return User defined table D
     */
    private UserDefinedTableD getUddBusByNumber(Map<String, UserDefinedTableD> buses, String busNumber) {
        UserDefinedTableD bus = buses.get(busNumber);
        if (bus == null) {
            bus = X2BaseBean.newInstance(UserDefinedTableD.class, getBroker().getPersistenceKey());
            bus.setOrganization1Oid(OrganizationManager.ROOT_ORGANIZATION);
            bus.setExtendedDataDictionaryOid(m_ddxUdd.getOid());
            bus.setFieldValueByBeanPath(m_fieldsUdd.get(TNBusData.ALIAS_BUS_NUM), busNumber);
            buses.put(busNumber, bus);
        }
        return bus;
    }

    /**
     * Load dictionaries.
     */
    private void loadDictionaries() {
        m_ddxRcd = getExtendedDataDictionaryById(DDX_ID_RCD);
        m_fieldsRcd = loadFieldsByDdx(m_ddxRcd);

        m_ddxUdd = getExtendedDataDictionaryById(DDX_ID_UDD);
        m_fieldsUdd = loadFieldsByDdx(m_ddxUdd);
    }

    /**
     * Load fields by ddx.
     *
     * @param ddx ExtendedDataDictionary
     * @return Map
     */
    private Map<String, String> loadFieldsByDdx(ExtendedDataDictionary ddx) {
        Map fields = new HashMap<>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                ddx, getBroker().getPersistenceKey());

        for (String alias : m_aliases) {
            DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(alias);
            if (dataField != null) {
                fields.put(alias, dataField.getJavaName());
            }
        }
        return fields;
    }

    /**
     * Log errors.
     *
     * @param busNumber String
     * @param errors List<ValidationError>
     */
    private void logErrors(String busNumber, List<ValidationError> errors) {
        if (errors != null && !errors.isEmpty()) {
            logMessage("Failed to save record for bus number: " + busNumber);
            for (ValidationError err : errors) {
                logMessage(err.toString());
            }
        }
    }
}
