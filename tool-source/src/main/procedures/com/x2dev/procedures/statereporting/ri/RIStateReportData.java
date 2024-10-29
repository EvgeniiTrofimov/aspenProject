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

package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import org.apache.commons.lang3.StringUtils;

/**
 * RI custom state report data.
 * This class contains method to retrieve district code for all RI state exports.
 *
 * @author X2 Development Corporation
 */
public class RIStateReportData extends StateReportData {

    /**
     * Returns School.[DOE ADJUSTED DISTRICT]
     * if not null/blank, otherwise
     * organization 1.[RI Reporting District Code]
     *
     * @author Follett Software Company
     */
    protected class RetrieveDistrictCode implements FieldRetriever {
        private static final String CALC_ID = "DISTRICT-CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {

            Object code = null;
            String path = field.getBeanPath();
            String sklOid = (String) getProperty(entity.getBean(), path);

            if (!StringUtils.isEmpty(sklOid)) {
                School school = (SisSchool) data.getBroker().getBeanByOid(SisSchool.class, sklOid);

                if (school != null) {
                    String adjustedDistrCode = (String) school.getFieldValueByBeanPath(m_sklFieldAdjDistr);
                    code = !StringUtils.isEmpty(adjustedDistrCode)
                            ? adjustedDistrCode
                            : school.getOrganization1().getFieldValueByBeanPath(m_districtIdField);
                }
            }
            return code;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_CODE = "RI Reporting District Code";
    protected static final String ALIAS_SKL_ADJ_DISTR = "DOE ADJUSTED DISTRICT";
    protected static final String ALIAS_SKL_ID = "State School Id";

    /**
     * Class members
     */
    protected String m_districtIdField;
    protected String m_sklFieldAdjDistr;
    protected String m_sklIdField;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDistrictCode.CALC_ID, new RetrieveDistrictCode());
        addCalcs(calcs);

    }

    /**
     * Initialize fields by aliases.
     */
    private void initializeFields() {
        m_districtIdField = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_sklFieldAdjDistr = translateAliasToJavaName(ALIAS_SKL_ADJ_DISTR, true);
        m_sklIdField = translateAliasToJavaName(ALIAS_SKL_ID, true);
    }
}
