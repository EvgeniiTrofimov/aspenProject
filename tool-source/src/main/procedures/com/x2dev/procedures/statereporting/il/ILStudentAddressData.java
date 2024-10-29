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
package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * The Class ILStudentAddressData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class ILStudentAddressData extends RcdtsBasedProcedureData {

    /**
     * The Class RetrieveAddress03.
     */
    protected class RetrieveAddress03 implements FieldRetriever {
        private static final String CALC_ID = "ADDRESS-03";
        private static final String CALC_PARAM_CITY = "CITY";
        private static final String CALC_PARAM_STATE = "STATE";
        private static final String CALC_PARAM_ZIP = "ZIP";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            String returnValue = "";
            SisStudent std = (SisStudent) entity.getBean();
            String addrLine03 = "";
            if (std.getPerson() != null && std.getPerson().getPhysicalAddress() != null
                    && std.getPerson().getPhysicalAddress().getAddressLine03() != null) {
                addrLine03 = std.getPerson().getPhysicalAddress().getAddressLine03();
                if (CALC_PARAM_CITY.equals(param)) {
                    returnValue = getCity(addrLine03);
                } else if (CALC_PARAM_STATE.equals(param)) {
                    returnValue = getState(addrLine03);
                } else if (CALC_PARAM_ZIP.equals(param)) {
                    returnValue = getZip(addrLine03);
                }
            }
            return returnValue;
        }

        /**
         * Gets the city.
         *
         * @param addrLine03 String
         * @return String
         */
        private String getCity(String addrLine03) {
            String city = null;
            if (!StringUtils.isEmpty(addrLine03)) {
                String[] splitteddAddr03 = addrLine03.split(",");
                if (splitteddAddr03.length > 1) {
                    city = splitteddAddr03[0];
                } else {
                    splitteddAddr03 = addrLine03.split(" ");
                    if (splitteddAddr03.length > 1) {
                        city = splitteddAddr03[0];
                    }
                }
            }
            return city;
        }

        /**
         * Gets the zip.
         *
         * @param addrLine03 String
         * @return String
         */
        private String getZip(String addrLine03) {
            String zip = null;
            if (!StringUtils.isEmpty(addrLine03)) {
                String[] splitteddAddr03 = addrLine03.split(", ");
                if (splitteddAddr03.length > 1) {
                    String stateAndZip = splitteddAddr03[1];
                    splitteddAddr03 = stateAndZip.split(" ");
                    if (splitteddAddr03.length > 1) {
                        zip = splitteddAddr03[1];
                    }
                } else {
                    splitteddAddr03 = addrLine03.split(" ");
                    if (splitteddAddr03.length > 2) {
                        zip = splitteddAddr03[2];
                    }
                }
            }
            return zip;
        }

        /**
         * Gets the state.
         *
         * @param addrLine03 String
         * @return String
         */
        private String getState(String addrLine03) {
            String state = null;
            if (!StringUtils.isEmpty(addrLine03)) {
                String[] splitteddAddr03 = addrLine03.split(", ");
                if (splitteddAddr03.length > 1) {
                    String stateAndZip = splitteddAddr03[1];
                    splitteddAddr03 = stateAndZip.split(" ");
                    if (splitteddAddr03.length > 1) {
                        state = splitteddAddr03[0];
                    }
                } else {
                    splitteddAddr03 = addrLine03.split(" ");
                    if (splitteddAddr03.length > 1) {
                        state = splitteddAddr03[1];
                    }
                }
            }
            return state;
        }
    }

    private static final String EXPORT_NAME = "Student Address";
    private static final String PARAM_SPED_ONLY = "spedOnly";
    private static final String PARAM_ENR_STATUS = "enrStatus";
    private static final String PARAM_REQUEST_UPDATE = "requestUpdate";

    private String m_enrStatus;
    private boolean spedOnly = true;
    private Set<String> m_spedCodes = new HashSet();

    /**
     * @see com.x2dev.procedures.statereporting.il.RcdtsBasedProcedureData#initialize()
     */
    @Override
    public void initialize() {
        super.initialize();
        // Initialize special ed codes
        DataDictionaryField dictionaryField =
                getDataDictionaryField(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            for (ReferenceCode code : getReferenceCodes(dictionaryField.getReferenceTableOid()).values()) {
                if ("01".equals(code.getStateCode())) {
                    m_spedCodes.add(code.getCode());
                }
            }
        }

        m_enrStatus = (String) getParameter(PARAM_ENR_STATUS);
        spedOnly = (Boolean) getParameter(PARAM_SPED_ONLY);

        super.initialize();

        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveAddress03.CALC_ID, new RetrieveAddress03());
        addCalcs(calcs);
    }

    /**
     * Adds the selection criteria.
     *
     * @param criteria X2Criteria
     * @param prefix String
     */
    @Override
    protected void addSelectionCriteria(X2Criteria criteria, String prefix) {
        if (spedOnly) {
            X2Criteria activeIepCriteria = new X2Criteria();

            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDate);
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addEmpty(IepData.COL_END_DATE, getBroker().getPersistenceKey());
            endDateCriteria.addOrCriteria(orCriteria);

            activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDate);
            activeIepCriteria.addAndCriteria(endDateCriteria);

            activeIepCriteria.addIn(IepData.COL_STATUS_CODE, Arrays.asList(
                    Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                    Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()),
                    Integer.valueOf(IepData.StatusCode.AMENDED.ordinal())));
            SubQuery stdIepSubQuery = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, activeIepCriteria);

            X2Criteria andStudentCritera = new X2Criteria();
            andStudentCritera.addIn(prefix + SisStudent.COL_SPED_STATUS_CODE, m_spedCodes);

            X2Criteria orStudentCritera = new X2Criteria();
            orStudentCritera.addIn(prefix + X2BaseBean.COL_OID, stdIepSubQuery);

            andStudentCritera.addOrCriteria(orStudentCritera);
            criteria.addAndCriteria(andStudentCritera);
        }

        // check for request (students without state ids) / update (students with state ids)
        int requestUpdateSelection = ((Integer) getParameter(PARAM_REQUEST_UPDATE)).intValue();
        switch (requestUpdateSelection) {
            case 1:
                criteria.addIsNull(prefix + SisStudent.COL_STATE_ID);
                break;
            case 2:
                criteria.addNotNull(prefix + SisStudent.COL_STATE_ID);
                break;
            default:
                break;
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.il.RcdtsBasedProcedureData#getExportName()
     */
    @Override
    protected String getExportName() {
        return EXPORT_NAME;
    }

    /**
     * @see com.x2dev.procedures.statereporting.il.RcdtsBasedProcedureData#isActiveOnly()
     */
    @Override
    protected boolean isActiveOnly() {
        return "Active".equals(m_enrStatus);
    }

    /**
     * @see com.x2dev.procedures.statereporting.il.RcdtsBasedProcedureData#isInactiveOnly()
     */
    @Override
    protected boolean isInactiveOnly() {
        return "Inactive".equals(m_enrStatus);
    }

}
