/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductAction;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductIncident;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnConductAction;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.stream.Collectors;

public class OnsisStudentIncidentOutcome extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisStudentOutcomeEntity extends OnsisStateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public OnsisStudentOutcomeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * APPEAL_FLAG
         *
         * If an ACT record with not empty all-act-AppealStartDate exists value is 'T'.
         * Otherwise 'F'
         *
         * @return
         */
        public Boolean getAppealFlag() {
            OnConductAction act = (OnConductAction) getBean();
            Date appealStartDate = act.getAppealStartDate();

            return appealStartDate != null ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        ToolConductIncident cnd = (ToolConductIncident) getParentEntity().getBean();
        Collection<ToolConductAction> conductActions =
                cnd.getConductActions(getBroker()).stream()
                        .filter(act -> !StringUtils
                                .isBlank(((OnConductAction) act).getActionCodeState()))
                        .collect(Collectors.toList());

        setBeans(conductActions);
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisStudentOutcomeEntity.class);
    }
}
