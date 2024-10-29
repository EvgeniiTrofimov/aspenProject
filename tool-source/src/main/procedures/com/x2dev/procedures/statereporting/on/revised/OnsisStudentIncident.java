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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolIncident.OnsisSchoolIncidentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.Optional;

public class OnsisStudentIncident extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisStudentIncidentEntity extends OnsisStateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public OnsisStudentIncidentEntity() {
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
         * @return
         */
        public OnConductAction getEarliestConductAction() {
            ToolConductIncident cnd = (ToolConductIncident) getBean();
            Collection<ToolConductAction> conductActions = cnd.getConductActions(getBroker());

            Optional<OnConductAction> earliestConductAction = conductActions.stream()
                    .map(act -> (OnConductAction) act)
                    .filter(act -> act.getAppealStartDate() != null)
                    .min(Comparator.comparing(act -> act.getAppealStartDate()));

            if (earliestConductAction.isPresent()) {
                return earliestConductAction.get();
            }

            return null;
        }

        /**
         * APPEAL_DECISION_DATE
         *
         * @return
         */
        public PlainDate getAppealDecisionDate() {
            OnConductAction earliestConductAction = getEarliestConductAction();
            if (earliestConductAction == null) {
                return null;
            }

            return earliestConductAction.getAppealDecisionDate();
        }

        /**
         * APPEAL_START_DATE
         *
         * @return
         */
        public PlainDate getAppealStartDate() {
            OnConductAction earliestConductAction = getEarliestConductAction();
            if (earliestConductAction == null) {
                return null;
            }

            return earliestConductAction.getAppealStartDate();
        }

        /**
         * APPEAL_OUTCOME_TYPE
         *
         * @return
         */
        public String getAppealOutcomeType() {
            OnConductAction earliestConductAction = getEarliestConductAction();
            if (earliestConductAction == null) {
                return null;
            }

            return earliestConductAction.getAppealStatus();
        }

        /**
         * REVISED_NUMBER_OF_SCHOOL_DAYS
         *
         * @return
         */
        public Object getRevisedNumberOfSchoolDays() {
            OnConductAction earliestConductAction = getEarliestConductAction();
            if (earliestConductAction == null) {
                return null;
            }

            return earliestConductAction.getNewSuspensionLength();
        }

        /**
         * Gets the student.
         *
         * @return Tool OnStudent
         */
        public OnStudent getStudent() {
            ToolConductIncident cnd = (ToolConductIncident) getBean();
            return (OnStudent) cnd.getStudent(getBroker());
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
        setBeans(getParentEntity().getConductIncidents());
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
        setEntityClass(OnsisStudentIncidentEntity.class);
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisSchoolIncidentEntity getParentEntity() {
        return (OnsisSchoolIncidentEntity) super.getParentEntity();
    }
}
