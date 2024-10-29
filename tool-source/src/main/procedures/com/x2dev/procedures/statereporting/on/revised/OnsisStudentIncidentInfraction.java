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

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductIncident;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductOffense;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

public class OnsisStudentIncidentInfraction extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisStudentInfractionEntity extends OnsisStateReportEntity {
        private final static String INFRACTION_TYPE_OTHER = "OTH";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public OnsisStudentInfractionEntity() {
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
         * INFRACTION_TYPE = cnoIncident#STATE begins with E the E, otherwise S
         *
         * @return
         */
        public String getSuspensionExpulsionFlag() {
            ToolConductOffense cno = (ToolConductOffense) getBean();
            String stateCode = cno.getIncidentCodeState();
            if (StringUtils.isBlank(stateCode)) {
                return null;
            }

            String firstCharacter = stateCode.substring(0, 1);
            if ("E".equals(firstCharacter)) {
                return firstCharacter;
            }

            return "S";
        }

        /**
         *
         * @return
         */
        public String getDescription() {
            ToolConductOffense cno = (ToolConductOffense) getBean();

            String description = null;
            /*
             * 2021-05-06 ONLY export Description if Incident is OTH
             */
            String stateCode = cno.getIncidentCodeState();
            if (!INFRACTION_TYPE_OTHER.equals(stateCode)) {
                return description;
            }

            ReferenceCode referenceCode = cno.getIncidentRefCode();
            if (referenceCode != null) {
                return referenceCode.getDescription();
            }

            return description;
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
        Collection<ToolConductOffense> conductOffenses = cnd.getConductOffenses(getBroker()).stream()
                .filter(cno -> !StringUtils.isBlank(cno.getIncidentCode()))
                .collect(Collectors.toList());

        setBeans(conductOffenses);
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
        setEntityClass(OnsisStudentInfractionEntity.class);
    }
}
