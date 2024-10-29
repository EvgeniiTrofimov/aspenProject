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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnConductAction;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Map;

public class OnsisStudentIncidentOutcomeNonAcademicProgram extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    public static class OnsisStudentIncidentOutcomeNonAcademicProgramEntity extends OnsisStateReportEntity {
        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            String nonAcademicProgram = ((OnConductAction) bean).getNonAcademicProgram();
            if (StringUtils.isBlank(nonAcademicProgram)) {
                setRowCount(0);
            }
        }

        /**
         * NON_ACADEMIC_PROGRAM/TYPE
         *
         * @return
         */
        public String getNonAcademicProgramType() {
            OnConductAction cndAction = (OnConductAction) getBean();
            String nonAcademicProgram = cndAction.getNonAcademicProgram();
            if (StringUtils.isBlank(nonAcademicProgram)) {
                return null;
            }

            String delimiter = ",";
            String referenceTableOid = cndAction.getNonAcademicProgramField().getReferenceTableOid();
            if (StringUtils.isEmpty(referenceTableOid)) {
                throw new RuntimeException("Reference table must exist for field by alias "
                        + OnConductAction.ALIAS_ACT_NON_ACADEMIC_PROGRAM);
            }

            return StringUtils.convertCollectionToDelimitedString(cndAction.getNonAcademicProgramState(), delimiter);
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
        setBeans(Arrays.asList(getParentEntity().getBean()));
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
        setEntityClass(OnsisStudentIncidentOutcomeNonAcademicProgramEntity.class);
    }
}
