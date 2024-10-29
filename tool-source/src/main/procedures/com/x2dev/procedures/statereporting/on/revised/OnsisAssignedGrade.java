/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
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
import com.x2dev.procedures.statereporting.on.revised.OnsisAssignedSubject.OnsisAssignedSubjectEntity;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The Class OnsisAssignedGrade.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisAssignedGrade extends OnsisStateReportData {

    /**
     * The Class OnsisAssignedGradeEntity.
     */
    public static class OnsisAssignedGradeEntity extends OnsisStateReportEntity {
        private List<String> m_grades;

        /**
         * Gets the grade type.
         *
         * @return String
         */
        public String getGradeType() {
            return m_grades.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisAssignedSubjectEntity parentSubjectEntity =
                    (OnsisAssignedSubjectEntity) getReportData().getParentEntity();

            m_grades = parentSubjectEntity.getGradesPrimaryDuringSubmission()
                    .stream().collect(Collectors.toList());
            setRowCount(m_grades.size());
        }
    }

    public static String ELEMENT_ASSIGNED_GRADE = "ASSIGNED_GRADE";

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
     * Gets the calcs.
     *
     * @return Map
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
        setEntityClass(OnsisAssignedGradeEntity.class);
    }
}
