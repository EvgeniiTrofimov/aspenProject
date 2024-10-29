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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolData.OnsisSchoolEntity;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;

/**
 * The Class OnsisPlarMatureReport.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisPlarMatureReport extends OnsisStateReportData {

    /**
     * The Class OnsisPlarMatureEntity.
     */
    public static class OnsisPlarMatureEntity extends OnsisStateReportEntity {
        private OnsisPlarMatureReport m_reportData;

        /**
         * Gets the grade cha flag.
         *
         * @return Boolean
         */
        public Boolean getGradeChaFlag() {
            return !m_reportData.isEmpty(OnTranscript.PLAR_TYPE_GRADE_CHA_11_12);
        }

        /**
         * Gets the grade equ flag.
         *
         * @return Boolean
         */
        public Boolean getGradeEquFlag() {
            return !m_reportData.isEmpty(OnTranscript.PLAR_TYPE_GRADE_EQU_11_12);
        }

        /**
         * Gets the grade ind flag.
         *
         * @return Boolean
         */
        public Boolean getGradeIndFlag() {
            return !m_reportData.isEmpty(OnTranscript.PLAR_TYPE_GRADE_IND_9_10);
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean ToolBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_reportData = (OnsisPlarMatureReport) data;
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
        if (!isEmpty(OnTranscript.PLAR_TYPE_GRADE_CHA_11_12) ||
                !isEmpty(OnTranscript.PLAR_TYPE_GRADE_EQU_11_12) ||
                !isEmpty(OnTranscript.PLAR_TYPE_GRADE_IND_9_10)) {
            setBeans(Arrays.asList(getParentEntity().getBean()));
        } else {
            setBeans(null);
        }
    }

    /**
     * Checks if is empty.
     *
     * @param plarType String
     * @return true, if is empty
     */
    public boolean isEmpty(String plarType) {
        OnsisSchoolEntity parentEntity = (OnsisSchoolEntity) getParentEntity();
        return parentEntity.getMaturePlarTranscripts().getGroup(OnTranscript.FIELD_PLAR_TYPE, plarType).isEmpty();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisPlarMatureEntity.class);
    }

}
