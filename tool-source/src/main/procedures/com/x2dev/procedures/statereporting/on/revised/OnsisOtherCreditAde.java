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
import com.x2dev.procedures.statereporting.on.revised.OnsisOtherCredit.OnsisOtherCreditEntity;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisOtherCreditAde extends OnsisStateReportData {

    /**
     * The Class OnsisOtherCreditAdeEntity.
     */
    public static class OnsisOtherCreditAdeEntity extends OnsisStateReportEntity {
        OnsisOtherCreditAde m_reportData;
        private OnTranscript m_transcript;

        /**
         * Gets the course offering type.
         *
         * @return String
         */
        public String getCourseOfferingType() {
            return m_transcript.getCourseOfferingType();
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_reportData = (OnsisOtherCreditAde) data;
            OnsisOtherCreditEntity parentEntity = (OnsisOtherCreditEntity) m_reportData.getParentEntity();
            m_transcript = parentEntity.getTranscript();
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
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisOtherCreditAdeEntity.class);
    }
}
