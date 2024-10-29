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
import com.x2dev.procedures.statereporting.on.revised.OnsisIle.OnsisIleEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisIle.OnsisIleInfo;
import com.x2dev.procedures.statereporting.on.revised.OnsisIle.OnsisIleInfo.OnsisIleTimeOfDayCount;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisIleLanguageProgramCountTod extends OnsisStateReportData {

    /**
     * The Class OnsisIleLanguageProgramCountTodEntity.
     */
    public static class OnsisIleLanguageProgramCountTodEntity extends OnsisStateReportEntity {
        private String m_campusNumber;
        private OnsisIleInfo m_info;
        private OnsisIleLanguageProgramCountTod m_reportData;
        private List<String> m_timeOfDayCounts;

        /**
         * Gets the time of day.
         *
         * @return String
         */
        public String getTimeOfDay() {
            return m_timeOfDayCounts.get(this.getCurrentRow());
        }

        /**
         * Gets the time of day count.
         *
         * @return Onsis ile time of day count
         */
        public OnsisIleTimeOfDayCount getTimeOfDayCount() {
            return m_info.getTimeOfDayCount(m_campusNumber, getTimeOfDay());
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

            m_reportData = (OnsisIleLanguageProgramCountTod) data;
            m_info = ((OnsisIleEntity) m_reportData.getParentEntity()).getIleInfo();
            m_campusNumber = ((OnsisIleEntity) m_reportData.getParentEntity()).getCampusNumber();
            m_timeOfDayCounts = new ArrayList(m_info.getTimeOfDayCounts(m_campusNumber));
            this.setRowCount(m_timeOfDayCounts.size());
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
        setEntityClass(OnsisIleLanguageProgramCountTodEntity.class);
    }

}
