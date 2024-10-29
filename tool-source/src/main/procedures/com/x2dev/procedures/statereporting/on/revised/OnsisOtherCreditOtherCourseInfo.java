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
import java.util.List;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisOtherCreditOtherCourseInfo extends OnsisStateReportData {
    public static class OtherCreditOtherCourseInfoEntity extends OnsisStateReportEntity {
        private List<String> m_otherCourseInfoTypes = null;

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            /*
             * Primary query returns a Student.
             * Entity rows are one per enrollment span.
             */
            super.intitialize(data, bean);

            OnTranscript transcript = getReportData().getParentEntity().getTranscript();
            m_otherCourseInfoTypes = transcript.getOtherCourseInfoTypes();

            if (m_otherCourseInfoTypes == null
                    || m_otherCourseInfoTypes.size() == 0) {
                setRowCount(0);
            } else {
                setRowCount(m_otherCourseInfoTypes.size());
            }
        }

        /**
         *
         * @return
         */
        public String getOtherCourseInfoType() {
            return m_otherCourseInfoTypes.get(getCurrentRow());
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getData()
         */
        @Override
        public OnsisOtherCreditOtherCourseInfo getReportData() {
            return (OnsisOtherCreditOtherCourseInfo) super.getData();
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
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisOtherCreditEntity getParentEntity() {
        return (OnsisOtherCreditEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OtherCreditOtherCourseInfoEntity.class);
    }
}
