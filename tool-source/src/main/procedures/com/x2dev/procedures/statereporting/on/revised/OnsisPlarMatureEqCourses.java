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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The Class OnsisPlarMatureEqCourses.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisPlarMatureEqCourses extends OnsisStateReportData {

    /**
     * The Class OnsisPlarMatureEqCoursesEntity.
     */
    public static class OnsisPlarMatureEqCoursesEntity extends OnsisStateReportEntity {
        private List<String> m_courseCodes = new ArrayList<>();
        private List<String> m_plarTypesToFilter = Arrays.<String>asList(
                OnTranscript.PLAR_TYPE_GRADE_CHA_10,
                OnTranscript.PLAR_TYPE_GRADE_CHA_11_12,
                OnTranscript.PLAR_TYPE_GRADE_EQU_11_12,
                OnTranscript.PLAR_TYPE_GRADE_IND_9_10);

        /**
         * Gets the course code.
         *
         * @return String
         */
        public String getCourseCode() {
            return m_courseCodes.get(getCurrentRow());
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
            OnsisSchoolEntity parentEntity = (OnsisSchoolEntity) getReportData().getParentEntity();
            m_courseCodes = parentEntity.getMaturePlarTranscripts().extract().stream()
                    .filter(trn -> m_plarTypesToFilter.contains(trn.getPlarType()))
                    .map(trn -> trn.getMinistryDefinedCourse())
                    .filter(Objects::nonNull)
                    .distinct()
                    .sorted()
                    .collect(Collectors.toList());

            setRowCount(m_courseCodes.size());
        }
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisPlarMatureEqCoursesEntity.class);
    }

}
