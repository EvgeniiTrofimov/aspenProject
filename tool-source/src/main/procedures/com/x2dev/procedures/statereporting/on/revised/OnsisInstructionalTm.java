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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStaffPosition;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolEducatorAssignment.OnsisSchoolEducatorAssignmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisInstructionalTm extends OnsisStateReportData {
    public static class OnsisInstructionalTmEntity extends OnsisStateReportEntity {
        private static final String DELIMITER_MULTIVALUE = "\\s*,\\s*";

        private List<String> m_instrTimeValues = new ArrayList<>();
        private Set<String> m_previousTimeValues = new HashSet<>();

        public String getAction() {
            String type = getType();
            return m_previousTimeValues.contains(type) ? "DELETE" : "ADD";
        }

        /**
         * Gets the type.
         *
         * @return String
         */
        public String getType() {
            return m_instrTimeValues.get(getCurrentRow());
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisInstructionalTm reportData = (OnsisInstructionalTm) data;
            OnStaffPosition sfp = (OnStaffPosition) bean;
            OnsisSchoolEducatorAssignmentEntity parentEntity =
                    (OnsisSchoolEducatorAssignmentEntity) reportData.getParentEntity();
            OnsisCsvDataRecord record = parentEntity.getEducatorAssignment().getCsvRecord();
            if (record != null) {
                String csvValues = record.getSingleFieldValue(OnsisExtractHelper.CsvField.INSTRUCTIONAL_TM_TYPE);
                if (!StringUtils.isEmpty(csvValues)) {
                    for (String value : csvValues.split(DELIMITER_MULTIVALUE)) {
                        m_previousTimeValues.add(value);
                    }
                }
            }

            Set<String> currentTimeValues = new HashSet<>();
            String teachingType =
                    reportData.getParentEntity().getFieldValue(OnsisSchoolEducatorAssignmentEntity.FIELD_TEACHING_TYPE);
            if (!OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_NOT_APPLICABLE.equals(teachingType)) { // MSED074
                currentTimeValues.addAll(sfp.getInstructionalTimeCodes());
            }
            // do not delete CSV records if the record is end-dated
            boolean isDeleteCSV = StringUtils.isEmpty(reportData.getParentEntity()
                    .getFieldValue(OnsisSchoolEducatorAssignmentEntity.FIELD_ASSIGNMENT_END_DATE))
                    && !OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_NOT_APPLICABLE.equals(teachingType)
                    && !OnsisSchoolEducatorAssignmentEntity.TEACHING_TYPE_TEACHING.equals(teachingType);
            for (String value : m_previousTimeValues) {
                if (currentTimeValues.contains(value)) {
                    currentTimeValues.remove(value);
                } else {
                    if (isDeleteCSV) {
                        m_instrTimeValues.add(value);
                    }
                }
            }
            m_instrTimeValues.addAll(currentTimeValues);

            Collections.sort(m_instrTimeValues);
            setRowCount(m_instrTimeValues.size());
        }
    }

    @Override
    public void buildBeans() throws X2BaseException {
        OnsisSchoolEducatorAssignmentEntity parentEntity =
                (OnsisSchoolEducatorAssignmentEntity) getParentEntity();
        OnStaffPosition sfp = parentEntity.getEducatorAssignment().getStaffPosition();
        setBeans(sfp == null ? Collections.EMPTY_LIST : Arrays.asList(sfp));
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisInstructionalTmEntity.class);
    }
}
