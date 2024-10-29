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
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSchoolStudent extends OnsisStateReportData {

    /**
     * The Class OnsisSchoolStudentEntity.
     */
    public static class OnsisSchoolStudentEntity extends OnsisStateReportEntity {
        private OnsisCsvDataRecord m_record;

        /**
         * Gets the csv data record.
         *
         * @return Onsis csv data record
         */
        public OnsisCsvDataRecord getCsvDataRecord() {
            return m_record;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();

            return name;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnsisStudent getStudent() {
            return (OnsisStudent) getBean();
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisSchoolStudent schoolData = (OnsisSchoolStudent) data;
            OnsisExtractRecords matcher =
                    schoolData.getGlobalData().getExtractHelper()
                            .getMatcherByExtractType(OnsisStateReportData.EXTRACT_TYPE_ENR_ROOT);

            // Initialize members
            m_record = matcher == null ? null
                    : matcher.findRecord(
                            Arrays.asList(
                                    OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                                    OnsisExtractHelper.CsvField.OEN.toString()),
                            Arrays.asList(
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString()),
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.OEN.toString())));
        }

    }

    @Override
    public void buildBeans() throws X2BaseException {
        List<OnsisStudent> studentList = new ArrayList(ToolBean.getCachedToolBeans(OnsisStudent.class));

        String debugStudentOid = getDebugStudentOid();
        if (!StringUtils.isEmpty(debugStudentOid)) {
            studentList = studentList.stream()
                    .filter(std -> std.getOid() != null
                            && debugStudentOid.equals(std.getOid()))
                    .collect(Collectors.toList());
        }

        // sort by OEN then OID
        Collections.sort(studentList, new Comparator<OnsisStudent>() {
            @Override
            public int compare(OnsisStudent o1, OnsisStudent o2) {
                int compareTo = OnsisHelper.compareTo(o1.getOen(), o2.getOen());
                if (compareTo == 0) {
                    compareTo = OnsisHelper.compareTo(o1.getOid(), o2.getOid());
                }
                return compareTo;
            }
        });

        setBeans(studentList);
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSchoolStudentEntity.class);
    }

}
