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
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentClassEnrolment.OnsisStudentClassEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Class OnsisSscOtherCourseInfo.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSscOtherCourseInfo extends OnsisStateReportData {

    public static final String ELEMENT_OTHER_COURSE_INFO = "OTHER_COURSE_INFO";

    private static final String CHAR_COMMA = ",";

    /**
     * The Class SscOtherCourseInfoEntity.
     */
    public static class SscOtherCourseInfoEntity extends OnsisStateReportEntity {
        private List<String> m_otherCourseInfoTypes = null;

        /**
         * Gets the credit recovery.
         *
         * @return String
         */
        public String getCreditRecovery() {
            String oci = getOtherCourseInfoType();
            if (oci != null && oci.contains("1")) {
                return "1";
            }
            return null;
        }

        /**
         * Gets the ministry dev type.
         *
         * @return String
         */
        public String getMinistryDevType() {
            String oci = getOtherCourseInfoType();
            if (oci != null && oci.contains("2")) {
                return "2";
            }
            return null;
        }

        /**
         * Gets the other course info type.
         *
         * @return String
         */
        public String getOtherCourseInfoType() {
            if (m_otherCourseInfoTypes != null
                    && m_otherCourseInfoTypes.size() > getCurrentRow()) {
                return m_otherCourseInfoTypes.get(getCurrentRow());
            }

            return null;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis ssc other course info
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisSscOtherCourseInfo getReportData() {
            return (OnsisSscOtherCourseInfo) super.getReportData();
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
            /*
             * Primary query returns a Student.
             * Entity rows are one per enrollment span.
             */
            super.intitialize(data, bean);

            List<String> courseInfoTypes = doGetOtherCourseInfoTypes();

            if (courseInfoTypes == null || courseInfoTypes.size() == 0) {
                setRowCount(0);
            } else {
                m_otherCourseInfoTypes = courseInfoTypes.stream()
                        .flatMap(value -> Stream.of(value.split(CHAR_COMMA)))
                        .map(code -> code.trim())
                        .collect(Collectors.toList());
                setRowCount(m_otherCourseInfoTypes.size());
            }
        }

        /**
         * Gets the other course info.
         *
         * @return String
         */
        private List<String> doGetOtherCourseInfoTypes() {
            StudentScheduleSpan span = getReportData().getParentEntity().getSpan();
            if (span == null) {
                return null;
            }
            OnTranscript transcript = (OnTranscript) span.getTranscript();
            if (transcript != null) {
                return transcript.getOtherCourseInfoTypes();
            }
            if (span.getSection() != null) {
                return ((OnSection) span.getSection()).getOtherCourseInfoTypes();
            }
            return null;
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
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Gets the match on empty key value.
     *
     * @param keyField String
     * @return boolean
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getMatchOnEmptyKeyValue(java.lang.String)
     */
    @Override
    public boolean getMatchOnEmptyKeyValue(String keyField) {
        return false;
    }

    /**
     * Gets the parent entity.
     *
     * @return Onsis student class enrollment entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisStudentClassEnrollmentEntity getParentEntity() {
        return (OnsisStudentClassEnrollmentEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(SscOtherCourseInfoEntity.class);
    }
}
