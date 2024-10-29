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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolScheduleClass;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolClass.OnsisSchoolClassEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisClassSegment extends OnsisStateReportData {
    /**
     * The Class OnsisClassSegmentEntity.
     */
    public static class OnsisClassSegmentEntity extends OnsisStateReportEntity {
        private OnsisClassSegment m_reportData;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnSection section = (OnSection) getBean();
            return section.getCourseView();
        }

        public int getTotalPeriodsNumber() {
            OnSection section = (OnSection) getBean();
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            int numPeriods = section.getTotalNumberPeriods(null, null, getBroker(), debugOutput);
            if (debugOutput != null) {
                debugOutput.append("getTotalPeriodsNumber() is : " + numPeriods + "\n");
                m_reportData.log(debugOutput.toString());
            }
            return numPeriods;
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

            m_reportData = (OnsisClassSegment) data;

            String key = getKey();
            if (m_reportData.m_includedSegments.contains(key)) {
                setRowCount(0);
            } else {
                m_reportData.m_includedSegments.add(key);
            }
        }

        /**
         * Gets the key.
         *
         * @return String
         */
        private String getKey() {
            StringBuilder key = new StringBuilder();
            key.append("key:");
            for (int i = 1; i < 6; ++i) {
                key.append(getFieldValue(i));
            }
            return key.toString();
        }
    }

    protected Set<String> m_includedSegments;

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans(com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity)
     */
    @Override
    public void buildBeans() throws X2BaseException {
        List<OnSection> allSections = new ArrayList();
        m_includedSegments = new HashSet();

        ToolBean parentEntityBean = getParentEntity().getBean();
        if (parentEntityBean instanceof ToolScheduleClass) {
            ToolScheduleClass cls = (ToolScheduleClass) parentEntityBean;
            allSections.addAll(getSections(cls.getOid()));
        } else if (parentEntityBean instanceof OnSection) {
            allSections.add((OnSection) parentEntityBean);
        } else {
            throw new RuntimeException("Unexpected parentEntityBean " + parentEntityBean);
        }

        List<OnSection> sectionsWithEnrolments = allSections.stream()
                // .filter(section -> getHistoryHelper().areStudentsEnrolled(section))
                .filter(mst -> mst.isStudentEnrolled(getBroker(), getGlobalData().getDateRange(), true))
                .sorted((mst0, mst1) -> {
                    // sub-sort on SchoolCourse for when CreditValue is the only difference
                    int compareTo = StringUtils.unNullify(mst0.getMinistryDefinedCourse())
                            .compareTo(StringUtils.unNullify(mst1.getMinistryDefinedCourse()));
                    if (compareTo == 0) {
                        String schoolCourseOid0 = StringUtils.emptyIfNull(mst0.getSchoolCourseOid());
                        String schoolCourseOid1 = StringUtils.emptyIfNull(mst1.getSchoolCourseOid());
                        compareTo = schoolCourseOid0.compareTo(schoolCourseOid1);
                    }
                    return compareTo;
                })
                .collect(Collectors.toList());
        setBeans(sectionsWithEnrolments);
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
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getMatchOnEmptyKeyValue(java.lang.String)
     */
    @Override
    public boolean getMatchOnEmptyKeyValue(String keyField) {
        return !(CsvField.LOCAL_COURSE_CODE.toString().equals(keyField)
                || CsvField.MINISTRY_DFND_CRS.toString().equals(keyField));
    }

    /**
     * Gets the sections.
     *
     * @param classOid String
     * @return List
     */
    public List<OnSection> getSections(String classOid) {
        OnsisSchoolClass parentData = (OnsisSchoolClass) getParentReportData();
        return parentData.getSections(classOid);
    }

    /**
     * Initialize data source from parent.
     *
     * @param parentData OnsisStateReportData
     * @param parentEntity OnsisStateReportEntity
     * @throws X2BaseException exception
     * @see com.follett.fsc.aspensif.framework.OnsisStateReportData#initializeDataSourceFromParent(com.follett.fsc.aspensif.framework.OnsisStateReportData,
     *      com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity,
     *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
     */
    @Override
    public void initializeDataSourceFromParent(OnsisStateReportData parentData,
                                               OnsisStateReportEntity parentEntity)
            throws X2BaseException {

        super.initializeDataSourceFromParent(parentData, parentEntity);

        if (parentEntity instanceof OnsisSchoolClassEntity) {
            // Nothing to do here. super already saves parentEntity.
        } else {
            throw new RuntimeException("Unsupported parent export " + parentEntity.getClass().getSimpleName());
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateAndAppendDelete(com.x2dev.procedures.statereporting.on.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord,
     *      java.util.List, java.util.List, org.w3c.dom.Element)
     */
    @Override
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        /*
         * 2020-11-11 Never send DELETE for a Class Segment
         */
        return null;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisClassSegmentEntity.class);
    }

}
