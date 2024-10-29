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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSalep;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSalepDetail;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisSalep.OnsisSalepEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The Class OnsisSalepComponent.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSalepComponent extends OnsisStateReportData {
    /**
     * The Class OnsisSalepComponentEntity.
     */
    public static class OnsisSalepComponentEntity extends OnsisStateReportEntity {
        private List<OnStudentSalepDetail> m_details;
        private OnsisSalepEntity m_onsisSalepEntity;

        /**
         * Gets the details.
         *
         * @return Onsis student salep detail
         */
        public OnStudentSalepDetail getDetails() {
            return m_details.get(getCurrentRow());
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_onsisSalepEntity = (OnsisSalepEntity) ((OnsisSalepComponent) data).getParentEntity();
            OnStudentSalep studentProgram = m_onsisSalepEntity.getStudentProgram();

            m_details = studentProgram.getProgramDetails(getBroker()).stream()
                    .filter(pgd -> {
                        if (StringUtils.isBlank(pgd.getComponent())) {
                            return false;
                        }
                        if (m_onsisSalepEntity.getDateRange().getEnd() != null) {
                            return pgd.getDateRange().isOverlap(m_onsisSalepEntity.getDateRange());
                        }
                        return pgd.getDateRange().contains(getGlobalData().getEndDate());
                    })
                    .sorted(new Comparator<OnStudentSalepDetail>() {
                        @Override
                        public int compare(OnStudentSalepDetail o1, OnStudentSalepDetail o2) {
                            return o1.getOid().compareTo(o2.getOid());
                        }
                    })
                    .collect(Collectors.toList());

            setRowCount(m_details.size());
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
        setBeans((Arrays.asList((OnsisStudent) getParentEntity().getBean())));
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
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSalepComponentEntity.class);
    }
}
