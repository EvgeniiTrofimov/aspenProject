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
import com.x2dev.procedures.statereporting.on.revised.OnsisIle.OnsisIleInfo.OnsisIleProgramCount;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisIleLanguageProgramCount extends OnsisStateReportData {
    /**
     * The Class OnsisIleLanguageProgramCountEntity.
     */
    public static class OnsisIleLanguageProgramCountEntity extends OnsisStateReportEntity {
        private String m_campusNumber;
        private OnsisIleInfo m_info;
        private List<String> m_languageCounts;
        private OnsisIleLanguageProgramCount m_reportData;

        /**
         * Gets the international language.
         *
         * @return String
         */
        public String getInternationalLanguage() {
            String[] values =
                    getInternationalLanguageCountName().split("\\" + OnsisIleInfo.OTHER_LANGUAGE_DESC_DELIMETER);
            return values.length > 0 ? values[0] : null;
        }

        /**
         * Gets the international language count.
         *
         * @return Onsis ile program count
         */
        public OnsisIleProgramCount getInternationalLanguageCount() {
            return m_info.getInternationalLanguageCount(m_campusNumber, getInternationalLanguageCountName());
        }

        /**
         * Gets the international language count name.
         *
         * @return String
         */
        public String getInternationalLanguageCountName() {
            return m_languageCounts.get(this.getCurrentRow());
        }

        /**
         * Gets the language description.
         *
         * @return String
         */
        public String getLanguageDescription() {
            String[] values =
                    getInternationalLanguageCountName().split("\\" + OnsisIleInfo.OTHER_LANGUAGE_DESC_DELIMETER);
            return values.length > 1 ? values[1] : null;
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

            m_reportData = (OnsisIleLanguageProgramCount) data;
            m_info = ((OnsisIleEntity) m_reportData.getParentEntity()).getIleInfo();
            m_campusNumber = ((OnsisIleEntity) m_reportData.getParentEntity()).getCampusNumber();
            m_languageCounts = new ArrayList(m_info.getInternationalLanguages(m_campusNumber));
            this.setRowCount(m_languageCounts.size());
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
        setEntityClass(OnsisIleLanguageProgramCountEntity.class);
    }

}
