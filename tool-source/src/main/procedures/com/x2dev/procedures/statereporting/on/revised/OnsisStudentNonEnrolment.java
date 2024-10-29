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
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolStudent.OnsisSchoolStudentEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Map;
import org.w3c.dom.Element;

/**
 * The Class OnsisStudentNonEnrolment.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisStudentNonEnrolment extends OnsisStateReportData {
    /**
     * The Class OnsisStudentEnrollmentEntity.
     */
    public static class OnsisStudentNonEnrolmentEntity extends OnsisStudentEntity {

        /**
         * Should remove.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if successful
         */
        public static boolean shouldRemove(Element entityElement, Element parentElement) {
            /*
             * Only export this Non-Enrolment if it has at least one Diploma, SHSM or SPCE
             */
            boolean hasContent = false;
            hasContent |= OnsisStateReportData.getChildElement(OnsisStudentSchoolEnrollment.ELEMENT_DIPLOMA,
                    entityElement) != null;
            hasContent |= OnsisStateReportData.getChildElement(OnsisStudentSchoolEnrollment.ELEMENT_SHSM_PROGRAM,
                    entityElement) != null;
            hasContent |= OnsisStateReportData.getChildElement(OnsisStudentSchoolEnrollment.ELEMENT_SPCE,
                    entityElement) != null;

            return !hasContent;
        }

        private OnsisStudentNonEnrolment m_reportData;

        /**
         * Gets the grade type.
         *
         * @return String
         */
        public String getGradeType() {
            String gradeLevel = getStudent().getGradeLevel();
            if (!StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = getStudent().getGradeLevelState();
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                GradesHelper gradesHelper = getGlobalData().getGradesHelper();
                int yog = getStudent().getYog();
                gradeLevel = gradesHelper.getGradeLevel(yog);
            }
            return gradeLevel;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis student non enrolment
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisStudentNonEnrolment getReportData() {
            return (OnsisStudentNonEnrolment) super.getReportData();
        }

        /**
         * Gets the scholarship flag.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEntity#getScholarshipFlag()
         */
        @Override
        public String getScholarshipFlag() {
            String value = super.getScholarshipFlag();
            return value == null ? "F" : value;
        }

        /**
         * Intitialize.
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

            m_reportData = (OnsisStudentNonEnrolment) data;
            OnsisSchoolStudentEntity parentEntity = ((OnsisSchoolStudentEntity) m_reportData.getParentEntity());
            OnsisCsvDataRecord record = parentEntity.getCsvDataRecord();
            if (record != null) {
                setRowCount(0);
            }
        }

        /**
         * Checks if is special education.
         *
         * @return true, if is special education
         */
        public boolean isSpecialEducation() {
            return isSpecialEducation(null);
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.jdom.Element,
         *      org.jdom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            return shouldRemove(entityElement, parentElement);
        }
    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans(com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity)
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();
        calcs.put(OnsisStateReportData.OnsisRetrieverAction.CALC_ID, new OnsisStateReportData.OnsisRetrieverAction());
        return calcs;
    }

    /**
     * Generate deletes.
     *
     * @param parentElement Element
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#generateDeletes(org.w3c.dom.Element)
     */
    @Override
    protected void generateDeletes(Element parentElement) {
        // Non-enrolments should never generate a Delete
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisStudentNonEnrolmentEntity.class);
    }
}
