/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ct;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;

/**
 * Connecticut state report for SASID Testing export.
 * This class implements the data export for CT SASID Testing export.
 *
 * @author X2 Development Corporation
 */
public class PsisStudentAddr extends StateReportData {

    /**
     * The Class PsisEntity.
     */
    public static class PsisStudentAddrEntity extends StateReportEntity {
        PsisStudentAddr m_reportData;
        SisStudent m_student;

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
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;
            m_reportData = (PsisStudentAddr) data;

            boolean overlaps =
                    m_reportData.getStudentHistoryHelper().getStudentEnrollmentSpans(m_student, true).stream()
                            .filter(span -> overlaps(span)).findAny().isPresent();
            if (overlaps && m_reportData.skipActive() || !overlaps && m_reportData.skipInactive()) {
                setRowCount(0);
            }
        }

        /**
         * Overlaps.
         *
         * @param span StudentEnrollmentSpan
         * @return true, if successful
         */
        private boolean overlaps(StudentEnrollmentSpan span) {
            // StartA <= EndB
            boolean leftCondition = m_reportData.getStartDate() == null || span.getLastActiveDate() == null
                    || m_reportData.getStartDate().compareTo(span.getLastActiveDate()) != 1;

            // EndA >= StartB
            boolean rightCondition = m_reportData.getEndDate() == null || span.getFirstActiveDate() == null
                    || m_reportData.getEndDate().compareTo(span.getFirstActiveDate()) != -1;

            return leftCondition && rightCondition;
        }
    }

    private static String INPUT_PARAM_ADDRESS_BEAN_PATH = "addressBeanPath";
    private static String INPUT_PARAM_END_DATE = "endDate";
    private static String INPUT_PARAM_OMIT_ADDRESS = "omitStudentsWithoutAddress";
    private static String INPUT_PARAM_OMIT_STATE_ID = "omitStudentsWithoutStateId";
    private static String INPUT_PARAM_START_DATE = "startDate";
    private static String INPUT_PARAM_STATUS = "status";

    private static String SKIP_ACTIVE = "inactive";
    private static String SKIP_INACTIVE = "active";


    private PlainDate m_endDate;
    private StudentHistoryHelper m_helper;
    private PlainDate m_startDate;
    private String m_status;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);
        m_status = (String) getParameter(INPUT_PARAM_STATUS);
        String addressBeanPath = (String) getParameter(INPUT_PARAM_ADDRESS_BEAN_PATH);
        boolean omitWithoutAddress = false;
        if (getParameter(INPUT_PARAM_OMIT_ADDRESS) != null
                && getParameter(INPUT_PARAM_OMIT_ADDRESS) instanceof Boolean) {
            omitWithoutAddress = ((Boolean) getParameter(INPUT_PARAM_OMIT_ADDRESS)).booleanValue();
        }
        boolean omitWithoutStateId = false;
        if (getParameter(INPUT_PARAM_OMIT_STATE_ID) != null
                && getParameter(INPUT_PARAM_OMIT_STATE_ID) instanceof Boolean) {
            omitWithoutStateId = ((Boolean) getParameter(INPUT_PARAM_OMIT_STATE_ID)).booleanValue();
        }

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        if (omitWithoutAddress && !StringUtils.isEmpty(addressBeanPath)) {
            m_helper.getStudentCriteria().addNotEmpty(addressBeanPath, getBroker().getPersistenceKey());
        }
        if (omitWithoutStateId) {
            m_helper.getStudentCriteria().addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        setQuery(m_helper.getStudentQuery(true));
        setEntityClass(PsisStudentAddrEntity.class);
    }

    /**
     * Gets the end date.
     *
     * @return Plain date
     */
    protected PlainDate getEndDate() {
        return m_endDate;
    }

    /**
     * Gets the student history helper.
     *
     * @return Student history helper
     */
    protected StudentHistoryHelper getStudentHistoryHelper() {
        return m_helper;
    }

    /**
     * Gets the start date.
     *
     * @return Plain date
     */
    protected PlainDate getStartDate() {
        return m_startDate;
    }

    /**
     * Skip active.
     *
     * @return true, if successful
     */
    protected boolean skipActive() {
        return SKIP_ACTIVE.equals(m_status);
    }

    /**
     * Skip inactive.
     *
     * @return true, if successful
     */
    protected boolean skipInactive() {
        return SKIP_INACTIVE.equals(m_status);
    }
}
