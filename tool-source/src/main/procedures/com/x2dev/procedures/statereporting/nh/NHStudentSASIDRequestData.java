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

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">Student Roster export.</blockquote>
 *
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHStudentSASIDRequestData extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHStudentSASIDRequestEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHStudentSASIDRequestEntity() {
            // public no argument constructor for dynamic instantiation.
        }


        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
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
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /*
     * Fields
     */
    protected StudentHistoryHelper m_helper;

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return getParameter(PARAM_REMOVE_HEADER) != null ? !((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()
                : false;
    }

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        /*
         * Build query object that will be used to retrieve export students.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                new PlainDate(OrganizationManager.getTimeZone(getOrganization())));
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        X2Criteria emptySasidAndCriteria = new X2Criteria();
        X2Criteria emptySasidOrCriteria = new X2Criteria();
        emptySasidAndCriteria.addIsNull(SisStudent.COL_STATE_ID);
        emptySasidOrCriteria.addEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        emptySasidAndCriteria.addOrCriteria(emptySasidOrCriteria);
        m_helper.getStudentCriteria().addAndCriteria(emptySasidAndCriteria);

        // Set the query to be used for student selection.
        setQuery(m_helper.getStudentQuery(true));
        setEntityClass(NHStudentSASIDRequestEntity.class);
    }
}
