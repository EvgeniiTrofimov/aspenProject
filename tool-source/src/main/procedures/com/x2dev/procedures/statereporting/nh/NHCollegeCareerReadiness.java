/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">College and Career Readiness export.</blockquote>
 *
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHCollegeCareerReadiness extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHCollegeCareerReadinessEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHCollegeCareerReadinessEntity() {
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
                    ", SASID: " + student.getStateId() +
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

    /*
     * Fields
     */
    protected StudentHistoryHelper m_helper;
    protected boolean m_includeStudentNames = false;
    protected PlainDate m_reportDate;

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
     * Input Definition Parameters
     */
    public static final String GRADE_12 = "12";
    public static final String PARAM_INCLUDE_STUDENT_NAMES = "includeStudentName";
    public static final String PARAM_REPORT_DATE = "reportDate";
    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * Student Name Field
     */
    private static final String STUDENT_NAME = "Name View";

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        if (getParameter(PARAM_INCLUDE_STUDENT_NAMES) != null) {
            m_includeStudentNames = ((Boolean) getParameter(PARAM_INCLUDE_STUDENT_NAMES)).booleanValue();
        }
        /*
         * Add the Student Name field if requested
         */
        if (m_includeStudentNames) {
            getFieldDefinitions().add(0, getName());
        }
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        /*
         * Build query object that will be used to retrieve export students.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_GRADE_LEVEL, GRADE_12);
        // Set the query to be used for student selection.
        setQuery(m_helper.getStudentQuery(true));
        setEntityClass(NHCollegeCareerReadinessEntity.class);
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null,
                null, null, null, null);

        return field;
    }
}
