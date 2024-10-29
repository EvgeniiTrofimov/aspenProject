/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports student's homeroom information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class HomeroomAndStaffAssignmentsExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_SKL_NUMBER = "School Number";
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM_CODE = "Home Form Code";
    private static final String FIELD_STF_ID = "Teacher ID";
    private static final String FIELD_STF_NAME_VIEW = "Teacher Name";
    private static final String FIELD_SKL_CURRENT_YEAR = "Current Year";
    private static final String FIELD_HMR_SEMESTER = "Semester";
    private static final String FIELD_HMR_ID = "Home Room ID";
    private static final String FIELD_ENR_ID = "Enrolment ID";
    private static final String FIELD_HMR_ENROLMENT_ID = "Home Room Enrolment ID";

    // Other constants
    private static final int FIELD_COUNT = 13;

    private List<String> m_columns;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, buildCriteria());
        query.addOrderByAscending(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                boolean deleteRow = false;
                try {
                    Map<String, Staff> homeroomToStaffMap = getHomeroomToStaffMap(student.getSchool());

                    if (homeroomToStaffMap != null && student.getPerson() != null) {
                        grid.append();
                        deleteRow = true;

                        // Fill grid data list with export information
                        grid.set(FIELD_SKL_NUMBER, student.getSchool().getSchoolId());
                        grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                        grid.set(FIELD_STD_LAST_NAME, student.getPerson().getLastName());
                        grid.set(FIELD_STD_FIRST_NAME, student.getPerson().getFirstName());
                        grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                        grid.set(FIELD_STD_HOMEROOM_CODE, student.getHomeroom());

                        Staff staff = homeroomToStaffMap.get(student.getHomeroom());
                        if (staff != null) {
                            grid.set(FIELD_STF_ID, staff.getLocalId());
                            grid.set(FIELD_STF_NAME_VIEW, staff.getNameView());
                        }

                        grid.set(FIELD_SKL_CURRENT_YEAR, String.valueOf(getCurrentContext().getSchoolYear()));
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(student.getOid());
                    strBldr.append("] for the Student with Local ID: [");
                    strBldr.append(student.getLocalId());
                    strBldr.append("].");

                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            students.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        // Set columns
        super.initialize();
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SKL_NUMBER);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM_CODE);
        m_columns.add(FIELD_STF_ID);
        m_columns.add(FIELD_STF_NAME_VIEW);
        m_columns.add(FIELD_SKL_CURRENT_YEAR);
        m_columns.add(FIELD_HMR_SEMESTER);
        m_columns.add(FIELD_HMR_ID);
        m_columns.add(FIELD_ENR_ID);
        m_columns.add(FIELD_HMR_ENROLMENT_ID);

    }

    /**
     * Builds export criteria for students.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(SisStudent.REL_SCHOOL, SisStudent.COL_SCHOOL_OID));
        criteria.addNotEmpty(SisStudent.COL_HOMEROOM, getBroker().getPersistenceKey());

        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        return criteria;
    }
}
