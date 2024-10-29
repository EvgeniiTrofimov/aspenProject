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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
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
 * Class to export secondary student information for BC as part of their GDE.
 *
 * @author Follett Software Company
 */
public class CrossEnrolledSchoolInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_NAME = "Student Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM = "Homeroom";
    private static final String FIELD_STD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_SKL_MINISTRY_NUMBER = "Ministry Number";
    private static final String FIELD_SKL_NUMBER = "School Number";
    private static final String FIELD_SKL_NAME = "School Name";

    // Other constants
    private static final int FIELD_COUNT = 8;

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

        QueryByCriteria query = new QueryByCriteria(StudentSchool.class, buildCriteria());
        query.addOrderByAscending(StudentSchool.COL_SCHOOL_OID);
        query.addOrderByAscending(StudentSchool.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_NAME_VIEW);

        QueryIterator secondaries = getBroker().getIteratorByQuery(query);
        try {
            while (secondaries.hasNext()) {
                StudentSchool secondary = (StudentSchool) secondaries.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = (SisStudent) secondary.getStudent();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                    String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                    SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());
                    SisSchool secondarySchool = (SisSchool) secondary.getSchool();

                    /*
                     * Fill grid data list with export information
                     */
                    grid.append();
                    deleteRow = true;
                    grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                    grid.set(FIELD_STD_NAME, student.getNameView());
                    grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_STD_HOMEROOM, homeroom);
                    grid.set(FIELD_SKL_MINISTRY_NUMBER, student.getStateId());
                    grid.set(FIELD_SKL_NUMBER, secondarySchool.getSchoolId());
                    grid.set(FIELD_SKL_NAME, secondarySchool.getName());

                    /*
                     * Pull the student's homeroom teacher
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(school);
                    if (staffMap != null) {
                        Staff staff = staffMap.get(homeroom);
                        if (staff != null) {
                            grid.set(FIELD_STD_TEACHER_NAME, staff.getNameView());
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(secondary.getOid());
                    Student student = secondary.getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
                    }

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
            secondaries.close();
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
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM);
        m_columns.add(FIELD_STD_TEACHER_NAME);
        m_columns.add(FIELD_SKL_MINISTRY_NUMBER);
        m_columns.add(FIELD_SKL_NUMBER);
        m_columns.add(FIELD_SKL_NAME);
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        SubQuery subQ = new SubQuery(School.class, X2BaseBean.COL_OID, getSchoolCriteria());
        criteria.addIn(StudentSchool.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, subQ);
        // criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(StudentSchool.REL_SCHOOL,
        // StudentSchool.COL_SCHOOL_OID));
        criteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        criteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        return criteria;
    }
}
