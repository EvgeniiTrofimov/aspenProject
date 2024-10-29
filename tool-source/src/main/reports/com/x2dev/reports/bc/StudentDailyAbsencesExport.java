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
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports daily attendance information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StudentDailyAbsencesExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    /*
     * Input parameters
     */
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";

    /*
     * Grid fields
     */
    private static final String FIELD_SCHOOL_ID = "School Number";
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_ABSENCE_DATE = "Absence Date";
    private static final String FIELD_REASON_CODE_AM = "Reason Code Am";
    private static final String FIELD_SUB_ALLOCATION_CODE_AM = "Sub Allocation Code Am";
    private static final String FIELD_AUTHORIZED_AM = "Authorized Am";
    private static final String FIELD_REASON_CODE_PM = "Reason Code Pm";
    private static final String FIELD_SUB_ALLOCATION_CODE_PM = "Sub Allocation Code Pm";
    private static final String FIELD_AUTHORIZED_PM = "Authorized Pm";
    private static final String FIELD_ABSENCE_CODE_AM = "Absence Code Am";
    private static final String FIELD_ABSENCE_CODE_PM = "Absence Code Pm";

    /*
     * Other constants
     */
    private static final String AUTORIZED_TRUE = "Y";
    private static final String AUTORIZED_FALSE = "N";
    private static final int FIELD_COUNT = 16;

    private List<String> m_columns;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy");
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, buildCriteria());
        QueryIterator attendances = getBroker().getIteratorByQuery(query);
        try {
            int progressCount = 0;
            while (attendances.hasNext()) {
                StudentAttendance attendance = (StudentAttendance) attendances.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = attendance.getStudent();

                    if (++progressCount % 10000 == 0) {
                        AppGlobals.getLog()
                                .info("GDE StudentDailyAbsences processed " + progressCount + " attendance records");
                    }

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                    String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                    SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

                    grid.append();
                    deleteRow = true;

                    /*
                     * Set student fields
                     */
                    grid.set(FIELD_SCHOOL_ID, attendance.getSchool().getSchoolId());
                    grid.set(FIELD_STUDENT_ID, student.getLocalId());
                    grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_HOMEROOM, homeroom);

                    if (student.getPerson() != null) {
                        grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                        grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                    }

                    /*
                     * Set homeroom teacher name
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(school);
                    if (staffMap != null) {
                        Staff staff = staffMap.get(homeroom);
                        if (staff != null) {
                            grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                        }
                    }

                    /*
                     * Set attendance fields
                     */
                    grid.set(FIELD_ABSENCE_DATE,
                            attendance.getDate() != null ? formatter.format(attendance.getDate()) : "");
                    grid.set(FIELD_REASON_CODE_AM, attendance.getReasonCode());
                    grid.set(FIELD_SUB_ALLOCATION_CODE_AM, attendance.getOtherCode());
                    grid.set(FIELD_AUTHORIZED_AM, attendance.getExcusedIndicator() ? AUTORIZED_TRUE : AUTORIZED_FALSE);
                    grid.set(FIELD_REASON_CODE_PM, attendance.getReasonCode02());
                    grid.set(FIELD_SUB_ALLOCATION_CODE_PM, attendance.getOtherCode02());
                    grid.set(FIELD_AUTHORIZED_PM,
                            attendance.getExcusedIndicator02() ? AUTORIZED_TRUE : AUTORIZED_FALSE);

                    /*
                     * AM code
                     */
                    String amCode = "";
                    if (attendance.getAbsentIndicator()) {
                        amCode = "A";
                    } else if (attendance.getTardyIndicator()) {
                        amCode = "T";
                    }
                    grid.set(FIELD_ABSENCE_CODE_AM, amCode);

                    /*
                     * PM code
                     */
                    String pmCode = "";
                    if (attendance.getAbsentIndicator02()) {
                        pmCode = "A";
                    } else if (attendance.getTardyIndicator02()) {
                        pmCode = "T";
                    }
                    grid.set(FIELD_ABSENCE_CODE_PM, pmCode);
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(attendance.getOid());
                    SisStudent student = attendance.getStudent();
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
            attendances.close();
        }

        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
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
        super.initialize();

        /*
         * Define columns
         */
        m_columns = new ArrayList<String>(FIELD_COUNT);

        m_columns.add(FIELD_SCHOOL_ID);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_ABSENCE_DATE);
        m_columns.add(FIELD_REASON_CODE_AM);
        m_columns.add(FIELD_SUB_ALLOCATION_CODE_AM);
        m_columns.add(FIELD_AUTHORIZED_AM);
        m_columns.add(FIELD_REASON_CODE_PM);
        m_columns.add(FIELD_SUB_ALLOCATION_CODE_PM);
        m_columns.add(FIELD_AUTHORIZED_PM);
        m_columns.add(FIELD_ABSENCE_CODE_AM);
        m_columns.add(FIELD_ABSENCE_CODE_PM);
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(StudentAttendance.REL_SCHOOL,
                StudentAttendance.COL_SCHOOL_OID));

        /*
         * Add incidents date range based on input
         */
        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

        return criteria;
    }
}
