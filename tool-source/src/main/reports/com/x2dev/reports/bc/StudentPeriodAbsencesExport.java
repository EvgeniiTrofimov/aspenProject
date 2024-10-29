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
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
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
 * Exports student period attendance information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StudentPeriodAbsencesExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Input parameters
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_INCL_IDS = "includeIds";
    private static final String PARAM_START_DATE = "startDate";

    // Grid fields
    private static final String FIELD_SCHOOL_ID = "School Number";
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_ABSENCE_DATE = "Absence Date";
    private static final String FIELD_COURSE_CODE = "Course Code";
    private static final String FIELD_ABSENCE_CATEGORY = "Absence Category";
    private static final String FIELD_ABSENCE_SUB_ALLOCATION_CODE = "Absence Sub Allocation Code";
    private static final String FIELD_AUTHORIZED_ABSENCE_CODE = "Authorized Absence Code";
    private static final String FIELD_MASTER_TIME_TABLE_ID = "Master Time Table Id";
    private static final String FIELD_OFFICE_REASON = "Office Reason";
    private static final String FIELD_SECTION_LETTER = "Section Letter";

    // Optional fields
    private static final int FIELD_COUNT = 14;
    private static final int FIELD_COUNT_WITH_IDS = 15;

    // Other constants
    private static final String ALIAS_COURSE_CODE = "crs-course-code";

    private List<String> m_columns;
    private int m_field_count;

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

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, buildCriteria());
        QueryIterator periods = getBroker().getIteratorByQuery(query);
        try {
            while (periods.hasNext()) {
                StudentPeriodAttendance period = (StudentPeriodAttendance) periods.next();

                boolean deleteRow = false;
                try {
                    SisStudent student = period.getStudent();
                    MasterSchedule master = period.getMasterSchedule();
                    String homeroom = period.getStudent().getHomeroom(getCurrentContext().getOid(), getBroker());

                    if (student != null && master != null) {
                        String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                        String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                        grid.append();
                        deleteRow = true;
                        grid.set(FIELD_SCHOOL_ID, period.getSchool().getSchoolId());
                        grid.set(FIELD_STUDENT_ID, student.getLocalId());
                        grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                        grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                        grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                        grid.set(FIELD_HOMEROOM, homeroom);
                        grid.set(FIELD_ABSENCE_DATE,
                                period.getDate() != null ? formatter.format(period.getDate()) : "");
                        grid.set(FIELD_ABSENCE_CATEGORY, period.getCodeView());
                        grid.set(FIELD_ABSENCE_SUB_ALLOCATION_CODE, period.getOtherCode());
                        grid.set(FIELD_AUTHORIZED_ABSENCE_CODE, Boolean.toString(period.getExcusedIndicator()));
                        grid.set(FIELD_OFFICE_REASON, period.getReasonCode());
                        grid.set(FIELD_SECTION_LETTER, master.getSectionNumber());
                        grid.set(FIELD_COURSE_CODE,
                                master.getSchoolCourse().getCourse().getFieldValueByAlias(ALIAS_COURSE_CODE));

                        Map<String, Staff> staffMap =
                                getHomeroomToStaffMap(student.getSchool(getCurrentContext().getOid(), getBroker()));
                        if (staffMap != null) {
                            Staff staff = staffMap.get(homeroom);
                            if (staff != null) {
                                grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                            }
                        }

                        if (m_field_count == FIELD_COUNT_WITH_IDS) {
                            grid.set(FIELD_MASTER_TIME_TABLE_ID, period.getMasterScheduleOid());
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(period.getOid());
                    SisStudent student = period.getStudent();
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
            periods.close();
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
    protected List<String> getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List<String> getColumnUserNames() {
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
        // Initialize field count
        boolean includeIds = ((Boolean) getParameter(PARAM_INCL_IDS)).booleanValue();
        if (includeIds) {
            m_field_count = FIELD_COUNT_WITH_IDS;
        } else {
            m_field_count = FIELD_COUNT;
        }

        // Initialize columns
        m_columns = new ArrayList<String>(m_field_count);

        m_columns.add(FIELD_SCHOOL_ID);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_ABSENCE_DATE);
        m_columns.add(FIELD_COURSE_CODE);
        m_columns.add(FIELD_ABSENCE_CATEGORY);
        m_columns.add(FIELD_ABSENCE_SUB_ALLOCATION_CODE);
        m_columns.add(FIELD_AUTHORIZED_ABSENCE_CODE);

        if (includeIds) {
            m_columns.add(FIELD_MASTER_TIME_TABLE_ID);
        }

        m_columns.add(FIELD_OFFICE_REASON);
        m_columns.add(FIELD_SECTION_LETTER);


    }

    /**
     * Builds the criteria for returning the period attendance.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(StudentPeriodAttendance.REL_SCHOOL,
                StudentPeriodAttendance.COL_SCHOOL_OID));

        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);
        criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, startDate);
        criteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, endDate);

        return criteria;
    }
}
