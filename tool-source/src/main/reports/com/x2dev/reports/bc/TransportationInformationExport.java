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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentTransportation;
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
 * Exports transportation information (student transportation for the date range) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class TransportationInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_BUS_ROUTE = "Bus Route";
    private static final String FIELD_BUS_NUMBER = "Bus Number";
    private static final String FIELD_BUS_STOP_INFO = "Bus Stop Information";

    // Transportation Information Aliases
    private static final String ALIAS_BUS_ROUTE = "str-bus-route";
    private static final String ALIAS_BUS_NUMBER = "str-bus-number";
    private static final String ALIAS_BUS_STOP_INFO = "str-bus-stop";

    // Other constants
    private static final int FIELD_COUNT = 9;

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
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(StudentTransportation.class, buildCriteria());
        query.addOrderByAscending(StudentTransportation.COL_SCHOOL_OID);
        query.addOrderByAscending(
                StudentTransportation.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_NAME_VIEW);
        query.addOrderByAscending(StudentTransportation.COL_STUDENT_OID);

        QueryIterator transportations = getBroker().getIteratorByQuery(query);
        try {
            while (transportations.hasNext()) {
                StudentTransportation transportation = (StudentTransportation) transportations.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = transportation.getStudent();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    grid.append();
                    deleteRow = true;

                    grid.set(FIELD_STUDENT_ID, student.getLocalId());
                    grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_HOMEROOM, student.getHomeroom());
                    grid.set(FIELD_BUS_ROUTE, transportation.getFieldValueByAlias(ALIAS_BUS_ROUTE));
                    grid.set(FIELD_BUS_NUMBER, transportation.getFieldValueByAlias(ALIAS_BUS_NUMBER));
                    grid.set(FIELD_BUS_STOP_INFO, transportation.getFieldValueByAlias(ALIAS_BUS_STOP_INFO));

                    /*
                     * Set homeroom teacher name
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(student.getSchool());
                    if (staffMap != null) {
                        Staff staff = staffMap.get(student.getHomeroom());
                        if (staff != null) {
                            grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(transportation.getOid());
                    SisStudent student = transportation.getStudent();
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
            transportations.close();
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

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_BUS_ROUTE);
        m_columns.add(FIELD_BUS_NUMBER);
        m_columns.add(FIELD_BUS_STOP_INFO);
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(StudentTransportation.REL_SCHOOL,
                StudentTransportation.COL_SCHOOL_OID));

        DistrictSchoolYearContext context = getCurrentContext();
        criteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, context.getEndDate());
        criteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, context.getStartDate());

        return criteria;
    }
}
