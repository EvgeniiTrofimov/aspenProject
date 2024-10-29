/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRResultSetDataSource;

/**
 * Prepares the data for the Student Locker List. It is an extension of the Student List report.
 *
 * @author X2 Development Corporation
 */
public class LockerAssignmentData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "display" report parameter. The value is an Integer.
     */
    public static final String DISPLAY_PARAM = "display";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Selected fields in query.
     */
    private static final String SELECTED_FIELDS = "STD_OID, STD_SKL_OID, STD_NAME_VIEW, STD_YOG, " +
            "STD_ID_LOCAL, STD_LOCKER, STD_HOMEROOM, LKR_OID, LKR_SKL_OID, LKR_NUMBER, LKR_COMBINATION_CODE ";

    private boolean m_activeOnly;
    private ResultSet m_resultSet;
    private PreparedStatement m_statement;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        int display = ((Integer) getParameter(DISPLAY_PARAM)).intValue();
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        m_activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();

        /*
         * Build sql statement.
         */
        StringBuilder sql = new StringBuilder(256);

        switch (display) {
            case 0: // student
                sql.append(buildStudentToLockerSql());
                break;

            case 1: // Missing lockers
                sql.append(buildStudentToLockerSql());
                sql.append("AND LKR_OID IS NULL ");
                break;

            case 2: // Locker
                sql.append(buildLockerToStudentSql());
                break;

            case 3: // Unassigned lockers
                sql.append(buildLockerToStudentSql());
                sql.append("AND STD_OID IS NULL ");
                break;

            case 4: // All
                sql.append(buildFullSql());
                break;
        }

        /*
         * Build the sort based on user input (first sort by the school)
         */
        switch (sort) {
            case 0: // Name view
                sql.append(" ORDER BY STD_SKL_OID, STD_NAME_VIEW, STD_OID, LKR_NUMBER ");
                break;

            case 1: // YOG
                sql.append(" ORDER BY STD_SKL_OID, STD_YOG, STD_NAME_VIEW, STD_OID, LKR_NUMBER ");
                break;

            case 2: // Homeroom
                sql.append(" ORDER BY STD_SKL_OID, STD_HOMEROOM, STD_NAME_VIEW, STD_OID, LKR_NUMBER ");
                break;

            case 3: // Locker
                sql.append(" ORDER BY LKR_SKL_OID, LKR_NUMBER, STD_NAME_VIEW, STD_OID ");
                break;
        }

        /*
         * Execute the query and return the results.
         */
        JRDataSource dataSource = null;
        try {
            Connection connection = getBroker().borrowConnection();
            m_statement = connection.prepareStatement(sql.toString());
            m_resultSet = m_statement.executeQuery();
            dataSource = new JRResultSetDataSource(m_resultSet);
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }

        return dataSource;
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
     */
    @Override
    protected void releaseResources() {
        if (m_resultSet != null) {
            try {
                m_resultSet.close();
            } catch (SQLException e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }
        }

        if (m_statement != null) {
            try {
                m_statement.close();
            } catch (SQLException e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }
        }

        getBroker().returnConnection();
    }

    /**
     * Returns the SQL used to get all the student and locker data.
     *
     * @return StringBuilder
     */
    private StringBuilder buildFullSql() {
        StringBuilder fullSql = new StringBuilder(256);

        fullSql.append(buildLockerToStudentSql());
        fullSql.append(" UNION ");
        fullSql.append(buildStudentToLockerSql());

        return fullSql;
    }

    /**
     * Returns the SQL that returns all the lockers and any students that are related.
     *
     * @return StringBuilder
     */
    private StringBuilder buildLockerToStudentSql() {
        StringBuilder lockerToStudent = new StringBuilder(128);

        lockerToStudent.append("SELECT " + SELECTED_FIELDS);
        lockerToStudent.append("FROM SCHOOL_LOCKER LEFT OUTER JOIN STUDENT  ");
        lockerToStudent.append("ON LKR_NUMBER = STD_LOCKER ");
        lockerToStudent.append("AND STD_SKL_OID = '" + getSchool().getOid() + "' ");

        if (m_activeOnly) {
            lockerToStudent.append(
                    "AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
        }

        lockerToStudent.append("WHERE LKR_SKL_OID = '" + getSchool().getOid() + "' ");

        return lockerToStudent;
    }

    /**
     * Returns the SQL that returns all the students and any lockers that are related.
     *
     * @return StringBuilder
     */
    private StringBuilder buildStudentToLockerSql() {
        StringBuilder studentToLocker = new StringBuilder(128);

        studentToLocker.append("SELECT " + SELECTED_FIELDS);
        studentToLocker.append("FROM SCHOOL_LOCKER RIGHT OUTER JOIN STUDENT  ");
        studentToLocker.append("ON LKR_NUMBER = STD_LOCKER ");
        studentToLocker.append("AND LKR_SKL_OID = '" + getSchool().getOid() + "' ");
        studentToLocker.append("WHERE STD_SKL_OID = '" + getSchool().getOid() + "' ");

        if (m_activeOnly) {
            studentToLocker.append(
                    "AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
        }

        return studentToLocker;
    }
}
