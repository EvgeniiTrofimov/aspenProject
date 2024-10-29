/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Staff Attendance Summary" report. This report shows aggregate
 * staff attendance statistics broken down by school.
 *
 * @author X2 Development Corporation
 */
public class StaffAttendanceSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "contract days" report parameter. The value is an Integer.
     */
    public static final String CONTRACT_DAYS_PARAM = "contractDays";

    /**
     * Name for the "end date" report parameter. The value is a PlainDate.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the enumerated "query by" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "start date" report parameter. The value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";

    private static final String ABSENCE_COUNT_FIELD = "absence_count";
    private static final String ABSENCE_CODE = "A";
    private static final String DEPARTURE_CODE = "D";
    private static final String EARLY_DEPARTURE_COUNT_FIELD = "early_count";
    private static final String LATE_ARRIVAL_COUNT_FIELD = "late_count";
    private static final String LATE_CODE = "L";
    private static final String SCHOOL_FIELD = "school";
    private static final String SCHOOL_SORT = "schoolSort";
    private static final String STAFF_COUNT_FIELD = "staff_count";
    private static final String PERFECT_ATTENDANCE_COUNT_FIELD = "perfect_attendance_count";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = null;

        try {
            Connection connection = getBroker().borrowConnection();

            PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);
            PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);

            StringBuilder staffCriteria = buildStaffCriteriaSql();

            /*
             * Count absences
             */
            StringBuilder sql = new StringBuilder(1500);
            sql.append("SELECT skl_oid, COUNT(sfa_oid) AS " + ABSENCE_COUNT_FIELD);
            sql.append(" FROM SCHOOL ");
            sql.append(" INNER JOIN STAFF ON skl_oid = stf_skl_oid ");
            sql.append(" INNER JOIN STAFF_ATTENDANCE ON sfa_stf_oid = stf_oid ");
            sql.append(" INNER JOIN REF_ATTENDANCE_STAFF sfa_ras_oid = ras_oid ");
            sql.append(" WHERE sfa_date >= ? ");
            sql.append(" AND sfa_date <= ? ");
            sql.append(" AND ras_code = '" + ABSENCE_CODE + "'");
            sql.append(staffCriteria);
            sql.append(" GROUP BY skl_oid ");

            PreparedStatement absencesStatement = connection.prepareStatement(sql.toString());
            absencesStatement.setDate(1, startDate);
            absencesStatement.setDate(2, endDate);
            ResultSet absences = absencesStatement.executeQuery();

            /*
             * Count late arrivals
             */
            sql.delete(0, sql.length());
            sql.append("SELECT skl_oid, COUNT(sfa_oid) AS " + LATE_ARRIVAL_COUNT_FIELD);
            sql.append(" FROM SCHOOL ");
            sql.append(" INNER JOIN STAFF ON skl_oid = stf_skl_oid ");
            sql.append(" INNER JOIN STAFF_ATTENDANCE ON sfa_stf_oid = stf_oid ");
            sql.append(" INNER JOIN REF_ATTENDANCE_STAFF sfa_ras_oid = ras_oid ");
            sql.append(" WHERE sfa_date >= ? ");
            sql.append(" AND sfa_date <= ? ");
            sql.append(" AND ras_code = '" + LATE_CODE + "'");
            sql.append(staffCriteria);
            sql.append(" GROUP BY skl_oid ");

            PreparedStatement lateStatement = connection.prepareStatement(sql.toString());
            lateStatement.setDate(1, startDate);
            lateStatement.setDate(2, endDate);
            ResultSet lateArrivals = lateStatement.executeQuery();

            /*
             * Count early departures
             */
            sql.delete(0, sql.length());
            sql.append("SELECT skl_oid, COUNT(sfa_oid) AS " + EARLY_DEPARTURE_COUNT_FIELD);
            sql.append(" FROM SCHOOL ");
            sql.append(" INNER JOIN STAFF ON skl_oid = stf_skl_oid ");
            sql.append(" INNER JOIN STAFF_ATTENDANCE ON sfa_stf_oid = stf_oid ");
            sql.append(" INNER JOIN REF_ATTENDANCE_STAFF sfa_ras_oid = ras_oid ");
            sql.append(" WHERE sfa_date >= ? ");
            sql.append(" AND sfa_date <= ? ");
            sql.append(" AND ras_code = '" + DEPARTURE_CODE + "'");
            sql.append(staffCriteria);
            sql.append(" GROUP BY skl_oid ");

            PreparedStatement earlyStatement = connection.prepareStatement(sql.toString());
            earlyStatement.setDate(1, startDate);
            earlyStatement.setDate(2, endDate);
            ResultSet earlyDepartures = earlyStatement.executeQuery();

            /*
             * Count staff members
             */
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

            sql.delete(0, sql.length());
            sql.append("SELECT skl_oid, COUNT(stf_oid) as " + STAFF_COUNT_FIELD);
            sql.append(" FROM SCHOOL ");
            sql.append(" INNER JOIN STAFF ON skl_oid = stf_skl_oid ");
            sql.append(" WHERE stf_status = '" + activeCode + "'");
            sql.append(staffCriteria);
            sql.append(" GROUP BY skl_oid ");

            Statement staffStatement = connection.createStatement();
            ResultSet staffMembers = staffStatement.executeQuery(sql.toString());

            /*
             * Count the number of staff members with perfect attendance
             */
            sql.delete(0, sql.length());
            sql.append("SELECT skl_oid, COUNT(stf_oid) as " + PERFECT_ATTENDANCE_COUNT_FIELD);
            sql.append(" FROM SCHOOL ");
            sql.append(" INNER JOIN STAFF ON skl_oid = stf_skl_oid ");
            sql.append(" WHERE NOT EXISTS");
            sql.append(" (");
            sql.append("   SELECT sfa_oid");
            sql.append("   FROM STAFF_ATTENDANCE");
            sql.append("   WHERE sfa_stf_oid = STAFF.stf_oid");
            sql.append(" )");
            sql.append(staffCriteria);
            sql.append(" GROUP BY skl_oid ");

            Statement perfectStatement = connection.createStatement();
            ResultSet perfectAttendance = perfectStatement.executeQuery(sql.toString());

            List attendanceSummary = groupData(absences, lateArrivals, earlyDepartures,
                    staffMembers, perfectAttendance);

            grid = new ReportDataGrid(attendanceSummary.size(), 16);
            grid.append(attendanceSummary);
            grid.sort(SCHOOL_SORT, true);
            grid.beforeTop();

            absences.close();
            absencesStatement.close();
            lateArrivals.close();
            lateStatement.close();
            earlyDepartures.close();
            earlyStatement.close();
            staffMembers.close();
            staffStatement.close();
            perfectAttendance.close();
            perfectStatement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return grid;
    }

    /**
     * Builds additional criteria on the Staff records based on user-input.
     *
     * @return StringBuilder
     */
    private StringBuilder buildStaffCriteriaSql() {
        StringBuilder sql = new StringBuilder(64);

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // Bargaining Unit
                sql.append(" AND stf_bargaining_unit = '" + getParameter(QUERY_STRING_PARAM) + "' ");
                break;

            case 2: // Department
                sql.append(" AND stf_department_code = '" + getParameter(QUERY_STRING_PARAM) + "' ");
                break;

            default: // All
                sql.append(" ");
                break;
        }

        return sql;
    }

    /**
     * Constructs a collection of Maps representing a data table containing the staff attendance
     * statistics to display on the report, grouped by school.
     *
     * This method merges the results of 5 separate queries used to obtain the difference
     * statistics.
     * It is only necessary because of the limitations of certain SQL engines. With some databases,
     * the data returned can be obtained with a single complex SQL statement.
     *
     * @param absences ResultSet containing the number of absences per school
     * @param lateArrivals ResultSet containing the number of late arrivals per school
     * @param earlyDepartures ResultSet containing the number of early departures per school
     * @param staffMembers ResultSet containing the number of staff members per school
     * @param perfectAttendance ResultSet containing the number of staff members with perfect
     *        attendance per school
     * @return a List of Maps
     * @throws SQLException exception
     */
    private List groupData(ResultSet absences,
                           ResultSet lateArrivals,
                           ResultSet earlyDepartures,
                           ResultSet staffMembers,
                           ResultSet perfectAttendance)
            throws SQLException {
        HashMap rows = new HashMap(100);

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class);
        QueryIterator schools = null;
        try {
            schools = getBroker().getIteratorByQuery(schoolQuery);
            while (schools.hasNext()) {
                SisSchool school = (SisSchool) schools.next();
                String sortKey = school.getSchoolLevelCode() + school.getSchoolId() + school.getOid();

                HashMap row = new HashMap(10);
                row.put(SCHOOL_FIELD, school);
                row.put(SCHOOL_SORT, sortKey);

                rows.put(school.getOid(), row);
            }
        } finally {
            if (schools != null) {
                schools.close();
            }
        }

        while (absences.next()) {
            String schoolOid = absences.getString("skl_oid");
            HashMap row = (HashMap) rows.get(schoolOid);

            int absenceCount = absences.getInt(ABSENCE_COUNT_FIELD);
            row.put(ABSENCE_COUNT_FIELD, Integer.valueOf(absenceCount));
        }

        while (lateArrivals.next()) {
            String schoolOid = lateArrivals.getString("skl_oid");
            HashMap row = (HashMap) rows.get(schoolOid);

            int lateCount = lateArrivals.getInt(LATE_ARRIVAL_COUNT_FIELD);
            row.put(LATE_ARRIVAL_COUNT_FIELD, Integer.valueOf(lateCount));
        }

        while (earlyDepartures.next()) {
            String schoolOid = earlyDepartures.getString("skl_oid");
            HashMap row = (HashMap) rows.get(schoolOid);

            int earlyCount = earlyDepartures.getInt(EARLY_DEPARTURE_COUNT_FIELD);
            row.put(EARLY_DEPARTURE_COUNT_FIELD, Integer.valueOf(earlyCount));
        }

        while (staffMembers.next()) {
            String schoolOid = staffMembers.getString("skl_oid");
            HashMap row = (HashMap) rows.get(schoolOid);

            int staffCount = staffMembers.getInt(STAFF_COUNT_FIELD);
            row.put(STAFF_COUNT_FIELD, Integer.valueOf(staffCount));
        }

        while (perfectAttendance.next()) {
            String schoolOid = perfectAttendance.getString("skl_oid");
            HashMap row = (HashMap) rows.get(schoolOid);

            int perfectAttendanceCount = perfectAttendance.getInt(PERFECT_ATTENDANCE_COUNT_FIELD);
            row.put(PERFECT_ATTENDANCE_COUNT_FIELD, Integer.valueOf(perfectAttendanceCount));
        }

        return new ArrayList(rows.values());
    }
}
