/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to create class attendance records from daily attendance records.
 * Note: this procedure is for UK only.
 *
 * @author X2 Development Corporation
 */
public class DailyToClassAttendanceConverter extends ProcedureJavaSource {
    /**
     * Name for the "date" input parameter. The value is a PlainDate.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the "required periods" input parameter. The value is an Integer.
     */
    public static final String PERIODS_VALUE_PARAM = "periodsValue";

    /**
     * Procedure instance variables.
     */
    private Map<String, Collection<StudentPeriodAttendance>> m_attendanceMap;
    private Map<String, RefAttendanceStudent> m_refAttMap;

    /**
     * Builds a map of student period attendance beans keyed on student OID.
     *
     * @param date PlainDate
     */
    private void buildPeriodAttendanceMap(PlainDate date) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentPeriodAttendance.COL_DATE, date);
        criteria.addEqualTo(StudentPeriodAttendance.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        m_attendanceMap = getBroker().getGroupedCollectionByQuery(query, StudentPeriodAttendance.COL_STUDENT_OID, 500);
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Get the parameter values
        PlainDate date = (PlainDate) getParameter(DATE_PARAM);
        int periodValue = ((Integer) getParameter(PERIODS_VALUE_PARAM)).intValue();

        // Counter for the updated / newly created StudentPeriodAttendance
        int createCount = 0;
        int updateCount = 0;

        // The period (from SYSPREF) which define which periods are AM / PM
        int inputPeriod1 = Integer
                .parseInt(PreferenceManager.getPreferenceValue(getSchool(), SisPreferenceConstants.ATT_INPUT_PERIOD));
        int inputPeriod2 = Integer.parseInt(
                PreferenceManager.getPreferenceValue(getSchool(), SisPreferenceConstants.ATT_INPUT_PERIOD_02));

        // Building the the period attendance map and also loading the reference attendance student
        // map
        buildPeriodAttendanceMap(date);
        loadRefAtt();

        // Create the StringBuilder for a SQL query to be executed later on to get all the
        // DailyAttendance for the given date
        // TODO we can remove the inner join if needed and if possible
        StringBuilder sql = new StringBuilder();
        sql.append(
                " SELECT ATT_STD_OID, STD_SKL_OID, ATT_OTHER_CODE, ATT_OTHER_CODE_02, ATT_REASON_CODE, ATT_REASON_CODE_02, ");
        sql.append("        ATT_COMMENT, ATT_COMMENT_02 ");
        sql.append("   FROM STUDENT_ATTENDANCE ");
        sql.append("  INNER JOIN STUDENT ON ATT_STD_OID = STD_OID ");
        sql.append("  WHERE ATT_DATE = ? ");
        sql.append("    AND ATT_SKL_OID = '" + getSchool().getOid() + "' ");
        sql.append("  GROUP BY ATT_STD_OID, ATT_SKL_OID ");

        ModelBroker broker = new ModelBroker(getPrivilegeSet());
        Connection connection = broker.borrowConnection();
        PreparedStatement statement = null;
        ResultSet resultSet = null;
        try {
            statement = connection.prepareStatement(sql.toString());
            statement.setDate(1, date);

            resultSet = statement.executeQuery();
            while (resultSet.next()) {
                // To keep track all the StudentPeriodAttendance that have been updated (or created
                // later on)
                Collection<String> sectionsUpdated = new HashSet<String>();

                // Get all the fields / variables that we want to use to update/create the
                // StudentPeriodAttendance
                String studentOid = resultSet.getString("ATT_STD_OID");
                String schoolOid = resultSet.getString("STD_SKL_OID");
                String otherCode = "";
                String reasonCode = "";
                String comment = "";
                RefAttendanceStudent refCode = null;

                // Set the reason, comment, and otherCode based on the AM or PM Attendance from the
                // input definition.
                // If periodValue = 1, it's AM attendance. If periodValue = 2, it's PM attendance.
                if (periodValue == 1) {
                    otherCode = resultSet.getString("ATT_OTHER_CODE");
                    reasonCode = resultSet.getString("ATT_REASON_CODE");
                    comment = resultSet.getString("ATT_COMMENT");
                }
                if (periodValue == 2) {
                    otherCode = resultSet.getString("ATT_OTHER_CODE_02");
                    reasonCode = resultSet.getString("ATT_REASON_CODE_02");
                    comment = resultSet.getString("ATT_COMMENT_02");
                }

                // Update the refCode
                if (m_refAttMap != null) {
                    refCode = m_refAttMap.get(otherCode);
                }

                // Then, try to get the existing StudentPeriodAttendance for the given student
                Collection<StudentPeriodAttendance> classAttendanceCol = null;
                if (m_attendanceMap != null) {
                    classAttendanceCol = m_attendanceMap.get(studentOid);
                }

                // If there's already class/periodAttendance for that student, then update it
                if (classAttendanceCol != null && !classAttendanceCol.isEmpty()) {
                    for (StudentPeriodAttendance classAttendance : classAttendanceCol) {
                        String period = classAttendance.getPeriodView();
                        String[] periods = null;

                        if (period != null) {
                            if (period.contains(",")) // in this case, the periods are separated by
                                                      // comma
                            {
                                periods = period.split(",");
                            } else if (period.contains("-")) // in this case, periods are in the
                                                             // range
                            {
                                int curPeriod = Integer
                                        .parseInt(period.substring(period.indexOf("-") - 1, period.indexOf("-")));
                                int endPeriod = Integer
                                        .parseInt(period.substring(period.indexOf("-") + 1, period.indexOf("-") + 2));
                                periods = new String[endPeriod - curPeriod + 1];

                                int counter = 0;
                                while (curPeriod < endPeriod) {
                                    periods[counter] = String.valueOf(curPeriod);
                                    counter++;
                                    curPeriod++;
                                }
                            } else // if it gets here, it means it should only contain one period
                            {
                                periods = new String[1];
                                periods[0] = period;
                            }

                            if (periods != null) {
                                for (String eachPeriod : periods) {
                                    try {
                                        int tempPeriod = Integer.parseInt(eachPeriod);

                                        /*
                                         * Check whether it's AM or PM attendance (from the input
                                         * definition), then update
                                         * if the period is within that AM/PM periods.
                                         */
                                        if ((periodValue == 1 && inputPeriod1 <= tempPeriod
                                                && tempPeriod < inputPeriod2) ||
                                                (periodValue == 2 && tempPeriod >= inputPeriod2)) {
                                            classAttendance.setOtherCode(otherCode);
                                            classAttendance.setReasonCode(reasonCode);
                                            classAttendance.setComment(comment);

                                            if (refCode != null) {
                                                classAttendance.setAbsentIndicator(refCode.getAbsentIndicator());
                                                classAttendance.setTardyIndicator(refCode.getTardyIndicator());
                                                classAttendance.setDismissedIndicator(refCode.getDismissedIndicator());
                                            }

                                            broker.saveBeanForced(classAttendance);
                                            sectionsUpdated.add(classAttendance.getMasterScheduleOid());
                                            updateCount++;
                                        }
                                    } catch (Exception ex) {
                                        logMessage("Cannnot Update the StudentPeriod Attendance with OID: "
                                                + classAttendance.getOid());
                                    }
                                }
                            }
                        }
                    }
                }

                String scheduleOid = ((SisSchool) getSchool()).getActiveScheduleOid();
                ScheduleManager scheduleManager = new ScheduleManager(broker);

                Collection periods = ((SisSchool) getSchool()).getActiveSchedule().getSchedulePeriods(broker);
                Iterator periodIterator = periods.iterator();
                while (periodIterator.hasNext()) {
                    SchedulePeriod period = (SchedulePeriod) periodIterator.next();
                    String periodOid = period.getOid();
                    int curPeriod = period.getNumber();

                    /*
                     * Check whether it's AM or PM attendance (from the input definition), then
                     * update
                     * if the period is within that AM/PM periods.
                     */
                    if ((periodValue == 1 && inputPeriod1 <= curPeriod && curPeriod < inputPeriod2) ||
                            (periodValue == 2 && curPeriod >= inputPeriod2)) {
                        Collection sections = scheduleManager.getSections(scheduleOid, studentOid, date, periodOid);
                        if (!sections.isEmpty()) {
                            StudentSchedule studentSchedule = (StudentSchedule) sections.iterator().next();

                            // If it hasn't been updated yet, then create a new
                            // StudentPeriodAttendance for it
                            if (!sectionsUpdated.contains(studentSchedule.getSectionOid())) {
                                StudentPeriodAttendance periodAttendance = X2BaseBean
                                        .newInstance(StudentPeriodAttendance.class, broker.getPersistenceKey());
                                periodAttendance.setStudentOid(studentOid);
                                periodAttendance.setSchoolOid(schoolOid);
                                periodAttendance.setMasterScheduleOid(studentSchedule.getSectionOid());
                                periodAttendance.setDate(date);
                                periodAttendance.setOtherCode(otherCode);
                                periodAttendance.setReasonCode(reasonCode);
                                periodAttendance.setComment(comment);

                                if (refCode != null) {
                                    periodAttendance.setAbsentIndicator(refCode.getAbsentIndicator());
                                    periodAttendance.setTardyIndicator(refCode.getTardyIndicator());
                                    periodAttendance.setDismissedIndicator(refCode.getDismissedIndicator());
                                }

                                broker.saveBeanForced(periodAttendance);
                                sectionsUpdated.add(periodAttendance.getMasterScheduleOid());
                                createCount++;
                            }
                        }
                    }
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException e) {
                    AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
                }
            }

            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException e) {
                    AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
                }
            }

            broker.returnConnection();
        }

        logMessage("Updated " + updateCount + " Period Attendance record" + (updateCount != 1 ? "s" : "") + ".");
        logMessage("Created " + createCount + " Period Attendance record" + (createCount != 1 ? "s" : "") + ".");
    }

    /**
     * Load a map of daily attendance other code records by attendance code.
     */
    private void loadRefAtt() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE,
                Integer.valueOf(RefAttendanceStudent.ATTENDANCE_TYPE_DAILY));
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE, Integer.valueOf(RefAttendanceStudent.TYPE_OTHER_CODE));
        QueryByCriteria query = new QueryByCriteria(RefAttendanceStudent.class, criteria);
        m_refAttMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 30);
    }
}
