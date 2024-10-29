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

package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Attendance Bulletin" report for UK.
 *
 * @author X2 Development Corporation
 */
public class AttendanceBulletinData extends ReportJavaSourceNet {
    /**
     * Name for the "group by YOG" report parameter. The value is a Boolean.
     */
    private static final String GROUP_BY_YOG_PARAM = "groupByYog";

    /**
     * Report parameter name for the input date. This value is a PlainDate object.
     */
    private static final String INPUT_DATE_PARAM = "inputDate";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    private static final String SORT_PARAM = "sort";

    /**
     * Report parameter name for the school. This value is a SisSchool object.
     */
    private static final String SCHOOL_PARAM = "school";

    /**
     * Report parameter name for the organization. This value is a SisOrganization object.
     */
    private static final String ORGANIZATION_PARAM = "organization";

    /**
     * Name for the "Number of Possible Attendance" report parameter.
     */
    private static final String TOTAL_ATTENDANCE_POSSIBLE_PARAM = "totalAttendance";

    /**
     * Name for the authorized absences report parameter.
     */
    private static final String AUTHORIZED_ABSENCES_PARAM = "authorizedAbsences";

    /**
     * Name for the unauthorized absences report parameter.
     */
    private static final String UNAUTHORIZED_ABSENCES_PARAM = "unauthorizedAbsences";

    /**
     * Name for the statistical absences report parameter.
     */
    private static final String STATISTICAL_ABSENCES_PARAM = "statisticalAbsences";

    /**
     * Name for the non-statistical absences report parameter.
     */
    private static final String NONSTATISTICAL_ABSENCES_PARAM = "nonStatisticalAbsences";

    /**
     * Name for the authorized tardies report parameter.
     */
    private static final String AUTHORIZED_TARDIES_PARAM = "authorizedTardies";

    /**
     * Name for the unauthorized tardies report parameter.
     */
    private static final String UNAUTHORIZED_TARDIES_PARAM = "unauthorizedTardies";

    /**
     * Name for the statistical tardies report parameter.
     */
    private static final String STATISTICAL_TARDIES_PARAM = "statisticalTardies";

    /**
     * Name for the non-statistical tardies report parameter.
     */
    private static final String NONSTATISTICAL_TARDIES_PARAM = "nonStatisticalTardies";

    /**
     * These are the absence codes that not counted in possible attendances
     */
    private static final String NON_COUNTED_ABSENCE_CODES = "XYZ#";

    /**
     * Name of the GRID / Field in the iReport
     */
    private static final String GRID_STUDENT_ATT_OID = "oid";
    private static final String GRID_STUDENT = "student";
    private static final String GRID_STUDENT_OID = "studentOid";
    private static final String GRID_SCHOOL = "studentOid";
    private static final String GRID_CODE_VIEW = "codeView";
    private static final String GRID_TIME_VIEW = "timeView";
    private static final String GRID_DATE = "date";

    /**
     * Report instance variables.
     */
    private static Collection<StudentAttendance> m_studentAttendance;
    private static PlainDate m_inputDate;
    private Map<String, RefAttendanceStudent> m_refAttMap;
    private DataDictionary m_refAttStdDictionary;

    /**
     * Alias for the Authorized and Statistical.
     */
    private static final String ALIAS_AUTHORIZED = "rat-att-std-authorized";
    private static final String ALIAS_STATISTICAL = "rat-att-std-statistical";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.x2dev.sis.reporting.ReportDataSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        addParameter(SCHOOL_PARAM, getSchool());
        addParameter(ORGANIZATION_PARAM, getOrganization());
        addParameter(INPUT_DATE_PARAM, new SimpleDateFormat("MMMM dd, yyyy").format(m_inputDate));

        if (m_studentAttendance != null && !m_studentAttendance.isEmpty()) {
            // load from the ref_attendance_student
            loadRefAtt();

            // Get the extended data dictionary (for Authorized, Statistical, and Explained)
            ReferenceTable refAttStdTabl = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    ReferenceTable.REF_TABLE_OID_STUDENT_ATTENDANCE);
            ExtendedDataDictionary refAttStdDdx = refAttStdTabl.getExtendedDataDictionary();
            m_refAttStdDictionary = DataDictionary.getDistrictDictionary(refAttStdDdx, getBroker().getPersistenceKey());

            // add the parameter that needed
            int sort = Integer.parseInt(getParameter(SORT_PARAM).toString());
            if (sort == 1) {
                addParameter(GROUP_BY_YOG_PARAM, Boolean.TRUE);
            } else {
                addParameter(GROUP_BY_YOG_PARAM, Boolean.FALSE);
            }

            int authorizedAbsences = 0;
            int unauthorizedAbsences = 0;
            int statisticalAbsences = 0;
            int nonStatisticalAbsences = 0;

            int authorizedTardies = 0;
            int unauthorizedTardies = 0;
            int statisticalTardies = 0;
            int nonStatisticalTardies = 0;

            int totalAttendance = 0;

            // for each student Attendance, fill up the grid. Also count the number of
            for (StudentAttendance stdAtt : m_studentAttendance) {
                // put the code in the list for AM and PM for each student (when the code is null or
                // empty, we mark it as present)
                ArrayList<String> codeListAMPM = new ArrayList<String>();
                codeListAMPM.add((stdAtt.getOtherCode() == null || stdAtt.getOtherCode().isEmpty()) ? "/"
                        : stdAtt.getOtherCode().toString());
                codeListAMPM.add((stdAtt.getOtherCode02() == null || stdAtt.getOtherCode02().isEmpty()) ? "\\"
                        : stdAtt.getOtherCode02().toString());

                for (String attCode : codeListAMPM) {
                    // Add to total attendance
                    if (!NON_COUNTED_ABSENCE_CODES.contains(attCode)) {
                        totalAttendance++;

                        // check whether absent or tardy
                        if (isAbsent(attCode)) {
                            if (isAuthorized(attCode)) {
                                authorizedAbsences++;
                            } else {
                                unauthorizedAbsences++;
                            }

                            if (isStatistical(attCode)) {
                                statisticalAbsences++;
                            } else {
                                nonStatisticalAbsences++;
                            }
                        }

                        if (isTardy(attCode)) {
                            if (isAuthorized(attCode)) {
                                authorizedTardies++;
                            } else {
                                unauthorizedTardies++;
                            }

                            if (isStatistical(attCode)) {
                                statisticalTardies++;
                            } else {
                                nonStatisticalTardies++;
                            }
                        }
                    }
                }

                grid.append();
                grid.set(GRID_STUDENT_ATT_OID, stdAtt.getOid()); // set the student attendance oid
                grid.set(GRID_STUDENT, stdAtt.getStudent()); // set the student object
                grid.set(GRID_STUDENT_OID, stdAtt.getStudentOid()); // set the student oid
                grid.set(GRID_SCHOOL, stdAtt.getSchool()); // set the school
                grid.set(GRID_CODE_VIEW, stdAtt.getCodeView()); // set the student attendance code
                                                                // view
                grid.set(GRID_TIME_VIEW, stdAtt.getTimeView()); // set the student attendance time
                                                                // view
                grid.set(GRID_DATE, stdAtt.getDate()); // set the student attendance date
            }

            addParameter(AUTHORIZED_ABSENCES_PARAM, Integer.valueOf(authorizedAbsences));
            addParameter(UNAUTHORIZED_ABSENCES_PARAM, Integer.valueOf(unauthorizedAbsences));
            addParameter(STATISTICAL_ABSENCES_PARAM, Integer.valueOf(statisticalAbsences));
            addParameter(NONSTATISTICAL_ABSENCES_PARAM, Integer.valueOf(nonStatisticalAbsences));
            addParameter(AUTHORIZED_TARDIES_PARAM, Integer.valueOf(authorizedTardies));
            addParameter(UNAUTHORIZED_TARDIES_PARAM, Integer.valueOf(unauthorizedTardies));
            addParameter(STATISTICAL_TARDIES_PARAM, Integer.valueOf(statisticalTardies));
            addParameter(NONSTATISTICAL_TARDIES_PARAM, Integer.valueOf(nonStatisticalTardies));

            addParameter(TOTAL_ATTENDANCE_POSSIBLE_PARAM, Integer.valueOf(totalAttendance));

            grid.beforeTop();
        }

        return grid;
    }

    /**
     * Returns an absent indicator based on the other code provided.
     *
     * @param attendanceCode String
     * @return boolean
     */
    private boolean isAbsent(String attendanceCode) {
        boolean isAbsent = false;
        if (m_refAttMap != null && attendanceCode != null) {
            RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
            if (refAttStd != null) {
                isAbsent = refAttStd.getAbsentIndicator();
            }
        }
        return isAbsent;
    }

    /**
     * Returns an excused indicator based on the other code provided.
     *
     * Note: these fields (authorized, statistical, and explained) must be defined as "logical" in
     * extended data
     * dictionary, otherwise you will get exception.
     *
     * @param attendanceCode String
     * @return boolean
     */
    private boolean isAuthorized(String attendanceCode) {
        boolean isAuthorized = false;
        if (m_refAttMap != null && attendanceCode != null) {
            RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
            Boolean tempAuthorizedCode =
                    (Boolean) refAttStd.getFieldValueByAliasExtended(ALIAS_AUTHORIZED, m_refAttStdDictionary);
            if (tempAuthorizedCode != null && tempAuthorizedCode.booleanValue()) {
                isAuthorized = true;
            }
        }

        return isAuthorized;
    }

    /**
     * Returns a statistical indicator based on the other code provided.
     *
     * Note: these fields (authorized, statistical, and explained) must be defined as "logical" in
     * extended data
     * dictionary, otherwise you will get exception.
     *
     * @param attendanceCode String
     * @return boolean
     */
    private boolean isStatistical(String attendanceCode) {
        boolean isStatistical = false;
        if (m_refAttMap != null && attendanceCode != null) {
            RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
            Boolean tempStatisticalCode =
                    (Boolean) refAttStd.getFieldValueByAliasExtended(ALIAS_STATISTICAL, m_refAttStdDictionary);
            if (tempStatisticalCode != null && tempStatisticalCode.booleanValue()) {
                isStatistical = true;
            }
        }
        return isStatistical;
    }

    /**
     * Returns a tardy indicator based on the other code provided.
     *
     * @param attendanceCode String
     * @return boolean
     */
    private boolean isTardy(String attendanceCode) {
        boolean isLate = false;
        if (m_refAttMap != null && attendanceCode != null) {
            RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
            if (refAttStd != null) {
                isLate = refAttStd.getTardyIndicator();
            }
        }
        return isLate;
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

    /**
     *
     * This method is provided as a way for subclasses to save session state information.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_inputDate = (PlainDate) getParameter(INPUT_DATE_PARAM);

        m_studentAttendance = new ArrayList(0);
        if (m_inputDate != null) {
            /*
             * Build the criteria based on the (possible) school and date range
             */
            Criteria criteria = new Criteria();

            if (isSchoolContext()) {
                criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
            }
            criteria.addEqualTo(StudentAttendance.COL_DATE, m_inputDate);

            QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
            query.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);

            /*
             * Build the sort based on user input
             */
            int sort = Integer.parseInt(getParameter(SORT_PARAM).toString());
            switch (sort) {
                case 0: // Name
                    query.addOrderByAscending(StudentAttendance.COL_DATE);
                    query.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    query.addOrderByAscending(StudentAttendance.COL_DATE);
                    query.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_YOG);
                    query.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // Homeroom
                    query.addOrderByAscending(StudentAttendance.COL_DATE);
                    query.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_HOMEROOM);
                    query.addOrderByAscending(StudentAttendance.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
                    break;

                default:
                    // No sort specified
                    break;
            }

            m_studentAttendance = getBroker().getCollectionByQuery(query);

        }
    }
}
