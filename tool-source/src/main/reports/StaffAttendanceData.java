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

import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PreferenceSet;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.sis.model.business.attendance.StaffAttendanceHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import net.sf.jasperreports3.engine.JRDataSource;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * Prepares the data for the "Staff Attendance" report. This report lists the staff attendance for
 * the entered date range and current school grouped by date.
 *
 * @author X2 Development Corporation
 */
public class StaffAttendanceData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Last date in the report date range
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Number of records on the report to be displayed in an attendance group grouping
     */
    public static final String GROUP_RECORD_COUNT_PARAM = "groupRecords";

    /**
     * Key to the set used for looking up records associated with school attendance
     */
    public static final String SCHOOL_LOOKUP_PARAM = "schoolLookup";

    /**
     * Sort order value
     */
    public static final String SORT_ORDER_PARAM = "sort";

    /**
     * First date in the report date range
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Sort order value constant for the name sort order
     */
    private static final int NAME_SORT_ORDER = 0;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

        List staffAttendance = new ArrayList(0);
        Integer groupRecords = Integer.valueOf(0);

        if (startDate != null && endDate != null) {
            /*
             * Determine whether or not Saturdays and Sundays count toward attendance totals and
             * build
             * a StaffAttendanceManager accordingly.
             */
            PreferenceSet preferenceSet = null;
            if (isSchoolContext()) {
                preferenceSet = PreferenceManager.getPreferenceSet(getSchool());
            } else {
                preferenceSet = PreferenceManager.getPreferenceSet(getOrganization());
            }
            Boolean includeSaturday = Boolean.valueOf(preferenceSet.getPreferenceValue(
                    SisPreferenceConstants.SFA_SATURDAY_INDICATOR));
            Boolean includeSunday = Boolean.valueOf(preferenceSet.getPreferenceValue(
                    SisPreferenceConstants.SFA_SUNDAY_INDICATOR));

            StaffAttendanceManager saManager =
                    AttendanceManagerFactory.getStaffAttendanceManager(getBroker(), includeSaturday.booleanValue(),
                            includeSunday.booleanValue());

            int sortOption = ((Integer) getParameter(SORT_ORDER_PARAM)).intValue();
            ArrayList sortOrder = new ArrayList(2);

            if (sortOption == NAME_SORT_ORDER) {
                sortOrder.add(StaffAttendanceHelper.REL_STAFF + "." + SisStaff.COL_NAME_VIEW);
                sortOrder.add(StaffAttendanceHelper.COL_DATE);
            } else {
                sortOrder.add(StaffAttendanceHelper.COL_DATE);
                sortOrder.add(StaffAttendanceHelper.REL_STAFF + "." + SisStaff.COL_NAME_VIEW);
            }


            try {
                if (isSchoolContext()) {
                    staffAttendance = saManager.getAttendanceForSchool(startDate,
                            endDate,
                            getSchool().getOid(),
                            null,
                            sortOrder);
                } else {
                    sortOrder.add(0, StaffAttendanceHelper.REL_STAFF + "." + SisStaff.COL_ATTENDANCE_GROUP);
                    List groupAttendance = saManager.getAttendanceForGroups(startDate,
                            endDate,
                            getOrganization().getOid(),
                            sortOrder);
                    staffAttendance.addAll(groupAttendance);
                    groupRecords = Integer.valueOf(groupAttendance.size());

                    sortOrder.remove(0);
                    sortOrder.add(0,
                            StaffAttendanceHelper.REL_STAFF + "." + SisStaff.REL_SCHOOL + "." + SisSchool.COL_NAME);
                    List schoolAttendance = saManager.getAttendanceForSchools(startDate,
                            endDate,
                            getOrganization().getOid(),
                            sortOrder);
                    staffAttendance.addAll(schoolAttendance);
                }
            } catch (X2BaseException xbe) {
                AppGlobals.getLog().log(Level.WARNING, xbe.getMessage(), xbe);
            }
        }

        /*
         * The number of group records is used on the report design to calculate the data grouping.
         * If the current record number is less than the number of group attendance records, then
         * the current grouping is for an attendance group. Otherwise it is for a school. This is
         * possible because all attendance group records appear first in the overall collection of
         * records.
         */
        addParameter(GROUP_RECORD_COUNT_PARAM, groupRecords);

        return new JRBeanCollectionDataSource(staffAttendance);
    }
}
