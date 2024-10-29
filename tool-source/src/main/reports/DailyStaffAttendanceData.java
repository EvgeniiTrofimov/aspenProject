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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.sis.model.business.attendance.StaffAttendanceHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import net.sf.jasperreports3.engine.JRDataSource;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * Prepares the data for the "Daily Staff Attendance" report, which lists the staff attendance
 * records for a single day.
 *
 * @author X2 Development Corporation
 */
public class DailyStaffAttendanceData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter for the attendance date. This value is a PlainDate object.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Number of records on the report to be displayed in an attendance group grouping
     */
    public static final String GROUP_RECORD_COUNT_PARAM = "groupRecords";

    /**
     * Report parameter for the sort order. This value is a String.
     */
    public static final String SORT_ORDER_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate reportDate = (PlainDate) getParameter(DATE_PARAM);

        Collection staffAttendance = new ArrayList();
        Integer groupRecords = Integer.valueOf(0);

        if (reportDate != null) {
            StaffAttendanceManager saManager =
                    AttendanceManagerFactory.getStaffAttendanceManager(getBroker(), false, false);

            ArrayList<String> sortOrder =
                    StringUtils.convertDelimitedStringToList((String) getParameter(SORT_ORDER_PARAM), ',', true);

            staffAttendance = new ArrayList(0);
            try {
                if (isSchoolContext()) {
                    sortOrder.add(0, StaffAttendanceHelper.REL_STAFF + PATH_DELIMITER + SisStaff.REL_SCHOOL +
                            PATH_DELIMITER + SisSchool.COL_NAME);
                    staffAttendance =
                            saManager.getAttendanceForSchool(reportDate, reportDate, getSchool().getOid(), null,
                                    sortOrder);
                } else {
                    sortOrder.add(0, StaffAttendanceHelper.REL_STAFF + PATH_DELIMITER + SisStaff.COL_ATTENDANCE_GROUP);
                    List groupAttendance = saManager.getAttendanceForGroups(reportDate,
                            reportDate,
                            getOrganization().getOid(),
                            sortOrder);
                    staffAttendance.addAll(groupAttendance);
                    groupRecords = Integer.valueOf(groupAttendance.size());

                    sortOrder.remove(0);
                    sortOrder.add(0, StaffAttendanceHelper.REL_STAFF + PATH_DELIMITER + SisStaff.REL_SCHOOL +
                            PATH_DELIMITER + SisSchool.COL_NAME);
                    List schoolAttendance = saManager.getAttendanceForSchools(reportDate,
                            reportDate,
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
