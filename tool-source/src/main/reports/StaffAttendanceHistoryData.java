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

import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.sis.model.business.attendance.StaffAttendanceHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Level;
import net.sf.jasperreports3.engine.JRDataSource;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * Prepares the data for the "Staff Attendance History" report. This report lists the staff
 * attendance for a single staff member for the entered date range.
 *
 * @author X2 Development Corporation
 */
public class StaffAttendanceHistoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Last date in the report date range
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * First date in the report date range
     */
    public static final String START_DATE_PARAM = "startDate";

    private SisStaff m_staffMember;

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

        Collection staffAttendance = new ArrayList();
        if (m_staffMember != null && startDate != null && endDate != null) {

            StaffAttendanceManager saManager =
                    AttendanceManagerFactory.getStaffAttendanceManager(getBroker(), false, false);

            ArrayList sortOrder = new ArrayList();
            sortOrder.add(StaffAttendanceHelper.COL_DATE);

            try {
                staffAttendance =
                        saManager.getAttendanceForStaff(startDate, endDate, m_staffMember.getOid(), sortOrder);
            } catch (X2BaseException xbe) {
                AppGlobals.getLog().log(Level.WARNING, xbe.getMessage(), xbe);
            }
        }

        return new JRBeanCollectionDataSource(staffAttendance);
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_staffMember = userData.getCurrentRecord(SisStaff.class);
    }
}
