/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

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
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.reports.AttendanceReport;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Attendance History" report.
 *
 * @author X2 Development Corporation
 */
public class AttendanceHistoryData extends AttendanceReport {

    /**
     * iReport Boolean parameter if `Second Daily Attendance` is turned on for app
     */
    private static final String PARAM_SECOND_DAILY_ATTENDANCE = "secondDailyAttendance";

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria, false);

        applySort(query);

        Boolean secondDailyAttendance = Boolean.FALSE;
        if (isSchoolContext()) {
            secondDailyAttendance = Boolean.valueOf(PreferenceManager.getPreferenceValue(getSchool(),
                    SisPreferenceConstants.ATT_SECOND_DAILY_ATTENDANCE));
        }
        addParameter(PARAM_SECOND_DAILY_ATTENDANCE, secondDailyAttendance);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Gets the report type.
     *
     * @return Report type
     * @see com.follett.fsc.core.k12.tools.reports.AttendanceReport#getReportType()
     */
    @Override
    protected ReportType getReportType() {
        return ReportType.MULTI_DAY;
    }
}
