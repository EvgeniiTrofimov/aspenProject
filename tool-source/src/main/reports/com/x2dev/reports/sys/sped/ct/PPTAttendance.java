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
package com.x2dev.reports.sys.sped.ct;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;

/**
 * The Class PPTAttendance.
 */
public class PPTAttendance extends BaseFormReportJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        IepData iep = (IepData) getFormOwner();
        PlainDate startDate = null;
        if (iep.getIepMeeting() != null) {
            for (IepMeeting meeting : iep.getIepMeeting()) {
                if (startDate == null || meeting.getDate().after(startDate)) {
                    startDate = meeting.getDate();
                }
            }
        }

        String reportDate = null;
        if (startDate != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
            reportDate = dateFormat.format(startDate);
        }
        addParameter("reportDate", reportDate);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(),
                getDictionary(), getLocale());
    }

}
