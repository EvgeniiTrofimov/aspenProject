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
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Collection;

/**
 * The Class ConsentForPlacementData.
 */
public class ConsentForPlacementData extends BaseFormReportJavaSource {

    private static final String PARAM_MEETING_DATE = "iepMeetingDate";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {

            /*
             * Get the collection of meetings from the IepData.
             * With the collection, iterate through each meeting, determining which is
             * the latest meeting not after today.
             */
            IepData iepData = (IepData) getFormOwner();
            PlainDate latestMeetingDate = null;
            PlainDate today = new PlainDate();
            Collection<IepMeeting> meetings = iepData.getIepMeeting();
            if (!meetings.isEmpty()) {
                for (IepMeeting meeting : meetings) {
                    if (latestMeetingDate == null
                            || (meeting.getDate().after(latestMeetingDate) && !meeting.getDate().after(today))) {
                        latestMeetingDate = meeting.getDate();
                    }
                }

                /*
                 * Correctly format the latestMeeting
                 */
                String iepMeetingDate = null;
                if (latestMeetingDate != null) {
                    SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
                    iepMeetingDate = dateFormat.format(latestMeetingDate);
                }

                addParameter(PARAM_MEETING_DATE, iepMeetingDate);
            }

        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(),
                getDictionary(), getLocale());
    }

}
