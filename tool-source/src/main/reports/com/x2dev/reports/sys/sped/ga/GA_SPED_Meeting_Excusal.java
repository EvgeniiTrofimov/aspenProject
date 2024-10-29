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
import java.text.SimpleDateFormat;
import java.util.Collection;

/**
 * The Class GA_SPED_Meeting_Excusal.
 *
 * @author
 */
public class GA_SPED_Meeting_Excusal extends BaseFormReportJavaSource {

    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_MEETING_TIME = "meetingTime";
    private static final String PARAM_MEETING_LOCATION = "meetingLocation";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            IepData iepData = (IepData) getFormOwner();

            // find most recent meeting and set meeting parameters
            IepMeeting mostRecentMeeting = null;
            Collection<IepMeeting> meetings = iepData.getIepMeeting();
            if (!meetings.isEmpty()) {
                for (IepMeeting meeting : meetings) {
                    if (mostRecentMeeting == null || meeting.getDate().after(mostRecentMeeting.getDate())) {
                        mostRecentMeeting = meeting;
                    }
                }

                // set meeting date parameter
                String meetingDate = null;
                if (mostRecentMeeting.getDate() != null) {
                    SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
                    meetingDate = dateFormat.format(mostRecentMeeting.getDate());
                }
                addParameter(PARAM_MEETING_DATE, meetingDate);

                // set meeting time parameter
                String meetingTime = null;
                if (mostRecentMeeting.getTime() != null) {
                    SimpleDateFormat timeFormat = new SimpleDateFormat("h:mm a");
                    meetingTime = timeFormat.format(mostRecentMeeting.getTime());
                }
                addParameter(PARAM_MEETING_TIME, meetingTime);

                // set meeting location parameter
                addParameter(PARAM_MEETING_LOCATION, mostRecentMeeting.getLocation());

            }
        }

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

}
