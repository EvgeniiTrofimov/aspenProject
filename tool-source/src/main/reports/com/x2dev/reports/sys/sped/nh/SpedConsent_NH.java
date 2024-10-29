/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.business.sped.NewHampshireAliases;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;

/**
 * The Written Prior Notice consent form used in New Hampshire (three pages).
 *
 * @author X2 Development Corporation
 */
public class SpedConsent_NH extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constant: Date formatting
     */
    public static final String DATE_FORMAT = "MM/dd/yyyy";

    /**
     * Format ID: Page 1 - Team Determination
     */
    public static final String PAGE_1_FORMAT_ID = "SYS-SPED-CON1";

    /**
     * Format ID: Page 2 - Parent Response
     */
    public static final String PAGE_2_FORMAT_ID = "SYS-SPED-CON2";

    /**
     * Format ID: Page 3 - How to Dispute Guidelines (static text)
     */
    public static final String PAGE_3_FORMAT_ID = "SYS-SPED-CON3";

    /**
     * Parameter: Iep (IepData.class)
     */
    public static final String PARAM_IEP = "iepData";

    /**
     * Parameter: Iep Meeting Date (String)
     */
    public static final String PARAM_MEETING_DATE = "meetingDate";

    /**
     * Gather data.
     *
     * @return ReportDataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        /*
         * The owner of the form. This will be an IepData, since
         * the form specifies the iep_data table as the owner.
         */
        IepData iep = (IepData) getFormOwner();

        /*
         * Get the generic form data pertaining to this form
         */
        GenericFormData gfd = (GenericFormData) getFormStorage();

        /*
         * The oid of the IEP meeting pertaining to this form
         */
        String oid = (String) gfd.getFieldValueByAlias(NewHampshireAliases.WRP_RELATED_MEETING, getDictionary());

        /*
         * An IEPMeeting object pertaining to this form
         */
        IepMeeting iepm = (IepMeeting) getBroker().getBeanByOid(IepMeeting.class, oid);

        /*
         * A string that will either hold the meeting date or blank (if
         * there was no meeting date passed in)
         */
        String dateString;

        /*
         * If there was a date passed in, set dateString. Otherwise, set
         * dateString to blank
         */
        if (iepm != null) {
            /*
             * The date of the IEP meeting pertaining to this form
             */
            PlainDate date = iepm.getDate();

            /*
             * A date format with which to display the meeting date
             */
            SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);

            /*
             * Format the IEP meeting date
             */
            dateString = sdf.format(date);
        } else {
            dateString = "";
        }

        /*
         * Send the IEP and the IEP meeting date as parameters
         */
        addParameter(PARAM_IEP, iep);
        addParameter(PARAM_MEETING_DATE, dateString);

        return (ReportDataGrid) super.gatherData();
    }

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID, PAGE_2_FORMAT_ID, PAGE_3_FORMAT_ID};
    }
}
