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

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;

/**
 * The Class SummaryOfPerformance.
 */
public class SummaryOfPerformance extends BaseFormReportJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (!isBlank()) {
            IepData iepData = (IepData) getFormOwner();

            /*
             * Retrieve and set most recent iep meeting
             */
            addParameter("mostRecentIEP", getMostRecentMeeting(iepData));

            /*
             * Retrieve and set contacts
             */
            SisStudent student = iepData.getStudent();
            if (student.getPrimaryContact() != null) {
                addParameter("primaryContact", student.getPrimaryContact().getContact());
            }
            if (student.getSecondaryContact() != null) {
                addParameter("secondaryContact", iepData.getStudent().getSecondaryContact().getContact());
            }

            /*
             * Retrieve and set the primaryDisability and secondaryDisabilities from the IepData
             */
            if (iepData.getPrimaryDisability() != null) {
                addParameter("primaryDisability", iepData.getPrimaryDisability().getDisabilityCode());
            }
            addParameter("secondaryDisabilities", iepData.getSecondaryDisabilitiesView());

            /*
             * Retrieve and set expected grad year
             */
            ExtendedDictionaryAttributes extendDictionary = iepData.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
            addParameter("yearOfGraduation", iepData.getFieldValueByAlias("iep-expected-grad-year", dictionary));
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(),
                getDictionary(), getLocale());
    }

    /**
     * Gets the most recent meeting.
     *
     * @param iepData IepData
     * @return String
     */
    /*
     * Get the collection of meetings from the IepData.
     * With the collection, iterate through each meeting, determining which is
     * the latest meeting
     */
    private String getMostRecentMeeting(IepData iepData) {
        PlainDate latestMeetingDate = null;
        for (IepMeeting meeting : iepData.getIepMeeting()) {
            if (latestMeetingDate == null || meeting.getDate().after(latestMeetingDate)) {
                latestMeetingDate = meeting.getDate();
            }
        }

        /*
         * Correctly format the latestMeeting
         */
        String mostRecentMeeting = null;
        if (latestMeetingDate != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
            mostRecentMeeting = dateFormat.format(latestMeetingDate);
        }

        return mostRecentMeeting;
    }

}
