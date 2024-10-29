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
import com.x2dev.sis.model.beans.SisPerson;

/**
 * The Class DetermineEligibility.
 */
public class DetermineEligibility extends BaseFormReportJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        if (!isBlank()) {
            /*
             * Retrieve IepData and its data dictionary
             */
            IepData iepData = (IepData) getFormOwner();
            addParameter("iepData", iepData);

            ExtendedDictionaryAttributes extendedDictionaryAttributes = iepData.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendedDictionaryAttributes, getBroker().getPersistenceKey());
            addParameter("iepDataDictionary", dictionary);

            /*
             * Retrieve the contact information for the Case Mangaer of the Iep.
             */
            if (iepData.getStaff() != null &&
                    iepData.getStaff().getPerson() != null) {
                SisPerson person = iepData.getStaff().getPerson();
                addParameter("contactName", person.getNameView());
                addParameter("contactTitle", "Case Manager");
                addParameter("contact", person.getPhone01());
            }
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(),
                getDictionary(), getLocale());
    }
}
