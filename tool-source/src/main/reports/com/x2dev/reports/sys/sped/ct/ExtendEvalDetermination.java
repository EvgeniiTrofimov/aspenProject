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
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;

/**
 * The Class ExtendEvalDetermination.
 */
public class ExtendEvalDetermination extends BaseFormReportJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        if (!isBlank()) {
            /*
             * Get the name of the primaryContact from the IepData.
             */
            IepData iepData = (IepData) getFormOwner();

            SisStudent student = iepData.getStudent();
            SisPerson person = null;

            if (student.getPrimaryContact() != null) {
                person = student.getPerson();
            }
            if (person != null) {
                addParameter("contactName", person.getNameView());
            }

        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(),
                getDictionary(), getLocale());
    }

}
