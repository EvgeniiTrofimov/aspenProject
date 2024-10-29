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
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.sped.NewHampshireAliases;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the New Hampshire New SPED Waiver of 10-day Notice of Meeting Form.
 *
 * @author X2 Development Corporation
 */

public class WaiverFormData_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parameter: Iep Meeting date (String)
     */
    private static final String PARAM_MEETING_DATE = "meetingDate";

    /**
     * Parameter: Parent who requests the waiver (Person.class)
     */
    private static final String PARAM_PARENT = "parent";

    /**
     * Parameter: Staff member who completed the waiver form (Staff.class)
     */
    private static final String PARAM_STAFF = "staff";

    /**
     * Report constants representing field aliases from the extended data dictionary.
     *
     * @return JRDataSource
     * @throws Exception exception
     */

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // Retrieve the 'more information contact' OID, then pass the corresponding bean as a
        // parameter
        String staffOid =
                (String) getFormStorage().getFieldValueByAlias(NewHampshireAliases.INFO_CONTACT_OID, getDictionary());
        SisStaff moreInfoUser = (SisStaff) getBroker().getBeanByOid(SisStaff.class, staffOid);
        if (moreInfoUser != null) {
            addParameter(PARAM_STAFF, moreInfoUser);
        }

        // Retrieve the parent OID, then pass the corresponding bean as a parameter
        String parentContactOid =
                (String) getFormStorage().getFieldValueByAlias(NewHampshireAliases.WAIVE_PARENT_OID, getDictionary());
        StudentContact parentContact =
                (StudentContact) getBroker().getBeanByOid(StudentContact.class, parentContactOid);
        if (parentContact != null) {
            Contact resolvedContact = parentContact.getContact();

            if (resolvedContact != null) {
                Person parent = parentContact.getPerson();

                if (parent != null) {
                    addParameter(PARAM_PARENT, parent);
                }
            }
        }
        // Retrieve the related meeting OID, then pass the corresponding bean as a parameter
        String meetingOid =
                (String) getFormStorage().getFieldValueByAlias(NewHampshireAliases.WAIVED_MEETING_OID, getDictionary());
        IepMeeting meeting = (IepMeeting) getBroker().getBeanByOid(IepMeeting.class, meetingOid);
        if (meeting != null) {
            addParameter(PARAM_MEETING_DATE, meeting.getDate());
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


}
