/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class DelegationOfRightsEdDecisions3457K extends BaseFormReportJavaSource {

    private static final String ALIAS_ED_DEC_PARENT_NAME = "ed-dec-parent-name";

    private static final String PARAM_DATE_OF_MAJORITY = "majority";
    private static final String PARAM_PG_NAME = "pg-name";
    private static final String PARAM_PG_RELATIONSHIP = "pg-relationship";
    private static final String PARAM_SIGNED_DATE = "signed-date";

    private SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        IepData iepData = (IepData) getFormOwner();
        if (iepData != null) {
            GregorianCalendar gregorianCalendar = new GregorianCalendar();
            gregorianCalendar.setTime(iepData.getStudent().getPerson().getDob());
            gregorianCalendar.add(Calendar.YEAR, 18);
            addParameter(PARAM_DATE_OF_MAJORITY, dateFormat.format(gregorianCalendar.getTime()));

            String iepTeamMemberOid = (String) iepData.getFieldValueByAlias(ALIAS_ED_DEC_PARENT_NAME, getDictionary());
            IepTeamMember iepTeamMember =
                    (IepTeamMember) getBroker().getBeanByOid(IepTeamMember.class, iepTeamMemberOid);
            if (iepTeamMember != null) {
                Person person = iepTeamMember.getPerson();
                if (person != null) {
                    addParameter(PARAM_PG_NAME, person.getNameView());
                    addParameter(PARAM_PG_RELATIONSHIP, iepTeamMember.getMemberRoleCode());
                }
            }
            if (iepData.getSignedDate() != null) {
                addParameter(PARAM_SIGNED_DATE, dateFormat.format(iepData.getSignedDate()));
            }
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
