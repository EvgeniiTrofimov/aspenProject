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
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.HashMap;
import java.util.Map;
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
public class ExchangeConfInf extends BaseFormReportJavaSource {

    private static final String ALIAS_PG_NAME = "pg-name";

    private final String PARAM_STUDENT = "student";

    private final String PARAM_HOME_PHONE = "homePhone";
    private final String PARAM_WORK_PHONE = "workPhone";

    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
        IepData iepData = (IepData) getFormOwner();
        if (iepData != null) {
            SisStudent student = iepData.getStudent();
            GenericFormData data = (GenericFormData) getFormStorage();

            if (data.getOid() != null) {
                String teamMemberID = (String) data.getFieldValueByAlias(ALIAS_PG_NAME, getDictionary());
                IepTeamMember teamMember = (IepTeamMember) getBroker().getBeanByOid(IepTeamMember.class, teamMemberID);
                Map<String, Object> parentInformation = new HashMap<String, Object>();
                if (teamMember != null) {
                    m_ilSpedHelper.fillParentInformation(teamMember, parentInformation);
                }
                Object homePhone = parentInformation.get(IlSpedHelper.KEY_HOME);
                Object workPhone = parentInformation.get(IlSpedHelper.KEY_WORK);
                addParameter(PARAM_HOME_PHONE, homePhone == null ? IlSpedHelper.EMPTY : homePhone);
                addParameter(PARAM_WORK_PHONE, workPhone == null ? IlSpedHelper.EMPTY : workPhone);
            }
            addParameter(PARAM_STUDENT, student);
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
