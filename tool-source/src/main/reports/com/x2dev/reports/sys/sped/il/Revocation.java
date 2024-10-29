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
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepTeamMember;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

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
public class Revocation extends BaseFormReportJavaSource {

    private static final String ALIAS_PMD = "pmd";

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

        GenericFormData data = (GenericFormData) getFormStorage();

        if (data.getOid() != null) {
            String personMakeDecisionID = (String) data.getFieldValueByAlias(ALIAS_PMD, getDictionary());

            IepTeamMember teamMember =
                    (IepTeamMember) getBroker().getBeanByOid(IepTeamMember.class, personMakeDecisionID);
            Staff personMakeDecision = null;
            if (teamMember != null) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(Staff.COL_PERSON_OID, teamMember.getPersonOid());
                personMakeDecision = (Staff) getBroker().getBeanByQuery(new QueryByCriteria(Staff.class, criteria));
            }

            addParameter(ALIAS_PMD, personMakeDecision);
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
