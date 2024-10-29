/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.business.sped.SpedUtils;
import java.util.Collection;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the RI IEP Review of Referral form.
 *
 * @author X2 Development Corporation
 */
public class LearningDisabilitiesIdData extends BaseFormReportJavaSource {

    private static final String SUBREPORT_FORMAT_ID = "SYS-SPED-RI-REV-ASM";

    // Report parameters
    public static final String PARAM_RECIPIENT_KEY = "recipient";
    public static final String PARAM_RECIPIENT_VAL = "Parent/Guardian/Adult Student";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        SimpleFormDataSource dataSource = null;

        Collection<GenericFormChildData> children =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren(getBroker());
        if (children.isEmpty() || isBlank()) {

            GenericFormChildData blankAction =
                    X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
            children.add(blankAction);
        }

        IepData iep = (IepData) getFormOwner();

        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 1);
        if (!contacts.isEmpty()) {
            Person contact = contacts.get(0).getPerson();
            addParameter(PARAM_RECIPIENT_KEY, contact.getFirstName() + " " + contact.getLastName());
        } else {
            addParameter(PARAM_RECIPIENT_KEY, PARAM_RECIPIENT_VAL);
        }

        /*
         * Add support for the sub-report
         */
        Report teamSubreport = ReportUtils.getReport(SUBREPORT_FORMAT_ID, getBroker());

        addParameter(ReportConstants.FIELD_FORMAT, teamSubreport.getCompiledFormat());
        addParameter(ReportConstants.FIELD_DATA_SOURCE,
                new BeanCollectionDataSource(children, getDictionary(), getLocale()));
        dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        return dataSource;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        IepData iep = (IepData) getFormOwner();

        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 1);
        if (!contacts.isEmpty()) {
            Person contact = contacts.get(0).getPerson();
            addParameter(PARAM_RECIPIENT_KEY, contact.getFirstName() + " " + contact.getLastName());
        } else {
            addParameter(PARAM_RECIPIENT_KEY, PARAM_RECIPIENT_VAL);
        }
    }
}
