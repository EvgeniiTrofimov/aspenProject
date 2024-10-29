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
/* DEBUG */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the RI SPED Permission to Evaluate report.
 *
 * @author X2 Development Corporation
 */
public class PriorWrittenNoticeData extends BaseFormReportJavaSource {
    // Report parameters
    public static final String PARAM_RECIPIENT_KEY = "recipient";
    public static final String PERSON = "person";
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
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        PlainDate pwnDate = null;
        if (formData.getFieldValueByAlias("pwn-date", dictionary) != null) {
            pwnDate = new PlainDate(format.parse((String) formData.getFieldValueByAlias("pwn-date", dictionary)));
        }

        FormInstance formInstance = getFormInstance();
        Date timestampDate = null;
        if (formInstance != null) {
            long timestamp = formInstance.getCreatedTime();
            timestampDate = new Date(timestamp);
        }

        addParameter("pwnDate", pwnDate);
        addParameter("formDate", timestampDate);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
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
        addParameter("studentGradeLevel", getGradeLevel(iep));
        addParameter("actionDate", iep.getStartDate());
        addParameter("primaryDisability", iep.getPrimaryDisability());
        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 1);
        if (!contacts.isEmpty()) {
            Person contact = contacts.get(0).getPerson();

            addParameter(PARAM_RECIPIENT_KEY, contact.getFirstName() + " " + contact.getLastName());
            addParameter(PERSON, contact);
        } else {
            addParameter(PARAM_RECIPIENT_KEY, PARAM_RECIPIENT_VAL);
        }
    }

    /**
     * Returns the student grade level.
     *
     * @param iep IepData
     * @return String
     */
    private String getGradeLevel(IepData iep) {
        String gradeLevel = null;
        if (StringUtils.isEmpty(gradeLevel)) {
            gradeLevel = iep.getStudent().getGradeLevel();
        }

        return gradeLevel;
    }
}
