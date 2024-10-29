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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import java.sql.Date;
import java.util.List;

/**
 * The Class ParentalConsentReport.
 */
public class ParentalConsentReport extends BaseFormReportJavaSource {

    /*
     * Parameters
     */
    private final String PARAM_CONTACT = "contact";
    private final String PARAM_IEPDATA = "iepData";
    private final String PARAM_ORGANIZATION = "organization";
    private final String PARAM_STUDENT = "student";

    /*
     * Fields
     */
    private final String FIELD_CONSENT1 = "InformedConsent1";
    private final String FIELD_CONSENT2 = "InformedConsent2";
    private final String FIELD_CONSENT3 = "InformedConsent3";
    private final String FIELD_CONSENT4 = "InformedConsent4";
    private final String FIELD_CONSENT5 = "InformedConsent5";
    private final String FIELD_PERMISSION1 = "Permission1";
    private final String FIELD_PERMISSION2 = "Permission2";
    private final String FIELD_CONSENT_DATE = "ConsentDate";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();

        Contact contact = null;
        List<StudentContact> contacts = SpedUtils.getStudentContacts(iepData, getBroker(), 1);
        if (contacts.size() > 0) {
        	contact = contacts.get(0).getContact();
        }

        ReportDataGrid grid = new ReportDataGrid();

        addParameter(PARAM_IEPDATA, iepData);
        addParameter(PARAM_ORGANIZATION, getOrganization());
        addParameter(PARAM_STUDENT, student);
        addParameter(PARAM_CONTACT, contact);

        grid.append();
        grid.set(FIELD_CONSENT1, formData.getFieldValueByAlias(FIELD_CONSENT1, dictionary));
        grid.set(FIELD_CONSENT2, formData.getFieldValueByAlias(FIELD_CONSENT2, dictionary));
        grid.set(FIELD_CONSENT3, formData.getFieldValueByAlias(FIELD_CONSENT3, dictionary));
        grid.set(FIELD_CONSENT4, formData.getFieldValueByAlias(FIELD_CONSENT4, dictionary));
        grid.set(FIELD_CONSENT5, formData.getFieldValueByAlias(FIELD_CONSENT5, dictionary));
        grid.set(FIELD_PERMISSION1, formData.getFieldValueByAlias(FIELD_PERMISSION1, dictionary));
        grid.set(FIELD_PERMISSION2, formData.getFieldValueByAlias(FIELD_PERMISSION2, dictionary));
        String consentDateAsString = (String) formData.getFieldValueByAlias(FIELD_CONSENT_DATE, dictionary);
        if (!StringUtils.isEmpty(consentDateAsString)) {
            grid.set(FIELD_CONSENT_DATE, Date.valueOf(consentDateAsString));
        } else {
            grid.set(FIELD_CONSENT_DATE, null);
        }

        grid.beforeTop();
        return grid;
    }

}
