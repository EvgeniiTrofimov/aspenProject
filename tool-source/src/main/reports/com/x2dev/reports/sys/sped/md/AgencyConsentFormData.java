/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.md;

import static com.x2dev.sis.model.business.sped.MarylandAliases.AGENCY_CONSENT_AGENCY_NAME;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the MD special ed agency consent form. Parameters agency0...n are added
 * for each agency invited.
 *
 * @author X2 Development Corporation
 */
public class AgencyConsentFormData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /**
     * Prefix used on agency name parameters.
     */
    public static final String AGENCY_PARAM_PREFIX = "agency";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        GenericFormData formData = (GenericFormData) getFormStorage();

        DataDictionaryField agencyField = getDictionary().findDataDictionaryFieldByAlias(AGENCY_CONSENT_AGENCY_NAME);
        Map<String, ReferenceCode> codeMap = agencyField.getReferenceTable().getCodeMap(getBroker());

        int agencyNumber = 0;
        for (GenericFormChildData agencyEntry : formData.getGenericFormDataChildren(getBroker())) {
            String agencyName = (String) agencyEntry.getFieldValueByAlias(AGENCY_CONSENT_AGENCY_NAME, getDictionary());
            ReferenceCode code = codeMap.get(agencyName);

            addParameter(AGENCY_PARAM_PREFIX + (agencyNumber++), code.getDescription());
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
