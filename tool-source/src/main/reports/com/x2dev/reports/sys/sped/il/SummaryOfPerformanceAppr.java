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

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
public class SummaryOfPerformanceAppr extends BaseFormReportJavaSource {
    private static final String COLUMN_ACCOMOD_SUPPORTS = "spappr-chld-accomod-sup";
    private static final String COLUMN_EFFECTIVE = "spappr-chld-effective";
    private static final String COLUMN_NOT_EFFECTIVE = "spappr-chld-not-effective";

    private static final String PARAM_ACCOMOD_SUPPORTS = "accomodSupportsData";


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

        List<Map<String, Object>> accomodRecordsList = new ArrayList<Map<String, Object>>();

        for (GenericFormChildData recordData : formData.getGenericFormDataChildren()) {
            Map<String, Object> fieldValueMap = new HashMap<String, Object>();

            String accommodSupValue =
                    (String) recordData.getFieldValueByAlias(COLUMN_ACCOMOD_SUPPORTS, getDictionary());
            fieldValueMap.put(COLUMN_ACCOMOD_SUPPORTS, accommodSupValue);

            String effectiveValue = (String) recordData.getFieldValueByAlias(COLUMN_EFFECTIVE, getDictionary());
            fieldValueMap.put(COLUMN_EFFECTIVE, effectiveValue);

            String notEffectiveValue = (String) recordData.getFieldValueByAlias(COLUMN_NOT_EFFECTIVE, getDictionary());
            fieldValueMap.put(COLUMN_NOT_EFFECTIVE, notEffectiveValue);

            accomodRecordsList.add(fieldValueMap);
        }
        addParameter(PARAM_ACCOMOD_SUPPORTS, accomodRecordsList);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

}
