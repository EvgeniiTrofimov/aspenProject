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

import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
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
public class Assessment extends BeanReport {

    private static final String ALIAS_ACCOMM_ASSMT_SECTION = "accomm-assmt-section";
    private static final String CARRIAGE = "\r\n";
    private static final String EMPTY = "";
    private static final String KEY_DESCR_MAP = "descrMap";
    private static final long serialVersionUID = 1L;

    /**
     * @see com.x2dev.reports.sys.sped.il.BeanReport#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        IepData iepData = (IepData) getFormStorage();
        fillAssessmentAccommodations(iepData);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Fill assessment accommodations.
     *
     * @param iepData IepData
     */
    private void fillAssessmentAccommodations(IepData iepData) {
        Map<String, StringBuilder> descrBuilder = new HashMap<String, StringBuilder>();
        Map<String, String> descrString = new HashMap<String, String>();
        for (IepAccommodation iepAccommodation : iepData.getAccommodations()) {
            String section =
                    (String) iepAccommodation.getFieldValueByAlias(ALIAS_ACCOMM_ASSMT_SECTION, getDictionary());
            if (section != null && !section.isEmpty()) {
                if (!descrBuilder.containsKey(section)) {
                    descrBuilder.put(section, new StringBuilder());
                }
                String description = StringUtils.isEmpty(iepAccommodation.getDescription()) ? EMPTY + CARRIAGE
                        : iepAccommodation.getDescription() + CARRIAGE;
                descrBuilder.get(section).append(description);
            }
        }
        for (Entry<String, StringBuilder> entry : descrBuilder.entrySet()) {
            descrString.put(entry.getKey(), entry.getValue().toString());
        }
        addParameter(KEY_DESCR_MAP, descrString);
    }
}
