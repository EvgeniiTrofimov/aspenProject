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
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
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
public class EligibilityDeterminationAllDisibilities extends BeanReport {
    /**
     *
     */
    private static final String PARAM_DISABILITY_CODES = "disCodes";
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
        IepData iepData = (IepData) getFormStorage();
        List<String> disabilityCodes = new ArrayList<String>();
        Map<String, String> dsblMap = m_ilSpedHelper.getDisabilities(iepData, true);
        if (!StringUtils.isEmpty(dsblMap.get("primaryCode"))) {
            disabilityCodes.add(dsblMap.get("primaryCode") + Boolean.TRUE.toString());
        }
        if (!StringUtils.isEmpty(dsblMap.get("secondaryCodes"))) {
            disabilityCodes.add(dsblMap.get("secondaryCodes") + Boolean.FALSE.toString());
        }

        addParameter(PARAM_DISABILITY_CODES, disabilityCodes);
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
