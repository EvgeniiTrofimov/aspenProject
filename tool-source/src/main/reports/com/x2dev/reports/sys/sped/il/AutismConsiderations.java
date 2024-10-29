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
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
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
public class AutismConsiderations extends BeanReport {


    private static final String ALIAS_AUT_CONF_CH_FACTORS = "aut-conf-ch-factors";
    private static final String ALIAS_AUT_CONF_CH_STD_NEEDS = "aut-conf-ch-std-needs";
    private static final String ALIAS_AUT_CONF_CH_SUPPORTS = "aut-conf-ch-supports";

    private static final long serialVersionUID = 1L;

    private Map<String, Map<String, String>> m_factorsMap = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_factorsMap = new HashMap<String, Map<String, String>>();
        GenericFormData storageTable = (GenericFormData) getFormStorage();
        if (storageTable != null && storageTable.getOid() != null) {
            for (GenericFormChildData formData : storageTable.getGenericFormDataChildren()) {
                String factor = (String) formData.getFieldValueByAlias(ALIAS_AUT_CONF_CH_FACTORS, getDictionary());
                if (!StringUtils.isEmpty(factor)) {
                    Map<String, String> values = new HashMap<String, String>();
                    values.put(ALIAS_AUT_CONF_CH_STD_NEEDS,
                            (String) formData.getFieldValueByAlias(ALIAS_AUT_CONF_CH_STD_NEEDS, getDictionary()));
                    values.put(ALIAS_AUT_CONF_CH_SUPPORTS,
                            (String) formData.getFieldValueByAlias(ALIAS_AUT_CONF_CH_SUPPORTS, getDictionary()));
                    m_factorsMap.put(factor, values);
                }
            }
            if (!m_factorsMap.isEmpty()) {
                addParameter(ALIAS_AUT_CONF_CH_FACTORS, m_factorsMap);
            }
        }


        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
