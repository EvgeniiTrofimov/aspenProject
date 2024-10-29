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

import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
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
public class Decision extends BeanReport {

    private static final String ALIAS_PG_NAME = "pg-name";
    private static final String ALIAS_SPS = "no-of-de-sps";
    private static final String ALIAS_PWS = "no-of-de-pws";

    private static final String DEFAULT_PG_NAME = "No Contact";

    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        GenericFormData data = (GenericFormData) getFormStorage();

        if (data.getOid() != null) {
            String stdContactID = (String) data.getFieldValueByAlias(ALIAS_PG_NAME, getDictionary());
            ReferenceCode spsDirectorRefCode = getReferenceCodeByAlias(data, ALIAS_SPS);

            Contact stdContact = (Contact) getBroker().getBeanByOid(Contact.class, stdContactID);
            String pwsOid = (String) data.getFieldValueByAlias(ALIAS_PWS, getDictionary());
            Staff staffWillSign = (Staff) getBroker().getBeanByOid(Staff.class, pwsOid);

            addParameter(ALIAS_PG_NAME, stdContact == null ? DEFAULT_PG_NAME : stdContact.getPerson().getNameView());
            addParameter(ALIAS_PWS, staffWillSign);

            addParameter(ALIAS_SPS, spsDirectorRefCode);
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Gets the reference code by alias.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return Reference code
     */
    private ReferenceCode getReferenceCodeByAlias(X2BaseBean bean, String alias) {
        ReferenceCode refCode = null;
        String beanValue = (String) bean.getFieldValueByAlias(alias, getDictionary());
        if (!StringUtils.isEmpty(beanValue)) {
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                ReferenceTable refTable = field.getReferenceTable();
                if (refTable != null) {
                    for (ReferenceCode code : refTable.getReferenceCodes()) {
                        if (code.getCode().equals(beanValue)) {
                            refCode = code;
                            break;
                        }
                    }
                }

            }

        }

        return refCode;
    }
}
