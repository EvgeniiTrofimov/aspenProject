
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
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
public class ParentNotificationOfConferenceFormData extends BeanReport {
    private static final String ALIAS_PG_NAME = "pg-name";
    private static final String ALIAS_PMD = "not-conf-pmd";
    private static final String ALIAS_PWS = "pws";

    private static final String NO_CONTACT = "No Contact";

    private static final long serialVersionUID = 1L;


    /**
     * @see com.x2dev.reports.sys.sped.il.BeanReport#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        GenericFormData data = (GenericFormData) getFormStorage();

        if (data.getOid() != null) {
            String stdContactID = (String) data.getFieldValueByAlias(ALIAS_PG_NAME, getDictionary());
            ReferenceCode pmd = getReferenceCodeByAlias(data, ALIAS_PMD);
            String personWillSignID = (String) data.getFieldValueByAlias(ALIAS_PWS, getDictionary());

            Contact stdContact = (Contact) getBroker().getBeanByOid(Contact.class, stdContactID);

            Staff personWillSign = (Staff) getBroker().getBeanByOid(Staff.class, personWillSignID);

            addParameter(ALIAS_PG_NAME, stdContact == null ? NO_CONTACT : stdContact.getPerson().getNameView());
            addParameter(ALIAS_PWS, personWillSign);
            addParameter(ALIAS_PMD, pmd);
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


    /**
     * Filter multiple form instance.
     *
     * @param formInstances List<FormInstance>
     * @param multipleNumber String
     * @param workflow Workflow
     * @param broker X2Broker
     * @return List
     * @see BeanReport#filterMultipleFormInstance(List, String, Workflow, X2Broker)
     * @see BeanReport#outcomeMultipleBehaviour(List, String, List, X2Broker, Workflow)
     */
    @Override
    public List<FormInstance> filterMultipleFormInstance(List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workflow,
                                                         X2Broker broker) {

        List<FormInstance> returnFormInstances = null;
        if (!StringUtils.isEmpty(multipleNumber) && StringUtils.isNumeric(multipleNumber)) {
            List<String> formDefIds = new ArrayList<String>(Arrays.asList("SPED-IL-3457D"));
            returnFormInstances = outcomeMultipleBehaviour(formInstances, multipleNumber, formDefIds, broker, workflow);
        } else {
            returnFormInstances = formInstances;
        }
        return returnFormInstances;
    }

    /**
     * return ReferenceCode by alias<br>
     * if field has attached reference table and field has value - logic try find Reference Code by
     * this field value.
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