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
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.StringUtils;
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
public class ConferenceRecommendations extends BeanReport {

    private static final String ALIAS_LOC = "location";
    private static final String ALIAS_OTHER_SCHOOL = "other-school";
    private static final String ALIAS_PG_NAME = "pg-name";
    private static final String ALIAS_PMD = "not-of-conf-pmd";
    private static final String ALIAS_PWS = "pws";

    private static final String DEFAULT_CODE_LOCATION = "No Location";
    private static final String DEFAULT_PG_NAME = "No Contact";

    private static final String OTHER_SCHOOL = "Other";

    private static final String PARAM_ELIG_DETERMIN = "elig-determin";
    private static final String P_POSTFIX = "(P)";

    private Map<String, ReferenceCode> m_conductAction;
    private static final long serialVersionUID = 1L;
    private IlSpedHelper m_helper = null;

    /**
     * @see com.x2dev.reports.sys.sped.il.BeanReport#filterMultipleFormInstance(java.util.List,
     *      java.lang.String, com.follett.fsc.core.k12.beans.Workflow,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public List<FormInstance> filterMultipleFormInstance(List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workflow,
                                                         X2Broker broker) {

        List<FormInstance> returnFormInstances = new ArrayList<FormInstance>(formInstances);
        return returnFormInstances;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_helper = new IlSpedHelper();
        m_helper.initializeHelper(getBroker(), getDictionary());
        GenericFormData data = (GenericFormData) getFormStorage();
        IepData iepData = (IepData) getFormOwner();
        if (data != null && iepData != null) {


            loadConductActionCodes();


            String stdContactID = (String) data.getFieldValueByAlias(ALIAS_PG_NAME, getDictionary());

            String personWillSignID = (String) data.getFieldValueByAlias(ALIAS_PWS, getDictionary());
            String codeLocation = (String) data.getFieldValueByAlias(ALIAS_LOC, getDictionary());
            String otherSchool = (String) data.getFieldValueByAlias(ALIAS_OTHER_SCHOOL, getDictionary());
            Contact stdContact = (Contact) getBroker().getBeanByOid(Contact.class, stdContactID);

            Staff personWillSign = (Staff) getBroker().getBeanByOid(Staff.class, personWillSignID);
            addParameter(ALIAS_PG_NAME, stdContact == null ? DEFAULT_PG_NAME : stdContact.getPerson().getNameView());
            addParameter(ALIAS_PMD, getReferenceCodeByAlias(data, ALIAS_PMD));
            addParameter(ALIAS_PWS, personWillSign);
            addParameter(ALIAS_LOC,
                    StringUtils.isEmpty(codeLocation) ? DEFAULT_CODE_LOCATION
                            : (codeLocation.equals(OTHER_SCHOOL) ? otherSchool
                                    : (m_conductAction.get(codeLocation) == null ? codeLocation
                                            : m_conductAction.get(codeLocation).getDescription())));
            addParameter(PARAM_ELIG_DETERMIN, getDisabilities(iepData));

        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * initialize m_conductAction map.
     */
    protected void loadConductActionCodes() {
        DataDictionaryField dictionaryField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_LOC);

        ReferenceTable refTable = dictionaryField.getReferenceTable();
        if (refTable == null) {
            m_conductAction = new HashMap<String, ReferenceCode>();
        } else {
            m_conductAction = refTable.getCodeMap(getBroker());
        }
    }


    /**
     * Gets the disabilities.
     *
     * @param iepData IepData
     * @return String
     */
    private String getDisabilities(IepData iepData) {
        StringBuilder builder = new StringBuilder();
        Map<String, String> disabilityMap = m_helper.getDisabilities(iepData, false);
        String primaryDisabity = disabilityMap.get(IlSpedHelper.DSBL_KEY_PRIMARY_NAME);
        String secondaryDisabilities = disabilityMap.get(IlSpedHelper.DSBL_KEY_SECONDARY_NAMES);
        if (!StringUtils.isEmpty(primaryDisabity)) {
            builder.append(primaryDisabity);
            builder.append(P_POSTFIX);
            if (!StringUtils.isEmpty(secondaryDisabilities)) {
                builder.append(IlSpedHelper.COMMA + IlSpedHelper.SPACE);
            }
        }
        if (!StringUtils.isEmpty(secondaryDisabilities)) {
            builder.append(secondaryDisabilities);
        }

        return builder.toString();
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
