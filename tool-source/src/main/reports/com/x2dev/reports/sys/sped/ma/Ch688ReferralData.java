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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class Ch688ReferralData.
 */
public class Ch688ReferralData extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * Report parameters
     */
    public static final String PARAM_TYPE_OF_PLACEMENT = "typeOfPlacement";
    public static final String PARAM_LEVEL_OF_NEED = "levelOfNeed";
    public static final String PARAM_TRANSITION_GRAD_DATE = "transitionGradDate";
    public static final String PARAM_GUARDIAN = "isGuardian";

    /**
     * FormOid's
     */
    protected static final String OID_LEVEL_OF_NEED = "fmdMaPL3";
    protected static final String OID_ED_ENVIRONMENT = "fmdMaPL2621";

    protected static final String EXTENDED_DICTIOANRY_MA_IEP = "SPED-MA-IEP";
    /**
     * Form Aliases
     */
    protected static final String ALIAS_LEVEL_OF_NEED = "level-of-need";
    protected static final String ALIAS_ED_ENVIRONMENT = "educational-environment";
    protected static final String ALIAS_TRANSITION_GRAD_DATE = "transition-graduation-date";
    protected static final String ALIAS_PLACEMENT_COST_AGENCY = "placement-cost-agency";

    protected static final String ALIAS_GUARDIAN = "DOE Parent Legal Guardian";

    /**
     * Instance variable
     */
    private MaSpedAttribHelper m_attribHelper;
    protected DataDictionary m_maIepDictionary;


    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        IepData iepData = (IepData) getFormOwner();
        if (iepData != null) {
            addParameter(PARAM_LEVEL_OF_NEED, getStateCode(OID_LEVEL_OF_NEED, ALIAS_LEVEL_OF_NEED, getFormOwner()));
            addParameter(PARAM_TYPE_OF_PLACEMENT,
                    getDictionaryValue(OID_ED_ENVIRONMENT, ALIAS_ED_ENVIRONMENT, getFormOwner()));
            m_maIepDictionary = DataDictionary.getDistrictDictionary(iepData.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());

            /*
             * get transition graduation date
             */
            PlainDate transitionGraduationDate = null;
            SimpleDateFormat parseDate = new SimpleDateFormat("yyyy-MM-dd");
            SimpleDateFormat formatDate = new SimpleDateFormat("MM/dd/yyyy");
            String dateString = (String) iepData.getFieldValueByAlias(ALIAS_TRANSITION_GRAD_DATE, m_maIepDictionary);
            if (dateString != null) {
                try {
                    transitionGraduationDate = new PlainDate(parseDate.parse(dateString));
                } catch (ParseException e) {
                    // do nothing
                }

                if (transitionGraduationDate != null) {
                    addParameter(PARAM_TRANSITION_GRAD_DATE, formatDate.format(transitionGraduationDate));
                }

            }
            DataDictionary dataDictionary = getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_MA_IEP);
            String plasementCostAgency =
                    (String) iepData.getFieldValueByAlias(ALIAS_PLACEMENT_COST_AGENCY, dataDictionary);
            addParameter(ALIAS_PLACEMENT_COST_AGENCY, plasementCostAgency);
            addParameter(PARAM_GUARDIAN, isPrimaryContactLegalGuardian(iepData));

        }
        return m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


    /**
     * Gets the dictionary by extended dictionary id.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    private DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        DataDictionary returnValue = null;

        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

        return returnValue;
    }


    /**
     * Gets the dictionary value.
     *
     * @param formOID String
     * @param fieldAlias String
     * @param bean X2BaseBean
     * @return String
     */
    private String getDictionaryValue(String formOID, String fieldAlias, X2BaseBean bean) {
        FormDefinition formDefinition = (FormDefinition) getBroker().getBeanByOid(FormDefinition.class, formOID);
        ExtendedDataDictionary extendedDataDictionary = formDefinition.getExtendedDataDictionary();
        DataDictionary dataDictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

        return (String) bean.getFieldValueByAlias(fieldAlias, dataDictionary);
    }

    /**
     * Determine if the students primary contact is a legal guardian.
     * Without a firm definition
     *
     * @param iepData
     * @return
     */
    private String isPrimaryContactLegalGuardian(IepData iepData) {
        String result = "false";
        Student student = iepData.getStudent();
        StudentContact contact = student.getPrimaryContact(getBroker());
        if (contact != null) {
            DataDictionaryField field1 = m_maIepDictionary.findDataDictionaryFieldByAlias(ALIAS_GUARDIAN);
            if (field1 != null) {
                result = "true";
                Object value = contact.getFieldValueByBeanPath(field1.getJavaName());
                if (value instanceof Boolean) {
                    result = Boolean.toString((Boolean) value);
                } else if (!BooleanAsStringConverter.TRUE.equals(value)) {
                    result = "false";
                }
            } else {
                result = Boolean.toString(contact.getLivesWithIndicator());
            }
        }
        return result;
    }

    /**
     * Gets the state code.
     *
     * @param formOID String
     * @param fieldAlias String
     * @param bean X2BaseBean
     * @return String
     */
    private String getStateCode(String formOID, String fieldAlias, X2BaseBean bean) {
        String returnValue = null;
        FormDefinition formDefinition = (FormDefinition) getBroker().getBeanByOid(FormDefinition.class, formOID);
        ExtendedDataDictionary extendedDataDictionary = formDefinition.getExtendedDataDictionary();
        DataDictionary dataDictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

        String fieldValue = (String) bean.getFieldValueByAlias(fieldAlias, dataDictionary);
        if (fieldValue != null) {
            DataDictionaryField dataDictionaryField = dataDictionary.findDataDictionaryFieldByAlias(fieldAlias);
            Map<String, ReferenceCode> referenceCodeMap = dataDictionaryField.getReferenceTable().getCodeMap();
            returnValue = referenceCodeMap.get(fieldValue).getStateCode();
        }
        return returnValue;
    }
}
