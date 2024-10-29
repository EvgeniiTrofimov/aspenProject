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
package com.x2dev.procedures.sys.sped.il;

import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class SpedTemplateTransferProcedure.
 */
public class SpedTemplateTransferProcedure implements DynamicFormProcedure {

    private static String ALIAS_PRIMARY_DISABILITY = "transfer-iep-prim-dsbl";
    private static String ALIAS_SECONDARY_DISABILITY_1 = "transfer-iep-sec-dsbl1";
    private static String ALIAS_SECONDARY_DISABILITY_2 = "transfer-iep-sec-dsbl2";



    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        return errors;
    }

    /**
     * Initialize template.
     *
     * @param template Template
     * @param applicationContext ApplicationContext
     * @param dictionary DataDictionary
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {

        // TODO Auto-generated method stub

    }

    /**
     * Modify form.
     *
     * @param detail GenericDetail
     * @param key String
     * @param value String
     * @param userData UserDataContainer
     * @param template Template
     * @param errors List
     * @return Map
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        Map<String, Object> returnMap = new HashMap<String, Object>();
        return returnMap;
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        List<String> disabilities = getFillingDisabilities(detail);
        boolean hasMatches = hasMatches(disabilities, true);
        if (hasMatches) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.RECORD_UNIQUE_BY_FIELDS),
                    "Disability codes must be unique"));
        }
        return errors;
    }

    /**
     * Checks for matches.
     *
     * @param list List<String>
     * @param excludeEmpty boolean
     * @return true, if successful
     */
    private boolean hasMatches(List<String> list, boolean excludeEmpty) {
        boolean hasMatches = false;
        List<String> uniqueList = new ArrayList<String>();
        for (String disability : list) {
            if (!excludeEmpty || !StringUtils.isEmpty(disability)) {
                if (!uniqueList.contains(disability)) {
                    uniqueList.add(disability);
                } else {
                    hasMatches = true;
                    break;
                }
            }
        }
        return hasMatches;
    }

    /**
     * Gets the filling disabilities.
     *
     * @param detail GenericDetail
     * @return List
     */
    private List<String> getFillingDisabilities(GenericDetail detail) {
        List<String> disabilities = new ArrayList<String>();
        DataDictionary parentDataDictionary = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();
        List<String> aliases = new ArrayList<String>(
                Arrays.asList(ALIAS_PRIMARY_DISABILITY, ALIAS_SECONDARY_DISABILITY_1, ALIAS_SECONDARY_DISABILITY_2));
        for (String alias : aliases) {
            String disability = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary, alias);
            if (!StringUtils.isEmpty(disability)) {
                disabilities.add(disability);
            }
        }
        return disabilities;
    }

    /**
     * Gets the value from property map.
     *
     * @param propertyMap Map<String,Object>
     * @param dictionary DataDictionary
     * @param alias String
     * @return Object
     */
    private Object getValueFromPropertyMap(Map<String, Object> propertyMap, DataDictionary dictionary, String alias) {
        Object returnValue = null;
        if (propertyMap != null && !propertyMap.isEmpty() && dictionary != null && alias != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            String fieldId = field == null ? "" : field.getId();
            returnValue = propertyMap.get(fieldId);
        }
        return returnValue;
    }


}
