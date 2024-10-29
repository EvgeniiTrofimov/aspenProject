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
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.procedures.sys.sped.il.ModifyTemplateProcedure.Strategy;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class ConsentForEvaluationProcedure57bc.
 */
public class ConsentForEvaluationProcedure57bc implements DynamicFormProcedure {
    private static final String ALIAS_EVALUATION_DATA_NEEDED = "add-evdata";
    private static final String ALIAS_INFORMATION_ABOUT_CHILD = "exist-inf";
    private static final String ALIAS_IS_RELEVANT = "is-relevant";
    private static final String ALIAS_SOURCES_DATA = "source";
    private static final String ALIAS_DOMAIN_TYPES = "domain-types";


    // change to enum



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

        DataDictionary dataDictionary = detail.getDataDictionary();
        ModifyTemplateProcedure modiFyTemplate = new ModifyTemplateProcedure(userData, Strategy.STRATEGY_RTB, "",
                new HashMap<String, Object>(), ALIAS_DOMAIN_TYPES, dataDictionary);
        EmbeddedListDetailSet embeddedDetail = ModifyTemplateProcedure.findEmbeddedListByName(detail, key);
        modiFyTemplate.createNewEmbaddedChild(embeddedDetail);
        Map<String, Object> returnMap = new HashMap<String, Object>();
        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
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
        DataDictionary parentDataDictionary = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();
        String value = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary, ALIAS_IS_RELEVANT);

        if (value != null && value.equals(Boolean.TRUE.toString())) {

            String childInformation = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary,
                    ALIAS_INFORMATION_ABOUT_CHILD);
            String evaluationDataNeeded = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary,
                    ALIAS_EVALUATION_DATA_NEEDED);
            String sourcesData =
                    (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary, ALIAS_SOURCES_DATA);

            if (StringUtils.isEmpty(childInformation) || StringUtils.isEmpty(evaluationDataNeeded)
                    || StringUtils.isEmpty(sourcesData)) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "Please, complete all relevant text boxes \""));
            }
        }

        return errors;
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
