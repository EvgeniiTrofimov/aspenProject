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
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.struts.action.ActionErrors;

/**
 * The Class PlacementValidate.
 */
public class PlacementValidate implements DynamicFormProcedure {

    private static final String ALIAS_SERVICE_NY_INITIATION_DATE = "service-ny-initiation-date";
    private static final String ALIAS_SERVICE_CY_DURATION_DATE = "service-cy-duration-date";
    private static final String ALIAS_SERVICE_LOCATION_REGULAR = "service-location-regular";
    private static final String EMPTY = "";
    private static final String TEMPLATE_ID_IEPFORM_SERVICE_REL = "iepform.service.rel";
    private static final String TEMPLATE_ID_IEPFORM_SERVICE_SPED = "iepform.service.sped";
    private static final String SERVICE_NAME_RELATED_SERVICES = "Related Services";
    private static final String SERVICE_NAME_SPECIAL_EDUCATION = "Special Education";

    protected SimpleDateFormat m_formatterFrom = new SimpleDateFormat("yyyy-MM-dd");

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
        // TODO Auto-generated method stub
        return null;
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
        validateGeneralEducationRadioButton(errors, detail);
        validateSpanDates(errors, detail);

        return errors;
    }

    /**
     * Validate general education radio button.
     *
     * @param errors List<ValidationError>
     * @param detail GenericDetail
     */
    private void validateGeneralEducationRadioButton(List<ValidationError> errors, GenericDetail detail) {
        Template template = detail.getTemplates().size() == 1 ? detail.getTemplates().get(0) : null;
        String templateName = template == null ? EMPTY : template.getName();

        String serviceMode = null;
        if (templateName.equals(TEMPLATE_ID_IEPFORM_SERVICE_SPED)) {
            serviceMode = SERVICE_NAME_SPECIAL_EDUCATION;
        } else if (templateName.equals(TEMPLATE_ID_IEPFORM_SERVICE_REL)) {
            serviceMode = SERVICE_NAME_RELATED_SERVICES;
        }



        if (serviceMode != null) {
            DataDictionary dataDictionary = detail.getDataDictionary();
            Map<String, Object> propertyValues = detail.getPropertyValues();

            String locationValue =
                    (String) getValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_SERVICE_LOCATION_REGULAR);

            if (locationValue == null || locationValue.isEmpty()) {

                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "\"Within Gen Ed\" or \"Outside Gen Ed\" must be selected"));
            }
        }
    }

    /**
     * Validate span dates.
     *
     * @param errors List<ValidationError>
     * @param detail GenericDetail
     */
    private void validateSpanDates(List<ValidationError> errors, GenericDetail detail) {
        DataDictionary ddx = detail.getDataDictionary();

        DataDictionaryField fieldCYinitiation =
                ddx.findDataDictionaryField(IepService.class.getName(), IepService.COL_START_DATE);
        DataDictionaryField fieldCYduration = ddx.findDataDictionaryFieldByAlias(ALIAS_SERVICE_CY_DURATION_DATE);
        DataDictionaryField fieldNYinitiation = ddx.findDataDictionaryFieldByAlias(ALIAS_SERVICE_NY_INITIATION_DATE);
        DataDictionaryField fieldNYduration =
                ddx.findDataDictionaryField(IepService.class.getName(), IepService.COL_END_DATE);

        PlainDate cyInitiation = getPlainDateByDataDictionaryField(fieldCYinitiation, detail);
        PlainDate cyDuration = getPlainDateByDataDictionaryField(fieldCYduration, detail);
        PlainDate nyInitiation = getPlainDateByDataDictionaryField(fieldNYinitiation, detail);
        PlainDate nyDuration = getPlainDateByDataDictionaryField(fieldNYduration, detail);

        if (cyInitiation == null && cyDuration == null && nyInitiation == null && nyDuration == null) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                    "\"Current Year\" and/or \"Next Year\" span must be populated."));
        } else {
            if ((cyInitiation != null || cyDuration != null) && (cyInitiation == null || cyDuration == null)) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "\"Current Year\" span must be populated completely. Initiation and duration dates."));
            }
            if ((nyInitiation != null || nyDuration != null) && (nyInitiation == null || nyDuration == null)) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "\"Next Year\" span must be populated completely. Initiation and duration dates."));
            }

        }
        if (cyInitiation != null && cyDuration != null && cyInitiation.after(cyDuration)) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.DATES_IN_ORDER),
                    "\"Current Year\" span. The end date cannot be prior to the start date."));
        }
        if (nyInitiation != null && nyDuration != null && nyInitiation.after(nyDuration)) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.DATES_IN_ORDER),
                    "\"Next Year\" span. The end date cannot be prior to the start date."));
        }



    }

    /**
     * Gets the plain date by data dictionary field.
     *
     * @param ddxField DataDictionaryField
     * @param detail GenericDetail
     * @return Plain date
     */
    private PlainDate getPlainDateByDataDictionaryField(DataDictionaryField ddxField, GenericDetail detail) {
        PlainDate date = null;
        Map<String, String> propValues = detail.getPropertyValues();
        String propValue = propValues.get(ddxField.getId());
        if (!StringUtils.isEmpty(propValue)) {
            Object value = GenericDetail.getTypedValue(ddxField, propValue, Locale.getDefault(), new ActionErrors());
            if (value instanceof PlainDate) {
                date = (PlainDate) value;
            } else if (value instanceof String) {
                try {
                    date = new PlainDate(m_formatterFrom.parse((String) value));
                } catch (ParseException e) {
                    // nothing to do
                }
            }
        }
        return date;
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
            String fieldId = field == null ? EMPTY : field.getId();
            returnValue = propertyMap.get(fieldId);
        }
        return returnValue;
    }

}
