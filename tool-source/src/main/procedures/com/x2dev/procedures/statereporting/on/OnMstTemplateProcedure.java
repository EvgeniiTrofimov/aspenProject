/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.ElementContainer;
import com.follett.fsc.core.k12.web.template.Property;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.TemplateElement;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

/**
 * The Class OnMstTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnMstTemplateProcedure implements DynamicFormProcedure {
    private static final String ALIAS_MTC_END_DATE = "all-mtc-EndDate";
    private static final String ALIAS_MTC_PRIMARY_TEACHER_SPLIT = "all-mtc-PrimaryTeacherSplit";
    private static final String BEAN_PATH_PRIMARY_MTC = "primaryStaff.nameView";
    private static final String KEY_MTC_VIEW = "mtcView";
    private static final String KEY_MTC_END_DATE = "mtc-end-date";
    private static final String PROPERTY_MTC_PRIMARY_TEACHER_INDICATOR = "mtcPrimaryTchr";
    private static final String VALUE_TRUE = "true";
    private static final String VALUE_UNDEFINED = "undefined";
    private static final String VALUE_Y = "Y";

    private ModelBroker m_broker;
    private PrivilegeSet m_privilegeSet;

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        return null;
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template,
     *      com.follett.fsc.core.k12.web.ApplicationContext,
     *      com.follett.fsc.core.k12.business.dictionary.DataDictionary,
     *      com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        m_privilegeSet = privilegeSet;

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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail,
     *      java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template, java.util.List)
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

        if (!StringUtils.isEmpty(key)) {
            if (key.equals(KEY_MTC_VIEW)) {
                PlainDate today = new PlainDate();
                PlainDate startDate = getStartDate((String) detail.getPropertyValues().get("mstTrmOID"));
                if (setPrimaryStaffReadonly(template, startDate != null && !today.before(startDate))) {
                    returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                }
            } else if (key.equals(KEY_MTC_END_DATE) && !VALUE_UNDEFINED.equals(value)) {
                DataDictionaryField fieldEndDate =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MTC_END_DATE);
                DataDictionaryField fieldIntervalPrimaryIndicator =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MTC_PRIMARY_TEACHER_SPLIT);
                if (fieldEndDate != null && fieldIntervalPrimaryIndicator != null) {
                    Map<String, Object> propertyValues = detail.getCurrentChild() == null ? detail.getPropertyValues()
                            : detail.getCurrentChild().getPropertyValues();

                    String propEndDate = (String) propertyValues.get(fieldEndDate.toString());
                    String propPrimaryIndicator = (String) propertyValues.get(PROPERTY_MTC_PRIMARY_TEACHER_INDICATOR);
                    if (StringUtils.isEmpty(propEndDate) && VALUE_TRUE.equals(propPrimaryIndicator)) {
                        Map<String, Object> prop = new HashMap<String, Object>();
                        prop.put(fieldIntervalPrimaryIndicator.toString(), VALUE_Y);
                        returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
                    }
                }
            }
        }
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        return null;
    }

    /**
     * Gets the broker.
     *
     * @return Model broker
     */
    private ModelBroker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(m_privilegeSet);
        }

        return m_broker;
    }

    /**
     * Gets the primary staff element.
     *
     * @param children List<? extends TemplateElement>
     * @return Property
     */
    private Property getPrimaryStaffElement(List<? extends TemplateElement> children) {
        Property primaryStaffElement = null;
        for (TemplateElement child : children) {
            if (child instanceof Property) {
                Property property = (Property) child;
                if (BEAN_PATH_PRIMARY_MTC.equals(property.getModelProperty().getBeanPath())) {
                    primaryStaffElement = property;
                    break;
                }
            } else if (child instanceof ElementContainer) {
                ElementContainer container = (ElementContainer) child;
                primaryStaffElement = getPrimaryStaffElement(container.getChildren());
                if (primaryStaffElement != null) {
                    break;
                }
            }
        }
        return primaryStaffElement;
    }

    /**
     * Gets the start date.
     *
     * @param oid String
     * @return Plain date
     */
    private PlainDate getStartDate(String oid) {
        PlainDate startDate = null;
        if (!StringUtils.isEmpty(oid)) {
            ScheduleTerm trm = getBroker().getBeanByOid(ScheduleTerm.class, oid);
            if (trm != null) {
                for (ScheduleTermDate tmd : trm.getScheduleTermDates()) {
                    if (startDate == null || startDate.after(tmd.getStartDate())) {
                        startDate = tmd.getStartDate();
                    }
                }
            }
        }
        // DateAsStringConverter converter =
        // (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
        // Locale.getDefault(),
        // true);
        // startDate = (PlainDate) converter.parseSystemString("2019-09-01");
        return startDate;
    }

    /**
     * Sets the primary staff readonly.
     *
     * @param template Template
     * @param readonly boolean
     * @return true, if successful
     */
    private boolean setPrimaryStaffReadonly(Template template, boolean readonly) {
        boolean updated = false;
        Property primaryStaffElement = getPrimaryStaffElement(template.getChildren());
        if (primaryStaffElement != null && primaryStaffElement.getReadOnlyProperty() != readonly) {
            primaryStaffElement.setReadOnly(readonly);
            updated = true;
        }
        return updated;
    }
}
