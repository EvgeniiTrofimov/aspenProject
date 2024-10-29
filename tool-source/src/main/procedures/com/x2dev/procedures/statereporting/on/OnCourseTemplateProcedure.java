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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class OnCourseTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnCourseTemplateProcedure implements DynamicFormProcedure {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String ALIAS_COURSE_CODE_TYPE = "all-crs-CourseCodeType";
    private static final String ALIAS_MINISTRY_COURSE_CODE = "all-crs-MinistryCourseCode";
    private static final HashSet COURSE_CODE_TYPES = new HashSet(Arrays.asList("DCC", "LDC", "MDC"));
    private static final String FIELD_ID_COURSE_NUM = "crsCourseNum";
    private static final String KEY_COURSE_CODE_TYPE = "course-code-type";
    private static final String KEY_COURSE_NUMBER = "course-number";
    private static final String VALUE_HOMEROOM = "Homeroom";

    private X2Broker m_broker = null;

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
        m_broker = new ModelBroker(privilegeSet);
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
        Map<String, Object> prop = new HashMap<String, Object>();

        if (!StringUtils.isEmpty(key) && !StringUtils.isEmpty(value)) {
            if (key.equals(KEY_COURSE_NUMBER)) {
                ReferenceCode rcd = getReferenceCode(value, detail.getDataDictionary());
                if (rcd != null && COURSE_CODE_TYPES.contains(rcd.getDependencyCode())) {
                    String courseNum = (String) detail.getPropertyValues().get(FIELD_ID_COURSE_NUM);
                    String suffix = courseNum != null && courseNum.length() > 5 ? courseNum.substring(5) : "";
                    prop.put(FIELD_ID_COURSE_NUM, (value.length() > 5 ? value.substring(0, 5) : value) + suffix);
                    returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
                }
            } else if (key.equals(KEY_COURSE_CODE_TYPE) && VALUE_HOMEROOM.equals(value)) {
                prop.put(FIELD_ID_COURSE_NUM, VALUE_HOMEROOM);
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
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
        List<ValidationError> errors = new LinkedList<ValidationError>();
        if (GenericDetail.class.isAssignableFrom(detail.getClass())) {
            String courseCodeType =
                    getAliasValue(detail.getDataDictionary(), detail.getPropertyValues(), ALIAS_COURSE_CODE_TYPE);
            if (COURSE_CODE_TYPES.contains(courseCodeType)) {
                String ministryCourseCode = getAliasValue(detail.getDataDictionary(), detail.getPropertyValues(),
                        ALIAS_MINISTRY_COURSE_CODE);
                String matchingMinistryCode = ministryCourseCode != null && ministryCourseCode.length() > 5
                        ? ministryCourseCode.substring(0, 5)
                        : ministryCourseCode;
                String courseNum = getValue(detail.getPropertyValues(), FIELD_ID_COURSE_NUM);
                ReferenceCode rcd = getReferenceCode(ministryCourseCode, detail.getDataDictionary());
                if (rcd == null) {
                    errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                            Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                            "The ministry course code value entered \"" + ministryCourseCode
                                    + "\" must have a reference code"));
                } else if (!courseCodeType.equals(rcd.getDependencyCode())) {
                    errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                            Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                            "The value entered \"" + ministryCourseCode
                                    + "\" must have a reference code with dependency value \"" + courseCodeType
                                    + "\""));
                } else if (!courseNum.startsWith(matchingMinistryCode)) {
                    errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                            Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                            "The value entered for course number \"" + courseNum + "\" must start with \""
                                    + matchingMinistryCode
                                    + "\""));
                }
            }
        }
        return errors;
    }

    /**
     * Gets the alias value.
     *
     * @param dataDictionary DataDictionary
     * @param propertyValues Map
     * @param alias String
     * @return String
     */
    private String getAliasValue(DataDictionary dataDictionary, Map propertyValues, String alias) {
        String value = null;
        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            value = getValue(propertyValues, field.getId());
        }
        return value;
    }

    /**
     * Gets the broker.
     *
     * @return X2Broker
     */
    private X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the reference code.
     *
     * @param value String
     * @param dataDictionary DataDictionary
     * @return Reference code
     */
    private ReferenceCode getReferenceCode(String value, DataDictionary dataDictionary) {
        ReferenceCode bean = null;
        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_MINISTRY_COURSE_CODE);
        if (field != null && field.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_CODE, value);
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
            bean = getBroker().getBeanByQuery(query);
        }
        return bean;
    }

    /**
     * Gets the value.
     *
     * @param propertyValues Map
     * @param key String
     * @return String
     */
    private String getValue(Map propertyValues, String key) {
        return (String) propertyValues.get(key);
    }
}
