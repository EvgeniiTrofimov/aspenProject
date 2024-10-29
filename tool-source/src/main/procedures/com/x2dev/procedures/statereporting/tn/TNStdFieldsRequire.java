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
package com.x2dev.procedures.statereporting.tn;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ValidationConstants.CUSTOM_ERROR;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
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
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for Tennesee Public Schools to validate that:
 * 1) at least one race code added
 * 2) at least one of PIN or SSN added, but not both.
 *
 * @author X2 Development Corporation
 */
public class TNStdFieldsRequire implements DynamicFormProcedure {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String EMPTY = " ";
    private static final String ERROR_MESSAGE_NATIVE_LANGUAGE_ENG_NOT_PERMITTED_FOR_LANGUAGE_BACKGROUND_CLASSIFICATIONS_OF =
            "Native language. 'ENG' not permitted for students with English language background classifications of ";

    private static final String ERROR_MESSAGE_NATIVE_LANGUAGE_ENG_IF_LANGUAGE_BACKGROUND_CLASSIFICATIONS_NOT_OF =
            "Native language. Should be 'ENG' with English language background classifications other than ";

    private static final String ERROR_MESSAGE_NATIVE_LANGUAGE =
            "Native language.";
    private static final String ERROR_MESSAGE_MISSING_VALUES =
            "Missing values (blanks) not permitted";
    private static final String ALIAS_DOE_DATE_FIRST_ENTER_US_SCHOOL = "DOE DATE FIRST ENTER US SCHOOL";
    private static final String ALIAS_DOE_ELB = "DOE ELB";
    private static final String ALIAS_DOE_IMMIGRANT = "DOE IMMIGRANT";
    private static final String ALIAS_DOE_PIN = "DOE PIN";
    private static final String ALIAS_NATIVE_LANGUAGE = "DOE NATIVE LANGUAGE";

    private static final String DOT = ".";
    private static final String MUST_ENTER_DATE_FIRST_ENROLLED_IN_US_SCHOOL =
            "MUST enter Date First Enrolled in US School for Student flagged as Immigrant or English Language Background ";
    private static final String PIN_SSN_REQ_MESSAGE = "PIN or SSN required.";
    private static final String PIN_SSN_MESSAGE = " Both PIN and SSN codes cannot be present.";

    private static final String PROPERTY_SSN = "relStdPsnOid.psnPersonID";

    private static final String RACE_MESSAGE = "You must add a minimum of 1 race code.  Click the Ethnicity tab.";
    private static final String RACE_PROPERTY = "racRaceCode";
    private static final String TRUE = Boolean.TRUE.toString().toLowerCase();

    private static final String WIZARD_CONTEXT_ID = "wizard.registration.demographics";
    private List<String> m_elbValidRuleCodes = new ArrayList<String>(Arrays.asList("L", "W", "1", "2",
            "3", "4", "F", "N"));
    private List<String> m_nativeLangValidRuleCodes = new ArrayList<String>(m_elbValidRuleCodes);

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.
     *      fsc.core.k12.web.GenericDetail, com.follett.fsc.core.k12.web.UserDataContainer,
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(
     *      com.follett.fsc.core.k12.web.template.Template,
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
        // Do nothing
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(
     *      com.follett.fsc.core.k12.web.GenericDetail, java.lang.String, java.lang.String,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template,
     *      java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        return new HashMap<String, Object>();
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(
     *      com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        if (form == null) {
            boolean valid = false;

            for (ChildDetailSet childset : detail.getChildDetailSets()) {
                childset.toString();
                if (childset.getChildClass().equals(Race.class)) {
                    for (GenericDetail childDetail : childset.getChildDetails()) {
                        String race = (String) childDetail.getValue(RACE_PROPERTY);
                        if (!StringUtils.isEmpty(race)) {
                            valid = true;
                            break;
                        }
                    }
                }
            }

            if (!valid) {
                errors.add(new ValidationError(CUSTOM_ERROR, null, RACE_MESSAGE));
            }

            String pinProperty = getPropertyByAlias(ALIAS_DOE_PIN, broker);
            String firstEnterUsSchProperty = getPropertyByAlias(ALIAS_DOE_DATE_FIRST_ENTER_US_SCHOOL, broker);
            String immigrantProperty = getPropertyByAlias(ALIAS_DOE_IMMIGRANT, broker);
            String elbProperty = getPropertyByAlias(ALIAS_DOE_ELB, broker);
            String nLangProperty = getPropertyByAlias(ALIAS_NATIVE_LANGUAGE, broker);

            Map<String, String> propValues = detail.getPropertyValues();

            String firstEnterUsSchValue = propValues.get(firstEnterUsSchProperty);
            boolean firstEnterUsSchExist =
                    !StringUtils.isEmpty(firstEnterUsSchProperty) && propValues.containsKey(firstEnterUsSchProperty);
            String immigrantValue = propValues.get(immigrantProperty);
            boolean immigrantExist =
                    !StringUtils.isEmpty(immigrantProperty) && propValues.containsKey(immigrantProperty);
            String elbValue = propValues.get(elbProperty);
            boolean elbExist = !StringUtils.isEmpty(elbProperty) && propValues.containsKey(elbProperty);
            String nodeId = userData.getCurrentNode().getId();

            boolean nLangExist = !StringUtils.isEmpty(nLangProperty) && propValues.containsKey(nLangProperty);
            String nLangValue = propValues.get(nLangProperty);

            if (!WIZARD_CONTEXT_ID.equals(nodeId)) {
                if (firstEnterUsSchExist && immigrantExist && elbExist &&
                        StringUtils.isEmpty(firstEnterUsSchValue) &&
                        (TRUE.equals(immigrantValue) || m_elbValidRuleCodes.contains(elbValue))) {
                    errors.add(new ValidationError(CUSTOM_ERROR, null, MUST_ENTER_DATE_FIRST_ENROLLED_IN_US_SCHOOL
                            + m_elbValidRuleCodes.toString() + DOT));
                }

                if (nLangExist && StringUtils.isEmpty(nLangValue)) {
                    errors.add(new ValidationError(CUSTOM_ERROR, null, ERROR_MESSAGE_NATIVE_LANGUAGE + EMPTY
                            + ERROR_MESSAGE_MISSING_VALUES));
                }

                if (nLangExist && elbExist && "ENG".equals(nLangValue)
                        && m_nativeLangValidRuleCodes.contains(elbValue)) {
                    errors.add(new ValidationError(CUSTOM_ERROR, null,
                            ERROR_MESSAGE_NATIVE_LANGUAGE_ENG_NOT_PERMITTED_FOR_LANGUAGE_BACKGROUND_CLASSIFICATIONS_OF +
                                    m_nativeLangValidRuleCodes.toString() + DOT));
                }

                if (nLangExist && elbExist && !StringUtils.isEmpty(nLangValue) && !"ENG".equals(nLangValue)
                        && !StringUtils.isEmpty(elbValue) && !m_nativeLangValidRuleCodes.contains(elbValue)) {
                    errors.add(new ValidationError(CUSTOM_ERROR, null,
                            ERROR_MESSAGE_NATIVE_LANGUAGE_ENG_IF_LANGUAGE_BACKGROUND_CLASSIFICATIONS_NOT_OF +
                                    m_nativeLangValidRuleCodes.toString() + DOT));
                }

            }

            // Only process if the user has access to PIN and SSN
            if (propValues.containsKey(pinProperty) && propValues.containsKey(PROPERTY_SSN)) {
                String pinValue = propValues.get(pinProperty);
                String ssnValue = propValues.get(PROPERTY_SSN);
                // 3
                if (!StringUtils.isEmpty(pinValue) && !StringUtils.isEmpty(ssnValue)) {
                    errors.add(new ValidationError(CUSTOM_ERROR, null, PIN_SSN_MESSAGE));
                }
            }
        }

        return errors;
    }

    private String getPropertyByAlias(String alias, ModelBroker broker) {
        String propertyName = null;

        // Determine property PIN
        X2Criteria pinPropertyCriteria = new X2Criteria();
        pinPropertyCriteria.addContainsIgnoreCase(DataField.REL_DATA_FIELD_CONFIGS + ModelProperty.PATH_DELIMITER +
                DataFieldConfig.COL_ALIAS, alias);
        QueryByCriteria pinPropertyQuery = new QueryByCriteria(DataField.class, pinPropertyCriteria);
        DataField field = (DataField) broker.getBeanByQuery(pinPropertyQuery);
        if (field != null) {
            propertyName = field.getOid().trim();
        }
        return propertyName;
    }
}
