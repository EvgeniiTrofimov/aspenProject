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

import static com.x2dev.reports.sys.sped.il.IlSpedHelper.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.reports.sys.sped.il.IlSpedHelper;
import com.x2dev.reports.sys.sped.il.IlSpedHelper.ILSpedGroupsOfServices;
import com.x2dev.reports.sys.sped.il.IlSpedHelper.PQSectionType;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.web.sped.IepDetailAction.IepDetail;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.action.ActionErrors;

/**
 * The Class EducationalServicesProcedure.
 */
public class EducationalServicesProcedure implements DynamicFormProcedure {

    private static final String TRUE_FOR_PROPERTY = "Y";

    private static final String ALIAS_SERVICE_DESCRIPTION = "service-description";
    private static final String ALIAS_SERVICE_PROVIDER_OTHER = "service-provider-other";
    private static final String ALIAS_SERVICE_PROVIDER = "service-provider";
    private static final String ALIAS_SERVICE_LOCATION_REGULAR = "service-location-regular";
    private static final String EMPTY = "";
    private static final String ALIAS_ATTR_NY_INITIATION_DATE = "attr-ny-initiation-date";
    private static final String ALIAS_ATTR_CY_DURATION_DATE = "attr-cy-duration-date";
    private static final String ALIAS_ATTR_NY_DURATION_DATE = "attr-ny-duration-date";
    private static final String ALIAS_ATTR_CY_INITIATION_DATE = "attr-cy-initiation-date";
    private static final String ALIAS_SERVICE_CY_DURATION_DATE = "service-cy-duration-date";
    private static final String ALIAS_SERVICE_NY_INITIATION_DATE = "service-ny-initiation-date";

    private static final String EMBADDED_LIST_ID_SERVICES_A_EXTENDED = "services-a-extended";
    private static final String EMBADDED_LIST_ID_SERVICES_B = "services-b";
    private static final String EMBADDED_LIST_ID_SERVICES_A = "services-a";
    private static final String KEY_PQ_TEMPLATE = "pqTemplate";
    private static final String SERVICE_NAME_SPECIAL_EDUCATION_EXTENDED_YEAR = "Special Education, extended year";


    private static final String SERVICE_NAME_RELATED_SERVICES = "Related Services";
    private static final String SERVICE_NAME_SPECIAL_EDUCATION = "Special Education";
    private static final String FIELD_ID_ISV_END_DATE = "isvEndDate";
    private static final String FIELD_ID_ISV_START_DATE = "isvStartDate";
    private static final String JAVASCRIPT_KEY_SERVICES_DATES = "service-dates";

    private boolean m_autocalculateWasEnabled = false;

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
        IepData iepData = getIepData(detail, userData);

        if (!StringUtils.isEmpty(key) && iepData != null)

        {
            if (key.equals(JAVASCRIPT_KEY_SERVICES_DATES) && iepData != null /* && isNewService */) {
                Map<String, Object> prop = new HashMap<String, Object>();
                // boolean isNewService = detail.getCurrentChild() == null ? false
                // :detail.getCurrentChild().isNew();
                DataDictionary ddx = detail.getDataDictionary();
                DataDictionaryField fieldNYinitiation =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SERVICE_NY_INITIATION_DATE);
                DataDictionaryField fieldCYduration =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SERVICE_CY_DURATION_DATE);

                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_START_DATE))) {
                    DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ATTR_CY_INITIATION_DATE);
                    ModelProperty fieldProp = new ModelProperty(iepData.getClass(), field.getJavaName(), ddx);
                    String iepStartDate =
                            (String) WebUtils.getPropertyForDetail(iepData, fieldProp, Locale.getDefault());


                    if (!StringUtils.isEmpty(iepStartDate)) {
                        prop.put(FIELD_ID_ISV_START_DATE, iepStartDate);
                        // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, new
                        // Integer(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_END_DATE))) {
                    DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ATTR_NY_DURATION_DATE);
                    ModelProperty fieldProp = new ModelProperty(iepData.getClass(), field.getJavaName(), ddx);
                    String iepEndDate = (String) WebUtils.getPropertyForDetail(iepData, fieldProp, Locale.getDefault());
                    if (!StringUtils.isEmpty(iepEndDate)) {
                        prop.put(FIELD_ID_ISV_END_DATE, iepEndDate);
                        // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, new
                        // Integer(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(fieldCYduration.getId()))) {
                    DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ATTR_CY_DURATION_DATE);
                    ModelProperty fieldProp = new ModelProperty(iepData.getClass(), field.getJavaName(), ddx);
                    String iepPauseDate =
                            (String) WebUtils.getPropertyForDetail(iepData, fieldProp, Locale.getDefault());
                    if (!StringUtils.isEmpty(iepPauseDate)) {
                        prop.put(fieldCYduration.getId(), iepPauseDate);
                        // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, new
                        // Integer(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils.isEmpty(
                        (String) detail.getCurrentChild().getPropertyValues().get(fieldNYinitiation.getId()))) {
                    DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ATTR_NY_INITIATION_DATE);
                    ModelProperty fieldProp = new ModelProperty(iepData.getClass(), field.getJavaName(), ddx);
                    String iepContinueDate =
                            (String) WebUtils.getPropertyForDetail(iepData, fieldProp, Locale.getDefault());
                    if (!StringUtils.isEmpty(iepContinueDate)) {
                        prop.put(fieldNYinitiation.getId(), iepContinueDate);
                        // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, new
                        // Integer(UserEvent.EVENT_REFRESH));
                    }
                }

                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            } else if (key.equals(KEY_PQ_TEMPLATE)) {
                if (!m_autocalculateWasEnabled) {
                    inableAutocalculateForNewForm(detail, userData, PQSectionType.CURRENT, returnMap);
                    inableAutocalculateForNewForm(detail, userData, PQSectionType.NEXT, returnMap);
                    m_autocalculateWasEnabled = true;
                }
                ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
                Map<String, Object> updatedValues =
                        getUpdatedEEdataIfAutocalculationIsEnable(detail, userData, PQSectionType.CURRENT, broker);
                updatedValues.putAll(
                        getUpdatedEEdataIfAutocalculationIsEnable(detail, userData, PQSectionType.NEXT, broker));

                if (!updatedValues.isEmpty()) {
                    Map<String, Object> prop = (Map<String, Object>) returnMap.get(PROPERTY_DYNAMIC_FORM_PROPERTIES);
                    if (prop == null) {
                        prop = new HashMap<String, Object>();
                    }
                    prop.putAll(updatedValues);
                    returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
                    detail.getPropertyValues().putAll(updatedValues);
                    // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, new
                    // Integer(UserEvent.EVENT_REFRESH));
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        validateTransportationRadioButton(detail, errors, PQSectionType.CURRENT);
        validateTransportationRadioButton(detail, errors, PQSectionType.NEXT);

        Map<String, Object> updatedPropertyValues =
                getUpdatedEEdataIfAutocalculationIsEnable(detail, userData, PQSectionType.CURRENT, broker);
        updatedPropertyValues
                .putAll(getUpdatedEEdataIfAutocalculationIsEnable(detail, userData, PQSectionType.NEXT, broker));
        if (!updatedPropertyValues.isEmpty()) {
            detail.getPropertyValues().putAll(updatedPropertyValues);
        }

        setEmptyBellToBellIfUserChosenValidValueFromDropdown(detail, PQSectionType.CURRENT);
        setEmptyBellToBellIfUserChosenValidValueFromDropdown(detail, PQSectionType.NEXT);

        validateExtendedYearCheckbox(detail, errors, PQSectionType.CURRENT);

        validateGenOutsideLocation(detail, errors);

        return errors;
    }



    /**
     * Copy and update iep data.
     *
     * @param iepData IepData
     * @param detail GenericDetail
     * @return copied IepData with updated values form detail
     */
    private IepData copyAndUpdateIepData(IepData iepData, GenericDetail detail)

    {
        IepData copied = (IepData) iepData.copyBean();
        DataDictionary spedDdx = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();

        for (Entry<String, Object> entry : parentPropertyValue.entrySet())

        {
            String StrignPropertyName = entry.getKey();
            Object propopetyValue = entry.getValue();
            ModelProperty property = new ModelProperty(StrignPropertyName, spedDdx);
            DataDictionaryField field = property.getField();
            ActionErrors errors = new ActionErrors();
            Object convertedValue = GenericDetail.getTypedValue(field, propopetyValue, Locale.getDefault(), errors);

            if (errors.size() == 0)


            {
                try {
                    /*
                     * Finally set the property
                     */
                    PropertyUtils.setProperty(copied, field.getJavaName(), convertedValue);
                } catch (Exception e)

                {
                    System.out.println(copied);
                    System.out.println(field);

                }
            }

        }
        return copied;

    }

    /**
     * return map embedded list where exist location radio buttons.
     *
     * @param detail GenericDetail
     * @return Map
     */
    private Map<String, EmbeddedListDetailSet> getEmbeddedListWithLocationRadio(GenericDetail detail) {
        Map<String, EmbeddedListDetailSet> embeddedListDetailSets = new HashMap<String, EmbeddedListDetailSet>();
        for (String childDetailSetId : detail.getChildDetailSetIds()) {
            ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);
            String serviceMode = null;
            if (childDetailSetId.equals(EMBADDED_LIST_ID_SERVICES_A)) {
                serviceMode = SERVICE_NAME_SPECIAL_EDUCATION;
            } else if (childDetailSetId.equals(EMBADDED_LIST_ID_SERVICES_A_EXTENDED)) {
                serviceMode = SERVICE_NAME_SPECIAL_EDUCATION_EXTENDED_YEAR;
            } else if (childDetailSetId.equals(EMBADDED_LIST_ID_SERVICES_B)) {
                serviceMode = SERVICE_NAME_RELATED_SERVICES;
            }

            if (serviceMode != null) {
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedListDetailSets.put(serviceMode, (EmbeddedListDetailSet) childDetailSet);
                }
            }
        }
        return embeddedListDetailSets;
    }

    /**
     * find and return IepData.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @return current IepData
     */
    private IepData getIepData(GenericDetail detail, UserDataContainer userData) {
        FormDetail formDetail = detail instanceof FormDetail ? (FormDetail) detail : null;
        IepDetail iepDetail = detail instanceof IepDetail ? (IepDetail) detail : null;
        SisOutcomeDetail outcomeDetail = detail instanceof SisOutcomeDetail ? (SisOutcomeDetail) detail : null;
        IepData iepData = null;

        if (formDetail != null) {
            iepData = (IepData) formDetail.getFormInstance().getOwner();
        } else if (iepDetail != null) {
            if (iepDetail.getRootClass().equals(IepData.class) && !StringUtils.isEmpty(iepDetail.getOid())) {
                ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
                iepData = (IepData) broker.getBeanByOid(IepData.class, iepDetail.getOid());
            }
        } else if (outcomeDetail != null) {
            iepData = (IepData) outcomeDetail.getCurrentFormDetail().getFormInstance().getOwner();
        }
        return iepData;
    }

    /**
     * detail(Storage) has open fields where located data about EE(Education Environment)
     * calculation.<br>
     * EE calculated based on data form IepServices<br>
     * If autocalculation is enable - try calculate EE from IepServices and put this data into
     * return map if calculated data
     * another than (old values) values from open text field in detail(Storage).
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param type PQSectionType
     * @param broker ModelBroker
     * @return Map
     */
    private Map<String, Object> getUpdatedEEdataIfAutocalculationIsEnable(GenericDetail detail,
                                                                          UserDataContainer userData,
                                                                          PQSectionType type,
                                                                          ModelBroker broker) {
        Map<String, Object> putMap = new HashMap<String, Object>();
        DataDictionary spedDdx = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();

        if (isAutoCalculate(detail, type)) {
            IepData iepData = getIepData(detail, userData);

            if (iepData != null) {
                IlSpedHelper helper = new IlSpedHelper();
                helper.initializeHelper(broker, spedDdx);
                SisDistrictSchoolYearContext currentContext =
                        iepData.getStudent().getOrganization1().getCurrentContext();
                ILSpedGroupsOfServices groupedServices = helper.getGroupsOfServices(iepData, type, currentContext);
                Map<String, Object> eeMaps = groupedServices.getEEMinutesMap(copyAndUpdateIepData(iepData, detail));

                String outSideMin = eeMaps.get(PARAM_TOTAL_MIN_OUTSIDE_GENERAL_ED) + EMPTY;
                String inSideMin = eeMaps.get(PARAM_TOTAL_MIN_INSIDE_GENERAL_ED) + EMPTY;
                String insideGEpercentage = (String) eeMaps.get(PARAM_OTH_PERCENT);
                if (!StringUtils.isEmpty(insideGEpercentage) && insideGEpercentage.contains(PERCENT)) {
                    insideGEpercentage = insideGEpercentage.substring(0, insideGEpercentage.indexOf(PERCENT));
                }

                validateAndSetChangedNumericIntoPutMap(putMap, parentPropertyValue, spedDdx,
                        type.getAlias(F_TOTAL_MIN_OUTS_GEN_ED), outSideMin);
                validateAndSetChangedNumericIntoPutMap(putMap, parentPropertyValue, spedDdx,
                        type.getAlias(F_TOTAL_MIN_GEN_ED), inSideMin);
                validateAndSetChangedNumericIntoPutMap(putMap, parentPropertyValue, spedDdx,
                        type.getAlias(F_PERCENT_GEN_ED), insideGEpercentage);
            }
        }
        return putMap;

    }

    /**
     * translate alias to field property name and get value from propertyMap.
     *
     * @param propertyMap key field property name
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

    /**
     * if user add new Form instance and storage object has unchecked "autocalc" checkboxes - check
     * them .
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param type PQSectionType
     * @param returnMap Map<String,Object>
     */
    private void inableAutocalculateForNewForm(GenericDetail detail,
                                               UserDataContainer userData,
                                               PQSectionType type,
                                               Map<String, Object> returnMap) {
        Map<String, Object> prop = (Map<String, Object>) returnMap.get(PROPERTY_DYNAMIC_FORM_PROPERTIES);
        if (prop == null) {
            prop = new HashMap<String, Object>();
        }

        boolean isNew = isNew(detail, userData);

        if (isNew && !isAutoCalculate(detail, type)) {
            DataDictionary spedDdx = detail.getDataDictionary();
            String alias = type.getAlias(F_AUTOCALC);
            DataDictionaryField field = spedDdx.findDataDictionaryFieldByAlias(alias);
            prop.put(field.getId(), TRUE_FOR_PROPERTY);
            // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
            returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
        }
    }

    /**
     * Checks if is auto calculate.
     *
     * @param detail GenericDetail
     * @param type PQSectionType
     * @return true if "autocalc" checkbox is checked
     */
    private boolean isAutoCalculate(GenericDetail detail, PQSectionType type) {
        boolean isAutopopulate = false;
        DataDictionary spedDdx = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();

        String isAutocalc = (String) getValueFromPropertyMap(parentPropertyValue, spedDdx, type.getAlias(F_AUTOCALC));

        if (!StringUtils.isEmpty(isAutocalc) && isAutocalc.equals(Boolean.TRUE.toString())) {
            isAutopopulate = true;
        }
        return isAutopopulate;
    }

    /**
     * Checks if is new.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @return true if PQ formInstance is new or doesn't exist
     */
    private boolean isNew(GenericDetail detail, UserDataContainer userData) {
        boolean isNew = false;
        FormDetail formDetail = detail instanceof FormDetail ? (FormDetail) detail : null;
        SisOutcomeDetail outcomeDetail = detail instanceof SisOutcomeDetail ? (SisOutcomeDetail) detail : null;
        IepDetail iepDetail = detail instanceof IepDetail ? (IepDetail) detail : null;
        if (outcomeDetail != null) {
            formDetail = outcomeDetail.getCurrentFormDetail();
        }
        if (formDetail != null) {
            isNew = formDetail.getFormInstance().isNew();
        }
        if (iepDetail != null) {
            if (iepDetail.getRootClass().equals(IepData.class) && !StringUtils.isEmpty(iepDetail.getOid())) {
                ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(FormInstance.COL_STORAGE_OBJECT_OID, iepDetail.getOid());
                criteria.addEqualTo(
                        FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        "SPED-IL-3454PQ");
                X2BaseBean bean = broker.getBeanByQuery(new QueryByCriteria(FormInstance.class, criteria));
                if (bean == null) {
                    isNew = true;
                }
            }
        }

        return isNew;
    }

    /**
     * try match stringValue and value from originalMap. If there are difference - put stringValue
     * into putMap
     * 
     * @param putMap map where will put "stringValue" value if it is difference than value form
     *        originalMap
     * @param originalMap - property values map from detail where key is property field name and
     *        value - string represent value
     * @param dictionary dictionary for translate alias
     * @param alias - target field. It is will translate into key for originalMap and putMap
     * @param stringValue new value
     */
    private void setChangedValueIntoPutMap(Map<String, Object> putMap,
                                           Map<String, Object> originalMap,
                                           DataDictionary dictionary,
                                           String alias,
                                           String stringValue) {

        if (originalMap != null && !originalMap.isEmpty() && dictionary != null && alias != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            String fieldId = field == null ? "" : field.getId();
            if (!StringUtils.isEmpty(fieldId)) {
                String oldValue = (String) originalMap.get(fieldId);
                if (

                ((oldValue != null && stringValue == null)
                        ||
                        (oldValue == null && stringValue != null)
                        ||
                        (oldValue != null && stringValue != null && !oldValue.equals(stringValue)))) {
                    putMap.put(fieldId, stringValue);
                }

            }

        }
    }

    /**
     * if user select other than "other" value in "school mpw" dropdown - <br>
     * "school mpw code other" field should be empty.
     *
     * @param detail GenericDetail
     * @param type PQSectionType
     */
    private void setEmptyBellToBellIfUserChosenValidValueFromDropdown(GenericDetail detail, PQSectionType type) {
        DataDictionary spedDdx = detail.getDataDictionary();
        Map<String, Object> parentPropertyValue = detail.getPropertyValues();

        String bellToBellCode =
                (String) getValueFromPropertyMap(parentPropertyValue, spedDdx, type.getAlias(F_SCHOOL_MPW));

        if (!StringUtils.isEmpty(bellToBellCode)) {
            if (!bellToBellCode.equals(SCHOOL_MPW_CODE_OTHER)) {
                Map<String, Object> putMap = new HashMap<String, Object>(1);
                setChangedValueIntoPutMap(putMap, parentPropertyValue, spedDdx, type.getAlias(F_TOTAL_MIN_BELL_TO_BELL),
                        null);
                if (!putMap.isEmpty()) {
                    parentPropertyValue.putAll(putMap);
                }
            }
        }

    }

    /**
     * validate that stringValue is numeric and set into putMap if stringValue another than value
     * form propertyMap.
     *
     * @param putMap map where will put new value
     * @param propertyMap map where old values (proprtyValues from detail)
     * @param dictionary for translate alias
     * @param alias will translate into key for putMap and propertyMap
     * @param stringValue new value
     */
    private void validateAndSetChangedNumericIntoPutMap(Map<String, Object> putMap,
                                                        Map<String, Object> propertyMap,
                                                        DataDictionary dictionary,
                                                        String alias,
                                                        String stringValue) {

        if (StringUtils.isEmpty(stringValue) || StringUtils.isNumeric(stringValue)) {
            setChangedValueIntoPutMap(putMap, propertyMap, dictionary, alias, stringValue);
        }

    }

    /**
     * If extended school year needed is checked - than require one or more Extended School Year
     * Service .
     *
     * @param detail GenericDetail
     * @param errors List<ValidationError>
     * @param type PQSectionType
     */
    private void validateExtendedYearCheckbox(GenericDetail detail, List<ValidationError> errors, PQSectionType type) {

        String extYearNeeded = (String) getValueFromPropertyMap(detail.getPropertyValues(), detail.getDataDictionary(),
                type.getAlias(F_EXT_SCH_YEAR_NEED));

        if (!StringUtils.isEmpty(extYearNeeded) && extYearNeeded.equals(Boolean.TRUE.toString())) {

            EmbeddedListDetailSet extendedDetailSet = null;
            for (String childDetailSetId : detail.getChildDetailSetIds()) {

                if (childDetailSetId.equals(EMBADDED_LIST_ID_SERVICES_A_EXTENDED)) {


                    extendedDetailSet = (EmbeddedListDetailSet) detail.getChildDetailSet(childDetailSetId);
                }
            }

            if (extendedDetailSet != null && extendedDetailSet.getChildDetails().size() == 0) {
                String year = type.ordinal() == 0 ? "Current Year" : "Next Year";

                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        year + ". \"Extended school year services are needed.\" Form hasn't any extended service"));
            }
        }

    }

    /**
     * in embedded list where location radio exist - require check general or outside location
     * radio.
     *
     * @param detail GenericDetail
     * @param errors List<ValidationError>
     */
    private void validateGenOutsideLocation(GenericDetail detail, List<ValidationError> errors) {
        Map<String, EmbeddedListDetailSet> embeddedListDetailSets = getEmbeddedListWithLocationRadio(detail);
        for (Entry<String, EmbeddedListDetailSet> entry : embeddedListDetailSets.entrySet()) {

            String tableName = entry.getKey();
            EmbeddedListDetailSet embeddedListDetailSet = entry.getValue();
            for (GenericDetail genericDetail : embeddedListDetailSet.getChildDetails()) {
                DataDictionary dataDictionary = genericDetail.getDataDictionary();
                Map<String, Object> propertyValues = genericDetail.getPropertyValues();

                String locationValue = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                        ALIAS_SERVICE_LOCATION_REGULAR);

                if (locationValue == null || locationValue.isEmpty()) {
                    String service = null;
                    if (tableName != null && tableName.equals(SERVICE_NAME_RELATED_SERVICES)) {
                        service = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                                ALIAS_SERVICE_PROVIDER);
                    } else if (tableName != null) {
                        service = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                                ALIAS_SERVICE_PROVIDER_OTHER);
                    }

                    if (service == null || service.isEmpty()) {
                        service = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                                ALIAS_SERVICE_DESCRIPTION);
                    }
                    if (!SERVICE_NAME_SPECIAL_EDUCATION_EXTENDED_YEAR.equals(tableName)) {
                        errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                                Integer.valueOf(BusinessRules.REQUIRE_FIELD), "EDUCATIONAL SERVICES AND PLACEMENT form, \""
                                        + tableName + "\"  table   \"" + service + "\" record, "
                                        + "\"Within Gen Ed\" or \"Outside Gen Ed\" must be selected"));
                    }
                }
            }
        }
    }

    /**
     * TransportationRadioButton is require.
     *
     * @param detail GenericDetail
     * @param errors List<ValidationError>
     * @param type PQSectionType
     */
    private void validateTransportationRadioButton(GenericDetail detail,
                                                   List<ValidationError> errors,
                                                   PQSectionType type) {
        DataDictionary parentDataDictionary = detail.getDataDictionary();

        Map<String, Object> parentPropertyValue = detail.getPropertyValues();

        String someValue = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary,
                type.getAlias(F_SPEC_TRANSP_BETWEEN_SCH));

        String errorHeader = "EDUCATIONAL SERVICES AND PLACEMENT form, TRANSPORTATION block. Fields: ";
        boolean useHeader = true;
        StringBuilder builder = new StringBuilder();
        if (StringUtils.isEmpty(someValue)) {
            builder.append(useHeader ? errorHeader : EMPTY);
            builder.append("Special transportation to and from schools and/or between schools. ");
            useHeader = false;

        }

        someValue = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary,
                type.getAlias(F_SPEC_TRANSP_AROUND_SCH));

        if (StringUtils.isEmpty(someValue)) {
            builder.append(useHeader ? errorHeader : EMPTY);
            builder.append("Special transportation in and around school buildings. ");
            useHeader = false;
        }
        someValue = (String) getValueFromPropertyMap(parentPropertyValue, parentDataDictionary,
                type.getAlias(F_SPEC_EQUIP));


        if (StringUtils.isEmpty(someValue)) {
            builder.append(useHeader ? errorHeader : EMPTY);
            builder.append("Specialized equipment (such as special or adapted buses, lifts, and ramps). ");
            useHeader = false;

        }

        if (builder.length() > 0) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.REQUIRE_FIELD), builder.toString()));
        }
    }


}


