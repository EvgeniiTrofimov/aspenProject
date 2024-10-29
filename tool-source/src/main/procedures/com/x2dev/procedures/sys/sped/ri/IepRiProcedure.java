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
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Cell;
import com.follett.fsc.core.k12.web.template.ElementContainer;
import com.follett.fsc.core.k12.web.template.Property;
import com.follett.fsc.core.k12.web.template.Section;
import com.follett.fsc.core.k12.web.template.Tab;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.Text;
import com.follett.fsc.core.k12.web.template.TextFormat;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * The Class IepRiProcedure.
 */
public class IepRiProcedure implements DynamicFormProcedure {


    private static final String END_JAVASCRIPT = "</script>";

    private static final String START_JAVASCRIPT = "<script language=\"Javascript\">";

    private static final String NEW_LINE = "\r\n";

    private static final String ALIAS_AFTER_3RD_OTHER = "riec-38d-iep-after-3rd-other-t";

    private static final String EI_TRANSITION_REASON_OTHER = "OTHER";

    private static final String ALIAS_IEP_AFTER_3RD = "riec-38d-iep-after-3rd-t";

    private static final String EI_TRANSITION_DELAY = "DELAY";

    private static final String ALIAS_EARLY_INTERVENTION = "riec-38d-early-intervention-t";

    private static final String ALIAS_PRGM_NAME = "riec-early-childhood-prgm-name";

    private static final String SPACE = " ";

    private static final String COMMA = ",";
    /**
     *
     */
    private static final String DOT = ".";
    private static final String ALIAS_EARLY_ENVIRONMENT = "riec-38a-e-early-environment";
    private static final String ALIAS_IEP_SERVICES_DATE_CONTINUE = "iep-services-date-cont";
    private static final String ALIAS_IEP_SERVICES_DATE_PAUSE = "iep-services-date-pause";
    private static final String ALIAS_REFCODE_BLOB_DESCRIPTION = "all-rcd-LongDesc";
    private static final String ALIAS_SERVICE_DATE_CONTINUE = "service-date-cont";
    private static final String ALIAS_SERVICE_DATE_PAUSE = "service-date-pause";

    private static final String ALIAS_SERVICE_LOCATION_OTHER = "service-location-other";
    private static final String ALIAS_SERVICE_LOCATION_OTHER_DESCRIPTION = "service-loc-description";
    private static final String ALIAS_SERVICE_LOCATION_REGULAR = "service-location-regular";

    private static final String EMBEDDED_LIST_ID_SERVICES_A = "services-a";
    private static final String EMBEDDED_LIST_ID_SERVICES_B = "services-b";
    /* private static final String EMBEDDED_LIST_ID_SERVICES_C = "services-c"; */

    private static final String EMBEDDED_LIST_VALUE_SERVICES_A = "Special Education";
    private static final String EMBEDDED_LIST_VALUE_SERVICES_B = "Related Services";
    /* private static final String EMBEDDED_LIST_VALUE_SERVICES_C = "Supplementary Aids"; */

    /**
     * A map of maps of reference code.
     * The outer map is indexed by the reference table OID. It contains maps of reference codes.
     * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
     * code.
     */
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;

    private static final String FIELD_ID_IAC_DESCRIPTION = "iacDescription";
    private static final String FIELD_ID_IAC_NAME = "iacName";
    private static final String FIELD_ID_IEP_END_DATE = "iepEndDate";
    private static final String FIELD_ID_IEP_START_DATE = "iepStartDate";
    private static final String FIELD_ID_ISV_END_DATE = "isvEndDate";
    private static final String FIELD_ID_ISV_START_DATE = "isvStartDate";

    private static final String JAVASCRIPT_KEY_CHANGING = "changing-";
    private static final String JAVASCRIPT_KEY_SERVICES_DATES = "service-dates";

    private static final String START_DATE = "isvStartDate";
    private X2Broker m_broker = null;
    private List<String> m_programNameRequire = Arrays.asList("01", "02", "03", "04", "05", "06", "07", "08", "09",
            "10", "11", "12", "13", "14");

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
        m_broker = new ModelBroker(privilegeSet);
        StringBuilder builder = new StringBuilder();
        Map<String, ReferenceCode> refCodeMap = null;
        String viewTextElementId = "field139Dependence";
        String fieldId = null;
        DataDictionaryField ddxField = dictionary.findDataDictionaryFieldByAlias("riec-38a-e-early-environment");
        ModelProperty targetModelProperty = null;
        if (ddxField != null) {
            fieldId = ddxField.getId();
            targetModelProperty = new ModelProperty(ddxField, dictionary);
            ReferenceTable refTable = ddxField.getReferenceTable();
            if (refTable != null) {
                refCodeMap = refTable.getCodeMap();
            }
        }

        if (refCodeMap != null && !refCodeMap.isEmpty()) {
            builder.append(START_JAVASCRIPT);
            builder.append(NEW_LINE);
            builder.append(NEW_LINE);
            builder.append("       /*");
            builder.append(NEW_LINE);
            builder.append("        * Onload. ");
            builder.append(NEW_LINE);
            builder.append("        */");
            builder.append(NEW_LINE);

            builder.append("var updateDependence139 = function() {");
            builder.append(NEW_LINE);
            builder.append("var $fieldValue = $('input[name^=\"propertyValue(" + fieldId + ")\"]').val();");
            builder.append(NEW_LINE);

            boolean isFirst = true;
            for (ReferenceCode refCode : refCodeMap.values()) {
                String code = refCode.getCode();
                String description = refCode.getDescription();

                if (isFirst) {
                    builder.append("if ($fieldValue.toUpperCase() === \"" + code + "\") {");
                    isFirst = false;
                } else {
                    builder.append("} else if ($fieldValue.toUpperCase() === \"" + code + "\") {");
                }

                builder.append(NEW_LINE);
                builder.append("$('#" + viewTextElementId + "').text(\"" + description + "\");");
                builder.append(NEW_LINE);

            }
            builder.append("} else {");
            builder.append(NEW_LINE);
            builder.append("$('#" + viewTextElementId + "').text(\"\");");
            builder.append(NEW_LINE);
            builder.append("}");
            builder.append(NEW_LINE);
            builder.append("};");
            builder.append(NEW_LINE);

            builder.append("$(function()");
            builder.append(NEW_LINE);
            builder.append("{");
            builder.append(NEW_LINE);
            builder.append("updateDependence139();");
            builder.append(NEW_LINE);
            builder.append("$('input[name^=\"propertyValue(" + fieldId + ")\"]').change(function() {");
            builder.append(NEW_LINE);
            builder.append("updateDependence139();");
            builder.append(NEW_LINE);
            builder.append("});");
            builder.append(NEW_LINE);
            builder.append("});");
            builder.append(NEW_LINE);
            builder.append(END_JAVASCRIPT);
            builder.append(NEW_LINE);

        }

        Section section = template.getFirstSection();
        if (section != null) {

            for (Tab tab : section.getTabs()) {
                if (tab.getModelProperties().contains(targetModelProperty)) {
                    tab.findElementById(targetModelProperty.getFieldId());
                    Set<Property> properties = tab.getProperty(targetModelProperty);
                    if (properties.size() == 1) {
                        Property targetProperty = properties.iterator().next();

                        ElementContainer parentCell = tab.findParentById(targetProperty.getElementId());


                        if (parentCell != null && parentCell instanceof Cell) {
                            Cell cell = (Cell) parentCell;
                            TextFormat textFormat = new TextFormat("small", false, false, false);
                            StringBuilder textForView = new StringBuilder();
                            textForView.append(NEW_LINE);
                            textForView
                                    .append("<span class = \" templateTextSmall\" id=\"" + viewTextElementId + "\">");
                            textForView.append(NEW_LINE);
                            textForView.append("</span>");
                            textForView.append(NEW_LINE);
                            textForView.append(builder);
                            textForView.append(NEW_LINE);


                            Text text = new Text(textForView.toString(), null, dictionary, textFormat, locale);
                            cell.getElements().add(text);

                        }
                    }
                }
            }
        }



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
        Map<String, Object> prop = new HashMap<String, Object>();


        if (!StringUtils.isEmpty(key)) {
            if (key.equals(JAVASCRIPT_KEY_SERVICES_DATES) && detail.getCurrentChild().getOid() == null) {
                DataDictionaryField fieldIepServicesContinueDate =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_SERVICES_DATE_CONTINUE);
                DataDictionaryField fieldIepServicesPauseDate =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_SERVICES_DATE_PAUSE);

                DataDictionaryField fieldServiceContinueDate =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SERVICE_DATE_CONTINUE);
                DataDictionaryField fieldServicePauseDate =
                        detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SERVICE_DATE_PAUSE);

                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_START_DATE))) {
                    String iepStartDate = (String) detail.getValue(FIELD_ID_IEP_START_DATE);
                    if (!StringUtils.isEmpty(iepStartDate)) {
                        prop.put(FIELD_ID_ISV_START_DATE, iepStartDate);
                        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_END_DATE))) {
                    String iepEndDate = (String) detail.getValue(FIELD_ID_IEP_END_DATE);
                    if (!StringUtils.isEmpty(iepEndDate)) {
                        prop.put(FIELD_ID_ISV_END_DATE, iepEndDate);
                        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils.isEmpty(
                        (String) detail.getCurrentChild().getPropertyValues().get(fieldServicePauseDate.getId()))) {
                    String iepPauseDate = (String) detail.getValue(fieldIepServicesPauseDate.getId());
                    if (!StringUtils.isEmpty(iepPauseDate)) {
                        prop.put(fieldServicePauseDate.getId(), iepPauseDate);
                        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                    }
                }
                if (StringUtils.isEmpty(
                        (String) detail.getCurrentChild().getPropertyValues().get(fieldServiceContinueDate.getId()))) {
                    String iepContinueDate = (String) detail.getValue(fieldIepServicesContinueDate.getId());
                    if (!StringUtils.isEmpty(iepContinueDate)) {
                        prop.put(fieldServiceContinueDate.getId(), iepContinueDate);
                        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                    }
                }

                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            } else if (key.equals(JAVASCRIPT_KEY_CHANGING + FIELD_ID_IAC_NAME)) {
                DataDictionaryField fieldName = detail.getDataDictionary().findDataDictionaryField(FIELD_ID_IAC_NAME);
                if (fieldName != null && fieldName.hasReferenceTable()) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                            fieldName.getReferenceTable().getExtendedDataDictionary(), getBroker().getPersistenceKey());
                    DataDictionaryField blobField =
                            dictionary.findDataDictionaryFieldByAlias(ALIAS_REFCODE_BLOB_DESCRIPTION);

                    String description = "";
                    Collection<ReferenceCode> codes =
                            ReferenceManager.getCodes(fieldName.getReferenceTable(), null, userData, getBroker());
                    for (ReferenceCode code : codes) {
                        if (value.equals(code.getCode())) {
                            if (blobField != null) {
                                description = (String) code.getFieldValueByBeanPath(blobField.getJavaName());
                            }
                            if (StringUtils.isEmpty(description)) {
                                description = code.getDescription();
                            }
                        }
                    }

                    prop.put(FIELD_ID_IAC_DESCRIPTION, description);
                    returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                    returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
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
        errors.addAll(validateServicePlacementTab(detail));
        errors.addAll(validateEarlyChildHoodTab(detail));
        return errors;
    }

    /*
     * private DistrictSchoolYearContext getContext(ModelBroker broker, GenericDetail detail)
     * {
     * DistrictSchoolYearContext context = null;
     *
     * SisStudent student = ((IepData) broker.getBeanByOid(IepData.class,
     * detail.getOid())).getStudent();
     * SisSchool school = student.getSchool();
     * Organization organization = school.getOrganization1();
     * context = school.getCurrentContext();
     * if (context == null)
     * {
     * context = organization.getCurrentContext();
     * }
     * return context;
     * }
     */

    /**
     * Returns first in session date.
     *
     * @param propertyMap Map<String,Object>
     * @param dictionary DataDictionary
     * @param alias String
     * @return Object
     */
    // it was need for auto calculate pause and continue dates, but pause date will after continue,
    // it's wrong.
    // getFirstInSessionDate work correct, issue in - how determine pause and continue date. After
    // call we decided to delete auto calculation
    // but in future this method can be useful
    /*
     * private PlainDate getFirstInSessionDate(PlainDate startDate, PlainDate endDate, ModelBroker
     * broker)
     * {
     * PlainDate firstInSessionDate = startDate;
     *
     * X2Criteria criteria = new X2Criteria();
     * criteria.addGreaterOrEqualThan(DistrictCalendar.COL_DATE, startDate);
     * criteria.addLessOrEqualThan(DistrictCalendar.COL_DATE, endDate);
     * criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
     *
     * QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
     * query.addOrderByAscending(DistrictCalendar.COL_DATE);
     *
     * DistrictCalendar calendarDate = (DistrictCalendar) broker.getBeanByQuery(query);
     * if (calendarDate != null)
     * {
     * firstInSessionDate = calendarDate.getDate();
     * }
     *
     * return firstInSessionDate;
     * }
     */

    /**
     * Returns last in session date.
     *
     * @return
     */
    // it was need for auto calculate pause and continue dates, but pause date will after continue,
    // it's wrong.
    // getLastInSessionDate work correct, issue in - how determine pause and continue date. After
    // call we decided to delete auto calculation
    // but in future this method can be useful
    /*
     * private PlainDate getLastInSessionDate(PlainDate startDate, PlainDate endDate, ModelBroker
     * broker)
     * {
     * PlainDate lastInSessionDate = startDate;
     *
     * X2Criteria criteria = new X2Criteria();
     * criteria.addGreaterOrEqualThan(DistrictCalendar.COL_DATE, startDate);
     * criteria.addLessOrEqualThan(DistrictCalendar.COL_DATE, endDate);
     * criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
     *
     * QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
     * query.addOrderByDescending(DistrictCalendar.COL_DATE);
     *
     * DistrictCalendar calendarDate = (DistrictCalendar) broker.getBeanByQuery(query);
     * if (calendarDate != null)
     * {
     * lastInSessionDate = calendarDate.getDate();
     * }
     * return lastInSessionDate;
     * }
     */
    /**
     *
     * @return X2Broker
     */
    private X2Broker getBroker() {
        return m_broker;
    }


    private String getStateValueFromPropertyMap(Map<String, Object> propertyMap,
                                                DataDictionary dictionary,
                                                String alias) {
        String stateCode = null;
        ReferenceCode code = getRefCodeByValueFromPropertyMap(propertyMap, dictionary, alias);
        if (code != null) {
            stateCode = code.getStateCode();
        }
        return stateCode;
    }

    private ReferenceCode getRefCodeByValueFromPropertyMap(Map<String, Object> propertyMap,
                                                           DataDictionary dictionary,
                                                           String alias) {
        ReferenceCode code = null;
        String value = (String) getValueFromPropertyMap(propertyMap, dictionary, alias);
        DataDictionaryField ddxField = dictionary.findDataDictionaryFieldByAlias(alias);
        String refTableOid = null;
        if (ddxField != null) {
            if (ddxField.getExtendedDataField() != null) {
                refTableOid = ddxField.getExtendedDataField().getReferenceTableOid();
            } else if (ddxField.hasReferenceTable()) {
                refTableOid = ddxField.getReferenceTableOid();
            }
        }

        if (!StringUtils.isEmpty(refTableOid)) {
            Map<String, ReferenceCode> refCodes = getReferenceCodes(refTableOid);
            code = refCodes.get(value);
        }
        return code;
    }

    private String getSubCodes(ReferenceCode code) {

        StringBuilder subCodes = new StringBuilder();
        if (code.getCategoryIndicator() && code.getCode() != null) {
            String category = code.getCode();
            for (ReferenceCode refCode : getReferenceCodes(code.getReferenceTableOid()).values()) {
                if (!refCode.getCategoryIndicator() && category.equals(refCode.getCategory())) {
                    if (subCodes.length() != 0) {
                        subCodes.append(COMMA + SPACE);
                    }
                    subCodes.append(refCode.getCode());
                }
            }

        }
        subCodes.append(DOT);
        return subCodes.toString();
    }

    private Object getValueFromPropertyMap(Map<String, Object> propertyMap, DataDictionary dictionary, String alias) {
        Object returnValue = null;
        if (propertyMap != null && !propertyMap.isEmpty() && dictionary != null && alias != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            String fieldId = field == null ? "" : field.getId();
            returnValue = propertyMap.get(fieldId);
        }
        return returnValue;
    }

    private List<ValidationError> validateServicePlacementTab(GenericDetail detail) {

        Map<String, EmbeddedListDetailSet> embeddedListDetailSets = new HashMap<String, EmbeddedListDetailSet>();
        List<ValidationError> errors = new ArrayList<ValidationError>();
        for (String childDetailSetId : detail.getChildDetailSetIds()) {
            ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);
            String serviceMode = null;
            if (childDetailSetId.equals(EMBEDDED_LIST_ID_SERVICES_A)) {
                serviceMode = EMBEDDED_LIST_VALUE_SERVICES_A;
            } else if (childDetailSetId.equals(EMBEDDED_LIST_ID_SERVICES_B)) {
                serviceMode = EMBEDDED_LIST_VALUE_SERVICES_B;
            }
            /*
             * else if(childDetailSetId.equals(EMBEDDED_LIST_ID_SERVICES_C))
             * {
             * serviceMode = EMBEDDED_LIST_VALUE_SERVICES_C;
             * }
             */

            if (serviceMode != null) {
                embeddedListDetailSets.put(serviceMode, (EmbeddedListDetailSet) childDetailSet);

            }
        }

        for (Entry<String, EmbeddedListDetailSet> entry : embeddedListDetailSets.entrySet()) {
            String serviceMode = entry.getKey();
            EmbeddedListDetailSet detailSet = entry.getValue();
            for (GenericDetail genericDetail : detailSet.getChildDetails()) {
                DataDictionary dataDictionary = genericDetail.getDataDictionary();
                Map<String, Object> propertyValues = genericDetail.getPropertyValues();

                String regularString = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                        ALIAS_SERVICE_LOCATION_REGULAR);
                String otherString =
                        (String) getValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_SERVICE_LOCATION_OTHER);
                String otherDescriptionString = (String) getValueFromPropertyMap(propertyValues, dataDictionary,
                        ALIAS_SERVICE_LOCATION_OTHER_DESCRIPTION);

                boolean regularValue = regularString != null && regularString.endsWith("true") ? true : false;
                boolean otherValue = otherString != null && otherString.endsWith("true") ? true : false;

                String startDate = (String) propertyValues.get(START_DATE);

                if ((regularValue && otherValue) || (!regularValue && !otherValue)) {
                    // S-26365 required go away validation, but in the future it can be right logic
                    // make temporary comment out code
                    // Please reverse the implemented logic within the RI Standard IEP Template
                    // where is requires either a Y or N for Regular / Other fields (does not allow
                    // both to be Y or N).
                    errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                            Integer.valueOf(BusinessRules.REQUIRE_FIELD), "Services/Placement tab, " + serviceMode + ", "
                                    + startDate
                                    + " Start Date. Regular or Other checkbox . Only one checkbox can be selected."));
                }
                if (!otherValue && !StringUtils.isEmpty(otherDescriptionString)) {
                    errors.add(new ValidationError(
                            ValidationConstants.BUSINESS_RULE_VIOLATION,
                            Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                            "Services/Placement tab, "
                                    + serviceMode
                                    + ", "
                                    + startDate
                                    + " Start Date. Other Description can be filling only in case if selected Other checkbox."));
                }
            }

        }
        return errors;
    }

    private List<ValidationError> validateEarlyChildHoodTab(GenericDetail detail) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        Map<String, Object> propertyValues = detail.getPropertyValues();
        DataDictionary dataDictionary = detail.getDataDictionary();
        ReferenceCode refCode =
                getRefCodeByValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_EARLY_ENVIRONMENT);
        if (refCode != null && refCode.getCategoryIndicator()) {
            String subcodes = getSubCodes(refCode);
            errors.add(new ValidationError(
                    ValidationConstants.CUSTOM_ERROR,
                    null,
                    "Early Childhood tab, "
                            + " 'Select Environment' field."
                            + " Please, choose one available code: " + subcodes + " for '"
                            + refCode.getCode() + "'"));
        }
        if (refCode != null) {
            String prmName = (String) getValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_PRGM_NAME);
            String envCode = refCode.getCode();
            if (m_programNameRequire.contains(envCode) && StringUtils.isEmpty(prmName)) {
                errors.add(new ValidationError(
                        ValidationConstants.CUSTOM_ERROR,
                        null,
                        "Early Childhood tab, "
                                + " 'NAME OF EARLY CHILDHOOD PROGRAM' field."
                                + " Should be filled if 'Select Environment' field has next values: "
                                + m_programNameRequire
                                + ". Current 'Environemtn' value is " + envCode));
            }
        }
        String eiTranstion = getStateValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_EARLY_INTERVENTION);
        String eiTranstionReason = getStateValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_IEP_AFTER_3RD);
        if (EI_TRANSITION_DELAY.equals(eiTranstion)) {

            if (StringUtils.isEmpty(eiTranstionReason)) {
                errors.add(new ValidationError(
                        ValidationConstants.CUSTOM_ERROR,
                        null,
                        "Early Childhood tab, "
                                + " 'reason for the delay' field."
                                + " Should be filled if 'EARLY INTERVENTION TRANSITION' is " + EI_TRANSITION_DELAY));
            }
        }
        if (EI_TRANSITION_REASON_OTHER.equals(eiTranstionReason)) {
            String eiTranstionReasonOther =
                    (String) getValueFromPropertyMap(propertyValues, dataDictionary, ALIAS_AFTER_3RD_OTHER);
            if (StringUtils.isEmpty(eiTranstionReasonOther)) {
                errors.add(new ValidationError(
                        ValidationConstants.CUSTOM_ERROR,
                        null,
                        "Early Childhood tab, "
                                + " 'If 'other' was selected as the reason for delay please specify reason:' field."
                                + " Should be filled if 'reason for the delay' is " + EI_TRANSITION_REASON_OTHER));
            }
        }


        return errors;
    }


    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable =
                    (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    codeMap.put(code.getCode(), code);
                }
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }


}
