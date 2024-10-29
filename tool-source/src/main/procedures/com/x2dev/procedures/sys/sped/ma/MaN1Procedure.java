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
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedFieldAttributes;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.*;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class AssessmentProcedure.
 */
public class MaN1Procedure implements DynamicFormProcedure {


    /**
     *
     */
    private static final String COMMENT_END = "*/";
    /**
     *
     */
    private static final String COMMENT_START = "/*";
    /**
     *
     */
    private static final String END_BLOCK = ");";
    /**
     *
     */
    private static final String END_FUNCTION = "}";
    /**
     *
     */
    private static final String FUNCTION = "function()";
    /**
     *
     */
    private static final String NEW_LINE = "\r\n";
    /**
     *
     */
    private static final String NONE = "none";
    /**
     *
     */
    private static final String SCRIPT_END = "</script>";
    /**
     *
     */
    private static final String SCRIPT_START = "<script language=\"Javascript\">";
    /**
     *
     */
    private static final String SMALL = "small";
    /**
     *
     */
    private static final String START_BLOCK = "$(";
    /**
     *
     */
    private static final String START_FUNCTION = "{";
    /**
     *
     */
    private static final String USER_DATA_TYPE_COMMENT_BANK = "Comment Bank";

    private List<ModelProperty> m_commonBankProperties = new ArrayList<ModelProperty>();
    private String m_parentElemetnId = null;
    private String m_javaScriptElementId = null;
    private UserDataContainer m_userData = null;


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



        for (Property prop : template.getProperties()) {
            prop.getElementId();
            ModelProperty modelProp = prop.getModelProperty();
            DataDictionaryField field = modelProp.getField();
            ExtendedFieldAttributes attr = field.getExtendedDataField();
            if (attr != null) {
                if (attr.getUserType() != null && attr.getUserType().equals(USER_DATA_TYPE_COMMENT_BANK)) {
                    m_commonBankProperties.add(modelProp);
                }
            }

        }

        if (!m_commonBankProperties.isEmpty() && template.getFirstSection() != null
                && template.getFirstSection().getTab(1) != null)
        {
            Section section = template.getFirstSection();
            Tab tab = section.getTab(1);

            StringBuilder builder = new StringBuilder();
            builder.append(SCRIPT_START);
            builder.append(NEW_LINE);
            builder.append(COMMENT_START);
            builder.append(NEW_LINE);
            builder.append("* Onload.");
            builder.append(NEW_LINE);
            builder.append(COMMENT_END);
            builder.append(NEW_LINE);
            builder.append(START_BLOCK);
            builder.append(FUNCTION);
            builder.append(NEW_LINE);
            builder.append(START_FUNCTION);
            builder.append(NEW_LINE);
            builder.append("updateDynamicForm(\"changeOwner\");\r\n");
            builder.append(END_FUNCTION);
            builder.append(END_BLOCK);
            builder.append(NEW_LINE);
            builder.append(SCRIPT_END);
            builder.append(NEW_LINE);

            addScript(builder.toString(), tab, dictionary, locale);

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
        initialize(userData);

        if (!StringUtils.isEmpty(m_parentElemetnId) && !StringUtils.isEmpty(m_javaScriptElementId)) {
            Tab parentElement = (Tab) template.findElementById(m_parentElemetnId);
            TemplateElement targetElemetn = template.findElementById(m_javaScriptElementId);
            if (parentElement != null && targetElemetn != null) {
                parentElement.getChildren().remove(targetElemetn);
            }
        }

        if (!m_commonBankProperties.isEmpty() && template.getFirstSection() != null
                && template.getFirstSection().getTab(1) != null)
        {

            FormDetail formDetail = getFormDetail(detail);
            String iepDataOid = formDetail.getFormInstance().getOwnerObjectOid();
            StringBuilder builder = new StringBuilder();
            builder.append(SCRIPT_START);
            builder.append(NEW_LINE);
            builder.append(COMMENT_START);
            builder.append(NEW_LINE);
            builder.append("* Onload.");
            builder.append(NEW_LINE);
            builder.append(COMMENT_END);
            builder.append(NEW_LINE);
            builder.append(START_BLOCK);
            builder.append(FUNCTION);
            builder.append(NEW_LINE);
            builder.append(START_FUNCTION);
            builder.append(NEW_LINE);

            for (ModelProperty prop : m_commonBankProperties) {
                builder.append("$(\"textarea[name='propertyValue(");
                builder.append(prop.getFieldId());
                builder.append(")']\").attr('data-beanoid','");
                builder.append(iepDataOid);
                builder.append("');");
                builder.append(NEW_LINE);
            }

            builder.append(END_FUNCTION + END_BLOCK);
            builder.append(NEW_LINE);
            builder.append(SCRIPT_END);
            builder.append(NEW_LINE);

            Section section = template.getFirstSection();
            Tab tab = section.getTab(1);

            addScript(builder.toString(), tab, formDetail.getDataDictionary(), m_userData.getLocale());


            returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));

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


        return errors;
    }

    /**
     * add (block - line - cell - text) into input tab like first element
     * + add script into text element
     *
     * @param script
     * @param tab
     * @param ddx
     * @param locale
     */
    private void addScript(String script, Tab tab, DataDictionary ddx, Locale locale) {
        m_parentElemetnId = tab.getElementId();
        List existingChildren = tab.getChildren();
        List tempList = new ArrayList(existingChildren);
        existingChildren.clear();
        TextFormat textFormat = new TextFormat(SMALL, false, false, false);

        Text text = new Text(script, null, ddx, textFormat, locale);

        Cell cell = new Cell(new ArrayList(Arrays.asList(text)), 1, 1, NONE, null, null, null);
        Line line = new Line(new LinkedList<Cell>(Arrays.asList(cell)), null, null, NONE, null);
        Block block = new Block(new LinkedList<Line>(Arrays.asList(line)), -1, null);
        Row row = new Row(new ArrayList<ElementContainer>(Arrays.asList(block)), false, false);
        existingChildren.add(row);
        existingChildren.addAll(tempList);
        m_javaScriptElementId = row.getElementId();
    }

    /**
     * initialize clock
     *
     * @param userData
     */
    private void initialize(UserDataContainer userData)
    {
        m_userData = userData;
    }


    /**
     * Gets the form detail.
     *
     * @param detail GenericDetail
     * @return Form detail
     */
    private static FormDetail getFormDetail(GenericDetail detail) {
        FormDetail formDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            SisOutcomeDetail outcomeDetail = (SisOutcomeDetail) detail;
            formDetail = outcomeDetail.getCurrentFormDetail();
        }
        if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
        }
        return formDetail;
    }

}
