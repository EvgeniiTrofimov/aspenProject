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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.*;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;


/**
 * This template procedure must be used for RI clients to modify the general.iac popup template to
 * restrict the iacCategory values.
 *
 * It is not included in the schema definition and should only be used at RI at this time.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class RIGeneralIacTemplateProcedure implements DynamicFormProcedure {
    private static final String END_JAVASCRIPT = "</script>";
    private static final String START_JAVASCRIPT = "<script language=\"Javascript\">";
    private static final String NEW_LINE = "\r\n";
    // Limit iacCategory to five values
    private static final String[] RI_SCRIPT = {
            "$(function() {",
            "var include = 'include';",
            "var iacCategories = {};",
            "iacCategories[''] = include;",
            "iacCategories['Universal Features'] = include;",
            "iacCategories['Designated Features'] = include;",
            "iacCategories['English Learners'] = include;",
            "iacCategories['General'] = include;",
            "$('select[name^=\"propertyValue(iacCategory)\"]').children('option').each(function () {",
            "    if (iacCategories[this.value] != include) {",
            "        this.remove();",
            "    }",
            "});",
            "});"
    };

    /**
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

        ModelProperty targetModelProperty =
                new ModelProperty(IepAccommodation.class, IepAccommodation.COL_CATEGORY, dictionary);

        Section section = template.getFirstSection();
        if (section != null) {
            for (Tab tab : section.getTabs()) {
                if (tab.getModelProperties().contains(targetModelProperty)) {
                    Set<Property> properties = tab.getProperty(targetModelProperty);
                    if (properties.size() == 1) {
                        TextFormat textFormat = new TextFormat("small", false, false, false);
                        StringBuilder textForView = new StringBuilder();
                        textForView.append(NEW_LINE);
                        textForView.append(NEW_LINE);

                        // Add javascript here
                        textForView.append(START_JAVASCRIPT);
                        textForView.append(NEW_LINE);
                        for (String line : RI_SCRIPT) {
                            textForView.append(line);
                            textForView.append(NEW_LINE);
                        }
                        textForView.append(END_JAVASCRIPT);
                        textForView.append(NEW_LINE);

                        textForView.append(NEW_LINE);
                        Text text = new Text(textForView.toString(), null, dictionary, textFormat, locale);

                        List<TemplateElement> elements = new ArrayList(1);
                        elements.add(text);
                        Cell cell = new Cell(elements, 0, 0, null, null, null, null);
                        List<ElementContainer> containers = new ArrayList(1);
                        containers.add(cell);
                        Line line = new Line(containers, null, null, null, null);
                        containers = new ArrayList(1);
                        containers.add(line);
                        Block block = new Block(containers, 0, null);
                        containers = new ArrayList(1);
                        containers.add(block);
                        Row row = new Row(containers, false, false);
                        ((List<ElementContainer>) tab.getChildren()).add(row);
                    }
                }
            }
        }
    }

    /**
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
        return null;
    }

    /**
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

}
