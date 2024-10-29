/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.ElementContainer;
import com.follett.fsc.core.k12.web.template.EmbeddedList;
import com.follett.fsc.core.k12.web.template.Section;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.TemplateElement;
import com.follett.fsc.core.k12.web.template.TemplateException;
import com.follett.fsc.core.k12.web.template.TemplateParser;
import com.follett.fsc.core.k12.web.template.Text;
import com.x2dev.utils.X2BaseException;
import java.io.StringReader;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class IepConfigurationTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class IepConfigurationTemplateProcedure implements DynamicFormProcedure {
    private static final String DDX_ORA_ID = "ORA-MA-SPED-CFIG";

    private static final String[] EMBEDDED_LIST_FRAGMENT = {
            "<template><tab><row show-border=\"false\"><block><line><cell>",
            "<embedded-list id=\"sped\" extended-dictionary-oid=\"{DDX-ID}\" relationship=\"relOrgOraOid\" embedded-edit=\"true\" show-add-button=\"false\" show-delete-button=\"false\" detail-context=\"district.dst.list.detail.ma.iep\">",
            "<field alias=\"ora-ma-sped-mult-forms\" sort=\"true\" />",
            "</embedded-list>",
            "</cell></line></block></row></tab></template>"
    };
    private static final String EMBEDDED_SEARCH = "EMBEDDED_LIST";

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

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ORA_ID);
        ExtendedDataDictionary ddx =
                (ExtendedDataDictionary) getBroker()
                        .getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
        if (ddx != null) {
            // make sure the organization attribute exists
            MaSpedWorkflowProcedure.getSpedConfig(getBroker());

            Section section = template.getFirstSection();
            if (section != null) {
                ElementContainer parent = findInsertParent(section);
                if (parent != null) {
                    EmbeddedList list =
                            getEmbeddedListFromTemplate(dictionary, applicationContext, privilegeSet, locale, ddx);
                    if (list == null) {
                        list = createEmbeddedList(dictionary, applicationContext, privilegeSet, locale, ddx);
                    }

                    for (TemplateElement item : parent.getChildren()) {
                        if (item instanceof Text && EMBEDDED_SEARCH.equals(((Text) item).getText())) {
                            ((List<TemplateElement>) parent.getChildren()).remove(item);
                            ((List<TemplateElement>) parent.getChildren()).add(list);
                            break;
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
     * Creates the embedded list.
     *
     * @param dictionary DataDictionary
     * @param applicationContext ApplicationContext
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @param ddx ExtendedDataDictionary
     * @return EmbeddedList
     */
    private EmbeddedList createEmbeddedList(DataDictionary dictionary,
                                            ApplicationContext applicationContext,
                                            PrivilegeSet privilegeSet,
                                            Locale locale,
                                            ExtendedDataDictionary ddx) {
        EmbeddedList list = null;
        List<ModelProperty> modelProperties = new LinkedList();
        DataDictionary extendedDictionary =
                DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        for (String aliasName : MaSpedWorkflowProcedure.ALIAS_NAMES) {
            DataDictionaryField dictionaryField =
                    extendedDictionary.findDataDictionaryFieldByAlias(aliasName);
            if (dictionaryField != null) {
                modelProperties.add(new ModelProperty(dictionaryField.getTable().getBeanClass(),
                        dictionaryField.getJavaName(), getBroker().getPersistenceKey()));
                break;
            }
        }
        try {
            list = new EmbeddedList(null, // String action,
                    null, // String detailClass,
                    null, // String multiAddAction,
                    "right", // String buttonPosition,
                    false, // boolean embeddedEdit,
                    false, // boolean showAddButton,
                    false, // boolean showBlobText,
                    false, // boolean showDeleteButton,
                    true, // boolean showDetails,
                    0, // int deleteIconColumn,
                    false, // boolean warnOnDelete,
                    null, // String warnOnDeleteResourceKey,
                    false, // boolean autoResize,
                    false, // boolean readOnly,
                    false, // boolean independentSave,
                    "multiple", // String selectionType,
                    null, // String label,
                    null, // List<Filter> filters,
                    modelProperties, // List<ModelProperty> displayProperties,
                    "relOrgOraOid", // String relationshipIds,
                    ddx.getOid(), // String extendedDictionaryOid,
                    "sped", // String id,
                    null, // List<NavigationValue> systemValues,
                    "district.dst.list.detail.ma.iep", // String detailContext,
                    null, // String extraCreatePrivileges,
                    null, // String extraDeletePrivileges,
                    null, // String extraUpdatePrivileges,
                    null, // String popupWidth,
                    null, // String popupHeight,
                    privilegeSet, // PrivilegeSet privilegeSet,
                    dictionary, // DataDictionary dictionary,
                    applicationContext, // ApplicationContext applicationContext,
                    ddx.getOrganization1(), // Organization district,
                    locale // Locale locale
            );
        } catch (TemplateException e) {
            // null in not created case
        }
        return list;
    }

    /**
     * Find embedded list.
     *
     * @param container ElementContainer
     * @return EmbeddedList
     */
    private EmbeddedList findEmbeddedList(ElementContainer container) {
        EmbeddedList list = null;
        for (TemplateElement child : container.getChildren()) {
            if (child instanceof EmbeddedList) {
                list = (EmbeddedList) child;
                break;
            } else if (child instanceof ElementContainer) {
                list = findEmbeddedList((ElementContainer) child);
                if (list != null) {
                    break;
                }
            }
        }
        return list;
    }

    /**
     * Find insert parent.
     *
     * @param container ElementContainer
     * @return ElementContainer
     */
    private ElementContainer findInsertParent(ElementContainer container) {
        ElementContainer parent = null;

        for (TemplateElement child : container.getChildren()) {
            if (child instanceof Text && EMBEDDED_SEARCH.equals(((Text) child).getText())) {
                parent = container;
            } else if (child instanceof ElementContainer) {
                parent = findInsertParent((ElementContainer) child);
            }

            if (parent != null) {
                break;
            }
        }

        return parent;
    }

    /**
     * Gets the embedded list from template.
     *
     * @param dictionary DataDictionary
     * @param applicationContext ApplicationContext
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @param ddx ExtendedDataDictionary
     * @return Embedded list
     */
    private EmbeddedList getEmbeddedListFromTemplate(DataDictionary dictionary,
                                                     ApplicationContext applicationContext,
                                                     PrivilegeSet privilegeSet,
                                                     Locale locale,
                                                     ExtendedDataDictionary ddx) {
        EmbeddedList list = null;
        StringBuilder xmlString = new StringBuilder();
        for (String fragment : EMBEDDED_LIST_FRAGMENT) {
            xmlString.append(fragment.replace("{DDX-ID}", ddx.getOid()));
        }

        TemplateParser parser = new TemplateParser(privilegeSet,
                dictionary,
                applicationContext,
                ddx.getOrganization1(),
                locale);
        Template template = null;
        try {
            template = parser.parse(null, "name", new StringReader(xmlString.toString()), false, false, "Description",
                    null);
        } catch (TemplateException e) {
            // template == null in parse failure case
        }

        if (template != null) {
            list = findEmbeddedList(template.getFirstSection());
        }
        return list;
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

}
