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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.ElementContainer;
import com.follett.fsc.core.k12.web.template.Property;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.TemplateElement;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class IepRiProcedure.
 */
public class IepMAProcedure implements DynamicFormProcedure {
    private static final String ALIAS_SPED_DECISION_NOTES = "sped-decision-notes";
    private static final String ALIAS_TRANSITION_PLANNING_DATE = "trplan-date-completed";
    private static final String ALIAS_CH688_RECOMMENDED = "transition-ch688-recommended";

    private static final String DEFAULT_PRIMARY_DISAB = "No Primary Disability Set";

    private static final String FIELD_ID_IEP_END_DATE = "iepEndDate";
    private static final String FIELD_ID_IEP_START_DATE = "iepStartDate";
    private static final String FIELD_ID_ISV_END_DATE = "isvEndDate";
    private static final String FIELD_ID_ISV_START_DATE = "isvStartDate";

    private static final String FORM_ID_TPF = "TPF";
    private static final String FORM_ID_CH688N = "CH688N";

    private static final String JAVASCRIPT_KEY_AUTISM_DISAB = "autism-disab";
    private static final String JAVASCRIPT_KEY_PRIMARY_DISAB = "primary-disab";
    private static final String JAVASCRIPT_KEY_SERVICES_DATES = "service-dates";

    private static final String LIST_ID_AUTISM_DISAB = "disabilityResp";
    private static final String LIST_ID_PRIMARY_DISAB = "DOE36";

    private static final String PROPERTY_AUTISM_DISAB = "autismDisab";
    private static final String PROPERTY_PRIMARY_DISAB = "primaryDisab";
    private static final String PROPERTY_SERVICES_DATES = "isvCycleDays";

    private static final String TEXT_AUTISM = "Autism";

    private DataDictionaryField m_fieldTransitionPlanningDate;

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
        IepData iep = (IepData) broker.getBeanByOid(IepData.class, detail.getOid());
        String formDefinitionOid = getFormDefinitonOid(broker, FORM_ID_TPF);
        if (iep != null) {
            if (!StringUtils.isEmpty(formDefinitionOid) && m_fieldTransitionPlanningDate != null) {
                String beanPath = m_fieldTransitionPlanningDate.getJavaName();
                String value = (String) iep.getFieldValueByBeanPath(beanPath);
                if (!StringUtils.isEmpty(value)) {
                    FormInstance instance = getFormInstance(broker, formDefinitionOid, iep.getOid(), iep.getOid());
                    if (instance == null) {
                        instance = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                        instance.setFormDefinitionOid(formDefinitionOid);
                        instance.setOwnerObjectOid(iep.getOid());
                        instance.setStorageObjectOid(iep.getOid());
                        instance.setCreatedTime(System.currentTimeMillis());
                        instance.setOwnerView(iep.getFirstIdentifyingValue());
                        errors.addAll(broker.saveBean(instance));
                    }
                }
            }
            createCh688Form(broker, iep);
        }
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
        m_fieldTransitionPlanningDate = dictionary.findDataDictionaryFieldByAlias(ALIAS_TRANSITION_PLANNING_DATE);
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SPED_DECISION_NOTES);
        if (field != null && field.getTable() != null) {
            removeDuplicateField(template.getSections(), field,
                    field.getTable().getObjectPrefix().toLowerCase().equals(IepData.OBJECT_PREFIX.toLowerCase()));
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
            if (key.equals(JAVASCRIPT_KEY_SERVICES_DATES)) {
                if (!detail.getCurrentChild().isNew()) {
                    String cycleDays =
                            (String) detail.getCurrentChild().getPropertyValues().get(PROPERTY_SERVICES_DATES);
                    prop.put(PROPERTY_SERVICES_DATES, cycleDays);
                }
                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_START_DATE))) {
                    String iepStartDate = (String) detail.getValue(FIELD_ID_IEP_START_DATE);
                    if (!StringUtils.isEmpty(iepStartDate)) {
                        prop.put(FIELD_ID_ISV_START_DATE, iepStartDate);
                    }
                }
                if (StringUtils
                        .isEmpty((String) detail.getCurrentChild().getPropertyValues().get(FIELD_ID_ISV_END_DATE))) {
                    String iepEndDate = (String) detail.getValue(FIELD_ID_IEP_END_DATE);
                    if (!StringUtils.isEmpty(iepEndDate)) {
                        prop.put(FIELD_ID_ISV_END_DATE, iepEndDate);
                    }
                }
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            } else if (key.equals(JAVASCRIPT_KEY_AUTISM_DISAB)) {
                List<GenericDetail> test =
                        new ArrayList<GenericDetail>(detail.getChildDetailSet(LIST_ID_AUTISM_DISAB).getChildDetails());
                for (GenericDetail gd : test) {
                    if (gd.getPropertyValues().get(SisBeanPaths.STUDENT_DISABILITY.disabilityCode().getColumnOid())
                            .equals(TEXT_AUTISM)) {
                        prop.put(PROPERTY_AUTISM_DISAB, "1");
                    }
                }

                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            } else if (key.equals(JAVASCRIPT_KEY_PRIMARY_DISAB)) {
                String disability = DEFAULT_PRIMARY_DISAB;
                List<GenericDetail> list =
                        new ArrayList<GenericDetail>(detail.getChildDetailSet(LIST_ID_PRIMARY_DISAB).getChildDetails());
                if (list != null && !list.isEmpty()) {
                    disability = (String) list.iterator().next().getPropertyValues()
                            .get(SisBeanPaths.STUDENT_DISABILITY.disabilityCode().getColumnOid());
                }
                prop.put(PROPERTY_PRIMARY_DISAB, disability);
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
     * Gets the form instance.
     *
     * @param broker ModelBroker
     * @param formDefinitionOid String
     * @param iepOid String
     * @param iepOid2 String
     * @return Form instance
     */
    private FormInstance getFormInstance(ModelBroker broker, String formDefinitionOid, String iepOid, String iepOid2) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, formDefinitionOid);
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iepOid);
        criteria.addEqualTo(FormInstance.COL_STORAGE_OBJECT_OID, iepOid);
        FormInstance bean = (FormInstance) broker.getBeanByQuery(new BeanQuery(FormInstance.class, criteria));
        return bean;
    }

    /**
     * Gets the form definiton oid.
     *
     * @param broker ModelBroker
     * @param formId String
     * @return String
     */
    private String getFormDefinitonOid(ModelBroker broker, String formId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, formId);
        X2BaseBean bean = broker.getBeanByQuery(new BeanQuery(FormDefinition.class, criteria));
        return bean == null ? null : bean.getOid();
    }

    /**
     * Removes the appropriate sped-decision-notes field for either iep or std.
     *
     * @param items List<? extends TemplateElement>
     * @param field DataDictionaryField
     * @param iepField boolean
     */
    private void removeDuplicateField(List<? extends TemplateElement> items,
                                      DataDictionaryField field,
                                      boolean iepField) {
        List<TemplateElement> itemsToRemove = new LinkedList();
        for (TemplateElement child : items) {
            if (child instanceof Property) {
                ModelProperty property = ((Property) child).getModelProperty();
                if (field.equals(property.getField())) {
                    if (iepField && !property.getField().equals(property.getRootField())) {
                        itemsToRemove.add(child);
                    } else if (!iepField && property.getField().equals(property.getRootField())) {
                        itemsToRemove.add(child);
                    }
                }
            } else if (child instanceof ElementContainer) {
                removeDuplicateField(((ElementContainer) child).getChildren(), field, iepField);
            }
        }
        items.removeAll(itemsToRemove);
    }

    /**
     * Create a new instance of the Chapter 688 Referral form if the form does not already exist
     * and
     * the referral option is selected.
     *
     * @param broker
     * @param iep
     */
    private void createCh688Form(ModelBroker broker, IepData iep) {
        ExtendedDictionaryAttributes iepExtendedDictionary = iep.getExtendedDataDictionary();
        DataDictionary iepDictionary =
                DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());

        DataDictionaryField fieldCh688Recommended =
                iepDictionary.findDataDictionaryFieldByAlias(ALIAS_CH688_RECOMMENDED);
        if (fieldCh688Recommended != null) {
            String ch688Recommended =
                    (String) iep.getFieldValueByBeanPath(fieldCh688Recommended.getJavaName());
            String ch688FormDefinitionOid = getFormDefinitonOid(broker, FORM_ID_CH688N);
            if ("Yes".equals(ch688Recommended) && ch688FormDefinitionOid != null) {
                // Make sure there is not already an existing instance of the form.
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, ch688FormDefinitionOid);
                criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
                BeanQuery query = new BeanQuery(FormInstance.class, criteria);
                X2BaseBean instance = broker.getBeanByQuery(query);
                if (instance == null) {
                    // Create a new instance of the form.
                    FormInstance ch688Form = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                    ch688Form.setFormDefinitionOid(ch688FormDefinitionOid);
                    ch688Form.setCreatedTime(System.currentTimeMillis());
                    ch688Form.setOwnerObjectOid(iep.getOid());
                    ch688Form.setStorageObjectOid(iep.getOid());
                    ch688Form.setOwnerView(iep.getFirstIdentifyingValue());

                    broker.saveBeanForced(ch688Form);
                }
            }
        }
    }
}
