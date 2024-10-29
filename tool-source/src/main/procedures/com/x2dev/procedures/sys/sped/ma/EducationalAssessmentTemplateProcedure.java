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
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class EducationalAssessmentTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class EducationalAssessmentTemplateProcedure implements DynamicFormProcedure {
    private static final String ALIAS_STAFF_OID = "owner-staff-oid";

    private ModelBroker m_broker;
    private String m_ddxOid;
    private DataDictionaryField m_fieldNameStaffOid;
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
        m_ddxOid = template.getExtendedDataDictionaryOid();
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
        Map<String, Object> returnMap = null;
        DataDictionaryField staffOidField = getStaffOidField();
        if (staffOidField != null) {
            Staff staff = userData.getUser().getPerson().getStaff();
            String staffOidFieldOid = staffOidField.getDataFieldConfig().getDataFieldOid().trim();
            Object currentValue = detail.getValue(staffOidFieldOid);
            if ((detail.getCurrentRecord(userData) == null || detail.getCurrentRecord(userData).isNew())
                    && StringUtils.isEmpty((String) currentValue) && staff != null) {
                detail.setValue(staffOidFieldOid, staff.getOid());
                detail.setValue("stfNameView", staff.getNameView());
                returnMap = new HashMap<String, Object>();
                Map<String, Object> prop = new HashMap<String, Object>();
                prop.put(staffOidFieldOid, staff.getOid());
                returnMap = new HashMap<String, Object>();
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
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
     * Gets the staff oid field.
     *
     * @return Data dictionary field
     */
    private DataDictionaryField getStaffOidField() {
        if (m_fieldNameStaffOid == null) {
            if (!StringUtils.isEmpty(m_ddxOid)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(X2BaseBean.COL_OID, m_ddxOid);
                ExtendedDataDictionary ddx =
                        (ExtendedDataDictionary) getBroker()
                                .getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
                if (ddx != null) {
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                    m_fieldNameStaffOid = dictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_OID);
                }
            }

        }
        return m_fieldNameStaffOid;
    }
}
