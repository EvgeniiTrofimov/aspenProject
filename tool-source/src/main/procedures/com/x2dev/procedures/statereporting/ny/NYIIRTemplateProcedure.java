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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.struts.action.ActionErrors;

/**
 * The Class NYIIRTemplateProcedure.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class NYIIRTemplateProcedure implements DynamicFormProcedure {
    private static final String ALIAS_IRR_DATE = "date";

    private static final String ALIAS_OFFENDER_AGE = "irr-offender-age";
    private static final String ALIAS_OFFENDER_NAME = "irr-offender-name";
    private static final String ALIAS_OFFENDER_GRADE = "irr-offender-grade";

    private static final String ALIAS_VICTIM_AGE = "irr-victim-age";
    private static final String ALIAS_VICTIM_GRADE = "irr-victim-grade";
    private static final String ALIAS_VICTIM_NAME = "irr-victim-name";

    private static final String KEY_VERIFY_STUDENT_DETAIL = "verifyStudentDetail";
    private static final String STUDENT_OID_MATCH = "StdOID";

    private ModelBroker m_broker;
    private DateAsStringConverter m_converter;
    private DataDictionary m_dictionary;
    private String m_ddxOid;
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

        if (KEY_VERIFY_STUDENT_DETAIL.equals(key)) {
            boolean refreshNeeded = false;

            for (ChildDetailSet childSet : detail.getChildDetailSets()) {
                for (GenericDetail child : childSet.getChildDetails()) {
                    SisStudent student = getStudent(child.getPropertyValues());
                    if (student != null) {
                        // Check and process if offender
                        String offenderNameKey = getDictionary().findDataDictionaryFieldByAlias(ALIAS_OFFENDER_NAME)
                                .getDataFieldConfig().getDataFieldOid().trim();
                        if (child.getPropertyValues().containsKey(offenderNameKey)) {
                            if (!student.getNameView().equals(child.getValue(offenderNameKey))) {
                                refreshNeeded = true;
                                child.getPropertyValues().put(offenderNameKey, student.getNameView());

                                PlainDate incidentDate = getIncidentDate(detail);
                                String offenderAgeKey =
                                        getDictionary().findDataDictionaryFieldByAlias(ALIAS_OFFENDER_AGE)
                                                .getDataFieldConfig().getDataFieldOid().trim();
                                int age = student.getPerson().getAgeAsOfDate(incidentDate);
                                child.getPropertyValues().put(offenderAgeKey, Integer.toString(age));

                                String offenderGradeKey =
                                        getDictionary().findDataDictionaryFieldByAlias(ALIAS_OFFENDER_GRADE)
                                                .getDataFieldConfig().getDataFieldOid().trim();
                                child.getPropertyValues().put(offenderGradeKey, student.getGradeLevel());
                            }
                        }

                        // Check and process if victim
                        String victimNameKey = getDictionary().findDataDictionaryFieldByAlias(ALIAS_VICTIM_NAME)
                                .getDataFieldConfig().getDataFieldOid().trim();
                        if (child.getPropertyValues().containsKey(victimNameKey)) {
                            if (!student.getNameView().equals(child.getValue(victimNameKey))) {
                                refreshNeeded = true;
                                child.getPropertyValues().put(victimNameKey, student.getNameView());

                                PlainDate incidentDate = getIncidentDate(detail);
                                String victimAgeKey =
                                        getDictionary().findDataDictionaryFieldByAlias(ALIAS_VICTIM_AGE)
                                                .getDataFieldConfig().getDataFieldOid().trim();
                                int age = student.getPerson().getAgeAsOfDate(incidentDate);
                                child.getPropertyValues().put(victimAgeKey, Integer.toString(age));

                                String victimGradeKey =
                                        getDictionary().findDataDictionaryFieldByAlias(ALIAS_VICTIM_GRADE)
                                                .getDataFieldConfig().getDataFieldOid().trim();
                                child.getPropertyValues().put(victimGradeKey, student.getGradeLevel());
                            }
                        }
                    }
                }
            }

            if (refreshNeeded) {
                returnMap = new HashMap<String, Object>();
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
     * Gets the date converter.
     *
     * @param detail GenericDetail
     * @return Date as string converter
     */
    private DateAsStringConverter getDateConverter(GenericDetail detail) {
        if (m_converter == null) {
            m_converter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    detail.getLocale(), true);
        }
        return m_converter;
    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDictionary() {
        if (m_dictionary == null) {
            if (!StringUtils.isEmpty(m_ddxOid)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(X2BaseBean.COL_OID, m_ddxOid);
                ExtendedDataDictionary ddx =
                        (ExtendedDataDictionary) getBroker()
                                .getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
                if (ddx != null) {
                    m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                }
            }
            if (m_dictionary == null) {
                m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            }
        }
        return m_dictionary;
    }

    /**
     * Gets the incident date.
     *
     * @param detail GenericDetail
     * @return Plain date
     */
    private PlainDate getIncidentDate(GenericDetail detail) {
        PlainDate date = null;
        DataDictionaryField dateField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IRR_DATE);
        if (dateField != null) {
            String key = dateField.getDataFieldConfig().getDataFieldOid().trim();
            String dateString = (String) detail.getValue(key);
            ActionErrors actionErrors = new ActionErrors();
            dateString = (String) GenericDetail.getTypedValue(dateField, dateString, detail.getLocale(), actionErrors);
            date = (PlainDate) getDateConverter(detail).parseSystemString(dateString);
        }
        return date;
    }

    /**
     * Gets the student.
     *
     * @param propertyValues Map
     * @return Sis student
     */
    private SisStudent getStudent(Map propertyValues) {
        SisStudent student = null;
        String studentOid = null;
        for (Object key : propertyValues.keySet()) {
            if (key instanceof String) {
                String candidate = (String) key;
                if (candidate.contains(STUDENT_OID_MATCH)) {
                    studentOid = (String) propertyValues.get(key);
                    break;
                }
            }
        }
        if (!StringUtils.isEmpty(studentOid)) {
            student = getBroker().getBeanByOid(SisStudent.class, studentOid);
        }
        return student;
    }


}
