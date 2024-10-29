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

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.DataCastType;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.IepTransferDirection;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ModelPropertyCreatorI;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI;
import com.x2dev.sis.web.sped.WorkflowMeetingDetail;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.tuple.Pair;

/**
 * The Class ParentNotificatoinOfConference.
 */
public class ParentNotificatoinOfConference implements DynamicFormProcedure {

    /**
     * The Class IepPNOFCForeignFieldsProcedure.
     */
    public class IepPNOFCForeignFieldsProcedure implements RunTransferDataI {

        private ModelBroker m_broker = null;
        private FormDetail m_formDetail = null;
        private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;
        private GenericFormData m_formStorage = null;
        private SpedTransferDataHelper m_transferDataHelper = null;
        private ModelPropertyCreatorI m_propertyCreator = null;
        private UserDataContainer m_userData = null;

        /**
         * Instantiates a new iep PNOFC foreign fields procedure.
         *
         * @param formDetail FormDetail
         * @param broker ModelBroker
         * @param userData UserDataContainer
         */
        IepPNOFCForeignFieldsProcedure(FormDetail formDetail, ModelBroker broker, UserDataContainer userData) {
            m_formDetail = formDetail;
            m_broker = broker;
            m_userData = userData;
            initialize();
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#execute(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.IepTransferDirection)
         */
        @Override
        public void execute(IepTransferDirection direction) {
            m_ilWorkflowHelper.initializeParentNotificationOfConference(m_formDetail.getFormInstance(), m_formStorage,
                    m_userData, m_formDetail.getDataDictionary());
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getDirtyPropertyFromForeign(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container, Map<String, String>> getDirtyPropertyFromForeign(Pair<Integer, Integer> key) {
            throw new UnsupportedOperationException();
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getDirtyPropertyFromResident(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<String, String> getDirtyPropertyFromResident(Pair<Integer, Integer> key) {
            Map<String, String> dirtyProperties = new HashMap<String, String>();
            for (Entry<String, Object> entry : m_formStorage.getDirtyMembers().entrySet()) {
                ModelProperty from = m_propertyCreator.create(entry.getKey(), m_formDetail.getRootClass(),
                        m_formDetail.getDataDictionary());
                CastInterface converterHelper = newConverterHelper(from, null, DataCastType.BEAN, DataCastType.UI);
                String value = (String) converterHelper.castObject(m_formStorage.getFieldValueByProperty(from));
                dirtyProperties.put(entry.getKey(), value);
            }

            return dirtyProperties;
        }

        /**
         * Initialize.
         */
        protected void initialize() {
            m_transferDataHelper = new SpedTransferDataHelper();
            m_propertyCreator = newModelPropertyCreator(m_broker);
            m_formStorage = X2BaseBean.newInstance(GenericFormData.class, m_broker.getPersistenceKey());
            m_ilWorkflowHelper =
                    new SpedIlWorkflowCommonProcedure(null, null, null, null, m_broker, Locale.getDefault());
        }


        /**
         * New converter helper.
         *
         * @param from ModelProperty
         * @param to ModelProperty
         * @param typeFrom DataCastType
         * @param typeTo DataCastType
         * @return CastInterface
         */
        private CastInterface newConverterHelper(ModelProperty from,
                                                 ModelProperty to,
                                                 DataCastType typeFrom,
                                                 DataCastType typeTo) {
            return m_transferDataHelper.new ConverterHelper(from, to, typeFrom, typeTo);
        }

        /**
         * New model property creator.
         *
         * @param broker ModelBroker
         * @return ModelPropertyCreatorI
         */
        private ModelPropertyCreatorI newModelPropertyCreator(ModelBroker broker) {
            return m_transferDataHelper.new ModelPropertyCreator(broker);
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            throw new UnsupportedOperationException();
        }
    }


    private RunTransferDataI m_foreignFieldsProcedure = null;

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

        ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
        FormDetail formDetail = getFormDetail(detail);
        Map<String, String> prop = new HashMap<String, String>();
        if (formDetail != null) {
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(formDetail, broker, userData);
            fieldsProcedure.execute(IepTransferDirection.UPDATE);
            prop = fieldsProcedure.getDirtyPropertyFromResident(null);
        }
        Map<String, Object> returnMap = new HashMap<String, Object>();
        returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
        // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
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
     * Gets the foreign fields procedure.
     *
     * @param formDetail FormDetail
     * @param broker ModelBroker
     * @param userData UserDataContainer
     * @return Run transfer data I
     */
    private RunTransferDataI getForeignFieldsProcedure(FormDetail formDetail,
                                                       ModelBroker broker,
                                                       UserDataContainer userData) {
        if (m_foreignFieldsProcedure == null) {
            m_foreignFieldsProcedure = new IepPNOFCForeignFieldsProcedure(formDetail, broker, userData);
        }
        return m_foreignFieldsProcedure;
    }


    /**
     * Gets the form detail.
     *
     * @param detail GenericDetail
     * @return Form detail
     */
    private FormDetail getFormDetail(GenericDetail detail) {
        FormDetail formDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            SisOutcomeDetail outcomeDetail = (SisOutcomeDetail) detail;
            formDetail = outcomeDetail.getCurrentFormDetail();
        } else if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
        } else if (detail instanceof WorkflowMeetingDetail) {
            WorkflowMeetingDetail workflowMeetingDetail = (WorkflowMeetingDetail) detail;
            formDetail = workflowMeetingDetail.getCurrentFormDetail();

        }
        return formDetail;
    }



}
