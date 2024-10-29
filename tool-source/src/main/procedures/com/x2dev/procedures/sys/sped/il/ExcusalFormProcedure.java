/* #PROCEDURE-ID [SYS-SPED-3457H] */
/*
 *
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
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
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container;
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
 * The Class ExcusalFormProcedure.
 */
public class ExcusalFormProcedure implements DynamicFormProcedure {

    /**
     * The Class IepExcusaltransferData.
     */
    private class IepExcusaltransferData implements RunTransferDataI {
        private FormDetail m_formDetail = null;
        private ModelBroker m_broker = null;
        private UserDataContainer m_userData = null;
        private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;
        private GenericFormData m_formStorage = null;
        private SpedTransferDataHelper m_transferDataHelper = null;
        private ModelPropertyCreatorI m_propertyCreator = null;
        private String m_alias = null;

        /**
         * Instantiates a new iep excusaltransfer data.
         *
         * @param userData UserDataContainer
         * @param formDetail FormDetail
         * @param alias String
         * @param broker ModelBroker
         */
        public IepExcusaltransferData(UserDataContainer userData, FormDetail formDetail, String alias,
                ModelBroker broker) {
            m_alias = alias;
            m_formDetail = formDetail;
            m_broker = broker;
            m_userData = userData;
            try {
                initialize();

            } catch (X2BaseException e) {
                // nothing to do
            }
        }

        /**
         * Execute.
         *
         * @param direction IepTransferDirection
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#execute(com.
         *      x2dev.procedures.sys.sped.il.SpedTransferDataHelper.IepTransferDirection)
         */
        @Override
        public void execute(IepTransferDirection direction) {
            DataDictionary ddx = m_formDetail.getDataDictionary();
            Map<String, String> propValues = m_formDetail.getPropertyValues();
            String notDiscussedKey = m_ilWorkflowHelper.translateAliasToProperty(ALIAS_NOT_DISCUSSED, ddx);
            m_formStorage.setFieldValueByAlias(ALIAS_NOT_DISCUSSED, propValues.get(notDiscussedKey), ddx);
            String discussedKey = m_ilWorkflowHelper.translateAliasToProperty(ALIAS_DISCUSSED, ddx);
            m_formStorage.setFieldValueByAlias(ALIAS_DISCUSSED, propValues.get(discussedKey), ddx);

            m_ilWorkflowHelper.initializeExcusalTemplate(m_formDetail.getFormInstance(), m_formStorage, m_userData,
                    m_alias, ddx);

        }



        /**
         * Gets the dirty property from foreign.
         *
         * @param key Pair<Integer,Integer>
         * @return Map
         * @throws UnsupportedOperationException exception
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#
         *      getDirtyPropertyFromForeign(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<Container, Map<String, String>> getDirtyPropertyFromForeign(Pair<Integer, Integer> key) {
            throw new UnsupportedOperationException();
        }

        /**
         * If team member/s had excused status - they will put into resident bean, and this method
         * return dirty properties from this bean .
         *
         * @param key Pair<Integer,Integer>
         * @return Map
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#
         *      getDirtyPropertyFromResident(com.sun.tools.javac.util.Pair)
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
         * Gets the keys.
         *
         * @return Sets the
         * @throws UnsupportedOperationException exception
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            throw new UnsupportedOperationException();
        }

        /**
         * intitialize fields and resources for this procedure.
         *
         * @throws X2BaseException exception
         */
        protected void initialize() throws X2BaseException {
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
         * @see SpedTransferDataHelper.ConverterHelper
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
         * @see SpedTransferDataHelper.ModelPropertyCreator
         */
        private ModelPropertyCreatorI newModelPropertyCreator(ModelBroker broker) {
            return m_transferDataHelper.new ModelPropertyCreator(broker);
        }


    }



    private static final String ALIAS_DISCUSSED = "discussed";
    private static final String ALIAS_NOT_DISCUSSED = "not-discussed";

    private Map<String, RunTransferDataI> m_foreignFieldsProcedures = new HashMap<String, RunTransferDataI>();



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
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(userData, formDetail, key, broker);
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
        FormDetail formDetail = getFormDetail(detail);
        IepExcusaltransferData data =
                (IepExcusaltransferData) getForeignFieldsProcedure(userData, formDetail, ALIAS_NOT_DISCUSSED, broker);
        IepExcusaltransferData data2 =
                (IepExcusaltransferData) getForeignFieldsProcedure(userData, formDetail, ALIAS_DISCUSSED, broker);
        validate(errors, data);
        validate(errors, data2);
        return errors;
    }


    /**
     * return procedure which help put team member/s with excused status into appropriate fields in
     * template.
     *
     * @param userData UserDataContainer
     * @param formDetail FormDetail
     * @param alias String
     * @param broker ModelBroker
     * @return Run transfer data I
     */
    private RunTransferDataI getForeignFieldsProcedure(UserDataContainer userData,
                                                       FormDetail formDetail,
                                                       String alias,
                                                       ModelBroker broker) {
        if (m_foreignFieldsProcedures.get(alias) == null) {
            IepExcusaltransferData proc = new IepExcusaltransferData(userData, formDetail, alias, broker);
            m_foreignFieldsProcedures.put(alias, proc);
        }
        return m_foreignFieldsProcedures.get(alias);
    }

    /**
     * try find and return form detail.
     *
     * @param detail GenericDetail
     * @return Form detail
     */
    private FormDetail getFormDetail(GenericDetail detail) {
        FormDetail formDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            SisOutcomeDetail outcomeDetail = (SisOutcomeDetail) detail;
            formDetail = outcomeDetail.getCurrentFormDetail();
        }
        if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
        }
        if (detail instanceof WorkflowMeetingDetail) {
            WorkflowMeetingDetail meetignDetail = (WorkflowMeetingDetail) detail;
            formDetail = meetignDetail.getCurrentFormDetail();
        }

        return formDetail;
    }



    /**
     * can validate data from <code>IepExcusaltransferData</code><br>
     * but now nothing to do .
     *
     * @param errors List<ValidationError>
     * @param data IepExcusaltransferData
     */
    private void validate(List<ValidationError> errors, IepExcusaltransferData data) {
        // nothing to do
    }

}
