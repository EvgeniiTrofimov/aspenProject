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

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
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
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.tuple.Pair;

/**
 * The Class FactsProcedure.
 */
public class FactsProcedure implements DynamicFormProcedure {


    /**
     * The Class IepFactsForeignFieldsProcedure.
     */
    public class IepFactsForeignFieldsProcedure implements RunTransferDataI {
        private static final String ALIAS_FTS_EXIT_DATE = "fts-exit-date";
        private static final String ALIAS_FTS_EXIT_REASON = "fts-exit-reason";
        private static final String ALIAS_DOE_SCHOOL_LANG = "DOE SCHOOL LANG";
        private static final String ALIAS_FTS_SCHOOL_LANG = "fts-school-lang";
        private static final String ALIAS_FTS_IS_UPDATE = "fts-is-update";
        private static final String FACTS_IDENTIFIER = "facts";
        private static final String WAS_LOADED = "Was Loaded";
        private ModelBroker m_broker = null;
        private FormDetail m_formDetail = null;
        private TransferContainerProcessor m_containerProcessor = null;
        private SpedTransferDataHelper m_transferDataHelper = null;
        private boolean m_isUpdate = false;
        private int loadCount = 0;
        private ModelProperty m_isUpdateProperty = null;
        private Set<Pair<Integer, Integer>> m_keys = new HashSet<Pair<Integer, Integer>>();


        /**
         * Instantiates a new iep facts foreign fields procedure.
         *
         * @param formDetail FormDetail
         * @param broker ModelBroker
         */
        public IepFactsForeignFieldsProcedure(FormDetail formDetail, ModelBroker broker) {
            m_formDetail = formDetail;
            m_broker = broker;
            initialize();
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#execute(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.IepTransferDirection)
         */
        @Override
        public void execute(IepTransferDirection direction) {
            try {
                if (direction.equals(IepTransferDirection.UPDATE)) {
                    loadCount++;
                }
                if (direction.equals(IepTransferDirection.UPDATE) && m_isUpdate == false) {
                    m_containerProcessor.transferFromForeignToResident();
                    m_isUpdate = true;
                    for (Pair<Integer, Integer> key : m_keys) {
                        m_containerProcessor.getResidentContainer(key).setFieldValueByProperty(m_isUpdateProperty,
                                WAS_LOADED);
                        break;
                    }

                } else if (direction.equals(IepTransferDirection.SET) && m_isUpdate == true) {
                    m_containerProcessor.transferFromResidentToForeign();
                    m_containerProcessor.clearResidentFields(new ArrayList<EmbeddedListDetailSet>());
                    for (Pair<Integer, Integer> key : m_keys) {
                        m_containerProcessor.getResidentContainer(key).setFieldValueByProperty(m_isUpdateProperty,
                                null);
                        break;
                    }
                }
            } finally {
                if (direction.equals(IepTransferDirection.SET)) {
                    m_isUpdate = false;
                }
            }

        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            return m_keys;
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getDirtyPropertyFromForeign(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<Container, Map<String, String>> getDirtyPropertyFromForeign(Pair<Integer, Integer> key) {
            throw new UnsupportedOperationException();
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getDirtyPropertyFromResident(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<String, String> getDirtyPropertyFromResident(Pair<Integer, Integer> key) {
            Map<String, String> returnMap = new HashMap<String, String>();
            if (loadCount == 1) {
                Container residentContainer = m_containerProcessor.getResidentContainer(key);
                Map<ModelProperty, Object> dirtyProperties = residentContainer.getDirtyPropertyValues();
                for (Entry<ModelProperty, Object> entry : dirtyProperties.entrySet()) {
                    ModelProperty from = entry.getKey();
                    Object value = entry.getValue();
                    String stringValue = null;
                    if (residentContainer.getCastType().equals(DataCastType.UI)) {
                        stringValue = (String) value;

                    } else if (residentContainer.getCastType().equals(DataCastType.BEAN)) {
                        CastInterface converter = newConverterHelper(from, null, DataCastType.BEAN, DataCastType.UI);
                        stringValue = (String) converter.castObject(value);
                    }
                    returnMap.put(from.getDictionaryPath(), stringValue);
                }
            }
            return returnMap;
        }

        /**
         * Initialize.
         */
        protected void initialize() {
            m_transferDataHelper = new SpedTransferDataHelper();
            Map<String, String> propertyValue = m_formDetail.getPropertyValues();
            FormInstance formInstance = m_formDetail.getFormInstance();
            DataDictionary dataDictionaryResident =
                    DataDictionary.getDistrictDictionary(formInstance.getFormDefinition().getExtendedDataDictionary(),
                            m_broker.getPersistenceKey());
            Container residentContainer =
                    newMapContainer(propertyValue, GenericFormData.class, FACTS_IDENTIFIER, dataDictionaryResident);
            m_containerProcessor = newTransferContainerProcessor(m_broker);
            ModelPropertyCreatorI prepertyCreator = newModelPropertyCreator(m_broker);
            m_isUpdateProperty =
                    prepertyCreator.create(ALIAS_FTS_IS_UPDATE, GenericFormData.class, dataDictionaryResident);

            DataDictionary dataDictionaryForeign = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            IepData iepData = (IepData) m_formDetail.getFormInstance().getOwner();
            Container foreignContainerIep = newBeanContainer(iepData, dataDictionaryForeign);
            Student student = iepData.getStudent();
            Container foreignContainerStd = newBeanContainer(student, dataDictionaryForeign);

            List<String> residentFieldsForIep =
                    new ArrayList<String>(Arrays.asList(ALIAS_FTS_EXIT_DATE, ALIAS_FTS_EXIT_REASON));
            List<String> iepDataFields =
                    new ArrayList<String>(Arrays.asList(IepData.COL_EXIT_DATE, IepData.COL_EXIT_REASON));

            Pair<Integer, Integer> key =
                    m_containerProcessor.registerContainers(residentContainer, foreignContainerIep);
            boolean hasErrors = false;
            if (key != null) {
                hasErrors = !m_containerProcessor.registerFields(residentFieldsForIep, iepDataFields);
                if (!hasErrors) {
                    m_keys.add(key);
                }
            }
            key = m_containerProcessor.registerContainers(residentContainer, foreignContainerStd);

            if (!hasErrors && key != null) {
                hasErrors = !m_containerProcessor.registerField(ALIAS_FTS_SCHOOL_LANG, ALIAS_DOE_SCHOOL_LANG);
                if (!hasErrors) {
                    m_keys.add(key);
                }
            }
            if (!hasErrors) {
                m_containerProcessor.initializeModelPropertyMap();
            }
        }



        /**
         * New bean container.
         *
         * @param x2BaseBean X2BaseBean
         * @param dataDictionary DataDictionary
         * @return Container
         */
        private Container newBeanContainer(X2BaseBean x2BaseBean, DataDictionary dataDictionary) {
            return m_transferDataHelper.new BeanContainer(x2BaseBean, dataDictionary);
        }

        /**
         * New map container.
         *
         * @param propoertiesMap Map<String,String>
         * @param x2BasebeanClass Class
         * @param identifier String
         * @param dataDictionary DataDictionary
         * @return Container
         */
        private Container newMapContainer(Map<String, String> propoertiesMap,
                                          Class x2BasebeanClass,
                                          String identifier,
                                          DataDictionary dataDictionary) {
            return m_transferDataHelper.new UiContainer(propoertiesMap, x2BasebeanClass, identifier, dataDictionary);
        }

        /**
         * New transfer container processor.
         *
         * @param broker ModelBroker
         * @return TransferContainerProcessor
         */
        private TransferContainerProcessor newTransferContainerProcessor(ModelBroker broker) {

            return m_transferDataHelper.new MultipleContainerProcessor(
                    m_transferDataHelper.new ModelPropertyCreator(broker));
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
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(formDetail, broker);
            fieldsProcedure.execute(IepTransferDirection.UPDATE);
            for (Pair<Integer, Integer> resdentForeignkey : fieldsProcedure.getKeys()) {
                prop = fieldsProcedure.getDirtyPropertyFromResident(resdentForeignkey);
                break;
            }


        }
        Map<String, Object> returnMap = new HashMap<String, Object>();

        if (!prop.isEmpty()) {
            returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            // returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH,
            // Integer.valueOf(UserEvent.EVENT_REFRESH));
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

        FormDetail formDetail = getFormDetail(detail);

        if (formDetail != null) {
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(formDetail, broker);
            fieldsProcedure.execute(IepTransferDirection.SET);
        }

        return errors;
    }

    /**
     * Gets the foreign fields procedure.
     *
     * @param formDetail FormDetail
     * @param broker ModelBroker
     * @return Run transfer data I
     */
    private RunTransferDataI getForeignFieldsProcedure(FormDetail formDetail, ModelBroker broker) {
        if (m_foreignFieldsProcedure == null) {
            m_foreignFieldsProcedure = new IepFactsForeignFieldsProcedure(formDetail, broker);
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
        }
        if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
        }
        return formDetail;
    }

}
