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

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.procedures.sys.sped.il.ModifyTemplateProcedure.Strategy;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.DataCastType;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.IepTransferDirection;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI;
import com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class ConferenceRecommendProcedure.
 */
public class ConferenceRecommendProcedure implements DynamicFormProcedure {


    /**
     * The Class IepCRtransferData.
     */
    private class IepCRtransferData implements RunTransferDataI {
        private static final String ALIASE_DISABILITY_CODE = "nocech-disability";

        public static final String EBEDDED_ID_STUDENT_PRIMARY_DISABILITY = "studentPrimaryDisability";
        private static final String EBEDDED_ID_STUDENT_DISABILITY = "studentDisability";

        private static final String EXTENDED_DICTIOANRY_ID_SPED_IL_IEP = "SPED-IL-IEP";

        private ModelBroker m_broker = null;
        private TransferContainerProcessor m_containerProcessor = null;
        private DataDictionary m_ddxResident = null;
        private DataDictionary m_ddxForeign = null;
        private FormDetail m_formDetail = null;
        private final List<String> m_iepDsblFields = new ArrayList<String>(
                Arrays.asList(IepDisability.COL_PRIMARY_INDICATOR, IepDisability.COL_DISABILITY_CODE, "dsb-priority"));
        private boolean m_isUpdate = false;
        private int m_loadCount = 0;
        private List<IepDisability> m_primariesDsbl = null;
        private EmbeddedListDetailSet m_prmryDsblEmbeddedDetail = null;

        private final List<String> m_residentFieldsForDsbl =
                new ArrayList<String>(Arrays.asList("nocech-is-primary", ALIASE_DISABILITY_CODE, "nocech-priority"));
        private List<IepDisability> m_secondaryDsbl = null;
        private EmbeddedListDetailSet m_secdryDsblEmbDetail = null;
        private SpedTransferDataHelper m_transferDataHelper = null;
        private UserDataContainer m_userData = null;

        /**
         * Instantiates a new iep C rtransfer data.
         *
         * @param userData UserDataContainer
         * @param formDetail FormDetail
         * @param broker ModelBroker
         */
        public IepCRtransferData(UserDataContainer userData, FormDetail formDetail, ModelBroker broker) {
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
            try {
                if (direction.equals(IepTransferDirection.UPDATE)) {
                    m_loadCount++;
                }
                if (direction.equals(IepTransferDirection.UPDATE) && m_isUpdate == false) {

                    m_containerProcessor.transferFromForeignToResident();
                    m_isUpdate = true;

                } else if (direction.equals(IepTransferDirection.SET) && m_isUpdate == true) {
                    List<Container> actualResidents = getCurrentResidents();
                    m_containerProcessor.markForeignContainersForDeliting(actualResidents);
                    markNewContainers(actualResidents);
                    m_containerProcessor.transferFromResidentToForeign();
                    List<EmbeddedListDetailSet> detailSetList =
                            new ArrayList<EmbeddedListDetailSet>(
                                    Arrays.asList(getPrimaryEmbeddedDetail(), getSecdryEmbeddedDetail()));
                    m_containerProcessor.clearResidentFields(detailSetList);
                }
            } finally {
                if (direction.equals(IepTransferDirection.SET)) {
                    m_isUpdate = false;
                }
            }

        }

        /**
         * Gets the broker.
         *
         * @return ModelBroker
         */
        public ModelBroker getBroker() {
            return m_broker;
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
         * try to convert into BEAN view, call only ones.
         *
         * @param key Pair<Integer,Integer>
         * @return Map
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#
         *      getDirtyPropertyFromResident(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Map<String, String> getDirtyPropertyFromResident(Pair<Integer, Integer> key) {
            Map<String, String> returnMap = new HashMap<String, String>();
            if (m_loadCount == 1) {
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
         * Gets the foreign ddx.
         *
         * @return DataDictionary for foreign bean
         */
        public DataDictionary getForeignDdx() {
            if (m_ddxForeign == null) {
                m_ddxForeign = getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
            }
            return m_ddxForeign;
        }

        /**
         * Gets the foreign fields.
         *
         * @return list of foreign fields
         */
        public List<String> getForeignFields() {
            return m_iepDsblFields;
        }

        /**
         * return formInstance .
         *
         * @return Form instance
         */
        public FormInstance getFormInstance() {
            return m_formDetail.getFormInstance();
        }

        /**
         * Gets the keys.
         *
         * @return Sets the
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.RunTransferDataI#getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            return new HashSet<Pair<Integer, Integer>>(m_containerProcessor.getKeys());
        }

        /**
         * Gets the primary disabilities.
         *
         * @return the list of primary disabilities (1 or 0 size)
         */
        public List<IepDisability> getPrimaryDisabilities() {
            if (m_primariesDsbl == null) {
                loadPrimaryAndSecondaryDisabilities();
            }
            return m_primariesDsbl;
        }

        /**
         * Gets the primary embedded detail.
         *
         * @return emceddedDatail for primary disability case
         */
        public EmbeddedListDetailSet getPrimaryEmbeddedDetail() {
            if (m_prmryDsblEmbeddedDetail == null) {
                m_prmryDsblEmbeddedDetail = findEmbeddedListByName(EBEDDED_ID_STUDENT_PRIMARY_DISABILITY, true);
            }

            return m_prmryDsblEmbeddedDetail;
        }

        /**
         * Gets the resdent ddx.
         *
         * @return DataDictionary for resident bean
         */
        public DataDictionary getResdentDdx() {
            if (m_ddxResident == null) {
                FormInstance formInstance = m_formDetail.getFormInstance();
                m_ddxResident =
                        DataDictionary.getDistrictDictionary(
                                formInstance.getFormDefinition().getExtendedDataDictionary(),
                                m_broker.getPersistenceKey());
            }
            return m_ddxResident;
        }

        /**
         * Gets the resident fields.
         *
         * @return list of resident fields
         */
        public List<String> getResidentFields() {
            return m_residentFieldsForDsbl;
        }

        /**
         * Gets the secdry embedded detail.
         *
         * @return emceddedDatail for secondary disability case
         */
        public EmbeddedListDetailSet getSecdryEmbeddedDetail() {
            if (m_secdryDsblEmbDetail == null) {
                m_secdryDsblEmbDetail = findEmbeddedListByName(EBEDDED_ID_STUDENT_DISABILITY, true);
            }
            return m_secdryDsblEmbDetail;
        }

        /**
         * Gets the secondary disabilities.
         *
         * @return list of secondary disability
         */
        public List<IepDisability> getSecondaryDisabilities() {
            if (m_secondaryDsbl == null) {
                loadPrimaryAndSecondaryDisabilities();
            }
            return m_secondaryDsbl;
        }

        /**
         * return new Container which working with bean<br>
         * .
         *
         * @param x2BaseBean X2BaseBean
         * @param dataDictionary DataDictionary
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.BeanContainer.BeanContainer
         */
        public Container newBeanContainer(X2BaseBean x2BaseBean, DataDictionary dataDictionary) {
            return m_transferDataHelper.new BeanContainer(x2BaseBean, dataDictionary);
        }

        /**
         * intitialize fields and resources for this procedure.
         *
         * @throws X2BaseException exception
         */
        protected void initialize() throws X2BaseException {
            m_transferDataHelper = new SpedTransferDataHelper();
            m_containerProcessor = newTransferContainerProcessor(m_broker);

            // make EmbeddedDetail childs in size like disability
            ModifyTemplateProcedure secondaryTemplateProcedure =
                    getModifyTemplateProcedure(getSecondaryDisabilities().size());
            secondaryTemplateProcedure.createNewEmbaddedChild(getSecdryEmbeddedDetail());

            ModifyTemplateProcedure primaryTemplateProcedure =
                    getModifyTemplateProcedure(getPrimaryDisabilities().size());
            primaryTemplateProcedure.createNewEmbaddedChild(getPrimaryEmbeddedDetail());
            // end

            registerContainersAndFields(getPrimaryDisabilities(), getPrimaryEmbeddedDetail().getChildDetails(), true);
            registerContainersAndFields(getSecondaryDisabilities(), getSecdryEmbeddedDetail().getChildDetails(), false);
            m_containerProcessor.initializeModelPropertyMap();
        }

        /**
         * Mark new containers.
         *
         * @param residentContainers List<Container>
         */
        private void markNewContainers(List<Container> residentContainers) {
            for (Container residentContainer : residentContainers) {
                CRPContainerCreator containerCreator = new CRPContainerCreator(this, residentContainer);
                m_containerProcessor.markForeignContainerForCreating(containerCreator);
            }

        }

        /**
         * Register containers and fields.
         *
         * @param dsblties List<IepDisability>
         * @param dsblDetails Collection<GenericDetail>
         * @param isPrimary boolean
         */
        private void registerContainersAndFields(List<IepDisability> dsblties,
                                                 Collection<GenericDetail> dsblDetails,
                                                 boolean isPrimary) {
            EmbeddedListDetailSet detailSet = null;
            if (isPrimary) {
                detailSet = getPrimaryEmbeddedDetail();
            } else {
                detailSet = getSecdryEmbeddedDetail();
            }
            if (dsblties.size() == dsblDetails.size()) {
                Iterator<IepDisability> primaryit = dsblties.iterator();
                for (GenericDetail detail : dsblDetails) {
                    IepDisability disability = primaryit.next();
                    Container foreignContainerIepDsbl = newBeanContainer(disability, getForeignDdx());

                    Container residentContainer =
                            newMapContainer(detail.getPropertyValues(), GenericFormChildData.class,
                                    "" + m_transferDataHelper.generateUniqueId(), m_ddxResident, detailSet);
                    Pair<Integer, Integer> key =
                            m_containerProcessor.registerContainers(residentContainer, foreignContainerIepDsbl);
                    if (key != null) {
                        m_containerProcessor.registerFields(m_residentFieldsForDsbl, m_iepDsblFields);
                    }

                }

            }
        }

        /**
         * Load primary and secondary disabilities.
         */
        private void loadPrimaryAndSecondaryDisabilities() {
            m_primariesDsbl = new ArrayList<IepDisability>();
            m_secondaryDsbl = new ArrayList<IepDisability>();
            for (IepDisability disability : getDisabilities()) {
                if (disability.getPrimaryIndicator()) {
                    m_primariesDsbl.add(disability);
                } else {
                    m_secondaryDsbl.add(disability);
                }
            }
        }

        /**
         * New map container.
         *
         * @param propoertiesMap Map<String,String>
         * @param x2BasebeanClass Class
         * @param identifier String
         * @param dataDictionary DataDictionary
         * @param detailSet EmbeddedListDetailSet
         * @return Container
         */
        private Container newMapContainer(Map<String, String> propoertiesMap,
                                          Class x2BasebeanClass,
                                          String identifier,
                                          DataDictionary dataDictionary,
                                          EmbeddedListDetailSet detailSet) {
            return m_transferDataHelper.new UiContainer(propoertiesMap, x2BasebeanClass, identifier, dataDictionary,
                    detailSet);
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
         * Gets the dictionary by extended dictionary id.
         *
         * @param extendedDataDictionaryID String
         * @return Data dictionary
         */
        private DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
            QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary extendedDataDictionary =
                    (ExtendedDataDictionary) m_broker.getBeanByQuery(byCriteria);
            DataDictionary returnValue =
                    DataDictionary.getDistrictDictionary(extendedDataDictionary, m_broker.getPersistenceKey());


            return returnValue;
        }

        /**
         * Gets the disabilities.
         *
         * @return Collection
         */
        private Collection<IepDisability> getDisabilities() {
            IepData iepData = (IepData) m_formDetail.getFormInstance().getOwner();
            Collection<IepDisability> disabilities = iepData.getIepDisability();
            return disabilities;
        }

        /**
         * Gets the modify template procedure.
         *
         * @param count int
         * @return Modify template procedure
         */
        private ModifyTemplateProcedure getModifyTemplateProcedure(int count) {
            return new ModifyTemplateProcedure(m_userData, Strategy.STRATEGY_COUNT, Integer.valueOf(count),
                    new HashMap<String, Object>(), ALIASE_DISABILITY_CODE, getResdentDdx());
        }


        /**
         * Find embedded list by name.
         *
         * @param embeddedId String
         * @param deleteOldValue boolean
         * @return EmbeddedListDetailSet
         */
        private EmbeddedListDetailSet findEmbeddedListByName(String embeddedId, boolean deleteOldValue) {
            EmbeddedListDetailSet emdeddedList =
                    ModifyTemplateProcedure.findEmbeddedListByName(m_formDetail, embeddedId);
            if (deleteOldValue && emdeddedList != null) {
                markFordeleteAllChilds(emdeddedList);
            }
            return emdeddedList;

        }

        /**
         * Mark fordelete all childs.
         *
         * @param emdeddedList EmbeddedListDetailSet
         */
        private void markFordeleteAllChilds(EmbeddedListDetailSet emdeddedList) {

            Set<String> idForDeliting = new HashSet<String>();
            Iterator<GenericDetail> it = emdeddedList.getChildDetails().iterator();
            while (it.hasNext()) {
                GenericDetail genericDetail = it.next();
                idForDeliting.add(genericDetail.getId());
            }

            for (String id : idForDeliting) {
                emdeddedList.markForDelete(id);
            }


        }

        /**
         * Gets the current residents.
         *
         * @return List
         */
        private List<Container> getCurrentResidents() {
            List<Container> currentResidents = new ArrayList<Container>();
            for (GenericDetail detail : getPrimaryEmbeddedDetail().getChildDetails()) {
                Container residentContainer = newMapContainer(detail.getPropertyValues(), GenericFormChildData.class,
                        m_transferDataHelper.generateUniqueId(), m_ddxResident, getPrimaryEmbeddedDetail());
                currentResidents.add(residentContainer);
            }
            for (GenericDetail detail : getSecdryEmbeddedDetail().getChildDetails()) {
                Container residentContainer = newMapContainer(detail.getPropertyValues(), GenericFormChildData.class,
                        m_transferDataHelper.generateUniqueId(), m_ddxResident, getSecdryEmbeddedDetail());
                currentResidents.add(residentContainer);
            }
            return currentResidents;
        }

    }

    /**
     * The Class CRPContainerCreator.
     */
    private class CRPContainerCreator implements ContainerCreator {
        private IepCRtransferData m_data = null;
        private Container m_resident = null;
        private Container m_foreign = null;

        /**
         * Instantiates a new CRP container creator.
         *
         * @param data IepCRtransferData
         * @param residentContainer Container
         */
        public CRPContainerCreator(IepCRtransferData data, Container residentContainer) {
            m_resident = residentContainer;
            m_data = data;
        }

        /**
         * Creates the.
         *
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator#create()
         */
        @Override
        public Container create() {
            IepData iepData = (IepData) m_data.getFormInstance().getOwner();
            String studentOid = iepData.getStudentOid();

            IepDisability disability =
                    X2BaseBean.newInstance(IepDisability.class, m_data.getBroker().getPersistenceKey());
            disability.setStudentOid(studentOid);
            disability.setIepDataOid(iepData.getOid());

            return m_data.newBeanContainer(disability, m_data.getForeignDdx());
        }

        /**
         * create foreign bean in first call using create method.
         *
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator#
         *      getForeignContainer()
         */
        @Override
        public Container getForeignContainer() {
            if (m_foreign == null) {
                m_foreign = create();
            }
            return m_foreign;
        }

        /**
         * Gets the foregin fields.
         *
         * @return List
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator#getForeginFields
         *      ()
         */
        @Override
        public List<String> getForeginFields() {
            return m_data.getForeignFields();
        }

        /**
         * Gets the resident container.
         *
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator#
         *      getResidentContainer()
         */
        @Override
        public Container getResidentContainer() {
            return m_resident;
        }

        /**
         * Gets the resident fields.
         *
         * @return List
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ContainerCreator#
         *      getResidentFields()
         */
        @Override
        public List<String> getResidentFields() {
            return m_data.getResidentFields();
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
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(userData, formDetail, broker);
            fieldsProcedure.execute(IepTransferDirection.UPDATE);
            for (Pair<Integer, Integer> resdentForeignkey : fieldsProcedure.getKeys()) {
                prop = fieldsProcedure.getDirtyPropertyFromResident(resdentForeignkey);
                if (!prop.isEmpty()) {
                    break;
                }

            }

        }
        Map<String, Object> returnMap = new HashMap<String, Object>();

        if (!prop.isEmpty()) {
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
        FormDetail formDetail = getFormDetail(detail);
        IepCRtransferData data = (IepCRtransferData) getForeignFieldsProcedure(userData, formDetail, broker);
        validate(errors, data);
        if (formDetail != null && errors.isEmpty()) {
            RunTransferDataI fieldsProcedure = getForeignFieldsProcedure(userData, formDetail, broker);
            fieldsProcedure.execute(IepTransferDirection.SET);

        }

        return errors;
    }

    /**
     * Gets the value from property map.
     *
     * @param propertyMap Map<String,Object>
     * @param dictionary DataDictionary
     * @param alias String
     * @return Object
     */
    private Object getValueFromPropertyMap(Map<String, Object> propertyMap, DataDictionary dictionary, String alias) {
        Object returnValue = null;
        if (propertyMap != null && !propertyMap.isEmpty() && dictionary != null && alias != null) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            String fieldId = field == null ? "" : field.getId();
            returnValue = propertyMap.get(fieldId);
        }
        return returnValue;
    }

    /**
     * Gets the foreign fields procedure.
     *
     * @param userData UserDataContainer
     * @param formDetail FormDetail
     * @param broker ModelBroker
     * @return Run transfer data I
     */
    private RunTransferDataI getForeignFieldsProcedure(UserDataContainer userData,
                                                       FormDetail formDetail,
                                                       ModelBroker broker) {
        if (m_foreignFieldsProcedure == null) {
            m_foreignFieldsProcedure = new IepCRtransferData(userData, formDetail, broker);
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

    /**
     * Validate.
     *
     * @param errors List<ValidationError>
     * @param data IepCRtransferData
     */
    private void validate(List<ValidationError> errors, IepCRtransferData data) {
        EmbeddedListDetailSet detailSetPrimary = data.getPrimaryEmbeddedDetail();
        Collection<GenericDetail> detailCollection = new ArrayList<GenericDetail>(detailSetPrimary.getChildDetails());
        if (detailCollection.size() > 1) {
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.RECORD_UNIQUE_BY_FIELDS), "primary disability"));
        }

        EmbeddedListDetailSet detailSetSecondary = data.getSecdryEmbeddedDetail();
        List<String> uniqueDisability = new ArrayList<String>();
        detailCollection.addAll(detailSetSecondary.getChildDetails());

        for (GenericDetail generidedail : detailCollection) {
            String dsbCode = (String) getValueFromPropertyMap(generidedail.getPropertyValues(), data.getResdentDdx(),
                    IepCRtransferData.ALIASE_DISABILITY_CODE);

            if (uniqueDisability.contains(dsbCode)) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.RECORD_UNIQUE_BY_FIELDS), dsbCode));
            } else {
                uniqueDisability.add(dsbCode);
            }
        }
    }

}
