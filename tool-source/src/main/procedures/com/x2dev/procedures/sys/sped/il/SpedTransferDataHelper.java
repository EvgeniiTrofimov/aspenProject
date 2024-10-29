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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.nav.NavigationValue;
import com.follett.fsc.core.k12.web.template.EmbeddedList;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.TimeAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.sql.Date;
import java.sql.Time;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.tuple.Pair;



/**
 * class with set of interface and classes which help transfer data from one bean to another<br>
 * resident bean - bean belong to current form<br>
 * foreign bean - bean which doesn't contain path from parent form bean, but form <br>
 * require to show data from foreign bean<br>
 * resident bean must duplicate fields form foreign bean. <br>
 *
 * @author Follett Software Company
 */
public class SpedTransferDataHelper {

    /**
     * Aspen can change data from bean when try using this data in UI<br>
     * for example boolean in bean will change to string with "true" and "false" values in UI<br>
     * Enum indicate Information condition - does this data was changed for using it on UI,<br>
     * or data contain original values from bean.
     *
     * @author Follett Software Company
     */
    public enum DataCastType {
        BEAN, UI;
    }

    /**
     * Enum indicate direction of transfer<br>
     * if you need transfer data from foreign bean to<br>
     * resident bean.
     *
     * @author Follett Software Company
     */
    public enum IepTransferDirection {
        SET, UPDATE;
    }

    /**
     * Container representative target/or all fields from bean.
     *
     * @author Follett Software Company
     */
    public interface Container {

        /**
         * try to delete representative bean.
         *
         * @param broker X2Broker
         */
        void delete(X2Broker broker);

        /**
         * return cast type.
         *
         * @return Data cast type
         * @see DataCastType
         */
        DataCastType getCastType();

        /**
         * return class for representative bean.
         *
         * @return Class
         */
        Class getContainerClass();

        /**
         * return unique Id.
         *
         * @return String
         */
        String getContainerId();

        /**
         * return dataDictionary which use in this bean.
         *
         * @return Data dictionary
         */
        DataDictionary getDataDictionary();

        /**
         * return Map which contain changed data.
         *
         * @return Map
         */
        Map<ModelProperty, Object> getDirtyPropertyValues();

        /**
         * return value form from bean by property.
         *
         * @param property ModelProperty
         * @return Object
         */
        Object getFieldValueByProperty(ModelProperty property);


        /**
         * return true if container representative new bean<br>
         * and bean must be saved.
         *
         * @return true, if is new
         */
        boolean isNew();

        /**
         * try save representative bean.
         *
         * @param broker X2Broker
         * @return List
         */
        List save(X2Broker broker);

        /**
         * Sets the field value by property.
         *
         * @param property ModelProperty
         * @param value Object
         */
        void setFieldValueByProperty(ModelProperty property, Object value);
    }

    /**
     * Container which representative fields straight from bean.
     *
     * @author Follett Software Company
     */
    public class BeanContainer implements Container {
        private X2BaseBean m_bean = null;
        DataCastType m_castType = null;
        private DataDictionary m_dataDictionary = null;
        private Map<ModelProperty, Object> m_dirtyProperties = new HashMap<ModelProperty, Object>();
        boolean m_isNew;
        private String m_identifier = null;

        /**
         * Instantiates a new bean container.
         *
         * @param x2BaseBean X2BaseBean
         * @param dataDictionary DataDictionary
         */
        public BeanContainer(X2BaseBean x2BaseBean, DataDictionary dataDictionary) {
            m_dataDictionary = dataDictionary;
            m_bean = x2BaseBean;
            m_castType = DataCastType.BEAN;
            if (!StringUtils.isEmpty(m_bean.getOid())) {
                m_identifier = m_bean.getOid();
            } else {
                m_identifier = generateUniqueId();
            }
            m_isNew = m_bean.isNew();
        }

        /**
         * delete current bean.
         *
         * @param broker X2Broker
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#delete(com.follett.fsc.
         *      core.k12.business.X2Broker)
         */
        @Override
        public void delete(X2Broker broker) {
            broker.deleteBean(m_bean);

        }

        /**
         * return DataCastType.BEAN cast type
         *
         * @return Data cast type
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getCastType()
         */
        @Override
        public DataCastType getCastType() {
            return m_castType;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_identifier == null) ? 0 : m_identifier.hashCode());
            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            BeanContainer other = (BeanContainer) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_identifier == null) {
                if (other.m_identifier != null) {
                    return false;
                }
            } else if (!m_identifier.equals(other.m_identifier)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the container class.
         *
         * @return Class
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getContainerClass()
         */
        @Override
        public Class getContainerClass() {
            return m_bean.getClass();
        }

        /**
         * return Oid from current bean.
         *
         * @return String
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getContainerId()
         */
        @Override
        public String getContainerId() {
            return m_identifier;
        }

        /**
         * Gets the data dictionary.
         *
         * @return Data dictionary
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getDataDictionary()
         */
        @Override
        public DataDictionary getDataDictionary() {
            return m_dataDictionary;
        }

        /**
         * Gets the dirty property values.
         *
         * @return Map
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getDirtyPropertyValues(
         *      )
         */
        @Override
        public Map<ModelProperty, Object> getDirtyPropertyValues() {
            return m_dirtyProperties;
        }

        /**
         * Gets the field value by property.
         *
         * @param property ModelProperty
         * @return Object
         * @see com.follett.fsc.core.k12.beans.X2BaseBean#getFieldValueByProperty(ModelProperty)
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getFieldValueByProperty
         *      (com.follett.fsc.core.k12.business.ModelProperty)
         */
        @Override
        public Object getFieldValueByProperty(ModelProperty property) {
            return property == null ? null : m_bean.getFieldValueByProperty(property);
        }

        /**
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#isNew()
         */
        @Override
        public boolean isNew() {
            return m_isNew;
        }



        /**
         * save current bean.
         *
         * @param broker X2Broker
         * @return List
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#save(com.follett.fsc.
         *      core.k12.business.X2Broker)
         */
        @Override
        public List save(X2Broker broker) {
            return broker.saveBean(m_bean);
        }

        /**
         * Sets the field value by property.
         *
         * @param property ModelProperty
         * @param value Object
         * @see com.follett.fsc.core.k12.beans.X2BaseBean#setFieldValueByProperty(ModelProperty,
         *      Object)
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#setFieldValueByProperty
         *      (com.follett.fsc.core.k12.business.ModelProperty, java.lang.Object)
         */
        @Override
        public void setFieldValueByProperty(ModelProperty property, Object value) {
            m_dirtyProperties.put(property, value);
            m_bean.setFieldValueByProperty(property, value);

        }

        /**
         * @see java.lang.Object#toString()
         */
        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "BeanContainer [m_bean=" + m_bean + "]";
        }

        /**
         * Gets the outer type.
         *
         * @return Sped transfer data helper
         */
        private SpedTransferDataHelper getOuterType() {
            return SpedTransferDataHelper.this;
        }


    }

    /**
     * Container which representative bean by propoertiesMap.<br>
     *
     * @author Follett Software Company
     * @see com.follett.fsc.core.k12.web.GenericDetail#getPropertyValues()
     *      Contain data in UI state<br>
     * @see DataCastType
     *      container know only target fields, it doesn't know about all fields form<br>
     *      representative bean.
     */
    public class UiContainer implements Container {
        private static final String STRING_CONSTANT = "constant";
        private DataCastType m_castType = null;
        private DataDictionary m_dataDictionary = null;
        private Map<ModelProperty, Object> m_dirtyProperties = new HashMap<ModelProperty, Object>();
        private String m_identifier = null;
        private Map<String, String> m_propoertiesMap;
        private Class m_x2BasebeanClass;
        private Map<ModelProperty, Object> m_systemValues = new HashMap<ModelProperty, Object>();

        /**
         * Instantiates a new ui container.
         *
         * @param propoertiesMap Map<String,String>
         * @param x2BasebeanClass Class
         * @param identifier unique Id, require use id from GenericDetail
         * @param dataDictionary DataDictionary
         * @see GenericDetail#getId()
         */
        public UiContainer(Map<String, String> propoertiesMap, Class x2BasebeanClass, String identifier,
                DataDictionary dataDictionary) {
            m_propoertiesMap = propoertiesMap;
            m_identifier = identifier;
            m_x2BasebeanClass = x2BasebeanClass;
            m_dataDictionary = dataDictionary;
            m_castType = DataCastType.UI;

        }

        /**
         * Instantiates a new ui container.
         *
         * @param propoertiesMap Map<String,String>
         * @param x2BasebeanClass Class
         * @param identifier String
         * @param dataDictionary DataDictionary
         * @param detailSetPrimary EmbeddedListDetailSet
         */
        public UiContainer(Map<String, String> propoertiesMap, Class x2BasebeanClass, String identifier,
                DataDictionary dataDictionary, EmbeddedListDetailSet detailSetPrimary) {
            m_propoertiesMap = propoertiesMap;
            m_identifier = identifier;
            m_x2BasebeanClass = x2BasebeanClass;
            m_dataDictionary = dataDictionary;
            m_castType = DataCastType.UI;
            initializeSystemPreference(detailSetPrimary.getEmbeddedList());
        }

        /**
         * nothing to do, UI container can not delete representative bean.
         *
         * @param broker X2Broker
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#delete(com.follett.fsc.
         *      core.k12.business.X2Broker)
         */
        @Override
        public void delete(X2Broker broker) {
            // TODO Auto-generated method stub
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            UiContainer other = (UiContainer) obj;
            if (m_identifier == null) {
                if (other.m_identifier != null) {
                    return false;
                }
            } else if (!m_identifier.equals(other.m_identifier)) {
                return false;
            }
            return true;
        }

        /**
         * return UI DataCastType.
         *
         * @return Data cast type
         * @see DataCastType
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getCastType()
         */
        @Override
        public DataCastType getCastType() {
            return m_castType;
        }

        /**
         * Gets the container class.
         *
         * @return Class
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getContainerClass()
         */
        @Override
        public Class getContainerClass() {

            return m_x2BasebeanClass;
        }

        /**
         * return initialized identifier.
         *
         * @return String
         * @see UiContainer#UiContainer(Map, Class, String, DataDictionary)
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getContainerId()
         */
        @Override
        public String getContainerId() {
            // TODO Auto-generated method stub
            return m_identifier;
        }

        /**
         * Gets the data dictionary.
         *
         * @return Data dictionary
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getDataDictionary()
         */
        @Override
        public DataDictionary getDataDictionary() {
            return m_dataDictionary;
        }

        /**
         * Gets the dirty property values.
         *
         * @return Map
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getDirtyPropertyValues(
         *      )
         */
        @Override
        public Map<ModelProperty, Object> getDirtyPropertyValues() {
            addSystemValuesToDirty();
            return m_dirtyProperties;
        }

        /**
         * Gets the field value by property.
         *
         * @param property ModelProperty
         * @return Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#getFieldValueByProperty
         *      (com.follett.fsc.core.k12.business.ModelProperty)
         */
        @Override
        public Object getFieldValueByProperty(ModelProperty property) {
            // system value has higher priority
            // If m_propoertiesMap and m_systemValues contain the the same property/dictionaryPath
            // it is mistake, because system values must be set after save and it override user data
            Object objectValue = m_systemValues.get(property);
            if (objectValue == null) {
                String stringValue = m_propoertiesMap.get(property.getDictionaryPath());
                objectValue = getObjectFromString(property, stringValue);
            }
            return objectValue;
        }

        /**
         * UI container hasn't ability to be saved and it will always isNew false.
         *
         * @return true, if is new
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#isNew()
         */
        @Override
        public boolean isNew() {
            return false;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((m_identifier == null) ? 0 : m_identifier.hashCode());
            return result;
        }

        /**
         * nothing to do, UI container can not save representative bean.
         *
         * @param broker X2Broker
         * @return List
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#save(com.follett.fsc.
         *      core.k12.business.X2Broker)
         */
        @Override
        public List save(X2Broker broker) {
            return new ArrayList();
        }

        /**
         * Sets the field value by property.
         *
         * @param property ModelProperty
         * @param value Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container#setFieldValueByProperty
         *      (com.follett.fsc.core.k12.business.ModelProperty, java.lang.Object)
         */
        @Override
        public void setFieldValueByProperty(ModelProperty property, Object value) {
            m_dirtyProperties.put(property, value);
            String stringValue = getStringValue(property, value);
            m_propoertiesMap.put(property.getDictionaryPath(), stringValue);

        }

        /**
         * @see java.lang.Object#toString()
         */
        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "MapContainer [m_identifier=" + m_identifier + ", m_x2BasebeanClass=" + m_x2BasebeanClass + "]";
        }

        /**
         * add system value to dirty, if properties map contain<br>
         * the same key(DictionaryPath) and another value or null.
         */
        private void addSystemValuesToDirty() {
            Set<String> propertyKey = m_propoertiesMap.keySet();

            for (ModelProperty property : m_systemValues.keySet()) {
                if (propertyKey.contains(property.getDictionaryPath())) {
                    String propValue = m_propoertiesMap.get(property.getDictionaryPath());
                    Object systemValueO = m_systemValues.get(property);
                    String systemValue = getStringValue(property, systemValueO);
                    if (systemValue != null && (StringUtils.isEmpty(propValue) || !propValue.equals(systemValue))) {
                        m_dirtyProperties.put(property, systemValueO);
                    }
                }
            }

        }

        /**
         * Gets the object from string.
         *
         * @param property ModelProperty
         * @param stringValue String
         * @return Object
         */
        private Object getObjectFromString(ModelProperty property, String stringValue) {
            return stringValue;
        }

        /**
         * Gets the string value.
         *
         * @param property ModelProperty
         * @param value Object
         * @return String
         */
        private String getStringValue(ModelProperty property, Object value) {
            return (String) value;
        }

        /**
         * Initialize system preference.
         *
         * @param embList EmbeddedList
         */
        private void initializeSystemPreference(EmbeddedList embList) {
            for (NavigationValue value : embList.getSystemValues()) {
                String source = value.getSource();
                if (source != null && source.equals(STRING_CONSTANT) && value.getField() != null
                        && value.getValue() != null) {
                    CastInterface converter =
                            new ConverterHelper(value.getField(), value.getField(), DataCastType.BEAN, getCastType());
                    Object valueO = converter.castObject(value.getValue());
                    m_systemValues.put(value.getField(), valueO);
                }
            }
        }

    }

    /**
     * interface for create container.
     *
     * @author Follett Software Company
     */
    public interface ContainerCreator {

        /**
         * create Container.
         *
         * @return Container
         */
        Container create();

        /**
         * return foreign container.
         *
         * @return Container
         */
        Container getForeignContainer();

        /**
         * return foreign fields.
         *
         * @return List
         */
        List<String> getForeginFields();

        /**
         * return resident container.
         *
         * @return Container
         */
        Container getResidentContainer();

        /**
         * return resident fields.
         *
         * @return List
         */
        List<String> getResidentFields();

    }

    /**
     * interface for create ModelProperty for input path.
     *
     * @author Follett Software Company
     */
    public interface ModelPropertyCreatorI {

        /**
         * method create ModelProperty for input path.
         *
         * @param path String
         * @param beanClass Class
         * @param dataDictionary DataDictionary
         * @return ModelProperty
         */
        ModelProperty create(String path, Class beanClass, DataDictionary dataDictionary);

        /**
         * Gets the broker.
         *
         * @return ModelBroker
         */
        ModelBroker getBroker();
    }

    /**
     * creator work with alias, fieldId, relationId<br>
     * path must be single<br>
     * you can not put path to child fields:<br>
     * rel***.fieldId
     *
     * @author Follett Software Company
     */
    public class ModelPropertyCreator implements ModelPropertyCreatorI {
        private ModelBroker m_broker = null;


        /**
         * Instantiates a new model property creator.
         *
         * @param broker ModelBroker
         */
        public ModelPropertyCreator(ModelBroker broker) {
            m_broker = broker;
        }

        /**
         * creator work with alias, fieldId, relationId<br>
         * path must be single<br>
         * you can not put path to child fields:<br>
         * rel***.fieldId
         *
         * @param path String
         * @param beanClass Class
         * @param dataDictionary DataDictionary
         * @return ModelProperty
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ModelPropertyCreatorI#create(java
         *      .lang.String, java.lang.Class,
         *      com.follett.fsc.core.k12.business.dictionary.DataDictionary)
         */
        @Override
        public ModelProperty create(String path, Class beanClass, DataDictionary dataDictionary) {

            ModelProperty returnModel = null;
            DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(path);
            DataDictionaryRelationship ddrelation = null;
            if (field == null) {
                field = dataDictionary.findDataDictionaryField(path);
                ddrelation = dataDictionary.findDataDictionaryRelationship(path);

            }
            if (field != null) {
                returnModel = new ModelProperty(field, dataDictionary);

            }
            if (ddrelation != null) {
                String dPath = ddrelation.getSystemDataRelationship().getPrimaryDataIndex().getFieldList();
                returnModel = new ModelProperty(dPath, getBroker().getPersistenceKey());
            }

            if (returnModel == null && beanClass != null) {
                returnModel = new ModelProperty(beanClass, path, dataDictionary);
            }

            return returnModel;
        }

        /**
         * Gets the broker.
         *
         * @return Model broker
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.ModelPropertyCreatorI#getBroker()
         */
        @Override
        public ModelBroker getBroker() {
            return m_broker;
        }
    }

    /**
     * processor work with container/containers and provide method for manage them.
     *
     * @author Follett Software Company
     */
    public interface TransferContainerProcessor {

        /**
         * clear target fields form resident container
         * clear all child in EmbeddedListDetailSet.
         *
         * @param detailsForDeleteAllChild List<EmbeddedListDetailSet>
         */
        void clearResidentFields(List<EmbeddedListDetailSet> detailsForDeleteAllChild);

        /**
         * get foreign container by key.
         *
         * @param key Pair<Integer,Integer>
         * @return Container
         */
        Container getForeignContainer(Pair<Integer, Integer> key);

        /**
         * return registered keys .
         *
         * @return Sets the
         */
        Set<Pair<Integer, Integer>> getKeys();

        /**
         * return resident container by key.
         *
         * @param key Pair<Integer,Integer>
         * @return Container
         */
        Container getResidentContainer(Pair<Integer, Integer> key);

        /**
         * Initialize target fields into container<br>
         * .
         */
        void initializeModelPropertyMap();

        /**
         * check is container was marked for delete.
         *
         * @param key Pair<Integer,Integer>
         * @return true, if is marked for delete
         */
        boolean isMarkedForDelete(Pair<Integer, Integer> key);

        /**
         * if resident bean was created<br>
         * - foreign bean must be created too<br>
         * this method register new resident and foreign cotainer and fields using
         * ContainerCreator<br>
         * this method can has ability to mark foreignContainer like new<br>
         * and save it during call transferFromResidentToForeign method<br>
         * .
         *
         * @param foreignCreator - creator which can create foreign container and contain info about
         *        resident container and fields
         */
        void markForeignContainerForCreating(ContainerCreator foreignCreator);

        /**
         * if resident bean was deleted<br>
         * - foreign bean must be deleted too<br>
         * this method get actual resident Containers and match them with registered container<br>
         * if registered container doesn't present in actual resident Containers - it must be mark
         * for deleting<br>
         * actual deleting become in transferFromResidentToForeign method<br>
         * Warning - method must change residentContainers:<br>
         * for delete containers which was registered<br>
         * in the result residentContainers must contain List<container> which was added.
         *
         * @param residentContainers - list of actual resident Containers
         */
        void markForeignContainersForDeliting(List<Container> residentContainers);

        /**
         * register resident and foreign container.
         *
         * @param residentBean Container
         * @param foreignBean Container
         * @return Pair
         */
        Pair<Integer, Integer> registerContainers(Container residentBean, Container foreignBean);

        /**
         * add to current container fields.<br>
         * field form resident tracking field from foreign<br>
         * it mean that foreignField will transfer into residentFieldName, and conversely
         *
         * @param residentFieldName String
         * @param foreignFieldName String
         * @return true, if successful
         */
        boolean registerField(String residentFieldName, String foreignFieldName);

        /**
         * the same like registerField, but:<br>
         * list must be the same size<br>
         * bounded resident and foreign fields must contain the same position<br>
         * (position in list form resident must match with position from foreign fields) .
         *
         * @param residentFieldNames List<String>
         * @param foreignFieldNames List<String>
         * @return true, if successful
         * @see registerField
         */
        boolean registerFields(List<String> residentFieldNames, List<String> foreignFieldNames);

        /**
         * transfer data from foreign to resident.
         */
        void transferFromForeignToResident();

        /**
         * transfer from resident to foreign<br>
         * method must delete foreign bean if container mark for deleting<br>
         * or save new bean if it is new.
         *
         * @return Set
         */
        Set<Pair<Integer, Integer>> transferFromResidentToForeign();
    }

    /**
     * The Class SingleContainerProcessor.
     *
     * @author Follett Software Company
     */
    public class SingleContainerProcessor implements TransferContainerProcessor {
        private Container m_residentBean;
        private List<String> m_residentFieldNames;
        private Container m_foreignBean;
        private List<String> m_foreignFieldNames;
        private List<ModelProperty> m_residentModelProperty = new ArrayList<ModelProperty>(0);
        private List<ModelProperty> m_foreignModelProperty = new ArrayList<ModelProperty>(0);
        private ModelPropertyCreator m_creator;
        private boolean m_markForDelete = false;
        private Set<Pair<Integer, Integer>> m_keys = null;

        /**
         * Instantiates a new single container processor.
         *
         * @param modelPropCreator ModelPropertyCreator
         */
        public SingleContainerProcessor(ModelPropertyCreator modelPropCreator) {
            m_creator = modelPropCreator;
        }

        /**
         * Clear resident fields.
         *
         * @param detailsForDeleteAllChild List<EmbeddedListDetailSet>
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      clearResidentFields()
         */
        @Override
        public void clearResidentFields(List<EmbeddedListDetailSet> detailsForDeleteAllChild) {
            for (ModelProperty mpResident : m_residentModelProperty) {
                m_residentBean.setFieldValueByProperty(mpResident, null);
            }

            for (EmbeddedListDetailSet detailSet : detailsForDeleteAllChild) {
                Set<String> idForDeliting = new HashSet<String>();
                Iterator<GenericDetail> it = detailSet.getChildDetails().iterator();
                while (it.hasNext()) {
                    GenericDetail genericDetail = it.next();
                    idForDeliting.add(genericDetail.getId());
                }

                for (String id : idForDeliting) {
                    detailSet.markForDelete(id);
                }
            }

        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            SingleContainerProcessor other = (SingleContainerProcessor) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_foreignBean == null) {
                if (other.m_foreignBean != null) {
                    return false;
                }
            } else if (!m_foreignBean.equals(other.m_foreignBean)) {
                return false;
            }
            if (m_residentBean == null) {
                if (other.m_residentBean != null) {
                    return false;
                }
            } else if (!m_residentBean.equals(other.m_residentBean)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the foreign container.
         *
         * @param key Pair<Integer,Integer>
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      getForeignContainer(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Container getForeignContainer(Pair<Integer, Integer> key) {
            Container returnContainer = null;
            Pair<Integer, Integer> containerKey = getKey(m_residentBean, m_foreignBean);
            if (containerKey != null && containerKey.equals(key)) {
                returnContainer = m_foreignBean;
            }
            return returnContainer;
        }

        /**
         * Gets the keys.
         *
         * @return Sets the
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            if (m_keys == null) {
                m_keys = new HashSet<Pair<Integer, Integer>>();
                m_keys.add(getKey(m_residentBean, m_foreignBean));
            }
            return m_keys;
        }

        /**
         * Gets the resident container.
         *
         * @param key Pair<Integer,Integer>
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      getResidentContainer(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Container getResidentContainer(Pair<Integer, Integer> key) {
            Container returnContainer = null;
            Pair<Integer, Integer> containerKey = getKey(m_residentBean, m_foreignBean);
            if (containerKey != null && containerKey.equals(key)) {
                returnContainer = m_residentBean;
            }
            return returnContainer;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_foreignBean == null) ? 0 : m_foreignBean.hashCode());
            result = prime * result + ((m_residentBean == null) ? 0 : m_residentBean.hashCode());
            return result;
        }

        /**
         * Initialize model property map.
         *
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      initializeModelPropertyMap()
         */
        @Override
        public void initializeModelPropertyMap() {
            if (m_residentFieldNames.size() == m_foreignFieldNames.size()) {

                for (int i = 0; i < m_residentFieldNames.size(); i++) {
                    String residentFieldName = m_residentFieldNames.get(i);
                    String foreignFieldName = m_foreignFieldNames.get(i);
                    ModelProperty residentProperty = m_creator.create(residentFieldName,
                            m_residentBean.getContainerClass(), m_residentBean.getDataDictionary());
                    ModelProperty foreignProperty = m_creator.create(foreignFieldName,
                            m_foreignBean.getContainerClass(), m_foreignBean.getDataDictionary());
                    if (!m_residentModelProperty.contains(residentProperty)) {
                        m_residentModelProperty.add(residentProperty);
                    }
                    if (!m_foreignModelProperty.contains(foreignProperty)) {
                        m_foreignModelProperty.add(foreignProperty);
                    }

                }
            }

        }

        /**
         * Checks if is marked for delete.
         *
         * @param key Pair<Integer,Integer>
         * @return true, if is marked for delete
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      isMarkedForDelete(com.sun.tools.javac.util.Pair)
         */
        @Override
        public boolean isMarkedForDelete(Pair<Integer, Integer> key) {
            return m_markForDelete;
        }


        /**
         * nothing to do,single container processor hasn't ability to create container inside<br>
         * .
         *
         * @param foreignCreator ContainerCreator
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      markForeignContainerForCreating(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.
         *      ContainerCreator)
         */
        @Override
        public void markForeignContainerForCreating(ContainerCreator foreignCreator) {
            // TODO Auto-generated method stub

        }

        /**
         * Mark foreign containers for deliting.
         *
         * @param residentContainers List<Container>
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      markForeignContainersForDeliting(java.util.List)
         */
        @Override
        public void markForeignContainersForDeliting(List<Container> residentContainers) {

            for (Pair<Integer, Integer> key : getKeys()) {
                Container rContainer = getResidentContainer(key);
                if (!residentContainers.contains(rContainer)) {
                    m_markForDelete = true;
                } else {
                    residentContainers.remove(rContainer);
                    m_markForDelete = false;
                }

            }
        }

        /**
         * params can not be null.
         *
         * @param residentBean Container
         * @param foreignBean Container
         * @return Pair
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerContainers(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container,
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container)
         */
        @Override
        public Pair<Integer, Integer> registerContainers(Container residentBean, Container foreignBean) {
            Pair<Integer, Integer> key = null;
            if (m_residentBean == null && m_foreignBean == null) {
                m_residentBean = residentBean;
                m_foreignBean = foreignBean;


                key = getKey(m_residentBean, m_foreignBean);
            }

            return key;
        }

        /**
         * params can not be null .
         *
         * @param residentFieldName String
         * @param foreignFieldName String
         * @return true, if successful
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerField(java.lang.String, java.lang.String)
         */
        @Override
        public boolean registerField(String residentFieldName, String foreignFieldName) {
            return registerFields(new ArrayList<String>(Arrays.asList(residentFieldName)),
                    new ArrayList<String>(Arrays.asList(foreignFieldName)));
        }

        /**
         * params can not be null and must contain the same size.
         *
         * @param residentFieldNames List<String>
         * @param foreignFieldNames List<String>
         * @return true, if successful
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerFields(java.util.List, java.util.List)
         */
        @Override
        public boolean registerFields(List<String> residentFieldNames, List<String> foreignFieldNames) {
            boolean isValid = isValid(residentFieldNames, foreignFieldNames);
            if (isValid) {
                m_residentFieldNames = summingToFistList(m_residentFieldNames, residentFieldNames);
                m_foreignFieldNames = summingToFistList(m_foreignFieldNames, foreignFieldNames);
            }
            return isValid;
        }

        /**
         * Transfer from foreign to resident.
         *
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      transferFromForeignToResident()
         */
        @Override
        public void transferFromForeignToResident() {
            for (Pair<Integer, Integer> key : getKeys()) {
                if (!isMarkedForDelete(key)) {
                    for (int i = 0; i < m_residentModelProperty.size(); i++) {
                        ModelProperty mpResident = m_residentModelProperty.get(i);
                        ModelProperty mpForiegn = m_foreignModelProperty.get(i);
                        Object object = m_foreignBean.getFieldValueByProperty(mpForiegn);
                        ConverterHelper converter = new ConverterHelper(mpForiegn, mpResident,
                                m_foreignBean.getCastType(), m_residentBean.getCastType());
                        object = converter.castObject(object);
                        m_residentBean.setFieldValueByProperty(mpResident, object);

                    }

                }

            }
        }

        /**
         * if container mark like ForDelete - call delete method for container
         * if container hasn't ID - call save method for container.
         *
         * @return Set
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      transferFromResidentToForeign()
         */
        @Override
        public Set<Pair<Integer, Integer>> transferFromResidentToForeign() {
            Set<Pair<Integer, Integer>> newKeys = new HashSet<Pair<Integer, Integer>>();
            for (Pair<Integer, Integer> key : getKeys()) {
                if (!isMarkedForDelete(key)) {
                    if (m_residentModelProperty.size() == m_foreignModelProperty.size()) {
                        for (int i = 0; i < m_residentModelProperty.size(); i++) {
                            ModelProperty mpResident = m_residentModelProperty.get(i);
                            ModelProperty mpForiegn = m_foreignModelProperty.get(i);
                            Object object = m_residentBean.getFieldValueByProperty(mpResident);
                            ConverterHelper converter = new ConverterHelper(mpResident, mpForiegn,
                                    m_residentBean.getCastType(), m_foreignBean.getCastType());
                            object = converter.castObject(object);
                            m_foreignBean.setFieldValueByProperty(mpForiegn, object);
                        }
                    }
                    Container residentBean = getResidentContainer(key);
                    Container foreignBean = getForeignContainer(key);
                    if (residentBean.isNew()) {
                        residentBean.save(m_creator.getBroker());
                    }
                    if (foreignBean.isNew()) {
                        foreignBean.save(m_creator.getBroker());
                    }

                    Pair<Integer, Integer> newKey = getKey(residentBean, foreignBean);
                    newKeys.add(newKey);

                } else {
                    m_foreignBean.delete(m_creator.getBroker());
                    m_residentBean.delete(m_creator.getBroker());
                }
            }

            m_keys = newKeys;
            return newKeys;


        }

        /**
         * return key for last registered container.
         *
         * @param residentBean Container
         * @param foreignBean Container
         * @return Pair
         */
        private Pair<Integer, Integer> getKey(Container residentBean, Container foreignBean) {
            Pair<Integer, Integer> key = null;
            if (residentBean != null && foreignBean != null) {
                key = Pair.of(Integer.valueOf(residentBean.hashCode()),
                        Integer.valueOf(foreignBean.hashCode()));
            }
            return key;
        }

        /**
         * Gets the outer type.
         *
         * @return Sped transfer data helper
         */
        private SpedTransferDataHelper getOuterType() {
            return SpedTransferDataHelper.this;
        }

        /**
         * Checks if is valid.
         *
         * @param residentFieldNames List<String>
         * @param foreignFieldNames List<String>
         * @return true, if is valid
         */
        private boolean isValid(List<String> residentFieldNames, List<String> foreignFieldNames) {
            return (residentFieldNames != null && foreignFieldNames != null && residentFieldNames.size() > 0
                    && residentFieldNames.size() == foreignFieldNames.size()
                    && m_foreignBean != null && m_residentBean != null) ? true : false;
        }

        /**
         * add values form "from" list into "to" list.<br>
         * "to" list can be null, there is why method has return.<br>
         *
         * @param <T> the generic type
         * @param to List<T>
         * @param from List<T>
         * @return return "to" list with old values + values from "from"
         * @see List
         */
        private <T> List<T> summingToFistList(List<T> to, List<T> from) {
            if (to == null) {
                to = from;
            } else {
                to.addAll(from);
            }
            return to;
        }


    }

    /**
     * class which contain .
     *
     * @author Follett Software Company
     */
    public class MultipleContainerProcessor implements TransferContainerProcessor {
        private List<String> m_errors = new ArrayList<String>();
        private List<SingleContainerProcessor> m_beanToBeanContainers = new ArrayList<SingleContainerProcessor>();
        private SingleContainerProcessor m_currentContainer;
        private ModelPropertyCreator m_creator = null;
        private Container m_residentBean = null;
        private Container m_foreignBean = null;
        private Set<Pair<Integer, Integer>> m_keys = new HashSet<Pair<Integer, Integer>>();



        /**
         * Instantiates a new multiple container processor.
         *
         * @param creator ModelPropertyCreator
         */
        public MultipleContainerProcessor(ModelPropertyCreator creator) {

            m_creator = creator;
        }

        /**
         * Clear resident fields.
         *
         * @param detailsForDeleteAllChild List<EmbeddedListDetailSet>
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      clearResidentFields()
         */
        @Override
        public void clearResidentFields(List<EmbeddedListDetailSet> detailsForDeleteAllChild) {
            for (SingleContainerProcessor container : m_beanToBeanContainers) {
                container.clearResidentFields(detailsForDeleteAllChild);
            }
        }


        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            MultipleContainerProcessor other = (MultipleContainerProcessor) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_beanToBeanContainers == null) {
                if (other.m_beanToBeanContainers != null) {
                    return false;
                }
            } else if (!m_beanToBeanContainers.equals(other.m_beanToBeanContainers)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the foreign container.
         *
         * @param key Pair<Integer,Integer>
         * @return Container
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      getForeignContainer(com.sun.tools.javac.util.Pair)
         */
        @Override
        public Container getForeignContainer(Pair<Integer, Integer> key) {
            Container container = null;
            for (SingleContainerProcessor bToBcontainer : m_beanToBeanContainers) {
                container = bToBcontainer.getForeignContainer(key);
                if (container != null) {
                    break;
                }
            }
            return container;
        }


        /**
         * Gets the keys.
         *
         * @return Sets the
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      getKeys()
         */
        @Override
        public Set<Pair<Integer, Integer>> getKeys() {
            return m_keys;
        }


        /**
         * Gets the resident container.
         *
         * @param key it is resident container hashCode + foreign hashCode
         * @return resident Container by key
         */
        @Override
        public Container getResidentContainer(Pair<Integer, Integer> key) {
            Container container = null;
            for (SingleContainerProcessor bToBcontainer : m_beanToBeanContainers) {
                container = bToBcontainer.getResidentContainer(key);
                if (container != null) {
                    break;
                }
            }
            return container;
        }


        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_beanToBeanContainers == null) ? 0 : m_beanToBeanContainers.hashCode());
            return result;
        }


        /**
         * Initialize model property map.
         */
        @Override
        public void initializeModelPropertyMap() {
            for (SingleContainerProcessor container : m_beanToBeanContainers) {
                container.initializeModelPropertyMap();
            }

        }

        /**
         * Checks if is marked for delete.
         *
         * @param key Pair<Integer,Integer>
         * @return true, if is marked for delete
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      isMarkedForDelete(com.sun.tools.javac.util.Pair)
         */
        @Override
        public boolean isMarkedForDelete(Pair<Integer, Integer> key) {
            boolean returnValue = false;
            for (SingleContainerProcessor bToBcontainer : m_beanToBeanContainers) {
                if (bToBcontainer.getKeys().contains(key)) {
                    returnValue = bToBcontainer.isMarkedForDelete(key);
                    break;
                }
            }
            return returnValue;
        }


        /**
         * Mark foreign container for creating.
         *
         * @param foreignCreator ContainerCreator
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      markForeignContainerForCreating(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.
         *      ContainerCreator)
         */
        @Override
        public void markForeignContainerForCreating(ContainerCreator foreignCreator) {
            Container residentContainer = foreignCreator.getResidentContainer();
            Container foreignContainerIepDsbl = foreignCreator.getForeignContainer();

            Pair<Integer, Integer> key = registerContainers(residentContainer, foreignContainerIepDsbl);
            if (key != null) {
                registerFields(foreignCreator.getResidentFields(), foreignCreator.getForeginFields());
                m_keys.add(key);
            }
            initializeModelPropertyMap();
        }


        /**
         * Mark foreign containers for deliting.
         *
         * @param residentContainers List<Container>
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      markForeignContainersForDeliting(java.util.List)
         */
        @Override
        public void markForeignContainersForDeliting(List<Container> residentContainers) {
            for (SingleContainerProcessor bToBcontainer : m_beanToBeanContainers) {
                bToBcontainer.markForeignContainersForDeliting(residentContainers);
            }
        }


        /**
         * Register containers.
         *
         * @param residentBean Container
         * @param foreignBean Container
         * @return Pair
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerContainers(com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container,
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.Container)
         */
        @Override
        public Pair<Integer, Integer> registerContainers(Container residentBean, Container foreignBean) {
            cleanUpOldValues();
            m_residentBean = residentBean;
            m_foreignBean = foreignBean;
            m_currentContainer = getCurrentContainer(m_residentBean, m_foreignBean);
            return getKey();

        }

        /**
         * Register field.
         *
         * @param residentFieldName String
         * @param foreignFieldName String
         * @return true, if successful
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerField(java.lang.String, java.lang.String)
         */
        @Override
        public boolean registerField(String residentFieldName, String foreignFieldName) {
            return registerFields(new ArrayList<String>(Arrays.asList(residentFieldName)),
                    new ArrayList<String>(Arrays.asList(foreignFieldName)));
        }

        /**
         * Register fields.
         *
         * @param residentFieldNames List<String>
         * @param foreignFieldNames List<String>
         * @return true, if successful
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      registerFields(java.util.List, java.util.List)
         */
        @Override
        public boolean registerFields(List<String> residentFieldNames, List<String> foreignFieldNames) {
            boolean isValid = isValid(residentFieldNames, foreignFieldNames);
            if (isValid) {
                isValid = m_currentContainer.registerFields(residentFieldNames, foreignFieldNames);
            }
            if (!isValid) {
                m_errors.add("Foreign and resident aliases must be filling and contain the same size,"
                        + " resident and foreign field must be register");
            }
            return isValid;

        }

        /**
         * Transfer from foreign to resident.
         *
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      transferFromForeignToResident()
         */
        @Override
        public void transferFromForeignToResident() {
            for (SingleContainerProcessor container : m_beanToBeanContainers) {
                container.transferFromForeignToResident();
            }
        }

        /**
         * Transfer from resident to foreign.
         *
         * @return Set
         * @see com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.TransferContainerProcessor#
         *      transferFromResidentToForeign()
         */
        @Override
        public Set<Pair<Integer, Integer>> transferFromResidentToForeign() {
            Set<Pair<Integer, Integer>> newKeys = new HashSet<Pair<Integer, Integer>>();
            for (SingleContainerProcessor container : m_beanToBeanContainers) {
                newKeys.addAll(container.transferFromResidentToForeign());
            }
            m_keys = newKeys;
            return newKeys;
        }

        /**
         * Clean up old values.
         */
        private void cleanUpOldValues() {
            m_residentBean = null;
            m_foreignBean = null;
            m_currentContainer = null;
        }


        /**
         * Gets the current container.
         *
         * @param resident Container
         * @param foreign Container
         * @return Single container processor
         */
        private SingleContainerProcessor getCurrentContainer(Container resident, Container foreign) {
            SingleContainerProcessor currentContainer = null;
            Pair<Integer, Integer> key = getKey(resident, foreign);
            if (key != null) {
                for (SingleContainerProcessor container : m_beanToBeanContainers) {
                    if (container.getResidentContainer(key) != null) {
                        currentContainer = container;
                        break;
                    }
                }

                if (currentContainer == null) {
                    currentContainer = new SingleContainerProcessor(m_creator);
                    currentContainer.registerContainers(resident, foreign);
                    m_beanToBeanContainers.add(currentContainer);
                    m_keys.add(key);
                }
            }
            return currentContainer;
        }


        /**
         * Gets the key.
         *
         * @return Pair
         */
        private Pair<Integer, Integer> getKey() {
            return getKey(m_residentBean, m_foreignBean);
        }

        /**
         * Gets the key.
         *
         * @param resident Container
         * @param foreign Container
         * @return Pair
         */
        private Pair<Integer, Integer> getKey(Container resident, Container foreign) {
            Pair<Integer, Integer> key = null;
            if (resident != null && foreign != null) {
                key = Pair.of(Integer.valueOf(resident.hashCode()),
                        Integer.valueOf(foreign.hashCode()));
            }
            return key;
        }

        /**
         * Gets the outer type.
         *
         * @return Sped transfer data helper
         */
        private SpedTransferDataHelper getOuterType() {
            return SpedTransferDataHelper.this;
        }


        /**
         * Checks if is valid.
         *
         * @param residentFieldNames List<String>
         * @param foreignFieldNames List<String>
         * @return true, if is valid
         */
        private boolean isValid(List<String> residentFieldNames, List<String> foreignFieldNames) {
            return (residentFieldNames != null && foreignFieldNames != null && residentFieldNames.size() > 0
                    && residentFieldNames.size() == foreignFieldNames.size()
                    && m_foreignBean != null && m_residentBean != null && m_currentContainer != null) ? true : false;
        }



    }

    /**
     * interface for cast object.
     *
     * @author Follett Software Company
     */
    public interface CastInterface {

        /**
         * Cast object.
         *
         * @param value Object
         * @return Object
         */
        Object castObject(Object value);
    }

    /**
     * convert object from bean state to UI.
     *
     * @author Follett Software Company
     */
    public class BeanToUIConverter implements CastInterface {
        private ModelProperty m_propertyFrom = null;

        /**
         * Instantiates a new bean to UI converter.
         *
         * @param from ModelProperty
         */
        public BeanToUIConverter(ModelProperty from) {
            m_propertyFrom = from;
        }

        /**
         * Cast object.
         *
         * @param value Object
         * @return Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface#castObject(java.
         *      lang.Object)
         */
        @Override
        public Object castObject(Object value) {
            Object object = null;
            object = WebUtils.getPropertyAsString(value, m_propertyFrom, Locale.getDefault(), null);
            return object;
        }
    }

    /**
     * convert object form ui state to bean<br>
     * class contain ModelProperty/JavaType for target field.<br>
     * Object will convert to type which using this field<br>
     * working with string, boolean and date<br>
     *
     * @author Follett Software Company
     */
    public class UIToBeanConverter implements CastInterface {

        private static final String UI_FORMAT_MM_DD_YYYY = "MM/dd/yyyy";
        private static final String JAVA_TYPE_BOOLEAN = "boolean";

        private String m_javaToType = null;
        private ModelProperty m_propertyTo = null;

        /**
         * Instantiates a new UI to bean converter.
         *
         * @param javaToType String
         */
        public UIToBeanConverter(String javaToType) {
            m_javaToType = javaToType;
        }

        /**
         * Instantiates a new UI to bean converter.
         *
         * @param to ModelProperty
         */
        public UIToBeanConverter(ModelProperty to) {
            m_propertyTo = to;
            m_javaToType = m_propertyTo.getField().getJavaType();
        }

        /**
         * Cast object.
         *
         * @param value Object
         * @return Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface#castObject(java.
         *      lang.Object)
         */
        @Override
        public Object castObject(Object value) {
            // even value is null - it must go through logic
            // because boolean into bean is primitive and castObject must return Boolean with value
            Object object = null;

            String stringClassName = String.class.getName();
            if (m_javaToType.equals(Date.class.getName())) {
                SimpleDateFormat dateFormatter = new SimpleDateFormat(UI_FORMAT_MM_DD_YYYY);

                try {
                    object = new PlainDate(dateFormatter.parse((String) value));
                } catch (ParseException e) {
                    // nothing to do
                }
            } else if (m_javaToType.equals(stringClassName)) {
                object = value == null ? null : value.toString();
            } else if (m_javaToType.equals(JAVA_TYPE_BOOLEAN)) {
                object = Boolean.valueOf((String) value);
            }


            return object;
        }
    }

    /**
     * convert object form bean state to bean<br>
     * beans can contain different java type for field value which converted.<br>
     *
     * @author Follett Software Company
     */
    public class BeanToBeanConverter implements CastInterface {
        private String m_javaFromType = null;
        private String m_javaToType = null;
        private ModelProperty m_propertyFrom = null;
        private ModelProperty m_propertyTo = null;

        /**
         * Instantiates a new bean to bean converter.
         *
         * @param javaFromType String
         * @param javaToType String
         */
        public BeanToBeanConverter(String javaFromType, String javaToType) {
            m_javaFromType = javaFromType;
            m_javaToType = javaToType;
        }

        /**
         * Instantiates a new bean to bean converter.
         *
         * @param from ModelProperty
         * @param to ModelProperty
         */
        public BeanToBeanConverter(ModelProperty from, ModelProperty to) {
            m_propertyFrom = from;
            m_propertyTo = to;
            m_javaFromType = m_propertyFrom.getField().getJavaType();
            m_javaToType = m_propertyTo.getField().getJavaType();
        }

        /**
         * Cast object.
         *
         * @param value Object
         * @return Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface#castObject(java.
         *      lang.Object)
         */
        @Override
        public Object castObject(Object value) {
            Object object = null;

            String stringClassName = String.class.getName();
            if (m_javaFromType.equals(stringClassName) && m_javaToType.equals(stringClassName)) {
                object = value;
            } else if (m_javaFromType.equals(stringClassName) && m_javaToType.equals(Time.class.getName())) {
                TimeAsStringConverter tConv = (TimeAsStringConverter) ConverterFactory
                        .getConverterForClass(Converter.TIME_CONVERTER, Locale.getDefault(), true);
                object = tConv.parseSystemString((String) value);
            } else if (m_javaFromType.equals(stringClassName) && m_javaToType.equals(Date.class.getName())) {
                DateAsStringConverter dConv = (DateAsStringConverter) ConverterFactory
                        .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
                object = dConv.parseSystemString((String) value);
            }

            return object;
        }

    }

    /**
     * help convert data.
     *
     * @author Follett Software Company
     */
    public class ConverterHelper implements CastInterface {
        private CastInterface m_converter = null;


        /**
         * Instantiates a new converter helper.
         *
         * @param from ModelProperty
         * @param to ModelProperty
         * @param typeFrom DataCastType
         * @param typeTo DataCastType
         */
        public ConverterHelper(ModelProperty from, ModelProperty to, DataCastType typeFrom, DataCastType typeTo) {
            if (typeFrom != null && typeTo != null) {
                if (typeFrom.equals(DataCastType.BEAN) && typeTo.equals(DataCastType.BEAN)) {
                    if (from != null && to != null) {
                        m_converter = new BeanToBeanConverter(from, to);
                    }
                } else if (typeFrom.equals(DataCastType.BEAN) && typeTo.equals(DataCastType.UI)) {
                    if (from != null) {
                        m_converter = new BeanToUIConverter(from);
                    }
                } else if (typeFrom.equals(DataCastType.UI) && typeTo.equals(DataCastType.BEAN)) {
                    if (to != null) {
                        m_converter = new UIToBeanConverter(to);
                    }
                }

            }
        }

        /**
         * Cast object.
         *
         * @param value Object
         * @return Object
         * @see
         *      com.x2dev.procedures.sys.sped.il.SpedTransferDataHelper.CastInterface#castObject(java.
         *      lang.Object)
         */
        @Override
        public Object castObject(Object value) {
            Object object = null;
            if (m_converter != null) {
                object = m_converter.castObject(value);
            }
            return object;
        }
    }

    /**
     * interface provides methods for manage transfer processor.
     *
     * @author Follett Software Company
     */
    public interface RunTransferDataI {

        /**
         * run main process which will transfer data .
         *
         * @param direction - from resident to foreign(SET) or from foreign to resident(Update)
         */
        void execute(IepTransferDirection direction);

        /**
         * return dirty property from foreign container with the same key.
         *
         * @param key Pair<Integer,Integer>
         * @return Map
         */
        Map<Container, Map<String, String>> getDirtyPropertyFromForeign(Pair<Integer, Integer> key);

        /**
         * return dirty property from Resident container with the same key.
         *
         * @param key Pair<Integer,Integer>
         * @return Map
         */
        Map<String, String> getDirtyPropertyFromResident(Pair<Integer, Integer> key);

        /**
         * must rerurn actual keys.
         *
         * @return Sets the
         */
        Set<Pair<Integer, Integer>> getKeys();
    }

    private List<String> m_randomIds = new ArrayList<String>();
    private static final String STRING_EMPTY = "";

    /**
     * return formDetail from GenericDetail.
     *
     * @param detail GenericDetail
     * @return Form detail
     */
    public static FormDetail getFormDetail(GenericDetail detail) {
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
     * return unique Id.
     *
     * @return String
     */
    String generateUniqueId() {
        boolean unique = false;
        String random = (int) (Math.random() * 1001) + STRING_EMPTY;
        while (!unique) {
            if (!m_randomIds.contains(random)) {
                m_randomIds.add(random);
                unique = true;
            } else {
                random = ((int) (Math.random() * 1001) + Integer.parseInt(random)) + STRING_EMPTY;
            }
        }
        return random;
    }

}

