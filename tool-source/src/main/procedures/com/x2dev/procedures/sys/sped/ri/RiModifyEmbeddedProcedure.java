/*
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
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.nav.NavigationValue;
import com.follett.fsc.core.k12.web.template.EmbeddedList;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.Filter;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.MakeDataMapFromBeanHelper;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.PropertyValueType;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.struts.action.ActionErrors;

/**
 * The Class RiModifyEmbeddedProcedure.
 */
public class RiModifyEmbeddedProcedure {

    /**
     * interface which provide behavior for createMewEmbeddedChild method.
     *
     * @author Follett Software Company
     */
    public interface ModifyBehavior {

        /**
         * createMewEmbeddedChild need list map where<br>
         * key is field java name for class which used in embedded child,<br>
         * value - value from some bean converted into PropertyForDetail (UI view)<br>
         * logic will create child for each list element with data put into map<br>
         * .
         *
         * @return List
         * @see WebUtils#getPropertyForDetail(X2BaseBean, ModelProperty, Locale);
         */
        List<Map<String, Object>> getListProperties();

        /**
         * Gets the bean class.
         *
         * @return embedded child class
         */
        Class getBeanClass();
    }

    /**
     * Behavior for RiModifyEmbeddedProcedure
     * current behavior help create embedded child for each m_beans.
     * class provide methods for mark field/s from m_beans which will put into embedded child
     * CT - class which used in Collection m_beans;
     * TBC - target bean class. It is class which used in Embedded child.
     * CT and TBC can be different class
     *
     * @author Follett Software Company
     * @param <CT> the generic type
     * @param <TBC> the generic type
     */
    public class MapBehaviour<CT extends X2BaseBean, TBC extends X2BaseBean> implements ModifyBehavior {
        private MakeDataMapFromBeanHelper m_dataMapHelper = null;
        private Collection<CT> m_beans;
        private Class m_beanClass;
        private DataDictionary m_mbDdx = null;
        private Filter m_filter = null;

        /**
         * Instantiates a new map behaviour.
         *
         * @param beans Collection<CT>
         * @param TBC Class
         */
        public MapBehaviour(Collection<CT> beans, Class TBC) {
            this(beans, getDataDictionary(), TBC);
        }

        /**
         * Instantiates a new map behaviour.
         *
         * @param beans Collection<CT>
         * @param ddx DataDictionary
         * @param TBC Class
         */
        public MapBehaviour(Collection<CT> beans, DataDictionary ddx, Class TBC) {
            // data dictionary for m_beans
            m_mbDdx = ddx;
            m_dataMapHelper = getSpedHelper().new MakeDataMapFromBeanHelper(false, ddx);
            m_beans = beans;
            m_beanClass = TBC;
        }

        /**
         * Set filter. It will apply on m_beans for using inly needed beans
         *
         * @param filter void
         */
        public void setFilter(Filter filter) {
            m_filter = filter;
        }

        /**
         * mark alias which will put into map .
         *
         * @param alias String
         * @param key - this key will used on map, if key will null - alias will translate to
         *        javaName and it will used like key<br>
         *        it should be used if alias is field for class which used in embedded list.
         *        if key is not empty it should be javaName for class which used in embedded list.
         *        example alias -"test" key - null; logic try translate alias to java name. for
         *        example
         *        "fieldA001"
         *        and put "fieldA001" like key
         */
        public void markAlias(String alias, String key) {
            m_dataMapHelper.markAlias(alias, key, m_mbDdx);
        }

        /**
         * mark alias which will put into map .
         *
         * @param alias String
         * @param key - this key will used on map, if key will null - will used javaName field
         *        like key<br>
         *        example alias -"test" key - null; logic try translate alias to java name. for
         *        example
         *        "fieldA001"
         *        and put "fieldA001" like key
         * @param ddx DataDictionary
         */
        public void markAlias(String alias, String key, DataDictionary ddx) {
            m_dataMapHelper.markAlias(alias, key, ddx);
        }

        /**
         * mark field which will put into map.
         *
         * @param field String
         * @param key this key will used on map, if key will null - will used
         *        <code>field</code><br>
         *        example field -"id" key - null; logic put "id" like key
         */
        public void markField(String field, String key) {
            m_dataMapHelper.markField(field, key);
        }

        /**
         * add constant which will put into map.
         *
         * @param constant value for map
         * @param key - key for map, key can not be null
         */
        public void markConstant(String constant, String key) {
            m_dataMapHelper.markConstant(constant, key);
        }

        /**
         * Gets the list properties.
         *
         * @return List
         * @see com.x2dev.procedures.sys.sped.ri.RiModifyEmbeddedProcedure.ModifyBehavior#
         *      getListProperties()
         */
        @Override
        public List<Map<String, Object>> getListProperties() {
            return m_dataMapHelper.createMap(getFilteredBeans(), PropertyValueType.PROPERTY_DETAIL);
        }

        /**
         * Gets the bean class.
         *
         * @return Class
         * @see com.x2dev.procedures.sys.sped.ri.RiModifyEmbeddedProcedure.ModifyBehavior#
         *      getBeanClass()
         */
        @Override
        public Class getBeanClass() {
            return m_beanClass;
        }

        /**
         * m_beans which went through filter.
         *
         * @return Collection
         */
        public Collection<CT> getFilteredBeans() {
            Collection<CT> returnList = null;
            if (m_filter != null) {
                returnList = m_filter.applyFilter(m_beans);

            }
            if (returnList == null) {
                returnList = m_beans;
            }

            return returnList;
        }

    }

    private static final String EMPTY_STRING = "";
    private static final String STRING_CONSTANT = "constant";

    private X2Broker m_broker = null;
    private DataDictionary m_ddx = null;
    private RiSpedHelper m_spedHelper = null;
    private Map<String, Object> m_systemValues = new HashMap<String, Object>();
    private UserDataContainer m_userData = null;

    /**
     * Instantiates a new ri modify embedded procedure.
     *
     * @param userData UserDataContainer
     * @param ddxId String
     */
    public RiModifyEmbeddedProcedure(UserDataContainer userData, String ddxId) {
        m_userData = userData;
        m_broker = getBroker();
        m_spedHelper = new RiSpedHelper(m_broker, userData.getOrganization());
        m_ddx = m_spedHelper.getDictionaryByExtendedDictionaryId(ddxId);

    }

    /**
     * Instantiates a new ri modify embedded procedure.
     *
     * @param userData UserDataContainer
     * @param dictionary DataDictionary
     */
    public RiModifyEmbeddedProcedure(UserDataContainer userData, DataDictionary dictionary) {
        m_userData = userData;
        m_broker = getBroker();
        m_ddx = dictionary;
        m_spedHelper = new RiSpedHelper(m_broker, userData.getOrganization());

    }

    /**
     * Creates the map behavior.
     *
     * @param <CT> the generic type
     * @param <TBC> the generic type
     * @param beans Collection<CT>
     * @param tbc Class<TBC>
     * @return MapBehaviour
     */
    public <CT extends X2BaseBean, TBC extends X2BaseBean> MapBehaviour createMapBehavior(Collection<CT> beans,
                                                                                          Class<TBC> tbc) {
        return new MapBehaviour(beans, tbc);
    }

    /**
     * Creates the map behavior.
     *
     * @param <CT> the generic type
     * @param <TBC> the generic type
     * @param beans Collection<CT>
     * @param ddx DataDictionary
     * @param tbc Class<TBC>
     * @return MapBehaviour
     */
    public <CT extends X2BaseBean, TBC extends X2BaseBean> MapBehaviour createMapBehavior(Collection<CT> beans,
                                                                                          DataDictionary ddx,
                                                                                          Class<TBC> tbc) {
        return new MapBehaviour(beans, ddx, tbc);
    }

    /**
     * create embedded child according input <code>behavior</code>.
     *
     * @param embeddedDetail EmbeddedListDetailSet
     * @param behavior ModifyBehavior
     * @param skipExisting - if true and embeddedDetail will contain child with the same values
     *        like provided by <code>behavior</code> - logic will not create child
     * @throws X2BaseException exception
     */
    public void createNewEmbaddedChild(EmbeddedListDetailSet embeddedDetail,
                                       ModifyBehavior behavior,
                                       boolean skipExisting)
            throws X2BaseException {
        Class beanClass = behavior.getBeanClass();
        initializeSystemPreference(embeddedDetail.getEmbeddedList());
        List<GenericDetail> copyChild = new ArrayList<GenericDetail>(embeddedDetail.getChildDetails());
        for (Map<String, Object> properties : behavior.getListProperties()) {
            boolean ignore = false;
            if (skipExisting) {
                for (GenericDetail chDetail : copyChild) {
                    boolean hasDif = false;
                    for (Entry<String, Object> enty : properties.entrySet()) {
                        Object newValue = enty.getValue();
                        newValue = newValue == null ? EMPTY_STRING : newValue;
                        Object existingValue = m_systemValues.get(enty.getKey());
                        if (existingValue == null) {
                            existingValue = chDetail.getValueByBeanPath(enty.getKey());
                        }
                        existingValue = existingValue == null ? EMPTY_STRING : existingValue;
                        if (!newValue.equals(existingValue)) {
                            hasDif = true;
                            break;
                        }
                    }
                    if (!hasDif) {
                        ignore = true;
                        break;
                    }
                }
            }

            if (!ignore) {
                X2BaseBean baseBean = getNewFillingBean(beanClass, properties);
                embeddedDetail.addChild(baseBean, m_userData, (ModelBroker) getBroker());
            }
        }
    }

    /**
     * find EmbeddedListDetailSet from detail by embeddedListName.
     *
     * @param detail GenericDetail
     * @param embeddedListName String
     * @return EmbeddedListDetailSet
     */
    public static EmbeddedListDetailSet findEmbeddedListByName(GenericDetail detail, String embeddedListName) {
        GenericDetail formdetail = null;
        EmbeddedListDetailSet embeddedDetail = null;
        if (detail instanceof SisOutcomeDetail) {
            formdetail = ((SisOutcomeDetail) detail).getCurrentFormDetail();
        } else {
            formdetail = detail;
        }

        for (String childDetailSetId : formdetail.getChildDetailSetIds()) {
            ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);

            if (childDetailSetId.equals(embeddedListName)) {
                if (childDetailSet instanceof EmbeddedListDetailSet) {
                    embeddedDetail = (EmbeddedListDetailSet) childDetailSet;
                    break;
                }
            }
        }

        return embeddedDetail;
    }

    /**
     * Gets the broker.
     *
     * @return X2Broker
     */
    public X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(m_userData.getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Gets the data dictionary.
     *
     * @return DataDictionary
     */
    public DataDictionary getDataDictionary() {
        return m_ddx;
    }

    /**
     * Gets the sped helper.
     *
     * @return RiSpedHelper
     */
    public RiSpedHelper getSpedHelper() {
        return m_spedHelper;
    }

    /**
     * create and fill new bean.
     *
     * @param beanClass Class
     * @param properties Map<String,Object>
     * @return X 2 base bean
     */
    private X2BaseBean getNewFillingBean(Class beanClass, Map<String, Object> properties) {
        X2BaseBean baseBean = X2BaseBean.newInstance(beanClass, getBroker().getPersistenceKey());
        setValuesToBean(baseBean, properties);
        return baseBean;
    }

    /**
     * put system preference values into m_systemValues map .
     *
     * @param embList EmbeddedList
     */
    private void initializeSystemPreference(EmbeddedList embList) {
        for (NavigationValue value : embList.getSystemValues()) {
            String source = value.getSource();
            if (source != null && source.equals(STRING_CONSTANT) && value.getField() != null
                    && value.getValue() != null) {

                String stringValue = value.getValue().toString();
                ModelProperty prop = value.getField();
                DataDictionaryField field = prop.getField();
                String beanPath = prop.getBeanPath();
                String javaType = field.getJavaType();
                boolean asString = field.isString();
                Object interpretedValue = null;
                try {
                    interpretedValue = WebUtils.interpretValue(stringValue, STRING_CONSTANT, javaType, m_userData,
                            Locale.getDefault(), asString);
                } catch (X2BaseException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                m_systemValues.put(beanPath, interpretedValue);
            }
        }
    }

    /**
     * set properties into bean.
     *
     * @param bean X2BaseBean
     * @param properties key - field java name, value - value from bean converted into
     *        PropertyForDetail
     * @see WebUtils#getPropertyForDetail(X2BaseBean, ModelProperty, Locale)
     */
    private void setValuesToBean(X2BaseBean bean, Map<String, Object> properties) {
        String className = bean.getClass().getName();

        for (Entry<String, Object> entry : properties.entrySet()) {
            ModelProperty property = new ModelProperty(className, entry.getKey(), getDataDictionary());
            DataDictionaryField field = property.getField();
            ActionErrors errors = new ActionErrors();
            Object convertedValue = GenericDetail.getTypedValue(field, entry.getValue(), Locale.getDefault(), errors);
            bean.setFieldValueByBeanPath(entry.getKey(), convertedValue);
        }

    }
}
