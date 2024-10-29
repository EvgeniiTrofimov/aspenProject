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
package com.x2dev.reports.sys.sped.ct;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.BeanDataSource;
// import com.x2dev.reports.sys.sped.ct.CtGenericFormData.GenericCollectionDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

/**
 * The Class ParentalConsentReevaluation.
 */
public class ParentalConsentReevaluation extends BaseFormReportJavaSource {

    private static final String PARAM_NUM_ROWS_DISPLAY = "numRowsToDisplay";
    private static final String PARAM_SAFEGUARD_NAME = "safeguardName";
    private static final String PARAM_SAFEGUARD_PHONE = "safeguardPhone";

    /**
     * The Class GenericCollectionDataSource.
     */
    public static class GenericCollectionDataSource extends BeanDataSource
            implements net.sf.jasperreports3.engine.JRRewindableDataSource,
            net.sf.jasperreports.engine.JRRewindableDataSource, dori.jasper.engine.JRRewindableDataSource {

        private static final String OWNER_PREFIX = "owner.";
        private static final String STORAGE_PREFIX = "storage.";

        private X2BaseBean m_formOwner = null;
        private X2BaseBean m_formStorage = null;

        private Collection<? extends X2BaseBean> m_beanCollection = null;
        private X2BaseBean m_currentBean = null;
        private Iterator<? extends X2BaseBean> m_iterator = null;

        /**
         * Instantiates a new generic collection data source.
         *
         * @param formOwner X2BaseBean
         * @param formStorage X2BaseBean
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public GenericCollectionDataSource(X2BaseBean formOwner, X2BaseBean formStorage,
                Collection<? extends X2BaseBean> beanCollection,
                DataDictionary dictionary,
                Locale locale) {
            this(beanCollection, dictionary, locale);
            m_formOwner = formOwner;
            m_formStorage = formStorage;
        }

        /**
         * Constructs a new BeanCollectionDataSource.
         *
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public GenericCollectionDataSource(Collection<? extends X2BaseBean> beanCollection,
                DataDictionary dictionary,
                Locale locale) {
            this(beanCollection, false, dictionary, locale);
        }

        /**
         * Constructs a new BeanCollectionDataSource.
         *
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param ignorePropertyRetrievalExceptions if true, property retrieval exceptions (like
         *        NullPointerExceptions and reflection exceptions)
         *        will be suppressed; this can allow greater
         *        flexibility in declaring fields on report formats
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public GenericCollectionDataSource(Collection<? extends X2BaseBean> beanCollection,
                boolean ignorePropertyRetrievalExceptions,
                DataDictionary dictionary,
                Locale locale) {
            super(true, ignorePropertyRetrievalExceptions, dictionary, locale);

            m_beanCollection = beanCollection;
            m_iterator = beanCollection.iterator();
        }

        /**
         * Constructs a new BeanCollectionDataSource created.
         *
         * @param breakValue Object
         * @param beanCollection Collection<? extends X2BaseBean>
         * @param ignorePropertyRetrievalExceptions if true, property retrieval exceptions (like
         *        NullPointerExceptions and reflection exceptions)
         *        will be suppressed; this can allow greater
         *        flexibility in declaring fields on report formats
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public GenericCollectionDataSource(Object breakValue,
                Collection<? extends X2BaseBean> beanCollection,
                boolean ignorePropertyRetrievalExceptions,
                DataDictionary dictionary,
                Locale locale) {
            this(beanCollection, ignorePropertyRetrievalExceptions, dictionary, locale);

            m_breakValue = breakValue;
        }

        /**
         * Gets the break data source.
         *
         * @param breakValue Object
         * @param beans ArrayList<X2BaseBean>
         * @return Bean data source
         * @see com.x2dev.sis.tools.reports.BeanDataSource#getBreakDataSource(java.lang.Object,
         *      java.util.ArrayList)
         */
        @Override
        public BeanDataSource getBreakDataSource(Object breakValue, ArrayList<X2BaseBean> beans) {
            return new BeanCollectionDataSource(breakValue, beans, m_autoConvert, m_ignorePropertyRetrievalExceptions,
                    m_dictionary, m_locale);
        }

        /**
         * Move first.
         *
         * @see JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() {
            m_iterator = m_beanCollection.iterator();
        }

        /**
         * Gets the current bean.
         *
         * @return X 2 base bean
         * @see com.x2dev.sis.tools.reports.BeanDataSource#getCurrentBean()
         */
        @Override
        protected X2BaseBean getCurrentBean() {
            return m_currentBean;
        }

        /**
         * Sets the current bean.
         *
         * @param bean void
         */
        protected void setCurrentBean(X2BaseBean bean) {
            m_currentBean = bean;
        }

        /**
         * Next bean.
         *
         * @return true, if successful
         * @see com.x2dev.sis.tools.reports.BeanDataSource#nextBean()
         */
        @Override
        protected boolean nextBean() {
            boolean next = m_iterator.hasNext();

            if (next) {
                m_currentBean = m_iterator.next();
            } else {
                m_currentBean = null;
            }

            return next;
        }

        /**
         * Gets the field value.
         *
         * @param fieldName String
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            /*
             * If the field starts with the "owner." or "storage." prefix, set the current bean for
             * value retrieval to
             * the owner or storage bean, respectively. Also strip off the owner or storage prefix
             * (note the special handling for doing this
             * if a prefix is present - e.g. a:owner.alias or a:storage.alias)
             */
            X2BaseBean storageBean = getCurrentBean();
            if (fieldName.startsWith(OWNER_PREFIX)) {
                fieldName = fieldName.substring(OWNER_PREFIX.length());
                setCurrentBean(m_formOwner);
            } else if (fieldName.startsWith(STORAGE_PREFIX)) {
                fieldName = fieldName.substring(STORAGE_PREFIX.length());
                setCurrentBean(m_formStorage);
            } else if (fieldName.matches("\\w:" + OWNER_PREFIX + ".*")) {
                setCurrentBean(m_formOwner);
                fieldName = fieldName.substring(0, 2)
                        + fieldName.substring(fieldName.indexOf(OWNER_PREFIX) + OWNER_PREFIX.length());
            } else if (fieldName.matches("\\w:" + STORAGE_PREFIX + ".*")) {
                setCurrentBean(m_formStorage);
                fieldName = fieldName.substring(0, 2)
                        + fieldName.substring(fieldName.indexOf(STORAGE_PREFIX) + STORAGE_PREFIX.length());
            }

            Object fieldValue = super.getFieldValue(fieldName);

            setCurrentBean(storageBean);

            return fieldValue;

        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        /*
         * Retrieve safeguardName and safeguardPhone, then pass to report as parameters.
         */
        addParameter("safeguardName", getParameter(PARAM_SAFEGUARD_NAME));
        addParameter("safeguardPhone", getParameter(PARAM_SAFEGUARD_PHONE));

        /*
         * Retrieve the childrenData and pad with additional rows if necessary.
         */
        IepData formOwner = (IepData) getFormOwner();
        GenericFormData formStorage = (GenericFormData) getFormStorage();
        Collection<GenericFormChildData> childrenData = formStorage.getGenericFormDataChildren();

        int numberOfRows = ((Integer) getParameter(PARAM_NUM_ROWS_DISPLAY)).intValue();
        if (childrenData.size() < numberOfRows) {
            for (int i = childrenData.size(); i < numberOfRows; i++) {
                childrenData.add(X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey()));
            }
        }

        GenericCollectionDataSource dataSource =
                new GenericCollectionDataSource(formOwner, formStorage, childrenData, getDictionary(), getLocale());
        return dataSource;
    }

}
