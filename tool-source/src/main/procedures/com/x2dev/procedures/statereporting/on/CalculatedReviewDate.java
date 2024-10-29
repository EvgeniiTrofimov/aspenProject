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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldProcedure;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * The Class CalculatedReviewDate.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class CalculatedReviewDate implements CalculatedFieldProcedure {
    private static final String ALIAS_IPRC_REVIEW_DATE_HISTORY = "all-std-IPRCReviewDateHistory";

    /**
     * Update all beans.
     *
     * @param field CalculatedField
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateAllBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateAllBeans(CalculatedField field, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(SisStudent.COL_SPED_LAST_EVALUATION_DATE);
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);


        try (QueryIterator iterator = broker.getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                SisStudent bean = (SisStudent) iterator.next();
                addReviewDate(bean, broker);
                broker.saveBeanForced(bean);
            }
        }
    }

    /**
     * Update bean.
     *
     * @param field CalculatedField
     * @param bean X2BaseBean
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateBean(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateBean(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        addReviewDate(bean, broker);
    }

    /**
     * Update referenced beans.
     *
     * @param field CalculatedField
     * @param bean X2BaseBean
     * @param broker X2Broker
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateReferencedBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateReferencedBeans(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        addReviewDate(bean, broker);
    }

    /**
     * Adds the review date.
     *
     * @param bean X2BaseBean
     * @param broker X2Broker
     */
    private void addReviewDate(X2BaseBean bean, X2Broker broker) {
        if (SisStudent.class.isAssignableFrom(bean.getClass())) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
            DataDictionaryField ddField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IPRC_REVIEW_DATE_HISTORY);
            if (ddField != null) {
                PlainDate date = ((SisStudent) bean).getSpedLastEvaluationDate();
                if (date != null) {
                    Converter converter =
                            ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
                    if (converter instanceof SystemStringConverter) {
                        String dateString = ((SystemStringConverter) converter).getSystemString(date);
                        Object value = bean.getFieldValueByBeanPath(ddField.getJavaName());
                        String valueString =
                                value.getClass().equals(byte[].class) ? new String((byte[]) value) : value.toString();
                        Set<String> dates = StringUtils.isEmpty(valueString) ? new HashSet()
                                : new HashSet(Arrays.asList(valueString.split("\\s*,\\s*")));
                        if (!dates.contains(dateString)) {
                            dates.add(dateString);
                            valueString = dates.stream().reduce((str1, str2) -> str1 + "," + str2).get();
                            bean.setFieldValueByBeanPath(ddField.getJavaName(),
                                    value.getClass().equals(byte[].class) ? valueString.getBytes() : valueString);
                        }
                    }
                }
            }
        }
    }

}
