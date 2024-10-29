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
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class SecondaryTransitionFormData extends BeanReport {
    private static final String ALIAS_OTHER_SERVICE_TYPE = "other-service-type";
    private static final String ALIAS_OTHER_VALUE = "other-value";
    private static final String ALIAS_TRANS_ASSESS_CATEGOTY = "transAssess-categoty";

    private static final String EMPTY = "";

    private static final String KEY_DICTIONARY = "dictionary";
    private static final String KEY_EXTENDED = "Extended";
    private static final String KEY_TR_ASSESSMENT_MAP = "trAssessmentMap";
    private static final String KEY_YEAR_1 = "Year 1";
    private static final String KEY_YEAR_2 = "Year 2";
    private static final String KEY_YEAR_3 = "Year 3";
    private static final String KEY_YEAR_4 = "Year 4";
    private static final String KEY_YEAR_SERVICE = "yearService";

    private static final String PERF_LVL_TRANSITION_PLANNING = "Transition Planning";

    private static final String SERVICE_TYPE_DELIM = "; ";

    private static final long serialVersionUID = 1L;

    private Map<String, Map<String, String>> m_trAssessment;
    private Map<String, String> m_year_service;
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
        m_trAssessment = new HashMap<String, Map<String, String>>();
        IepData iepData = (IepData) getFormStorage();
        for (IepPerformanceLevel performanceLevel : iepData.getIepPerformanceLevel()) {
            if (performanceLevel.getType() != null && performanceLevel.getType().equals(PERF_LVL_TRANSITION_PLANNING)) {
                String transitionCategory = (String) performanceLevel.getFieldValueByAlias(ALIAS_TRANS_ASSESS_CATEGOTY,
                        getDictionary());
                if (transitionCategory != null) {
                    Map<String, String> performLvlValues = new HashMap<String, String>();
                    performLvlValues.put("transAssess-type",
                            (String) performanceLevel.getFieldValueByAlias("transAssess-type", getDictionary()));
                    performLvlValues.put("transAssess-agency",
                            (String) performanceLevel.getFieldValueByAlias("transAssess-agency", getDictionary()));
                    performLvlValues.put("transAssess-contact-person", (String) performanceLevel
                            .getFieldValueByAlias("transAssess-contact-person", getDictionary()));
                    performLvlValues.put("transAssess-desire-goal",
                            (String) performanceLevel.getFieldValueByAlias("transAssess-desire-goal", getDictionary()));
                    performLvlValues.put("transAssess-date",
                            m_ilSpedHelper.getDateByAlias(performanceLevel, "transAssess-date"));

                    m_trAssessment.put(transitionCategory, performLvlValues);
                }
            }
        }
        m_year_service = new HashMap<String, String>();
        m_year_service.put(KEY_YEAR_1, EMPTY);
        m_year_service.put(KEY_YEAR_2, EMPTY);
        m_year_service.put(KEY_YEAR_3, EMPTY);
        m_year_service.put(KEY_YEAR_4, EMPTY);
        m_year_service.put(KEY_EXTENDED, EMPTY);
        for (IepOtherService otherService : iepData.getIepOtherServices()) {
            String serviceType = (String) otherService.getFieldValueByAlias(ALIAS_OTHER_SERVICE_TYPE, getDictionary());
            if (serviceType != null) {
                String courseDecr = (String) otherService.getFieldValueByAlias(ALIAS_OTHER_VALUE, getDictionary());
                if (!StringUtils.isEmpty(courseDecr)) {
                    String addedServiceTypes = m_year_service.get(serviceType);
                    if (!addedServiceTypes.isEmpty()) {
                        addedServiceTypes = addedServiceTypes + SERVICE_TYPE_DELIM;
                    }
                    addedServiceTypes = addedServiceTypes + courseDecr;
                    m_year_service.put(serviceType, addedServiceTypes);
                }
            }
        }

        addParameter(KEY_TR_ASSESSMENT_MAP, m_trAssessment);
        addParameter(KEY_DICTIONARY, getDictionary());
        addParameter(KEY_YEAR_SERVICE, m_year_service);
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


    /**
     * return new Array List with filling empty sting.
     *
     * @param count - size new Array List
     * @return List
     */
    public List<String> getList(int count) {
        List<String> list = new ArrayList<String>(count);
        for (int i = 0; i < count; i++) {
            list.add(i, EMPTY);
        }
        return list;
    }
}
