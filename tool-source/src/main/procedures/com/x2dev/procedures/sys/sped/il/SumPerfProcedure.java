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

import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.procedures.sys.sped.il.ModifyTemplateProcedure.Strategy;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class SumPerfProcedure.
 */
public class SumPerfProcedure implements DynamicFormProcedure {

    private static final String ALIAS_TRANS_ASSESS_CATEGOTY = "transAssess-categoty";
    private static final String ALIAS_TRANS_ASSESS_RECOM_AREA = "transAssess-recom-area";
    private static final String EMBEDDED_NAME_POST_SCHOOL_RECOMMENDATION = "post-school-recommendation";
    private static final String EMBEDDED_NAME_FUNCTIONAL_PERFORMANCE = "functional-performance";
    private static final String EMBEDDED_NAME_TRANS_ASSESS = "trans-assess";
    private static final String FIELD_DD_ID_IPL_TYPE = "iplType";
    private static final String REF_CODE_ACADEMIC_READ_MATH = "Academic Read & Math";
    private static final String REF_CODE_COMMUNICATION_STATUS = "Communication Status";
    private static final String REF_CODE_FUNCTIONAL_PERFORM = "Functional Perform";
    private static final String REF_CODE_INDEPENDENT_LIVING = "Independent Living";
    private static final String REF_CODE_VOCATIONAL_CAREER = "Vocational & Career";

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

        DataDictionary dataDictionary = detail.getDataDictionary();
        Map<String, Object> mapfilter = new HashMap<String, Object>();

        ModifyTemplateProcedure modiFyTemplate = new ModifyTemplateProcedure(userData, Strategy.STRATEGY_RTB, null,
                mapfilter, ALIAS_TRANS_ASSESS_CATEGOTY, dataDictionary);
        EmbeddedListDetailSet embeddedDetail =
                ModifyTemplateProcedure.findEmbeddedListByName(detail, EMBEDDED_NAME_TRANS_ASSESS);
        modiFyTemplate.createNewEmbaddedChild(embeddedDetail);


        mapfilter = new HashMap<String, Object>();
        ModifyTemplateProcedure modiFyTemplate1 = new ModifyTemplateProcedure(userData, Strategy.STRATEGY_RTB, null,
                mapfilter, ALIAS_TRANS_ASSESS_RECOM_AREA, dataDictionary);
        EmbeddedListDetailSet embeddedDetail1 =
                ModifyTemplateProcedure.findEmbeddedListByName(detail, EMBEDDED_NAME_POST_SCHOOL_RECOMMENDATION);
        modiFyTemplate1.createNewEmbaddedChild(embeddedDetail1);


        mapfilter = new HashMap<String, Object>();
        List<String> needCodesSumPerf = new ArrayList<String>();
        needCodesSumPerf.add(REF_CODE_ACADEMIC_READ_MATH);
        needCodesSumPerf.add(REF_CODE_FUNCTIONAL_PERFORM);
        needCodesSumPerf.add(REF_CODE_INDEPENDENT_LIVING);
        needCodesSumPerf.add(REF_CODE_COMMUNICATION_STATUS);
        needCodesSumPerf.add(REF_CODE_VOCATIONAL_CAREER);
        ModifyTemplateProcedure modiFyTemplate2 = new ModifyTemplateProcedure(userData, Strategy.STRATEGY_FILTER_RTB,
                needCodesSumPerf, mapfilter, FIELD_DD_ID_IPL_TYPE, dataDictionary);

        EmbeddedListDetailSet embeddedDetail2 =
                ModifyTemplateProcedure.findEmbeddedListByName(detail, EMBEDDED_NAME_FUNCTIONAL_PERFORMANCE);
        modiFyTemplate2.createNewEmbaddedChild(embeddedDetail2);


        Map<String, Object> returnMap = new HashMap<String, Object>();
        returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
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



}
