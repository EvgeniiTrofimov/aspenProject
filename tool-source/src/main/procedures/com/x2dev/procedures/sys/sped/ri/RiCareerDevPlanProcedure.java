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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Tab;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.template.TemplateElement;
import com.x2dev.procedures.sys.sped.ri.RiModifyEmbeddedProcedure.MapBehaviour;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.Filter;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.MakeDataMapFromBeanHelper;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper.PropertyValueType;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class RiCareerDevPlanProcedure.
 */
public class RiCareerDevPlanProcedure implements DynamicFormProcedure {

    private static final String EMB_TYPE_WORK_EXPERIENCES = "Work Experiences";
    private static final String EMB_ID_WORK_EXPERIENCES = "workExperiences";
    private static final String PLVL_TYPE_TRANSITION_PLANNING = "Transition Planning";
    private static final String EMB_TYPE_TRANSITION_ASSESSMENTS = "Transition Assessments";
    private static final String ALIAS_GFD_CH_EMBEDDED_TYPE = "crrpln-ch-embedded-type";
    private static final String ALIAS_IEP_PLVL_TRANS_ASSESS_TOOL = "transAssess-tool";
    private static final String ALIAS_IEP_PLVL_TRANS_ASSESS_DATE = "transAssess-date";
    private static final String ALIAS_GFD_CH_DATE1 = "crrpln-ch-date1";
    private static final String DDX_SPED_RI_IEP = "SPED-RI-IEP";
    private static final String EMB_ID_TRANSITION_ASSESSMENTS = "transitionAssessments";
    private static final String ALIAS_GFD_CH_METHOD = "crrpln-ch-method";

    private UserDataContainer m_userData = null;
    private X2Broker m_broker = null;
    private FormInstance m_currentFI = null;
    private FormInstance m_previosFI = null;
    private RiModifyEmbeddedProcedure m_modiFyTemplate = null;
    private RiSpedHelper m_spedHelper = null;
    private FormDetail m_formDetail = null;
    private DataDictionary m_formDDX = null;
    private DataDictionary m_spedRiIepDDX = null;
    protected Map<String, DataDictionary> m_dictionaryMap = new HashMap<String, DataDictionary>();
    private String m_javaScriptElementId = null;
    private String m_parentElemetnId = null;

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
            throws X2BaseException {}

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
        initialize(detail, userData);
        if (!StringUtils.isEmpty(m_parentElemetnId) && !StringUtils.isEmpty(m_javaScriptElementId)) {
            Tab parentElement = (Tab) template.findElementById(m_parentElemetnId);
            TemplateElement targetElemetn = template.findElementById(m_javaScriptElementId);
            if (parentElement != null && targetElemetn != null) {
                parentElement.getChildren().remove(targetElemetn);
            }
        }



        Map<String, Object> returnMap = new HashMap<String, Object>();
        if (key.equals("copyData")) {
            if (m_previosFI != null && detail.isNew()) {

                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, copyDataFromPreviousCrrPlanStorage());
            }
        } else {

            MapBehaviour<IepPerformanceLevel, GenericFormChildData> mapBeh = getPerfLvlMapBehavior();
            EmbeddedListDetailSet embeddedDetail = RiModifyEmbeddedProcedure.findEmbeddedListByName(detail,
                    EMB_ID_TRANSITION_ASSESSMENTS);
            m_modiFyTemplate.createNewEmbaddedChild(embeddedDetail, mapBeh, true);

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
        return errors;
    }

    /**
     * Copy transition assessment.
     */
    private void copyTransitionAssessment() {
        MapBehaviour<GenericFormChildData, GenericFormChildData> trAmntBeh = getTransitionAssessmentBehaviour();
        EmbeddedListDetailSet trAssessmentEmb = RiModifyEmbeddedProcedure.findEmbeddedListByName(m_formDetail,
                EMB_ID_TRANSITION_ASSESSMENTS);
        try {
            m_modiFyTemplate.createNewEmbaddedChild(trAssessmentEmb, trAmntBeh, true);
        } catch (X2BaseException e) {
            //
        }
    }

    /**
     * Copy work experience.
     */
    private void copyWorkExperience() {
        MapBehaviour<GenericFormChildData, GenericFormChildData> workExpBeh = getWorkExperienceBehaviour();
        EmbeddedListDetailSet workExpEmb = RiModifyEmbeddedProcedure.findEmbeddedListByName(m_formDetail,
                EMB_ID_WORK_EXPERIENCES);

        try {
            m_modiFyTemplate.createNewEmbaddedChild(workExpEmb, workExpBeh, true);
        } catch (X2BaseException e) {
            //
        }
    }

    /**
     * copy storage data from prevFI into current generic detail.
     *
     * @return Map
     */
    private Map<String, Object> copyDataFromPreviousCrrPlanStorage() {
        copyTransitionAssessment();
        copyWorkExperience();
        GenericFormData oldStorage = (GenericFormData) m_previosFI.getStorageObject();
        MakeDataMapFromBeanHelper<GenericFormData> mapDataHelper =
                m_spedHelper.new MakeDataMapFromBeanHelper<GenericFormData>(false, m_formDDX);
        Set<String> propKeys = m_formDetail.getProperties();
        for (String key : propKeys) {
            if (!key.endsWith("OID")) {
                mapDataHelper.markProperty(key, key);
            }
        }
        List<Map<String, Object>> list = mapDataHelper.createMap(
                new ArrayList<GenericFormData>(Arrays.asList(oldStorage)), PropertyValueType.PROPERTY_DETAIL);
        return list.size() == 1 ? list.get(0) : new HashMap<String, Object>();

    }

    /**
     * Gets the work experience behaviour.
     *
     * @return Map behaviour
     */
    private MapBehaviour<GenericFormChildData, GenericFormChildData> getWorkExperienceBehaviour() {
        GenericFormData prevStorage = (GenericFormData) m_previosFI.getStorageObject();
        // create
        MapBehaviour<GenericFormChildData, GenericFormChildData> mapBeh =
                m_modiFyTemplate.createMapBehavior(prevStorage.getGenericFormDataChildren(),
                        GenericFormChildData.class);
        // fill
        mapBeh.setFilter(getWorkExperienceFileter());
        mapBeh.markAlias("crrpln-ch-experience-type", null);
        mapBeh.markAlias("crrpln-ch-location", null);

        mapBeh.markAlias("crrpln-ch-anticipated-dates", null);

        mapBeh.markAlias("crrpln-ch-person-responsible", null);
        mapBeh.markAlias("crrpln-ch-completed", null);
        mapBeh.markAlias("crrpln-ch-total-days", null);

        mapBeh.markAlias(ALIAS_GFD_CH_EMBEDDED_TYPE, null);
        // return
        return mapBeh;
    }

    /**
     * Gets the transition assessment behaviour.
     *
     * @return Map behaviour
     */
    private MapBehaviour<GenericFormChildData, GenericFormChildData> getTransitionAssessmentBehaviour() {
        GenericFormData prevStorage = (GenericFormData) m_previosFI.getStorageObject();
        // create
        MapBehaviour<GenericFormChildData, GenericFormChildData> mapBeh =
                m_modiFyTemplate.createMapBehavior(prevStorage.getGenericFormDataChildren(),
                        GenericFormChildData.class);
        // fill
        mapBeh.setFilter(getTransitionAssessmentFileter());
        mapBeh.markAlias("crrpln-ch-method", null);
        mapBeh.markAlias("crrpln-ch-date1", null);
        mapBeh.markAlias(ALIAS_GFD_CH_EMBEDDED_TYPE, null);
        // return
        return mapBeh;
    }

    /**
     * get behavior which will transfer data from list IepPerformanceLevel into embedded child
     * (GenericFormChildData) .
     *
     * @return Map behaviour
     */
    private MapBehaviour<IepPerformanceLevel, GenericFormChildData> getPerfLvlMapBehavior() {
        // Initialize
        IepData iepData = (IepData) m_currentFI.getOwner();

        // create
        MapBehaviour<IepPerformanceLevel, GenericFormChildData> mapBeh =
                m_modiFyTemplate.createMapBehavior(iepData.getIepPerformanceLevel(),
                        m_spedRiIepDDX, GenericFormChildData.class);
        // fill
        mapBeh.setFilter(getPerformanceLevelFilter());
        mapBeh.markAlias(ALIAS_IEP_PLVL_TRANS_ASSESS_DATE,
                RiSpedHelper.translateAliasToJavaName(ALIAS_GFD_CH_DATE1, m_formDDX));
        mapBeh.markAlias(ALIAS_IEP_PLVL_TRANS_ASSESS_TOOL,
                RiSpedHelper.translateAliasToJavaName(ALIAS_GFD_CH_METHOD, m_formDDX));
        mapBeh.markConstant(EMB_TYPE_TRANSITION_ASSESSMENTS,
                RiSpedHelper.translateAliasToJavaName(ALIAS_GFD_CH_EMBEDDED_TYPE, m_formDDX));
        // return
        return mapBeh;
    }


    /**
     * return filter for transition assessment generic form child data.
     *
     * @return Filter
     */
    private Filter<GenericFormChildData> getWorkExperienceFileter() {
        Filter beanFilter = m_spedHelper.new Filter<GenericFormChildData>(GenericFormChildData.class, m_formDDX);
        beanFilter.addAliasEqualTo(ALIAS_GFD_CH_EMBEDDED_TYPE, EMB_TYPE_WORK_EXPERIENCES);
        return beanFilter;
    }

    /**
     * return filter for transition assessment generic form child data.
     *
     * @return Filter
     */
    private Filter<GenericFormChildData> getTransitionAssessmentFileter() {
        Filter beanFilter = m_spedHelper.new Filter<GenericFormChildData>(GenericFormChildData.class, m_formDDX);
        beanFilter.addAliasEqualTo(ALIAS_GFD_CH_EMBEDDED_TYPE, EMB_TYPE_TRANSITION_ASSESSMENTS);
        return beanFilter;
    }


    /**
     * return filter for IepPerformanceLevel.
     *
     * @return Filter
     */
    private Filter<IepPerformanceLevel> getPerformanceLevelFilter() {
        Filter beanFilter = m_spedHelper.new Filter<IepPerformanceLevel>(IepPerformanceLevel.class);
        beanFilter.addEqualTo(IepPerformanceLevel.COL_TYPE, PLVL_TYPE_TRANSITION_PLANNING);
        return beanFilter;
    }

    /**
     * try find previous FormInstance than current with the same owner and form definition
     * and load it in member.
     */
    private void loadPreviousFormInstance() {

        String formDefinitionOid = m_currentFI.getFormDefinitionOid();
        String iepDataOid = m_currentFI.getOwnerObjectOid();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iepDataOid);
        criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, formDefinitionOid);
        if (!StringUtils.isEmpty(m_currentFI.getOid())) {
            criteria.addNotEqualTo(X2BaseBean.COL_OID, m_currentFI.getOid());

        }
        QueryByCriteria byCriteria = new QueryByCriteria(FormInstance.class, criteria);
        byCriteria.addOrderBy(FormInstance.COL_CREATED_TIME, false);
        m_previosFI = (FormInstance) m_broker.getBeanByQuery(byCriteria);

    }


    /**
     * Initialize.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     */
    private void initialize(GenericDetail detail, UserDataContainer userData) {
        m_userData = userData;
        if (m_broker == null) {
            m_broker = new ModelBroker(m_userData.getPrivilegeSet());
        }
        m_formDDX = detail.getDataDictionary();

        m_formDetail = RiSpedHelper.getFormDetail(detail);
        m_currentFI = m_formDetail.getFormInstance();
        loadPreviousFormInstance();
        m_modiFyTemplate = new RiModifyEmbeddedProcedure(m_userData, m_formDetail.getDataDictionary());
        m_spedHelper = m_modiFyTemplate.getSpedHelper();
        m_spedRiIepDDX = m_spedHelper.getDictionaryByExtendedDictionaryId(DDX_SPED_RI_IEP);

    }



}
