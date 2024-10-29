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
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;


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
public class TransitionServicesFormData extends BeanReport {


    private static final String ALIAS_HB_SER_CAN_ELIGIBLE = "hb-ser-can-eligible";
    private static final String ALIAS_HB_SER_DETERMINING_ELIGIBLE = "hb-ser-determining-eligible";
    private static final String ALIAS_HB_SER_DEVELOP_PLANS_FOR_STD = "hb-ser-develop-plans-for-std";
    private static final String ALIAS_HB_SER_STD_PLANS_FOR_ENROLL = "hb-ser-std-plans-for-enroll";
    private static final String ALIAS_SERVICE_PROVIDER_OTHER = "service-provider-other";
    private static final String ALIAS_SERVICE_DESCRIPTION = "service-description";
    private static final String ALIAS_TRANSITION_AREA = "transition-area";

    private static final String ALL_GOALS_SELECTED = "*";
    private static final String ALL_GOALS_UPDATE = "All";

    private static final String NONE = "NONE";

    private static final String PARAM_DETERMINE_ELIGIBLE = "determineEligible";
    private static final String PARAM_DEVELOP_PLANS = "developPlans";
    private static final String PARAM_IS_CAN_ELIGIBLE = "isCanEligible";
    private static final String PARAM_STD_PLANS = "stdPlans";

    private static final String POSTFIX_END_DATE = "EndDate";
    private static final String POSTFIX_GOAL = "Goal";
    private static final String POSTFIX_PROVIDER_CODE = "ProviderCode";
    private static final String POSTFIX_START_DATE = "StartDate";

    private static final String PREFIX_COMMUNITY_EXPERIENCES = "comExp";
    private static final String PREFIX_DEVELOPMENT_OF_EMPLOYMENT = "devEmpl";
    private static final String PREFIX_INSTRACTION = "instr";
    private static final String PREFIX_LINKAGES = "link";
    private static final String PREFIX_LIVING_SKILLS = "livSkill";
    private static final String PREFIX_RELATED_SERVICES = "relSrvc";

    private static final String SERVICE_MODE_TRANSITION = "Transition Services";

    private static final String TRANSITION_AREA_COMMUNITY_EXPERIENCES = "COMMUNITY EXPERIENCES";
    private static final String TRANSITION_AREA_DAILY_LIVING_SKILLS = "DAILY LIVING SKILLS";
    private static final String TRANSITION_AREA_DEVELOPMENT_OF_EMPLOYMENT = "DEVELOPMENT OF EMPLOYMENT";
    private static final String TRANSITION_AREA_INSTRUCTION = "INSTRUCTION";
    private static final String TRANSITION_AREA_LINKAGES = "LINKAGES";
    private static final String TRANSITION_AREA_RELATED_SERVICES = "RELATED SERVICES";

    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
        IepData iepData = (IepData) getFormStorage();
        if (iepData != null && iepData.getOid() != null) {
            String canEligible = (String) iepData.getFieldValueByAlias(ALIAS_HB_SER_CAN_ELIGIBLE, getDictionary());
            String determineEligible = (String) iepData.getFieldValueByAlias(ALIAS_HB_SER_DETERMINING_ELIGIBLE,
                    getDictionary());
            String stdPlans = (String) iepData.getFieldValueByAlias(ALIAS_HB_SER_STD_PLANS_FOR_ENROLL, getDictionary());
            String developPlans = (String) iepData.getFieldValueByAlias(ALIAS_HB_SER_DEVELOP_PLANS_FOR_STD,
                    getDictionary());

            addParameter(PARAM_IS_CAN_ELIGIBLE, canEligible != null && canEligible.equals(BooleanAsStringConverter.TRUE)
                    ? Boolean.valueOf(true)
                    : Boolean.valueOf(false));
            addParameter(PARAM_DETERMINE_ELIGIBLE, determineEligible);
            addParameter(PARAM_STD_PLANS, stdPlans);
            addParameter(PARAM_DEVELOP_PLANS, developPlans);
            fillDefaultValue();
            for (IepService iepService : iepData.getIepServices()) {
                String prefix = null;
                String serviceMode = iepService.getServiceMode();
                if (serviceMode != null && serviceMode.equals(SERVICE_MODE_TRANSITION)) {

                    String transitionCode = (String) iepService.getFieldValueByAlias(ALIAS_TRANSITION_AREA,
                            getDictionary());

                    if (transitionCode != null) {
                        if (transitionCode.equals(TRANSITION_AREA_INSTRUCTION)) {
                            prefix = PREFIX_INSTRACTION;
                        } else if (transitionCode.equals(TRANSITION_AREA_RELATED_SERVICES)) {
                            prefix = PREFIX_RELATED_SERVICES;
                        } else if (transitionCode.equals(TRANSITION_AREA_COMMUNITY_EXPERIENCES)) {
                            prefix = PREFIX_COMMUNITY_EXPERIENCES;
                        } else if (transitionCode.equals(TRANSITION_AREA_DEVELOPMENT_OF_EMPLOYMENT)) {
                            prefix = PREFIX_DEVELOPMENT_OF_EMPLOYMENT;
                        } else if (transitionCode.equals(TRANSITION_AREA_DAILY_LIVING_SKILLS)) {
                            prefix = PREFIX_LIVING_SKILLS;
                        } else if (transitionCode.equals(TRANSITION_AREA_LINKAGES)) {
                            prefix = PREFIX_LINKAGES;
                        }
                        if (prefix != null) {
                            String description = (String) iepService.getFieldValueByAlias(ALIAS_SERVICE_DESCRIPTION,
                                    getDictionary());
                            description = StringUtils.isEmpty(description) ? NONE : description;
                            addParameter(prefix, description);

                            String goalView = iepService.getGoalView();
                            String goal = (goalView != null && goalView.equals(ALL_GOALS_SELECTED)) ? ALL_GOALS_UPDATE
                                    : goalView;
                            addParameter(prefix + POSTFIX_GOAL, goal);

                            String providerCode = (String) iepService.getFieldValueByAlias(ALIAS_SERVICE_PROVIDER_OTHER,
                                    getDictionary());
                            addParameter(prefix + POSTFIX_PROVIDER_CODE, providerCode);

                            addParameter(prefix + POSTFIX_START_DATE,
                                    m_ilSpedHelper.formatDate(iepService.getStartDate()));
                            addParameter(prefix + POSTFIX_END_DATE, m_ilSpedHelper.formatDate(iepService.getEndDate()));
                        }
                    }
                }
            }
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * put "NONE" by default on each type of "transition area" .
     */
    private void fillDefaultValue() {
        addParameter(PREFIX_INSTRACTION, NONE);
        addParameter(PREFIX_RELATED_SERVICES, NONE);
        addParameter(PREFIX_COMMUNITY_EXPERIENCES, NONE);
        addParameter(PREFIX_DEVELOPMENT_OF_EMPLOYMENT, NONE);
        addParameter(PREFIX_LIVING_SKILLS, NONE);
        addParameter(PREFIX_LINKAGES, NONE);
    }

}
