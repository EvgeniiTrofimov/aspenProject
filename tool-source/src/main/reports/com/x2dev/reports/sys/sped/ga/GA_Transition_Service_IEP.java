/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.SisPerson;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class GA_Transition_Service_IEP.
 */
public class GA_Transition_Service_IEP extends BaseFormReportJavaSource {
    /**
     * Member variables for report
     */
    private IepData m_iepData;
    private Map<String, String> m_serviceDescriptions;
    private DataDictionary m_dictionary;
    private List<String> m_serviceAreas;

    /**
     * Parameters to be passed into report
     */
    String PARAM_GRADUATION_DATE = "graduationdate";
    String PARAM_SERVICE_DESCRIPTION = "serviceDescription";
    String PARAM_SERVICE_AREA = "serviceArea";
    String PARAM_SERVICE_DATA = "serviceData";
    String PARAM_SERVICE_DATA_FORMAT = "serviceDataFormat";
    String PARAM_STUDENT_NAME = "studentName";
    String PARAM_STUDENT_NAME_VIEW = "studentNameView";
    String PARAM_STUDENT_YOG = "studentYOG";

    /**
     * Report Formats
     */
    String FORMAT_SERVICE_DATA = "SYS-SPED-GA-IEP-TRSR";

    /**
     * Student alias used by report
     */
    String ALIAS_STUDENT_GRADUATION_DATE = "DOE GRADUATION DATE";

    /**
     * Iep aliases used by report.
     * Please note these are also used as report parameter names
     */
    String ALIAS_TRANSITION_DATE_INFORMED = "transition-date-informed";
    String ALIAS_TRANSITION_DATE_RIGHTS = "transition-date-rights-xferred";
    String ALIAS_TRANSITION_DEVELOPMENT_DATE = "transition-development-date";
    String ALIAS_TRANSITION_EDUCATION_TRAINING = "transition-education-training";
    String ALIAS_TRANSITION_EMPLOYMENT = "transition-employment";
    String ALIAS_TRANSITION_INDEPENDENT_LIVING = "transition-independent-living";
    String ALIAS_TRANSITION_STRENGTHS = "transition-strengths-interests";
    String ALIAS_TRANSITION_UPDATE_DATE = "transition-update-date";

    /**
     * IepService aliases used by report.
     */
    String ALIAS_ACTIVITIES_SERVICES = "iep-svc-trans-activ-services";
    String ALIAS_SERVICE_AREA = "iep-svc-trans-service-area";

    String ALIAS_TRANSITION_CALCREDIT = "iep-trs-cal-credit";
    String ALIAS_TRANSITION_AGECREDIT = "iep-trs-age-credit";
    String ALIAS_TRANSITION_MATH_COMP = "iep-trs-math-completed";
    String ALIAS_TRANSITION_MATH_SEQ = "iep-trs-math-sequence";
    String ALIAS_TRANSITION_MATH_SUMMARY = "iep-trs-math-sequence-summary";
    /**
     * Codes used by report
     */
    String CODE_TRANSITION_SERVICE = "Transition Service";
    String CODE_EDUCATION_TRAINING = "Education/Training";
    String CODE_DEVELOPMENT_OF_EMPLOYMENT = "Development of Employment";
    String CODE_COMMUNITY_PARTICIPATION = "Community Participation";
    String CODE_ADULT_SKILLS = "Adult Living Skills & Post School Options";
    String CODE_RELATED_SERVICES = "Related Services";
    String CODE_DAILY_LIVING_SKILLS = "Daily Living Skills";

    /**
     * Constants used by report
     */
    String CONSTANT_BLANK = "";
    String CONSTANT_SPACE = " ";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        // initialize method variables
        ReportDataGrid grid = new ReportDataGrid();
        Map<String, Collection<IepService>> serviceData = new HashMap<String, Collection<IepService>>();

        // initialize member variables
        m_iepData = (IepData) getFormOwner();
        m_dictionary = getDictionary();
        initializeServiceDescriptions();
        initializeServiceAreas();

        // add subreport
        addSubReport();

        // fill in report
        if (!isBlank()) {
            addReportParameters();
            serviceData = addServiceDataToMap();
        }
        serviceData = fillEmptyServices(serviceData);
        grid = populateDataGrid(grid, serviceData);

        return grid;
    }

    /**
     * Add report parameters to grid for report.
     */
    private void addReportParameters() {
        // add iep fields to report
        SisPerson person = m_iepData.getStudent().getPerson();
        if (person != null) {
            addParameter(PARAM_STUDENT_NAME, person.getFirstName() + CONSTANT_SPACE + person.getLastName());
        }
        addParameter(PARAM_STUDENT_NAME_VIEW, m_iepData.getStudent().getNameView());
        addParameter(PARAM_STUDENT_NAME, m_iepData.getStudent().getNameView());
        addParameter(PARAM_STUDENT_YOG, Integer.valueOf(m_iepData.getStudent().getYog()));
        addParameter(ALIAS_TRANSITION_DATE_INFORMED,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_DATE_INFORMED, m_dictionary));
        addParameter(ALIAS_TRANSITION_DATE_RIGHTS,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_DATE_RIGHTS, m_dictionary));
        addParameter(ALIAS_TRANSITION_DEVELOPMENT_DATE,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_DEVELOPMENT_DATE, m_dictionary));
        addParameter(ALIAS_TRANSITION_EDUCATION_TRAINING,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_EDUCATION_TRAINING, m_dictionary));
        addParameter(ALIAS_TRANSITION_EMPLOYMENT,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_EMPLOYMENT, m_dictionary));
        addParameter(ALIAS_TRANSITION_INDEPENDENT_LIVING,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_INDEPENDENT_LIVING, m_dictionary));
        addParameter(ALIAS_TRANSITION_STRENGTHS,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_STRENGTHS, m_dictionary));
        addParameter(ALIAS_TRANSITION_UPDATE_DATE,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_UPDATE_DATE, m_dictionary));

        addParameter(ALIAS_TRANSITION_CALCREDIT,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_CALCREDIT, m_dictionary));
        addParameter(ALIAS_TRANSITION_AGECREDIT,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_AGECREDIT, m_dictionary));
        addParameter(ALIAS_TRANSITION_MATH_COMP,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_MATH_COMP, m_dictionary));
        addParameter(ALIAS_TRANSITION_MATH_SEQ,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_MATH_SEQ, m_dictionary));
        addParameter(ALIAS_TRANSITION_MATH_SUMMARY,
                m_iepData.getFieldValueByAlias(ALIAS_TRANSITION_MATH_SUMMARY, m_dictionary));

        // add student field to report
        if (m_iepData.getStudent() != null) {
            String graduationDate =
                    (String) m_iepData.getStudent().getFieldValueByAlias(ALIAS_STUDENT_GRADUATION_DATE, m_dictionary);
            addParameter(PARAM_GRADUATION_DATE, graduationDate);
        }
    }

    /**
     * For all IEP services, if they are of type "Transition Services" add
     * them to the appropriate collection. Then add the collection to a map
     * of service data.
     *
     * @return serviceData
     */
    private Map<String, Collection<IepService>> addServiceDataToMap() {
        Map<String, Collection<IepService>> serviceData = new HashMap<String, Collection<IepService>>();
        Collection<IepService> services = m_iepData.getIepServices();
        for (IepService service : services) {
            String serviceMode = service.getServiceMode();

            // if service is of type "Transition Services" add it to appropriate collection
            if (CODE_TRANSITION_SERVICE.equals(serviceMode)) {
                String serviceArea = (String) service.getFieldValueByAlias(ALIAS_SERVICE_AREA, m_dictionary);
                Collection<IepService> currentServices = new ArrayList<IepService>();

                if (serviceData.containsKey(serviceArea)) {
                    currentServices = serviceData.get(serviceArea);
                }
                currentServices.add(service);
                serviceData.put(serviceArea, currentServices);
            }
        }

        return serviceData;
    }

    /**
     * Adds the subreport used by the report.
     */
    private void addSubReport() {
        Report report = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, FORMAT_SERVICE_DATA);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        report = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SERVICE_DATA_FORMAT, new ByteArrayInputStream(report.getCompiledFormat()));
    }

    /**
     * Adds empty service collection to any service area not containing one.
     *
     * @param serviceData Map<String,Collection<IepService>>
     * @return serviceData
     */
    private Map<String, Collection<IepService>> fillEmptyServices(Map<String, Collection<IepService>> serviceData) {
        for (String serviceArea : m_serviceAreas) {
            if (!serviceData.containsKey(serviceArea)) {
                Collection<IepService> currentServices = new ArrayList<IepService>();

                IepService emptyService = X2BaseBean.newInstance(IepService.class, getBroker().getPersistenceKey());
                emptyService.setFieldValueByAlias(ALIAS_SERVICE_AREA, serviceArea, m_dictionary);
                emptyService.setFieldValueByAlias(ALIAS_ACTIVITIES_SERVICES, CONSTANT_BLANK, m_dictionary);

                currentServices.add(emptyService);
                serviceData.put(serviceArea, currentServices);
            }
        }

        return serviceData;
    }

    /**
     * Returns service description for service area if there is one.
     *
     * @param serviceArea String
     * @return serviceDescription
     */
    private String getServiceDescription(String serviceArea) {
        String serviceDescription = null;
        if (m_serviceDescriptions.containsKey(serviceArea)) {
            serviceDescription = m_serviceDescriptions.get(serviceArea);
        }

        return serviceDescription;
    }

    /**
     * Initialize map m_serviceDescriptions with service descriptions keyed on
     * service area.
     */
    private void initializeServiceDescriptions() {
        m_serviceDescriptions = new HashMap<String, String>();

        // define service descriptions
        String educationTrainingDescription = "(Goals based on academics, functional academics, " +
                "life centered competencies or career/technical or " +
                "agricultural training needs and job training.)";
        String employmentDescription = "(Goals based on occupational awareness, employment " +
                "related knowledge and skills and specific career " +
                "pathway knowledge and skills.)";
        String communityParticipationDescription = "(Goals based on knowledge and demonstration of skills " +
                "needed to participate in the community (e.g., tax forms, " +
                "voter registration, building permits, social interactions, " +
                "consumer activities, accessing and using various " +
                "transportation modes.))";
        String adultSkillsDescription = "(Goals based on skills for self-determination, interpersonal " +
                "interactions, communication, health/fitness and the knowledge " +
                "needed to successfully participate in Adult Lifestyles and " +
                "other Post School Activities (e.g., skills needed to manage a " +
                "household, maintain a budget and other responsibilities of an adult.))";
        String relatedSkillsDescription = "(Goals based on Related Services that may be required now " +
                "to help a child benefit from regular and special education " +
                "and transition services (e.g., speech/language, occupational " +
                "therapy, counseling, vocational rehabilitation training or " +
                "the planning for related services that the individual may " +
                "need access to as an adult.)";
        String dailyLivingSkills = "(Goals based on adaptive behaviors related to personal " +
                "care and well-being to decrease dependence on others.)";

        // add descriptions to service description map
        m_serviceDescriptions.put(CODE_EDUCATION_TRAINING, educationTrainingDescription);
        m_serviceDescriptions.put(CODE_DEVELOPMENT_OF_EMPLOYMENT, employmentDescription);
        m_serviceDescriptions.put(CODE_COMMUNITY_PARTICIPATION, communityParticipationDescription);
        m_serviceDescriptions.put(CODE_ADULT_SKILLS, adultSkillsDescription);
        m_serviceDescriptions.put(CODE_RELATED_SERVICES, relatedSkillsDescription);
        m_serviceDescriptions.put(CODE_DAILY_LIVING_SKILLS, dailyLivingSkills);
    }

    /**
     * Initialize m_serviceAreas with all service areas.
     */
    private void initializeServiceAreas() {
        m_serviceAreas = new ArrayList<String>();

        m_serviceAreas.add(CODE_EDUCATION_TRAINING);
        m_serviceAreas.add(CODE_DEVELOPMENT_OF_EMPLOYMENT);
        m_serviceAreas.add(CODE_COMMUNITY_PARTICIPATION);
        m_serviceAreas.add(CODE_ADULT_SKILLS);
        m_serviceAreas.add(CODE_RELATED_SERVICES);
        m_serviceAreas.add(CODE_DAILY_LIVING_SKILLS);
    }

    /**
     * Use map containing services, keyed on service to fill in grid.
     * Grid is set to have each row contain collection of services related
     * to each service area.
     *
     * @param grid ReportDataGrid
     * @param serviceData Map<String,Collection<IepService>>
     * @return grid
     */
    private ReportDataGrid populateDataGrid(ReportDataGrid grid,
                                            Map<String, Collection<IepService>> serviceData) {
        // use service data map to populate grid
        for (String serviceArea : m_serviceAreas) {
            grid.append();
            grid.set(PARAM_SERVICE_AREA, serviceArea);
            grid.set(PARAM_SERVICE_DESCRIPTION, getServiceDescription(serviceArea));
            if (serviceData.containsKey(serviceArea)) {
                BeanCollectionDataSource currentServiceData =
                        new BeanCollectionDataSource(serviceData.get(serviceArea), m_dictionary, getLocale());
                grid.set(PARAM_SERVICE_DATA, currentServiceData);
            }
        }

        grid.beforeTop();
        return grid;
    }
}
