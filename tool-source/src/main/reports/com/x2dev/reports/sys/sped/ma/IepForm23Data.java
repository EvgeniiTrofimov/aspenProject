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
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.JarPluginManager;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper.MaSpedDataSource;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
//import net.sf.jasperreports5.engine.JRDataSource;
//import net.sf.jasperreports5.engine.JRException;
//import net.sf.jasperreports5.engine.JRExporterParameter;
//import net.sf.jasperreports5.engine.JasperExportManager;
//import net.sf.jasperreports5.engine.data.JRMapCollectionDataSource;
//import net.sf.jasperreports5.engine.export.JRCsvExporter;
//import net.sf.jasperreports5.engine.export.JRHtmlExporter;
//import net.sf.jasperreports5.engine.export.JRHtmlExporterParameter;
//import net.sf.jasperreports5.engine.export.JRXlsExporter;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Massachusetts IEP form. This class prepares a ReportDataGrid that contains
 * a row for each section of the IEP. Each row contains a format and a java source for the
 * corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>IEP 1 - Vision</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 2 - PLEP A</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 3 - PLEP B</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 4 - Goals</td>
 * <td>ReportDataGrid containing IepGoal records - one row per goal field to allow floating text
 * </td>
 * </tr>
 * <tr>
 * <td>IEP 5 - Services</td>
 * <td>ReportDataGrid containing IepService records (with blanks inserted to property render the
 * service grid)</td>
 * </tr>
 * <tr>
 * <td>IEP 6 - Schedule</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 7 - Assessment</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 8 - Response</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * </table>
 * <p>
 * A response page only parameter is provided. If selected, only IEP 8 is prepared.
 *
 * @author X2 Development Corporation
 */
public class IepForm23Data extends MaBeanReport {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    /*
     * -------- Input parameter constants -------
     */

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String RESPONSE_PAGE_ONLY_PARAM = "responseOnly";

    /*
     * -------- Constants for the main report -------
     */
    private static final String ALIAS_PERSONEL_TYPE = "personnel-type";
    private static final String SCV_GOAL_NO = "isv-goal-id";
    private static final String SCV_SERVICE_TYPE = "isv-service-type";
    private static final String SCV_SERVICE_PROVIDER = "isv-service-provider";
    private static final String SCV_SETTING_CODE = "isv-setting-code";
    private static final String ISV_FREQUENCY = "isv-frequency";
    private static final String ISV_LOCATION = "isv-location";
    private static final String ISV_START_DATE = "isv-start-date";
    private static final String ISV_END_DATE = "isv-end-date";

    private static final String PARAM_ADM1_DATA = "adm1Data";
    private static final String PARAM_ADM1_FORMAT = "adm1Format";
    private static final String PARAM_GOALS_DATA = "goalsData";
    private static final String PARAM_GOALS_FORMAT = "goalsFormat";
    private static final String PARAM_YEAR_SERVICES_CONSULT = "isv-year-services-consult";
    private static final String PARAM_YEAR_SERVICES_GENED = "isv-year-services-gened";
    private static final String PARAM_YEAR_SERVICES_OTHER = "isv-year-services-other";
    private static final String PARAM_EXTENDED_SERVICES_CONSULT = "isv-extended-services-consult";
    private static final String PARAM_EXTENDED_SERVICES_GENED = "isv-extended-services-gened";
    private static final String PARAM_EXTENDED_SERVICES_OTHER = "isv-extended-services-other";
    private static final String PARAM_STUDENT_AGE = "studentAge";
    private static final String PARAM_STUDENT_GRADE = "studentGradeLevel";
    private static final String PARAM_SCHOOL_TYPE = "schoolType";

    private static final String PARAM_ALIAS_AREAS_ADDRESSED_TECHNOLOGY = "iep-assistive-tech-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_GEN_CURR = "iep-autism-gen-curr-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_BEHAVIORAL = "iep-autism-behavioral-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_AAC = "iep-aac-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_COMMUNICATION = "iep-autism-communication-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_DEAF_HEARING = "iep-deaf-hard-hearing-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_BRAILLE = "iep-braille-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_SCREEN_READER = "iep-screen-reader-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_MOBILITY = "iep-mobility-areas";
    private static final String PARAM_ALIAS_AREAS_ADDRESSED_POSTSECONDARY = "iep-postsecondary-trans-areas";

    private static final String NODE_ID_IEP_IEP_LIST_DETAIL = "iep.iep.list.detail";
    private static final String NODE_ID_STD_STD_LIST_IEP_DETAIL = "student.std.list.iep.detail";
    private static final String PARAM_FORMAT_ID_GOALS = "SYS-SPED-MA-IEP-GOALS";
    private static final String PARAM_FORMAT_ADM1 = "SYS-SPED-MA-ADM1";

    /*
     * -------- Constants for the goals subreport -------
     */
    private static final String COL_GOAL = "goal";
    private static final String COL_GOAL_FIELD_ID = "fieldId";
    private static final String COL_GOAL_TEXT = "goalText";

    private static final String FIELD_ID_BASELINE = "baseline";
    private static final String FIELD_ID_BENCHMARK = "benchmark";
    private static final String FIELD_ID_GOAL = "goal";

    private static final String PARAM_COMMUNITY_SERVICES = "communityServices";
    private static final String COMMUNITY_SERVICE_TYPE = "community";

    private static final String ALIAS_ACCOM_MOD = "acm-type";
    private static final String ALIAS_ACCOM_MOD_AREAS = "acm-accom-type";
    private static final String ALIAS_ACCOM_INSTRUCTION = "acm-accom-instruction";
    private static final String ALIAS_ACCOM_RESPONSE = "acm-accom-response";
    private static final String ALIAS_ACCOM_SCHEDULE = "acm-accom-schedule";
    private static final String ALIAS_ACCOM_SETTING = "acm-accom-setting";
    private static final String ALIAS_MOD_CONTENT = "acm-accom-content";

    private static final String ALIAS_COM_AGENT = "acm-agency";
    private static final String ALIAS_COM_SUPP_PROV = "acm-support-provided";
    private static final String ALIAS_COM_SUPP_ROLE = "acm-support-role";
    private static final String ALIAS_COM_CUPP_CONTACT = "acm-support-contact";

    private static final String ACCOM_TYPE_ACCOM = "accom";
    private static final String ACCOM_TYPE_MOD = "mod";
    private static final String ACCOM_TYPE_TESTING = "Testing";
    private static final String ACCOM_TYPE_COMMUNITY = "community";
    private static final String ACCOM_CASE_CLASSROOM = "Classroom";
    private static final String ACCOM_CASE_NONACC = "NonAcc";
    private static final String ACCOM_CASE_EXTRACUR = "Extracur";
    private static final String ACCOM_CASE_COMMUNITY = "Community";
    private static final String ACCOM_AREA_ENGLISH = "English";
    private static final String ACCOM_AREA_HISTORY = "History";
    private static final String ACCOM_AREA_MATH = "Math";
    private static final String ACCOM_AREA_SCIENCE = "Science";
    private static final String ACCOM_AREA_READING = "Reading";
    private static final String ACCOM_AREA_ELL = "ELL";
    private static final String ACCOM_AREA_OTHER = "Other";
    private static final String ACCOM_AREA_ALL = "All";
    private static final String ACCOM_AREAS[] =
            new String[] {ACCOM_AREA_ENGLISH, ACCOM_AREA_MATH, ACCOM_AREA_SCIENCE, ACCOM_AREA_OTHER};

    private static final String ALIAS_SPED_DECISION = "sped-decision";

    /*
     * -------- Constant parameters to see if page is printable or no -------
     */
    private static final String VISION = "vision";
    private static final String PLEP_A = "plaepA";
    private static final String PLEP_B = "plaepB";
    private static final String GOALS = "goals";
    private static final String SERVICES = "services";
    private static final String SCHEDULE = "schedule";
    private static final String ASSESSMENT = "assessment";
    private static final String RESPONSE = "response";

    /*
     * -------- Constants for the services subreport -------
     */
    private static final String PRINT_ADM1 = "printAdm";

    private static final String SERVICES_SECTION_A_MODE = "Consultation";
    private static final String SERVICES_SECTION_B_MODE = "SpecialEd - General";
    private static final String SERVICES_SECTION_C_MODE = "SpecialEd - Other";

    private static final String STD_GRADE_LEVEL = "gradeLevel";
    private static final String STD_DISABILITY = "disability";
    private static final String STD_DISABILITIES = "disabilities";
    private static final String STD_SECONDARY_DISABILITY = "secondaryDisability";

    private static final String RTB_SIS_IMAGES = "SIS Image";
    private static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";
    private static final String CODE_SIGNATURE_LEA = "LEA Admin";
    private static final String CODE_SIGNATURE_SEA = "SPED Admin";
    private static final String PARAM_SIGNATURE_LEA = "ImgLEAAdmin";
    private static final String PARAM_SIGNATURE_SEA = "ImgSPEDAdmin";

    Map<String, String> m_areasAffected = new HashMap<String, String>();
    ToolJob m_adm1Job;

    /**
     * Data source that allows access to the IepGoal.getObjectivesView(broker) method.
     * This allows a broker to be attached and used to access that method.
     *
     * @author Follett Software Company
     * @copyright 2023
     */
    public class GoalDataSource extends BeanCollectionDataSource {
        private static final String FIELD_OBJECTIVES_VIEW = "objectivesView";
        private X2Broker m_bbroker;

        /**
         * Constructs a new SimpleFormDataSource.
         *
         * @param goals
         * @param dictionary DataDictionary
         * @param locale Locale
         */
        public GoalDataSource(Collection<IepGoal> goals, DataDictionary dictionary, Locale locale) {
            super(goals, dictionary, locale);
        }

        /**
         * Set the broker so it can be used to get the objectives view.
         *
         * @param broker
         */
        public void setBroker(X2Broker broker) {
            m_bbroker = broker;
        }

        /**
         * add override field name for objectives view.
         *
         * @see com.follett.fsc.core.k12.tools.reports.BeanDataSource#getFieldValue(java.lang.String)
         */
        @Override
        protected Object getFieldValue(String fieldName) throws X2BaseException {
            if (FIELD_OBJECTIVES_VIEW.equals(fieldName)) {
                return ((IepGoal) getCurrentBean()).getObjectivesView(m_bbroker);
            }
            return super.getFieldValue(fieldName);
        }
    }

    private MaSpedAttribHelper m_attribHelper;
    private boolean m_isDetailsView = false;

    /**
     * Gets the form storage.
     *
     * @return X 2 base bean
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#getFormStorage()
     */
    @Override
    /**
     * if report print from details view - storage should be existing IEP data object. It need for
     * show real data, not blank.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#getFormStorage()
     */
    public X2BaseBean getFormStorage() {
        X2BaseBean storage = null;
        if (m_isDetailsView) {
            storage = getFormOwner();
        } else {
            storage = super.getFormStorage();
        }
        return storage;
    }

    /**
     * Returns true if the form being printed is blank.
     * form storage has specific initialize. original method used m_formStorage member variable
     * ignore specific initialize.
     * there is why method was override and used getFormStorage() instead m_formStorage member
     * variable
     *
     * @return boolean
     */
    @Override
    public boolean isBlank() {
        return getFormStorage().getOid() == null;
    }

    /**
     * Override a public method from a core class to allow the caller to
     * inject a replacement IEP and reset the report for the new IEP.
     *
     * A bit of a hack, to allow a wrapping report to reinitialize this report instance
     * with a different IEP, reinitialize it and run it again. This must happen through a
     * method that already exists in parent core classes.
     * This is the only public settable method from core classes.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#setOrganization(com.follett.fsc.core.k12.beans.Organization)
     */
    @Override
    public void setFormDefinition(FormDefinition formDefinition) {
        if (formDefinition != null) {
            if (formDefinition.getOid() != null) {
                super.setFormDefinition(formDefinition);
            } else {
                // The IEP OID is stored in the category field.
                // Retrieve that and set it as the new storage IepData,
                // reinitialize everything for the new IEP.
                String iepOid = formDefinition.getCategory();
                if (iepOid != null && iepOid.startsWith("IEP")) {
                    IepData iepData = getBroker().getBeanByOid(IepData.class, iepOid);
                    if (iepData != null) {
                        setFormOwner(iepData);
                        setFormStorage(iepData);
                        setDictionary(DataDictionary.getDistrictDictionary(iepData.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey()));
                        addFormParameters();
                    }
                }
            }
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        IepData iep = getIep();

        MaSpedDataSource dataGrid = m_attribHelper.getMaSpedDataSource(iep, iep, getDictionary(), getLocale());

        loadRefTabes();
        setDisability(iep);
        setGradeLevel(iep);
        getServices(iep);
        getSpedDecision(iep);
        // get688Agency(iep);
        getTestingAccommodations(iep);
        getGoalsSubreport(iep);
        getStudentAge(iep);
        getAdm1Subreport();

        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_TECHNOLOGY,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_TECHNOLOGY));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_GEN_CURR,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_GEN_CURR));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_BEHAVIORAL,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_BEHAVIORAL));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_AAC,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_AAC));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_COMMUNICATION,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_AUTISM_COMMUNICATION));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_DEAF_HEARING,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_DEAF_HEARING));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_BRAILLE,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_BRAILLE));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_SCREEN_READER,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_SCREEN_READER));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_MOBILITY,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_MOBILITY));
        addParameter(PARAM_ALIAS_AREAS_ADDRESSED_POSTSECONDARY,
                getAreasAddressed(iep, PARAM_ALIAS_AREAS_ADDRESSED_POSTSECONDARY));

        addParameter(PARAM_SIGNATURE_LEA,
                getBase64ImageString(CODE_SIGNATURE_LEA, iep.getStudent().getSchoolOid(), getBroker()));
        addParameter(PARAM_SIGNATURE_SEA,
                getBase64ImageString(CODE_SIGNATURE_SEA, iep.getStudent().getSchoolOid(), getBroker()));

        return dataGrid;
    }

    /**
     * Direct call through to getherDate()
     */
    @Override
    public Object getDataSource() {
        try {
            return gatherData();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Override for IEP Report List mode.
     * Return the current Parameters map for the current instance.
     * If the parameters map is not null, use it to reset the current parameters map.
     */
    @Override
    public Map copyParameterMap(Map parameters) {
        if (parameters != null) {
            getParameters().clear();
            getParameters().putAll(parameters);
        }
        return getParameters();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        List<String> detailsNodes =
                new ArrayList<String>(Arrays.asList(NODE_ID_IEP_IEP_LIST_DETAIL, NODE_ID_STD_STD_LIST_IEP_DETAIL));
        if (detailsNodes.contains(userData.getCurrentNode().getId())) {
            m_isDetailsView = true;
        }
    }

    /**
     * Load accommodations and modifications.
     *
     * @param iep
     */
    private void getTestingAccommodations(IepData iep) {
        Map<String, Map<String, String>> accoms = new HashMap<String, Map<String, String>>();
        Map<String, Map<String, String>> mods = new HashMap<String, Map<String, String>>();
        List comms = new ArrayList<Map<String, Object>>();
        // <Map<String, Object>>
        Map<String, String> testing = new HashMap<String, String>();

        List<String> accomTypes = new ArrayList<String>();
        accomTypes.add(ACCOM_TYPE_ACCOM);
        accomTypes.add(ACCOM_TYPE_MOD);
        accomTypes.add(ACCOM_TYPE_TESTING);
        accomTypes.add(ACCOM_TYPE_COMMUNITY);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, iep.getOid());
        criteria.addIn(IepAccommodation.COL_TYPE, accomTypes);
        QueryByCriteria accommodationsQuery = new QueryByCriteria(IepAccommodation.class, criteria);
        Collection<IepAccommodation> accomData = getBroker().getCollectionByQuery(accommodationsQuery);

        for (IepAccommodation accom : accomData) {
            String mapType = accom.getType();
            if (ACCOM_TYPE_TESTING.equals(mapType)) {
                String name = accom.getName();
                String area = accom.getContentArea();
                if (area != null) {
                    if (area.contains(ACCOM_AREA_ENGLISH)) {
                        area = ACCOM_AREA_ENGLISH;
                    } else if (area.contains(ACCOM_AREA_HISTORY)) {
                        area = ACCOM_AREA_OTHER; // Report has Other.
                    } else if (area.contains(ACCOM_AREA_MATH)) {
                        area = ACCOM_AREA_MATH;
                    } else if (area.contains(ACCOM_AREA_SCIENCE)) {
                        area = ACCOM_AREA_SCIENCE;
                    } else if (area.contains(ACCOM_AREA_READING)) {
                        area = ACCOM_AREA_ENGLISH; // Report has English.
                    } else if (area.contains(ACCOM_AREA_ELL)) {
                        area = ACCOM_AREA_OTHER; // Report has Other.
                    } else if (area.contains(ACCOM_AREA_ALL)) {
                        area = ACCOM_AREA_ALL;
                    } else {
                        area = ACCOM_AREA_OTHER; // Anything else to Other.
                    }
                    if (area == ACCOM_AREA_ALL) {
                        for (String areaIdx : ACCOM_AREAS) {
                            String nameString = testing.get(areaIdx);
                            if (nameString == null) {
                                nameString = name;
                            } else {
                                nameString += "\n" + name;
                            }
                            testing.put(areaIdx, nameString);
                        }
                    } else {
                        String nameString = testing.get(area);
                        if (nameString == null) {
                            nameString = name;
                        } else {
                            nameString += "\n" + name;
                        }
                        testing.put(area, nameString);
                    }
                }
            }
        }

        DataDictionaryField accomModField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ACCOM_MOD);
        DataDictionaryField accomAreasField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ACCOM_MOD_AREAS);
        Collection<ReferenceCode> codes = null;
        if (accomAreasField != null) {
            ReferenceTable refTable = accomAreasField.getReferenceTable();
            if (refTable != null) {
                codes = refTable.getReferenceCodes(m_broker);
            }
        }
        accomTypes = new ArrayList<String>();
        accomTypes.add(ACCOM_TYPE_ACCOM);
        accomTypes.add(ACCOM_TYPE_MOD);
        accomTypes.add(ACCOM_TYPE_COMMUNITY);
        criteria = new Criteria();
        criteria.addEqualTo(UserDefinedTableD.COL_IEP_DATA_OID, iep.getOid());
        criteria.addIn(accomModField.getJavaName(), accomTypes);
        accommodationsQuery = new QueryByCriteria(UserDefinedTableD.class, criteria);
        Collection<UserDefinedTableD> accomModData = getBroker().getCollectionByQuery(accommodationsQuery);
        for (UserDefinedTableD accom : accomModData) {
            String mapType = (String) accom.getFieldValueByBeanPath(accomModField.getJavaName());
            String area = (String) accom.getFieldValueByBeanPath(accomAreasField.getJavaName());
            if (codes != null) {
                for (ReferenceCode code : codes) {
                    if (code.getCode().equals(area)) {
                        area = code.getLocalCode();
                        break;
                    }
                }
            }
            if (ACCOM_TYPE_ACCOM.equals(mapType)) {
                Map<String, String> areasMap = accoms.get(area);
                if (areasMap == null) {
                    areasMap = new HashMap<String, String>(4);
                    accoms.put(area, areasMap);
                }
                appendArea(areasMap, accom, ALIAS_ACCOM_INSTRUCTION);
                appendArea(areasMap, accom, ALIAS_ACCOM_RESPONSE);
                appendArea(areasMap, accom, ALIAS_ACCOM_SCHEDULE);
                appendArea(areasMap, accom, ALIAS_ACCOM_SETTING);
            }
            if (ACCOM_TYPE_MOD.equals(mapType)) {
                Map<String, String> areasMap = mods.get(area);
                if (areasMap == null) {
                    areasMap = new HashMap<String, String>(4);
                    mods.put(area, areasMap);
                }
                appendArea(areasMap, accom, ALIAS_ACCOM_INSTRUCTION);
                appendArea(areasMap, accom, ALIAS_MOD_CONTENT);
                appendArea(areasMap, accom, ALIAS_ACCOM_RESPONSE);
            }
            if (ACCOM_TYPE_COMMUNITY.equals(mapType)) {
                Map<String, String> commMap = new HashMap<String, String>();
                appendArea(commMap, accom, ALIAS_COM_AGENT);
                appendArea(commMap, accom, ALIAS_COM_SUPP_PROV);
                appendArea(commMap, accom, ALIAS_COM_SUPP_ROLE);
                appendArea(commMap, accom, ALIAS_COM_CUPP_CONTACT);
                comms.add(commMap);
            }
        }
        if (comms.size() == 0) {
            comms.add(new HashMap<String, String>());
        }

        addParameter(ACCOM_TYPE_ACCOM, accoms);
        addParameter(ACCOM_TYPE_MOD, mods);
        addParameter(ACCOM_TYPE_TESTING, testing);
        addParameter(PARAM_COMMUNITY_SERVICES, getMappedCollectionDataSource(comms));

    }

    /**
     * Add or append a bean value to a map entry by the same alias.
     *
     * @param areasMap
     * @param bean
     * @param alias
     */
    private void appendArea(Map<String, String> areasMap, X2BaseBean bean, String alias) {
        String value = areasMap.get(alias);
        String areaValue = (String) bean.getFieldValueByAlias(alias, getDictionary());
        if (value == null) {
            value = areaValue;
        } else if (areaValue != null) {
            value += "\n" + areaValue;
        }
        areasMap.put(alias, value);
    }

    /**
     * Loads goal data into dataset.
     *
     * @param iep
     */
    private void getGoalsSubreport(IepData iep) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, iep.getOid());

        QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, criteria);

        goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
        goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);

        Collection<IepGoal> goals = getBroker().getCollectionByQuery(goalsQuery);
        if (goals.size() == 0) {
            goals.add(X2BaseBean.newInstance(IepGoal.class, getDictionary()));
        }
        GoalDataSource dataSource = new GoalDataSource(goals, getDictionary(), getLocale());
        dataSource.setBroker(getBroker());
        addParameter(PARAM_GOALS_DATA, dataSource);
        addParameter(PARAM_GOALS_FORMAT, getSubreportFormat(PARAM_FORMAT_ID_GOALS));
    }

    private void getServices(IepData iep) {
        DecimalFormat decimalFormat = new DecimalFormat("##0");
        Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        List<Map<String, Object>> yearServicesConsult = new ArrayList<Map<String, Object>>(8);
        List<Map<String, Object>> yearServicesGenEd = new ArrayList<Map<String, Object>>(8);
        List<Map<String, Object>> yearServicesDirect = new ArrayList<Map<String, Object>>(8);
        List<Map<String, Object>> extendedServicesConsult = new ArrayList<Map<String, Object>>(8);
        List<Map<String, Object>> extendedServicesGenEd = new ArrayList<Map<String, Object>>(8);
        List<Map<String, Object>> extendedServicesDirect = new ArrayList<Map<String, Object>>(8);
        // if blank need empty lines for services
        if (iep.getOid() != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepService.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);

            servicesQuery.addOrderByAscending(IepService.COL_SERVICE_MODE);
            servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW); // Focus on goal number
            servicesQuery.addOrderByAscending(IepService.COL_SERVICE_CODE);

            Collection<IepService> servicesData = getBroker().getCollectionByQuery(servicesQuery);

            if (servicesData != null) {
                Iterator iterator = servicesData.iterator();
                while (iterator.hasNext()) {
                    IepService service = (IepService) iterator.next();
                    Map<String, Object> row = new HashMap<String, Object>();

                    // Goal number
                    StringBuilder goalNumber = new StringBuilder();
                    Collection<IepServiceGoalAlignment> goals = service.getIepServiceGoalAlignments(getBroker());
                    for (IepServiceGoalAlignment sga : goals) {
                        if (goalNumber.length() > 0) {
                            goalNumber.append(", ");
                        }
                        goalNumber.append(sga.getIepGoal().getId());
                    }
                    // Goal Number.
                    row.put(SCV_GOAL_NO, goalNumber.toString());

                    // Type of service.
                    row.put(SCV_SERVICE_TYPE, service.getServiceType());

                    // Service provider.
                    row.put(SCV_SERVICE_PROVIDER, service.getFieldValueByAlias(ALIAS_PERSONEL_TYPE, getDictionary()));

                    // Setting code
                    row.put(SCV_SETTING_CODE, service.getSettingCode());

                    // Location
                    String location = (String) service.getFieldValueByAlias(ISV_LOCATION, getDictionary());
                    row.put(ISV_LOCATION, location == null ? null : location.trim());

                    // Frequency/Duration/Length
                    BigDecimal freq = service.getFrequency();
                    int duration = service.getDuration();
                    String cycle = service.getCycle();

                    StringBuilder builder = new StringBuilder();
                    if (freq != null && freq.floatValue() > 0.0f) {
                        builder.append(decimalFormat.format(freq));
                        if (duration > 0) {
                            builder.append(" x ");
                        }
                    }
                    if (duration > 0) {
                        builder.append(Integer.toString(duration));
                    }
                    if (builder.length() > 0 && !StringUtils.isEmpty(cycle)) {
                        builder.append(" / ");
                    }
                    if (!StringUtils.isEmpty(cycle)) {
                        builder.append(cycle);
                    }
                    row.put(ISV_FREQUENCY, builder.toString());

                    // Start Date
                    if (service.getStartDate() != null) {
                        row.put(ISV_START_DATE, converter.javaToString(service.getStartDate()));
                    }

                    // End Date
                    if (service.getStartDate() != null) {
                        row.put(ISV_END_DATE, converter.javaToString(service.getEndDate()));
                    }

                    boolean ey = service.getExtendedYearIndicator();
                    String serviceMode = service.getServiceMode();
                    if (SERVICES_SECTION_A_MODE.equals(serviceMode)) {
                        if (ey) {
                            extendedServicesConsult.add(row);
                        } else {
                            yearServicesConsult.add(row);
                        }
                    } else if (SERVICES_SECTION_B_MODE.equals(serviceMode)) {
                        if (ey) {
                            extendedServicesGenEd.add(row);
                        } else {
                            yearServicesGenEd.add(row);
                        }
                    } else if (SERVICES_SECTION_C_MODE.equals(serviceMode)) {
                        if (ey) {
                            extendedServicesDirect.add(row);
                        } else {
                            yearServicesDirect.add(row);
                        }
                    }
                }
            }
        }

        Map tmap = new HashMap();
        tmap.put(ISV_LOCATION, " "); // non empty string so the default does not display.
        while (yearServicesConsult.size() < 1) {
            yearServicesConsult.add(tmap);
        }
        while (yearServicesGenEd.size() < 1) {
            yearServicesGenEd.add(tmap);
        }
        while (yearServicesDirect.size() < 1) {
            yearServicesDirect.add(tmap);
        }
        while (extendedServicesConsult.size() < 1) {
            extendedServicesConsult.add(tmap);
        }
        while (extendedServicesGenEd.size() < 1) {
            extendedServicesGenEd.add(tmap);
        }
        while (extendedServicesDirect.size() < 1) {
            extendedServicesDirect.add(tmap);
        }

        addParameter(PARAM_YEAR_SERVICES_CONSULT, getMappedCollectionDataSource((List) yearServicesConsult));
        addParameter(PARAM_YEAR_SERVICES_GENED, getMappedCollectionDataSource((List) yearServicesGenEd));
        addParameter(PARAM_YEAR_SERVICES_OTHER, getMappedCollectionDataSource((List) yearServicesDirect));
        addParameter(PARAM_EXTENDED_SERVICES_CONSULT, getMappedCollectionDataSource((List) extendedServicesConsult));
        addParameter(PARAM_EXTENDED_SERVICES_GENED, getMappedCollectionDataSource((List) extendedServicesGenEd));
        addParameter(PARAM_EXTENDED_SERVICES_OTHER, getMappedCollectionDataSource((List) extendedServicesDirect));
    }

    /**
     * Retrieve the value in the field sped-decision.
     * This alias field could be defined on the Student record or on the IEP record.
     * Check where the alias field is and get the value from the correct record.
     *
     * @param iep
     */
    private void getSpedDecision(IepData iep) {
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_SPED_DECISION);
        String decision = null;
        if (field != null) {
            String code = null;
            DataDictionaryTable table = field.getTable();
            if ("IEP".equals(table.getObjectPrefix())) {
                code = (String) iep.getFieldValueByBeanPath(field.getJavaName());
            } else if ("STD".equals(table.getObjectPrefix())) {
                SisStudent student = iep.getStudent();
                code = (String) student.getFieldValueByBeanPath(field.getJavaName());
            }
            if (code != null) {
                ReferenceTable reftbl = field.getReferenceTable();
                if (reftbl != null) {
                    Collection<ReferenceCode> codes = reftbl.getReferenceCodes(getBroker());
                    for (ReferenceCode refCode : codes) {
                        if (code.equals(refCode.getCode())) {
                            decision = refCode.getLocalCode();
                            break;
                        }
                    }
                }
            }
        }
        addParameter(ALIAS_SPED_DECISION, decision);
    }

    /**
     * Sets the primary disability.
     *
     * @param iep void
     */
    private void setDisability(IepData iep) {
        String primaryDisabiity = "";
        StringBuilder secondaryDisability = new StringBuilder();
        Set<String> disabilityStateCodes = new HashSet<String>();
        DataDictionaryField field = getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                IepDisability.COL_DISABILITY_CODE);
        ReferenceTable disabilityCodesTable = field.getReferenceTable();
        Collection<ReferenceCode> disabilityCodes = disabilityCodesTable.getReferenceCodes(getBroker());
        Collection<IepDisability> disabilities = iep.getIepDisability(getBroker());
        for (IepDisability disability : disabilities) {
            if (disability.getPrimaryIndicator()) {
                primaryDisabiity = disability.getDisabilityCode();
            } else {
                if (secondaryDisability.length() > 0) {
                    secondaryDisability.append(", ");
                }
                secondaryDisability.append(disability.getDisabilityCode());
            }
            for (ReferenceCode code : disabilityCodes) {
                if (code.getCode().equals(disability.getDisabilityCode())) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        disabilityStateCodes.add(code.getStateCode());
                    }
                    break;
                }
            }
        }
        addParameter(STD_DISABILITIES, disabilityStateCodes);
        addParameter(STD_DISABILITY, primaryDisabiity);
        addParameter(STD_SECONDARY_DISABILITY, secondaryDisability.toString());
    }

    private void getStudentAge(IepData iep) {
        // get age as of iep start date.
        PlainDate startDate = new PlainDate();
        int currentAgeOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(startDate);
        int ageOfStudent = 0;
        // get age as of iep start date.
        if (iep.getStartDate() != null) {
            ageOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(iep.getStartDate());
        } else {
            ageOfStudent = currentAgeOfStudent;
        }

        String gradeLevelAtStart = iep.getStudent().getGradeLevel();
        int yogAtStart = getYog(iep.getStudent(), iep.getStartDate());
        int schoolYearAtStart = getSchoolYear(iep.getStartDate());

        // get grade level on creation time based on iep start date, if not form creation date,
        // on most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        List<String> grades =
                StudentManager.getMatchingGradeLevels(maxGradeLevel, yogAtStart, schoolYearAtStart, gradeLevels);
        if (grades != null && !grades.isEmpty()) {
            gradeLevelAtStart = grades.get(0);
        }

        SisSchool school = iep.getStudent().getSchool();
        String type = school.getSchoolTypeCode();
        if (type != null) {
            if (StringUtils.isEqualsIgnoreCase("MIDDLE", type)) {
                addParameter(PARAM_SCHOOL_TYPE, "M");
            } else if (StringUtils.isEqualsIgnoreCase("ELEMENTARY", type)) {
                addParameter(PARAM_SCHOOL_TYPE, "E");
            }
        }

        addParameter(PARAM_STUDENT_AGE, String.valueOf(ageOfStudent) + "yrs:" + gradeLevelAtStart + "g");
        addParameter(PARAM_STUDENT_GRADE,
                String.valueOf(currentAgeOfStudent) + "yrs:" + iep.getStudent().getGradeLevel() + "g");
    }

    /**
     * Gets the school year for a particular date. If no school year matches, return the most recent
     * school year before the date.
     *
     * @param startDate the start date
     * @return the school year
     */
    private int getSchoolYear(PlainDate startDate) {
        DistrictSchoolYearContext ctx = null;
        if (startDate != null) {
            // get matching CTX
            X2Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    ctx = (DistrictSchoolYearContext) iterator.next();
                }
            }

            // get latest CTX before date
            if (ctx == null) {
                criteria = new X2Criteria();
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_END_DATE);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    if (iterator.hasNext()) {
                        ctx = (DistrictSchoolYearContext) iterator.next();
                    }
                }
            }
        } else {
            ctx = getCurrentContext();
        }
        return ctx.getSchoolYear();
    }

    /**
     * Preload areas addressed reference codes into map.
     */
    private void loadRefTabes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRelatedArea");
        BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        for (ReferenceCode code : codes) {
            m_areasAffected.put(code.getCode(), code.getLocalCode());
        }
    }

    /**
     * For a given field, break out reference codes and get a holding string of all local codes.
     *
     * @param iep
     * @param alias
     * @return
     */
    private String getAreasAddressed(IepData iep, String alias) {
        String value = "";
        String areasAffected = (String) iep.getFieldValueByAlias(alias, getDictionary());
        List<String> codes = StringUtils.convertDelimitedStringToList(areasAffected, ",", true);
        for (String code : codes) {
            String localCode = m_areasAffected.get(code);
            if (localCode != null) {
                value += localCode;
            }
        }
        return value;
    }

    /**
     * Gets the yog for the student on a particular date
     *
     * @param student the student
     * @param startDate the start date
     * @return the yog
     */
    private int getYog(SisStudent student, PlainDate startDate) {
        int yog = student.getYog();
        if (startDate != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_YOG, Integer.valueOf(0));
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    StudentEnrollment enr = (StudentEnrollment) iterator.next();
                    if (enr.getYog() > 0) {
                        yog = enr.getYog();
                    }
                }
            }
        }
        return yog;
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned. (IEP created automatic in BaseFormReportJavaSource in initialize method and set
     * like storage)
     *
     * @return IepData
     */
    private IepData getIep() {
        return (IepData) getFormStorage();
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private Object getSubreportFormat(String formatId) {
        Object loadedJRReport = null;
        try {
        	Report report = ReportUtils.getReport(formatId, getBroker());
            byte[] compiledFormat = report.getCompiledFormat();
            if (Report.REPORT_ENGINE_5_RELEASE.equals(report.getEngineVersion())) {
            	loadedJRReport = net.sf.jasperreports5.engine.util.JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
            } else if (Report.REPORT_ENGINE_6_RELEASE.equals(report.getEngineVersion())) { 
            	loadedJRReport = net.sf.jasperreports6.engine.util.JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
            }
            
        } catch (net.sf.jasperreports5.engine.JRException | net.sf.jasperreports6.engine.JRException e) {
            String message = "ERROR: Loading subreport from report " + formatId;
            message += "\n" + e.getMessage();
            this.addCustomErrorMessage(message);
        }
        return loadedJRReport;
    }

    /**
     * Return student contacts which added like iep team members.
     * method created instead SpedUtils.getStudentContacts, because SpedUtils working with all
     * contacts
     * Contact sorted by COL_FORM_PRIORITY and limited by maxContacts param
     *
     * @param iep IepData
     * @param maxContacts int
     * @return List
     */
    private List<StudentContact> getStudentContacts(IepData iep, int maxContacts) {
        List<StudentContact> contacts = new ArrayList<StudentContact>();
        /*
         * Load the student contact team members with form priority ordered by form priority
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
        criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
        criteria.addNotEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY);
        contacts.addAll(getBroker().getCollectionByQuery(query));

        if (contacts.isEmpty()) {
            /*
             * Load additional student contact team members without form priority ordered by
             * emergency priority
             */
            criteria = new X2Criteria();
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
            criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
            criteria.addEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());
            query = new QueryByCriteria(StudentContact.class, criteria);

            query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);
            contacts.addAll(getBroker().getCollectionByQuery(query));
        }

        if (maxContacts != 0 && contacts.size() > maxContacts) {
            contacts = contacts.subList(0, maxContacts);
        }

        return contacts;
    }

    // ///////////////////////////////////////////////

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        String gradeLevel = null;

        if (iep.getOid() != null) {
            // get age on as of iep start date.
            PlainDate startDate = new PlainDate();
            if (iep.getStartDate() != null) {
                startDate = iep.getStartDate();
            }

            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (StudentEnrollment e : enrollments) {
                if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                    // student's YOG at this particular time
                    int yog = e.getYog();

                    // get the school year from basedDate
                    X2Criteria schoolYearCriteria = new X2Criteria();
                    schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                    schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                    QueryByCriteria schoolYearQuery =
                            new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                    DistrictSchoolYearContext ctx =
                            (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                    String currentContextOid = getCurrentContext().getContextId();
                    if (!StringUtils.isEmpty(currentContextOid) && ctx != null
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else if (ctx != null) {
                        int schoolYear = ctx.getSchoolYear();
                        List<String> grades =
                                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                        gradeLevel = grades.get(0);
                    }
                    break;
                }
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = iep.getStudent().getGradeLevel();
            }
        }
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }

    /**
     * Gets the base 64 image string.
     *
     * @param imageCode String
     * @param broker X2Broker
     * @return String
     */
    private String getBase64ImageString(String imageCode, String schoolOid, X2Broker broker) {
        String base64Image = "";
        List<String> ownerOids = new ArrayList<String>();
        ownerOids.add(OrganizationManager.ROOT_ORGANIZATION);
        if (!StringUtils.isEmpty(schoolOid)) {
            ownerOids.add(schoolOid);
        }
        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER +
                ReferenceTable.COL_USER_NAME, RTB_SIS_IMAGES);
        imageCriteria.addEqualTo(ReferenceCode.COL_CODE, imageCode);
        imageCriteria.addIn(ReferenceCode.COL_OWNER_OID, ownerOids);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        imageQuery.addOrderBy(ReferenceCode.COL_OWNER_TYPE, false);
        ReferenceCode rcdBean = broker.getBeanByQuery(imageQuery);
        if (rcdBean != null) {
            ExtendedDataDictionary ddx = rcdBean.getExtendedDataDictionary();
            if (ddx != null) {
                DataDictionary rtbDictionary =
                        DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64, rtbDictionary);
                if (StringUtils.isEmpty(base64Image)) {
                    base64Image = "";
                }
            }
        }
        return base64Image;
    }

    /**
     * 
     * @param list
     * @return
     */
    private Object getMappedCollectionDataSource(List list) {
        Report report = (Report) getJob().getTool();
        if (Report.REPORT_ENGINE_5_RELEASE.equals(report.getEngineVersion())) {
        	return new net.sf.jasperreports5.engine.data.JRMapCollectionDataSource(list);
        } else if (Report.REPORT_ENGINE_6_RELEASE.equals(report.getEngineVersion())) { 
        	return new net.sf.jasperreports6.engine.data.JRMapCollectionDataSource(list);
        }
        return null;
    }
    
    // ** Prepare ADM1 subreport ****************************

    /**
     * Report parameter containing the first priority contact
     */
    public static final String PARAM_CONTACT_0 = "contact0";
    /**
     * Report parameter containing the second priority contact
     */
    public static final String PARAM_CONTACT_1 = "contact1";
    /**
     * Report parameter containing accommodation strings for PLEPA, PLEPB and Assessments.
     */
    public static final String PARAM_ACCOMMODATIONS_PLEPA = "accommodations-plepa";
    public static final String PARAM_ACCOMMODATIONS_PLEPB = "accommodations-plepb";
    public static final String PARAM_ACCOMMODATIONS_ASSESSMENT = "accommodations-assessment";
    /**
     * Report parameter containing a the most recent IEP meeting
     */
    public static final String PARAM_MEETING = "meeting";
    /**
     * Report parameter containing a the most recent IEP meeting type name from the related
     * preference
     */
    public static final String PARAM_MEETING_TYPE = "meetingType";
    /**
     * Report parameter containing a the first Placement of IEPData
     */
    public static final String FIRST_PLACEMENT = "firstPlacement";
    /**
     * Report parameter containing a the second Placement of IEPData
     */
    public static final String SECOND_PLACEMENT = "secondPlacement";
    /**
     * Report parameter containing a the third Placement of IEPData
     */
    public static final String THIRD_PLACEMENT = "thirdPlacement";

    /**
     * Check if the ADM1 will be included, and include it in the subreport parameters.
     */
    protected void getAdm1Subreport() {
        Boolean printAdm1 = (Boolean) getParameter(PRINT_ADM1);
        if (printAdm1 != null && printAdm1.booleanValue()) {
            addParameter(PARAM_ADM1_DATA, getDataSourceADM());
            addParameter(PARAM_ADM1_FORMAT, getSubreportFormat(PARAM_FORMAT_ADM1));
        }
    }

    /**
     * Gets the data source ADM.
     *
     * @return JR data source
     */
    private Object getDataSourceADM() {
        IepData iep = null;

        if (!isBlank()) {
            iep = (IepData) getFormOwner();
            List<IepMeeting> meetings = (List<IepMeeting>) iep.getIepMeeting();
            String[] typeCodePreferenceKeys = IepMeeting.TypeCode.getDisplayPreferences();
            if (meetings != null && meetings.size() > 0) {
                IepMeeting meeting = meetings.get(meetings.size() - 1);
                int typeCode = meeting.getTypeCode();
                String meetingTypeName = PreferenceManager.getPreferenceValue(
                        getOrganization(), typeCodePreferenceKeys[typeCode]);
                addParameter(PARAM_MEETING, meeting);
                addParameter(PARAM_MEETING_TYPE, meetingTypeName);
            }

            // get age as of iep start date.
            PlainDate startDate = new PlainDate();
            if (iep.getStartDate() != null) {
                startDate = iep.getStartDate();
                int ageOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(startDate);
                addParameter("studentAge", String.valueOf(ageOfStudent));
                addParameter("studentAgeI", Integer.valueOf(ageOfStudent));
            }
            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
            String gradeLevel = null;
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);

            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (StudentEnrollment e : enrollments) {
                if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                    // student's YOG at this particular time
                    int yog = e.getYog();

                    // get the school year from basedDate
                    X2Criteria schoolYearCriteria = new X2Criteria();
                    schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                    schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                    QueryByCriteria schoolYearQuery =
                            new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                    DistrictSchoolYearContext ctx =
                            (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                    String currentContextOid = getCurrentContext().getContextId();
                    if (!StringUtils.isEmpty(currentContextOid)
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else {
                        int schoolYear = ctx.getSchoolYear();
                        List<String> grades =
                                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                        gradeLevel = grades.get(0);
                    }
                    break;
                }
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = iep.getStudent().getGradeLevel();
            }
            addParameter("studentGradeLevel", gradeLevel);


            List<StudentContact> contacts = getStudentContacts(iep, 2);

            if (!contacts.isEmpty()) {
                addParameter(PARAM_CONTACT_0, contacts.get(0));
            }

            if (contacts.size() >= 2) {
                addParameter(PARAM_CONTACT_1, contacts.get(1));
            }

        } else {
            iep = new IepData(getBroker().getPersistenceKey());
            if (getFormOwner() == null) {
                // When printing from District > Tools, we do not want the district name to print,
                // but we need it on all other blank forms.
                // This overrides the organization that would normally be pulled from
                // ToolJavaSource.prepareParameters()
                addParameter("organization", null);
            }
        }
        initIEPPlacementParams(iep);
        return m_attribHelper.getMaSpedDataSource(iep, getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Initializes report placement parameters
     *
     * @param iepData
     */
    private void initIEPPlacementParams(IepData iepData) {

        List<IepPlacement> iepPlacementList = new ArrayList<IepPlacement>();

        iepPlacementList.addAll(iepData.getPlacements());

        // Sorts IEPPlacement parameters by start date
        Collections.sort(iepPlacementList, new Comparator<IepPlacement>() {

            @Override
            public int compare(IepPlacement o1, IepPlacement o2) {
                PlainDate date1 = o1.getStartDate();
                PlainDate date2 = o2.getStartDate();
                if (date1 == null) {
                    return -1;
                }
                if (date2 == null) {
                    return 1;
                }

                return date2.compareTo(date1);
            }

        });

        // Initializes placement parameters from sorted list.
        addParameter(FIRST_PLACEMENT, iepPlacementList.size() > 0 ? iepPlacementList.get(0) : null);
        addParameter(SECOND_PLACEMENT, iepPlacementList.size() > 1 ? iepPlacementList.get(1) : null);
        addParameter(THIRD_PLACEMENT, iepPlacementList.size() > 2 ? iepPlacementList.get(2) : null);
    }

    // ** Publish to Documents ******************************

    protected static final String SAVE_TO_DOCUMENTS_IEP_PARAM = "saveToDocumentsIEP";

    private File m_saveToDocumentsResultFile = null;

    /**
     * Override publish so we can capture the output and save results to Documents.
     * All reports are Jasper 5.5 or 6.19
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     */
    @Override
    protected void publishResults() throws Exception {

        boolean saveToDocuments = getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM) != null &&
                ((Boolean) getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM)).booleanValue();

        ResultHandler resultHandler = getJob().getResultHandler();
        resultHandler.open(getJob(), null);
        OutputStream pdfOutputStream = resultHandler.getOutputStream();
        if (saveToDocuments) {
            pdfOutputStream = prepareSaveToDocuments(pdfOutputStream);
        }

        Object data = getDataSource();

        /*
         * Check to see if gatherData returned a structure that contains no data.
         * If it does, discard it so that a JREmptyDataSource is created and used below
         */
        if (data != null && data instanceof DataGrid) {
            DataGrid dataSource = (DataGrid) data;
            if (dataSource.getRows().isEmpty()) {
                data = null;
                getResultHandler().setEmpty(true);
            }
        }

        Report report = (Report) getJob().getTool();
        if (data == null) {
        	if (Report.REPORT_ENGINE_5_RELEASE.equals(report.getEngineVersion())) { 
        		data = new net.sf.jasperreports5.engine.JREmptyDataSource(0);
        	} else if (Report.REPORT_ENGINE_6_RELEASE.equals(report.getEngineVersion())) {
        		data = new net.sf.jasperreports6.engine.JREmptyDataSource(0);
        	}
        }

        logDataPrepared();

        ThreadUtils.checkInterrupt();

        /*
         * If the job has been aborted then don't bother filling the format or exporting the
         * results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (PublishReportsManager.isPublishing(getJob(), data, this)) {
                PublishReportsManager publishManager =
                        createReportsManager(getJob(),
                                getFormat(),
                                getParameters(),
                                getSchool(),
                                getOrganization(),
                                getBroker());
                publishManager.publishReport();
            } else if (Report.REPORT_ENGINE_5_RELEASE.equals(report.getEngineVersion()) && 
            		data instanceof net.sf.jasperreports5.engine.JRDataSource) {
                try {
                    JarPluginManager jarPluginManager = new JarPluginManager();
                    Tool tool = getJob().getTool();

                    if (!StringUtils.isEmpty(tool.getJarPluginPath())) {
                        ClassLoader jarClassLoader =
                                jarPluginManager.getParentClassLoader(tool, getBroker(),
                                        tool.getClass().getClassLoader(), null);
                        net.sf.jasperreports5.engine.util.JRClassLoader.setCustomClassLoader(jarClassLoader);
                    }

                    try {
                    	net.sf.jasperreports5.engine.JasperPrint reportPrint = net.sf.jasperreports5.engine.JasperFillManager.fillReport(getFormat(), getParameters(),
                                (net.sf.jasperreports5.engine.JRDataSource) data);

                        if (reportPrint != null && reportPrint.getPages().size() > 0) {
                            if (data instanceof ReportDataGrid) {
                                ((ReportDataGrid) data).applyJasperPrintParameters(reportPrint);
                            }

                            exportResults5(reportPrint, getJob(), pdfOutputStream);
                        } else {
                            getJob().getResultHandler().setEmpty(true);
                        }
                    } finally {
                    	net.sf.jasperreports5.engine.util.JRClassLoader.clearCustomClassLoader();
                    }
                } catch (net.sf.jasperreports5.engine.JRException jre) {
                    throw new X2BaseException(jre);
                }
            } else if (Report.REPORT_ENGINE_6_RELEASE.equals(report.getEngineVersion()) && 
            		data instanceof net.sf.jasperreports6.engine.JRDataSource) {
                try {
                    JarPluginManager jarPluginManager = new JarPluginManager();
                    Tool tool = getJob().getTool();

                    if (!StringUtils.isEmpty(tool.getJarPluginPath())) {
                        ClassLoader jarClassLoader =
                                jarPluginManager.getParentClassLoader(tool, getBroker(),
                                        tool.getClass().getClassLoader(), null);
                        net.sf.jasperreports6.engine.util.JRClassLoader.setCustomClassLoader(jarClassLoader);
                    }

                    try {
                        net.sf.jasperreports6.engine.JasperPrint reportPrint = net.sf.jasperreports6.engine.JasperFillManager.fillReport(getFormat(), getParameters(),
                                (net.sf.jasperreports6.engine.JRDataSource) data);

                        if (reportPrint != null && reportPrint.getPages().size() > 0) {
                            if (data instanceof ReportDataGrid) {
                                ((ReportDataGrid) data).applyJasperPrintParameters(reportPrint);
                            }

                            exportResults6(reportPrint, getJob(), pdfOutputStream);
                        } else {
                            getJob().getResultHandler().setEmpty(true);
                        }
                    } finally {
                    	net.sf.jasperreports6.engine.util.JRClassLoader.clearCustomClassLoader();
                    }
                } catch (net.sf.jasperreports6.engine.JRException jre) {
                    throw new X2BaseException(jre);
                }
            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                        data.getClass().toString(), "5.5 or 6.19"
                });
            }
        }
        if (saveToDocuments) {
            saveToDocuments(pdfOutputStream);
        }
    }

    /**
     * @param outputStream
     * @return
     * @throws FileNotFoundException
     */
    private OutputStream prepareSaveToDocuments(OutputStream outputStream) throws FileNotFoundException {
        m_saveToDocumentsResultFile = new File(
                getJob().getTempFolder() + File.separator + System.currentTimeMillis()
                        + Integer.toString(getJob().getJobId()) + "report.pdf");

        @SuppressWarnings("resource")
        FileOutputStream foStream = new FileOutputStream(m_saveToDocumentsResultFile);
        OutputStream oss[] = new OutputStream[2];
        oss[0] = outputStream;
        oss[1] = foStream;
        DuplicateOutputStream dos = new DuplicateOutputStream(oss);
        return dos;
    }

    /**
     * @param pdfOutputStream
     */
    private void saveToDocuments(OutputStream pdfOutputStream) {
        // Create a Student Document record with the PDF set as the file source.
        if (m_saveToDocumentsResultFile.isFile()) {
            Student student = null;
            Person person = null;
            School school = null;
            X2BaseBean owner = getFormStorage();
            if (owner instanceof IepData) {
                student = ((IepData) owner).getStudent();
                person = student.getPerson();
                school = student.getSchool();
            } else {
                owner = getFormOwner();
                if (owner instanceof IepData) {
                    student = ((IepData) owner).getStudent();
                    person = student.getPerson();
                    school = student.getSchool();
                }
            }
            if (student != null && person != null) {
                // Prepare some global variables so the method createDocument() will work.
                // Find or create the document record.
                com.follett.fsc.core.k12.beans.Document document =
                        createDocumentIEP(true, person, student, school, m_saveToDocumentsResultFile);

                getBroker().saveBeanForced(document);
            }
            m_saveToDocumentsResultFile.delete();
        }
    }

    /**
     * Exports the Jasper 5.5 results to an appropriately named file based on the output format.
     *
     * @param reportPrint JasperPrint
     * @param job ToolJob
     * @param resultHandler ResultHandler
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportResults5(net.sf.jasperreports5.engine.JasperPrint reportPrint, ToolJob job, OutputStream outputStream)
            throws net.sf.jasperreports5.engine.JRException, IOException {
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
            	net.sf.jasperreports5.engine.export.JRCsvExporter csvExporter = 
            		new net.sf.jasperreports5.engine.export.JRCsvExporter();
                csvExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                csvExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                csvExporter.exportReport();
                break;

            case ToolInput.HTML_FORMAT:
            	net.sf.jasperreports5.engine.export.JRHtmlExporter exporter = 
            	    new net.sf.jasperreports5.engine.export.JRHtmlExporter();
                exporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                exporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                exporter.setParameter(net.sf.jasperreports5.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN, Boolean.FALSE);
                exporter.exportReport();
                break;

            case ToolInput.PDF_FORMAT:
            	net.sf.jasperreports5.engine.JasperExportManager.exportReportToPdfStream(reportPrint, outputStream);
                break;

            case ToolInput.XLS_FORMAT:
                // report name must be less than 31 characters
                reportPrint.setName(StringUtils.truncate(reportPrint.getName(), ReportConstants.XLS_NAME_MAX_CHARS));
                net.sf.jasperreports5.engine.export.JRXlsExporter xlsExporter = new net.sf.jasperreports5.engine.export.JRXlsExporter();
                xlsExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                xlsExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                xlsExporter.exportReport();
                break;
        }
    }

    /**
     * Exports the Jasper 6.19.1 results to an appropriately named file based on the output format.
     *
     * @param reportPrint JasperPrint
     * @param job ToolJob
     * @param resultHandler ResultHandler
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportResults6(net.sf.jasperreports6.engine.JasperPrint reportPrint, ToolJob job, OutputStream outputStream)
            throws net.sf.jasperreports6.engine.JRException, IOException {
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
            	net.sf.jasperreports6.engine.export.JRCsvExporter csvExporter = 
            		new net.sf.jasperreports6.engine.export.JRCsvExporter();
                csvExporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                csvExporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                csvExporter.exportReport();
                break;

            case ToolInput.HTML_FORMAT:
            	net.sf.jasperreports6.engine.export.HtmlExporter exporter = new net.sf.jasperreports6.engine.export.HtmlExporter();
                exporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                exporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                // exporter.setParameter(net.sf.jasperreports6.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN, Boolean.FALSE);
                exporter.exportReport();
                break;

            case ToolInput.PDF_FORMAT:
            	net.sf.jasperreports6.engine.JasperExportManager.exportReportToPdfStream(reportPrint, outputStream);
                break;

            case ToolInput.XLS_FORMAT:
                // report name must be less than 31 characters
                reportPrint.setName(StringUtils.truncate(reportPrint.getName(), ReportConstants.XLS_NAME_MAX_CHARS));
                net.sf.jasperreports6.engine.export.JRXlsExporter xlsExporter = new net.sf.jasperreports6.engine.export.JRXlsExporter();
                xlsExporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.JASPER_PRINT, reportPrint);
                xlsExporter.setParameter(net.sf.jasperreports6.engine.JRExporterParameter.OUTPUT_STREAM, outputStream);
                xlsExporter.exportReport();
                break;
        }
    }

    /**
     * Creates the document.
     *
     * @param overwriteExisting boolean
     * @param person Person
     * @param bean X2BaseBean
     * @param school School
     * @param resultFile File
     * @return Document
     * @see com.follett.fsc.core.k12.tools.reports.SaveToDocuments#createDocument(boolean,
     *      com.follett.fsc.core.k12.beans.Person, com.follett.fsc.core.k12.beans.Student,
     *      java.io.File)
     */
    private com.follett.fsc.core.k12.beans.Document createDocumentIEP(boolean overwriteExisting,
                                                                      Person person,
                                                                      Student student,
                                                                      School school,
                                                                      File resultFile) {
        com.follett.fsc.core.k12.beans.Document document = null;

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField documentTypeField = dictionary.findDataDictionaryField(
                com.follett.fsc.core.k12.beans.Document.class.getName(),
                com.follett.fsc.core.k12.beans.Document.COL_TYPE_CODE);

        String reportName = (getParameter(DOCUMENT_NAME_PARAM) != null
                ? (String) getParameter(DOCUMENT_NAME_PARAM)
                : "IEPReport");
        String documentName = buildDocumentNameIEP(reportName, getCurrentContext(), dictionary);
        String documentType =
                getParameter(DOCUMENT_TYPE_PARAM) != null ? (String) getParameter(DOCUMENT_TYPE_PARAM) : "IEP";

        String fileName = org.apache.commons.lang3.StringUtils.stripAccents(student.getNameView()).replace("'", "")
                .replace(",", "").replace(" ", "")
                + "_" + student.getOid() + "_" + documentName + "_" + new PlainDate()
                + FolderUtils.FILE_EXTENSION_SEPARATOR + "pdf";

        if (documentType != null && documentType.length() > documentTypeField.getDatabaseLength()) {
            documentType = documentType.substring(0, documentTypeField.getDatabaseLength());
        }

        if (overwriteExisting) {
            Collection<com.follett.fsc.core.k12.beans.Document> documents =
                    getExistingDocumentsIEP(person.getOid(), documentName);
            document = getMostRecentDocumentIEP(documents);
        }

        if (document == null) {
            document = X2BaseBean.newInstance(com.follett.fsc.core.k12.beans.Document.class, dictionary);
            document.setPersonOid(person.getOid());
            document.setName(documentName);
            document.setFormatCode("pdf");
            document.setFilename(fileName);
        }

        document.setBinaryFile(resultFile);
        document.setTypeCode(documentType);
        document.setName(documentName);
        document.setFilename(fileName);
        document.setFieldValueByAlias(ALIAS_UPLOAD_DATE, getPlainDate().toString());
        document.setFieldValueByAlias(ALIAS_SCHOOL_ID, school.getSchoolId());
        return document;
    }

    /**
     * Retrieve a list of existing Documents for the student and document name.
     *
     * @param documentName
     * @param columnKeys
     * @param mapSizes
     *
     * @return Map
     */
    private Collection<com.follett.fsc.core.k12.beans.Document> getExistingDocumentsIEP(String personOid,
                                                                                        String documentName) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_PERSON_OID, personOid);
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_NAME, documentName);
        QueryByCriteria query = new QueryByCriteria(com.follett.fsc.core.k12.beans.Document.class, criteria);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Gets the most recent document.
     *
     * @param document Document
     * @param documents Collection<Document>
     * @return Document
     */
    private com.follett.fsc.core.k12.beans.Document getMostRecentDocumentIEP(Collection<com.follett.fsc.core.k12.beans.Document> documents) {
        com.follett.fsc.core.k12.beans.Document mostRecentDocument = null;
        com.follett.fsc.core.k12.beans.Document document = null;
        if (documents != null) {
            for (com.follett.fsc.core.k12.beans.Document doc : documents) {
                if (document == null) {
                    mostRecentDocument = doc;
                } else {
                    if (doc.getLastModifiedTime() > document.getLastModifiedTime()) {
                        mostRecentDocument = doc;
                    }
                }
            }
        }

        return mostRecentDocument;
    }

    /**
     * Builds the document name.
     *
     * @param reportName String
     * @param context DistrictSchoolYearContext
     * @param dictionary DataDictionary
     * @return String
     */
    private String buildDocumentNameIEP(String reportName,
                                        DistrictSchoolYearContext context,
                                        DataDictionary dictionary) {
        String documentName;

        if (reportName == null) {
            return null;
        }

        DataDictionaryField documentNameField = null;
        if (dictionary != null) {
            documentNameField =
                    dictionary.findDataDictionaryField(
                            com.follett.fsc.core.k12.beans.Document.class.getName(),
                            com.follett.fsc.core.k12.beans.Document.COL_NAME);
        }

        documentName = reportName;

        if (context != null && context.getContextId() != null) {
            documentName = documentName + "_" + context.getContextId();
        }

        if (documentNameField != null) {
            if (documentName != null && documentName.length() > documentNameField.getDatabaseLength()) {
                documentName = documentName.substring(0, documentNameField.getDatabaseLength());
            }
        }

        return documentName;
    }

    /**
     * A duplicating output stream.
     * Anything written to the output stream will be written to
     * multiple underlying output streams.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    private static class DuplicateOutputStream extends FilterOutputStream {
        private OutputStream m_streams[];

        /**
         * Constructor.
         * Provides a list of output streams to be written to.
         *
         * @param streams
         */
        public DuplicateOutputStream(OutputStream streams[]) {
            super(streams[0]);
            m_streams = streams;
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(int)
         */
        @Override
        public void write(int i) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(i);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[])
         */
        @Override
        public void write(byte[] b) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[], int, int)
         */
        @Override
        public void write(byte[] b, int offset, int length) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b, offset, length);
            }
        }

        /**
         * Flush all underlying output streams
         *
         * @see java.io.FilterOutputStream#flush()
         */
        @Override
        public void flush() throws IOException {
            for (OutputStream os : m_streams) {
                os.flush();
            }
        }

        /**
         * Close all underlying output streams
         *
         * @see java.io.FilterOutputStream#close()
         */
        @Override
        public void close() throws IOException {
            for (OutputStream os : m_streams) {
                os.close();
            }
        }
    }
}
