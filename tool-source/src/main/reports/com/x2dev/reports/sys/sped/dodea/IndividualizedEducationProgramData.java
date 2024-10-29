/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.dodea;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceGoalAlignment;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 * <p>
 * This form for DoDEA is the "Individualized Education Program (IEP)" form.
 *
 * @author X2 Development Corporation
 */
public class IndividualizedEducationProgramData extends BaseFormReportJavaSource {
    /**
     * Name for the "accommodation display" report parameter. The value is a String.
     */
    public static final String ACCOMMODATION_DISPLAY_PARAM = "accommodationDisplay";

    /**
     * Name for the "general/special ed. accommodation display" report paramter. The value is a
     * String.
     */
    public static final String GEN_SPED_ACCOMMODATIONS_DISPLAY_PARAM = "genSpedAccommodationDisplay";

    /**
     * Name for the "general/special ed. accommodation display" report paramter. The value is a
     * String.
     */
    public static final String DISABILITIES_DISPLAY_PARAM = "disabilitiesDisplay";

    /**
     * Name for the "goals data source" report paramter. The value is a JRDataSource object.
     */
    public static final String GOALS_DATA_SOURCE_PARAM = "goalsData";

    /**
     * Name for the "goals format" report paramter. The value is a java.io.InputStream.
     */
    public static final String GOALS_FORMAT_PARAM = "goalsFormat";

    /**
     * Name for the "goal provider map" report parameter. The value is a Map of a String
     * representation of the distinct service providers for each goal keyed to the goal OID.
     */
    public static final String GOALS_PROVIDER_MAP = "goalsProviderMap";
    public static final String GOALS_BASELINE_MAP = "goalsBaselineMap";

    /**
     * Name for the "Category map" report parameter. The value is a Map of a String
     * representation of the Category for each Testing Accommodation keyed to the Accommodation OID.
     */
    public static final String ACC_CATEGORY_MAP = "categoryMap";

    /**
     * Name for the "Main Area map" report parameter. The value is a Map of a String
     * representation of the Main Area for each Testing Accommodation keyed to the Accommodation
     * OID.
     */
    public static final String ACC_MAINAREA_MAP = "mainAreaMap";

    /**
     * Name for the "reference description lookup" parameter. The value is a Map of a reference
     * code's
     * long description (field D001) keyed to the code. This map in in a Map keyed to the reference
     * table OID.
     */
    public static final String REFERENCE_LOOKUP_MAP_PARAM = "referenceLookupMap";

    /**
     * Name for the "consultation services data source" report parameter. The value is a
     * JRDataSource object.
     */
    public static final String SERVICES_CONSULT_DATA_SOURCE_PARAM = "consultServicesData";

    /**
     * Name for the "consultation services format" report parameter. The value is a
     * java.io.InputStream object.
     */
    public static final String SERVICES_CONSULT_FORMAT_PARAM = "consultServicesFormat";

    /**
     * Name for the "related services data source" report parameter. The value is a JRDataSource
     * object.
     */
    public static final String SERVICES_RELATED_DATA_SOURCE_PARAM = "relatedServicesData";

    /**
     * Name for the "related services format" report parameter. The value is a java.io.InputStream
     * object.
     */
    public static final String SERVICES_RELATED_FORMAT_PARAM = "relatedServicesFormat";

    /**
     * Name for the "SPED services data source" report parameter. The value is a JRDataSource
     * object.
     */
    public static final String SERVICES_SPED_DATA_SOURCE_PARAM = "spedServicesData";

    /**
     * Name for the "SPED services map" report parameter. The value is a Map from service OID
     * to associated Goal Focus text.
     */
    public static final String SERVICES_SPED_DATA_SOURCE_MAP_PARAM = "spedServicesDataMap";

    /**
     * Name for the "SPED services format" report parameter. The value is a java.io.InputStream
     * object.
     */
    public static final String SERVICES_SPED_FORMAT_PARAM = "spedServicesFormat";

    /**
     * Name for the "accomodations system data source " report parameter. The value is a
     * JRDataSource object.
     *
     */
    public static final String ACCOMMODATION_SYSTEM_DATA_SOURCE_PARAM = "accommodationsSystemData";

    /**
     * Name for the "accomodations general data source " report parameter. The value is a
     * JRDataSource object.
     *
     */
    public static final String ACCOMMODATION_GENERAL_DATA_SOURCE_PARAM = "accommodationsGeneralData";

    /**
     * Name for the "ACCOMMODATION_SYSTEM format" report parameter. The value is a
     * java.io.InputStream object.
     */
    public static final String ACCOMMODATION_SYSTEM_FORMAT_PARAM = "accommodationsSystemFormat";
    public static final String IEP_PLAAPF_MAP = "plaafpMap";
    /**
     * Name for the "ACCOMMODATION_GENERAL format" report parameter. The value is a
     * java.io.InputStream object.
     */
    public static final String ACCOMMODATION_GENERAL_FORMAT_PARAM = "accommodationsGeneralFormat";

    public static final String STANDARDS_LOOKUP_MAP_PARAM = "standardLookupMap";
    /**
     * Special Factors Parameters
     */
    public static final String SF_AT_PARAM = "assistiveTechnology";
    public static final String SF_BI_PARAM = "behaviorIntervention";
    public static final String SF_B_PARAM = "braille";
    public static final String SF_CN_PARAM = "communicationNeeds";
    public static final String SF_LEP_PARAM = "limitedEnglishProficiency";

    /*
     * Services and Accommodations sub-report IDs
     */
    private static final String SERVICES_CONSULT_SUBREPORT_ID = "FSS-DOD-SPED-009-SRVC";
    private static final String SERVICES_RELATED_SUBREPORT_ID = "FSS-DOD-SPED-009-SRVR";
    private static final String SERVICES_SPED_SUBREPORT_ID = "FSS-DOD-SPED-009-SRVS";
    private static final String ACCOMMODATION_SYSTEM_SUBREPORT_ID = "FSS-DOD-SPED-009-SA";
    private static final String ACCOMMODATION_GENERAL_SUBREPORT_ID = "FSS-DOD-SPED-009-GA";

    /*
     * Service and Accommodation type constants
     */
    private static final String SERVICE_MODE_CONSULT = "Consultation";
    private static final String SERVICE_MODE_RELATED = "Related Services";
    private static final String SERVICE_MODE_SPED = "SpecialEd Services";
    private static final String ACCOMMODATION_MODE_SYSTEM = "System";
    private static final String ACCOMMODATION_MODE_GENERAL = "General";
    private static final String DISABILITY_REFERENCE_OID = "rtbSpedDisab  ";

    /*
     * Services sub-report IDs
     */
    private static final String GOALS_SUBREPORT_ID = "FSS-DOD-SPED-009-GOAL";

    /*
     * Constants for Team Subreport
     */
    private static final String TEAM_SUBREPORT_ID = "FSS-DOD-SPED-008-TEAM";
    private static final String PARAM_TEAM_SUBREPORT_FORMAT = "teamFormat";
    private static final String COL_TEAM_MEMBER = "teamMember";
    private static final String TEAM_MEMBER_LIST_PARAM = "teamMembersGrid";

    /*
     * Accommodation constants
     */
    private static final String ACCOMMODATIONS_ALIAS = "iep-accomodations";
    private static final String ACCOMMODATIONS_REFERENCE_TABLE_OID = "RTB0000004o1Fd";
    private static final String GEN_SPED_ACCOMMODATIONS_ALIAS = "iep-accomodations-gen-sped";
    private static final String GEN_SPED_ACCOMMODATIONS_REFERENCE_OID = "rtbAmendType";
    private static final String STANDARDS_REFERENCE_TABLE_OID = "RTB000001HbuAW";


    private IepData m_iep;
    private Report m_teamMemberSubreport;

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        if (m_iep == null) {
            m_iep = (IepData) getFormOwner();
        }

        loadTeamMembers();
        loadAccommodationDisplay();
        loadGenSpedAccommodationDisplay();
        loadGoals();
        loadServices();
        loadReferenceLookup();
        loadStandardsLookup();
        loadAccommodations();
        loadAccommodationMaps();
        loadDisabilities(m_iep);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_iep != null) {
            setFormOwner(m_iep);
            setFormStorage(m_iep);
            setDictionary(DataDictionary.getDistrictDictionary(m_iep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));

            addFormParameters();
        }
    }

    /**
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_iep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Loads the team members that attended the meeting into the proper report parameters.
     */
    private void loadTeamMembers() {

        X2Criteria teamCriteria = new X2Criteria();
        teamCriteria.addEqualTo(Report.COL_ID, TEAM_SUBREPORT_ID);

        QueryByCriteria teamQuery = new QueryByCriteria(Report.class, teamCriteria);
        m_teamMemberSubreport = getBroker().getBeanByQuery(teamQuery);
        addParameter(PARAM_TEAM_SUBREPORT_FORMAT, m_teamMemberSubreport);

        ReportDataGrid teamMemberGrid = new ReportDataGrid();

        IepData iep = (IepData) getFormStorage();

        Collection<IepTeamMember> teamMembers = iep.getTeamMembers();

        for (IepTeamMember teamMember : teamMembers) {

            /*
             * Add team members to the team member grid.
             */
            teamMemberGrid.append();
            teamMemberGrid.set(COL_TEAM_MEMBER, teamMember);
        }

        teamMemberGrid.beforeTop();
        addParameter(TEAM_MEMBER_LIST_PARAM, teamMemberGrid);
    }

    /**
     * Loads the accommodations (alias "iep-accomodations") into a single display that uses the
     * reference code descriptions.
     */
    private void loadAccommodationDisplay() {
        StringBuilder display = new StringBuilder(500);

        /*
         * Retrieve the reference lookup
         */
        ReferenceDescriptionLookup lookup =
                (ReferenceDescriptionLookup) getParameter(ToolJavaSource.REFERENCE_LOOKUP_KEY);

        /*
         * Iterate over the selected values building a string of the descriptions
         */
        String fieldValue = (String) m_iep.getFieldValueByAlias(ACCOMMODATIONS_ALIAS, getDictionary());
        Collection<String> accommodations = StringUtils.convertDelimitedStringToList(fieldValue, ',', true);

        for (String accommodation : accommodations) {
            display.append(lookup.getDescription(ACCOMMODATIONS_REFERENCE_TABLE_OID, accommodation));
            display.append(", ");
        }

        /*
         * Remove last delimiter if necessary
         */
        if (display.length() > 0) {
            display.replace(display.length() - 2, display.length(), "");
        }

        addParameter(ACCOMMODATION_DISPLAY_PARAM, display.toString());
    }

    /**
     * Loads the accommodations (alias "iep-accomodations") Main Area and Categories into maps that
     * can be used for the report.
     */
    private void loadAccommodationMaps() throws Exception {
        /*
         * Build the criteria
         */
        Map<String, String> categoryMap = new HashMap<String, String>(128);
        Map<String, String> mainAreaMap = new HashMap<String, String>(128);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ACCOMMODATIONS_REFERENCE_TABLE_OID);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        QueryIterator codes = getBroker().getIteratorByQuery(query);

        /*
         * Iterate over the selected values building a map of the categories and map of main area
         */
        try {
            while (codes.hasNext()) {

                ReferenceCode code = (ReferenceCode) codes.next();
                mainAreaMap.put(code.getCode(), code.getCategory());
                categoryMap.put(code.getCode(), code.getStateCode());

            }

        } finally {
            codes.close();
        }

        addParameter(ACC_CATEGORY_MAP, categoryMap);
        addParameter(ACC_MAINAREA_MAP, mainAreaMap);
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.YEAR, 2018);
        cal.set(Calendar.MONTH, Calendar.JANUARY);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        Date dateRepresentation = cal.getTime();
        addParameter("janOne", dateRepresentation);
        String start = new PlainDate().toString();
        if (m_iep.getStartDate() != null) {
            start = m_iep.getStartDate().toString();
        }
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH);
        Date result = df.parse(start);
        addParameter("IepStartDate", result);
    }

    /**
     * Loads the general/special ed. accommodations (alias "iep-accomodations-gen-sped") into a
     * single display that uses the reference code descriptions.
     */
    private void loadGenSpedAccommodationDisplay() {
        StringBuilder display = new StringBuilder(500);

        /*
         * Retrieve the reference lookup
         */
        ReferenceDescriptionLookup lookup =
                (ReferenceDescriptionLookup) getParameter(ToolJavaSource.REFERENCE_LOOKUP_KEY);

        /*
         * Iterate over the selected values building a string of the descriptions
         */
        String fieldValue = (String) m_iep.getFieldValueByAlias(GEN_SPED_ACCOMMODATIONS_ALIAS, getDictionary());
        Collection<String> accommodations = StringUtils.convertDelimitedStringToList(fieldValue, ',', true);

        for (String accommodation : accommodations) {
            display.append(lookup.getDescription(GEN_SPED_ACCOMMODATIONS_REFERENCE_OID, accommodation));
            display.append(", ");
        }

        /*
         * Remove last delimiter if necessary
         */
        if (display.length() > 0) {
            display.replace(display.length() - 2, display.length(), "");
        }

        addParameter(GEN_SPED_ACCOMMODATIONS_DISPLAY_PARAM, display.toString());
    }

    /**
     * Loads the goals used on the report format.
     */
    private void loadGoals() {
        String assistiveTechnology = "No";
        String behaviorIntervention = "No";
        String braille = "No";
        String communicationNeeds = "No";
        String limitedEnglishProficiency = "No";
        Collection<IepGoalObjective> objectives = new LinkedList<IepGoalObjective>();
        Map<String, String> providerMap = new HashMap<String, String>(128);
        Map<String, String> baselineMap = new HashMap<String, String>(128);
        Map<String, String> focusBaselineMap = new HashMap<String, String>(128);
        Map<String, String> plaafpMap = new HashMap<String, String>(128);

        for (IepPerformanceLevel level : m_iep.getIepPerformanceLevel()) {
            if (level.getFieldC001() != null) {
                switch (level.getFieldC001()) {
                    case "Adaptive-Alternative-Assistive Techniques":
                        plaafpMap.put("Aug./Adapt./Assit. Tech.", level.getFieldD002());
                        break;
                    case "Cognitive Skills":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Communication Skills":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Fine Motor Skills":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Functional Life Skills":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Language Arts":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Language Development":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Learning Strategies -Study Skills":
                        plaafpMap.put("Learning Stgy/Stdy Skills", level.getFieldD002());
                        break;
                    case "Mathematics":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Motor Skills":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Orientation - Mobility":
                        plaafpMap.put("Orientation/Mobility", level.getFieldD002());
                        break;
                    case "Pre-academics":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        plaafpMap.put("PreK Cognitive", level.getFieldD002());
                        plaafpMap.put("PreK Language", level.getFieldD002());
                        plaafpMap.put("PreK Literacy", level.getFieldD002());
                        plaafpMap.put("PreK Math", level.getFieldD002());
                        plaafpMap.put("PreK Physical", level.getFieldD002());
                        plaafpMap.put("PreK Social/Emotional", level.getFieldD002());
                        break;
                    case "Prevocational - Vocational Skills":
                        plaafpMap.put("Prevoc./Voc. Skills", level.getFieldD002());
                        break;
                    case "Reading":
                        plaafpMap.put(level.getFieldC001(), level.getFieldD002());
                        break;
                    case "Social - Interpersonal Skills":
                        plaafpMap.put("Social/Interpers. Skills", level.getFieldD002());
                        break;
                    case "Transition Services - Support":
                        plaafpMap.put("Transition Svcs/Support", level.getFieldD002());
                        break;
                }
            }

        }


        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, m_iep.getOid());

        QueryByCriteria query = new QueryByCriteria(IepGoal.class, criteria);
        query.addOrderByAscending(IepGoal.COL_FOCUS);
        query.addOrderByAscending(IepGoal.COL_ID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepGoal goal = (IepGoal) iterator.next();
                if (goal.getIepGoalObjectives(getBroker()).isEmpty()) {
                    IepGoalObjective noObjectives =
                            X2BaseBean.newInstance(IepGoalObjective.class, getBroker().getPersistenceKey());
                    noObjectives.setIepGoalOid(goal.getOid());
                    objectives.add(noObjectives);
                } else {
                    objectives.addAll(goal.getIepGoalObjectives(getBroker()));
                }
                if (goal.getFieldD001() != null) {
                    if (goal.getFieldD001().contains("Assistive")) {
                        assistiveTechnology = "Yes";
                    }
                    if (goal.getFieldD001().contains("Behavior")) {
                        behaviorIntervention = "Yes";
                    }
                    if (goal.getFieldD001().contains("Braille")) {
                        braille = "Yes";
                    }
                    if (goal.getFieldD001().contains("Communication")) {
                        communicationNeeds = "Yes";
                    }
                    if (goal.getFieldD001().contains("Limited")) {
                        limitedEnglishProficiency = "Yes";
                    }
                }
                if (focusBaselineMap.get(goal.getFocus()) == null && (!StringUtils.isEmpty(goal.getBaseline()))) {
                    focusBaselineMap.put(goal.getFocus(), goal.getBaseline());
                }

                if (!(StringUtils.isEmpty(goal.getBaseline()))) {
                    baselineMap.put(goal.getOid(), goal.getBaseline());
                } else {
                    if (focusBaselineMap.get(goal.getFocus()) != null) {
                        baselineMap.put(goal.getOid(), focusBaselineMap.get(goal.getFocus()));
                    } else {
                        if (!(StringUtils.isEmpty(goal.getFieldD003()))) {
                            baselineMap.put(goal.getOid(), goal.getFieldD003());
                        } else {
                            baselineMap.put(goal.getOid(), "No PLAAFP entered");
                        }
                    }
                }

                /*
                 * Build string of distinct service providers and put into Map
                 */
                TreeSet<String> providers = new TreeSet<String>();

                for (IepServiceGoalAlignment alignment : goal.getIepServiceGoalAlignments(getBroker())) {
                    if (!StringUtils.isEmpty(alignment.getIepService().getProviderCode())) {
                        providers.add(alignment.getIepService().getProviderCode());
                    }
                }

                providerMap.put(goal.getOid(), StringUtils.convertCollectionToDelimitedString(providers, ", "));
            }
        } finally {
            iterator.close();
        }

        addParameter(GOALS_PROVIDER_MAP, providerMap);
        addParameter(GOALS_BASELINE_MAP, baselineMap);
        addParameter(IEP_PLAAPF_MAP, plaafpMap);
        addParameter(GOALS_DATA_SOURCE_PARAM, new BeanCollectionDataSource(objectives,
                getDictionary(),
                getLocale()));

        Report report = ReportUtils.getReport(GOALS_SUBREPORT_ID, getBroker());
        addParameter(GOALS_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));
        addParameter(SF_AT_PARAM, assistiveTechnology);
        addParameter(SF_BI_PARAM, behaviorIntervention);
        addParameter(SF_B_PARAM, braille);
        addParameter(SF_CN_PARAM, communicationNeeds);
        addParameter(SF_LEP_PARAM, limitedEnglishProficiency);
    }

    /**
     * Loads a general reference lookup of codes to their "long description" (field D001). This is
     * stored in a Map keyed to the reference table OID.
     */
    private void loadReferenceLookup() {
        Map<String, Map<String, String>> referenceMap = new HashMap<String, Map<String, String>>();

        X2Criteria spedRefCriteria = new X2Criteria();
        spedRefCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + SisReferenceTable.COL_CATEGORY,
                "Special Ed.");
        // spedRefCriteria.addNotEmpty(ReferenceCode.COL_FIELD_D001,
        // getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, spedRefCriteria);
        query.addOrderByAscending(ReferenceCode.COL_REFERENCE_TABLE_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisReferenceTable lastTable = null;
            Map<String, String> codeMap = new HashMap<String, String>();

            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                SisReferenceTable table = (SisReferenceTable) (code.getReferenceTable());
                String longDescription = code.getFieldD001();

                if (!StringUtils.isEmpty(longDescription)) {
                    if (!ObjectUtils.match(table, lastTable)) {
                        codeMap = new HashMap<String, String>();
                        referenceMap.put(table.getOid().trim(), codeMap);
                    }

                    codeMap.put(code.getCode().trim(), longDescription);

                    lastTable = table;
                }
            }
        } finally {
            iterator.close();
        }

        addParameter(REFERENCE_LOOKUP_MAP_PARAM, referenceMap);
    }


    /**
     * Loads a general reference lookup of codes to their "long description" (field D001). This is
     * stored in a Map keyed to the reference table OID.
     */
    private void loadStandardsLookup() {
        Map<String, String> standardMap = new HashMap<String, String>();

        X2Criteria standardCriteria = new X2Criteria();
        standardCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + X2BaseBean.COL_OID,
                STANDARDS_REFERENCE_TABLE_OID);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, standardCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {

            while (iterator.hasNext()) {

                ReferenceCode code = (ReferenceCode) iterator.next();
                String longDescription = code.getFieldD001();
                // AppGlobals.getLog().severe("Code " + code.getCode().trim() + " Description " +
                // longDescription);
                standardMap.put(code.getCode().trim(), longDescription);
            }
        } finally {
            iterator.close();
        }

        addParameter(STANDARDS_LOOKUP_MAP_PARAM, standardMap);
    }

    /**
     * Loads the service information used on the report format. This loads each type of service
     * (SPED,
     * Related, Consultation) separately.
     */
    private void loadServices() {
        Collection<IepService> consultServices = new LinkedList<IepService>();
        Collection<IepService> relatedServices = new LinkedList<IepService>();
        Collection<IepService> spedServices = new LinkedList<IepService>();

        for (IepService service : m_iep.getIepServices(getBroker())) {
            if (SERVICE_MODE_CONSULT.equals(service.getServiceMode())) {
                consultServices.add(service);
            } else if (SERVICE_MODE_RELATED.equals(service.getServiceMode())) {
                relatedServices.add(service);
            } else if (SERVICE_MODE_SPED.equals(service.getServiceMode())) {
                spedServices.add(service);
            }
        }

        // Look up goal focuses for goals associated to the service.
        Map<String, String> focusMap = new HashMap<String, String>();
        for (IepService service : spedServices) {
            Set<String> focusSet = new HashSet<String>();
            Collection<IepServiceGoalAlignment> serviceGoals = service.getIepServiceGoalAlignments(getBroker());
            for (IepServiceGoalAlignment serviceGoal : serviceGoals) {
                IepGoal goal = serviceGoal.getIepGoal();
                String focus = goal.getFocus();
                if (!StringUtils.isEmpty(focus)) {
                    focusSet.add(focus);
                }
            }
            focusMap.put(service.getOid(),
                    StringUtils.convertCollectionToDelimitedString(focusSet, "\n"));
        }

        addParameter(SERVICES_CONSULT_DATA_SOURCE_PARAM, new BeanCollectionDataSource(consultServices,
                getDictionary(),
                getLocale()));
        addParameter(SERVICES_RELATED_DATA_SOURCE_PARAM, new BeanCollectionDataSource(relatedServices,
                getDictionary(),
                getLocale()));
        addParameter(SERVICES_SPED_DATA_SOURCE_PARAM, new BeanCollectionDataSource(spedServices,
                getDictionary(),
                getLocale()));
        addParameter(SERVICES_SPED_DATA_SOURCE_MAP_PARAM, focusMap);

        Report report = ReportUtils.getReport(SERVICES_CONSULT_SUBREPORT_ID, getBroker());
        addParameter(SERVICES_CONSULT_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));

        report = ReportUtils.getReport(SERVICES_RELATED_SUBREPORT_ID, getBroker());
        addParameter(SERVICES_RELATED_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));

        report = ReportUtils.getReport(SERVICES_SPED_SUBREPORT_ID, getBroker());
        addParameter(SERVICES_SPED_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));
    }

    /**
     * Loads the accomodation information used on the report format. This loads each type of
     * accommodation (System and General) separately.
     */
    private void loadAccommodations() {
        Collection<IepAccommodation> systemAccom = new LinkedList<IepAccommodation>();
        Collection<IepAccommodation> generalAccom = new LinkedList<IepAccommodation>();


        for (IepAccommodation accommodation : m_iep.getAccommodations(getBroker())) {
            if (ACCOMMODATION_MODE_SYSTEM.equals(accommodation.getType())) {
                systemAccom.add(accommodation);
            } else if (ACCOMMODATION_MODE_GENERAL.equals(accommodation.getType())) {
                generalAccom.add(accommodation);
            }

        }
        boolean systemAssess = true;
        boolean generalAssess = true;

        if (systemAccom.isEmpty()) {
            systemAssess = false;
        }

        if (generalAccom.isEmpty()) {
            generalAssess = false;
        }

        addParameter("noSystem", systemAssess);
        addParameter("noGeneral", generalAssess);
        addParameter(ACCOMMODATION_SYSTEM_DATA_SOURCE_PARAM, new BeanCollectionDataSource(systemAccom,
                getDictionary(),
                getLocale()));
        addParameter(ACCOMMODATION_GENERAL_DATA_SOURCE_PARAM, new BeanCollectionDataSource(generalAccom,
                getDictionary(),
                getLocale()));


        Report report = ReportUtils.getReport(ACCOMMODATION_SYSTEM_SUBREPORT_ID, getBroker());
        addParameter(ACCOMMODATION_SYSTEM_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));

        report = ReportUtils.getReport(ACCOMMODATION_GENERAL_SUBREPORT_ID, getBroker());
        addParameter(ACCOMMODATION_GENERAL_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));

    }


    /**
     * Loads the disabilities display parameter and uses the reference code descriptions.
     */
    private void loadDisabilities(IepData iep1) {
        StringBuilder display = new StringBuilder(500);

        /*
         * Retrieve the reference lookup
         */
        ReferenceDescriptionLookup lookup =
                (ReferenceDescriptionLookup) getParameter(ToolJavaSource.REFERENCE_LOOKUP_KEY);

        /*
         * Iterate over the selected values building a string of the descriptions
         */
        String query = new String(
                "SELECT  " +
                        "IDB_DISABILITY_CODE " +
                        "FROM dbo.STUDENT_DISABILITY as SD " +
                        "WHERE SD.IDB_IEP_OID = '" + iep1.getOid() + "'" +
                        "AND SD.IDB_PRIMARY_IND != 1");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("IDB_DISABILITY_CODE");
                    display.append(lookup.getDescription(DISABILITY_REFERENCE_OID, name));
                    display.append(", ");
                }

                resultSet.close();
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }



        /*
         * Remove last delimiter if necessary
         */
        if (display.length() > 0) {
            display.replace(display.length() - 2, display.length(), "");
        } else {
            display.append("N/A");
        }
        addParameter(DISABILITIES_DISPLAY_PARAM, display.toString());
    }


}
