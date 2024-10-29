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
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
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
 * This form for DoDEA is the "Case Study Committee Eligibility Report" form.
 *
 * @author X2 Development Corporation
 */
public class EligibilityReportData extends BaseFormReportJavaSource {
    /**
     * Name for the "disability string" report paramter. The value is a String representation of
     * the disabilities associated with the IEP.
     */
    public static final String DISABILITIES_PARAM = "disabilities";

    /**
     * Name for the "evaluations data source" report parameter. The value is a JRDataSource object.
     */
    public static final String EVALUATIONS_DATA_SOURCE_PARAM = "evaluationsData";

    /**
     * Names for the "disability string" report formats. The value is a String representation of
     * the three suspected disabilities associated with the IEP.
     */
    public static final String ONE_FORMAT_PARAM = "oneFormat";
    public static final String TWO_FORMAT_PARAM = "twoFormat";
    public static final String THREE_FORMAT_PARAM = "threeFormat";


    /**
     * Names for the "disability data sources" report parameter. The value is a JRDataSource object.
     */
    public static final String ONE_DATA_SOURCES_PARAM = "oneData";
    public static final String TWO_DATA_SOURCES_PARAM = "twoData";
    public static final String THREE_DATA_SOURCES_PARAM = "threeData";


    /**
     * Name for the "evaluations format" report parameter. The value is a java.io.InputStream
     * object.
     */
    public static final String EVALUATIONS_FORMAT_PARAM = "evaluationsFormat";

    /**
     * Name for the "performances data source" report parameter. The value is a Map of JRDataSource
     * objects keyed to the performance type (educational, physical, social, etc.).
     */
    public static final String PERFORMANCES_DATA_SOURCES_PARAM = "performancesData";

    /**
     * Name for the "performances format" report parameter. The value is a java.io.InputStream
     * object.
     */
    public static final String PERFORMANCES_FORMAT_PARAM = "performancesFormat";

    /**
     * Name for the "team member list" report parameter. The value is List of IEP Team Member beans.
     */
    public static final String TEAM_MEMBER_LIST_PARAM = "teamMembersGrid";

    /*
     * Additional parameters
     */
    private static final String ELIGIBILITY_MEETING_PARAM = "eligibilityMeeting";

    /*
     * Disability Constants should mathc the Disability Codes reference table codes
     */
    private static final String AUTISM = "Autism SD";
    private static final String DEAF_BLIND = "Deaf-Blindness";
    private static final String DEAF = "Deafness";
    private static final String DEVELOP = "Developmental Delay";
    private static final String EMOTIONAL = "Emotion Disturbance";
    private static final String HEARING = "Hearing Impairment";
    private static final String INTELLECTUAL = "Intellectual";
    private static final String MULTIPLE = "Multiple";
    private static final String ORTHO = "Ortho Impairment";
    private static final String OTHER = "Oth Hlth Impairment";
    private static final String SPEECH_ARTICULATION = "SL-Articulation";
    private static final String SPEECH_DYSFLUENCY = "SL-Dysfluency";
    private static final String SPEECH_LANGPHON = "SL-Language";
    private static final String SPEECH_VOICE = "SL-Voice";
    private static final String SPEECH_LANGUAGE = "Speech/Lang Impairments";
    private static final String SPECIFIC = "Spec Lrn Disability";
    private static final String TBI = "Traumatic Brain Inj";
    private static final String VISION = "Visual Impairment";



    /*
     * Assessment/Evaluations aliases
     */
    private static final String HEARING_DATE_REF_ALIAS = "csc-ref-hearing-date";
    private static final String HEARING_RESULT_REF_ALIAS = "csg-ref-hearing-passed";
    private static final String VISION_DATE_REF_ALIAS = "csc-ref-vision-date";
    private static final String VISION_RESULT_REF_ALIAS = "csg-ref-vision-passed";

    private static final String HEARING_DATE_IEP_ALIAS = "csc-mtgmins-hear-date";
    private static final String HEARING_RESULT_IEP_ALIAS = "csc-mtgmins-hear-result";
    private static final String VISION_DATE_IEP_ALIAS = "csc-mtgmins-vis-date";
    private static final String VISION_RESULT_IEP_ALIAS = "csc-mtgmins-vis-result";

    /*
     * Assessment subreport grid fields
     */
    private static final String FIELD_EVALUATION = "evaluation";
    private static final String FIELD_ASSESSMENT = "assessment";
    private static final String FIELD_ASSESSMENT_OTHER = "assessment-other";
    private static final String FIELD_EVALUATION_STAFF = "evaluation-staff";
    private static final String FIELD_EVALUATION_DATE = "evaluation-date";
    private static final String PARAM_HEARING_DATE = "hearing-date";
    private static final String PARAM_HEARING_RESULT = "hearing-result";
    private static final String PARAM_VISION_DATE = "vision-date";
    private static final String PARAM_VISION_RESULT = "vision-result";

    /*
     * Assessment/Evaluations sub-report ID
     */
    private static final String EVALUATIONS_SUBREPORT_ID = "FSS-DOD-SPED-008-EVALS";
    private static final String PERFORMANCES_SUBREPORT_ID = "FSS-DOD-SPED-008-PERFS";
    private static final String TEAM_SUBREPORT_ID = "FSS-DOD-SPED-008-TEAM";
    private static final String AUTISMSD_SUBREPORT_ID = "FSS-DOD-ELIG-AUTISMSD";
    private static final String DEAFNESS_SUBREPORT_ID = "FSS-DOD-ELIG-DEAF";
    private static final String DEAFBLINDNESS_SUBREPORT_ID = "FSS-DOD-ELIG-DEAFBLIND";
    private static final String DEVELOPMENTALDELAY_SUBREPORT_ID = "FSS-DOD-ELIG-DEVDELAY";
    private static final String EMOTIONALIMPAIRMENT_SUBREPORT_ID = "FSS-DOD-ELIG-EMOTIONAL";
    private static final String HEARINGIMPAIRMENT_SUBREPORT_ID = "FSS-DOD-ELIG-HEARING";
    private static final String INTELLECTUALDISABILITY_SUBREPORT_ID = "FSS-DOD-ELIG-INTELLECT";
    private static final String MULTIPLE_SUBREPORT_ID = "FSS-DOD-ELIG-MULTIPLE";
    private static final String ORTHOIMPAIRMENT_SUBREPORT_ID = "FSS-DOD-ELIG-ORTHO";
    private static final String OTHHLTHIMPAIRMENT_SUBREPORT_ID = "FSS-DOD-ELIG-OTHERHLTH";
    private static final String SPEECHLANGUAGE_SUBREPORT_ID = "FSS-DOD-ELIG-SPEECHLANG";
    private static final String SPECLRNDISABILITY_SUBREPORT_ID = "FSS-DOD-ELIG-SPECLEARN";
    private static final String TRAUMATICBRAININJ_SUBREPORT_ID = "FSS-DOD-ELIG-TBI";
    private static final String VISUALIMPAIRMENT_SUBREPORT_ID = "FSS-DOD-ELIG-VISUAL";

    /*
     * Performance level types
     */
    private static final String PERFORMANCE_TYPE_COGNITIVE = "Cognitive";
    private static final String PERFORMANCE_TYPE_COMMUNICATION = "Communication";
    private static final String PERFORMANCE_TYPE_EDUCATIONAL = "Educational";
    private static final String PERFORMANCE_TYPE_PHYSICAL = "Physical";
    private static final String PERFORMANCE_TYPE_SOCIAL = "Social";
    private static final String PERFORMANCE_TYPE_TRANSITION = "Transition";

    private static final String PARAM_TEAM_SUBREPORT_FORMAT = "teamFormat";
    private static final String COL_TEAM_MEMBER = "teamMember";

    private DateAsStringConverter m_dateConverter;
    private IepData m_iep;
    private ReferenceDescriptionLookup m_referenceLookup;
    private Report m_teamMemberSubreport;

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_iep = (IepData) getFormOwner();

        loadDisabilities();
        loadEvaluations();
        loadPerformances();
        loadTeamMembers();
        loadEligibilityMeeting();

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);
        m_referenceLookup = new ReferenceDescriptionLookup(getBroker(), getOrganization());
    }

    /**
     * Loads the most recent "Eligibility" meeting as a report parameter to use on the format.
     */
    private void loadEligibilityMeeting() {
        IepMeeting meeting = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, m_iep.getOid());
        criteria.addIn(IepMeeting.COL_FIELD_B002,
                Arrays.asList("Eligibility", "ACCEPT ELIGIBILITY/IEP", "Triennial Review"));

        QueryByCriteria query = new QueryByCriteria(IepMeeting.class, criteria);
        query.addOrderByDescending(IepMeeting.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            if (iterator.hasNext()) {
                meeting = (IepMeeting) iterator.next();
            }
        } finally {
            iterator.close();
        }

        addParameter(ELIGIBILITY_MEETING_PARAM, meeting);
    }

    /**
     * Returns the form instance for the passed owner and definition ID. Note this method does not
     * distinguish if multiple instances of a form are supported for 1 owner.
     *
     * @param owner
     * @param formId
     *
     *        return FormInstance
     */
    private FormInstance findForm(X2BaseBean owner, String formId) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, owner.getOid());
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER + FormDefinition.COL_ID, formId);

        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);

        return (FormInstance) getBroker().getBeanByQuery(query);
    }

    /**
     * Loads the disability information used on the report format. Build all disabilities into a
     * single string to display on the report.
     */
    private void loadDisabilities() {
        /*
         * Load the disability code reference table
         */
        DataDictionaryField field = getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                IepDisability.COL_DISABILITY_CODE);
        SisReferenceTable table = (SisReferenceTable) (field.getReferenceTable());

        /*
         * Build the display string
         */
        StringBuilder display = new StringBuilder(500);
        for (IepDisability disability : m_iep.getIepDisability(getBroker())) {
            display.append(m_referenceLookup.getDescription(table.getOid(), disability.getDisabilityCode()));
            display.append("\n");
        }

        addParameter(DISABILITIES_PARAM, display.toString());
    }

    /**
     * Loads the evaluation data for the report. This data is stored on a the iep accommodations.
     */
    private void loadEvaluations() {
        /*
         * Build report grid of evaluations
         */
        ReportDataGrid grid = new ReportDataGrid(10);

        grid.append();

        String visionDate = null;
        String hearingDate = null;
        String visionPassed = null;
        String hearingPassed = null;

        if (m_iep != null) {
            /*
             * Lookup the referral form, if one exists
             */
            FormInstance referralForm = findForm(m_iep, "Referral");
            if (referralForm != null) {
                GenericFormData formData = (GenericFormData) referralForm.getStorageObject(getBroker());

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(formData.getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

                visionPassed = (String) formData.getFieldValueByAlias(VISION_RESULT_REF_ALIAS, dictionary);
                visionDate = (String) formData.getFieldValueByAlias(VISION_DATE_REF_ALIAS, dictionary);

                hearingPassed = (String) formData.getFieldValueByAlias(HEARING_RESULT_REF_ALIAS, dictionary);
                hearingDate = (String) formData.getFieldValueByAlias(HEARING_DATE_REF_ALIAS, dictionary);
            } else {
                visionPassed = (String) m_iep.getFieldValueByAlias(VISION_RESULT_IEP_ALIAS, getDictionary());
                visionDate = (String) m_iep.getFieldValueByAlias(VISION_DATE_IEP_ALIAS, getDictionary());

                hearingPassed = (String) m_iep.getFieldValueByAlias(HEARING_RESULT_IEP_ALIAS, getDictionary());
                hearingDate = (String) m_iep.getFieldValueByAlias(HEARING_DATE_IEP_ALIAS, getDictionary());
            }
        }

        addParameter(PARAM_VISION_DATE, m_dateConverter.javaToString(visionDate));
        addParameter(PARAM_HEARING_DATE, m_dateConverter.javaToString(hearingDate));
        addParameter(PARAM_VISION_RESULT, visionPassed);
        addParameter(PARAM_HEARING_RESULT, hearingPassed);

        /*
         * Add the accommodation records
         */
        for (IepAccommodation accommodation : m_iep.getAccommodations(getBroker())) {
            grid.set(FIELD_EVALUATION, accommodation.getName());
            grid.set(FIELD_ASSESSMENT, accommodation.getCategory());
            grid.set(FIELD_ASSESSMENT_OTHER, accommodation.getFieldD002());
            grid.set(FIELD_EVALUATION_DATE, accommodation.getImplementationDate());
            if (accommodation.getStaff() != null) {
                grid.set(FIELD_EVALUATION_STAFF, accommodation.getStaff().getNameView());
            }


            grid.append();
        }

        /*
         * Add the performance records
         */
        for (IepPerformanceLevel performance : m_iep.getIepPerformanceLevel(getBroker())) {
            if ("Assess".equals(performance.getType())) {
                String dateAsString = (String) performance.getFieldValueByAlias("iep-evaluation-date", getDictionary());
                PlainDate date = (PlainDate) m_dateConverter.parseSystemString(dateAsString);


                grid.set(FIELD_EVALUATION, "Other");
                grid.set(FIELD_ASSESSMENT, performance.getPerformanceSummary());
                grid.set(FIELD_EVALUATION_DATE, date);

                grid.append();
            }
        }

        /*
         * Add parameters
         */
        grid.beforeTop();

        addParameter(EVALUATIONS_DATA_SOURCE_PARAM, grid);

        Report report = ReportUtils.getReport(EVALUATIONS_SUBREPORT_ID, getBroker());
        addParameter(EVALUATIONS_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));
    }

    /**
     * Loads the performances for the parent IEP and the question datasource and formats.
     */
    private void loadPerformances() {
        /*
         * Build collections to iterate over in sub-report
         */
        Collection<IepPerformanceLevel> cognitive = new LinkedList<IepPerformanceLevel>();
        Collection<IepPerformanceLevel> communication = new LinkedList<IepPerformanceLevel>();
        Collection<IepPerformanceLevel> educational = new LinkedList<IepPerformanceLevel>();
        Collection<IepPerformanceLevel> physical = new LinkedList<IepPerformanceLevel>();
        Collection<IepPerformanceLevel> social = new LinkedList<IepPerformanceLevel>();
        Collection<IepPerformanceLevel> transition = new LinkedList<IepPerformanceLevel>();

        Collection<IepData> ieps = new LinkedList<IepData>();
        ieps.add(m_iep);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepPerformanceLevel.COL_IEP_DATA_OID, m_iep.getOid());

        QueryByCriteria query = new QueryByCriteria(IepPerformanceLevel.class, criteria);
        query.addOrderByAscending(IepPerformanceLevel.COL_TYPE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepPerformanceLevel performance = (IepPerformanceLevel) iterator.next();

                if (PERFORMANCE_TYPE_COGNITIVE.equals(performance.getType())) {
                    cognitive.add(performance);
                } else if (PERFORMANCE_TYPE_COMMUNICATION.equals(performance.getType())) {
                    communication.add(performance);
                } else if (PERFORMANCE_TYPE_EDUCATIONAL.equals(performance.getType())) {
                    educational.add(performance);
                } else if (PERFORMANCE_TYPE_PHYSICAL.equals(performance.getType())) {
                    physical.add(performance);
                } else if (PERFORMANCE_TYPE_SOCIAL.equals(performance.getType())) {
                    social.add(performance);
                } else if (PERFORMANCE_TYPE_TRANSITION.equals(performance.getType())) {
                    transition.add(performance);
                }
            }
        } finally {
            iterator.close();
        }

        /*
         * Put all datasources into a Map keyed to the type
         */
        Map<String, BeanCollectionDataSource> dataSources = new HashMap<String, BeanCollectionDataSource>(16);
        dataSources.put(PERFORMANCE_TYPE_COGNITIVE,
                new BeanCollectionDataSource(cognitive, getDictionary(), getLocale()));
        dataSources.put(PERFORMANCE_TYPE_COMMUNICATION,
                new BeanCollectionDataSource(communication, getDictionary(), getLocale()));
        dataSources.put(PERFORMANCE_TYPE_EDUCATIONAL,
                new BeanCollectionDataSource(educational, getDictionary(), getLocale()));
        dataSources.put(PERFORMANCE_TYPE_PHYSICAL,
                new BeanCollectionDataSource(physical, getDictionary(), getLocale()));
        dataSources.put(PERFORMANCE_TYPE_SOCIAL, new BeanCollectionDataSource(social, getDictionary(), getLocale()));
        dataSources.put(PERFORMANCE_TYPE_TRANSITION,
                new BeanCollectionDataSource(transition, getDictionary(), getLocale()));

        // setting data sources for all the subreports
        dataSources.put(ONE_DATA_SOURCES_PARAM, new BeanCollectionDataSource(ieps, getDictionary(), getLocale()));
        dataSources.put(TWO_DATA_SOURCES_PARAM, new BeanCollectionDataSource(ieps, getDictionary(), getLocale()));
        dataSources.put(THREE_DATA_SOURCES_PARAM, new BeanCollectionDataSource(ieps, getDictionary(), getLocale()));


        /*
         * Add report parameters As the subreports are completed uncomment the two lines associated
         * with the subreports
         */
        addParameter(PERFORMANCES_DATA_SOURCES_PARAM, dataSources);

        Report report = ReportUtils.getReport(PERFORMANCES_SUBREPORT_ID, getBroker());
        addParameter(PERFORMANCES_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));


        // Added datasource and format for the three disability sub reports that match the Suspected
        // Disabilities
        // Added code to pull actual disabilities for EDIS and Triennial since they may not have
        // suspected disabilities
        // Code will NOT overwrite a suspected disability

        String oneSus = m_iep.getFieldC027();
        String twoSus = m_iep.getFieldC028();
        String threeSus = m_iep.getFieldC029();
        if ((oneSus != null && oneSus.contains("SL")) && (twoSus != null && twoSus.contains("SL"))) {
            twoSus = "";
        }

        if (((oneSus != null && oneSus.contains("SL")) || (twoSus != null && twoSus.contains("SL")))
                && (threeSus != null && threeSus.contains("SL"))) {
            threeSus = "";
        }

        int disCounter = 1;
        if ("Transfer from EDIS, Triennial Review".contains(m_iep.getFieldB002())) {
            for (IepDisability disability : m_iep.getIepDisability(getBroker())) {
                if (disCounter == 1 && oneSus == null) {
                    oneSus = disability.getDisabilityCode();
                } else {
                    if (twoSus == null)

                    {
                        twoSus = disability.getDisabilityCode();
                    } else {
                        threeSus = disability.getDisabilityCode();
                    }
                }

                if (disCounter == 2 && twoSus == null) {
                    twoSus = disability.getDisabilityCode();
                } else {
                    if (disCounter == 3 && threeSus == null)

                    {
                        threeSus = disability.getDisabilityCode();
                    }
                }
                if (disCounter == 3 && threeSus == null) {
                    threeSus = disability.getDisabilityCode();
                }
                disCounter++;
            }
        }

        Report oneReport = getReport(oneSus);
        if (twoSus != null && (oneSus != null && oneSus.equals(twoSus))) {
            twoSus = "";
        }
        if ((threeSus != null && (oneSus != null && oneSus.equals(threeSus)))
                || (twoSus != null && threeSus != null && twoSus.equals(threeSus))) {
            threeSus = "";
        }
        Report twoReport = getReport(twoSus);
        Report threeReport = getReport(threeSus);

        if (oneReport != null) {
            addParameter(ONE_FORMAT_PARAM, new ByteArrayInputStream(oneReport.getCompiledFormat()));
        } else {
            addParameter(ONE_FORMAT_PARAM, null);
        }

        if (twoReport != null && oneReport != twoReport) {
            addParameter(TWO_FORMAT_PARAM, new ByteArrayInputStream(twoReport.getCompiledFormat()));
        } else {
            addParameter(TWO_FORMAT_PARAM, null);
        }


        if (threeReport != null && threeReport != twoReport && oneReport != threeReport) {
            addParameter(THREE_FORMAT_PARAM, new ByteArrayInputStream(threeReport.getCompiledFormat()));
        } else {
            addParameter(THREE_FORMAT_PARAM, null);
        }

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
     * Loads the team members that attended the meeting into the proper report parameters.
     */
    private Report getReport(String disability) {
        Report report = null;
        if (disability != null) {
            switch (disability) {
                case AUTISM:
                    report = ReportUtils.getReport(AUTISMSD_SUBREPORT_ID, getBroker());
                    break;

                case VISION:
                    report = ReportUtils.getReport(VISUALIMPAIRMENT_SUBREPORT_ID, getBroker());
                    break;

                case DEAF:
                    report = ReportUtils.getReport(DEAFNESS_SUBREPORT_ID, getBroker());
                    break;

                case DEAF_BLIND:
                    report = ReportUtils.getReport(DEAFBLINDNESS_SUBREPORT_ID, getBroker());
                    break;

                case DEVELOP:
                    report = ReportUtils.getReport(DEVELOPMENTALDELAY_SUBREPORT_ID, getBroker());
                    break;

                case EMOTIONAL:
                    report = ReportUtils.getReport(EMOTIONALIMPAIRMENT_SUBREPORT_ID, getBroker());
                    break;

                case INTELLECTUAL:
                    report = ReportUtils.getReport(INTELLECTUALDISABILITY_SUBREPORT_ID, getBroker());
                    break;

                case OTHER:
                    report = ReportUtils.getReport(OTHHLTHIMPAIRMENT_SUBREPORT_ID, getBroker());
                    break;

                case ORTHO:
                    report = ReportUtils.getReport(ORTHOIMPAIRMENT_SUBREPORT_ID, getBroker());
                    break;

                case HEARING:
                    report = ReportUtils.getReport(HEARINGIMPAIRMENT_SUBREPORT_ID, getBroker());
                    break;

                case MULTIPLE:
                    report = ReportUtils.getReport(MULTIPLE_SUBREPORT_ID, getBroker());
                    break;

                case SPECIFIC:
                    report = ReportUtils.getReport(SPECLRNDISABILITY_SUBREPORT_ID, getBroker());
                    break;

                case SPEECH_ARTICULATION:
                    report = ReportUtils.getReport(SPEECHLANGUAGE_SUBREPORT_ID, getBroker());
                    break;

                case TBI:
                    report = ReportUtils.getReport(TRAUMATICBRAININJ_SUBREPORT_ID, getBroker());
                    break;

                case SPEECH_DYSFLUENCY:
                    report = ReportUtils.getReport(SPEECHLANGUAGE_SUBREPORT_ID, getBroker());
                    break;

                case SPEECH_LANGPHON:
                    report = ReportUtils.getReport(SPEECHLANGUAGE_SUBREPORT_ID, getBroker());
                    break;

                case SPEECH_LANGUAGE:
                    report = ReportUtils.getReport(SPEECHLANGUAGE_SUBREPORT_ID, getBroker());
                    break;

                case SPEECH_VOICE:
                    report = ReportUtils.getReport(SPEECHLANGUAGE_SUBREPORT_ID, getBroker());
                    break;

                default:
                    report = null;
                    break;


            }
        }
        return report;

    }
}