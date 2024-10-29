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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.BeanPathException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.SchoolYearDateRangeLookup;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * State report data module for the RI Special Education Student file. The base class for this
 * state report is IEP_DATA. IEPs to include are marked in the IEP_DATA field with the
 * "sped-in-last-state-report" alias. IEPs active since the day after the previous school year's
 * last session day and the report date, inclusively, are included. The following additional
 * criteria is applied:
 * <ul>
 * <li>Students must be in the organization selected in the orgOid input parameter
 * <li>Students marked for exclusion from state reports are excluded unless the
 * includeExcludedStudents input parameter is set
 * <li>Students without SASIDs are excluded if the sasidStudentsOnly input parameter is set
 * <li>The most recent IEP that is ACTIVE or PREVIOUS between the day after the previous school
 * year's last session day and the report date is included; only one IEP per student is included
 * </ul>
 * <p>
 * Setting the recalculateIepsToInclude to false bypasses marking the IEPs to include; the IEPs
 * included in the last run are used.
 * <p>
 * A number of custom field retrievers and validators are used; see javadoc on the corresponding
 * inner classes for details. If the bypassStateRules input parameter is set, custom validators are
 * bypassed.
 *
 * @author mmastrangelo
 */
public class RISpecialEducationStudent extends RIStateReportData {
    // Input Parameters ---------------------------------------------------------------------------
    public static final String ORGANIZATION_PARAM = "orgOid";
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String BYPASS_STATE_VALIDATION_RULES = "bypassStateRules";
    public static final String INCLUDE_STUDENTS_MARKED_FOR_EXCLUSION = "includeExcludedStudents";
    public static final String RECALCULATE_IEPS_TO_INCLUDE = "recalculateIepsToInclude";
    public static final String EXCLUDE_NON_RESIDENTS = "excludeNonResidents";
    public static final String SENIOR_GRAD_DATE = "seniorGraduationDate";
    public static final String SUMMER_EXIT_START_DATE = "summerExitStartDate";

    // Field Aliases ------------------------------------------------------------------------------
    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";
    private static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";
    private static final String ALTERNATE_ASSESSMENT_PARTICIPATION_ALIAS = "riaa-participate";
    private static final String NON_RESIDENT_DISTRICT_ALIAS = "Non Resident District Code";
    private static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outside Placement School";
    private static final String MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS = "OP Location Code";

    private static final String STATE_VALIDATION_CDP =
            "Career Development Plan has ended.";
    private static final String STATE_VALIDATION_R2037 =
            "R2037 - When student is in PF, PK grades, and age is between 3 and 5 years old, the early childhood data is required (ECEnvironment must have a value). (Age calculated on collection date)";
    private static final String STATE_VALIDATION_R2040 =
            "R2040 - Students in Student Core File without a summer exit must contain at least 1 service entry.";
    private static final String STATE_VALIDATION_R2801 =
            "R2801 - Students whose age is less than 3 and greater than 21 on collection date. (when summer exit is null)";
    private static final String STATE_VALIDATION_R2803 =
            "R2803 - Primary Disability L (Developmentally Delayed) not valid for students >= 9 years old (age calculated on December 1st).";
    private static final String STATE_VALIDATION_R2808 =
            "R2808 - Students ages <= 4 on collection as of date must be in grade PK/PF.";
    private static final String STATE_VALIDATION_R2844 =
            "R2844 - An IEP Attendance record is required for each student with a record on the SPED Student Core file.  At least one of the IEP Meeting dates have to match.";
    private static final String STATE_VALIDATION_R2837 =
            "R2837 - When Summer_Exit Code is L (Graduated with Certificate) or G (Graduated), the Grade must be 12";
    private static final String STATE_VALIDATION_R2849 =
            "R2849 - When student is at least 14 years old at the time of the last IEP Meeting Date, the transition data is required, when the student is not receiving a service plan ONLY.";
    private static final String STATE_VALIDATION_R2851 =
            "R2851 - Student in Grades other than PK and PF and School code is 28702, 07702, 07704, Beneficiary must be Y.";
    private static final String STATE_VALIDATION_R2879 =
            "R2879 - When IEP=NEW and summerexit is null, student must have at least 1 service in which Service_Type='SpEd'.";
    private static final String STATE_VALIDATION_R3000 =
            "R3000 - If transition data is entered, and IEP_Type='NEW', then transition section must be complete (AssessmentTools, PostSchoolGoals, TransitionServices and Assurance cannot be blank).";
    private static final String STATE_VALIDATION_R3001 =
            "R3001 - IEP Team Meeting Date must be less than or equal to the date that the user is submitting the file.";
    private static final String STATE_VALIDATION_R3016 =
            "R3016 - If Summer_Exit is blank, then HISPANIC, WHITE, BLACK, ASIAN, NATIVE, PACIFIC are required.";
    private static final String STATE_VALIDATION_R3017 =
            "R3017 - If Summer_Exit is blank, at least one of (WHITE, BLACK, ASIAN, NATIVE, PACIFIC) must be Y.";
    private static final String STATE_VALIDATION_R3022 =
            "When Schcode contains 990 the student is privately placed. SERVICE_PROGRAM_CONTINUUM must be 0 or 7 AND PRIVATE_SCHOOL_ENROLLEE must be Y.";
    private static final String STATE_VALIDATION_R3026 =
            "R3026 - When student identified with a primary disability of Intellectual Disabilites is at least 14 years old at the time of the last IEP Meeting Date,"
                    + " then CareerDevPlan, VocAssPerCentPlan, SchBasedPrepExp and IntTrialWorkExp must all be Y, when the student is not receiving a service plan ONLY.";
    private static final String STATE_VALIDATION_R3027 =
            "R3027 - When CareerDevPlan is Y, then DateOfCareerDevPlan is required.";
    private static final String STATE_VALIDATION_R3029 =
            "R3029 - When ECEnvironment is between 1 and 14, then ECNameOfProgram is required.";
    private static final String STATE_VALIDATION_R3030 =
            "R3030 - If EITransition is DELAY, then EITransitionDelay is required.";
    private static final String STATE_VALIDATION_R3031 =
            "R3031 - If EITransitionDelay is OTHER, then EITransitionDelayOther is required.";
    private static final String STATE_VALIDATION_CUSTOM1 =
            "Can not find iep start date";
    private static final String STATE_VALIDATION_R3035 =
            "R3035 - If student is in grade PK/PF, then Child Outcomes Entry fields are required - ChildOutcomesEntryDate, Outcome1EntryRating, Outcome2EntryRating, Outcome3EntryRating.";
    private static final String STATE_VALIDATION_R3037 =
            "R3037 - If Outcome1EntryRating is 7 or 6 and Outcome1ExitRating is 7 or 6, then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3038 =
            "R3038 - If Outcome1EntryRating is 5 and Outcome1ExitRating is 7, 6 or 5, then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3039 =
            "R3039 - If Outcome1EntryRating is 4 and Outcome1ExitRating is 7, 6, 5, or 4 then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3040 =
            "R3040 - If Outcome1EntryRating is 3 and Outcome1ExitRating is 7, 6, 5, 4, or 3 then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3041 =
            "R3041 - If Outcome1EntryRating is 2 and Outcome1ExitRating is 7, 6, 5, 4, 3 or 2 then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3042 =
            "R3042 - If Outcome1EntryRating is 1 and Outcome1ExitRating is 7, 6, 5, 4, 3, 2 or 1 then Outcome1Progress cannot be No.";
    private static final String STATE_VALIDATION_R3043 =
            "R3043 - If Outcome2EntryRating is 7 or 6 and Outcome2ExitRating is 7 or 6, then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3044 =
            "R3044 - If Outcome2EntryRating is 5 and Outcome2ExitRating is 7, 6 or 5, then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3045 =
            "R3045 - If Outcome2EntryRating is 4 and Outcome2ExitRating is 7, 6, 5, or 4 then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3046 =
            "R3046 - If Outcome2EntryRating is 3 and Outcome2ExitRating is 7, 6, 5, 4, or 3 then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3047 =
            "R3047 - If Outcome2EntryRating is 2 and Outcome2ExitRating is 7, 6, 5, 4, 3 or 2 then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3048 =
            "R3048 - If Outcome2EntryRating is 1 and Outcome2ExitRating is 7, 6, 5, 4, 3, 2 or 1 then Outcome2Progress cannot be No.";
    private static final String STATE_VALIDATION_R3049 =
            "R3049 - If Outcome3EntryRating is 7 or 6 and Outcome3ExitRating is 7 or 6, then Outcome3Progress cannot be No.";
    private static final String STATE_VALIDATION_R3050 =
            "R3050 - If Outcome3EntryRating is 5 and Outcome3ExitRating is 7, 6 or 5, then Outcome3Progress cannot be No.";
    private static final String STATE_VALIDATION_R3051 =
            "R3051 - If Outcome3EntryRating is 4 and Outcome3ExitRating is 7, 6, 5, or 4 then Outcome3Progress cannot be No.";
    private static final String STATE_VALIDATION_R3052 =
            "R3052 - If Outcome3EntryRating is 3 and Outcome3ExitRating is 7, 6, 5, 4, or 3 then Outcome3Progress cannot be No.";
    private static final String STATE_VALIDATION_R3053 =
            "R3053 - If Outcome3EntryRating is 2 and Outcome3ExitRating is 7, 6, 5, 4, 3 or 2 then Outcome3Progress cannot be No.";
    private static final String STATE_VALIDATION_R3054 =
            "R3054 - If Outcome3EntryRating is 1 and Outcome3ExitRating is 7, 6, 5, 4, 3, 2 or 1 then Outcome3Progress cannot be No.";

    private static final String ENR_TYPE_GRADUATE = "Graduate";

    /**
     * Entity Fields
     */
    private static final String FIELD_CAREER_DEV_PLAN = "CareerDevPlan";
    private static final String FIELD_DATE_CAREER_PLAN = "DateOfCareerDevPlan";
    private static final String FIELD_EC_ENVIRONMENT = "ECEnvironment";
    private static final String FIELD_INT_TRIAL_WORK_EXP = "IntTrialWorkExp";
    private static final String FIELD_OUTCOME_ENTRY_RATING_1 = "Outcome1EntryRating";
    private static final String FIELD_OUTCOME_ENTRY_RATING_2 = "Outcome2EntryRating";
    private static final String FIELD_OUTCOME_ENTRY_RATING_3 = "Outcome3EntryRating";
    private static final String FIELD_OUTCOME_EXIT_RATING_1 = "Outcome1ExitRating";
    private static final String FIELD_OUTCOME_EXIT_RATING_2 = "Outcome2ExitRating";
    private static final String FIELD_OUTCOME_EXIT_RATING_3 = "Outcome3ExitRating";
    private static final String FIELD_OUTCOME_PROGRESS_1 = "Outcome1Progress";
    private static final String FIELD_OUTCOME_PROGRESS_2 = "Outcome2Progress";
    private static final String FIELD_OUTCOME_PROGRESS_3 = "Outcome3Progress";
    private static final String FIELD_SCH_BASED_PREP_EXP = "SchBasedPrepExp";
    private static final String FIELD_PRIVATE_SKL_ENROL = "Private_School_Enrol";
    private static final String FIELD_SKL_CODE = "School_Code";
    private static final String FIELD_VOC_ASS_PER_CENT_PLAN = "VocAssPerCentPlan";

    /**
     * Class members
     */
    protected boolean m_bypassStateRules;
    protected DistrictSchoolYearContext m_currentSchoolYear;
    protected Map<String, Collection<IepDisability>> m_disabilityMap;
    protected SchoolYearDateRangeLookup m_schoolYearLookup;
    protected SchoolYearDateRangeLookup m_schoolYearLookupPreviousYear;
    protected Map<String, StudentContact> m_educationalSurrogates;
    protected Set<String> m_iepsWithAssessmentAccommodations;
    protected Set<String> m_iepsWithAssessmentTools;
    protected Set<String> m_iepsWithMeetingAttendance;
    protected Set<String> m_iepsWithServices;
    protected Set<String> m_iepsWithTransitionServices;
    protected Set<String> m_iepsWithTransportation;
    protected Map<String, Collection<StateReportValidationError>> m_benefitsErros = new HashMap<>();
    protected Map<String, StudentEnrollment> m_latestEntries;
    protected String m_municipalitiesReferenceTableOid;
    protected String m_organizationOid;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected String m_raceReferenceTableOid = null;
    protected PlainDate m_reportDate;
    protected PlainDate m_senior_grade_date = null;

    private Set<String> m_isAgeFiveOrUnder;
    private Criteria m_reportCriteria;
    private BeforeFirstJulyRetriever m_beforeFirstJulyRetriever = null;

    /**
     * Fields
     */
    protected String m_fieldStdNonResCode;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_organizationOid = (String) getParameter(ORGANIZATION_PARAM);
        m_bypassStateRules = Boolean.TRUE.equals(getParameter(BYPASS_STATE_VALIDATION_RULES));
        m_senior_grade_date = (PlainDate) getParameter(SENIOR_GRAD_DATE);
        m_fieldStdNonResCode = translateAliasToJavaName(NON_RESIDENT_DISTRICT_ALIAS, true);

        /*
         * Retrieve the current and previous school years;
         * Calculate the current year first in-session date and previous year last in-session date
         */
        loadCalendarInfo();
        if (getSetupErrors().size() != 0) {
            return;
        }

        /*
         * Mark the latest IEP for each student on the report date
         */
        markIepsToInclude();

        /*
         * Build the criteria for records to include in the report
         */
        m_reportCriteria = getReportCriteria();

        /*
         * Load a race code map for quick lookup
         */
        loadRaceInfo(m_reportCriteria);

        /*
         * Load a set of IEP OIDs with Transportation as a related service for quick lookup
         */
        loadTransportationLookup(m_reportCriteria);

        /*
         * Load a disability map for quick lookup
         */
        loadDisabilityLookup(m_reportCriteria);

        /*
         * Load a set of IEP OIDs with Assessment Accommodations for quick lookup
         */
        loadAccommodationLookup(m_reportCriteria);

        /*
         * Query for the OID of the Municipalities reference table
         */
        loadMunicipalitiesInfo();

        /*
         * Load a set of IEP OIDs with sped services for quick lookup
         */
        loadServicesLookup(m_reportCriteria);

        /*
         * Load a set of IEP OIDs with meeting attendance records on the IEP meeting date for quick
         * lookup
         */
        loadMeetingAttendanceLookup(m_reportCriteria);

        /*
         * Load a map of educational surrogates for quick lookup
         */
        loadEducationalSurrogates(m_reportCriteria);

        /*
         * Load a set of IEP OIDs with assessment tools for quick lookup
         */
        loadAssessmentToolLookup(m_reportCriteria);

        /*
         * Load a set of IEP OIDs with transition services for quick lookup
         */
        loadTransitionServicesLookup(m_reportCriteria);

        /*
         * Load a map of the latest entry enrollment records on or before the report date
         */
        loadLatestEntriesLookup(m_reportCriteria);
        /*
         * Set the student query. The root object for this report is IepData.
         */
        BeanQuery query = new BeanQuery(IepData.class, m_reportCriteria);
        applyInputSort(query, null);
        setQuery(query);

        /*
         * Add field retrievers to support calculated columns
         */
        addCustomCalcs(m_reportCriteria);

        /*
         * Add field validators to support custom validation rules
         */
        addCustomValidators();
    }

    /**
     * Checks if is age five or under.
     *
     * @param stdOid String
     * @return true, if is age five or under
     */
    protected boolean isAgeFiveOrUnder(String stdOid) {
        if (m_isAgeFiveOrUnder == null) {
            Calendar cal = Calendar.getInstance();
            cal.setTime(m_reportDate);
            cal.add(Calendar.YEAR, -6);
            PlainDate isSixDate = new PlainDate(cal.getTime());

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, new SubQuery(IepData.class, IepData.COL_STUDENT_OID, m_reportCriteria));
            criteria.addGreaterThan(SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                    isSixDate);
            m_isAgeFiveOrUnder = getBroker()
                    .getMapByQuery(new BeanQuery(SisStudent.class, criteria), X2BaseBean.COL_OID, 512).keySet();
        }
        return m_isAgeFiveOrUnder.contains(stdOid);
    }


    /**
     * Add custom field retrievers.
     *
     * @param reportCriteria Criteria
     */
    private void addCustomCalcs(Criteria reportCriteria) {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("risped-Race", new RaceRetriever());
        calcRetrievers.put("risped-Disability", new PrimaryDisabilityRetriever());
        calcRetrievers.put("risped-Transport", new TransportationRetriever());
        calcRetrievers.put("risped-AssessAccom", new AssessmentAccommodationRetriever());
        calcRetrievers.put("risped-SummerExit", new SummerExitReasonRetriever());
        calcRetrievers.put("risped-Municipality", new MunicipalityRetriever());
        calcRetrievers.put("risped-Surrogate", new EducationalSurrogateRetriever());
        calcRetrievers.put("risped-AssessTools", new AssessmentToolsRetriever());
        calcRetrievers.put("risped-PostSchool", new PostSchoolGoalsRetriever());
        calcRetrievers.put("risped-TransitionSvc", new TransitionServicesRetriever());
        calcRetrievers.put("risped-Assurance", new AssuranceRetriever());
        calcRetrievers.put("risped-CareerDevPlan", new CareerDevelopmentPlanRetriever(reportCriteria));
        calcRetrievers.put("risped-ChildOutcome", new ChildOutcomeSummaryRetriever(reportCriteria));
        calcRetrievers.put("risped-ServicingDst", new ServicingDistrictRetriever());
        calcRetrievers.put("risped-SchoolCode", new SchoolCodeRetriever());
        calcRetrievers.put("skipLeadingZeroes", new CleanLeadingZeroesRetriever());
        m_beforeFirstJulyRetriever = new BeforeFirstJulyRetriever();
        calcRetrievers.put(BeforeFirstJulyRetriever.CAL_ID, m_beforeFirstJulyRetriever);

        addCalcs(calcRetrievers);
    }

    /**
     * Add custom validators.
     */
    private void addCustomValidators() {
        if (!m_bypassStateRules) {
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();

            validators.put("risped-MeetingDate", new MeetingDateValidator());
            validators.put("risped-SummerExit", new SummerExitValidator());
            validators.put("risped-Transition", new TransitionDataValidator());
            validators.put("risped-Age", new StudentAgeValidator());
            validators.put(EITransitionValidator.VAL_ID, new EITransitionValidator());
            validators.put(ECProgramNameValidator.VAL_ID, new ECProgramNameValidator());
            validators.put(BeforeFirstJulyValidate.VAL_ID, new BeforeFirstJulyValidate());
            validators.put(ValidatorCareerDevPlanDate.VAL_ID, new ValidatorCareerDevPlanDate());
            validators.put(ValidatorBeneficiary.VAL_ID, new ValidatorBeneficiary());
            validators.put(ValidatorOutcome1EntryRating.VAL_ID, new ValidatorOutcome1EntryRating());
            validators.put(ValidatorOutcome2EntryRating.VAL_ID, new ValidatorOutcome2EntryRating());
            validators.put(ValidatorOutcome3EntryRating.VAL_ID, new ValidatorOutcome3EntryRating());
            validators.put(ValidatorChildOutcome1EntryDate.VAL_ID, new ValidatorChildOutcome1EntryDate());
            validators.put(ValidatorCareerDevPlan.VAL_ID, new ValidatorCareerDevPlan());
            validators.put(ValidatorSchoolCode.VAL_ID, new ValidatorSchoolCode());
            validators.put(ValidatorPrimaryDisability.VAL_ID, new ValidatorPrimaryDisability());
            addValidators(validators);
        }
    }

    /**
     * Returns the criteria for the IEPs to include.
     *
     * @return Criteria - IEPs with the IN_LAST_STATE_REPORT_ALIAS field set to true
     */
    private Criteria getReportCriteria() {
        X2Criteria reportCriteria = new X2Criteria();
        String lastStateReportField = translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true);
        if (lastStateReportField != null) {
            reportCriteria.addEqualTo(lastStateReportField, BooleanAsStringConverter.TRUE);
        }
        if (m_senior_grade_date != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, ENR_TYPE_GRADUATE);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_DATE, m_senior_grade_date);
            reportCriteria.addIn(IepData.COL_STUDENT_OID,
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria));
        }
        return reportCriteria;
    }

    /**
     * Loads the set of IEP OIDs with testing accommodations.
     *
     * @param reportCriteria Criteria
     */
    private void loadAccommodationLookup(Criteria reportCriteria) {
        m_iepsWithAssessmentAccommodations = new HashSet<String>(1000);

        Criteria accommodationCriteria = new Criteria();
        accommodationCriteria.addEqualTo(IepAccommodation.COL_TYPE, "Testing");
        accommodationCriteria.addIn(IepAccommodation.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery accommodationQuery =
                new SubQuery(IepAccommodation.class, IepAccommodation.COL_IEP_DATA_OID, accommodationCriteria);
        m_iepsWithAssessmentAccommodations.addAll(getBroker().getSubQueryCollectionByQuery(accommodationQuery));
    }

    /**
     * Loads the set of IEP OIDs with assessment-tool entries.
     *
     * @param reportCriteria Criteria
     */
    private void loadAssessmentToolLookup(Criteria reportCriteria) {
        m_iepsWithAssessmentTools = new HashSet<String>(1000);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepPerformanceLevel.COL_TYPE, "Transition Planning");
        criteria.addNotEmpty(translateAliasToJavaName("transAssess-tool", true),
                getDataDictionary().getPersistenceKey());
        criteria.addIn(IepPerformanceLevel.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery query = new SubQuery(IepPerformanceLevel.class, IepPerformanceLevel.COL_IEP_DATA_OID, criteria);

        m_iepsWithAssessmentTools.addAll(getBroker().getSubQueryCollectionByQuery(query));
    }

    /**
     * Loads calendar information.
     */
    private void loadCalendarInfo() {
        Criteria schoolYearCriteria = new Criteria();
        schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);

        BeanQuery schoolYearQuery = new BeanQuery(DistrictSchoolYearContext.class, schoolYearCriteria);

        m_currentSchoolYear = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);

        DistrictSchoolYearContext previousSchoolYear = null;
        if (m_currentSchoolYear != null) {
            if (CalendarManager.getDistrictInSessionStartEndDate(getOrganization(), m_currentSchoolYear, true,
                    getBroker()) != null) {
                int schoolYearNumber = m_currentSchoolYear.getSchoolYear();

                Criteria previousYearCriteria = new Criteria();
                previousYearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(schoolYearNumber - 1));

                BeanQuery previousSchoolYearQuery =
                        new BeanQuery(DistrictSchoolYearContext.class, previousYearCriteria);

                previousSchoolYear = (DistrictSchoolYearContext) getBroker().getBeanByQuery(previousSchoolYearQuery);

                m_schoolYearLookup =
                        new SchoolYearDateRangeLookup(m_currentSchoolYear, getOrganization().getOid(), getBroker());
            } else {
                addSetupError("Current Year Calendar", "Unable to determine district in-session start and end dates");
            }
        } else {
            addSetupError("Current Year Calendar",
                    "Unable to identify the school year corresponding to the report date");
        }

        if (previousSchoolYear != null) {
            if (CalendarManager.getDistrictInSessionStartEndDate(getOrganization(), previousSchoolYear, true,
                    getBroker()) != null) {
                m_schoolYearLookupPreviousYear =
                        new SchoolYearDateRangeLookup(previousSchoolYear, getOrganization().getOid(), getBroker());

            } else {
                addSetupError("Previous Year Calendar", "Unable to determine district in-session start and end dates");
            }
        } else {
            addSetupError("Previous Year Calendar",
                    "Unable to determine the previous school year's last district in-session date");
        }
    }

    /**
     * Loads the disability lookup map.
     *
     * @param reportCriteria Criteria
     */
    private void loadDisabilityLookup(Criteria reportCriteria) {
        Criteria disabilityCriteria = new Criteria();
        disabilityCriteria.addIn(IepDisability.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        BeanQuery disabilityQuery = new BeanQuery(IepDisability.class, disabilityCriteria);
        m_disabilityMap = getBroker().getGroupedCollectionByQuery(disabilityQuery, IepDisability.COL_IEP_DATA_OID, 100);
    }

    /**
     * Loads the educational surrogates lookup map.
     *
     * @param reportCriteria Criteria
     */
    private void loadEducationalSurrogates(Criteria reportCriteria) {
        Criteria surrogateCriteria = new Criteria();
        surrogateCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, "Surrogate");
        surrogateCriteria.addIn(StudentContact.COL_STUDENT_OID,
                new SubQuery(IepData.class, IepData.COL_STUDENT_OID, reportCriteria));

        BeanQuery surrogateQuery = new BeanQuery(StudentContact.class, surrogateCriteria);

        m_educationalSurrogates = getBroker().getMapByQuery(surrogateQuery, StudentContact.COL_STUDENT_OID, 100);
    }

    /**
     * Loads the enrollment lookup map - latest 'E' records on or prior to the report date.
     *
     * @param reportCriteria Criteria
     */
    private void loadLatestEntriesLookup(Criteria reportCriteria) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                new SubQuery(IepData.class, IepData.COL_STUDENT_OID, reportCriteria));

        BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE); // ensures that
        // the record
        // with the max
        // date will end
        // up in the map

        m_latestEntries = getBroker().getMapByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);
    }

    /**
     * Loads the set of IEP OIDs with meeting attendance entries on the IEP Meeting Date.
     *
     * @param reportCriteria Criteria
     */
    private void loadMeetingAttendanceLookup(Criteria reportCriteria) {
        m_iepsWithMeetingAttendance = new HashSet<String>(1000);

        Criteria meetingAttendanceCriteria = new Criteria();
        meetingAttendanceCriteria.addIn(IepMeetingAttendance.REL_IEP_MEETING + "." + IepMeeting.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));
        meetingAttendanceCriteria.addEqualTo(IepMeetingAttendance.COL_PRESENT_INDICATOR, Boolean.TRUE);
        meetingAttendanceCriteria.addEqualToField(IepMeetingAttendance.REL_IEP_MEETING + "." + IepMeeting.COL_DATE,
                IepMeetingAttendance.REL_IEP_MEETING + "." + IepMeeting.REL_IEP_DATA + "." + IepData.COL_MEETING_DATE);

        SubQuery meetingAttendanceQuery = new SubQuery(IepMeetingAttendance.class,
                IepMeetingAttendance.REL_IEP_MEETING + "." + IepMeeting.COL_IEP_DATA_OID, meetingAttendanceCriteria);
        m_iepsWithMeetingAttendance.addAll(getBroker().getSubQueryCollectionByQuery(meetingAttendanceQuery));
    }

    /**
     * Loads the municipalities reference table OID.
     */
    private void loadMunicipalitiesInfo() {
        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, "Municipalities");

        BeanQuery refTableQuery = new BeanQuery(ReferenceTable.class, refTableCriteria);

        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByQuery(refTableQuery);
        if (refTable != null) {
            m_municipalitiesReferenceTableOid = refTable.getOid();
        } else {
            addSetupError("Municipalities", "Unable to find a reference table named 'Municipalities'");
        }
    }

    /**
     * Loads the race code map and race reference table OID.
     *
     * @param reportCriteria Criteria
     */
    private void loadRaceInfo(Criteria reportCriteria) {
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID,
                new SubQuery(IepData.class, IepData.REL_STUDENT + "." + Student.COL_PERSON_OID, reportCriteria));

        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        m_raceReferenceTableOid =
                new ModelProperty(Race.class, Race.COL_RACE_CODE, getDataDictionary().getPersistenceKey()).getField()
                        .getReferenceTableOid();
    }

    /**
     * Loads the set of IEP OIDs with special education services.
     *
     * @param reportCriteria Criteria
     */
    private void loadServicesLookup(Criteria reportCriteria) {
        m_iepsWithServices = new HashSet<String>(1000);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, "Special Education");
        criteria.addIn(IepService.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery query = new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, criteria);

        m_iepsWithServices.addAll(getBroker().getSubQueryCollectionByQuery(query));
    }

    /**
     * Loads the set of IEP OIDs with transition services.
     *
     * @param reportCriteria Criteria
     */
    private void loadTransitionServicesLookup(Criteria reportCriteria) {
        m_iepsWithTransitionServices = new HashSet<String>(1000);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, "Transition Services");
        criteria.addNotEmpty(translateAliasToJavaName("transition-area", true),
                getDataDictionary().getPersistenceKey());
        criteria.addIn(IepService.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery query = new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, criteria);

        m_iepsWithTransitionServices.addAll(getBroker().getSubQueryCollectionByQuery(query));
    }

    /**
     * Loads the set of IEP OIDs with transportation services.
     *
     * @param reportCriteria Criteria
     */
    private void loadTransportationLookup(Criteria reportCriteria) {
        m_iepsWithTransportation = new HashSet<String>(1000);

        Criteria serviceCriteria = new Criteria();
        serviceCriteria.addEqualTo(IepService.COL_SERVICE_TYPE, "Transportation");
        serviceCriteria.addEqualTo(IepService.COL_SERVICE_MODE, "Related Services");
        serviceCriteria.addIn(IepService.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

        SubQuery serviceQuery = new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, serviceCriteria);
        m_iepsWithTransportation.addAll(getBroker().getSubQueryCollectionByQuery(serviceQuery));
    }

    /**
     * Marks the IEPs to include. See class javadoc for description of IEPs that are included.
     */
    private void markIepsToInclude() {
        if (Boolean.TRUE.equals(getParameter(RECALCULATE_IEPS_TO_INCLUDE))) {
            X2Broker broker = getBroker();

            boolean excludeNonResidents = Boolean.TRUE.equals(getParameter(EXCLUDE_NON_RESIDENTS));

            Criteria orgCriteria = OrganizationManager.getOrganizationCriteria(m_organizationOid);
            SubQuery studentOrgSubquery = new SubQuery(Student.class, X2BaseBean.COL_OID, orgCriteria);

            /*
             * First clear the include indicator
             */
            Criteria updateCriteria = new Criteria();
            updateCriteria.addIn(IepData.COL_STUDENT_OID, studentOrgSubquery);

            HashMap<String, Object> attributesToUpdate = new HashMap<String, Object>();
            String lastStateReportField = translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true);
            if (lastStateReportField != null) {
                attributesToUpdate.put(lastStateReportField, BooleanAsStringConverter.FALSE);
            }

            UpdateQuery updateQuery = new UpdateQuery(IepData.class, updateCriteria, attributesToUpdate);
            broker.executeUpdateQuery(updateQuery);

            /*
             * IEPs starting before the report date, sorted in ascending order by status code and
             * descending order by start date
             *
             * Include students were active at any time since the summer exit start date
             */
            PlainDate reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

            PlainDate summerExitStartDate = (PlainDate) getParameter(SUMMER_EXIT_START_DATE);
            if (summerExitStartDate == null) {
                summerExitStartDate = m_schoolYearLookupPreviousYear.getEarliestEndDate();
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(IepData.COL_STATUS_CODE,
                    Arrays.asList(new Integer[] {Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                            Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal())}));
            criteria.addLessThan(IepData.COL_START_DATE, reportDate);
            criteria.addGreaterThan(IepData.COL_END_DATE, summerExitStartDate);
            criteria.addIn(IepData.COL_STUDENT_OID, studentOrgSubquery);

            /*
             * Exclude students that exited from special education prior to the summer exit start
             * date
             */
            X2Criteria studentExitDateCriteria = new X2Criteria();
            studentExitDateCriteria.addIsNull(IepData.REL_STUDENT + "." + Student.COL_SPED_EXIT_DATE);

            X2Criteria studentExitDateOrCriteria = new X2Criteria();
            studentExitDateOrCriteria.addGreaterThan(IepData.REL_STUDENT + "." + Student.COL_SPED_EXIT_DATE,
                    summerExitStartDate);

            studentExitDateCriteria.addOrCriteria(studentExitDateOrCriteria);
            studentExitDateCriteria.setEmbraced(true);

            criteria.addAndCriteria(studentExitDateCriteria);

            BeanQuery beanQuery = new BeanQuery(IepData.class, criteria);
            beanQuery.addOrderByAscending(IepData.COL_STUDENT_OID);
            beanQuery.addOrderByAscending(IepData.COL_STATUS_CODE); // Active is ordinal "1",
            // Previous is "2" so this will
            // ensure active IEPs sort in
            // front
            beanQuery.addOrderByDescending(IepData.COL_START_DATE);
            beanQuery.addOrderByDescending(X2BaseBean.COL_OID); // Should be unnecessary... included
            // to ensure deterministic sort

            QueryIterator ieps = broker.getIteratorByQuery(beanQuery);

            try {
                String lastStudentOid = null;
                while (ieps.hasNext()) {
                    IepData iep = (IepData) ieps.next();
                    if (!iep.getStudentOid().equals(lastStudentOid)) {
                        /*
                         * If non residents are being excluded, check the student's non-resident
                         * district code. If provided an different than the reporting district,
                         * omit the record.
                         */
                        boolean validResidence = true;
                        if (excludeNonResidents) {
                            String nonResidentDistrict = (String) iep.getStudent()
                                    .getFieldValueByAlias(NON_RESIDENT_DISTRICT_ALIAS, getDataDictionary());
                            if (!StringUtils.isEmpty(nonResidentDistrict)) {
                                String nonResidentDistrictStateCode = lookupStateValue(Student.class,
                                        translateAliasToJavaName(NON_RESIDENT_DISTRICT_ALIAS, true),
                                        nonResidentDistrict);
                                validResidence = nonResidentDistrictStateCode.equals(m_districtIdField);
                            }
                        }

                        if (validResidence) {
                            iep.setFieldValueByAlias(IN_LAST_STATE_REPORT_ALIAS, BooleanAsStringConverter.TRUE,
                                    getDataDictionary());
                            broker.saveBeanForced(iep);
                        }
                    }

                    lastStudentOid = iep.getStudentOid();
                }
            } finally {
                ieps.close();
            }

            // Further narrow the IEPs to include based on additional criteria
            X2Criteria iepCriteria = new X2Criteria();

            /*
             * IEP CRITERIA: IEPs marked for inclusion
             */
            iepCriteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                    BooleanAsStringConverter.TRUE);

            /*
             * EXCLUDE FROM STATE REPORTING CRITERIA: Exclude students who are excluded from state
             * reporting, if present
             */

            if (!Boolean.TRUE.equals(getParameter(INCLUDE_STUDENTS_MARKED_FOR_EXCLUSION))) {
                String excludeStudentField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);
                if (!StringUtils.isEmpty(excludeStudentField)) {
                    iepCriteria.addNotEqualTo(IepData.REL_STUDENT + "." + excludeStudentField,
                            BooleanAsStringConverter.TRUE);
                }
            }

            /*
             * SASID CRITERIA: Exclude students with no SASID if applicable
             */
            if (Boolean.TRUE.equals(getParameter(SASID_STUDENTS_ONLY_PARAM))) {
                iepCriteria.addNotEmpty(IepData.REL_STUDENT + "." + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
            }

            /*
             * SPED STATUS CODE:
             */
            String spedActiveCode = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                    SisPreferenceConstants.SPED_ACTIVE_CODE);
            String spedExitedCode = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                    SisPreferenceConstants.SPED_EXITED_CODE);
            iepCriteria.addIn(IepData.REL_STUDENT + "." + Student.COL_SPED_STATUS_CODE,
                    Arrays.asList(new String[] {spedActiveCode, spedExitedCode}));

            // Update the IEPS to include
            attributesToUpdate.put(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                    BooleanAsStringConverter.TRUE);
            updateQuery = new UpdateQuery(IepData.class, iepCriteria, attributesToUpdate);
            broker.executeUpdateQuery(updateQuery);
        }
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * FIELD RETRIEVERS
     * /* -----------------------------------------------------------------------------------------
     */

    /**
     * Retrieves the assessment accommodation status: "1"-No accomodation, "2"-with accomodations,
     * "3"-alternate assessment.
     *
     * @author Follett Software Company
     */
    protected class AssessmentAccommodationRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String result = "1";

            IepData iep = (IepData) entity.getBean();

            try {
                String altAssessment =
                        (String) iep.getFieldValueByAlias(ALTERNATE_ASSESSMENT_PARTICIPATION_ALIAS,
                                getDataDictionary());
                if ("1".equals(altAssessment)) {
                    result = "3";
                } else {
                    result = m_iepsWithAssessmentAccommodations.contains(iep.getOid()) ? "2" : "1";
                }
            } catch (BeanPathException bpe) {
                addSetupError("Missing alias", "Alias " + ALTERNATE_ASSESSMENT_PARTICIPATION_ALIAS + " is undefined");
            }

            return result;
        }
    }

    /**
     * Retrieves "Y" if the IEP contains assessment tools.
     *
     * @author Follett Software Company
     */
    protected class AssessmentToolsRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_iepsWithAssessmentTools.contains(entity.getBean().getOid()));
        }
    }

    /**
     * Retrieves "Y" if both assurance fields are checked on the IEP.
     *
     * @author Follett Software Company
     */
    protected class AssuranceRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();

            String assurance1 = (String) iep.getFieldValueByAlias("trans-assurance1", getDataDictionary());
            String assurance2 = (String) iep.getFieldValueByAlias("trans-assurance2", getDataDictionary());

            return Boolean.valueOf("1".equals(assurance1) && "1".equals(assurance2));
        }
    }

    /**
     * The Class CareerDevelopmentPlanRetriever.
     */
    protected class CareerDevelopmentPlanRetriever implements FieldRetriever {
        private static final String ALIAS_BENEFIT_PLAN_INDICATOR = "crrpln-benefit-plan-ind";
        private static final String ALIAS_DATE = "crrpln-plan-date";
        private static final String CAREER_DEV_PLAN_FORM_ID_CD = "SPED-RI-CD-PLAN";
        private static final String CAREER_DEV_PLAN_FORM_ID_CRR = "SPED-RI-CRR-PLAN";
        private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
        private static final String PARAM_DATE = "date";

        private SystemStringConverter m_converterDate;
        private DataDictionaryField m_fieldBenefitPlanIndicator;
        private DataDictionaryField m_fieldDate;
        private Map<String, Collection<SisGenericFormData>> m_mapCareerDevelopmentPlans = new HashMap();

        /**
         * Instantiates a new career development plan retriever.
         *
         * @param reportCriteria Criteria
         */
        public CareerDevelopmentPlanRetriever(Criteria reportCriteria) {
            X2Criteria fmdCriteriaCD = new X2Criteria();
            fmdCriteriaCD.addEqualTo(FormDefinition.COL_ID, CAREER_DEV_PLAN_FORM_ID_CD);
            FormDefinition fmdCD =
                    (FormDefinition) getBroker().getBeanByQuery(new BeanQuery(FormDefinition.class, fmdCriteriaCD));
            ExtendedDataDictionary ddxCD = fmdCD == null ? null : fmdCD.getExtendedDataDictionary();

            X2Criteria fmdCriteriaCRR = new X2Criteria();
            fmdCriteriaCRR.addEqualTo(FormDefinition.COL_ID, CAREER_DEV_PLAN_FORM_ID_CRR);
            FormDefinition fmdCRR =
                    (FormDefinition) getBroker().getBeanByQuery(new BeanQuery(FormDefinition.class, fmdCriteriaCRR));
            ExtendedDataDictionary ddxCRR = fmdCRR == null ? null : fmdCRR.getExtendedDataDictionary();

            if (ddxCD != null) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddxCD, ddxCD.getPersistenceKey());
                m_fieldDate = translateAliasToJavaName(dictionary, ALIAS_DATE, SisGenericFormData.OBJECT_PREFIX);
                if (m_fieldDate != null) {
                    m_converterDate = getStringConverter(m_fieldDate);
                }

                m_fieldBenefitPlanIndicator = translateAliasToJavaName(dictionary, ALIAS_BENEFIT_PLAN_INDICATOR,
                        SisGenericFormData.OBJECT_PREFIX);
            } else if (ddxCRR != null) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddxCRR, ddxCRR.getPersistenceKey());
                m_fieldDate = translateAliasToJavaName(dictionary, ALIAS_DATE, SisGenericFormData.OBJECT_PREFIX);
                if (m_fieldDate != null) {
                    m_converterDate = getStringConverter(m_fieldDate);
                }

                m_fieldBenefitPlanIndicator = translateAliasToJavaName(dictionary, ALIAS_BENEFIT_PLAN_INDICATOR,
                        SisGenericFormData.OBJECT_PREFIX);
            }
            if (((fmdCD != null && ddxCD != null) || (fmdCRR != null && ddxCRR != null)) && m_fieldDate != null
                    && m_fieldBenefitPlanIndicator != null) {
                X2Criteria fmiCriteria = new X2Criteria();
                fmiCriteria.addIn(
                        FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        Arrays.asList(CAREER_DEV_PLAN_FORM_ID_CD, CAREER_DEV_PLAN_FORM_ID_CRR));
                fmiCriteria.addIn(FormInstance.COL_OWNER_OBJECT_OID,
                        new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));
                X2Criteria gfdCriteria = new X2Criteria();
                gfdCriteria.addIn(SisGenericFormData.COL_EXTENDED_DATA_DICTIONARY_OID,
                        Arrays.asList(ddxCD != null ? ddxCD.getOid() : "___dummy___",
                                ddxCRR != null ? ddxCRR.getOid() : "___dummy___"));
                gfdCriteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, fmiCriteria));
                Map<String, SisGenericFormData> mapFormData = getBroker()
                        .getMapByQuery(new BeanQuery(SisGenericFormData.class, gfdCriteria), X2BaseBean.COL_OID, 256);

                ReportQueryByCriteria query = new ReportQueryByCriteria(FormInstance.class,
                        new String[] {FormInstance.COL_OWNER_OBJECT_OID, FormInstance.COL_STORAGE_OBJECT_OID},
                        fmiCriteria);
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String iepOid = (String) row[0];
                        String gfdOid = (String) row[1];
                        SisGenericFormData form = mapFormData.get(gfdOid);
                        if (form != null) {
                            Collection<SisGenericFormData> forms = m_mapCareerDevelopmentPlans.get(iepOid);
                            if (forms == null) {
                                forms = new LinkedList();
                                m_mapCareerDevelopmentPlans.put(iepOid, forms);
                            }
                            forms.add(form);
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            Collection<SisGenericFormData> forms = m_mapCareerDevelopmentPlans.get(entity.getBean().getOid());
            if (forms != null) {
                if (PARAM_DATE.equals(field.getParameter()) && m_converterDate != null) {
                    for (SisGenericFormData form : forms) {
                        PlainDate date = (PlainDate) m_converterDate
                                .parseSystemString((String) form.getFieldValueByBeanPath(m_fieldDate.getJavaName()));
                        if (date != null) {
                            if (value == null) {
                                value = date;
                            } else {
                                if (date.after((PlainDate) value)) {
                                    value = date;
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the string converter.
         *
         * @param field DataDictionaryField
         * @return System string converter
         */
        private SystemStringConverter getStringConverter(DataDictionaryField field) {
            SystemStringConverter converter = null;
            if (field.isString()) {
                Converter baseConverter = ConverterFactory.getConverterForClass(
                        field.getEffectiveJavaType(),
                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                        field.isString());
                if (baseConverter instanceof SystemStringConverter) {
                    converter = ((SystemStringConverter) baseConverter);
                }
            }
            return converter;
        }

        /**
         * Translate alias to java name.
         *
         * @param dictionary DataDictionary
         * @param alias String
         * @param objectPrefix String
         * @return String
         */
        private DataDictionaryField translateAliasToJavaName(DataDictionary dictionary,
                                                             String alias,
                                                             String objectPrefix) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                String aliasMsg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
                addSetupError(aliasMsg, alias);
            } else {
                if (!objectPrefix.equals(field.getTable().getObjectPrefix())) {
                    addSetupError("Alias field is defined on the wrong table", alias);
                }
            }
            return field;
        }
    }


    /**
     * The Class ChildOutcomeSummaryRetriever.
     */
    protected class ChildOutcomeSummaryRetriever implements FieldRetriever {
        private static final String CHILD_OUTCOME_SUMMARY_FORM_ID = "SPED-RI-COSF";

        private static final String ALIAS_ENTRY_DATE = "cos-date-a";
        private static final String ALIAS_EXIT_DATE = "cos-date-c";
        private static final String ALIAS_ENTRY_SOCIAL = "cos-social-skills-c";
        private static final String ALIAS_ENTRY_KNOWLEDGE = "cos-knowledge-skills-c";
        private static final String ALIAS_ENTRY_ACTION = "cos-action-c";
        private static final String ALIAS_EXIT_SOCIAL = "cos-social-skills-exit-c";
        private static final String ALIAS_EXIT_KNOWLEDGE = "cos-knowledge-skills-exit-c";
        private static final String ALIAS_EXIT_ACTION = "cos-action-exit-c";
        private static final String ALIAS_PROGRESS_SOCIAL = "cos-social-skills-ind-c";
        private static final String ALIAS_PROGRESS_KNOWLEDGE = "cos-knowledge-skills-ind-c";
        private static final String ALIAS_PROGRESS_ACTION = "cos-action-ind-c";
        private static final String ALIAS_NOEXIT_MONTHS = "cos-less-6-mos-ind-c";
        private static final String ALIAS_NOEXIT_CONTACT = "cos-loss-of-contact-ind-c";

        private static final String PARAM_ENTRY_DATE = "entryDate";
        private static final String PARAM_EXIT_DATE = "exitDate";
        private static final String PARAM_ENTRY_SOCIAL = "entrySocial";
        private static final String PARAM_ENTRY_KNOWLEDGE = "entryKnowledge";
        private static final String PARAM_ENTRY_ACTION = "entryAction";
        private static final String PARAM_EXIT_SOCIAL = "exitSocial";
        private static final String PARAM_EXIT_KNOWLEDGE = "exitKnowledge";
        private static final String PARAM_EXIT_ACTION = "exitAction";
        private static final String PARAM_PROGRESS_SOCIAL = "progressSocial";
        private static final String PARAM_PROGRESS_KNOWLEDGE = "progressKnowledge";
        private static final String PARAM_PROGRESS_ACTION = "progressAction";
        private static final String PARAM_NOEXIT = "noExit";

        private static final String VALUE_TIME = "TIME";
        private static final String VALUE_CONTACT = "LOSTCONTACT";

        private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

        private DateAsStringConverter m_converterDate;
        private String m_fieldEntryDate;
        private String m_fieldExitDate;
        private String m_fieldEntrySocial;
        private String m_fieldEntryKnowledge;
        private String m_fieldEntryAction;
        private String m_fieldExitSocial;
        private String m_fieldExitKnowledge;
        private String m_fieldExitAction;
        private String m_fieldProgressSocial;
        private String m_fieldProgressKnowledge;
        private String m_fieldProgressAction;
        private String m_fieldNoExitMonths;
        private String m_fieldNoExitContact;
        private Map<String, SisGenericFormData> m_mapCOSFs;

        /**
         * Instantiates a new career development plan retriever.
         *
         * @param reportCriteria Criteria
         */
        public ChildOutcomeSummaryRetriever(Criteria reportCriteria) {
            X2Criteria fmdCriteria = new X2Criteria();
            fmdCriteria.addEqualTo(FormDefinition.COL_ID, CHILD_OUTCOME_SUMMARY_FORM_ID);
            FormDefinition fmd =
                    (FormDefinition) getBroker().getBeanByQuery(new BeanQuery(FormDefinition.class, fmdCriteria));
            ExtendedDataDictionary ddx = fmd == null ? null : fmd.getExtendedDataDictionary();
            if (ddx != null) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, ddx.getPersistenceKey());
                Locale locale = LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey());
                m_converterDate = (DateAsStringConverter) ConverterFactory
                        .getConverterForClass(Converter.DATE_CONVERTER, locale, true);
                m_fieldEntryDate =
                        translateAliasToJavaName(dictionary, ALIAS_ENTRY_DATE, SisGenericFormData.OBJECT_PREFIX);
                m_fieldExitDate =
                        translateAliasToJavaName(dictionary, ALIAS_EXIT_DATE, SisGenericFormData.OBJECT_PREFIX);
                m_fieldEntrySocial =
                        translateAliasToJavaName(dictionary, ALIAS_ENTRY_SOCIAL, SisGenericFormData.OBJECT_PREFIX);
                m_fieldEntryKnowledge =
                        translateAliasToJavaName(dictionary, ALIAS_ENTRY_KNOWLEDGE, SisGenericFormData.OBJECT_PREFIX);
                m_fieldEntryAction =
                        translateAliasToJavaName(dictionary, ALIAS_ENTRY_ACTION, SisGenericFormData.OBJECT_PREFIX);
                m_fieldExitSocial =
                        translateAliasToJavaName(dictionary, ALIAS_EXIT_SOCIAL, SisGenericFormData.OBJECT_PREFIX);
                m_fieldExitKnowledge =
                        translateAliasToJavaName(dictionary, ALIAS_EXIT_KNOWLEDGE, SisGenericFormData.OBJECT_PREFIX);
                m_fieldExitAction =
                        translateAliasToJavaName(dictionary, ALIAS_EXIT_ACTION, SisGenericFormData.OBJECT_PREFIX);
                m_fieldProgressSocial =
                        translateAliasToJavaName(dictionary, ALIAS_PROGRESS_SOCIAL, SisGenericFormData.OBJECT_PREFIX);
                m_fieldProgressKnowledge = translateAliasToJavaName(dictionary, ALIAS_PROGRESS_KNOWLEDGE,
                        SisGenericFormData.OBJECT_PREFIX);
                m_fieldProgressAction =
                        translateAliasToJavaName(dictionary, ALIAS_PROGRESS_ACTION, SisGenericFormData.OBJECT_PREFIX);
                m_fieldNoExitMonths =
                        translateAliasToJavaName(dictionary, ALIAS_NOEXIT_MONTHS, SisGenericFormData.OBJECT_PREFIX);
                m_fieldNoExitContact =
                        translateAliasToJavaName(dictionary, ALIAS_NOEXIT_CONTACT, SisGenericFormData.OBJECT_PREFIX);

                X2Criteria fmiCriteria = new X2Criteria();
                fmiCriteria.addEqualTo(
                        FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        CHILD_OUTCOME_SUMMARY_FORM_ID);
                fmiCriteria.addIn(FormInstance.COL_OWNER_OBJECT_OID,
                        new SubQuery(IepData.class, X2BaseBean.COL_OID, reportCriteria));

                X2Criteria gfdCriteria = new X2Criteria();
                gfdCriteria.addEqualTo(SisGenericFormData.COL_EXTENDED_DATA_DICTIONARY_OID, ddx.getOid());
                gfdCriteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, fmiCriteria));
                Map<String, SisGenericFormData> mapFormData = getBroker()
                        .getMapByQuery(new BeanQuery(SisGenericFormData.class, gfdCriteria), X2BaseBean.COL_OID, 256);

                ReportQueryByCriteria query = new ReportQueryByCriteria(FormInstance.class,
                        new String[] {FormInstance.COL_OWNER_OBJECT_OID, FormInstance.COL_STORAGE_OBJECT_OID},
                        fmiCriteria);

                m_mapCOSFs = new HashMap<String, SisGenericFormData>();
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String iepOid = (String) row[0];
                        String gfdOid = (String) row[1];
                        SisGenericFormData form = mapFormData.get(gfdOid);
                        if (form != null) {
                            m_mapCOSFs.put(iepOid, form);
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            IepData iep = ((IepData) entity.getBean());
            SisStudent student = iep.getStudent();
            String gradeLevel = lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, student.getGradeLevel());
            // Only report for PK/PF students.
            if (!"PK".equals(gradeLevel) && !"PF".equals(gradeLevel)) {
                return null;
            }
            SisGenericFormData form = m_mapCOSFs.get(iep.getOid());
            if (form != null) {
                if (PARAM_ENTRY_DATE.equals(field.getParameter())) {
                    String date = (String) form.getFieldValueByBeanPath(m_fieldEntryDate);
                    if (date != null) {
                        value = m_converterDate.parseSystemString(date);
                    }
                } else if (PARAM_EXIT_DATE.equals(field.getParameter())) {
                    String date = (String) form.getFieldValueByBeanPath(m_fieldExitDate);
                    if (date != null) {
                        value = m_converterDate.parseSystemString(date);
                    }
                } else if (PARAM_ENTRY_SOCIAL.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldEntrySocial);
                } else if (PARAM_ENTRY_KNOWLEDGE.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldEntryKnowledge);
                } else if (PARAM_ENTRY_ACTION.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldEntryAction);
                } else if (PARAM_EXIT_SOCIAL.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldExitSocial);
                } else if (PARAM_EXIT_KNOWLEDGE.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldExitKnowledge);
                } else if (PARAM_EXIT_ACTION.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldExitAction);
                } else if (PARAM_PROGRESS_SOCIAL.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldProgressSocial);
                } else if (PARAM_PROGRESS_KNOWLEDGE.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldProgressKnowledge);
                } else if (PARAM_PROGRESS_ACTION.equals(field.getParameter())) {
                    value = form.getFieldValueByBeanPath(m_fieldProgressAction);
                } else if (PARAM_NOEXIT.equals(field.getParameter())) {
                    String months = (String) form.getFieldValueByBeanPath(m_fieldNoExitMonths);
                    String contact = (String) form.getFieldValueByBeanPath(m_fieldNoExitContact);
                    if (BooleanAsStringConverter.TRUE.equals(months)) {
                        value = VALUE_TIME;
                    } else if (BooleanAsStringConverter.TRUE.equals(contact)) {
                        value = VALUE_CONTACT;
                    }
                }
            }
            return value;
        }

        /**
         * Translate alias to java name.
         *
         * @param dictionary DataDictionary
         * @param alias String
         * @param objectPrefix String
         *
         * @return String
         */
        private String translateAliasToJavaName(DataDictionary dictionary,
                                                String alias,
                                                String objectPrefix) {
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                String aliasMsg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
                addSetupError(aliasMsg, alias);
            } else {
                if (!objectPrefix.equals(field.getTable().getObjectPrefix())) {
                    addSetupError("Alias field is defined on the wrong table", alias);
                }
            }
            return field != null ? field.getJavaName() : null;
        }
    }


    /**
     * Retriever that clean up leading zeroes for reference values defined in chars.
     *
     * @author maliseenko
     */
    protected class CleanLeadingZeroesRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object baseValue = WebUtils.getProperty(entity.getBean(), field.getBeanPath());
            baseValue = entity.getData().lookupReferenceCodeByBeanPath(entity.getBean().getClass(), field.getBeanPath(),
                    (String) baseValue, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            if (baseValue != null) {
                baseValue = baseValue.toString().trim().replaceAll("^0*", "");
            } else {
                baseValue = StateReportData.EMPTY_STRING;
            }

            return baseValue;
        }
    }


    /**
     * Returns the data element of the student's educational surrogate (if one exists) specified
     * in the field definition parameter.
     *
     * @author Follett Software Company
     */
    protected class EducationalSurrogateRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String result = "";

            IepData iep = (IepData) entity.getBean();
            StudentContact surrogate = m_educationalSurrogates.get(iep.getStudentOid());

            if (surrogate != null) {
                try {
                    String parameter = (String) field.getParameter();
                    if ("Last_Name".equals(parameter)) {
                        result = surrogate.getPerson().getLastName();
                    } else if ("First_Name".equals(parameter)) {
                        result = surrogate.getPerson().getFirstName();
                    } else if ("Address_1".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getAddressLine01();
                    } else if ("Address_2".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getAddressLine02();
                    } else if ("City".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getCity();
                    } else if ("State".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getState();
                    } else if ("Zip".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getPostalCode();
                    } else if ("Work_Phone".equals(parameter)) {
                        result = surrogate.getPerson().getPhysicalAddress().getPhone02();
                    }
                } catch (NullPointerException npe) {
                    /*
                     * No person or address attached to the surrogate contact record; value will
                     * be blank
                     */
                }
            }

            return result;
        }
    }


    /**
     * return value by bean path if field param = true and iep start date before July 01.2017
     * or param = false and iep start date after or equal July 01.2017
     * iep start date it is initial Eligibility Date
     * if null - it is iepData start date(bean path).
     * if null - it is iepData meeting date
     * if null - it is date when created formInstance with "IEP_RI" form definition Id for current
     * iepData
     * if null - it is date when created any first formInstance where owner is current iepDate. Sort
     * by ascending
     * if null - it is last time when iepData modified.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class BeforeFirstJulyRetriever implements FieldRetriever {

        private static final String CAL_ID = "beforeJuly";
        private final String TRUE = Boolean.TRUE.toString();
        private final String FALSE = Boolean.FALSE.toString();
        private Map<String, PlainDate> m_iepStartDateMap = new HashMap<String, PlainDate>();
        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        private static final String FIRST_JULY = "07/01/2017";
        private PlainDate m_firstJuly = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object returnValue = null;
            try {
                if (m_firstJuly == null) {
                    m_firstJuly = new PlainDate(m_dateFormat.parse(FIRST_JULY));
                }
            } catch (ParseException e) {
                // nothing to do. It using constant. have never be exception. I hope.
            }

            IepData iepData = (IepData) entity.getBean();
            PlainDate iepStartDate = getIepStartDate(iepData);
            if (iepStartDate != null) {
                String beforeFirstJuly = (String) field.getParameter();
                if (StringUtils.isEmpty(beforeFirstJuly)) {
                    beforeFirstJuly = FALSE;
                }
                boolean getByBeanPath = false;
                if (iepStartDate.before(m_firstJuly) && TRUE.equals(beforeFirstJuly)) {
                    getByBeanPath = true;
                } else if (!iepStartDate.before(m_firstJuly) && FALSE.equals(beforeFirstJuly)) {
                    getByBeanPath = true;
                }

                if (getByBeanPath) {
                    returnValue = iepData.getFieldValueByBeanPath(field.getBeanPath());
                }
            }

            return returnValue;
        }

        /**
         * iep start date it is initial Eligibility Date
         * if null - it is iepData start date(bean path).
         *
         * @param iepData IepData
         * @return Plain date
         */
        public PlainDate getIepStartDate(IepData iepData) {
            PlainDate startDate = m_iepStartDateMap.get(iepData.getOid());

            if (startDate == null) {
                startDate = iepData.getInitialEligibilityDate();
                if (startDate == null) {
                    startDate = iepData.getStartDate();
                }

                m_iepStartDateMap.put(iepData.getOid(), startDate);
            }
            return startDate;
        }


    }


    /**
     * Returns the state reference code equivalent (in the Municipalities reference table) of the
     * student's physical address city.
     *
     * @author Follett Software Company
     */
    protected class MunicipalityRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String result = "";

            IepData iep = (IepData) entity.getBean();
            String city = null;

            try {
                city = iep.getStudent().getPerson().getPhysicalAddress().getCity();
            } catch (NullPointerException npe) {
                // No address or city associated
            }

            if (city != null) {
                result = getDataDictionary().findStateReferenceCode(m_municipalitiesReferenceTableOid, city);
            }
            if (result == null) {
                String servicingDistrict = (String) iep.getStudent().getFieldValueByBeanPath(m_fieldStdNonResCode);

                // Use a state code equivalent if one exists
                if (!StringUtils.isEmpty(servicingDistrict)) {
                    result = lookupStateValue(SisStudent.class, m_fieldStdNonResCode, servicingDistrict);
                }
            }
            return result;
        }
    }


    /**
     * Retrieves "Y" if the student's IEP has post-school goals specified. At least one of the
     * trans-psg-edu, trans-psg-emp, or trans-psg-ind fields must contain a value.
     *
     * @author Follett Software Company
     */
    protected class PostSchoolGoalsRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();

            String educationAndTraining = (String) iep.getFieldValueByAlias("trans-psg-edu", getDataDictionary());
            String employment = (String) iep.getFieldValueByAlias("trans-psg-emp", getDataDictionary());
            String independentLiving = (String) iep.getFieldValueByAlias("trans-psg-ind", getDataDictionary());

            return Boolean.valueOf(!StringUtils.isEmpty(educationAndTraining) ||
                    !StringUtils.isEmpty(employment) ||
                    !StringUtils.isEmpty(independentLiving));
        }
    }


    /**
     * Retrieves the student's primary disability. If only one disability exists, that disability
     * is used. If more than one exists, the one flagged as primary is used. If more than one
     * exists,
     * and one is not flagged as primary, or more than one is flagged as primary, a validation
     * error is generated.
     *
     * @author Follett Software Company
     */
    protected class PrimaryDisabilityRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();
            IepDisability primaryDisability = null;

            int primaries = 0;
            int unflagged = 0;

            Collection<IepDisability> disabilities = m_disabilityMap.get(iep.getOid());

            if (disabilities != null && !disabilities.isEmpty()) {
                primaryDisability = disabilities.iterator().next();

                for (IepDisability disability : disabilities) {
                    if (disability.getPrimaryIndicator()) {
                        primaryDisability = disability;
                        primaries++;
                    } else {
                        unflagged++;
                    }
                }
            }

            String stateCode = null;
            if (primaryDisability == null) {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field, "No disability",
                                "No disability information found"));
            } else {
                stateCode = lookupStateValue(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                        primaryDisability.getDisabilityCode());

                if (primaries > 1 || (unflagged > 1 && primaries == 0)) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, "Multipe Disabilities",
                                    "Unable to determine primary disability; using '"
                                            + primaryDisability.getDisabilityCode() + "' [" + stateCode + "]"));
                }
            }

            return stateCode;
        }
    }


    /**
     * Retrieves the race code indicator ("Y" or "N") specified in the FeidlDefinition's parameter.
     *
     * @author Follett Software Company
     */
    protected class RaceRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(iep.getStudent().getPersonOid());

            String stateCodeParam = (String) field.getParameter();

            boolean raceIndicator = false;

            if (races != null) {
                for (Race personRace : races) {
                    String stateCode = getDataDictionary().findStateReferenceCode(m_raceReferenceTableOid,
                            personRace.getRaceCode());
                    if (stateCodeParam.equals(stateCode)) {
                        raceIndicator = true;
                    }
                }
            }
            return Boolean.valueOf(raceIndicator);
        }
    }


    /**
     * Returns the servicing district. If the NON_RESIDENT_DISTRICT_ALIAS field contains an null/
     * empty value, the reporting district is used.
     *
     * @author Follett Software Company
     */
    protected class ServicingDistrictRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();

            String servicingDistrict = (String) iep.getStudent().getFieldValueByAlias(NON_RESIDENT_DISTRICT_ALIAS);

            // Use a state code equivalent if one exists
            if (!StringUtils.isEmpty(servicingDistrict)) {
                servicingDistrict = getDataDictionary().findStateReferenceCode(m_municipalitiesReferenceTableOid,
                        servicingDistrict);
            }

            if (StringUtils.isEmpty(servicingDistrict)) {
                servicingDistrict = entity.getFieldValue("Reporting_District");
            }

            return servicingDistrict;
        }
    }


    /**
     * Returns a summer exit reason (the state code equivalent of the Special Ed Exit Reason code)
     * if the IEP status code is PREVIOUS.
     *
     * risped-SummerExit
     *
     * @author Follett Software Company
     */
    protected class SummerExitReasonRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String result = "";

            IepData iep = (IepData) entity.getBean();

            // Determine if the student exited special ed during the summer
            if (iep.getStatusCodeEnum() == StatusCode.PREVIOUS) {
                PlainDate summerExitStartDate = (PlainDate) getParameter(SUMMER_EXIT_START_DATE);
                if (summerExitStartDate == null) {
                    summerExitStartDate = m_schoolYearLookupPreviousYear.getEndDate(iep.getStudent());
                }

                Student student = iep.getStudent();
                PlainDate firstSessionDate = m_schoolYearLookup.getStartDate(student);
                if (student.getSpedExitDate() != null && !student.getSpedExitDate().before(summerExitStartDate)
                        && !student.getSpedExitDate().after(firstSessionDate)) {
                    result = lookupStateValue(IepData.class, IepData.COL_EXIT_REASON, iep.getExitReason());
                }
            }

            return result;
        }
    }


    /**
     * Retrieves "Y" if the student's IEP contains transition services.
     *
     * @author Follett Software Company
     */
    protected class TransitionServicesRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_iepsWithTransitionServices.contains(entity.getBean().getOid()));
        }
    }


    /**
     * Retrieves "Y" if the IEP contains transportation services.
     *
     * risped-Transport
     *
     * @author Follett Software Company
     */
    protected class TransportationRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            return Boolean.valueOf(m_iepsWithTransportation.contains(entity.getBean().getOid()));
        }
    }


    /**
     * Returns the school code to use for each student record. For outplaced students, this is the
     * outside placement location
     * pulled from the most recent enrollment record. Outplaced students are identified by being in
     * an Aspen school with the
     * outside placement flag set, or a school with a state code ending in "90" (per RIDE
     * conventions).
     *
     * @author mmastrangelo
     */
    protected class SchoolCodeRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            IepData iep = (IepData) entity.getBean();
            Student student = iep.getStudent();
            School school = student.getSchool();

            String outsidePlacementSchoolFlag = (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
            String stateSchoolId = (String) school.getFieldValueByBeanPath(m_sklIdField);

            String outsidePlacementCode = stateSchoolId;

            if ("1".equals(outsidePlacementSchoolFlag) || (stateSchoolId != null && stateSchoolId.endsWith("90"))) {
                StudentEnrollment enrollment = m_latestEntries.get(student.getOid());
                if (enrollment != null) {
                    String outsidePlacementUserCode =
                            (String) enrollment.getFieldValueByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS);
                    outsidePlacementCode = lookupReferenceCodeByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS,
                            outsidePlacementUserCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return outsidePlacementCode;
        }
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * VALIDATORS
     * /* -----------------------------------------------------------------------------------------
     */


    /**
     * The Class ECProgramNameValidator.
     */
    protected class ECProgramNameValidator implements FieldValidator {

        private static final String VAL_ID = "risped-prName";
        private List<String> m_programNameRequire = Arrays.asList("01", "02", "03", "04", "05", "06", "07", "08", "09",
                "10", "11", "12", "13", "14");

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String icEnvironment = entity.getFieldValue(FIELD_EC_ENVIRONMENT);
            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(icEnvironment)
                    && m_programNameRequire.contains(icEnvironment)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3029,
                        "ECEnvironment = " + icEnvironment));
            }

            return errors;
        }
    }


    /**
     * validate iep start Date needed for before first July retriever.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class BeforeFirstJulyValidate implements FieldValidator {

        private static final String VAL_ID = "IepStartDate";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            IepData iepData = (IepData) entity.getBean();
            PlainDate plainDate = m_beforeFirstJulyRetriever.getIepStartDate(iepData);
            if (plainDate == null) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_CUSTOM1,
                        "Student " + iepData.getStudent().getNameView() + " Iep Status Code "
                                + iepData.getStatusCode()));
            }
            return errors;
        }

    }


    /**
     * The Class EITransitionValidator.
     */
    protected class EITransitionValidator implements FieldValidator {

        private static final String FIELD_TRANSITION_DELAY_OTH = "EITransitionDelayOth";
        private static final String EI_TRANSITION_REASON_OTHER = "OTHER";
        private static final String DELAY = "DELAY";

        private static final String FIELD_EARLY_INTERVENTION = "EITransition";
        private static final String VAL_ID = "risped-trDelay";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String eiTransition = entity.getFieldValue(FIELD_EARLY_INTERVENTION);
            if (StringUtils.isEmpty(value) && DELAY.equals(eiTransition)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3030,
                        FIELD_EARLY_INTERVENTION + " = " + eiTransition));
            }
            String eiTransitionDelayOther = entity.getFieldValue(FIELD_TRANSITION_DELAY_OTH);
            if (EI_TRANSITION_REASON_OTHER.equals(value) && StringUtils.isEmpty(eiTransitionDelayOther)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3031,
                        FIELD_TRANSITION_DELAY_OTH + " = " + eiTransitionDelayOther));
            }
            return errors;
        }
    }


    /**
     * Validates the following state validation rules related to the meeting date:
     * <ul>
     * <li>R2844
     * <li>R3001
     * </ul>
     * See validation rule constants for descriptions.
     *
     * @author Follett Software Company
     */
    protected class MeetingDateValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            IepData iep = (IepData) entity.getBean();
            String formattedMeetingDate = DateFormat.getDateInstance(DateFormat.SHORT).format(iep.getMeetingDate());

            if (!m_iepsWithMeetingAttendance.contains(iep.getOid())) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2844,
                        "Meeting Date = " + formattedMeetingDate));
            }

            if (iep.getMeetingDate().after(m_reportDate)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3001,
                        "Meeting Date = " + formattedMeetingDate + ", Report Date = "
                                + DateFormat.getDateInstance(DateFormat.SHORT).format(m_reportDate)));
            }

            return errors;
        }
    }


    /**
     * Validates the following state validation rules related to student age.
     * <ul>
     * <li>R2808
     * <li>R2801
     * <li>R2803
     * </ul>
     * See validation rule constants for descriptions.
     *
     * @author Follett Software Company
     */
    protected class StudentAgeValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            IepData iep = (IepData) entity.getBean();
            String gradeLevel =
                    lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, iep.getStudent().getGradeLevel());
            int age = iep.getStudent().getPerson().getAgeAsOfDate(m_reportDate);

            String formattedReportDate = DateFormat.getDateInstance(DateFormat.SHORT).format(m_reportDate);

            if (Arrays.asList("PK", "PF").contains(gradeLevel) && age >= 3 && age <= 5
                    && StringUtils.isBlank(entity.getFieldValue(FIELD_EC_ENVIRONMENT))) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2037, ""));
            }

            if (age < 3 || age > 21) {
                String summerExit = entity.getFieldValue("Summer_Exit");
                if (StringUtils.isEmpty(summerExit)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2808,
                            "Age as of " + formattedReportDate + " = " + age));
                }
            } else if (age <= 4) {
                if (!"PK".equals(gradeLevel) && !"PF".equals(gradeLevel)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2801,
                            "Age as of " + formattedReportDate + " = " + age + ", Grade = " + gradeLevel));
                }
            } else {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(m_reportDate);

                if (calendar.get(Calendar.MONTH) < Calendar.AUGUST) {
                    calendar.set(Calendar.MONTH, Calendar.DECEMBER);
                    calendar.set(Calendar.DAY_OF_MONTH, 1);
                    calendar.set(Calendar.YEAR, calendar.get(Calendar.YEAR) - 1);
                } else {
                    calendar.set(Calendar.MONTH, Calendar.DECEMBER);
                    calendar.set(Calendar.DAY_OF_MONTH, 1);
                }

                PlainDate dec1 = new PlainDate(calendar.getTime());
                int ageAsOfDec1 = iep.getStudent().getPerson().getAgeAsOfDate(dec1);

                if (ageAsOfDec1 >= 9) {
                    String primaryDisability = entity.getFieldValue("Primary_Disability");
                    if ("L".equals(primaryDisability)) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2803,
                                "Age as of " + DateFormat.getDateInstance(DateFormat.SHORT).format(dec1) + " = "
                                        + ageAsOfDec1 +
                                        ", Primary_Disability = " + primaryDisability));
                    }
                }
            }

            return errors;
        }
    }


    /**
     * Validates the following state validation rules related to the summer exit reason:
     * <ul>
     * <li>R2040
     * <li>R2879
     * <li>R3017
     * <li>R2837
     * </ul>
     * See validation rule constants for descriptions.
     *
     * @author Follett Software Company
     */
    protected class SummerExitValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            IepData iep = (IepData) entity.getBean();

            if (StringUtils.isEmpty(value)) {
                if (!m_iepsWithServices.contains(iep.getOid())) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2040, ""));
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2879, ""));
                }

                boolean white = "Y".equals(entity.getFieldValue("White"));
                boolean black = "Y".equals(entity.getFieldValue("Black"));
                boolean asian = "Y".equals(entity.getFieldValue("Asian"));
                boolean nativeAm = "Y".equals(entity.getFieldValue("Native"));
                boolean pacific = "Y".equals(entity.getFieldValue("Pacific"));

                if (!white && !black && !asian && !nativeAm && !pacific) {
                    String detail = "White = " + (white ? "Y" : "N") +
                            ", Black = " + (black ? "Y" : "N") +
                            ", Asian = " + (asian ? "Y" : "N") +
                            ", Native = " + (nativeAm ? "Y" : "N") +
                            ", Pacific = " + (pacific ? "Y" : "N");
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3017, detail));
                }
                boolean hispanic = entity.getFieldValue("White") == null;
                white = entity.getFieldValue("White") == null;
                black = entity.getFieldValue("Black") == null;
                asian = entity.getFieldValue("Asian") == null;
                nativeAm = entity.getFieldValue("Native") == null;
                pacific = entity.getFieldValue("Pacific") == null;

                if (hispanic || white || black || asian || nativeAm || pacific) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3016, ""));
                }
            } else if ("L".equals(value) || "G".equals(value)) {
                String gradeLevel =
                        lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, iep.getStudent().getGradeLevel());
                if (!"12".equals(gradeLevel)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2837,
                            "Grade = " + gradeLevel + ", Summer_Exit = " + value));
                }
            }

            return errors;
        }
    }


    /**
     * Validates the following state validation rules related to transition data:
     * <ul>
     * <li>R3000
     * </ul>
     * See validation rule constants for descriptions.
     *
     * @author Follett Software Company
     */
    protected class TransitionDataValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            if ("Y".equals(value)) {
                boolean assessmentTools = "Y".equals(entity.getFieldValue("Assessment_Tools"));
                boolean postSchoolGoals = "Y".equals(entity.getFieldValue("Post_School_Goals"));
                boolean transitionServices = "Y".equals(entity.getFieldValue("Transition_Services"));
                boolean assurance = "Y".equals(entity.getFieldValue("Assurance"));

                if (!assessmentTools || !postSchoolGoals || !transitionServices || !assurance) {
                    String detail = "Assessment_Tools = " + (assessmentTools ? "Y" : "N") +
                            ", Post_School_Goals = " + (postSchoolGoals ? "Y" : "N") +
                            ", Transition_Services = " + (transitionServices ? "Y" : "N") +
                            ", Assurance = " + (assurance ? "Y" : "N");
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3000, detail));
                }
            } else {
                IepData iep = (IepData) entity.getBean();
                PlainDate iepMettingDate = iep.getMeetingDate();
                if (iepMettingDate != null) {

                    int age = iep.getStudent().getPerson().getAgeAsOfDate(iepMettingDate);
                    if (age >= 14 && !m_iepsWithServices.contains(iep.getOid())) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2849, ""));
                    }
                }
            }
            return errors;
        }
    }


    /**
     * The Class ValidatorBeneficiary.
     */
    protected class ValidatorBeneficiary implements FieldValidator {

        private static final String VAL_ID = "val-beneficiary";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            IepData iep = (IepData) entity.getBean();
            String gradeLevel =
                    lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, iep.getStudent().getGradeLevel());

            if (!Arrays.asList("PK", "PF").contains(gradeLevel)
                    && Arrays.asList("28702", "07702", "07704").contains(entity.getFieldValue(FIELD_SKL_CODE))
                    && !"Y".equals(value)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2851,
                        "Field value is: " + value));
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorCareerDevPlan.
     */
    protected class ValidatorCareerDevPlan implements FieldValidator {

        private static final String VAL_ID = "careerDevPlan-val";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String dateOfCareerDevPlan = entity.getFieldValue(FIELD_DATE_CAREER_PLAN);
            if ("Y".equals(value) && StringUtils.isBlank(dateOfCareerDevPlan)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3027,
                        FIELD_DATE_CAREER_PLAN + ": " + dateOfCareerDevPlan + "."));
            }
            return errors;
        }
    }
    /**
     * The Class ValidatorCareerDevPlanDate.
     */
    protected class ValidatorCareerDevPlanDate implements FieldValidator {

        private static final String VAL_ID = "risped-val-CDPDate";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String endDateString = entity.getFieldValue("DateOfCareerDevPlan");
            if (!StringUtils.isEmpty(endDateString)) {
                try {
                    Date endDate = ((DateFormat) field.getFormatter()).parse(endDateString);
                    PlainDate oneYearAgo = DateUtils.add(new PlainDate(getTimeZone()), Calendar.YEAR, -1);
                    if (endDate != null && oneYearAgo != null && endDate.before(oneYearAgo)) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_CDP,
                                "Carrer End Date = " + DateFormat.getDateInstance(DateFormat.SHORT).format(endDate)
                                        + "; Today One Year Ago = "
                                        + DateFormat.getDateInstance(DateFormat.SHORT).format(oneYearAgo)));
                    }
                } catch (ParseException e) {
                    AppGlobals.getLog().log(Level.WARNING, "Date Parse Error", e);
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorChildOutcome1EntryDate.
     */
    protected class ValidatorChildOutcome1EntryDate implements FieldValidator {

        private static final String VAL_ID = "valOutEntryDate";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            IepData iep = ((IepData) entity.getBean());
            SisStudent student = iep.getStudent();
            String gradeLevel = lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, student.getGradeLevel());
            if ("PK".equals(gradeLevel) || "PF".equals(gradeLevel)) {
                String valueOutcomeEntryRating1 = entity.getFieldValue(FIELD_OUTCOME_ENTRY_RATING_1);
                String valueOutcomeEntryRating2 = entity.getFieldValue(FIELD_OUTCOME_ENTRY_RATING_2);
                String valueOutcomeEntryRating3 = entity.getFieldValue(FIELD_OUTCOME_ENTRY_RATING_3);
                if (StringUtils.isBlank(value) && StringUtils.isBlank(valueOutcomeEntryRating1)
                        && StringUtils.isBlank(valueOutcomeEntryRating2)
                        && StringUtils.isBlank(valueOutcomeEntryRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3035, ""));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorOutcome1EntryRating.
     */
    protected class ValidatorOutcome1EntryRating implements FieldValidator {

        private static final String VAL_ID = "valOut1EntryRating";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String valueOutcomeExitRating1 = entity.getFieldValue(FIELD_OUTCOME_EXIT_RATING_1);
            String valueOutcomeProgress1 = entity.getFieldValue(FIELD_OUTCOME_PROGRESS_1);

            if ("N".equals(valueOutcomeProgress1)) {
                if (Arrays.asList("7", "6").contains(value)
                        && Arrays.asList("7", "6").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3037, ""));
                } else if ("5".equals(value) && Arrays.asList("7", "6", "5").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3038, ""));
                } else if ("4".equals(value) && Arrays.asList("7", "6", "5", "4").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3039, ""));
                } else if ("3".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3040, ""));
                } else if ("2".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3041, ""));
                } else if ("1".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2", "1").contains(valueOutcomeExitRating1)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3042, ""));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorOutcome2EntryRating.
     */
    protected class ValidatorOutcome2EntryRating implements FieldValidator {

        private static final String VAL_ID = "valOut2EntryRating";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String valueOutcomeExitRating2 = entity.getFieldValue(FIELD_OUTCOME_EXIT_RATING_2);
            String valueOutcomeProgress2 = entity.getFieldValue(FIELD_OUTCOME_PROGRESS_2);

            if ("N".equals(valueOutcomeProgress2)) {
                if (Arrays.asList("7", "6").contains(value)
                        && Arrays.asList("7", "6").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3043, ""));
                } else if ("5".equals(value) && Arrays.asList("7", "6", "5").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3044, ""));
                } else if ("4".equals(value) && Arrays.asList("7", "6", "5", "4").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3045, ""));
                } else if ("3".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3046, ""));
                } else if ("2".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3047, ""));
                } else if ("1".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2", "1").contains(valueOutcomeExitRating2)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3048, ""));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorOutcome3EntryRating.
     */
    protected class ValidatorOutcome3EntryRating implements FieldValidator {

        private static final String VAL_ID = "valOut3EntryRating";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String valueOutcomeExitRating3 = entity.getFieldValue(FIELD_OUTCOME_EXIT_RATING_3);
            String valueOutcomeProgress3 = entity.getFieldValue(FIELD_OUTCOME_PROGRESS_3);

            if ("N".equals(valueOutcomeProgress3)) {
                if (Arrays.asList("7", "6").contains(value)
                        && Arrays.asList("7", "6").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3049, ""));
                } else if ("5".equals(value) && Arrays.asList("7", "6", "5").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3050, ""));
                } else if ("4".equals(value) && Arrays.asList("7", "6", "5", "4").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3051, ""));
                } else if ("3".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3052, ""));
                } else if ("2".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3053, ""));
                } else if ("1".equals(value)
                        && Arrays.asList("7", "6", "5", "4", "3", "2", "1").contains(valueOutcomeExitRating3)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3054, ""));
                }
            }
            return errors;
        }
    }

    protected class ValidatorPrimaryDisability implements FieldValidator {

        private static final String VAL_ID = "val-Prim-Disab";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            IepData iep = (IepData) entity.getBean();
            if ("B".equals(value)) {
                String careeDevPlan = entity.getFieldValue(FIELD_CAREER_DEV_PLAN);
                String vocAss = entity.getFieldValue(FIELD_VOC_ASS_PER_CENT_PLAN);
                String schBase = entity.getFieldValue(FIELD_SCH_BASED_PREP_EXP);
                String intTrial = entity.getFieldValue(FIELD_INT_TRIAL_WORK_EXP);
                if (!m_iepsWithServices.contains(iep.getOid()) && (!"Y".equals(careeDevPlan) || !"Y".equals(vocAss)
                        || !"Y".equals(schBase) || !"Y".equals(intTrial))) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3026,
                            "Field value is: " + value));
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidatorSchoolCode.
     */
    protected class ValidatorSchoolCode implements FieldValidator {

        private static final String VAL_ID = "School_Code-val";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            LinkedList<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            IepData iep = (IepData) entity.getBean();
            if (value.contains("990") && !m_iepsWithServices.contains(iep.getOid())
                    && !"Y".equals(entity.getFieldValue(FIELD_PRIVATE_SKL_ENROL))) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3022,
                        "Field value is: " + value));
            }
            return errors;
        }
    }
}
