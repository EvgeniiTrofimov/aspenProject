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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.SchoolYearDateRangeLookup;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;

/**
 * State report data module for the RI Special Education Services census. The base class for this
 * state report is IEP_SERVICE. Service records delivered between the start of the school year
 * and the report date are included in this report. Only students included in the Student Census
 * are included, based on the "In Last State Report" boolean alias on the IEP record. Therefore,
 * it is required that the Student Census be run prior to running this report.
 * <p>
 * For the students included in the student census, all services delivered between the start of the
 * school year (of the student's primary school) and the report date are included. This includes
 * services from previous or amended IEPs. Within this selection, services are eliminated and dates
 * are adjusted automatically if the "adjustServiceDates" parameter has been set. Adjustments are
 * made into the "service-adjusted-start-date" and "service-adjusted-end-date" aliases on the
 * service record. If the entire service is eliminated, the "service-in-last-state report" boolean
 * alias is set to false. The reason for automatic adjustment is saved in the
 * "service-adjustment-reason" alias. This allows the adjustments to be viewed on the IEP_SERVICE
 * table after running this report. The "Service Census Audit" report can also be run for a single
 * student and provides a convenient format for viewing this information.
 * <p>
 * The following automatic adjustments are made:
 * <ul>
 * <li>Overlapping date ranges between services in previous and active IEPs are adjusted. Services
 * in the older IEPs are set to end one day before the start date of the newer IEP.
 * <li>Overlapping date ranges between the same service in an amended and new IEP are eliminated.
 * The amended IEP service is adjusted so it does not conflict with the new IEP service. Note that
 * this requires the service to be paired between the amended and the new IEP. If this cannot be
 * done accurately, a validation error is generated. (This is the case if two services of the same
 * type exist, for example.)
 * <li>Services whose dates fall outside of the school year are adjusted to fit within the
 * boundaries of the year.
 * <li>Future service end dates are reported as blank.
 * </ul>
 * <p>
 * A number of custom field retrievers and validators are used; see javadoc on the corresponding
 * inner classes for details. If the bypassStateRules input parameter is set, custom validators
 * are bypassed.
 *
 * @author mmastrangelo
 */
public class RISpecialEducationServices extends StateReportData {
    // Input Parameters ---------------------------------------------------------------------------
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String BYPASS_STATE_VALIDATION_RULES = "bypassStateRules";
    public static final String ADJUST_SERVICE_DATES_PARAM = "adjustServiceDates";
    public static final String SENIOR_GRAD_DATE = "seniorGraduationDate";
    public static final String SUMMER_EXIT_START_DATE = "summerExitStartDate";

    // Field Aliases ------------------------------------------------------------------------------
    protected static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";
    protected static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";
    protected static final String ALTERNATE_ASSESSMENT_PARTICIPATION_ALIAS = "riaa-participate";
    protected static final String NON_RESIDENT_DISTRICT_ALIAS = "Non Resident District Code";
    protected static final String ADJUSTED_START_DATE_ALIAS = "service-adjusted-start-date";
    protected static final String ADJUSTED_END_DATE_ALIAS = "service-adjusted-end-date";
    protected static final String SERVICE_IN_LAST_STATE_REPORT_ALIAS = "service-in-last-state-report";
    protected static final String SERVICE_ADJUSTMENT_REASON_ALIAS = "service-adjustment-reason";
    protected static final String SERVICE_DESCRIPTION_ALIAS = "service-description";
    protected static final String SERVICE_END_REASON_ALIAS = "service-end-reason";
    protected static final String PRIVATE_SCHOOL_ENROLLEE_IEP_ALIAS = "private-school-enrollee";
    protected static final String BENEFICIARY_IEP_ALIAS = "beneficiary";
    protected static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outside Placement School";
    protected static final String STATE_SCHOOL_ID_ALIAS = "State School Id";
    protected static final String MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS = "OP Location Code";
    protected static final String SERVICE_SCHOOL_CODE_OVERRIDE_ALIAS = "service-school-code";

    // State Code Constants -----------------------------------------------------------------------
    protected static final String ENR_TYPE_GRADUATE = "Graduate";
    protected static final String RELATED_SERVICE_OTHER_CODE = "Q";
    protected static final String REASSESSMENT_INDICATED_SERVICE_END_REASON = "E";

    private static final String ERROR_INVALID_VALUE_KEY = "error.state.report.invalidvalue";
    private static final String ERROR_LONG_VALUE_KEY = "error.state.report.longvalue";
    private static final String ERROR_MISSING_VALUE_KEY = "error.state.report.missingvalue";
    private static final String ERROR_SHORT_VALUE_KEY = "error.state.report.shortvalue";

    /**
     * Other constants
     */
    protected static final String FIELD_ID_SERV_END = "Service_Record_End";
    protected static final String FIELD_ID_SERV_START = "Service_Record_Start";

    // State Validation Rules ---------------------------------------------------------------------
    private static final String STATE_VALIDATION_R2860 =
            "R2860 - When Service_program_continuum = 1,  Service_Cost_Center must be 32202 or 32221 or 32222 or 32223.";
    private static final String STATE_VALIDATION_R2862 =
            "R2862 - When Service_program_continuum = 2,  Service_Cost_Center must be 32203 or 32204 or 32205 or 32206 or 32218 or 32219.";
    private static final String STATE_VALIDATION_R2865 =
            "R2865 - When Service_program_continuum = 4,  Service_Cost_Center must be 32207.";
    private static final String STATE_VALIDATION_R2866 =
            "R2866 - When Service_program_continuum = 5,  Service_Cost_Center must be 32213 or 32214.";
    private static final String STATE_VALIDATION_R3002 =
            "R3002 - When Service_program_continuum = 6,  Service_Cost_Center must be 32211 or 32212.";
    private static final String STATE_VALIDATION_R2869 =
            "R2869 - When Service_program_continuum = 7,  Service_Cost_Center must be 32215.";
    private static final String STATE_VALIDATION_R2871 =
            "R2871 - When Service_program_continuum = 0,  Service_Cost_Center must be 90000.";
    private static final String STATE_VALIDATION_R2863 =
            "R2863 - When Service_program_continuum = 3 and Beneficiary=Y,  Service_Cost_Center must be 32210 or 32217.";
    private static final String STATE_VALIDATION_R2864 =
            "R2864 - When Service_program_continuum = 3 and Beneficiary=N,  Service_Cost_Center must be 32208 or 32209.";
    private static final String STATE_VALIDATION_R2867 =
            "R2867 - When Service_program_continuum = 6 and Beneficiary=Y,  Service_Cost_Center must be 32211.";
    private static final String STATE_VALIDATION_R2868 =
            "R2868 - When Service_program_continuum = 6 and Beneficiary=N,  Service_Cost_Center must be 32212.";
    private static final String STATE_VALIDATION_R2880 =
            "R2880 - If Service_Type='RelServ' or Service_Type='SpEd' then Service_Program_Continuum must be 1, 2, 3, 4, 5, 6, 7, or 0.";
    private static final String STATE_VALIDATION_R2850 =
            "R2850 - Students in grades PK/PF must have Service_Program_Continuum = 5, 0, F, J, K or Z.";
    private static final String STATE_VALIDATION_R2809 = "Program Code 5 being valid for PK/PF students";
    private static final String STATE_VALIDATION_R2861 =
            "R2861 - When Service_program_continuum = 1,  Private_School_Enrollee must be N.";
    private static final String STATE_VALIDATION_R2872 =
            "R2872 - When Service_program_continuum = 0, 7, or Z,  Private_School_Enrollee must be Y.";
    private static final String STATE_VALIDATION_R2038 =
            "R2038 - If ending date is entered, then an exit reason is required";
    private static final String STATE_VALIDATION_R2903 = "R2903 - Ending Date cannot be less than the Starting Date";
    private static final String STATE_VALIDATION_R2402 = "Services hours per day > 6";
    private static final String STATE_VALIDATION_R2403 = "Service days per week > 5";
    private static final String STATE_VALIDATION_R2404 = "Service weeks per month > 4";
    private static final String STATE_VALIDATION_R2873 =
            "R2873 - if Service_Type='SpEd' then Service_Provider must be C, G, W, X or Y";
    private static final String STATE_VALIDATION_R2874 =
            "R2874 - if Service_Type='RelServ' then Service_Provider must be C, D, E, F, I, J, N, O, Q, S, Z, 1, 2, 3, 4, 5 or 6";
    private static final String STATE_VALIDATION_R2876 =
            "R2876 - if Service_Type='RelServ' then Related_Service is required";
    private static final String STATE_VALIDATION_R2877 =
            "R2877 - if Related_Service='RelServ' and Related_Service='Q' then Related_Service_Other is required";

    protected boolean m_bypassStateRules;
    protected DistrictSchoolYearContext m_currentSchoolYear;
    protected SchoolYearDateRangeLookup m_schoolYearLookup;
    protected SchoolYearDateRangeLookup m_schoolYearLookupPreviousYear;
    protected PlainDate m_reportDate;
    protected PlainDate m_seniorGradeDate = null;
    protected Set<String> m_amendmendOverlapErrors = new HashSet<>();
    protected Set<String> m_previousIepOverlapErrors = new HashSet<>();
    protected Set<String> m_schoolYearDateErrors = new HashSet<>();
    protected Map<String, StudentEnrollment> m_latestEntries = new HashMap<>();

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
        m_bypassStateRules = Boolean.TRUE.equals(getParameter(BYPASS_STATE_VALIDATION_RULES));
        m_seniorGradeDate = (PlainDate) getParameter(SENIOR_GRAD_DATE);

        /*
         * Retrieve the current and previous school years;
         * Calculate the current year first in-session date and previous year last in-session date
         */
        loadCalendarInfo();
        if (getSetupErrors().size() != 0) {
            return;
        }

        ModelBroker modelBroker = new ModelBroker(getPrivilegeSet());
        resetServicesAdjustmentFields(modelBroker);

        boolean adjustServiceDates = Boolean.TRUE.equals(getParameter(ADJUST_SERVICE_DATES_PARAM));
        if (adjustServiceDates) {
            adjustPreviousIepServices(modelBroker);
            adjustAmendedIepServices(modelBroker);
            adjustServicesOutsideSchoolYear(modelBroker);
            cleanupInconsistentDates(modelBroker);
        }

        Criteria serviceCriteria = new Criteria();
        serviceCriteria.addEqualTo(translateAliasToJavaName(SERVICE_IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);
        X2Criteria datesAndCriteria = new X2Criteria();
        datesAndCriteria.addEmpty(IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE,
                getBroker().getPersistenceKey());
        X2Criteria datesOrCriteria = new X2Criteria();
        datesOrCriteria.addGreaterOrEqualThan(
                IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE,
                m_currentSchoolYear.getStartDate());
        datesAndCriteria.addOrCriteria(datesOrCriteria);
        serviceCriteria.addAndCriteria(datesAndCriteria);

        X2Criteria dateFieldsCriteria = new X2Criteria();
        dateFieldsCriteria.addEmpty(IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE,
                getBroker().getPersistenceKey());
        X2Criteria dateOrFieldsCriteria = new X2Criteria();
        dateOrFieldsCriteria.addLessOrEqualThanField(IepService.COL_START_DATE,
                IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE);
        dateFieldsCriteria.addOrCriteria(dateOrFieldsCriteria);
        serviceCriteria.addAndCriteria(dateFieldsCriteria);
        applySeniorGraduateCriteria(serviceCriteria);
        setQuery(new BeanQuery(IepService.class, serviceCriteria));
        setEntityClass(RISpecialEducationServicesEntity.class);

        /*
         * Load a map of the latest entry enrollment records on or before the report date
         */
        loadLatestEntriesLookup();

        /*
         * Add field retrievers to support calculated columns
         */
        addCustomCalcs();

        /*
         * Add field validators to support custom validation rules
         */
        addCustomValidators();
    }

    /**
     * Convenience method used to determine if the passed code equals one of the codes provided in
     * the list of codes.
     *
     * @param code String
     * @param codes String[]
     * @return boolean
     */
    protected boolean isInList(String code, String... codes) {
        boolean inList = false;

        for (String aCode : codes) {
            if (code.equals(aCode)) {
                inList = true;
                break;
            }
        }

        return inList;
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("risped-RelatedSvcOth", new OtherRelatedServiceRetriever());
        calcRetrievers.put("risped-SvcEndReason", new ServiceEndReasonRetriever());
        calcRetrievers.put("risped-SvcEndDate", new ServiceEndDateRetriever());
        calcRetrievers.put("risped-SchoolCode", new SchoolCodeRetriever());

        addCalcs(calcRetrievers);
    }

    /**
     * Add custom validators.
     */
    private void addCustomValidators() {
        if (!m_bypassStateRules) {
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();

            validators.put(CostCenterValidator.VAL_ID, new CostCenterValidator());
            validators.put(EndDateValidator.VAL_ID, new EndDateValidator());
            validators.put(ProgramContinuumValidator.VAL_ID, new ProgramContinuumValidator());
            validators.put(HoursValidator.VAL_ID, new HoursValidator());
            validators.put(DaysValidator.VAL_ID, new DaysValidator());
            validators.put(WeeksValidator.VAL_ID, new WeeksValidator());
            validators.put(ProviderValidator.VAL_ID, new ProviderValidator());
            validators.put(RelatedServiceValidator.VAL_ID, new RelatedServiceValidator());
            validators.put(RelatedServiceOtherValidator.VAL_ID, new RelatedServiceOtherValidator());
            validators.put(ServiceTypeValidator.VAL_ID, new ServiceTypeValidator());
            validators.put(ServicePlanDistValidator.VAL_ID, new ServicePlanDistValidator());

            addValidators(validators);
        }
    }

    /**
     * Adjusts amended IEP services. This method uses the AmendedServiceOverlapHandler inner class
     * to make the adjustments.
     *
     * @param modelBroker ModelBroker
     */
    private void adjustAmendedIepServices(ModelBroker modelBroker) {
        m_amendmendOverlapErrors = new HashSet<String>(256);

        Criteria reportCriteria = buildServiceCriteria(new IepData.StatusCode[] {IepData.StatusCode.ACTIVE,
                IepData.StatusCode.PREVIOUS, IepData.StatusCode.AMENDED});

        BeanQuery query = new BeanQuery(IepService.class, reportCriteria);
        query.addOrderByAscending(IepService.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID);
        query.addOrderByAscending(IepService.COL_SERVICE_TYPE);
        query.addOrderByAscending(translateAliasToJavaName(SERVICE_DESCRIPTION_ALIAS, true));
        query.addOrderByDescending(IepService.REL_IEP_DATA + "." + IepData.COL_STATUS_CODE);
        query.addOrderByDescending(IepService.COL_START_DATE);

        QueryIterator ieps = getBroker().getIteratorByQuery(query);
        try {
            String lastStudentOid = "";
            String lastServiceType = "";

            AmendedServiceOverlapHandler handler = null;

            /*
             * Iterate over IEPs ordered by student and servicetype. When the student or service
             * changes, create a new overlap handler. The overlap handler is designed to work on
             * a set of the same services for a student, privded in descending status code and start
             * date order.
             */
            int numberAdjusted = 0;
            while (ieps.hasNext()) {
                IepService service = (IepService) ieps.next();


                String studentOid = service.getIepData().getStudentOid();
                String serviceType = service.getServiceType();
                if ("Special Education".equals(service.getServiceMode())) {
                    serviceType = (String) service.getFieldValueByAlias(SERVICE_DESCRIPTION_ALIAS, getDataDictionary());
                }

                if (!StringUtils.isEmpty(studentOid) && !StringUtils.isEmpty(serviceType)) {
                    if (!studentOid.equals(lastStudentOid) || !serviceType.equals(lastServiceType)) {
                        if (handler != null) {
                            numberAdjusted += handler.m_numberAdusted;
                        }

                        PlainDate firstSessionDate = m_schoolYearLookup.getStartDate(service.getIepData().getStudent());
                        handler = new AmendedServiceOverlapHandler(modelBroker, firstSessionDate, getDataDictionary(),
                                true);
                    }

                    int result = handler.addService(service);
                    if (result != AmendedServiceOverlapHandler.SUCCESS) {
                        m_amendmendOverlapErrors.add(service.getOid());
                    }
                }
                lastStudentOid = studentOid;
                lastServiceType = serviceType;

            }

            AppGlobals.getLog()
                    .info("RI Sped Census Services - Adjusted " + numberAdjusted + " amended service records");
        } finally {
            ieps.close();
        }
    }

    /**
     * Adjusts previous IEP services to eliminate overlaps. This method uses the
     * PreviousServiceEndDateAdjuster inner class to make the adjustments.
     *
     * @param broker ModelBroker
     */
    private void adjustPreviousIepServices(ModelBroker broker) {
        Criteria previousServiceCriteria = buildServiceCriteria(new IepData.StatusCode[] {IepData.StatusCode.PREVIOUS});

        BeanQuery previousServiceQuery = new BeanQuery(IepService.class, previousServiceCriteria);
        Map<String, Collection<IepService>> previousServiceLookup =
                getBroker().getGroupedCollectionByQuery(previousServiceQuery, IepService.COL_IEP_DATA_OID, 1024);

        Criteria allServicesCriteria =
                buildServiceCriteria(new IepData.StatusCode[] {IepData.StatusCode.PREVIOUS, IepData.StatusCode.ACTIVE});

        Criteria iepCriteria = new Criteria();
        iepCriteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, allServicesCriteria));

        BeanQuery iepQuery = new BeanQuery(IepData.class, iepCriteria);
        iepQuery.addOrderByAscending(IepData.COL_STUDENT_OID);
        iepQuery.addOrderByDescending(IepData.COL_STATUS_CODE);
        iepQuery.addOrderByAscending(IepData.COL_START_DATE);

        QueryIterator ieps = getBroker().getIteratorByQuery(iepQuery);
        try {
            PreviousServiceEndDateAdjuster adjuster =
                    new PreviousServiceEndDateAdjuster(broker, getDataDictionary(), true);
            m_previousIepOverlapErrors = adjuster.adjust(previousServiceLookup, m_schoolYearLookup, ieps);
            AppGlobals.getLog().info(
                    "RI Sped Census Services - Adjusted " + adjuster.m_numberAdjusted + " previous service records");
        } finally {
            ieps.close();
        }
    }

    /**
     * Adjusts service dates outside the school year. This method uses the
     * OutsideYearServiceAdjuster inner class to make the changes.
     *
     * @param broker ModelBroker
     */
    private void adjustServicesOutsideSchoolYear(ModelBroker broker) {
        Criteria reportCriteria = buildServiceCriteria(new IepData.StatusCode[] {IepData.StatusCode.ACTIVE,
                IepData.StatusCode.AMENDED, IepData.StatusCode.PREVIOUS});

        Criteria outsideDateCriteria = new Criteria();
        outsideDateCriteria.addLessThan(IepService.COL_START_DATE, m_schoolYearLookup.getLatestStartDate());

        Criteria outsideDateCriteriaOr = new Criteria();
        outsideDateCriteriaOr.addGreaterThan(IepService.COL_END_DATE, m_schoolYearLookup.getEarliestEndDate());

        outsideDateCriteria.addOrCriteria(outsideDateCriteriaOr);

        reportCriteria.addAndCriteria(outsideDateCriteria);
        // reportCriteria.addEqualTo(IepService.COL_EXTENDED_YEAR_INDICATOR, Boolean.FALSE);

        BeanQuery serviceQuery = new BeanQuery(IepService.class, reportCriteria);
        QueryIterator services = getBroker().getIteratorByQuery(serviceQuery);
        try {
            OutsideYearServiceAdjuster adjuster = new OutsideYearServiceAdjuster(broker, getDataDictionary(), true);
            m_schoolYearDateErrors = adjuster.adjust(services, m_schoolYearLookup, m_currentSchoolYear);
            AppGlobals.getLog().info("RI Sped Census Services - Adjusted " + adjuster.m_numberAdjusted
                    + " service records that were outside the school year");
        } finally {
            services.close();
        }
    }

    /**
     * Apply senior graduate criteria.
     *
     * @param criteria Criteria
     */
    private void applySeniorGraduateCriteria(Criteria criteria) {
        if (m_seniorGradeDate != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, ENR_TYPE_GRADUATE);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_DATE, m_seniorGradeDate);
            criteria.addIn(IepService.COL_STUDENT_OID,
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria));
        }

    }

    /**
     * Cleanup inconsistent dates.
     *
     * @param modelBroker ModelBroker
     */
    private void cleanupInconsistentDates(ModelBroker modelBroker) {
        SystemStringConverter dateStringConverter = (SystemStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
        SystemStringConverter booleanStringConverter = (SystemStringConverter) ConverterFactory
                .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);

        Criteria serviceCriteria = new Criteria();
        serviceCriteria.addEqualTo(translateAliasToJavaName(SERVICE_IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);

        BeanQuery serviceQuery = new BeanQuery(IepService.class, serviceCriteria);

        QueryIterator services = modelBroker.getIteratorByQuery(serviceQuery);
        try {
            while (services.hasNext()) {
                IepService service = (IepService) services.next();

                PlainDate effectiveStartDate = service.getStartDate();
                String adjustedStartDateString =
                        (String) service.getFieldValueByAlias(ADJUSTED_START_DATE_ALIAS, getDataDictionary());
                if (!StringUtils.isEmpty(adjustedStartDateString)) {
                    effectiveStartDate = (PlainDate) dateStringConverter.parseSystemString(adjustedStartDateString);
                }

                PlainDate effectiveEndDate = service.getEndDate();
                String adjustedEndDateString =
                        (String) service.getFieldValueByAlias(ADJUSTED_END_DATE_ALIAS, getDataDictionary());
                if (!StringUtils.isEmpty(adjustedEndDateString)) {
                    effectiveEndDate = (PlainDate) dateStringConverter.parseSystemString(adjustedEndDateString);
                }

                if (effectiveEndDate.before(effectiveStartDate)) {
                    service.setFieldValueByAlias(SERVICE_IN_LAST_STATE_REPORT_ALIAS,
                            booleanStringConverter.getSystemString(Boolean.FALSE), getDataDictionary());
                    modelBroker.saveBeanForced(service);
                }
            }
        } finally {
            services.close();
        }

    }

    /**
     * Returns the criteria for the IEPs to include. All services for students included in the
     * student census, based on the IN_LAST_STATE_REPORT_ALIAS on the IEP are included. Services
     * falling within the school year starting before the report date are included.
     *
     * @param statusCodes StatusCode[]
     * @return Criteria - IEPs with the IN_LAST_STATE_REPORT_ALIAS field set to true
     */
    private Criteria buildServiceCriteria(IepData.StatusCode[] statusCodes) {
        Criteria subqueryCriteria = new Criteria();
        subqueryCriteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);

        SubQuery subQuery = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, subqueryCriteria);

        /*
         * ** The criteria below generate the following WHERE clause **
         *
         * WHERE
         * (
         * (
         * (
         * (
         * ( A0.ISV_START_DATE <= [report date])
         * AND
         * (
         * ((A0.ISV_START_DATE >= [school year start]) AND A0.ISV_START_DATE <= [school year end])
         * OR
         * ((A0.ISV_END_DATE >= [school year start]) AND A0.ISV_END_DATE <= [school year end])
         * OR
         * ((A0.ISV_START_DATE < [school year start]) AND A0.ISV_END_DATE > [school year end])
         * )
         * )
         * )
         * AND
         * (A0.ISV_SERVICE_MODE IN ('Related Services','Special Education'))
         * )
         * AND
         * (A1.IEP_STATUS IN ([list of passed status codes]))
         * )
         * AND
         * A1.IEP_STD_OID IN (SELECT B0.IEP_STD_OID FROM IEP_DATA B0 WHERE B0.IEP_FIELDA_053 = '1')
         * // IEP included in last state report alias
         */

        // ------

        Criteria schoolYearCriteria = new Criteria();

        Criteria dateOrCriteria1 = new Criteria(); // Start date falls within the school year
        dateOrCriteria1.addGreaterOrEqualThan(IepService.COL_START_DATE, m_currentSchoolYear.getStartDate());
        dateOrCriteria1.addLessOrEqualThan(IepService.COL_START_DATE, m_currentSchoolYear.getEndDate());
        dateOrCriteria1.setEmbraced(true);

        Criteria dateOrCriteria2 = new Criteria(); // End date falls within the school year
        dateOrCriteria2.addGreaterOrEqualThan(IepService.COL_END_DATE, m_currentSchoolYear.getStartDate());
        dateOrCriteria2.addLessOrEqualThan(IepService.COL_END_DATE, m_currentSchoolYear.getEndDate());
        dateOrCriteria2.setEmbraced(true);

        Criteria dateOrCriteria3 = new Criteria(); // Start date is before the start of the year and
                                                   // end date is after the end of the year
        dateOrCriteria3.addLessThan(IepService.COL_START_DATE, m_currentSchoolYear.getStartDate());
        dateOrCriteria3.addGreaterThan(IepService.COL_END_DATE, m_currentSchoolYear.getEndDate());
        dateOrCriteria3.setEmbraced(true);

        schoolYearCriteria.addOrCriteria(dateOrCriteria1);
        schoolYearCriteria.addOrCriteria(dateOrCriteria2);
        schoolYearCriteria.addOrCriteria(dateOrCriteria3);

        // -----

        Criteria dateCriteria = new Criteria();
        dateCriteria.addLessOrEqualThan(IepService.COL_START_DATE, m_reportDate);
        dateCriteria.addAndCriteria(schoolYearCriteria);

        // -----

        X2Criteria reportCriteria = new X2Criteria();
        reportCriteria.addAndCriteria(dateCriteria);
        reportCriteria.addIn(IepService.COL_SERVICE_MODE,
                Arrays.asList(new String[] {"Related Services", "Special Education"}));
        reportCriteria.addNotEqualTo(IepService.COL_SERVICE_TYPE, "Transportation");

        ArrayList statusCodeValues = new ArrayList(statusCodes.length);
        for (int i = 0; i < statusCodes.length; i++) {
            statusCodeValues.add(Integer.valueOf(statusCodes[i].ordinal()));
        }
        reportCriteria.addIn(IepService.REL_IEP_DATA + "." + IepData.COL_STATUS_CODE, statusCodeValues);

        reportCriteria.addIn(IepService.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID, subQuery);

        X2Criteria dateFieldsCriteria = new X2Criteria();
        dateFieldsCriteria.addEmpty(IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE,
                getBroker().getPersistenceKey());
        X2Criteria dateOrFieldsCriteria = new X2Criteria();
        dateOrFieldsCriteria.addLessOrEqualThanField(IepService.COL_START_DATE,
                IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_EXIT_DATE);
        dateFieldsCriteria.addOrCriteria(dateOrFieldsCriteria);
        reportCriteria.addAndCriteria(dateFieldsCriteria);
        applySeniorGraduateCriteria(reportCriteria);
        return reportCriteria;
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
     * Loads the enrollment lookup map - latest 'E' records on or prior to the report date.
     */
    private void loadLatestEntriesLookup() {
        Criteria iepCriteria = new Criteria();
        iepCriteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);

        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID,
                new SubQuery(IepData.class, IepData.COL_STUDENT_OID, iepCriteria));

        BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE); // ensures that
                                                                                    // the record
                                                                                    // with the max
                                                                                    // date will end
                                                                                    // up in the map

        m_latestEntries = getBroker().getMapByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);
    }

    /**
     * Initializes the SERVICE_IN_LAST_STATE_REPORT_ALIAS, which identifies the services included
     * in the census after all querying and adjustments are made.
     *
     * @param modelBroker ModelBroker
     */
    private void resetServicesAdjustmentFields(ModelBroker modelBroker) {
        // First clear the "in last state report" flag for ALL services
        Criteria criteria = new Criteria();
        criteria.addNotNull(translateAliasToJavaName(SERVICE_IN_LAST_STATE_REPORT_ALIAS, true));

        HashMap<String, Object> updateMap = new HashMap<String, Object>(3);
        updateMap.put(translateAliasToJavaName(SERVICE_IN_LAST_STATE_REPORT_ALIAS, true), null);

        UpdateQuery update = new UpdateQuery(IepService.class, criteria, updateMap);
        int affectedRecords = modelBroker.executeUpdateQuery(update);

        AppGlobals.getLog().info(
                "RI Sped Census Services - Cleared state report flag for " + affectedRecords + " service records");

        // For services falling within the report's date range, clear the adjusted dates and
        // initialize the "in last state report" flag to TRUe
        Criteria reportCriteria = buildServiceCriteria(new IepData.StatusCode[] {IepData.StatusCode.ACTIVE,
                IepData.StatusCode.AMENDED, IepData.StatusCode.PREVIOUS});

        updateMap = new HashMap<String, Object>(3);
        updateMap.put(translateAliasToJavaName(ADJUSTED_START_DATE_ALIAS, true), null);
        updateMap.put(translateAliasToJavaName(ADJUSTED_END_DATE_ALIAS, true), null);
        updateMap.put(translateAliasToJavaName(SERVICE_IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);
        updateMap.put(translateAliasToJavaName(SERVICE_ADJUSTMENT_REASON_ALIAS, true), null);

        update = new UpdateQuery(IepService.class, reportCriteria, updateMap);
        affectedRecords = modelBroker.executeUpdateQuery(update);

        AppGlobals.getLog().info("RI Sped Census Services - Reinitialized " + affectedRecords + " service records");
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * ENTITY CLASS
     * /* -----------------------------------------------------------------------------------------
     */

    /**
     * Custom entity class for this state report that eliminates services for summer exit students.
     *
     * @author mmastrangelo
     */
    public static class RISpecialEducationServicesEntity extends StateReportEntity {
        private StateReportData m_data = null;
        private SimpleDateFormat m_formatter = new SimpleDateFormat("MM/dd/yyyy");

        /**
         * Filter miles can not be ZERO or EMPTY.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            FieldDefinition field = getData().getFieldDefinition(FIELD_ID_SERV_START);
            String startDateValue = getFieldValue(FIELD_ID_SERV_START);
            String endDateValue = getFieldValue(FIELD_ID_SERV_END);
            if (!StringUtils.isEmpty(endDateValue)) {
                Date startDate = null;
                try {
                    startDate = m_formatter.parse(startDateValue);
                } catch (ParseException e) {
                    // DO nothing
                }
                Date endDate = null;
                try {
                    endDate = m_formatter.parse(endDateValue);
                } catch (ParseException e) {
                    // Do nothing
                }
                if (startDate != null && endDate != null && !startDate.equals(endDate)
                        && startDate.after(endDate)) {
                    error = new StateReportValidationError(this, field,
                            "Start date = " + startDate + " can't be after exit date = " + endDateValue,
                            "Entity was not reported.");
                }
            }
            return error;
        }

        /**
         * Returns any field validations that occurred for a given field.
         * Also include retrieval errors, errors that occur trying to
         * look up the value from the database.
         *
         * @param index int
         * @return a collection of validation errors
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(int index) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            // Retrieve the value. If it has not been retrieved already,
            // this will retrieve it and set any retrieval errors.
            String value = getFieldValue(index);

            // Get the definition and check all validation parameters there.
            FieldDefinition field = m_data.getFieldDefinition(index);
            if (field != null) {
                // Check value existence and size.
                if (StringUtils.isEmpty(value) && field.getMinLength() > 0) {
                    String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_MISSING_VALUE_KEY);
                    StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                    errors.add(error);
                } else if (value.length() < field.getMinLength()) {
                    String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_SHORT_VALUE_KEY);
                    String details = message + " (Must be " + field.getMinLength() + " characters)";
                    StateReportValidationError error = new StateReportValidationError(this, field, details, value);
                    errors.add(error);
                } else if (value.length() > field.getMaxLength()) {
                    String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_LONG_VALUE_KEY);
                    String details = message + " (Exceeds " + field.getMaxLength() + " characters)";
                    StateReportValidationError error = new StateReportValidationError(this, field, details, value);
                    errors.add(error);
                } else {
                    // Check regex pattern match.
                    if (field.getPattern() != null && !value.matches(field.getPattern())) {
                        String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                                .getMessage(ERROR_INVALID_VALUE_KEY);
                        StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                        errors.add(error);
                    }
                    if (field.getValidator() != null) {
                        // Check custom validator.
                        errors.addAll(field.getValidator().getFieldValidation(m_data, this, field, value));
                    }
                }
            }
            return errors;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_data = data;

            IepService service = (IepService) bean;
            IepData iep = service.getIepData();

            RISpecialEducationServices servicesReportData = (RISpecialEducationServices) data;

            // Determine if the student exited special ed during the summer
            if (iep.getStatusCodeEnum() == StatusCode.PREVIOUS) {
                Student student = iep.getStudent();

                PlainDate summerExitStartDate = (PlainDate) data.getParameter(SUMMER_EXIT_START_DATE);
                if (summerExitStartDate == null) {
                    summerExitStartDate = servicesReportData.m_schoolYearLookupPreviousYear.getEndDate(student);
                }

                PlainDate firstSessionDate = servicesReportData.m_schoolYearLookup.getStartDate(student);
                if (student.getSpedExitDate() != null && !student.getSpedExitDate().before(summerExitStartDate)
                        && !student.getSpedExitDate().after(firstSessionDate)) {
                    String summerExitCode =
                            data.lookupStateValue(IepData.class, IepData.COL_EXIT_REASON, iep.getExitReason());
                    if (!StringUtils.isEmpty(summerExitCode)) {
                        setRowCount(0);
                    }
                }
            }
        }
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * Date Adjusters
     * /* -----------------------------------------------------------------------------------------
     */

    /**
     * Parent class for all adjuster inner classes defined in this class.
     *
     * @author Follett Software Company
     */
    protected static class BaseServiceAdjuster {
        protected DataDictionary m_dictionary;

        /**
         * Constructs a new BaseServiceAdjuster.
         *
         * @param dictionary DataDictionary
         */
        protected BaseServiceAdjuster(DataDictionary dictionary) {
            m_dictionary = dictionary;
        }

        /**
         * Adds an adjustment reason to the service.
         *
         * @param reason String
         * @param service IepService
         */
        protected void addAdjustmentReason(String reason, IepService service) {
            String reasons = "";
            String existingReason =
                    (String) service.getFieldValueByAlias(SERVICE_ADJUSTMENT_REASON_ALIAS, m_dictionary);

            if (existingReason == null || !existingReason.contains("ERROR")) {
                if (!StringUtils.isEmpty(existingReason) && !reason.contains(("ERROR"))) {
                    reasons = existingReason + "; " + reason;
                } else {
                    reasons = reason;
                }

                if (reasons.length() > 50) {
                    reasons = reasons.substring(0, 50);
                }

                service.setFieldValueByAlias(SERVICE_ADJUSTMENT_REASON_ALIAS,
                        reasons, m_dictionary);
            }
        }
    }

    /**
     * Adjuster that forces the start and end dates of a service within the school year if
     * necessary.
     *
     * @author Follett Software Company
     */
    protected static class OutsideYearServiceAdjuster extends BaseServiceAdjuster {
        private boolean m_autoAdjust = false;
        private SystemStringConverter m_dateStringConverter;
        private X2Broker m_broker;

        protected int m_numberAdjusted = 0;

        /**
         * Constructs a new OutsideYearServiceAdjuster.
         *
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param autoAdjust boolean
         */
        protected OutsideYearServiceAdjuster(X2Broker broker, DataDictionary dictionary, boolean autoAdjust) {
            super(dictionary);

            m_broker = broker;
            m_autoAdjust = autoAdjust;

            m_dateStringConverter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
        }

        /**
         * Adjusts the services in the passed iterator. School year start and end dates are
         * determined from the passed SchoolYearDateRangeLookup.
         *
         * @param services Iterator
         * @param dateRangeLookup SchoolYearDateRangeLookup
         * @param currentSchoolYear DistrictSchoolYearContext
         * @return the set of service OIDs with overlap errors, if auto-adjustment is not being used
         */
        protected Set<String> adjust(Iterator services,
                                     SchoolYearDateRangeLookup dateRangeLookup,
                                     DistrictSchoolYearContext currentSchoolYear) {
            Set<String> servicesWithErrors = new HashSet<String>(512);

            while (services.hasNext()) {
                IepService service = (IepService) services.next();

                PlainDate firstSessionDate = dateRangeLookup.getStartDate(service.getIepData().getStudent());
                PlainDate lastSessionDate = dateRangeLookup.getEndDate(service.getIepData().getStudent());

                if (service.getExtendedYearIndicator()) {
                    firstSessionDate = currentSchoolYear.getStartDate();
                    lastSessionDate = currentSchoolYear.getEndDate();
                } else {
                    lastSessionDate = currentSchoolYear.getEndDate();
                }
                if (service.getStartDate().before(firstSessionDate)) {
                    boolean adjust = true;

                    /*
                     * Before setting the adjusted date, check to see if an adjustment has already
                     * been made. If so, only override it if it is outside the school year.
                     */
                    String adjustedStartDateSystemString =
                            (String) service.getFieldValueByAlias(ADJUSTED_START_DATE_ALIAS, m_dictionary);
                    if (!StringUtils.isEmpty(adjustedStartDateSystemString)) {
                        PlainDate adjustedStartDate =
                                (PlainDate) m_dateStringConverter.parseSystemString(adjustedStartDateSystemString);
                        adjust = adjustedStartDate.before(firstSessionDate);
                    }

                    if (adjust) {
                        if (m_autoAdjust) {
                            service.setFieldValueByAlias(ADJUSTED_START_DATE_ALIAS,
                                    m_dateStringConverter.getSystemString(firstSessionDate), m_dictionary);
                            addAdjustmentReason("Dates outside school yr", service);
                        } else {
                            servicesWithErrors.add(service.getOid());
                        }
                    }
                }

                if (service.getEndDate().after(lastSessionDate)) {
                    boolean adjust = true;

                    /*
                     * Before setting the adjusted date, check to see if an adjustment has already
                     * been made. If so, only override it if it is outside the school year.
                     */
                    String adjustedEndDateSystemString =
                            (String) service.getFieldValueByAlias(ADJUSTED_END_DATE_ALIAS, m_dictionary);
                    if (!StringUtils.isEmpty(adjustedEndDateSystemString)) {
                        PlainDate adjustedEndDate =
                                (PlainDate) m_dateStringConverter.parseSystemString(adjustedEndDateSystemString);
                        adjust = adjustedEndDate.after(lastSessionDate);
                    }

                    if (adjust) {
                        if (m_autoAdjust) {
                            service.setFieldValueByAlias(ADJUSTED_END_DATE_ALIAS,
                                    m_dateStringConverter.getSystemString(lastSessionDate), m_dictionary);
                            addAdjustmentReason("Dates outside school yr", service);
                        } else {
                            servicesWithErrors.add(service.getOid());
                        }
                    }
                }

                if (m_autoAdjust && service.isDirty()) {
                    _saveService(service);
                    m_numberAdjusted++;
                }
            }

            return servicesWithErrors;
        }

        /**
         * Save service.
         *
         * @param service IepService
         */
        protected void _saveService(IepService service) {
            m_broker.saveBeanForced(service);
        }
    }

    /**
     * Adjuster that sets the end dates of services within previous IEPs to end one day before the
     * start date of the next newer IEP.
     *
     * @author Follett Software Company
     */
    protected static class PreviousServiceEndDateAdjuster extends BaseServiceAdjuster {
        private boolean m_autoAdjust = false;
        private SystemStringConverter m_dateStringConverter;
        private SystemStringConverter m_booleanStringConverter;
        private X2Broker m_broker;

        protected int m_numberAdjusted = 0;

        /**
         * Constructs a new PreviousServiceEndDateAdjuster.
         *
         * @param broker X2Broker
         * @param dictionary DataDictionary
         * @param autoAdjust boolean
         */
        protected PreviousServiceEndDateAdjuster(X2Broker broker, DataDictionary dictionary, boolean autoAdjust) {
            super(dictionary);

            m_broker = broker;
            m_autoAdjust = autoAdjust;

            m_dateStringConverter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
            m_booleanStringConverter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);
        }

        /**
         * Adjusts the services in the passed map of previous services, keyed on IEP OID.
         *
         * @param previousServiceLookup Map<String,Collection<IepService>>
         * @param dateRangeLookup SchoolYearDateRangeLookup
         * @param ieps iterator of IEPs whose start date should be used as the
         *        cutoff for any previous IEP services
         * @return the set of service OIDs with overlap errors, if auto-adjustment is not being used
         */
        protected Set<String> adjust(Map<String, Collection<IepService>> previousServiceLookup,
                                     SchoolYearDateRangeLookup dateRangeLookup,
                                     Iterator ieps) {
            Set<String> servicesWithErrors = new HashSet<String>(512);

            IepData lastIep = null;
            String lastStudentOid = null;

            while (ieps.hasNext()) {
                IepData iep = (IepData) ieps.next();

                if (!iep.getStudentOid().equals(lastStudentOid)) {
                    lastIep = null;
                }

                if (lastIep != null) {
                    // Adjust service end dates if necessary;
                    Collection<IepService> lastIepServices = previousServiceLookup.get(lastIep.getOid());
                    if (lastIepServices != null) {
                        for (IepService lastIepService : lastIepServices) {
                            if (lastIepService != null &&
                                    (lastIepService.getEndDate() == null || iep != null && iep.getStartDate() != null
                                            && !lastIepService.getEndDate().before(iep.getStartDate()))) {
                                Calendar calendar = Calendar.getInstance();
                                calendar.setTime(iep.getStartDate());
                                calendar.add(Calendar.DAY_OF_YEAR, -1);

                                PlainDate adjustedEndDate = new PlainDate(calendar.getTime());

                                if (m_autoAdjust) {
                                    if (!adjustedEndDate.after(lastIepService.getStartDate())) {
                                        lastIepService.setFieldValueByAlias(SERVICE_IN_LAST_STATE_REPORT_ALIAS,
                                                m_booleanStringConverter.getSystemString(Boolean.FALSE), m_dictionary);
                                    } else {
                                        lastIepService.setFieldValueByAlias(ADJUSTED_END_DATE_ALIAS,
                                                m_dateStringConverter.getSystemString(adjustedEndDate), m_dictionary);
                                    }

                                    PlainDate firstSessionDate =
                                            dateRangeLookup.getStartDate(lastIepService.getIepData().getStudent());

                                    if (adjustedEndDate.before(firstSessionDate)) {
                                        lastIepService.setFieldValueByAlias(SERVICE_IN_LAST_STATE_REPORT_ALIAS,
                                                m_booleanStringConverter.getSystemString(Boolean.FALSE), m_dictionary);
                                    }

                                    addAdjustmentReason("Overlap with newer IEP", lastIepService);

                                    m_numberAdjusted++;

                                    _saveService(lastIepService);
                                } else {
                                    servicesWithErrors.add(lastIepService.getOid());
                                }
                            }
                        }
                    }
                }

                lastStudentOid = iep.getStudentOid();
                lastIep = iep;
            }

            return servicesWithErrors;
        }

        /**
         * Save service.
         *
         * @param service IepService
         */
        protected void _saveService(IepService service) {
            m_broker.saveBeanForced(service);
        }
    }

    /**
     * Adjuster that eliminates overlaps between services in an amended IEP and the original IEP.
     * This adjuster differs from the PreviousServiceEndDateAdjuster in that it identifies overlaps
     * by individual service across the IEPs. Example:
     *
     * <pre>
     * Original (Amended) IEP Services                          New (Active) IEP Services
     * +---------+------------+----------+-------------+        +---------+------------+----------+-------------+
     * | Service | Start Date | End Date | Hrs Per Day |        | Service | Start Date | End Date | Hrs Per Day |
     * +---------+------------+----------+-------------+        +---------+------------+----------+-------------+
     * | OT      | 9/10/13    | 6/24/14  | 1           |        | OT      | 2/1/14     | 6/24/14  | .5          |
     * +---------+------------+----------+-------------+        +---------+------------+----------+-------------+
     * | PT      | 9/10/13    | 6/24/14  | .5          |        | PT      | 9/10/13    | 6/24/14  | .5          |
     * +---------+------------+----------+-------------+        +---------+------------+----------+-------------+
     *                                                          | Speech  | 2/1/14     | 6/24/14  | .75         |
     *                                                          +---------+------------+----------+-------------+
     * </pre>
     *
     * In this example, PT would be eliminated because it is a duplicate of what is in the Active.
     * OT would be forced to end on 1/31/14 since it overlaps with the newer IEP. If there were
     * two OTs in the original and/or newer IEP, an error would be generated because accurate
     * matching would not be possible.
     * <p>
     * A single instance of this class should be used to handle one set of services for a student
     * across their IEPs, added in descending date order.
     *
     * @author Follett Software Company
     */
    protected static class AmendedServiceOverlapHandler extends BaseServiceAdjuster {
        public static final int SUCCESS = 0;

        public static final int ERROR_SERVICES_OVERLAP = 1;
        public static final int ERROR_UNABLE_TO_AUTOADJUST_OVERLAP = 3;

        private static final long DAY_MILLIS = 1000 * 60 * 60 * 24;
        private static final int BITSET_START_BUFFER = 400;

        protected HashSet<IepService> m_adjustedServices;

        private boolean m_autoAdjust = false;
        private LinkedList<IepService> m_addedServices = new LinkedList<IepService>();
        private Calendar m_calendar;
        private SystemStringConverter m_dateStringConverter;
        private SystemStringConverter m_booleanStringConverter;
        private X2Broker m_broker;

        private PlainDate m_districtFirstSessionDate;

        protected int m_numberAdusted = 0;

        /**
         * Construcst a new AmendedServiceOverlapHandler.
         *
         * @param broker X2Broker
         * @param districtFirstSessionDate PlainDate
         * @param dictionary DataDictionary
         * @param autoAdjust boolean
         */
        protected AmendedServiceOverlapHandler(X2Broker broker, PlainDate districtFirstSessionDate,
                DataDictionary dictionary, boolean autoAdjust) {
            super(dictionary);

            m_broker = broker;
            m_districtFirstSessionDate = districtFirstSessionDate;
            m_autoAdjust = autoAdjust;

            m_addedServices = new LinkedList<IepService>();
            m_calendar = Calendar.getInstance();
            m_dateStringConverter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
            m_booleanStringConverter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);
            m_adjustedServices = new HashSet<IepService>();
        }

        /**
         * Adds the service.
         *
         * @param service IepService
         * @return int
         */
        protected int addService(IepService service) {
            int result = SUCCESS;

            LinkedList<IepService> conflictingServices = findConflicts(service);

            if (!m_autoAdjust) {
                if (!conflictingServices.isEmpty()) {
                    result = ERROR_SERVICES_OVERLAP;
                }
            } else {
                /*
                 * Now check to see if more than one of the previous services are from the same IEP.
                 * If this is the case, we cannot automatically adjust the end date because we don't
                 * know which previous service corresponds to this one.
                 */
                HashSet<String> iepOids = new HashSet<String>();
                for (IepService conflictingService : conflictingServices) {
                    iepOids.add(conflictingService.getIepDataOid());
                }

                if (iepOids.size() < conflictingServices.size()) {
                    result = ERROR_UNABLE_TO_AUTOADJUST_OVERLAP;
                    addAdjustmentReason("ERROR: Cannot adjust amendment overlap", service);
                } else {
                    /*
                     * Now that we have determined that the service being added is a conflict, we
                     * also
                     * need to check to see if others from the same IEP as this one are also
                     * conflicts.
                     * Similar to above, in this case, cannot automatically adjust the end date
                     * because we don't know if this service is the one corresponding to the
                     * conflict,
                     * or one of the others.
                     */
                    LinkedList<IepService> servicesFromSameIep = new LinkedList<IepService>();
                    for (IepService previousService : m_addedServices) {
                        if (previousService.getIepDataOid().equals(service.getIepDataOid())) {
                            servicesFromSameIep.add(previousService);
                        }
                    }

                    for (IepService serviceFromSameIep : servicesFromSameIep) {
                        if (!findConflicts(serviceFromSameIep).isEmpty()) {
                            result = ERROR_UNABLE_TO_AUTOADJUST_OVERLAP;
                            addAdjustmentReason("ERROR: Cannot adjust amendment overlap", service);
                            break;
                        }
                    }

                    if (result == SUCCESS) {
                        // Adjust dates to eliminate overlaps
                        for (IepService conflictingService : conflictingServices) {
                            BitSet conflictingBitSet = getServiceDateBits(conflictingService); // m_addedServices.get(conflictingService);
                            conflictingBitSet.andNot(getServiceDateBits(service));

                            if (conflictingBitSet.length() == 0) {
                                conflictingService.setFieldValueByAlias(SERVICE_IN_LAST_STATE_REPORT_ALIAS,
                                        m_booleanStringConverter.getSystemString(Boolean.FALSE), m_dictionary);
                                m_adjustedServices.add(conflictingService);

                                addAdjustmentReason("Amended service overlap", conflictingService);

                            } else {
                                int startIndex = conflictingBitSet.nextSetBit(0);
                                int endIndex = conflictingBitSet.nextClearBit(startIndex + 1) - 1;

                                m_calendar.setTime(m_districtFirstSessionDate);
                                m_calendar.add(Calendar.DAY_OF_YEAR, startIndex - BITSET_START_BUFFER);

                                PlainDate startDate = new PlainDate(m_calendar.getTimeInMillis());

                                m_calendar.setTime(m_districtFirstSessionDate);
                                m_calendar.add(Calendar.DAY_OF_YEAR, endIndex - BITSET_START_BUFFER);

                                PlainDate endDate = new PlainDate(m_calendar.getTimeInMillis());

                                // For services on ammended ieps, set the adjusted start date to the
                                // iep start date.
                                IepData iep = conflictingService.getIepData();
                                if (iep.getAmendedIndicator()) {
                                    PlainDate iepStartDate = iep.getStartDate();
                                    if (iepStartDate.after(conflictingService.getStartDate())) {
                                        if (iepStartDate.after(startDate)) {
                                            startDate = iepStartDate;
                                        }
                                    }
                                }
                                if (!startDate.equals(conflictingService.getStartDate())) {
                                    conflictingService.setFieldValueByAlias(ADJUSTED_START_DATE_ALIAS,
                                            m_dateStringConverter.getSystemString(startDate), m_dictionary);
                                    m_adjustedServices.add(conflictingService);
                                    addAdjustmentReason("Amended service overlap", conflictingService);
                                }

                                if (!endDate.equals(conflictingService.getEndDate())) {
                                    conflictingService.setFieldValueByAlias(ADJUSTED_END_DATE_ALIAS,
                                            m_dateStringConverter.getSystemString(endDate), m_dictionary);
                                    m_adjustedServices.add(conflictingService);
                                    addAdjustmentReason("Amended service overlap", conflictingService);
                                }
                            }
                        }
                    }
                }
            }

            _saveServices();

            m_addedServices.add(service);

            return result;
        }

        /**
         * Gets the service date bits.
         *
         * @param service IepService
         * @return Bit set
         */
        protected BitSet getServiceDateBits(IepService service) {
            int startDatePos = BITSET_START_BUFFER
                    + (int) ((service.getStartDate().getTime() - m_districtFirstSessionDate.getTime()) / DAY_MILLIS);
            int endDatePos = BITSET_START_BUFFER
                    + (int) ((service.getEndDate().getTime() - m_districtFirstSessionDate.getTime()) / DAY_MILLIS);

            if (startDatePos < 0) {
                startDatePos = 0;
            }

            BitSet serviceDates = new BitSet(1000);
            serviceDates.set(startDatePos, endDatePos + 1);
            return serviceDates;
        }

        /**
         * Find conflicts.
         *
         * @param service IepService
         * @return LinkedList
         */
        protected LinkedList<IepService> findConflicts(IepService service) {
            LinkedList<IepService> conflictingServices = new LinkedList<IepService>();

            BitSet serviceDates = getServiceDateBits(service);

            /*
             * Determine if any previous services overlap with this one. Note that within the same
             * IEP, services can overlap.
             */
            for (IepService previousService : m_addedServices) {
                BitSet previousServiceDates = getServiceDateBits(previousService); // m_addedServices.get(previousService);
                if (previousServiceDates.intersects(serviceDates)) {
                    if (!previousService.getIepDataOid().equals(service.getIepDataOid())
                            && previousService.getIepData().getStatusCodeEnum() == IepData.StatusCode.AMENDED) {
                        conflictingServices.add(previousService);
                    }
                }
            }
            return conflictingServices;
        }

        /**
         * Save services.
         */
        protected void _saveServices() {
            for (IepService serviceToSave : m_adjustedServices) {
                m_broker.saveBeanForced(serviceToSave);
                m_numberAdusted++;
            }
        }
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * RETRIEVERS
     * /* -----------------------------------------------------------------------------------------
     */

    /**
     * Returns the related service description if the related service type is "Other."
     *
     * @author Follett Software Company
     */
    protected class OtherRelatedServiceRetriever implements FieldRetriever {

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
            String fieldValue = null;

            IepService service = (IepService) entity.getBean();
            String relatedService = service.getServiceType();

            if (!StringUtils.isEmpty(relatedService)) {
                String stateCode = lookupStateValue(IepService.class, IepService.COL_SERVICE_TYPE, relatedService);
                if (RELATED_SERVICE_OTHER_CODE.equalsIgnoreCase(stateCode)) {
                    fieldValue = (String) service.getFieldValueByAlias(SERVICE_DESCRIPTION_ALIAS, getDataDictionary());
                }
            }

            return fieldValue;
        }
    }

    /**
     * Returns the school code to use for each service record. For outplaced students, this is the
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
            IepService service = (IepService) entity.getBean();
            String outsidePlacementCode =
                    (String) service.getFieldValueByAlias(SERVICE_SCHOOL_CODE_OVERRIDE_ALIAS, getDataDictionary());
            if (StringUtils.isEmpty(outsidePlacementCode)) {
                Student student = service.getIepData().getStudent();
                School school = student.getSchool();

                String outsidePlacementSchoolFlag =
                        (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
                String stateSchoolId = (String) school.getFieldValueByAlias(STATE_SCHOOL_ID_ALIAS);

                outsidePlacementCode = stateSchoolId;

                if ("1".equals(outsidePlacementSchoolFlag) || (stateSchoolId != null && stateSchoolId.endsWith("90"))) {
                    StudentEnrollment enrollment = m_latestEntries.get(student.getOid());
                    if (enrollment != null) {
                        String outsidePlacementUserCode =
                                (String) enrollment.getFieldValueByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS);
                        outsidePlacementCode = lookupReferenceCodeByAlias(MEMBERSHIP_PLACEMENT_LOCATION_CODE_ALIAS,
                                outsidePlacementUserCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }

            return outsidePlacementCode;
        }
    }

    /**
     * If the adjusted end date (or end date if adjusted is null) is prior to the end of the
     * school year, the end reason is retrieved. End reason is pulled from the service record if
     * one exists; otherwise the IEP exit reason is used.
     *
     * @author Follett Software Company
     */
    protected class ServiceEndReasonRetriever implements FieldRetriever {

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
            String fieldValue = null;
            IepService service = (IepService) entity.getBean();
            String endDateValue = entity.getFieldValue(FIELD_ID_SERV_END);
            if (!StringUtils.isEmpty(endDateValue)) {
                String code = service.getIepData().getExitReason();
                if (!StringUtils.isEmpty(code)) {
                    fieldValue = lookupStateValue(IepData.class, IepData.COL_EXIT_REASON, code);
                }
                if (StringUtils.isEmpty(fieldValue)) {
                    fieldValue = REASSESSMENT_INDICATED_SERVICE_END_REASON;
                }
            }
            return fieldValue;
        }
    }

    /**
     * The Class ServiceEndDateRetriever.
     *
     * @author Follett Software Company
     */
    protected class ServiceEndDateRetriever implements FieldRetriever {

        private String m_javaNameAdjustedEndDateAlias;

        /**
         *
         */
        public ServiceEndDateRetriever() {
            m_javaNameAdjustedEndDateAlias = translateAliasToJavaName(ADJUSTED_END_DATE_ALIAS, true);
        }

        /**
         * Gets the Exit Date field value.
         * 1, Use the override end date on the service, if present.
         * 2. Use the exit date of the IEP, if present.
         * 2. use the service end date, if it is there and before the IEP exit date.
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
            Object fieldValue = null;

            IepService service = (IepService) entity.getBean();
            PlainDate endDate = null;
            Object override = data.getPropertyAsJavaType(service, m_javaNameAdjustedEndDateAlias);
            if (override != null && override instanceof PlainDate) {
                endDate = (PlainDate) override;
            } else {
                endDate = service.getEndDate();
            }
            IepData iep = service.getIepData();
            PlainDate exitDate = iep.getExitDate();
            if (exitDate != null && (endDate == null || exitDate.before(endDate))) {
                endDate = exitDate;
            }
            if (endDate != null && !endDate.after(m_reportDate)) {
                fieldValue = endDate;
            }
            return fieldValue;
        }
    }

    /**
     * Validate service plan district.
     *
     * @author Follett Software Company
     */
    protected class ServicePlanDistValidator implements FieldValidator {
        public static final String VAL_ID = "risped-PlanDist";

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

            String continuum = entity.getFieldValue("Service_Program_Cont");
            String servicePlanDistrict = entity.getFieldValue("Service_Plan_Distric");

            if ("0".equals(continuum)) {
                if (StringUtils.isEmpty(servicePlanDistrict)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Service Plan District required when Service Program Continuum = 0", value));
                }
            }

            return errors;
        }
    }

    /* ----------------------------------------------------------------------------------------- */
    /*
     * VALIDATORS
     * /* -----------------------------------------------------------------------------------------
     */

    /**
     * R2860
     * R2862
     * R2865
     * R2866
     * R3002
     * R2869
     * R2871
     * R2863
     * R2864
     * R2867
     * R2868.
     *
     * @author Follett Software Company
     */
    protected class CostCenterValidator implements FieldValidator {
        public static final String VAL_ID = "risped-CostCenter";

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

            IepService service = (IepService) entity.getBean();

            String continuum = entity.getFieldValue("Service_Program_Cont");
            String costCenter = entity.getFieldValue("Service_Cost_Center");

            boolean beneficiary =
                    "1".equals(service.getIepData().getFieldValueByAlias(BENEFICIARY_IEP_ALIAS, getDataDictionary()));

            if ("0".equals(continuum)) {
                if (!"90000".equals(costCenter)) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2871, detail));
                }
            } else if ("1".equals(continuum)) {
                if (!isInList(costCenter, "32202", "32221", "32222", "32223")) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2862, detail));
                }
            } else if ("2".equals(continuum)) {
                if (!isInList(costCenter, "32203", "32204", "32205", "32206", "32218", "32219")) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2860, detail));
                }

            } else if ("3".equals(continuum)) {
                if (beneficiary) {
                    if (!isInList(costCenter, "32210", "32217")) {
                        String detail = "Service_Cost_Center = " + costCenter + ", Beneficiary = Y";
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2863, detail));
                    }
                } else {
                    if (!isInList(costCenter, "32208", "32209")) {
                        String detail = "Service_Cost_Center = " + costCenter + ", Beneficiary = N";
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2864, detail));
                    }
                }
            } else if ("4".equals(continuum)) {
                if (!"32207".equals(costCenter)) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2865, detail));
                }
            } else if ("5".equals(continuum)) {
                if (!isInList(costCenter, "32213", "32214")) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2866, detail));
                }
            } else if ("6".equals(continuum)) {
                if (!isInList(costCenter, "32211", "32212")) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R3002, detail));
                }

                if (beneficiary) {
                    if (!"32211".equals(costCenter)) {
                        String detail = "Service_Cost_Center = " + costCenter + ", Beneficiary = Y";
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2867, detail));
                    }
                } else {
                    if (!"32212".equals(costCenter)) {
                        String detail = "Service_Cost_Center = " + costCenter + ", Beneficiary = N";
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2868, detail));
                    }
                }

            } else if ("7".equals(continuum)) {
                if (!"32215".equals(costCenter)) {
                    String detail = "Service_Cost_Center = " + costCenter;
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2869, detail));
                }
            }

            return errors;
        }
    }

    /**
     * R2038
     * R2903.
     *
     * @author Follett Software Company
     */
    protected class EndDateValidator implements FieldValidator {
        public static final String VAL_ID = "risped-SvcEndDate";

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

            String startDateString = entity.getFieldValue("Service_Record_Start");
            String endDateString = entity.getFieldValue("Service_Record_End");
            String endReason = entity.getFieldValue("Service_End_Reason");

            IepService service = (IepService) entity.getBean();
            IepData iep = service.getIepData();

            if (!StringUtils.isEmpty(endDateString)) {
                if (!StringUtils.isEmpty(startDateString)) {
                    try {
                        Date startDate = ((DateFormat) field.getFormatter()).parse(startDateString);
                        Date endDate = ((DateFormat) field.getFormatter()).parse(endDateString);

                        if (endDate.before(startDate)) {
                            errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2903, ""));
                        }
                        if (iep != null && endDate != null && iep.getEndDate() != null
                                && endDate.after(iep.getEndDate())) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Review Service End Date. Service date (s) exceed IEP End Date",
                                    "Service_Record_End = "
                                            + DateFormat.getDateInstance(DateFormat.SHORT).format(endDate)
                                            + "; EIP End Date = "
                                            + DateFormat.getDateInstance(DateFormat.SHORT).format(iep.getEndDate())));
                        }
                    } catch (ParseException e) {
                        AppGlobals.getLog().log(Level.WARNING, "Date Parse Error", e);
                    }
                }
            } else {
                if (!StringUtils.isEmpty(endReason)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Service Record End Date required if Service End Reason is not blank", ""));
                }
            }
            if (m_previousIepOverlapErrors.contains(service.getOid())) {
                errors.add(new StateReportValidationError(entity, field, "Previous service date overlap", ""));
            }
            if (m_schoolYearDateErrors.contains(service.getOid())) {
                errors.add(
                        new StateReportValidationError(entity, field, "Service dates are outside the school year", ""));
            }

            return errors;
        }
    }

    /**
     * Validate End Reason.
     *
     * @author Follett Software Company
     */
    protected class EndReasonValidator implements FieldValidator {
        public static final String VAL_ID = "risped-SvcEndReason";

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

            String endDateString = entity.getFieldValue("Service_Record_End");
            String endReason = entity.getFieldValue("Service_End_Reason");

            if (!StringUtils.isEmpty(endDateString)) {
                if (StringUtils.isEmpty(endReason)) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2038, ""));
                }
            }

            return errors;
        }
    }

    /**
     * R2880
     * R2850
     * R2809
     * R2861
     * R2872.
     *
     * @author Follett Software Company
     */
    protected class ProgramContinuumValidator implements FieldValidator {
        public static final String VAL_ID = "risped-ProgramCont";

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

            IepService service = (IepService) entity.getBean();

            String continuum = entity.getFieldValue("Service_Program_Cont");

            if (!isInList(continuum, "1", "2", "3", "4", "5", "6", "7", "0")) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2880, ""));
            }

            Student student = service.getIepData().getStudent();
            String gradeLevel = lookupStateValue(Student.class, Student.COL_GRADE_LEVEL, student.getGradeLevel());

            if (isInList(gradeLevel, "PK", "PF") && !isInList(continuum, "5", "0", "F", "J", "K", "Z")) {
                String detail = "GradeLevel = " + gradeLevel;
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2850, detail));
            }

            if ("5".equals(continuum) && !isInList(gradeLevel, "PK", "PF")) {
                String detail = "GradeLevel = " + gradeLevel;
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2809, detail));
            }

            boolean privateSchool = "1".equals(
                    service.getIepData().getFieldValueByAlias(PRIVATE_SCHOOL_ENROLLEE_IEP_ALIAS, getDataDictionary()));
            if ("1".equals(continuum) && privateSchool) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2861, ""));
            }

            if (isInList(continuum, "0", "7", "Z") && !privateSchool) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2872, ""));
            }

            return errors;
        }
    }

    /**
     * Service type validator.
     *
     * @author Follett Software Company
     */
    protected class ServiceTypeValidator implements FieldValidator {
        private final static String DETAIL_MESSAGE = "Must be RelServ or SpEd";
        private final static String VAL_ID = "risped-ServiceType";

        private Collection<String> m_serviceTypes = null;

        /**
         * Instantiates a new service type validator.
         */
        ServiceTypeValidator() {
            m_serviceTypes = Arrays.asList("RelServ", "SpEd");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> m_errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value) && !m_serviceTypes.contains(value)) {

                m_errors.add(new StateReportValidationError(entity, field, DETAIL_MESSAGE, value));
            }

            return m_errors;
        }
    }

    /**
     * R2402.
     *
     * @author Follett Software Company
     */
    protected class HoursValidator implements FieldValidator {
        public static final String VAL_ID = "risped-Hours";

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
            if (!StringUtils.isEmpty(value)) {
                try {
                    double hoursDouble = Double.parseDouble(value);
                    if (hoursDouble == 0) {
                        errors.add(new StateReportValidationError(entity, field,
                                LocalizationCache.getMessages(getBroker().getPersistenceKey())
                                        .getMessage(ERROR_MISSING_VALUE_KEY),
                                value));
                    }
                    if (hoursDouble > 6) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2402, value));
                    }
                    if (hoursDouble < 0.1d) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Service Record Hours [frequency-hours] cannot be less than 0.10.",
                                "Service_Record_Hours = " + value));
                    }
                } catch (NumberFormatException nfe) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Invalid numeric value for Service_Record_Hours", "Hours = " + value));
                }
            }
            return errors;
        }
    }

    /**
     * 2403.
     *
     * @author Follett Software Company
     */
    protected class DaysValidator implements FieldValidator {
        public static final String VAL_ID = "risped-Days";

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

            String daysString = entity.getFieldValue("Service_Record_Days_");
            if (!StringUtils.isEmpty(daysString)) {
                try {
                    if (Double.parseDouble(daysString) == 0) {
                        errors.add(new StateReportValidationError(entity, field,
                                LocalizationCache.getMessages(getBroker().getPersistenceKey())
                                        .getMessage(ERROR_MISSING_VALUE_KEY),
                                value));
                    }
                    if (Double.parseDouble(daysString) > 5) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2403, value));
                    }
                } catch (NumberFormatException nfe) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Invalid numeric value for Service_Record_Days_", "Days = " + daysString));
                }
            }

            return errors;
        }
    }

    /**
     * 2404.
     *
     * @author Follett Software Company
     */
    protected class WeeksValidator implements FieldValidator {
        public static final String VAL_ID = "risped-Weeks";

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

            String weeksString = entity.getFieldValue("Service_Record_Weeks");
            if (!StringUtils.isEmpty(weeksString)) {
                try {
                    if (Double.parseDouble(weeksString) == 0) {
                        errors.add(new StateReportValidationError(entity, field,
                                LocalizationCache.getMessages(getBroker().getPersistenceKey())
                                        .getMessage(ERROR_MISSING_VALUE_KEY),
                                value));
                    }
                    if (Double.parseDouble(weeksString) > 4) {
                        errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2404, value));
                    }
                } catch (NumberFormatException nfe) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Invalid numeric value for Service_Record_Weeks", "Weeks = " + weeksString));
                }
            }

            return errors;
        }
    }

    /**
     * 2873
     * 2878
     * 2874.
     *
     * @author Follett Software Company
     */
    protected class ProviderValidator implements FieldValidator {
        public static final String VAL_ID = "risped-Provider";

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

            String serviceType = entity.getFieldValue("Service_Type");
            String serviceProvider = entity.getFieldValue("Service_Provider");

            if ("SpEd".equals(serviceType)) {
                if (!isInList(serviceProvider, "C", "G", "W", "X", "Y")) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2873, value));
                }
            } else if ("RelServ".equals(serviceType)) {
                if (!isInList(serviceProvider, "C", "D", "E", "F", "I", "J", "N", "O", "Q", "S", "Z", "1", "2", "3",
                        "4", "5", "6")) {
                    errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2874, value));
                }
            }
            return errors;
        }
    }

    /**
     * 2876.
     *
     * @author Follett Software Company
     */
    protected class RelatedServiceValidator implements FieldValidator {
        private final static String VAL_ID = "risped-RelatedSvc";

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

            String serviceType = entity.getFieldValue("Service_Type");
            String relatedService = entity.getFieldValue("Related_Service");

            if ("RelServ".equals(serviceType) && StringUtils.isEmpty(relatedService)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2876, value));
            }

            return errors;
        }
    }

    /**
     * 2877.
     *
     * @author Follett Software Company
     */
    protected class RelatedServiceOtherValidator implements FieldValidator {
        private final static String VAL_ID = "risped-RelatedSvcOth";

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

            String serviceType = entity.getFieldValue("Service_Type");
            String relatedService = entity.getFieldValue("Related_Service");
            String relatedServiceOther = entity.getFieldValue("Related_Service_Othe");

            if ("RelServ".equals(serviceType) && "Q".equals(relatedService)
                    && StringUtils.isEmpty(relatedServiceOther)) {
                errors.add(new StateReportValidationError(entity, field, STATE_VALIDATION_R2877, value));
            }

            return errors;
        }
    }
}
