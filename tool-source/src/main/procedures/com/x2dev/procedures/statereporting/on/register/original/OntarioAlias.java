/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.on.register.original;

import com.x2dev.sis.model.beans.StudentEnrollment;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Container class Ontario alias constants, calculated values OIDs used in some
 * cases as names maybe changed more easily.
 *
 * @author Follett Software Company
 */
public class OntarioAlias {
    private static final long serialVersionUID = 1L;

    /*
     * Images and other constants
     */
    public static final String CONST_EMPTY = "";
    public static final String CONST_SPACE = " ";
    public static final String IMAGE_FILE_NAME_ONTARIO = "\\\\EDCASPENSQL\\resources\\ONTARIO.jpg";
    public static final String IMAGE_SCISSORS = "Scissors";
    public static final String IMAGE_ARROWRIGHT = "ArrowRight";

    // OnSis Images code constants
    public static final String ON_MINISTRY_CODE_LOGO = "OnMinistry";
    public static final String ON_TRILLIUM_CODE_LOGO = "OnTrillium";
    public static final String ON_BOARD_CODE_LOGO = "OnBoardLogo";

    // Constants for OSSD (1999) Waivers
    public static final String WAIVER_OSSD_1999_COMMUNITY_SERVICE_INVOLVEMENT_REQ_CODE = "Community Involvement Hours";
    public static final String WAIVER_OSSD_1999_PROVINCIAL_SECONDAR_SCHOOL_LITERACY_REQUIREMENT =
            "Provincial Secondary School Literacy Requirement";

    // Common Ontario Graduation Diploma constants
    public static final String OSSD_1999_GPR_OID = "gpr000OSSD1999";
    public static final String OSSD_1989_GPR_OID = "gpr000OSSD1989";
    public static final String OSSC_1999_GPR_OID = "gpr000OSSC1999";
    public static final String COE_1989_GPR_OID = "gpr0000CoE1999";

    // Transcript Definition
    public static final String KINDERGARTEN_GTD_OID = "gtdSm0000000KG";
    public static final String KINDERGARTEN_CATHOLIC_GTD_OID = "gtdSm000cathKG";

    /*
     * Reference tables, extended dictionaries, language
     */
    // Reference table constants
    // Age verification codes
    public static final String REF_OID_SOURCE_VER = "RTB0000000cKFC";
    // Bsid Schools
    public static final String REF_OID_BSID_SCHOOLS = "rtbOnBsidSchol";
    // Entry Demit codes
    public static final String REF_OID_ENR_CODES = "rtbEntryDemit";
    // OnSIS Images
    public static final String REF_OID_ON_SIS_IMAGES = "rtbOnImage    ";
    // Grade Levels
    public static final String REF_OID_GRADE_LEVEL = "rtbGradeLevel ";
    // Min course codes
    public static final String REF_OID_CRS_MINISTRY_CODES = "rtbCrsOnMinCod ";
    // OnSIS Credit Type
    public static final String REF_OID_ON_SIS_CREDIT_TYPE = "RTB0000000i0vt ";
    // OnSIS Course Delivery Type
    public static final String REF_OID_ON_SIS_COURSE_DELIVERY_TYPE = "rtbOnCrsDelTyp ";
    // OnSIS Course Offering Type
    public static final String REF_OID_ON_SIS_COURSE_OFFERING_TYPE = "rtbOnCrsOffTyp ";
    // OnSIS Class Type
    public static final String REF_OID_ON_SIS_CLASS_TYPE = "rtbMstOnClsTyp ";
    // OnSIS Other Course Information Type
    public static final String REF_OID_ON_SIS_OTHER_COURSE_INFORMATION_TYPE = "RTB0000001W063";
    // Incident Location Codes
    public static final String REF_OID_CND_LOCATION_CODE = "rtbCndLocation ";
    // Attendance Am PM
    public static final String REF_OID_ATTENDANCE_AM_PM_CODES = "rtbAttAmPm    ";
    // Buidling Codes
    public static final String REF_OID_BUILDING_CODES = "rtbRmsBuilding";
    // School Year Codes
    public static final String REF_OID_SCHOOL_YEARS = "rtbContextYear";
    // Case Email Messages
    public static final String REF_OID_CASE_EMAIL_MESSAGES = "rtbCaseEmailMs";
    // Case Management Visibility
    public static final String REF_OID_CASE_VISIBILITY = "rtbCaseVisblty";

    // Reference code aliases
    public static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";

    // Reference Class Attendance Codes
    public static final String ALIAS_RAT_ROLL_UP_DAILY_ATTENDANCE_CODE = "all-rat-RollupDailyAttendanceCode";

    // Extended dictionary constants
    public static final String EXT_OID_BSID_SCHOOLS = "DDX000000060A4";
    public static final String EXT_OID_REF_GRADE_LEVELS = "ddxGradeLevels ";
    public static final String EXT_OID_INCIDENT = "ddxOnIncident";
    public static final String EXT_OID_ONSIS_MINISTRY_COURSE_CODE = "ddxCrsOnMinCod";
    public static final String EXT_OID_RCD_SCHOOL_YEARS = "ddxRefCtxYears";
    public static final String EXT_OID_STF_RPT_SIGN = "ddxOnStfSign  ";
    public static final String EXT_ID_ON_PREFERENCES = "ON-PREFERENCES";
    public static final String EXT_ID_CASE_MANAGEMENT_EMAIL = "REF-CM-EMAIL";
    public static final String EXT_ID_CASE_MANAGEMENT_STUDENT_ATT_PLAN = "STD-SEP-ATT-PLAN";

    // Extended dictionary aliases
    public static final String ALIAS_EXT_BSID_STREET = "rcd-bsid-street";
    public static final String ALIAS_EXT_BSID_CITY = "rcd-bsid-city";
    public static final String ALIAS_EXT_BSID_PROVINCE = "rcd-bsid-province";
    public static final String ALIAS_EXT_BSID_POSTAL_CODE = "rcd-bsid-postal-code";
    public static final String ALIAS_EXT_NUMERIC_GRADE_LEVEL = "NumericGradeLevel";
    public static final String ALIAS_EXT_COLLEGE_COURSE_TITLE = "rcd-crs-college-course-title";
    public static final String ALIAS_EXT_RCD_CTX_COMM_HOURS = "rcd-ctx-comm-hours";

    public static final String ALIAS_EXT_RCD_CRS_OLD_MINISTRY_CODE = "rcd-crs-old-ministry-code";

    public static final String ALIAS_EXT_ORA_PREF_PUBLISH_ESIGNATURE = "ora-pref-publish-esignature";

    public static final String ALIAS_EXT_UDC_STF_SIGNATURE_FILE = "udc-stf-signature-file";
    public static final String ALIAS_EXT_UDC_STF_SIGNATURE_END_DATE = "udc-stf-signature-end-date";

    // REF-CM-EMAIL extension aliases
    public static final String ALIAS_EXT_RCD_EMAIL_MESSAGE = "rcd-cm-email-message";

    // STD-SEP-ATT-PLAN extension aliases
    public static final String ALIAS_EXT_SEE_ATT_PLAN_LOGIN_NAME = "see-att-plan-login-name";
    public static final String ALIAS_EXT_SEE_ATT_PLAN_USER = "see-att-plan-user";
    public static final String ALIAS_EXT_SEP_ATT_PLAN_NARRATIVE = "sep-att-plan-narrative";
    public static final String ALIAS_EXT_SEP_ATT_PLAN_OBJECTIVES = "sep-att-plan-objectives";
    public static final String ALIAS_EXT_SEJ_ATT_PLAN_TYPE = "sej-att-plan-type";
    public static final String ALIAS_EXT_SEJ_ATT_PLAN_USER = "sej-att-plan-user";
    public static final String ALIAS_EXT_SEJ_ATT_PLAN_DATE = "sej-att-plan-date";
    public static final String ALIAS_EXT_SEJ_ATT_PLAN_DESCRIPTION = "sej-att-plan-description";
    public static final String ALIAS_EXT_SEJ_ATT_PLAN_LOGIN_NAME = "sej-att-plan-login-name";

    // Language constants
    public static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    public static final String CONST_LOCALE_ENGLISH = "en_US";
    public static final String CONST_LANG_FRENCH = "French";
    public static final String CONST_LOCALE_FRENCH = "fr_CA";

    /*
     * Organization/School
     */
    // Organization aliases
    public static final String ALIAS_ORG_LANGUAGE = "all-org-BoardLanguage";

    // School aliases
    public static final String ALIAS_SKL_BSID = "all-skl-BSID";
    public static final String ALIAS_SKL_LANGUAGE = "all-skl-LanguageType";
    public static final String ALIAS_SKL_DESIGNATED_ELEMENTARY_BSID = "all-skl-DesignatedElementaryBSID";
    public static final String ALIAS_SKL_DESIGNATED_SECONDARY_BSID = "all-skl-DesignatedSecondaryBSID";
    public static final String ALIAS_SKL_SPECIAL_CONDITION = "all-skl-SpecialCondition";
    public static final String ALIAS_ALL_SKL_CLASS_ATTENDANCE_OVERWRITES_DAILY_ATTENDANCE =
            "all-skl-ClassAttendanceOverwritesDailyAttendance";
    public static final String ALIAS_SKL_PUBLISH_WITH_ESIGNATURE = "all-skl-PublishWithElectronicSignature";

    // School constants
    public static final String CONST_SKL_TYPE_ELEM = "E";
    public static final String CONST_SKL_TYPE_SEC = "S";
    public static final List<String> CONST_SKL_TYPES_REL = new ArrayList(Arrays.asList("2", "Catholic"));
    public static final String CONST_SKL_SPECIAL_CONDITION_CON_ED = "N";
    public static final List CONST_SKL_SPECIAL_CONDITION_CON_ED_LIST = new ArrayList(
            Arrays.asList(CONST_SKL_SPECIAL_CONDITION_CON_ED));

    // Constant values - Months
    public static final int CONST_SKL_MONTH_JUL = 1;
    public static final int CONST_SKL_MONTH_AUG = 2;
    public static final int CONST_SKL_MONTH_SEP = 3;
    public static final int CONST_SKL_MONTH_OCT = 4;
    public static final int CONST_SKL_MONTH_NOV = 5;
    public static final int CONST_SKL_MONTH_DEC = 6;
    public static final int CONST_SKL_MONTH_JAN = 7;
    public static final int CONST_SKL_MONTH_FEB = 8;
    public static final int CONST_SKL_MONTH_MAR = 9;
    public static final int CONST_SKL_MONTH_APR = 10;
    public static final int CONST_SKL_MONTH_MAY = 11;
    public static final int CONST_SKL_MONTH_JUN = 12;

    // Constant value - map of school months to calendar months (will be different
    // calendar years)
    public static final Map<Integer, Integer> CONST_CAL_MONTH_TO_SKL_MONTH_MAP = new HashMap<Integer, Integer>();
    static {
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(1), Integer.valueOf(7));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(2), Integer.valueOf(8));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(3), Integer.valueOf(9));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(4), Integer.valueOf(10));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(5), Integer.valueOf(11));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(6), Integer.valueOf(12));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(7), Integer.valueOf(1));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(8), Integer.valueOf(2));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(9), Integer.valueOf(3));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(10), Integer.valueOf(4));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(11), Integer.valueOf(5));
        CONST_CAL_MONTH_TO_SKL_MONTH_MAP.put(Integer.valueOf(12), Integer.valueOf(6));
    }

    // Calendar constants
    public static final String CSD_DAY_TYPE_GENERAL_ABSENCE = "G";
    public static final String CSD_DAY_TYPE_BOARD_HOLIDAY = "B";
    public static final String CSD_DAY_TYPE_PROF_ACTIVITY = "PA Day";
    public static final String CSD_DAY_TYPE_STAT_HOLIDAY = "H";
    public static final String CSD_DAY_TYPE_BOARD_HOLIDAY_FRENCH = "C";
    public static final String CSD_DAY_TYPE_PROF_ACTIVITY_FRENCH = "JP";
    public static final String CSD_DAY_TYPE_STAT_HOLIDAY_FRENCH = "F";

    // Calendar aliases
    public static final String ALIAS_ALL_CAS_USE_THIS_CALENDAR_FOR_ROLL_UP =
            "all-cas-UseThisCalendarForRollUpProcedure";

    /*
     * Staff
     */
    //Staff aliases
    public static final String ALIAS_STF_ESIGNATURE_FILE = "all-stf-eSignatureFile";
    public static final String ALIAS_STF_ESIGNATURE_END_DATE = "all-stf-StaffElectronicSignatureEndDate";

    /*
     * Person
     */
    // Person aliases
    public static final String ALIAS_PSN_LEGAL_LAST_NAME = "all-psn-LegalLastName";
    public static final String ALIAS_PSN_LEGAL_FIRST_NAME = "all-psn-LegalFirstName";
    public static final String ALIAS_PSN_LEGAL_MIDDLE_NAME = "all-psn-LegalMiddleName";
    public static final String ALIAS_PSN_CANADA_STATUS_CODE = "all-psn-CanadaStatusCode";
    public static final String ALIAS_PSN_CITIZENSHIP_COUNTRY = "all-psn-CitizenshipCountry";
    public static final String ALIAS_PSN_ARRIVAL_DATE_CANADA = "all-psn-ArrivalDateCanada";
    public static final String ALIAS_PSN_PREFERRED_LANGUAGE = "all-psn-PreferredLanguage";
    public static final String ALIAS_PSN_BIRTH_COUNTRY = "all-psn-BirthCountry";
    public static final String ALIAS_PSN_BIRTH_PROVINCE = "all-psn-BirthProvince";
    public static final String ALIAS_PSN_LAST_RESIDENCE_COUNTRY = "all-psn-LastResidenceCountry";
    public static final String ALIAS_PSN_FIRST_ARRIVAL_PROVINCE = "all-psn-ArrivalDateProvince";
    public static final String ALIAS_PSN_PHONE_01_TYPE = "all-psn-Phone01Type";
    public static final String ALIAS_PSN_PHONE_02_TYPE = "all-psn-Phone02Type";
    public static final String ALIAS_PSN_PHONE_03_TYPE = "all-psn-Phone03Type";

    // Person address aliases
    public static final String ALIAS_ADR_STREET_TYPE_PRECEDES_NAME = "all-adr-StreetTypePrecedesName";
    public static final String ALIAS_ADR_UNIT_TYPE = "all-adr-UnitType";
    public static final String ALIAS_ADR_UNIT_NUMBER = "all-adr-UnitNumber";
    public static final String ALIAS_ADR_DELIVERY_TYPE = "all-adr-DeliveryType";
    public static final String ALIAS_ADR_DELIVERY_NUMBER = "all-adr-DeliveryNumber";
    public static final String ALIAS_ADR_TOWNSHIP = "all-adr-Township";
    public static final String ALIAS_ADR_ADDRESS_ID = "all-adr-AddressID";

    // Person Address aliases - pad table
    public static final String ALIAS_PAD_PHONE_NUMBER = "all-pad-PhoneNumber";
    public static final String ALIAS_PAD_PHONE_EXTENSION = "all-pad-PhoneExtension";
    public static final String ALIAS_PAD_PRIORITY = "all-pad-Priority";

    /*
     * Student
     */
    // Student constants
    public static final String CONST_GENDER_MALE = "M";
    public static final String CONST_GENDER_FEMALE = "F";
    public static final String CONST_GENDER_UNKNOWN = "U";
    public static final Integer CONST_ADULT_AGE = Integer.valueOf(21);
    public static final Collection STD_GRADES_01_TO_06 = new ArrayList(
            Arrays.asList("01", "02", "03", "04", "05", "06"));
    public static final Collection STD_GRADES_07_TO_08 = new ArrayList(Arrays.asList("07", "08"));
    public static final Collection STD_GRADES_01_TO_08 = new ArrayList(
            Arrays.asList("01", "02", "03", "04", "05", "06", "07", "08"));
    // report use only
    public static final String CONST_GRADE_LVLS_01_TO_06 = "01_to_06";
    // report use only
    public static final String CONST_GRADE_LVLS_07_TO_08 = "07_to_08";
    public static final List CONST_STD_PGM_WITH_COMMUNITY_HOURS_LIST = new ArrayList<String>(
            Arrays.asList("OSSD (1989)", "OSSD (1999)", "OSSD-E with subs (1999)"));
    public static final Double CONST_STD_COMMUNITY_HOURS_REQD = Double.valueOf(40.00);

    // General student aliases
    public static final String ALIAS_STD_AGE_VER = "all-std-AgeVerification";
    public static final String ALIAS_STD_SUBSTITUTED_CREDITS = "all-std-SubstitutedCredits";
    public static final String ALIAS_STD_SUBSTITUTED_LS_CREDITS = "all-std-SubstitutedLsCredits";
    public static final String ALIAS_STD_NATIVE_LANG = "all-std-FirstLanguageSpoken";
    public static final String ALIAS_STD_COMMUNITY_HOURS_ACTUAL = "all-std-CommunityAccuHours";
    public static final String ALIAS_STD_MATURE_FLAG = "all-std-MatureStudentFlag";
    public static final String ALIAS_STD_MATURE = "all-std-MatureStudent";
    public static final String ALIAS_STD_MIN = "all-std-Min";
    public static final String ALIAS_STD_OEN = "all-std-Oen";
    public static final String ALIAS_STD_OSSLT_ELIGIBILITY = "all-std-OssltEligibility";
    public static final String ALIAS_STD_INDIGENOUS = "all-std-Indigenous";
    public static final String ALIAS_STD_ELL_PROGRAM = "all-std-EllProgram";
    public static final String ALIAS_STD_EQAO_GROUPING = "all-std-eqaoGroup";
    public static final String ALIAS_STD_RETAINED = "all-std-Retained";
    public static final String ALIAS_STD_GRADE9_COHORT = "all-std-Grade9Cohort";
    public static final String ALIAS_STD_AWARD_OSSD_SEPARATELY = "all-std-AwardOssdSeparately";
    public static final String ALIAS_STD_PGM_DELIVER_TYPE = "all-std-ProgramDeliveryType";
    public static final String ALIAS_STD_STUDENT_COUNSELLOR = "all-std-Counsellor";
    public static final String ALIAS_STD_CONSENT_FOR_ACCESS = "all-std-ConsentForAccess";
    public static final String ALIAS_STD_CONSENT_NOTES = "all-std-ConsentNotes";
    public static final String ALIAS_STD_CONSENT_SET_BY = "all-std-ConsentSetBy";
    public static final String ALIAS_STD_CONSENT_STATUS = "all-std-ConsentStatus";
    public static final String ALIAS_STD_CONSENT_USER = "all-std-ConsentUser";
    public static final String ALIAS_STD_DATE_CONSENT_LAST_MODIFIED = "all-std-DateConsentLastModified";
    public static final String ALIAS_STD_DATE_OF_CONSENT_UPDATE = "all-std-DateOfConsentUpdate";
    public static final String ALIAS_STD_LATEST_CONSENT_NOTES = "all-std-LatestConsentNotes";
    public static final String ALIAS_STD_PHYSICAL_LOCATION_OF_PLANS = "all-std-PhysicalLocationOfPlans";

    // Student homeroom
    public static final String ALIAS_STD_HR_TEACHER_VIEW = "all-std-HomeroomTeacher";

    // Student Grade aliases
    public static final String ALIAS_STD_GRAD_REQ_CALC_ON = "all-std-GradReqCalcOn";
    public static final String ALIAS_STD_GRAD_REQ_CALC_TIMESTAMP = "all-std-GradReqCalcTimestamp";

    // Student School aliases
    public static final String ALIAS_SSK_ARRIVAL_STATUS = "all-ssk-ArrivalStatus";
    public static final String ALIAS_SSK_BOARD_RESIDENT_STATUS = "all-ssk-BoardResidentStatus";
    public static final String ALIAS_SSK_NOT_ARRIVED_AS_OF_DATE = "all-ssk-NotArrivedAsOfDate";

    // Student Entry date aliases
    public static final String ALIAS_STD_FIRST_ENTRY_DATE_ELE_SCHOOL = "all-std-FirstEntryDateToElementarySchool";
    public static final String ALIAS_STD_FIRST_ENTRY_DATE_SECONDARY_SCHOOL = " all-std-FirstEntryDateToSecondarySchool";

    // OCAS/OUAC student aliases
    public static final String ALIAS_STD_OCAS_APPLICANT = "all-std-OcasApplicant";
    public static final String ALIAS_STD_OUAC_APPLICANT = "all-std-OuacApplicant";
    public static final String ALIAS_STD_OCAS_DO_NOT_REPORT = "all-std-OcasDoNotReport";
    public static final String ALIAS_STD_OUAC_DO_NOT_REPORT = "all-std-OuacDoNotReport";

    // Student graduation credit totals
    public static final String ALIAS_STD_COMPULSORY_CREDITS = "all-std-CompulsoryCredits";
    public static final String ALIAS_STD_OPTIONAL_CREDITS = "all-std-OptionalCredits";
    public static final String ALIAS_STD_TOTAL_CREDITS = "all-std-TotalCredits";
    public static final String ALIAS_STD_ONLINE_LEARNING_CREDITS = "all-std-OnlineLearningCredits";

    // Student Schedule aliases
    public static final String ALIAS_SSC_PROJECTED_REQ_AREA = "all-ssc-ProjectedRequirementArea";
    public static final String ALIAS_SSC_SHSM_PROJECTED_REQ_AREA = "all-ssc-SHSMProjectedRequirementArea";

    // Student Grade 9 Cohor Calculated Map ToolHelper#getGrade9CohortCalculatedMap
    public static final String CONST_G9C_CALC_MAP_YOG = "yog";
    public static final String CONST_G9C_CALC_MAP_GRADE9COHORT = "grade9Cohort";
    public static final String CONST_G9C_CALC_MAP_YEARS_HS = "yearsInHighSchool";
    public static final String CONST_G9C_CALC_MAP_YEARS_HS_SIMP = "yearsInHighSchoolSimplified";

    /*
     * Student Contacts
     */
    // Student Contact constants
    public static final String CTJ_REL_CODE_MOTHER = "Mother";
    public static final String CTJ_REL_CODE_FATHER = "Father";
    public static final String CTJ_REL_CODE_GUARDIAN = "Guardian";

    // Student Contact aliases
    public static final String ALIAS_CTJ_GUARDIAN = "all-ctj-LegalGuardian";
    public static final String ALIAS_CTJ_LEGAL_CUSTODY = "all-ctj-LegalCustody";

    // Student Course Request aliases
    public static final String ALIAS_REQ_PROJECTED_REQ_AREA = "all-req-ProjectedRequirementArea";
    public static final String ALIAS_REQ_SHSM_PROJECTED_REQ_AREA = "all-req-SHSMProjectedRequirementArea";

    /*
     * Student Enrolment
     */
    // Enrolment constants
    public static final Collection<String> CONST_ENR_TYPE_ENTRY = new ArrayList<String>(
            Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.STATUS_CHANGE, StudentEnrollment.YOG_CHANGE));
    public static final Integer ENR_ELEM_CYCLE_MINS_TOT = Integer.valueOf(300);
    public static final Collection<String> ENR_OTHER_PUPIL_CODES = new ArrayList<String>(
            Arrays.asList("02", "03", "05", "07", "12", "13", "14", "15"));
    public static final Collection<String> ENR_TYPE_CODES = new ArrayList<>(
            Arrays.asList("E", "S"));
    public static final String ENR_REG_TYPE_FT = "FT";
    public static final String ENR_REG_TYPE_PT = "PT";
    public static final String ENR_CODE_RCD_SYS_TRANS_INT = "Internal Transfer";
    public static final String ENR_CODE_RCD_SYS_TRANS_EXT = "External Transfer";
    public static final String ENR_CODE_RCD_SYS_RETIRE = "Retirement";
    public static final String ENR_REASON_SEC_FTE_CHG = "Change in FTE";
    // Used by method in tool helper class
    public static final String CONST_ENR_OP_IND_SUFFIX = "-OpInd";
    public static final String CONST_ENR_OP_PAYER_SUFFIX = "-OpPayer";
    public static final String CONST_ENR_OP_TXT = "OP";
    public static final String CONST_ENR_REG_TYP_SUFFIX = "-RegTyp";
    public static final String CONST_ENR_DT_FIRST = "-DtFirst";
    // Arrival status constants
    public static final String CONST_ARRIVAL_STATUS_ARRIVED = "Arrived";
    public static final String CONST_ARRIVAL_STATUS_NOT_ARRIVED = "Not Arrived";
    public static final String CONST_ARRIVAL_STATUS_NO_SHOW = "No Show";
    public static final String CONST_ARRIVAL_STATUS_EXPECTED = "Expected";

    // Enrolment aliases
    public static final String ALIAS_ENR_ARRIVAL_STATUS = "all-enr-ArrivalStatus";
    public static final String ALIAS_ENR_ENTRY_DEMIT_BSID = "all-enr-EntryDemitBsid";
    public static final String ALIAS_ENR_START_DATE = "all-ctj-StartDate";
    public static final String ALIAS_ENR_END_DATE = "all-ctj-EndDate";
    public static final String ALIAS_ENR_OTHER_PUPIL = "all-enr-BoardResidentStatus";
    public static final String ALIAS_ENR_FUNDING_PAYER = "all-enr-FundingPayer";
    public static final String ALIAS_ENR_GRADE_LEVEL = "all-enr-GradeLevel";
    public static final String ALIAS_ENR_REGISTER = "all-enr-EnrolmentRegister";
    public static final String ALIAS_ENR_MINUTES = "all-enr-TotalMinutesInCycle";
    public static final String ALIAS_ENR_FTE = "all-enr-Fte";
    public static final String ALIAS_ENR_FTE_HC = "all-enr-FteHc";
    public static final String ALIAS_ENR_OSR_REQUESTED = "all-enr-OsrRequested";
    public static final String ALIAS_ENR_OSR_REQUEST_RECEIVED = "all-enr-OsrRequestReceived ";
    public static final String ALIAS_ENR_OSR_SENT = "all-enr-OsrSent";
    public static final String ALIAS_ENR_OSR_RECEIVED = "all-enr-OsrReceived";

    // Student Fte enrolment based extension table
    // Extended dictionary constants
    public static final String EXT_OID_STD_FTE = "ddxStdFte     ";
    // Student Fte constants - register
    public static final String CONST_STD_FTE_REG_FT = "FT";
    public static final String CONST_STD_FTE_REG_PT = "PT";
    // Student Fte constants - month contains
    public static final String CONST_STD_FTE_REG_OCT = "OCT";
    public static final String CONST_STD_FTE_REG_MAR = "MAR";
    // Extended dictionary aliases - UDC By Date
    public static final String ALIAS_STD_FTE_UDC_SCHOOL_YEAR = "udc-fte-school-year";
    public static final String ALIAS_STD_FTE_UDC_DATE = "udc-fte-date";
    public static final String ALIAS_STD_FTE_UDC_REG = "udc-fte-register";
    public static final String ALIAS_STD_FTE_UDC_MINS = "udc-fte-minutes";
    public static final String ALIAS_STD_FTE_UDC_MINS_HC = "udc-fte-minutes-hc";
    public static final String ALIAS_STD_FTE_UDC_FTE = "udc-fte-fte";
    public static final String ALIAS_STD_FTE_UDC_FTE_HC = "udc-fte-fte-hc";
    // Extended dictionary aliases - UDD By Month
    public static final String ALIAS_STD_FTE_UDD_SCHOOL_YEAR = "udd-fte-school-year";
    public static final String ALIAS_STD_FTE_UDD_DATE = "udd-fte-report-date";
    public static final String ALIAS_STD_FTE_UDD_MONTH = "udd-fte-month";
    public static final String ALIAS_STD_FTE_UDD_REG = "udd-fte-register";
    public static final String ALIAS_STD_FTE_UDD_MINS = "udd-fte-minutes";
    public static final String ALIAS_STD_FTE_UDD_MINS_HC = "udd-fte-minutes-hc";
    public static final String ALIAS_STD_FTE_UDD_FTE = "udd-fte-fte";
    public static final String ALIAS_STD_FTE_UDD_FTE_HC = "udd-fte-fte-hc";
    public static final String ALIAS_STD_FTE_UDD_RESIDENCY_OP_CODE = "udd-fte-SBResidentStatus";
    // Extended dictionary aliases - School Years context table
    public static final String ALIAS_RCD_MANDATED_COMM_INVOLMENT_HOURS = "rcd-ctx-comm-hours";

    /*
     * Student Ed. Plans
     */
    public static final String ALIAS_SEP_PLAN_VISIBILITY = "all-sep-Visibility";

    /*
     * Student Attendance
     */
    // Attendance constants
    public static final String ATT_CODE_ABSENT = "A";
    public static final String ATT_CODE_TARDY = "L";
    public static final String ATT_CODE_TARDY_FRENCH = "R";
    public static final String ATT_CODE_GENERAL_ABSENCE = "G";
    public static final String ATT_CODE_BOARD_HOLIDAY = "B";
    public static final String ATT_CODE_PROF_ACTIVITY = "PA";
    public static final String ATT_CODE_STAT_HOLIDAY = "H";
    public static final String ATT_CODE_BOARD_HOLIDAY_FRENCH = "C";
    public static final String ATT_CODE_PROF_ACTIVITY_FRENCH = "JP";
    public static final String ATT_CODE_STAT_HOLIDAY_FRENCH = "F";
    public static final String ATT_CODE_NON_INSTRUCTIONAL = "N";
    public static final String ATT_CODE_NON_INSTRUCTIONAL_FRENCH = "SE";
    public static final String ATT_CODE_PARENT_CONTACT = "C";
    public static final String ATT_CODE_PARENT_CONTACT_FRENCH = "EC";
    public static final String ATT_CODE_CON_ED_CANCELLED_UNFUNDED = "CED";
    public static final String ATT_CODE_CON_ED_CANCELLED_FUNDED = "CEC";
    public static final String ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT = "D";
    public static final String ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT = "C";
    public static final Collection<String> ATT_CODE_CON_ED_CONSECUTIVE_INCL = new ArrayList<String>(
            Arrays.asList(ATT_CODE_ABSENT, ATT_CODE_GENERAL_ABSENCE, ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT,
                    ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT));
    public static final Collection<String> ATT_CODE_ABS_CODES_OTHER = new ArrayList<String>(
            Arrays.asList(ATT_CODE_GENERAL_ABSENCE, ATT_CODE_NON_INSTRUCTIONAL, ATT_CODE_PARENT_CONTACT,
                    ATT_CODE_NON_INSTRUCTIONAL_FRENCH, ATT_CODE_PARENT_CONTACT_FRENCH));
    public static final Map<String, String> ATT_CODE_ABS_CODES_CSD_MAP = new HashMap<String, String>();

    static {
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_GENERAL_ABSENCE, ATT_CODE_GENERAL_ABSENCE);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_BOARD_HOLIDAY, ATT_CODE_BOARD_HOLIDAY);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_PROF_ACTIVITY, ATT_CODE_PROF_ACTIVITY);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_STAT_HOLIDAY, ATT_CODE_STAT_HOLIDAY);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_BOARD_HOLIDAY_FRENCH, ATT_CODE_BOARD_HOLIDAY_FRENCH);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_PROF_ACTIVITY_FRENCH, ATT_CODE_PROF_ACTIVITY_FRENCH);
        ATT_CODE_ABS_CODES_CSD_MAP.put(CSD_DAY_TYPE_STAT_HOLIDAY_FRENCH, ATT_CODE_STAT_HOLIDAY_FRENCH);
    }


    public static final Set<String> ATT_CODE_ABS_CODES_CSD_IN = ATT_CODE_ABS_CODES_CSD_MAP.keySet();

    // Attendance constants - Localization
    public static final Map<String, String> ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP = new HashMap<String, String>();
    static {
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("A", "A");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("B", "C");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("C", "EC");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("CEC", "CEC");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("CED", "CED");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("E", "E");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("G", "G");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("H", "F");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("N", "SE");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("P", "P");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("PA", "JP");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("L", "R");
        ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("S", "S");
    }
    public static final Map<String, String> ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP = new HashMap<String, String>();
    static {
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("A", "A");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("C", "B");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("EC", "C");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("CEC", "CEC");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("CED", "CED");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("E", "E");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("G", "G");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("F", "H");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("SE", "N");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("P", "P");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("JP", "PA");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("R", "L");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("S", "S");
    }

    // Attendance aliases
    public static final String ALIAS_ATT_ALL_CALCULATE_BY_CLASS = "all-att-CalculatedByClass";
    public static final String ALIAS_ATT_ALL_SYSTEM_MESSAGES = "all-att-SystemMessages";

    /*
     * Student Programs
     */
    // Student Program constants
    public static final String PROGRAM_TYPE_ELD = "ELD";
    public static final String PROGRAM_TYPE_ESL = "ESL";
    public static final String PROGRAM_TYPE_SLP = "SLP";
    public static final String EXT_OID_CIH_PROGRAM = "ddxOnCih";
    public static final String EXT_OID_ELL_PROGRAM = "ddxOnEll";
    public static final String EXT_OID_FSL_PROGRAM = "ddxOnFsl";
    public static final String EXT_OID_SAL_PROGRAM = "ddxOnSal";
    public static final String EXT_OID_SPECIAL_ED = "ddxOnSpecEd";
    // Student Program constants - SLP
    public static final String CONST_PGM_SLP_CODE_CORE = "Core";
    public static final String CONST_PGM_SLP_CODE_IMMERSION = "Immersion";
    public static final String CONST_PGM_SLP_CODE_EXTENDED = "Extended";
    public static final String CONST_PGM_SLP_CODE_EXEMPTED = "Exempted";
    public static final String CONST_PGM_SLP_TYPE_NATIVE_LANG = "NSL";
    public static final String CONST_PGM_SLP_TYPE_FRENCH = "FSL";
    public static final int CONST_PGM_SLP_TOOL_INDEX_CORE = 0;
    public static final int CONST_PGM_SLP_TOOL_INDEX_IMMERSION = 1;
    public static final int CONST_PGM_SLP_TOOL_INDEX_EXTENDED = 2;
    public static final int CONST_PGM_SLP_TOOL_INDEX_EXEMPTED = 3;

    // Student Accommodation aliases - EQAO
    public static final String ALIAS_IAC_EQAO_ACCOMMODATION = "all-iac-EqaoAccommodation";
    public static final String ALIAS_IAC_EQAO_SPECIAL_ACCOMMODATION = "all-iac-EqaoSpecAccomm";

    // Student Program aliases
    public static final String ALIAS_PGM_SCHOOL_NAME = "all-pgm-SchoolName";
    public static final String ALIAS_PGM_SCHOOL_OID = "all-pgm-SchoolOid";
    // SLP - these are defined on the Student Program table, not an extension
    public static final String ALIAS_PGM_SLP_CODE = "pgm-slp-code";
    public static final String ALIAS_PGM_SLP_TYPE = "pgm-slp-type";
    public static final String ALIAS_PGM_SLP_MINUTES_OF_INSTRUCTION = "pgm-slp-minutes-of-instruction";
    // Community Involvement Hours (STD-PGM-CIH)
    public static final String ALIAS_PGM_CIH_HOURS = "pgm-cih-hours";
    // ELL Student Program (STD-PGM-ELL)
    public static final String ALIAS_PGM_ELL_PROGRAM = "pgm-ell-program";
    public static final String ALIAS_PGM_ELL_ASSESSMENT_DATE = "pgm-ell-assessment-date";
    public static final String ALIAS_PGM_ELL_STEP = "pgm-ell-step";
    public static final String ALIAS_PGM_ELL_STEP_LISTENING = "pgm-ell-listening-step";
    public static final String ALIAS_PGM_ELL_STEP_SPEAKING = "pgm-ell-speaking-step";
    public static final String ALIAS_PGM_ELL_STEP_READING = "pgm-ell-reading-step";
    public static final String ALIAS_PGM_ELL_STEP_WRITING = "pgm-ell-writing-step";
    public static final String ALIAS_PGM_ELL_TESTER_NAME = "pgm-ell-tester-name";
    // Supervised Alternative Learning (STD-PGM-SAL)
    public static final String ALIAS_PGM_SAL_STATUS = "pgm-sal-status";
    public static final String ALIAS_PGM_SAL_EXIT_TYPE = "pgm-sal-exit-type";
    public static final String ALIAS_PGD_SAL_COMPONENT_START = "pgd-sal-component-start";
    public static final String ALIAS_PGD_SAL_COMPONENT_END = "pgd-sal-component-end";
    public static final String ALIAS_PGD_SAL_COMPONENT = "pgd-sal-component";
    // Special Education (STD-PGM-SPECED)
    public static final String ALIAS_PGM_SPECED_EXCEPTIONALITY = "pgm-speced-exceptionality";
    public static final String ALIAS_PGM_SPECED_IS_PRIMARY = "pgm-speced-pri-exceptionality";
    public static final String ALIAS_PGM_SPECED_REPORT_INDICATOR = "pgm-speced-report-ind";

    // Special Education (STD-PGD-SPECED)
    public static final String ALIAS_PGD_SPECED_IEP_REQUIRED = "pgd-speced-iep-required";
    public static final String ALIAS_PGD_SPECED_START_DATE = "pgd-speced-start-date";
    public static final String ALIAS_PGD_SPECED_END_DATE = "pgd-speced-end-date";
    public static final String ALIAS_PGD_SPECED_PLACEMENT_TYPE = "pgd-speced-placement-type";
    public static final String ALIAS_PGD_SPECED_PROGRAM_NAME = "pgd-speced-program-name";
    public static final String ALIAS_PGD_SPECED_PROGRAM_LEVEL = "pgd-speced-program-level";
    public static final String ALIAS_PGD_SPECED_PROGRAM_LOCATION = "pgd-speced-program-location";
    public static final String ALIAS_PGD_SPECED_TYPE = "pgd-speced-type";

    // Graduatiom SHSM Program Constants
    public static final String CONST_SHSM_CERTIFICATE_COMPULSORY = "Compulsory";
    public static final String CONST_SHSM_CERTIFICATE_ELECTIVE = "Elective";

    // Graduation student program aliases
    public static final String ALIAS_GSR_SPC_CODING = "all-gsr-SpcCoding";
    public static final String ALIAS_GSR_END_DATE = "all-gsr-EndDate";
    public static final String ALIAS_GSR_EXPERIENTIAL_LEARNING = "all-gsr-ExperientialLearning";
    public static final String ALIAS_GSR_SPC_ICE = "all-gsr-SpcIce";
    public static final String ALIAS_GSR_ISSUED_DATE = "all-gsr-IssuedDate";
    public static final String ALIAS_GSR_SPC_MATH_LITERACY = "all-gsr-SpcMathLiteracy";
    public static final String ALIAS_GSR_ONTARIO_SKILLS_PASSPORT = "all-gsr-OntarioSkillsPassport";
    public static final String ALIAS_GSR_REACH_AHEAD = "all-gsr-ReachAhead";
    public static final String ALIAS_GSR_START_DATE = "all-gsr-StartDate";
    public static final String ALIAS_GSR_REQUIREMENT_WAIVED = "all-gsr-SHSMRequirementsWaived";
    public static final String ALIAS_GSR_BSID_DIPLOMA_EARNED = "all-gsr-BsidDiplomaEarned";
    public static final String ALIAS_GSR_COMMUNITY_HOURS_GRANTED = "all-gsr-CommunityHoursGranted";

    // Graduation student waiver aliases
    public static final String ALIAS_GSW_WAIVER_REASON = "all-gsw-WaiverReason";

    // Graduation program aliases
    public static final String ALIAS_GPR_SHSM_SECTOR = "all-gpr-SHSMSector";
    public static final String ALIAS_GPR_AUTO_ASSIGN = "all-gpr-AutoAssignSecondaryStudents";
    public static final String ALIAS_GPR_DIPLOMA_TYPE = "all-gpr-DiplomaType";
    public static final String ALIAS_GPR_SHSM_PROGRAM_TYPE = "all-gpr-SHSMProgramType";
    public static final String ALIAS_GPR_SHSM_PATHWAY = "all-gpr-SHSMDestination";

    /*
     * Schedule bell
     */
    // Schedule bell aliases
    public static final String ALIAS_ALL_BELL_MIDDAY_PM_START_TIME = "all-bel-MiddayPmStartTime";

    // Schedule bell period aliases
    public static final String ALIAS_BPE_PERIOD_ROLL_UP_MAP = "all-bpe-PeriodRollUpMap";

    // Graduation requirement aliases
    public static final String GRQ_EQUIVALENT_COURSE = "all-grq-EquivalentCourse";
    public static final String ALIAS_GRQ_END_DATE = "all-grq-EndDate";
    public static final String ALIAS_GRQ_START_DATE = "all-grq-StartDate";
    public static final String ALIAS_GRQ_SEQUENCE_ORDER = "all-grq-sequenceOrder";

    /*
     * Grade Term Date
     */
    // Grade Term Date aliases
    public static final String ALIAS_GTA_ELEM_PROGRESS_REPORT_DATE = "all-gta-ElementaryProgressReportDate";
    public static final String ALIAS_GTA_FULL_DISCLOSURE_DATE = "all-gta-FullDisclosureDate";
    public static final String ALIAS_GTA_GENERAL_MSG_1 = "all-gta-GeneralMessage1";
    public static final String ALIAS_GTA_GENERAL_MSG_2 = "all-gta-GeneralMessage2";
    public static final String ALIAS_GTA_GENERAL_MSG_3 = "all-gta-GeneralMessage3";

    /*
     * Student Assessment
     */
    // Assessment constants
    public static final String CONST_ASM_OSSLT_RESULT_SUCCESS_ENGLISH = "1";
    public static final String CONST_ASM_OSSLT_RESULT_SUCCESS_FRENCH = "2";
    public static final String CONST_ASM_OSSLT_RESULT_OLC_CCL = "6";
    public static final String CONST_ASM_OSSLT_RESULT_NA = "3";
    public static final String CONST_ASM_OSSLT_RESULT_ADJUDICATION = "4";
    public static final Collection<String> CONST_ASM_OSSLT_COMPLETE = new ArrayList<String>(
            Arrays.asList(CONST_ASM_OSSLT_RESULT_SUCCESS_ENGLISH, CONST_ASM_OSSLT_RESULT_SUCCESS_FRENCH,
                    CONST_ASM_OSSLT_RESULT_OLC_CCL, CONST_ASM_OSSLT_RESULT_ADJUDICATION));
    public static final Collection<String> CONST_ASM_OSSLT_NA = new ArrayList<String>(
            Arrays.asList(CONST_ASM_OSSLT_RESULT_NA));
    public static final String CONST_ASM_OSSLT_LANGUAGE_ENGLISH = "1";
    public static final String CONST_ASM_OSSLT_LANGUAGE_FRENCH = "2";
    public static final String CONST_ASM_SHSM_COMPULSORY_CERT_SUBST = "1";
    public static final String CONST_ASM_SHSM_ELECTIVE_CERT_SUBST = "2";

    // Assessment Definition constants
    public static final String CONST_ASD_OSSLT_OID = "asd000000OSSLT";
    public static final String CONST_ASD_COMM_INVOLVE_OID = "asd00000000CIH";

    // Student Assessment aliases
    public static final String ALIAS_ASM_OSSLT_RESULT = "asm-osslt-outcome";
    public static final String ALIAS_ASM_SHSM_ATTAINED = "asm-shsm-attained";
    public static final String ALIAS_ASM_SHSM_CERTIFICATE = "asm-shsm-certificate";
    public static final String ALIAS_ASM_SHSM_HOURS = "asm-shsm-hours";
    public static final String ALIAS_ASM_SHSM_SUBSTITUTION = "asm-shsm-substitution";
    public static final String ALIAS_ASM_CIH_HOURS = "asm-cih-hours";
    public static final String ALIAS_ASM_OSSLT_LANGUAGE = "asm-osslt-language";

    /*
     * Grade terms, courses
     */
    // Grade Term definition constants - Elementary
    public static final String GTM_ELEM_TERM_DEF_NAME = "Elementary Reporting Periods";
    public static final Integer GTM_ELEM_TERM_NUM_WINTER = Integer.valueOf(1);
    public static final Integer GTM_ELEM_TERM_NUM_SPRING = Integer.valueOf(2);

    // Course constants
    public static final int CONST_CRS_LEN_OSSLT = 5;
    public static final Collection<String> CONST_CRS_PREFIX_OSSLT_LIST = new ArrayList<String>(
            Arrays.asList("OLC3O", "OLC4O", "CCL3O", "CCL4O"));
    public static final Collection<String> CONST_CRS_PREFIX_OSSLT_ENGLISH_LIST = new ArrayList<String>(
            Arrays.asList("OLC3O", "OLC4O"));
    public static final Collection<String> CONST_CRS_PREFIX_OSSLT_FRENCH_LIST = new ArrayList<String>(
            Arrays.asList("CCL3O", "CCL4O"));

    public static final int CONST_CRS_TYPE_POS = 5;
    public static final Collection<String> CONST_CRS_TYPE_T_LIST = new ArrayList<String>(Arrays.asList("T", "Y"));
    public static final int CONST_CRS_GRD_LVL_POS = 4;
    public static final String CONST_CRS_GRD_LVL_09_ONT = "1";
    public static final String CONST_CRS_GRD_LVL_10_ONT = "2";
    public static final String CONST_CRS_GRD_LVL_11_ONT = "3";
    public static final String CONST_CRS_GRD_LVL_12_ONT = "4";
    public static final String CONST_CRS_GRD_LVL_13_ONT = "5";
    public static final String CONST_CRS_GRD_LVL_13_0_ONT = "0";
    // Constant value - map of hs grade level to Ontario course grade levels
    public static final Map<String, String> CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT = new HashMap<String, String>();

    static {
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_09_ONT, "09");
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_10_ONT, "10");
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_11_ONT, "11");
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_12_ONT, "12");
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_13_ONT, "13");
        CONST_HS_GRD_LVL_TO_CRS_GRD_LVL_ONT.put(CONST_CRS_GRD_LVL_13_0_ONT, "13");
    }

    public static final String CONST_CRS_DELIVERY_TYPE_CORR = "16";
    public static final String CONST_CRS_DELIVERY_TYPE_ELEARN_NOTLMS = "18";
    public static final String CONST_CRS_DELIVERY_TYPE_ELEARN_LMS = "19";
    public static final String CONST_CRS_DELIVERY_TYPE_IND_STUDY = "2";
    public static final Collection<String> CONST_CRS_DELIVERY_TYPE_DUAL_CRED_LIST = new ArrayList<String>(
            Arrays.asList("12", "13"));
    public static final Collection<String> CONST_CRS_DELIVERY_TYPE_COOP_LIST = new ArrayList<String>(
            Arrays.asList("4"));
    public static final Collection<String> CONST_CRS_CREDIT_TYPE_COOP_LIST = new ArrayList<String>(Arrays.asList("15"));
    public static final String CONST_CRS_PLE_PRFX = "PLE";
    public static final String CONST_CRS_QEE_PRFX = "QEE";
    public static final String CONST_CRS_QAP_PRFX = "QAP";
    public static final String CONST_CRS_QMA_PRFX = "QMA";
    public static final String CONST_CRS_QSE_PRFX = "QSE";

    // Course aliases
    public static final String ALIAS_CRS_MINISTRY_CRS_CODE = "all-crs-MinistryCourseCode";
    public static final String ALIAS_CRS_DELIVERY_TYPE = "all-crs-CourseDeliveryType";
    public static final String ALIAS_CRS_CREDIT_TYPE = "all-crs-CreditType";
    public static final String ALIAS_CRS_CODE_TYPE = "all-crs-CourseCodeType";
    public static final String ALIAS_CRS_LANGUAGE_OF_INSTRUCTION = "all-crs-LanguageOfInstruction";
    public static final String ALIAS_CRS_COURSE_PART = "all-crs-CoursePart";
    public static final String ALIAS_CRS_CLASS_TYPE = "all-crs-ClassType";
    public static final String ALIAS_CRS_MUTUALLY_EXCLUSIVE_COURSE_CODE = "all-crs-MutuallyExclusiveCourseCode";

    // School Course aliases
    public static final String ALIAS_CSK_MEDIAN_LAST_CALC_TS = "all-csk-termMedianLastCalculatedTs";
    public static final String ALIAS_CSK_MEDIAN_TERM_A = "all-csk-medianTermA";
    public static final String ALIAS_CSK_MEDIAN_TERM_B = "all-csk-medianTermB";
    public static final String ALIAS_CSK_MEDIAN_TERM_C = "all-csk-medianTermC";
    public static final String ALIAS_CSK_COURSE_DELIVERY_TYPE = "all-csk-CourseDeliveryType";
    public static final String ALIAS_CSK_COURSE_OFFERING_TYPE = "all-csk-CourseOfferingType";
    public static final String ALIAS_CSK_CON_ED_PROGRAM_TYPE = "all-csk-ConEdProgramType";
    public static final String ALIAS_CSK_INTL_LANGUAGE = "all-csk-IntlLanguage";
    public static final String ALIAS_CSK_COURSE_PART_EXCEPTION = "all-csk-CoursePartException";
    public static final String ALIAS_CSK_EXCLUDE_FROM_SHSM_REQUIREMENTS = "all-csk-ExcludeCSKFromSHMSRequirements";
    public static final String ALIAS_CSK_OTHER_COURSE_INFORMATION_TYPE = "all-csk-OtherCourseInformationType";
    public static final String ALIAS_CSK_COURSE_PART = "all-csk-CoursePart";
    public static final String ALIAS_CSK_RELIGIOUS_COMMENT_INDICATOR = "all-csk-ReligiousCommentIndicator";
    public static final String ALIAS_CSK_LANGUAGE_OF_INSTRUCTION = "all-csk-LanguageOfInstruction";
    public static final String ALIAS_CSK_CLASS_TYPE = "all-csk-ClassType";
    public static final String ALIAS_CSK_NUMBER_LESSONS = "all-csk-Lessons";
    public static final String ALIAS_CSK_OTHER_COURSE_INFO_TYPE = "all-csk-OtherCourseInformationType";

    /*
     * Gradebook, assignment, score
     */
    // Assignment aliases
    public static final String ALIAS_GCD_CON_ED_REGISTER_INCL_ON = "all-gcd-IncludeonConEdRegister";

    /*
     * Transcript definitions, rubrics, transcripts
     */
    // Transcript definition constants - Elementary
    public static final String TRN_COL_HDR_ELEM_PRG_CMT = "PrgTxt";
    public static final String TRN_COL_HDR_ELEM_PRG_CMT_REL = "Religious Family Life Ed";
    public static final String TRN_COL_HDR_ELEM_PRG_RUBRIC = "PrgRubric";
    public static final String TRN_COL_HDR_ELEM_RETAINED = "Retained";
    public static final String TRN_COL_HDR_ELEM_TRM1_CMT = "S1Txt";
    public static final String TRN_COL_HDR_ELEM_TRM1_CMT_REL = "Religious Family Life Ed";
    public static final String TRN_COL_HDR_ELEM_TRM1_RUBRIC = "S1Rubric";
    public static final String TRN_COL_HDR_ELEM_TRM2_CMT = "S2Txt";
    public static final String TRN_COL_HDR_ELEM_TRM2_CMT_REL = "Religious Family Life Ed";
    public static final String TRN_COL_HDR_ELEM_TRM2_RUBRIC = "S2Rubric";

    // Transcript definition constants - Secondary
    public static final String TRN_COL_HDR_SEC_FG = "FM";
    public static final String TRN_COL_HDR_SEC_Q1 = "S1T1Mrk";
    public static final String TRN_COL_HDR_SEC_Q2 = "S1T2Mrk";
    public static final String TRN_COL_HDR_SEC_Q3 = "S2T1Mrk";
    public static final String TRN_COL_HDR_SEC_Q4 = "S2T2Mrk";
    public static final String TRN_COL_HDR_SEC_NON_SEM_1ST = "FY1stMrk";
    public static final String TRN_COL_HDR_SEC_NON_SEM_2ND = "FY2ndMrk";
    public static final String TRN_COL_HDR_SEC_NON_SEM_FINAL = "FYFinMrk";
    public static final String TRN_COL_HDR_QUAD_Q1_T1 = "Q1T1Mrk";
    public static final String TRN_COL_HDR_QUAD_Q1_T2 = "Q1T2Mrk";
    public static final String TRN_COL_HDR_QUAD_Q2_T1 = "Q2T1Mrk";
    public static final String TRN_COL_HDR_QUAD_Q2_T2 = "Q2T2Mrk";
    public static final String TRN_COL_HDR_QUAD_Q3_T1 = "Q3T1Mrk";
    public static final String TRN_COL_HDR_QUAD_Q3_T2 = "Q3T2Mrk";
    public static final String TRN_COL_HDR_QUAD_Q4_T1 = "Q4T1Mrk";
    public static final String TRN_COL_HDR_QUAD_Q4_T2 = "Q4T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC1_T1 = "OC1T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC1_T2 = "OC1T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC2_T1 = "OC2T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC2_T2 = "OC2T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC3_T1 = "OC3T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC3_T2 = "OC3T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC4_T1 = "OC4T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC4_T2 = "OC4T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC5_T1 = "OC5T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC5_T2 = "OC5T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC6_T1 = "OC6T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC6_T2 = "OC6T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC7_T1 = "OC7T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC7_T2 = "OC7T2Mrk";
    public static final String TRN_COL_HDR_OCTO_OC8_T1 = "OC8T1Mrk";
    public static final String TRN_COL_HDR_OCTO_OC8_T2 = "OC8T2Mrk";
    public static final String TRN_COL_HDR_SEC_RUBRIC_Q1 = "S1T1Rubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_Q2 = "S1T2Rubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_Q3 = "S2T1Rubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_Q4 = "S2T2Rubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_NON_SEM_1ST = "FY1stRbc";
    public static final String TRN_COL_HDR_SEC_RUBRIC_NON_SEM_2ND = "FY2ndRbc";
    public static final String TRN_COL_HDR_SEC_RUBRIC_NON_SEM_FINAL = "FYFinRbc";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q1_T1 = "Q1T1Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q1_T2 = "Q1T2Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q2_T1 = "Q2T1Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q2_T2 = "Q2T2Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q3_T1 = "Q3T1Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q3_T2 = "Q3T2Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q4_T1 = "Q4T1Rubric";
    public static final String TRN_COL_HDR_QUAD_RUBRIC_Q4_T2 = "Q4T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC1_T1 = "OC1T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC1_T2 = "OC1T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC2_T1 = "OC2T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC2_T2 = "OC2T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC3_T1 = "OC3T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC3_T2 = "OC3T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC4_T1 = "OC4T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC4_T2 = "OC4T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC5_T1 = "OC5T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC5_T2 = "OC5T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC6_T1 = "OC6T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC6_T2 = "OC6T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC7_T1 = "OC7T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC7_T2 = "OC7T2Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC8_T1 = "OC8T1Rubric";
    public static final String TRN_COL_HDR_OCTO_RUBRIC_OC8_T2 = "OC8T2Rubric";
    public static final String TRN_COL_HDR_SEC_CMT_Q1 = "S1T1Com";
    public static final String TRN_COL_HDR_SEC_CMT_Q2 = "S1T2Com";
    public static final String TRN_COL_HDR_SEC_CMT_Q3 = "S2T1Com";
    public static final String TRN_COL_HDR_SEC_CMT_Q4 = "S2T2Com";
    public static final String TRN_COL_HDR_SEC_NON_SEM_CMT_1ST = "FY1Com";
    public static final String TRN_COL_HDR_SEC_NON_SEM_CMT_2ND = "FY2Com";
    public static final String TRN_COL_HDR_SEC_NON_SEM_CMT_FINAL = "FYFinCom";
    public static final String TRN_COL_HDR_QUAD_CMT_Q1_T1 = "Q1T1Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q1_T2 = "Q1T2Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q2_T1 = "Q2T1Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q2_T2 = "Q2T2Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q3_T1 = "Q3T1Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q3_T2 = "Q3T2Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q4_T1 = "Q4T1Com";
    public static final String TRN_COL_HDR_QUAD_CMT_Q4_T2 = "Q4T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC1_T1 = "OC1T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC1_T2 = "OC1T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC2_T1 = "OC2T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC2_T2 = "OC2T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC3_T1 = "OC3T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC3_T2 = "OC3T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC4_T1 = "OC4T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC4_T2 = "OC4T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC5_T1 = "OC5T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC5_T2 = "OC5T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC6_T1 = "OC6T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC6_T2 = "OC6T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC7_T1 = "OC7T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC7_T2 = "OC7T2Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC8_T1 = "OC8T1Com";
    public static final String TRN_COL_HDR_OCTO_CMT_OC8_T2 = "OC8T2Com";
    public static final String TRN_COL_HDR_SEC_IND_ESL_Q1 = "S1T1ELL";
    public static final String TRN_COL_HDR_SEC_IND_ESL_Q2 = "S1T2ELL";
    public static final String TRN_COL_HDR_SEC_IND_ESL_Q3 = "S2T1ELL";
    public static final String TRN_COL_HDR_SEC_IND_ESL_Q4 = "S2T2ELL";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_ESL_1ST = "FY1ELL";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_ESL_2ND = "FY2ELL";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_ESL_FINAL = "FYFinELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q1_T1 = "Q1T1ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q1_T2 = "Q1T2ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q2_T1 = "Q2T1ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q2_T2 = "Q2T2ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q3_T1 = "Q3T1ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q3_T2 = "Q3T2ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q4_T1 = "Q4T1ELL";
    public static final String TRN_COL_HDR_QUAD_IND_ESL_Q4_T2 = "Q4T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC1_T1 = "OC1T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC1_T2 = "OC1T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC2_T1 = "OC2T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC2_T2 = "OC2T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC3_T1 = "OC3T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC3_T2 = "OC3T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC4_T1 = "OC4T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC4_T2 = "OC4T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC5_T1 = "OC5T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC5_T2 = "OC5T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC6_T1 = "OC6T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC6_T2 = "OC6T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC7_T1 = "OC7T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC7_T2 = "OC7T2ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC8_T1 = "OC8T1ELL";
    public static final String TRN_COL_HDR_OCTO_IND_ESL_OC8_T2 = "OC8T2ELL";
    public static final String TRN_COL_HDR_SEC_IND_IEP_Q1 = "S1T1IEP";
    public static final String TRN_COL_HDR_SEC_IND_IEP_Q2 = "S1T2IEP";
    public static final String TRN_COL_HDR_SEC_IND_IEP_Q3 = "S2T1IEP";
    public static final String TRN_COL_HDR_SEC_IND_IEP_Q4 = "S2T2IEP";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_IEP_1ST = "FY1IEP";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_IEP_2ND = "FY2IEP";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_IEP_FINAL = "FYFinIEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q1_T1 = "Q1T1IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q1_T2 = "Q1T2IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q2_T1 = "Q2T1IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q2_T2 = "Q2T2IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q3_T1 = "Q3T1IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q3_T2 = "Q3T2IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q4_T1 = "Q4T1IEP";
    public static final String TRN_COL_HDR_QUAD_IND_IEP_Q4_T2 = "Q4T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC1_T1 = "OC1T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC1_T2 = "OC1T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC2_T1 = "OC2T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC2_T2 = "OC2T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC3_T1 = "OC3T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC3_T2 = "OC3T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC4_T1 = "OC4T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC4_T2 = "OC4T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC5_T1 = "OC5T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC5_T2 = "OC5T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC6_T1 = "OC6T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC6_T2 = "OC6T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC7_T1 = "OC7T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC7_T2 = "OC7T2IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC8_T1 = "OC8T1IEP";
    public static final String TRN_COL_HDR_OCTO_IND_IEP_OC8_T2 = "OC8T2IEP";
    public static final String TRN_COL_HDR_SEC_IND_INTERVIEW_Q1 = "S1T1Interview?";
    public static final String TRN_COL_HDR_SEC_IND_INTERVIEW_Q3 = "S2T1Interview?";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_INTERVIEW_1ST = "FY1stIntvw";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_INTERVIEW_2ND = "FY2ndIntvw";
    public static final String TRN_COL_HDR_SEC_NON_SEM_IND_INTERVIEW_FINAL = "FYFinIntvw";
    public static final String TRN_COL_HDR_QUAD_IND_INTERVIEW_Q1 = "Q1T1Interview?";
    public static final String TRN_COL_HDR_QUAD_IND_INTERVIEW_Q2 = "Q2T1Interview?";
    public static final String TRN_COL_HDR_QUAD_IND_INTERVIEW_Q3 = "Q3T1Interview?";
    public static final String TRN_COL_HDR_QUAD_IND_INTERVIEW_Q4 = "Q4T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC1 = "OC1T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC2 = "OC2T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC3 = "OC3T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC4 = "OC4T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC5 = "OC5T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC6 = "OC6T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC7 = "OC7T1Interview?";
    public static final String TRN_COL_HDR_OCTO_IND_INTERVIEW_OC8 = "OC8T1Interview?";
    public static final String TRN_COL_HDR_SEC_CON_ED_FY_CMT = "FYCom";

    // Continue Ed: FY and Non-Semester courses
    public static final String TRN_COL_HDR_SEC_RUBRIC_FY = "FYRubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_T3 = "S1T3Rubric";
    public static final String TRN_COL_HDR_SEC_RUBRIC_T6 = "S2T3Rubric";

    // Rubric constants - Elementary
    public static final String RBD_ID_LEARN_SKILLS = "Standard";
    public static final String RBD_ID_NATIVE_LANG = "NtvLng";
    public static final String RBD_CRIT_NAME_ELEM_GRD_PRG = "Progress";
    public static final String RBD_CRIT_NAME_ELEM_GRD_TRM = "Evaluation";
    public static final String RBD_CRIT_NAME_ELEM_IND_ESL = "ESL/ELD";
    public static final String RBD_CRIT_NAME_ELEM_IND_IEP = "IEP";
    public static final String RBD_CRIT_NAME_ELEM_IND_FRENCH = "French";
    public static final String RBD_CRIT_NAME_ELEM_IND_NA = "NA";
    public static final String RBD_CRIT_NAME_ELEM_IND_CORE = "Core";
    public static final String RBD_CRIT_NAME_ELEM_IND_IMMERSION = "Immersion";
    public static final String RBD_CRIT_NAME_ELEM_IND_EXTENDED = "Extended";
    public static final Collection<String> RBD_CRIT_NAME_ELEM_IND_LIST = new ArrayList<String>(
            Arrays.asList(RBD_CRIT_NAME_ELEM_IND_ESL, RBD_CRIT_NAME_ELEM_IND_IEP, RBD_CRIT_NAME_ELEM_IND_FRENCH,
                    RBD_CRIT_NAME_ELEM_IND_NA, RBD_CRIT_NAME_ELEM_IND_CORE, RBD_CRIT_NAME_ELEM_IND_IMMERSION,
                    RBD_CRIT_NAME_ELEM_IND_EXTENDED));
    public static final String RBD_CRIT_NAME_ELEM_LRN_RESP = "Responsibility";
    public static final String RBD_CRIT_NAME_ELEM_LRN_ORG = "Organization";
    public static final String RBD_CRIT_NAME_ELEM_LRN_IND = "Independent Work";
    public static final String RBD_CRIT_NAME_ELEM_LRN_COLLAB = "Collaboration";
    public static final String RBD_CRIT_NAME_ELEM_LRN_INIT = "Initiative";
    public static final String RBD_CRIT_NAME_ELEM_LRN_SELF = "Self-Regulation";
    public static final Collection<String> RBD_CRIT_NAME_ELEM_LRN_LIST = new ArrayList<String>(
            Arrays.asList(RBD_CRIT_NAME_ELEM_LRN_RESP, RBD_CRIT_NAME_ELEM_LRN_ORG, RBD_CRIT_NAME_ELEM_LRN_IND,
                    RBD_CRIT_NAME_ELEM_LRN_COLLAB, RBD_CRIT_NAME_ELEM_LRN_INIT, RBD_CRIT_NAME_ELEM_LRN_SELF));

    // Rubric rating values - ELementary
    public static final String GRD_ELM_PRG_RUBRIC_PROG_1 = "d";
    public static final String GRD_ELM_PRG_RUBRIC_PROG_2 = "w";
    public static final String GRD_ELM_PRG_RUBRIC_PROG_3 = "v";
    public static final String GRD_ELM_PRG_ELEM_IND_X = "x";

    // Transcript aliases
    public static final String ALIAS_TRN_COURSE_REPEATED = "all-trn-CourseRepeated";
    public static final String ALIAS_TRN_COMPULSARY_REQT_MET = "all-trn-CompulsoryRequirementMet";
    public static final String ALIAS_TRN_DATE_COMPLETED = "all-trn-DateCompleted";
    public static final String ALIAS_TRN_PLE_COURSE_CODE = "all-trn-PLECourseCode";
    public static final String ALIAS_TRN_CREDIT_EXAMPT = "all-trn-CreditExemptFromHighCreditThreshold";
    public static final String ALIAS_TRN_FRENCH_HOURS = "all-trn-FrenchHours";
    public static final String ALIAS_TRN_FRENCH_OTHER_HOURS = "all-trn-FrenchOtherHours";
    public static final String ALIAS_TRN_COURSE_WITHDRAW_DATE = "all-trn-CourseWithdrawDate";
    public static final String ALIAS_TRN_S1T1_MARK = "all-trn-S1T1Mark";
    public static final String ALIAS_TRN_S1T2_MARK = "all-trn-S1T2Mark";
    public static final String ALIAS_TRN_S2T1_MARK = "all-trn-S2T1Mark";
    public static final String ALIAS_TRN_S2T2_MARK = "all-trn-S2T2Mark";
    public static final String ALIAS_TRN_SHSM_REQUIREMENT = "all-trn-MeetsShsmRequirement";
    public static final String ALIAS_TRN_SHSM_REQ_BY_PRIORITY = "all-trn-SHSMRequirementsByPriority";
    public static final String ALIAS_TRN_SHSM_CREDIT_BY_REQ_DETAIL = "all-trn-SHSMRequirementsMet";
    public static final String ALIAS_TRN_PRIVATE_SCHOOL_REQUIREMENT = "all-trn-CreditEarnedAtPrivateSchool";
    public static final String ALIAS_TRN_ALTERNATE_EXPECTATIONS = "all-trn-AlternateExpectations";
    public static final String ALIAS_TRN_LANGUAGE_OF_INSTR_OVERRIDE = "all-trn-LanguageOfInstructionOverride";
    public static final String ALIAS_TRN_MODIFIED_EXPECTATIONS = "all-trn-ModifiedExpectations";
    public static final String ALIAS_TRN_SPECIAL_INDICATOR = "all-trn-SpecialIndicator";
    public static final String ALIAS_TRN_COURSE_DELIVERY_TYPE_OVERRIDE = "all-trn-CourseDeliveryTypeOverride";
    public static final String ALIAS_TRN_BSID_CREDIT_EARNED = "all-trn-BsidCreditEarned";
    public static final String ALIAS_TRN_CREDIT_EARNED_PRIVATE_SCHOOL = "all-trn-CreditEarnedAtPrivateSchool";
    public static final String ALIAS_TRN_COURSE_WITHDRAWN = "all-trn-WithdrawnCourse";
    public static final String ALIAS_TRN_PLAR_TYPE = "all-trn-PLARType";
    public static final String ALIAS_TRN_PLAR_STATUS = "all-trn-PLARStatus";
    public static final String ALIAS_TRN_COURSE_PART = "all-trn-CoursePart";
    public static final String ALIAS_TRN_COURSE_PART_OVERRIDE = "all-trn-CoursePartOverride";
    public static final String ALIAS_TRN_CREDIT_BY_REQ_DETAIL = "all-trn-CreditByReqDetail";
    public static final String ALIAS_TRN_REQ_BY_PRIORITY = "all-trn-ReqByPriority";
    public static final String ALIAS_TRN_SUBSTITUTE_CREDIT_IND = "all-trn-SubstituteCreditInd";
    public static final String ALIAS_TRN_SUBSTITUTE_ORIGINAL_CREDITS = "all-trn-SubstituteOriginalCredits";
    public static final String ALIAS_TRN_COURSE_INCOMPLETE = "all-trn-CourseIncomplete";
    public static final String ALIAS_TRN_EARLY_COMPLETION = "all-trn-EarlyCompletion";
    public static final String ALIAS_TRN_COURSE_PART_EXCEPTION_OVERRIDE = "all-trn-CoursePartExceptionOverride";
    public static final String ALIAS_TRN_COMPULSORY_CREDITE_APPLIED_FOR_PLE = "all-trn-CompulsoryCreditApplied";
    public static final String ALIAS_TRN_CREDIT_TYPE = "all-trn-CreditType";
    public static final String ALIAS_TRN_ONLINE_LEARNING_CREDIT = "all-trn-OnlineLearningCredit";
    public static final String ALIAS_TRN_OTHER_COURSE_INFO_TYPE_OVERRIDE = "all-trn-OtherCourseInformationTypeOverride";

    /*
     * Schedules
     */
    // Master Schedule
    public static final String ALL_MST_CLASS_TYPE = "all-mst-ClassType";
    public static final String ALL_MST_INSTITUTION = "all-mst-InstitutionTypeCode";
    public static final String ALL_MST_LANGUAGE_OF_INSTRUCTION = "all-mst-LanguageOfInstruction";
    public static final String ALIAS_MST_CON_ED_OFFERING_TYPE = "all-mst-ContinuingEducationOfferingType";
    public static final String ALIAS_MST_CON_ED_START_DATE = "all-mst-ContinuingEducationStartDate";
    public static final String ALIAS_MST_CON_ED_END_DATE = "all-mst-ContinuingEducationEndDate";
    public static final String ALIAS_MST_CON_ED_START_TIME = "all-mst-ContinuingEducationStartTime";
    public static final String ALIAS_MST_CON_ED_END_TIME = "all-mst-ContinuingEducationEndTime";
    public static final String ALIAS_MST_CON_ED_LENGTH_MINS = "all-mst-ContinuingEducationMinutes";
    public static final String ALIAS_MST_FULL_DISCLOSURE_REPORT_DATE_OVERRIDE =
            "all-mst-FullDisclosureReportDateOverride";
    public static final String ALIAS_MST_TIME_OF_DAY_CODE = "all-mst-TimeOfDay";

    // Master Schedule constants
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_01_NIGHT_WEEKEND = "01";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_02_DAY = "02";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_03_AFTER_SKL = "03";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_04_CREDIT_ON_MATH = "04";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_05_ADDITIONAL_PREP = "05";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_13_7_8_REMEDIAL_LIT_NUM = "13";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_14_NON_CREDIT_9_10_REMEDIAL_LIT_NUM = "14";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_15_LIT_NUM_PARENTS_GUARDIANS = "15";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_07_SUMMER_CREDIT_COURSE = "07";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_08_SUMMER_CREDIT_ON_MATH = "08";
    public static final String CONST_MST_CON_ED_OFFERING_TYPE_09_SUMMER_ADDITIONAL_PREP = "09";
    public static final List<String> CONST_MST_CON_ED_OFFERING_SUMMER = new ArrayList(
            Arrays.asList(CONST_MST_CON_ED_OFFERING_TYPE_07_SUMMER_CREDIT_COURSE,
                    CONST_MST_CON_ED_OFFERING_TYPE_08_SUMMER_CREDIT_ON_MATH,
                    CONST_MST_CON_ED_OFFERING_TYPE_09_SUMMER_ADDITIONAL_PREP));
    public static final String CONST_MST_TIME_OF_DAY_CODE_1_REG_DAY = "1";
    public static final String CONST_MST_TIME_OF_DAY_CODE_2_SUMMER = "2";
    public static final String CONST_MST_TIME_OF_DAY_CODE_3_WEEKENDS = "3";
    public static final String CONST_MST_TIME_OF_DAY_CODE_4_AFTER_SKL = "4";
    public static final String CONST_MST_TIME_OF_DAY_CODE_5_EXTENDED_SKL_DAY = "5";
    public static final String CONST_MST_TIME_OF_DAY_CODE_6_BEF_SKL_OR_LUNCH = "6";
    public static final String CONST_MST_TIME_OF_DAY_CODE_7_LATE_AFT_OR_EVE = "7";
    public static final List<String> CONST_MST_TIME_OF_DAY_SUMMER = new ArrayList(
            Arrays.asList(CONST_MST_TIME_OF_DAY_CODE_2_SUMMER));

    // Master Schedule Teacher
    public static final String ALIAS_MCT_START_DATE = "all-mtc-StartDate";
    public static final String ALIAS_MCT_END_DATE = "all-mtc-EndDate";

    /*
     * Navigation constants
     */
    // Staff view
    public static final String CONST_STF_VW_NAV_CLASS_LIST = "gradebook.classes.list";
    public static final String CONST_STF_VW_NAV_CLASS_LIST_IN = "gradebook.classes.list.input";
    public static final String CONST_STF_VW_NAV_CLASS_LIST_IN_DTL = "gradebook.classes.list.input.detail";

    /*
     * Methods
     */
    /**
     * Build map of value to key for constant values above Will return null for any
     * invalid key.
     *
     * @param keys - List<String>
     * @return Map<String, String> - Values to Keys map
     */
    public static Map<String, String> buildStaticValuesMap(List<String> keys) {
        Map<String, String> valuesMap = new HashMap<String, String>();

        for (String key : keys) {
            String value = getStaticValue(key);

            valuesMap.put(key, value);
        }

        return valuesMap;
    }

    /**
     * Return constant value - uses reflection to avoid looking for specific
     * constants Exceptions are caught (required) but not logged - null is returned.
     *
     * @param key the key
     * @return String - value
     */
    public static String getStaticValue(String key) {
        Field keyField = null;
        try {
            keyField = OntarioAlias.class.getDeclaredField(key);
        } catch (NoSuchFieldException nsfe) {
            return null;
        } catch (SecurityException se) {
            return null;
        }

        String value = null;
        try {
            if (keyField.getType().getName().contains("Integer")) {
                value = (keyField.get(null).toString());
            } else if (keyField.getType().getName().contains("String")) {
                value = (String) keyField.get(null);
            }
        } catch (IllegalAccessException iae) {
            return null;
        }

        return value;
    }
}