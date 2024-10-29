/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * This class is used by the State of New Jersey for printing the IEP Reports.
 *
 * @author Follett Software Company
 *
 */
public class IepFormData extends BaseFormReportJavaSource {
    private IepData m_currentIep = null;
    private Boolean m_calcService;

    /**
     * The variables below are used for setting the sub report streams.
     */
    private Report m_reportAssessment = null;
    private Report m_reportBehavioralInt = null;
    private Report m_reportCoverPageNames = null;
    private Report m_reportGoals = null;
    private Report m_reportInterAgency = null;
    private Report m_reportObjectives = null;
    private Report m_reportPresentLevels = null;
    private Report m_reportRelatedServices = null;
    private Report m_reportRights = null;
    private Report m_reportServices = null;
    private Report m_reportSpedServices = null;
    private Report m_reportTransitionPlanning = null;
    private Report m_reportTransSvcs = null;
    private Report m_reportModifSupplAids = null;

    /**
     * The variables below are used for setting the grids of each sub report.
     */
    private ReportDataGrid m_assessmentGrid = null;
    private ReportDataGrid m_behavioralIntGrid = null;
    private ReportDataGrid m_coverPageNamesGrid = null;
    private ReportDataGrid m_goalsGrid = null;
    private ReportDataGrid m_interAgencyGrid = null;
    private ReportDataGrid m_objectivesGrid = null;
    private ReportDataGrid m_presentLevelsGrid = null;
    private ReportDataGrid m_relServicesGrid = null;
    private ReportDataGrid m_rightsGrid = null;
    private ReportDataGrid m_servicesGrid = null;
    private ReportDataGrid m_spedServicesGrid = null;
    private ReportDataGrid m_transPlanningGrid = null;
    private ReportDataGrid m_transServicesGrid = null;
    private ReportDataGrid m_modifSupplAidsGrid = null;

    /**
     * Reference code maps
     */
    private Map<String, ReferenceCode> m_transitionSvcsRefCodes = null;
    private Map<String, ReferenceCode> m_placementDecisionRefCodes = null;
    private Map<String, ReferenceCode> m_serviceSettingRefCodes = null;
    private Map<String, ReferenceCode> m_serviceCycleRefCodes = null;

    /**
     * The variables below are the alias variables that are used in the printed report.
     */
    private static final String ALIAS_ACCOMMODATION_MODIFICATION = "iep-accomodation-modification";
    private static final String ALIAS_ADDITIONAL_LIAISON = "iep-liaison-name";
    private static final String ALIAS_ALT_ASSESS_TYPE = "iep-alt-assess-type";
    private static final String ALIAS_APA_COURSE1 = "iep-apa-course1";
    private static final String ALIAS_APA_COURSE2 = "iep-apa-course2";
    private static final String ALIAS_APA_COURSE3 = "iep-apa-course3";
    private static final String ALIAS_ASSESSMENT_TYPE = "iep-assess-assess-procedu";
    private static final String ALIAS_COURSE_COURSES1 = "iep-course-courses1";
    private static final String ALIAS_COURSE_COURSES2 = "iep-course-courses2";
    private static final String ALIAS_COURSE_COURSES3 = "iep-course-courses3";
    private static final String ALIAS_COURSE_COURSES4 = "iep-course-courses4";
    private static final String ALIAS_COURSE_GRADE1 = "iep-course-grade1";
    private static final String ALIAS_COURSE_GRADE2 = "iep-course-grade2";
    private static final String ALIAS_COURSE_GRADE3 = "iep-course-grade3";
    private static final String ALIAS_COURSE_GRADE4 = "iep-course-grade4";
    private static final String ALIAS_DATA_COLLECT_PROC = "iep-data-collect-proc";
    private static final String ALIAS_DEVELOPMENT_SRC = "iep-development-src";
    private static final String ALIAS_DEVELOPMENT_SRC_STD_STRENGTHS = "iep-development-src-strengths";
    private static final String ALIAS_DEVELOPMENT_SRC_PAR_CONCERNS = "iep-development-src-concerns";
    private static final String ALIAS_DISTRICT_ASSESSMENT = "iep-assess-dist-assess";
    private static final String ALIAS_DISTRICT_ASSESSMENT_MODIFICATION = "iep-assess-modif-accomo";
    private static final String ALIAS_DISTRICT_ASSESSMENT_NOT_APPROPRIATE_REASON = "iep-assess-why-not-appr";
    private static final String ALIAS_EDUCATIONAL_NEEDS = "iep-education-needs";
    private static final String ALIAS_GOAL_COMM_PARTI = "iep-goal-comm-parti";
    private static final String ALIAS_GOAL_EMP_CAREER = "iep-goal-emp-career";
    private static final String ALIAS_GOAL_IND_LIVING = "iep-goal-ind-living";
    private static final String ALIAS_GOAL_POSTSEC_ED = "iep-goal-postsec-ed";
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_INTERAGENCY_SCHOOL_DIST_RESP = "iep-school-dist-resp";
    private static final String ALIAS_INTERAGENCY_STD_PARENT_RESP = "iep-std-parent-resp";
    private static final String ALIAS_IS_SPEECH_IEP = "iep-is-speech-iep";
    private static final String ALIAS_LIAISON = "iep-itm-liaison";
    private static final String ALIAS_MODIFICATIONS = "iep-state-modi-acc";
    private static final String ALIAS_MODIFICATIONS_OTHER = "iep-state-other-modi-acc";
    private static final String ALIAS_MODIF_SUPPL_AIDS_REG_ED_MODIF = "iep-regular-ed-modifs";
    private static final String ALIAS_MODIF_SUPPL_AIDS_REG_ED_OTHER = "iep-reg-modif-suppl-aid-other";
    private static final String ALIAS_MODIF_SUPPL_AIDS_REG_ED_SUPPL_AID = "iep-regular-ed suppl-aids";
    private static final String ALIAS_MODIF_SUPPL_AIDS_SPL_ED_MODIF = "iep-special-ed-modifs";
    private static final String ALIAS_MODIF_SUPPL_AIDS_SPL_ED_OTHER = "iep-sped-modif-suppl-aid-other";
    private static final String ALIAS_MODIF_SUPPL_AIDS_SPL_ED_SUPPL_AID = "iep-special-ed-suppl-aids";
    private static final String ALIAS_OBJECTIVE_BENCHMARK = "iep-acad-short-term-obj";
    private static final String ALIAS_OBJECTIVE_CRITERIA = "iep-acad-obj-criteria";
    private static final String ALIAS_OBJECTIVE_EVAL_PROC = "iep-acad-obj-eval-proc";
    private static final String ALIAS_PARENT_NOTIFIED = "iep-transfer-notif-par";
    private static final String ALIAS_PARENT_NOTIFIED_DATE = "iep-transfer-par-date";
    private static final String ALIAS_PARENTAL_INVOLVEMENT = "iep-parental-involve";
    private static final String ALIAS_PLACEMENT_DECISION = "iep-pl-categ";
    private static final String ALIAS_POS_SUPP_INT_DESC = "iep-pos-supp-int-desc";
    private static final String ALIAS_PRESENT_LEVEL = "iep-present-level";
    private static final String ALIAS_PRIOR_INT_STD_RESP = "iep-prior-int-std-resp";
    private static final String ALIAS_REASON_NOT_APPR = "iep-state-not-appr";
    // private static final String ALIAS_REL_SERVICE_GROUP_SIZE = "iep-nj-svc-grp-size";
    private static final String ALIAS_SERVICE_CODE = "iep-nj-svc-service-code";
    private static final String ALIAS_LOCATION_CODE = "iep-nj-svc-location-code";
    private static final String ALIAS_SERVICE_ACTIVITY = "iep-nj-svc-goal";
    private static final String ALIAS_SERVICE_PROVIDER = "iep-nj-svc-provider";
    private static final String ALIAS_SERVICE_PROVIDER_OTHER = "iep-nj-svc-provider-other";
    private static final String ALIAS_SPECIAL_FACTORS = "iep-sped-special-factors";
    private static final String ALIAS_STATE_COURSE1 = "iep-state-course1";
    private static final String ALIAS_STATE_COURSE2 = "iep-state-course2";
    private static final String ALIAS_STATE_COURSE3 = "iep-state-course3";
    private static final String ALIAS_STRATGY_ACTIVITY = "iep-stratgy-activit";
    private static final String ALIAS_STRENGTH_INTS_PREFS = "iep-strength-interest-prefs";
    private static final String ALIAS_STUDENT_CONTACT_NAME = "iep-transfer-par-name";
    private static final String ALIAS_STUDENT_NOTIFIED = "iep-transfer-notif-std";
    private static final String ALIAS_STUDENT_NOTIFIED_DATE = "iep-transfer-std-date";
    private static final String ALIAS_SUPP_INT_COND_CHANGE = "iep-supint-cond-change";
    private static final String ALIAS_SUPP_INT_COND_TERMINATE = "iep-supint-cond-termin";
    private static final String ALIAS_TARGET_BEHAVIORAL = "iep-target-behavior";
    private static final String ALIAS_TRANS_SETT_ACTIVIT = "iep-trans-sett-activi";
    private static final String ALIAS_TRANSFER_OF_RIGHTS_OPTION = "iep-transfer-option-type";
    private static final String ALIAS_GUARDIANSHIP_NOTES = "iep-sped-guardian-notes";

    /**
     * The variables below are used for setting the data source fields of the printed report..
     */
    private static final String DATASOURCE_ASSESSMENT = "DATASOURCE_ASSESSMENT";
    private static final String DATASOURCE_BEHAVIORAL_INTERVENTION = "DATASOURCE_BEHAVIORAL_INTERVENTION";
    private static final String DATASOURCE_COVERPAGE_NAMES = "DATASOURCE_COVERPAGE_NAMES";
    private static final String DATASOURCE_GOALS = "DATASOURCE_GOALS";
    private static final String DATASOURCE_INTERAGENCY_OTHER_SERVICES = "DATASOURCE_INTERAGENCY_SERVICES";
    private static final String DATASOURCE_PRESENT_LEVELS = "DATASOURCE_PRESENT_LEVELS";
    private static final String DATASOURCE_RIGHTS = "DATASOURCE_RIGHTS";
    private static final String DATASOURCE_SERVICES = "DATASOURCE_SERVICES";
    private static final String DATASOURCE_TRANSITION_PLANNING = "DATASOURCE_TRANSITION_PLANNING";
    private static final String DATASOURCE_TRANSITION_SERVICES = "DATASOURCE_TRANSITION_SERVICES";
    private static final String DATASOURCE_MODIFICATIONS_SUPPLEMENTARY_AIDS = "DATASOURCE_MODIF_SUPPL_AIDS";

    private static final String DASHED_STRING = "__________________________________";
    private static final String EMPTY_STRING = "";
    private static final String SERVICE_NOT_ADDED = "Considered. Not Needed";
    private static final String TEXT_LIAISON = "Liaison";

    /**
     * The variables below are used for setting the fields in the sub reports.
     */
    private static final String FIELD_ACCOMMODATION_MODIFICATION = "accommodationModification";
    // private static final String FIELD_ASSESSMENT_TYPE = "assessmentType";
    private static final String FIELD_CONDITIONS_SUPP_INT_CHG = "conditionsSuppIntChg";
    private static final String FIELD_CONDITIONS_SUPP_INT_TERMINATE = "conditionsSuppIntTerminate";
    private static final String FIELD_COURSE_COURSES1 = "courses1";
    private static final String FIELD_COURSE_COURSES2 = "courses2";
    private static final String FIELD_COURSE_COURSES3 = "courses3";
    private static final String FIELD_COURSE_COURSES4 = "courses4";
    private static final String FIELD_COURSE_GRADE1 = "courseGrade1";
    private static final String FIELD_COURSE_GRADE2 = "courseGrade2";
    private static final String FIELD_COURSE_GRADE3 = "courseGrade3";
    private static final String FIELD_COURSE_GRADE4 = "courseGrade4";
    private static final String FIELD_DATA_COLLECTION_PROC = "dataCollectionProc";
    private static final String FIELD_DATA_GRID_GOAL_OBJECTIVES = "goalObjectives";
    private static final String FIELD_DATA_GRID_RELATED = "relatedServices";
    private static final String FIELD_DATA_GRID_SPED = "spedServices";
    private static final String FIELD_DEVELOPMENT_SRC = "developmentSrc";
    private static final String FIELD_DEVELOPMENT_SRC_STUDENT_STRENGTHS = "developmentSrcStrengths";
    private static final String FIELD_DEVELOPMENT_SRC_PARENT_CONCERNS = "developmentSrcConcerns";
    // private static final String FIELD_DIST_ASSESSMENT_MODIFICATIONS = "distAssessmentModif";
    // private static final String FIELD_DISTRICT_ASSESSMENT = "distAssessment";
    private static final String FIELD_EDUCATIONAL_NEEDS = "educationalNeeds";
    private static final String FIELD_GOAL_COMM_PARTI = "goalCommParti";
    private static final String FIELD_GOAL_DESCRIPTION = "goalDescription";
    private static final String FIELD_GOAL_EMP_CAREER = "goalEmpCareer";
    private static final String FIELD_GOAL_FOCUS = "goalFocus";
    private static final String FIELD_GOAL_ID = "goalId";
    private static final String FIELD_GOAL_IND_LIVING = "goalIndLiving";
    private static final String FIELD_GOAL_OID = "goalOid";
    private static final String FIELD_GOAL_POSTSEC_ED = "goalPostsecEd";

    /*
     * private static final String FIELD_HSPA = "hspa";
     * private static final String FIELD_HSPA_SRA_ARTS = "hspaSraArts";
     * private static final String FIELD_HSPA_SRA_MATHS = "hspaSraMaths";
     * private static final String FIELD_HSPA_SRA_SCIENCE = "hspaSraScience";
     */
    private static final String FIELD_INTERAGENCY_NAME = "agencyName";
    private static final String FIELD_INTERAGENCY_OID = "serviceId";
    private static final String FIELD_INTERAGENCY_SCHOOL_DIST_RESP = "schoolDistResp";
    private static final String FIELD_INTERAGENCY_STD_PARENT_RESP = "studentParentResp";
    private static final String FIELD_MODIF_SUPPL_AIDS_ED_MODIF = "modification";
    private static final String FIELD_MODIF_SUPPL_AIDS_ED_OTHER = "otherReason";
    private static final String FIELD_MODIF_SUPPL_AIDS_ED_SUPPL_AID = "supplementaryAids";
    private static final String FIELD_MODIF_SUPPL_AIDS_ED_TYPE = "educationType";
    private static final String FIELD_OBJECTIVE_BENCHMARK = "objective";
    private static final String FIELD_OBJECTIVE_CRITERIA = "criteria";
    private static final String FIELD_OBJECTIVE_EVALUATION_PROC = "evalProc";
    private static final String FIELD_OPTION_SELECTED = "optionSelected";
    private static final String FIELD_OPTION1_LINE1 = "option1Line1";
    private static final String FIELD_OPTION1_LINE2 = "option1Line2";
    private static final String FIELD_OPTION1_LINE3 = "option1Line3";
    private static final String FIELD_OPTION1_LINE4 = "option1Line4";
    private static final String FIELD_OPTION1_LINE5 = "option1Line5";
    private static final String FIELD_OPTION1_LINE6 = "option1Line6";
    private static final String FIELD_OPTION2_LINE1 = "option2Line1";
    private static final String FIELD_OPTION2_LINE2 = "option2Line2";
    private static final String FIELD_GUARDIANSHIP_NOTES = "guardianshipNotes";
    // private static final String FIELD_PARCC = "parcc";
    private static final String FIELD_PARENT_NOTIFIED = "parentNotified";
    private static final String FIELD_PARENTAL_INVOLVEMENT = "parentalInvolvement";
    private static final String FIELD_POS_SUPP_INT = "posSuppInt";
    private static final String FIELD_PRESENT_LEVEL = "presentLevel";
    private static final String FIELD_PRIOR_INT_STD_RESPONSE = "priorIntStdReponse";
    // Has the start and end date in the format: Start Date + " - " End Date
    private static final String FIELD_SERVICE_DATE = "serviceDate";
    private static final String FIELD_SERVICE_DETAILS = "serviceDetails";
    private static final String FIELD_SERVICE_DURATION = "duration";
    private static final String FIELD_SERVICE_FREQUENCY = "frequency";
    private static final String FIELD_SERVICE_LOCATION = "location";
    private static final String FIELD_SERVICE_OID = "svcOid";
    private static final String FIELD_SERVICE_PROVIDER = "serviceAgency";
    private static final String FIELD_SERVICE_START_DATE = "serviceStartDate";
    private static final String FIELD_SERVICE_TYPE = "serviceType";
    private static final String FIELD_SPECIAL_FACTORS = "specialFactors";
    // private static final String FIELD_SRA = "sra";
    // private static final String FIELD_STATE_ASSESSMENT_MODIFICATIONS = "stateAssessmentModif";

    private static final String FIELD_STATE_CODE = "stateCode";
    private static final String FIELD_STATE_ARTS = "stateArts";
    private static final String FIELD_STATE_MATHS = "stateMaths";
    private static final String FIELD_STATE_SCIENCE = "stateScience";
    private static final String FIELD_MODIF = "modif";
    private static final String FIELD_NOT_APPR = "notAppr";
    private static final String FIELD_ALT_ASSESS_TYPE = "altAssessmentType";
    private static final String FIELD_APA_ARTS = "apaArts";
    private static final String FIELD_APA_MATHS = "apaMaths";
    private static final String FIELD_APA_SCIENCE = "apaScience";

    /*
     * private static final String FIELD_STATE_GRADE3_ARTS = "stateGrade3Arts";
     * private static final String FIELD_STATE_GRADE3_MATHS = "stateGrade3Maths";
     * private static final String FIELD_STATE_GRADE4_ARTS = "stateGrade4Arts";
     * private static final String FIELD_STATE_GRADE4_MATHS = "stateGrade4Maths";
     * private static final String FIELD_STATE_GRADE4_SCIENCE = "stateGrade4Science";
     * private static final String FIELD_STATE_GRADE5_ARTS = "stateGrade5Arts";
     * private static final String FIELD_STATE_GRADE5_MATHS = "stateGrade5Maths";
     * private static final String FIELD_STATE_GRADE6_ARTS = "stateGrade6Arts";
     * private static final String FIELD_STATE_GRADE6_MATHS = "stateGrade6Maths";
     * private static final String FIELD_STATE_GRADE7_ARTS = "stateGrade7Arts";
     * private static final String FIELD_STATE_GRADE7_MATHS = "stateGrade7Maths";
     * private static final String FIELD_STATE_GRADE8_ARTS = "stateGrade8Arts";
     * private static final String FIELD_STATE_GRADE8_MATHS = "stateGrade8Maths";
     * private static final String FIELD_STATE_GRADE8_SCIENCE = "stateGrade8Science";
     */
    private static final String FIELD_STRATGY_ACTIVITY = "stratgyActivit";
    private static final String FIELD_STRENGTH_INTS_PREFS = "strengthInterestPrefs";
    private static final String FIELD_STUDENT_NOTIFIED = "studentNotified";
    private static final String FIELD_TARGET_BEHAVIOR = "targetBehavior";
    private static final String FIELD_TITLE = "title";
    private static final String FIELD_NAME = "name";
    private static final String FIELD_TRANS_SETT_ACTIVIT = "transSettActivi";

    private static final String IEP_TYPE_REGULAR = "REGULAR";
    private static final String IEP_TYPE_SPEECH = "SPEECH";

    /**
     * The variables below are input parameters
     */
    private static final String PARAM_CALC_SERVICE = "calcServiceHours";

    /**
     * The variables below are used for setting the parameters of the printed report.
     */
    private static final String PARAM_ASSESSMENT_MODIFICATIONS = "distAssessmentModif";
    private static final String PARAM_ASSESSMENT_NOT_APPR_REASON = "distAssessmentNotAppr";
    private static final String PARAM_ASSESSMENT_TYPE = "assessmentType";
    private static final String PARAM_CASE_MANAGER_NAME = "CASE_MANAGER_NAME";
    private static final String PARAM_PRIMARY_DISABILITY = "PRIMARY_DISABILITY";
    private static final String PARAM_CONTACT_NAME = "CONTACT_NAME";
    private static final String PARAM_DISTRICT_ASSESSMENT = "distAssessment";
    private static final String PARAM_IEP_TYPE = "IEP_TYPE";
    private static final String PARAM_LIAISON_NAME = "LIAISON_NAME";
    private static final String PARAM_MEETING_TYPE = "MEETING_TYPE";
    private static final String PARAM_MEETING_TYPE_INITIAL = "MEETING_TYPE_INITIAL";
    private static final String PARAM_SECONDARY_CONTACT_NAME = "SECONDARY_CONTACT_NAME";
    private static final String PARAM_SERVICE_NAME_INTERAGENCY = "InterAgency";
    private static final String PARAM_SERVICE_NAME_RELATED = "Related";
    private static final String PARAM_SERVICE_NAME_SPECIAL_ED = "SpecialEd";
    private static final String PARAM_SERVICE_NAME_SPEECH = "Speech";
    private static final String PARAM_SERVICE_NAME_TRANSITION = "Transition";
    private static final String PARAM_STATUS_CODE = "STATUS_CODE";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_CURRENT_DATE = "CURRENT_DATE";
    private static final String PARAM_SUBREPORT_ASSESSMENT = "SUB_REPORT_ASSESSMENT";
    private static final String PARAM_SUBREPORT_BEHAVIORAL_INTERVENTION = "SUB_REPORT_BEHAVIORAL_INTERVENTION";
    private static final String PARAM_SUBREPORT_COVERPAGE_NAMES = "SUB_REPORT_COVERPAGE_NAMES";
    private static final String PARAM_SUBREPORT_GOALS = "SUB_REPORT_GOALS";
    private static final String PARAM_SUBREPORT_INTERAGENCY = "SUB_REPORT_INTERAGENCY";
    private static final String PARAM_SUBREPORT_MODIFICATIONS_SUPPLEMENTARY_AIDS = "SUB_REPORT_MODIF_SUPPL_AIDS";
    private static final String PARAM_SUBREPORT_OBJECTIVES = "SUB_REPORT_OBJECTIVES";
    private static final String PARAM_SUBREPORT_PRESENT_LEVELS = "SUB_REPORT_PRESENT_LEVELS";
    private static final String PARAM_SUBREPORT_RELATED = "SUB_REPORT_RELATED";
    private static final String PARAM_SUBREPORT_RIGHTS = "SUB_REPORT_RIGHTS";
    private static final String PARAM_SUBREPORT_SERVICES = "SUB_REPORT_SERVICES";
    private static final String PARAM_SUBREPORT_SPED = "SUB_REPORT_SPED";
    private static final String PARAM_SUBREPORT_TRANSITION_PLANNING = "SUB_REPORT_TRANSITION_PLANNING";
    private static final String PARAM_SUBREPORT_TRANSITION_SERVICES = "SUB_REPORT_TRANSITION_SERVICES";

    private static final String MODIF_SUPPL_AIDS_ED_TYPE_REGULAR = "REGULAR";
    private static final String MODIF_SUPPL_AIDS_ED_TYPE_SPECIALED = "SPECIAL";

    /*
     * private static final String REFCODE_GRADE3 = "Grade 3";
     * private static final String REFCODE_GRADE4 = "Grade 4";
     * private static final String REFCODE_GRADE5 = "Grade 5";
     * private static final String REFCODE_GRADE6 = "Grade 6";
     * private static final String REFCODE_GRADE7 = "Grade 7";
     * private static final String REFCODE_GRADE8 = "Grade 8";
     * private static final String REFCODE_HSPA = "HSPA";
     * private static final String REFCODE_PARCC = "PARCC";
     */
    private static final String REFCODE_PARENT = "Parent/Guardian";
    // private static final String REFCODE_SRA = "SRA";

    /**
     * The variables below represent the reference codes for the service setting.
     */
    private static final String REFCODE_SETTING_GEN_ED = "01";
    private static final String REFCODE_SETTING_SPED = "02";
    private static final String REFCODE_SETTING_PUB_SEP = "03";
    private static final String REFCODE_SETTING_PRIV = "04";
    private static final String REFCODE_SETTING_PRIV_RES = "05";
    private static final String REFCODE_SETTING_HOME = "06";
    private static final String REFCODE_SETTING_PUB_RES = "07";

    /**
     * The variables below represent the reference state codes for the placement decision.
     */
    private static final String REFCODE_PLACEMENT_EIGHTY = "09";
    private static final String REFCODE_PLACEMENT_FORTY = "10";
    private static final String REFCODE_PLACEMENT_SOME = "11";
    private static final String REFCODE_PLACEMENT_PUB_SEP = "12";
    private static final String REFCODE_PLACEMENT_PRIV_DAY = "13";
    private static final String REFCODE_PLACEMENT_PRIV_RES = "14";
    private static final String REFCODE_PLACEMENT_PUB_RES = "15";
    private static final String REFCODE_PLACEMENT_HOME_INST = "16";

    private static final String OID_REFTABLE_PLACEMENT_DECISION = "rtbNjPlCat";
    private static final String OID_REFTABLE_SERVICE_CYCLE = "rtbNjSvcCycle";
    private static final String OID_REFTABLE_SERVICE_SETTING = "rtbNJSetting";

    private static final String USER_NAME_REFTABLE_INTERAGENCY = "Interagency Codes";
    /**
     * The variables below represent the report ids of the default IEP sub reports.
     */
    private static final String REPORT_ID_ASSESSMENTS = "SYS-SPED-NJ-IEP9";
    private static final String REPORT_ID_MODIFICATIONS_SUPPLEMENTARY_AIDS = "SYS-SPED-NJ-IEP9-2";
    private static final String REPORT_ID_BEHAVIORAL_INTERVENTION = "SYS-SPED-NJ-IEP6";
    private static final String REPORT_ID_GOALS_OBJECTIVES = "SYS-SPED-NJ-IEP7";
    private static final String REPORT_ID_INTERAGENCY = "SYS-SPED-NJ-IEP3";
    private static final String REPORT_ID_OBJECTIVES = "SYS-SPED-NJ-IEP8";
    private static final String REPORT_ID_PRESENT_LEVELS = "SYS-SPED-NJ-IEP1";
    private static final String REPORT_ID_RIGHTS = "SYS-SPED-NJ-IEP5";
    private static final String REPORT_ID_SERVICES = "SYS-SPED-NJ-IEP10";
    private static final String REPORT_ID_SERVICES_REL = "SYS-SPED-NJ-IEP12";
    private static final String REPORT_ID_SERVICES_SPED = "SYS-SPED-NJ-IEP11";
    private static final String REPORT_ID_TRANSITION_PLANNING = "SYS-SPED-NJ-IEP2";
    private static final String REPORT_ID_TRANSITION_SVCS = "SYS-SPED-NJ-IEP4";
    private static final String REPORT_ID_COVERPAGE_NAMES = "SYS-SPED-NJ-IEPCOVER";

    /**
     * The variables below represent the report ids of the default IEP and speech IEP.
     */
    private static final String REPORT_ID_DEFAULT_IEP = "SYS-SPED-NJ-IEP";
    private static final String REPORT_ID_SPEECH_IEP = "SYS-SPED-NJ-SIEP";

    /**
     * The variables below represent the report ids of the speech IEP sub reports.
     */
    private static final String REPORT_ID_SPEECH_ASSESSMENTS = "SYS-SPED-NJ-SIEP5";
    private static final String REPORT_ID_SPEECH_GOALS_OBJECTIVES = "SYS-SPED-NJ-SIEP3";
    private static final String REPORT_ID_SPEECH_OBJECTIVES = "SYS-SPED-NJ-SIEP4";
    private static final String REPORT_ID_SPEECH_PRESENT_LEVELS = "SYS-SPED-NJ-SIEP1";
    private static final String REPORT_ID_SPEECH_RIGHTS = "SYS-SPED-NJ-SIEP2";
    private static final String REPORT_ID_SPEECH_SERVICES = "SYS-SPED-NJ-SIEP6";

    private static final String SERVICE_TYPE_MODE_TRANSITION = "Transition";
    private static final String TRANSFER_OF_RIGHTS_OPTION2 = "Option 2";
    private static final String TRANSITION_SVC_REF_TABLE = "rtbServiceType";

    private Map<String, String> mAgencyInfo = new HashMap<String, String>();
    /**
     * Codes used in report
     */
    private static final String CODE_SERVICE_PROVIDER_OTHER = "Other";
    private static final String CODE_SPED_MODE = "SpecialEd";

    /**
     * Prepares the data source that will be used by the Jasper design. This method is called after
     * <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        JRDataSource dataSource = selectIEPPrintTypeAndLoad();

        // determine sped placement if requested
        m_calcService = (Boolean) getParameter(PARAM_CALC_SERVICE);
        if (m_calcService != null ? m_calcService.booleanValue() : false) {
            determinePlacement();
        }

        return dataSource;
    }

    /**
     * Determine placement.
     */
    private void determinePlacement() {
        double spedMinutes = 0.0;
        double peerMinutes = 0.0;
        double peerPercent = 0.0;
        double totalMinutes = 0.0;
        boolean placementDetermined = false;
        String placement = null;

        // populate reference code maps needed by method
        m_serviceSettingRefCodes = getIepRefCodes(OID_REFTABLE_SERVICE_SETTING, ReferenceCode.COL_CODE);
        m_placementDecisionRefCodes = getIepRefCodes(OID_REFTABLE_PLACEMENT_DECISION, ReferenceCode.COL_STATE_CODE);
        m_serviceCycleRefCodes = getIepRefCodes(OID_REFTABLE_SERVICE_CYCLE, ReferenceCode.COL_CODE);

        Collection<IepService> services = m_currentIep.getIepServices();

        for (IepService service : services) {
            String serviceCode = (String) service.getFieldValueByAlias(ALIAS_SERVICE_CODE, getDictionary());
            String settingMode = service.getServiceMode();

            // Include only special education indicated by having a SPED mode
            // Make sure it has a setting code.
            if (CODE_SPED_MODE.equals(settingMode) && !StringUtils.isEmpty(serviceCode)) {
                if (m_serviceSettingRefCodes.containsKey(serviceCode)) {
                    ReferenceCode refCode = m_serviceSettingRefCodes.get(serviceCode);
                    serviceCode = refCode.getStateCode();
                }
                if (REFCODE_SETTING_SPED.equals(serviceCode)) {
                    spedMinutes += calculateServiceMinutes(service);
                } else if (REFCODE_SETTING_GEN_ED.equals(serviceCode)) {
                    peerMinutes += calculateServiceMinutes(service);
                } else if (REFCODE_SETTING_PUB_SEP.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PUB_SEP);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PRIV.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PRIV_DAY);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PRIV_RES.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PRIV_RES);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_HOME.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_HOME_INST);
                    // placement = getServicePlacement(REFCODE_PLACEMENT_HOME);
                    placementDetermined = true;
                    break;
                } else if (REFCODE_SETTING_PUB_RES.equals(serviceCode)) {
                    placement = getServicePlacement(REFCODE_PLACEMENT_PUB_RES);
                    placementDetermined = true;
                    break;
                }
            }
        }

        // calculate total minutes
        totalMinutes = spedMinutes + peerMinutes;

        if (!placementDetermined && totalMinutes > 0.0) {
            final double eightyPercent = 0.8;
            final double fortyPercent = 0.4;

            peerPercent = peerMinutes / totalMinutes;

            if (peerPercent >= eightyPercent) {
                placement = getServicePlacement(REFCODE_PLACEMENT_EIGHTY);
                placementDetermined = true;
            } else if (peerPercent >= fortyPercent) {
                placement = getServicePlacement(REFCODE_PLACEMENT_FORTY);
                placementDetermined = true;
            } else {
                placement = getServicePlacement(REFCODE_PLACEMENT_SOME);
                placementDetermined = true;
            }

        }

        if (placementDetermined) {
            m_currentIep.setFieldValueByAlias(ALIAS_PLACEMENT_DECISION, placement, getDictionary());
            getBroker().saveBeanForced(m_currentIep, getDictionary());
        }
    }

    /**
     * Returns the corresponding service placement.
     *
     * @param key String
     * @return String
     */
    private String getServicePlacement(String key) {
        String servicePlacement = null;
        if (m_placementDecisionRefCodes != null &&
                m_placementDecisionRefCodes.containsKey(key)) {
            ReferenceCode refCode = m_placementDecisionRefCodes.get(key);
            servicePlacement = refCode.getCode();
        }

        return servicePlacement;
    }

    /**
     * Calculates service minutes.
     *
     * @param service IepService
     * @return double
     */
    private double calculateServiceMinutes(IepService service) {
        double serviceMinutes = 0.0;
        BigDecimal frequency = service.getFrequency();
        int duration = service.getDuration();

        // get the total minutes considered from the cycle code
        // the cycle code will have a state value corresponding to the number
        // of days in the cycle. this needs to be resolved into minutes.
        double totalMinutes = 0.0;
        String cycleCode = service.getCycle();
        if (m_serviceCycleRefCodes.containsKey(cycleCode)) {
            ReferenceCode refCode = m_serviceCycleRefCodes.get(cycleCode);
            cycleCode = refCode.getStateCode();
            totalMinutes = Double.parseDouble(cycleCode) * 24.0 * 60.0;
        }

        // calculate the service minutes using the frequency, duration and total minutes
        if (frequency != null) {
            serviceMinutes += frequency.doubleValue() * duration / totalMinutes;
        }

        return serviceMinutes;
    }

    /**
     * This method gets reference codes specified by refTableOid.
     *
     * @param refTableOid String
     * @param keyColumn String
     * @return Map
     */
    private Map<String, ReferenceCode> getIepRefCodes(String refTableOid, String keyColumn) {
        Map<String, ReferenceCode> iepRefCodeMap = null;
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, refTableOid);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);

        iepRefCodeMap = getBroker().getMapByQuery(refCodeQuery, keyColumn, 32);

        return iepRefCodeMap;
    }

    /**
     * Select IEP Print Type and Load.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    private JRDataSource selectIEPPrintTypeAndLoad() throws Exception {
        boolean isSpeechIEP = false;
        JRDataSource dataSource = null;
        String iepType = (String) m_currentIep.getFieldValueByAlias(ALIAS_IS_SPEECH_IEP, getDictionary());

        if (BooleanAsStringConverter.TRUE.equals(iepType)) {
            isSpeechIEP = true;
        }
        if (isSpeechIEP) {
            setFormatId(REPORT_ID_SPEECH_IEP);
            addParameter(PARAM_IEP_TYPE, IEP_TYPE_SPEECH);
            dataSource = loadSpeechIEP();
        } else {
            setFormatId(REPORT_ID_DEFAULT_IEP);
            addParameter(PARAM_IEP_TYPE, IEP_TYPE_REGULAR);
            dataSource = loadNormalIEP();
        }

        return dataSource;
    }

    /**
     * Load parameters for Normal IEP Report.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    private JRDataSource loadNormalIEP() throws Exception {
        loadAssessments();
        loadBehavioralInterventions();
        loadGoalsAndObjectives();
        loadInterAgencyServices();
        loadPresentLevels();
        loadModificationsAndSupplementaryAids();
        loadRights();
        loadSpedRelServices();
        loadTransitionPlanning();
        loadTransitionServices();
        setLiaisonNameAndRole();
        setCoverMeetingTypeAndStatusCode();
        loadCoverPageNames();

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        return dataSource;
    }

    /**
     * Load parameters for Speech IEP Report.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    private JRDataSource loadSpeechIEP() throws Exception {
        loadSpeechAssessments();
        loadSpeechGoalsAndObjectives();
        loadSpeechPresentLevels();
        loadModificationsAndSupplementaryAids();
        loadSpeechRights();
        loadSpeechSpedRelServices();
        loadCoverPageNames();
        setCoverMeetingTypeAndStatusCode();

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        return dataSource;
    }

    /**
     * This method sets the liaison name parameter for the current IEP report form.
     */
    private void setLiaisonNameAndRole() {
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        String liaison = "";
        String memberRole = "";

        for (IepTeamMember member : teamMembers) {
            String isLiaison = (String) member.getFieldValueByAlias(ALIAS_LIAISON, getDictionary());
            if (BooleanAsStringConverter.TRUE.equals(isLiaison)) {
                if (null != member) {
                    memberRole = member.getMemberRoleCode();
                    if (member.getPerson() != null) {
                        String firstName = member.getPerson().getFirstName();
                        String lastName = member.getPerson().getLastName();
                        String tempName = firstName + " " + lastName;
                        tempName = tempName + "," + " " + memberRole + ";" + " ";
                        liaison = liaison.concat(tempName);
                    }
                }
            }
        }

        String additionalLiaison =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_ADDITIONAL_LIAISON, getDictionary());
        if (!StringUtils.isEmpty(additionalLiaison)) {
            liaison = liaison + ", " + additionalLiaison;
        }

        addParameter(PARAM_LIAISON_NAME, liaison);
    }

    /**
     * This method sets the meeting type and status code on the cover page of the printed IEP.
     */
    private void setCoverMeetingTypeAndStatusCode() {
        boolean isInitial = false;
        TypeCode typeCode = m_currentIep.getMeetingTypeCodeEnum();
        if (IepMeeting.TypeCode.INITIAL.equals(typeCode)) {
            isInitial = true;
        }

        addParameter(PARAM_MEETING_TYPE_INITIAL, Boolean.valueOf(isInitial));
        String meetingTypeCode = "";
        if (typeCode != null) {
            meetingTypeCode = typeCode.name();
        }

        addParameter(PARAM_MEETING_TYPE, meetingTypeCode);
        StatusCode statusCode = m_currentIep.getStatusCodeEnum();
        String status = "";
        if (statusCode != null) {
            status = statusCode.name();
        }

        addParameter(PARAM_STATUS_CODE, status);
    }

    /**
     * Load Cover Page Names.
     */
    private void loadCoverPageNames() {
        getCoverPageNamesSubRptCriteria();
        getCoverPageNamesCriteria();
    }

    /**
     * This method sets the input stream for the cover page names sub report.
     *
     * @return void
     */
    private void getCoverPageNamesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_COVERPAGE_NAMES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportCoverPageNames = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_COVERPAGE_NAMES,
                new ByteArrayInputStream(m_reportCoverPageNames.getCompiledFormat()));
    }

    /**
     * This method sets parameters for the names of all people on the cover page
     * of the IEP report form.
     *
     * @return void
     */
    private void getCoverPageNamesCriteria() {
        String studentName = "";
        String caseManagerName = "";
        String schoolName = "";
        String primaryContactName = "";
        String secondaryContactName = "";
        String primaryDisability = "";
        IepTeamMember primaryContactTeamMember = null;
        IepTeamMember secondaryContactTeamMember = null;
        SisStudent student = m_currentIep.getStudent();
        // Get today's date for printing
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");
        Date date = new Date();
        String currentDate = sdf.format(date);

        if (student != null) {
            studentName = student.getPerson().getFirstName() + " " + student.getPerson().getLastName();
            schoolName = student.getSchool().getName();
            Collection<IepDisability> disabilities = m_currentIep.getIepDisability();
            for (IepDisability disability : disabilities) {
                if (disability.getPrimaryIndicator()) {
                    String disabilityRefCode = disability.getDisabilityCode();
                    if (!StringUtils.isEmpty(primaryDisability)) {
                        primaryDisability = primaryDisability + "; ";
                    }

                    primaryDisability = primaryDisability + disabilityRefCode;
                }
            }
        }

        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            caseManagerName = caseManager.getPerson().getFirstName() + " " + caseManager.getPerson().getLastName();
        }

        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();

        m_coverPageNamesGrid = new ReportDataGrid();
        for (IepTeamMember member : teamMembers) {
            if (null != member) {

                String title = member.getMemberRoleCode();
                Person person = member.getPerson();
                String name = "";

                if (person != null) {
                    name = person.getFirstName() + " " + person.getLastName();
                }

                if (title != null && REFCODE_PARENT.equalsIgnoreCase(title)) {
                    if (primaryContactTeamMember == null) {
                        primaryContactTeamMember = member;
                        primaryContactName = name;
                    } else {
                        if ((primaryContactTeamMember.getFormPriority() == null && member.getFormPriority() != null) ||
                                (primaryContactTeamMember.getFormPriority() != null && member.getFormPriority() != null
                                        &&
                                        Integer.parseInt(primaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = primaryContactTeamMember;
                            secondaryContactName = primaryContactName;
                            primaryContactTeamMember = member;
                            primaryContactName = name;
                        } else if (secondaryContactTeamMember == null ||
                                (secondaryContactTeamMember.getFormPriority() != null
                                        && member.getFormPriority() != null &&
                                        Integer.parseInt(secondaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = member;
                            secondaryContactName = name;
                        }
                    }
                }

                String isLiaison = (String) member.getFieldValueByAlias(ALIAS_LIAISON, getDictionary());
                if (BooleanAsStringConverter.TRUE.equals(isLiaison)) {
                    title = title + ";" + " " + TEXT_LIAISON;
                }
                m_coverPageNamesGrid.append();
                m_coverPageNamesGrid.set(FIELD_TITLE, title);
                m_coverPageNamesGrid.set(FIELD_NAME, name);
            }
        }

        m_coverPageNamesGrid.beforeTop();
        addParameter(DATASOURCE_COVERPAGE_NAMES, m_coverPageNamesGrid);
        addParameter(PARAM_STUDENT_NAME, studentName);
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        addParameter(PARAM_CURRENT_DATE, currentDate);
        addParameter(PARAM_CONTACT_NAME, primaryContactName);
        addParameter(PARAM_SECONDARY_CONTACT_NAME, secondaryContactName);
        addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);
        addParameter(PARAM_PRIMARY_DISABILITY, primaryDisability);
    }

    /**
     * This method loads the present levels sub report.
     */
    private void loadPresentLevels() {
        getPresentLevelsSubRptCriteria();
        getPresentLevelsCriteria();
    }

    /**
     * This method sets the input stream for the present levels sub report.
     *
     * @return void
     */
    private void getPresentLevelsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_PRESENT_LEVELS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportPresentLevels = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_PRESENT_LEVELS,
                new ByteArrayInputStream(m_reportPresentLevels.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the present levels sub report.
     *
     * @return void
     */
    private void getPresentLevelsCriteria() {
        String developmentSrc = null;
        String developmentSrcStudentStrengths = null;
        String developmentSrcParentConcerns = null;
        String presentLevel = null;
        String accommodationModification = null;
        String educationalNeeds = null;
        String specialFactors = null;

        developmentSrc = (String) m_currentIep.getFieldValueByAlias(ALIAS_DEVELOPMENT_SRC, getDictionary());
        developmentSrcStudentStrengths =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_DEVELOPMENT_SRC_STD_STRENGTHS, getDictionary());
        developmentSrcParentConcerns =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_DEVELOPMENT_SRC_PAR_CONCERNS, getDictionary());
        presentLevel = (String) m_currentIep.getFieldValueByAlias(ALIAS_PRESENT_LEVEL, getDictionary());
        accommodationModification =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_ACCOMMODATION_MODIFICATION, getDictionary());
        educationalNeeds = (String) m_currentIep.getFieldValueByAlias(ALIAS_EDUCATIONAL_NEEDS, getDictionary());
        specialFactors = (String) m_currentIep.getFieldValueByAlias(ALIAS_SPECIAL_FACTORS, getDictionary());

        m_presentLevelsGrid = new ReportDataGrid();
        m_presentLevelsGrid.append();
        m_presentLevelsGrid.set(FIELD_DEVELOPMENT_SRC, developmentSrc);
        m_presentLevelsGrid.set(FIELD_DEVELOPMENT_SRC_STUDENT_STRENGTHS, developmentSrcStudentStrengths);
        m_presentLevelsGrid.set(FIELD_DEVELOPMENT_SRC_PARENT_CONCERNS, developmentSrcParentConcerns);
        m_presentLevelsGrid.set(FIELD_PRESENT_LEVEL, presentLevel);
        m_presentLevelsGrid.set(FIELD_ACCOMMODATION_MODIFICATION, accommodationModification);
        m_presentLevelsGrid.set(FIELD_EDUCATIONAL_NEEDS, educationalNeeds);
        m_presentLevelsGrid.set(FIELD_SPECIAL_FACTORS, specialFactors);
        m_presentLevelsGrid.beforeTop();

        addParameter(DATASOURCE_PRESENT_LEVELS, m_presentLevelsGrid);
    }

    /**
     * This method loads the present levels sub report.
     */
    private void loadModificationsAndSupplementaryAids() {
        getModifAndSupplAidsSubRptCriteria();
        getModifAndSupplAidsCriteria();
    }

    /**
     * This method sets the input stream for the present levels sub report.
     *
     * @return void
     */
    private void getModifAndSupplAidsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_MODIFICATIONS_SUPPLEMENTARY_AIDS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportModifSupplAids = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_MODIFICATIONS_SUPPLEMENTARY_AIDS,
                new ByteArrayInputStream(m_reportModifSupplAids.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the present levels sub report.
     *
     * @return void
     */
    private void getModifAndSupplAidsCriteria() {
        String regEdModif = null;
        String regEdSupplAid = null;
        String regEdOtherReason = null;
        String splEdModif = null;
        String splEdSupplAid = null;
        String splEdOtherReason = null;

        boolean isSpeechIEP = false;
        String iepType = (String) m_currentIep.getFieldValueByAlias(ALIAS_IS_SPEECH_IEP, getDictionary());
        if (BooleanAsStringConverter.TRUE.equals(iepType)) {
            isSpeechIEP = true;
        }
        if (isSpeechIEP) {
            regEdModif =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_MODIF, getDictionary());
            regEdSupplAid = (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_SUPPL_AID,
                    getDictionary());
            regEdOtherReason =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_OTHER, getDictionary());

            m_modifSupplAidsGrid = new ReportDataGrid();

            m_modifSupplAidsGrid.append();
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_TYPE, MODIF_SUPPL_AIDS_ED_TYPE_REGULAR);
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_MODIF, regEdModif != null ? regEdModif : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_SUPPL_AID, regEdSupplAid != null ? regEdSupplAid : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_OTHER, regEdOtherReason != null ? regEdOtherReason : "");

            m_modifSupplAidsGrid.beforeTop();

            addParameter(DATASOURCE_MODIFICATIONS_SUPPLEMENTARY_AIDS, m_modifSupplAidsGrid);
        } else {
            regEdModif =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_MODIF, getDictionary());
            regEdSupplAid = (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_SUPPL_AID,
                    getDictionary());
            regEdOtherReason =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_REG_ED_OTHER, getDictionary());

            splEdModif =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_SPL_ED_MODIF, getDictionary());
            splEdSupplAid = (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_SPL_ED_SUPPL_AID,
                    getDictionary());
            splEdOtherReason =
                    (String) m_currentIep.getFieldValueByAlias(ALIAS_MODIF_SUPPL_AIDS_SPL_ED_OTHER, getDictionary());

            m_modifSupplAidsGrid = new ReportDataGrid();

            m_modifSupplAidsGrid.append();
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_TYPE, MODIF_SUPPL_AIDS_ED_TYPE_REGULAR);
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_MODIF, regEdModif != null ? regEdModif : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_SUPPL_AID, regEdSupplAid != null ? regEdSupplAid : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_OTHER, regEdOtherReason != null ? regEdOtherReason : "");

            m_modifSupplAidsGrid.append();
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_TYPE, MODIF_SUPPL_AIDS_ED_TYPE_SPECIALED);
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_MODIF, splEdModif != null ? splEdModif : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_SUPPL_AID, splEdSupplAid != null ? splEdSupplAid : "");
            m_modifSupplAidsGrid.set(FIELD_MODIF_SUPPL_AIDS_ED_OTHER, splEdOtherReason != null ? splEdOtherReason : "");

            m_modifSupplAidsGrid.beforeTop();

            addParameter(DATASOURCE_MODIFICATIONS_SUPPLEMENTARY_AIDS, m_modifSupplAidsGrid);
        }
    }

    /**
     * This method loads the transition planning sub report.
     */
    private void loadTransitionPlanning() {
        getTransitionPlanningRptCriteria();
        getTransitionPlanningCriteria();
    }

    /**
     * This method sets the input stream for the transition planning sub report.
     *
     * @return void
     */
    private void getTransitionPlanningRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_TRANSITION_PLANNING);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportTransitionPlanning = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_TRANSITION_PLANNING,
                new ByteArrayInputStream(m_reportTransitionPlanning.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the transition planning sub report.
     *
     * @return void
     */
    private void getTransitionPlanningCriteria() {
        String strengthInterestPrefs = null;
        String goalPostsecEd = null;
        String goalEmpCareer = null;
        String goalCommParti = null;
        String goalIndLiving = null;
        String courseGrade1 = null;
        String courseGrade2 = null;
        String courseGrade3 = null;
        String courseGrade4 = null;
        String courses1 = null;
        String courses2 = null;
        String courses3 = null;
        String courses4 = null;
        String stratgyActivit = null;
        String transSettActivi = null;

        strengthInterestPrefs = (String) m_currentIep.getFieldValueByAlias(ALIAS_STRENGTH_INTS_PREFS, getDictionary());
        goalPostsecEd = (String) m_currentIep.getFieldValueByAlias(ALIAS_GOAL_POSTSEC_ED, getDictionary());
        goalEmpCareer = (String) m_currentIep.getFieldValueByAlias(ALIAS_GOAL_EMP_CAREER, getDictionary());
        goalCommParti = (String) m_currentIep.getFieldValueByAlias(ALIAS_GOAL_COMM_PARTI, getDictionary());
        goalIndLiving = (String) m_currentIep.getFieldValueByAlias(ALIAS_GOAL_IND_LIVING, getDictionary());
        courseGrade1 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_GRADE1, getDictionary());
        courseGrade2 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_GRADE2, getDictionary());
        courseGrade3 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_GRADE3, getDictionary());
        courseGrade4 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_GRADE4, getDictionary());
        courses1 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_COURSES1, getDictionary());/*
                                                                                                       * courseNames
                                                                                                       * .
                                                                                                       * toString
                                                                                                       * (
                                                                                                       * )
                                                                                                       * ;
                                                                                                       */
        courses2 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_COURSES2, getDictionary());
        courses3 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_COURSES3, getDictionary());
        courses4 = (String) m_currentIep.getFieldValueByAlias(ALIAS_COURSE_COURSES4, getDictionary());
        stratgyActivit = (String) m_currentIep.getFieldValueByAlias(ALIAS_STRATGY_ACTIVITY, getDictionary());
        transSettActivi = (String) m_currentIep.getFieldValueByAlias(ALIAS_TRANS_SETT_ACTIVIT, getDictionary());

        m_transPlanningGrid = new ReportDataGrid();
        m_transPlanningGrid.append();
        m_transPlanningGrid.set(FIELD_STRENGTH_INTS_PREFS, strengthInterestPrefs);
        m_transPlanningGrid.set(FIELD_GOAL_POSTSEC_ED, goalPostsecEd);
        m_transPlanningGrid.set(FIELD_GOAL_EMP_CAREER, goalEmpCareer);
        m_transPlanningGrid.set(FIELD_GOAL_COMM_PARTI, goalCommParti);
        m_transPlanningGrid.set(FIELD_GOAL_IND_LIVING, goalIndLiving);
        m_transPlanningGrid.set(FIELD_COURSE_GRADE1, courseGrade1);
        m_transPlanningGrid.set(FIELD_COURSE_GRADE2, courseGrade2);
        m_transPlanningGrid.set(FIELD_COURSE_GRADE3, courseGrade3);
        m_transPlanningGrid.set(FIELD_COURSE_GRADE4, courseGrade4);
        m_transPlanningGrid.set(FIELD_COURSE_COURSES1, courses1);
        m_transPlanningGrid.set(FIELD_COURSE_COURSES2, courses2);
        m_transPlanningGrid.set(FIELD_COURSE_COURSES3, courses3);
        m_transPlanningGrid.set(FIELD_COURSE_COURSES4, courses4);
        m_transPlanningGrid.set(FIELD_STRATGY_ACTIVITY, stratgyActivit);
        m_transPlanningGrid.set(FIELD_TRANS_SETT_ACTIVIT, transSettActivi);

        m_transPlanningGrid.beforeTop();

        addParameter(DATASOURCE_TRANSITION_PLANNING, m_transPlanningGrid);
    }

    /**
     * This method loads the transition services sub report.
     */
    private void loadTransitionServices() {
        getTransitionSvcsSubRptCriteria();
        loadTransitionSvcsRefCodes();
        getTransitionSvcsCriteria();
    }

    /**
     * This method sets the input stream for the transition services sub report.
     *
     * @return void
     */
    private void getTransitionSvcsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_TRANSITION_SVCS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportTransSvcs = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_TRANSITION_SERVICES,
                new ByteArrayInputStream(m_reportTransSvcs.getCompiledFormat()));
    }

    /**
     * This method loads the transition services reference codes.
     */
    private void loadTransitionSvcsRefCodes() {
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID,
                TRANSITION_SVC_REF_TABLE);
        refCodeCriteria.addEqualTo(ReferenceCode.COL_DEPENDENCY_CODE, SERVICE_TYPE_MODE_TRANSITION);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        refCodeQuery.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);

        m_transitionSvcsRefCodes = getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 8);
    }

    /**
     * This method sets the criteria for the transition services sub report.
     *
     * @return void
     */
    private void getTransitionSvcsCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, m_currentIep.getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, PARAM_SERVICE_NAME_TRANSITION);
        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_TYPE);

        Map<String, Collection<IepService>> servicesMap =
                getBroker().getGroupedCollectionByQuery(servicesQuery, IepService.COL_SERVICE_TYPE, 32);

        m_transServicesGrid = new ReportDataGrid();
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");

        List<String> transitionSvcCodes = new ArrayList<String>(m_transitionSvcsRefCodes.keySet());

        for (String serviceCode : transitionSvcCodes) {
            ReferenceCode refCode = m_transitionSvcsRefCodes.get(serviceCode);
            if (SERVICE_TYPE_MODE_TRANSITION.equalsIgnoreCase(refCode.getDependencyCode())) {
                if (servicesMap.containsKey(serviceCode)) {
                    Collection<IepService> services = servicesMap.get(serviceCode);
                    for (IepService service : services) {
                        m_transServicesGrid.append();
                        m_transServicesGrid.set(FIELD_SERVICE_TYPE, refCode.getDescription() + ":");
                        m_transServicesGrid.set(FIELD_SERVICE_DETAILS,
                                service.getFieldValueByAlias(ALIAS_SERVICE_ACTIVITY, getDictionary()));
                        Date startDate = service.getStartDate();
                        Date endDate = service.getEndDate();
                        m_transServicesGrid.set(FIELD_SERVICE_START_DATE, EMPTY_STRING);
                        if (startDate != null && endDate != null) {
                            m_transServicesGrid.set(FIELD_SERVICE_START_DATE, dateFormat.format(startDate)
                                    + " - " + dateFormat.format(endDate));
                        }
                        String serviceProviderDetails =
                                (String) service.getFieldValueByAlias(ALIAS_SERVICE_PROVIDER, getDictionary());
                        String serviceProviderOtherDetails =
                                (String) service.getFieldValueByAlias(ALIAS_SERVICE_PROVIDER_OTHER, getDictionary());

                        // if there are other service provider details and the user has also
                        // selected other,
                        // replace other with the other service provider details.
                        if (!StringUtils.isEmpty(serviceProviderOtherDetails)) {
                            if (serviceProviderDetails.contains(CODE_SERVICE_PROVIDER_OTHER)) {
                                serviceProviderDetails = serviceProviderDetails.replace(CODE_SERVICE_PROVIDER_OTHER,
                                        serviceProviderOtherDetails);
                            }
                        }

                        m_transServicesGrid.set(FIELD_SERVICE_PROVIDER, serviceProviderDetails);
                    }
                } else {
                    m_transServicesGrid.append();
                    m_transServicesGrid.set(FIELD_SERVICE_TYPE, refCode.getDescription() + ":");
                    m_transServicesGrid.set(FIELD_SERVICE_DETAILS, SERVICE_NOT_ADDED);
                    m_transServicesGrid.set(FIELD_SERVICE_START_DATE, EMPTY_STRING);
                    m_transServicesGrid.set(FIELD_SERVICE_PROVIDER, EMPTY_STRING);
                }
            }
        }
        m_transServicesGrid.beforeTop();

        addParameter(DATASOURCE_TRANSITION_SERVICES, m_transServicesGrid);
    }

    /**
     * This method loads the rights sub report.
     *
     * @throws Exception exception
     */
    private void loadRights() throws Exception {
        getRightsRptCriteria();
        getRightsCriteria();
    }

    /**
     * This method sets the input stream for the rights sub report.
     *
     * @return void
     */
    private void getRightsRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_RIGHTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportRights = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_RIGHTS, new ByteArrayInputStream(m_reportRights.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the rights sub report.
     *
     * @return void
     * @throws Exception exception
     */
    private void getRightsCriteria() throws Exception {
        String option1Line1 = null;
        String option1Line2 = null;
        String option1Line3 = null;
        String option1Line4 = null;
        String option1Line5 = null;
        String option1Line6 = null;
        String option2Line1 = null;
        String option2Line2 = null;

        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd");
        SisStudent student = m_currentIep.getStudent();
        String stdFullName = new StringBuilder(student.getPerson().getFirstName()).append(" ")
                .append(student.getPerson().getLastName()).toString();
        PlainDate dob = student.getPerson().getDob();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(dob);
        calendar.add(Calendar.YEAR, 18);
        String dobPlus18Yrs = dateFormat.format(calendar.getTime());

        option1Line1 = new StringBuilder("On ")
                .append(dobPlus18Yrs)
                .append(", ")
                .append(stdFullName)
                .append("  will turn age 18 and become an adult student. " +
                        "The following rights will transfer to ")
                .append(stdFullName)
                .append(":").toString();

        option1Line2 = new StringBuilder(">> The school district must receive written permission from ")
                .append(stdFullName)
                .append(" before it conducts any assessments as part of an evaluation " +
                        "or reevaluation and before implementing an IEP for the first " +
                        "time.")
                .toString();

        option1Line3 = new StringBuilder(">> The school must send a written notice to ")
                .append(stdFullName)
                .append(" whenever it wishes to change or refuses to change the " +
                        "evaluation, eligibility, individualized education program (IEP), " +
                        "placement, or the provision of a free appropriate public  " +
                        "education (FAPE).")
                .toString();

        option1Line4 = new StringBuilder(">> You, the parent(s) may not have access to ")
                .append(stdFullName)
                .append("\'s educational records without the student's consent, unless the student continues to be financially "
                        +
                        "dependent on you.")
                .toString();

        option1Line5 = new StringBuilder(">> Any time ")
                .append(stdFullName)
                .append(" disagrees with the special education program, the student is the only " +
                        "one who can request mediation or a due process hearing to resolve any " +
                        "disputes arising in those areas.")
                .toString();

        option1Line6 = new StringBuilder("If ")
                .append(stdFullName)
                .append(" wishes,  the student may write a letter to the school giving you, the " +
                        "parent(s), the right to continue to act on " + student.getPerson().getFirstName()
                        + "'s behalf in these " +
                        "matters.")
                .toString();

        String studentNotified = "";
        String parentNotified = "";
        String studentNotificationDate = DASHED_STRING;
        String parentNotificationDate = DASHED_STRING;
        String contactName = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_CONTACT_NAME, getDictionary());
        String optionSelected =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_TRANSFER_OF_RIGHTS_OPTION, getDictionary());
        String guardianshipNotes =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_GUARDIANSHIP_NOTES, getDictionary());

        if (TRANSFER_OF_RIGHTS_OPTION2.equalsIgnoreCase(optionSelected)) {
            studentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED, getDictionary());
            parentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED, getDictionary());
            try {
                String notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED_DATE, getDictionary());
                Date tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    studentNotificationDate = dateFormat.format(tempDate);
                }

                notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED_DATE, getDictionary());
                tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    parentNotificationDate = dateFormat.format(tempDate);
                } else {
                    parentNotificationDate = DASHED_STRING;
                }
            } catch (ParseException e) {
                throw e;
            }
        } else {
            studentNotified = "";
            parentNotified = "";
            contactName = DASHED_STRING;
            studentNotificationDate = DASHED_STRING;
            parentNotificationDate = DASHED_STRING;
        }

        option2Line1 = new StringBuilder().append(stdFullName).append(" was informed in writing on ")
                .append(studentNotificationDate)
                .append(" of the rights that will transfer to the student at age eighteen.").toString();

        option2Line2 = new StringBuilder().append(contactName).append(" was/were informed in writing on ")
                .append(parentNotificationDate).append(" of the rights that will transfer at age eighteen.").toString();

        m_rightsGrid = new ReportDataGrid();
        m_rightsGrid.append();
        m_rightsGrid.set(FIELD_OPTION1_LINE1, option1Line1);
        m_rightsGrid.set(FIELD_OPTION1_LINE2, option1Line2);
        m_rightsGrid.set(FIELD_OPTION1_LINE3, option1Line3);
        m_rightsGrid.set(FIELD_OPTION1_LINE4, option1Line4);
        m_rightsGrid.set(FIELD_OPTION1_LINE5, option1Line5);
        m_rightsGrid.set(FIELD_OPTION1_LINE6, option1Line6);
        m_rightsGrid.set(FIELD_OPTION2_LINE1, option2Line1);
        m_rightsGrid.set(FIELD_OPTION2_LINE2, option2Line2);
        m_rightsGrid.set(FIELD_STUDENT_NOTIFIED, studentNotified);
        m_rightsGrid.set(FIELD_PARENT_NOTIFIED, parentNotified);
        m_rightsGrid.set(FIELD_OPTION_SELECTED, optionSelected);
        m_rightsGrid.set(FIELD_GUARDIANSHIP_NOTES, guardianshipNotes);
        m_rightsGrid.beforeTop();

        addParameter(DATASOURCE_RIGHTS, m_rightsGrid);
    }

    /**
     * This method loads the behavioral intervention sub report.
     */
    private void loadBehavioralInterventions() {
        getBehavioralIntSubRptCriteria();
        getBehavioralIntCriteria();
    }

    /**
     * This method sets the input stream for the behavioral intervention sub report.
     *
     * @return void
     */
    private void getBehavioralIntSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_BEHAVIORAL_INTERVENTION);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportBehavioralInt = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_BEHAVIORAL_INTERVENTION,
                new ByteArrayInputStream(m_reportBehavioralInt.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the Behavioral Intervention sub report.
     *
     * @return void
     */
    private void getBehavioralIntCriteria() {
        String targetBehavior = null;
        String priorIntStdResp = null;
        String posSuppIntDesc = null;
        String dataCollectProc = null;
        String suppIntCondChg = null;
        String suppIntCondTerm = null;
        String parentalInvolvement = null;

        targetBehavior = (String) m_currentIep.getFieldValueByAlias(ALIAS_TARGET_BEHAVIORAL, getDictionary());
        priorIntStdResp = (String) m_currentIep.getFieldValueByAlias(ALIAS_PRIOR_INT_STD_RESP, getDictionary());
        posSuppIntDesc = (String) m_currentIep.getFieldValueByAlias(ALIAS_POS_SUPP_INT_DESC, getDictionary());
        dataCollectProc = (String) m_currentIep.getFieldValueByAlias(ALIAS_DATA_COLLECT_PROC, getDictionary());
        suppIntCondChg = (String) m_currentIep.getFieldValueByAlias(ALIAS_SUPP_INT_COND_CHANGE, getDictionary());
        suppIntCondTerm = (String) m_currentIep.getFieldValueByAlias(ALIAS_SUPP_INT_COND_TERMINATE, getDictionary());
        parentalInvolvement = (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENTAL_INVOLVEMENT, getDictionary());

        m_behavioralIntGrid = new ReportDataGrid();
        m_behavioralIntGrid.append();
        m_behavioralIntGrid.set(FIELD_TARGET_BEHAVIOR, targetBehavior);
        m_behavioralIntGrid.set(FIELD_PRIOR_INT_STD_RESPONSE, priorIntStdResp);
        m_behavioralIntGrid.set(FIELD_POS_SUPP_INT, posSuppIntDesc);
        m_behavioralIntGrid.set(FIELD_DATA_COLLECTION_PROC, dataCollectProc);
        m_behavioralIntGrid.set(FIELD_CONDITIONS_SUPP_INT_CHG, suppIntCondChg);
        m_behavioralIntGrid.set(FIELD_CONDITIONS_SUPP_INT_TERMINATE, suppIntCondTerm);
        m_behavioralIntGrid.set(FIELD_PARENTAL_INVOLVEMENT, parentalInvolvement);
        m_behavioralIntGrid.beforeTop();

        addParameter(DATASOURCE_BEHAVIORAL_INTERVENTION, m_behavioralIntGrid);
    }

    /**
     * This method loads the inter agency(other) services sub report.
     */
    private void loadInterAgencyServices() {
        getInterAgencySubRptCriteria();
        getInterAgencyCriteria();
    }

    /**
     * This method sets the input stream for the inter agency sub report.
     *
     * @return void
     */
    private void getInterAgencySubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_INTERAGENCY);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportInterAgency = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_INTERAGENCY, new ByteArrayInputStream(m_reportInterAgency.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the inter agency sub report.
     *
     * @return void
     */
    private void getInterAgencyCriteria() {
        Criteria agencyCriteria = new Criteria();
        agencyCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + ReferenceTable.COL_USER_NAME,
                USER_NAME_REFTABLE_INTERAGENCY);

        QueryByCriteria cGetAgencyQuery = new QueryByCriteria(ReferenceCode.class, agencyCriteria);

        QueryIterator attniterator = getBroker().getIteratorByQuery(cGetAgencyQuery);

        try {
            while (attniterator.hasNext()) {
                ReferenceCode refCode = (ReferenceCode) attniterator.next();
                mAgencyInfo.put(refCode.getCode(), refCode.getDescription());
                mAgencyInfo.put(refCode.getCode() + "Phone", refCode.getLocalizedDescription(1));
                mAgencyInfo.put(refCode.getCode() + "Web", refCode.getLocalizedDescription(2));
            }
        } finally {
            attniterator.close();
        }

        String agencyName = null;
        String schoolDistResp = null;
        String stdParentResp = null;
        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());

        m_interAgencyGrid = new ReportDataGrid();
        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            String serviceType = otherInterAgencySvc.getServiceType();

            if (PARAM_SERVICE_NAME_INTERAGENCY.equals(serviceType)) {
                agencyName = (String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_NAME, getDictionary());
                agencyName = mAgencyInfo.get(agencyName);
                schoolDistResp = (String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_SCHOOL_DIST_RESP,
                        getDictionary());
                stdParentResp = (String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_STD_PARENT_RESP,
                        getDictionary());
                m_interAgencyGrid.append();
                m_interAgencyGrid.set(FIELD_INTERAGENCY_OID, otherInterAgencySvc.getOid());
                m_interAgencyGrid.set(FIELD_INTERAGENCY_NAME, agencyName);
                m_interAgencyGrid.set(FIELD_INTERAGENCY_SCHOOL_DIST_RESP, schoolDistResp);
                m_interAgencyGrid.set(FIELD_INTERAGENCY_STD_PARENT_RESP, stdParentResp);
            }
        }

        if (otherInterAgencySvcs.isEmpty() || otherInterAgencySvcs.size() < 3) {
            for (int i = 0; i < (3 - otherInterAgencySvcs.size()); i++) {
                m_interAgencyGrid.append();
                m_interAgencyGrid.set(FIELD_INTERAGENCY_OID, EMPTY_STRING);
                m_interAgencyGrid.set(FIELD_INTERAGENCY_NAME, EMPTY_STRING);
                m_interAgencyGrid.set(FIELD_INTERAGENCY_SCHOOL_DIST_RESP, EMPTY_STRING);
                m_interAgencyGrid.set(FIELD_INTERAGENCY_STD_PARENT_RESP, EMPTY_STRING);
            }
        }
        m_interAgencyGrid.beforeTop();

        addParameter(DATASOURCE_INTERAGENCY_OTHER_SERVICES, m_interAgencyGrid);
    }

    /**
     * This method loads the goals and objectives sub report.
     */
    private void loadGoalsAndObjectives() {
        getGoalsAndObjectivesSubRptCriteria();
        getGoalsAndObjectivesCriteria();
    }

    /**
     * This method sets the input stream for the goals sub report.
     * It then sets the input stream for the objectives sub report.
     *
     * @return void
     */
    private void getGoalsAndObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_GOALS_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportGoals = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_GOALS, new ByteArrayInputStream(m_reportGoals.getCompiledFormat()));

        getObjectivesSubRptCriteria();
    }

    /**
     * This method sets the criteria for the goals and objectives.
     *
     * @return void
     */
    private void getGoalsAndObjectivesCriteria() {
        Criteria goalsCriteria = new Criteria();
        goalsCriteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, m_currentIep.getOid());
        QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, goalsCriteria);
        goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
        goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);

        Collection<IepGoal> goals = getBroker().getCollectionByQuery(goalsQuery);

        m_goalsGrid = new ReportDataGrid();
        if (goals.isEmpty()) {
            goals.add(X2BaseBean.newInstance(IepGoal.class, getBroker().getPersistenceKey()));
        }

        for (IepGoal goal : goals) {
            String goalId = goal.getId();
            String goalDescription = goal.getGoal();

            Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives(getBroker());
            m_objectivesGrid = new ReportDataGrid();
            for (IepGoalObjective objective : objectives) {
                String shortTermBenchmark =
                        (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_BENCHMARK, getDictionary());
                String criteria = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_CRITERIA, getDictionary());
                String evalProc = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_EVAL_PROC, getDictionary());

                m_objectivesGrid.append();
                m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, shortTermBenchmark);
                m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, criteria);
                m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, evalProc);
            }
            // if (objectives.size() < 4)
            // {
            // for (int i=0; i<(4-objectives.size()); i++)
            // {
            // m_objectivesGrid.append();
            // m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, EMPTY_STRING);
            // m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, EMPTY_STRING);
            // m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, EMPTY_STRING);
            // }
            // }
            if (objectives.isEmpty()) {
                m_objectivesGrid.append();
                m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, EMPTY_STRING);
                m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, EMPTY_STRING);
                m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, EMPTY_STRING);
            }
            m_objectivesGrid.beforeTop();

            m_goalsGrid.append();
            m_goalsGrid.set(FIELD_GOAL_ID, goalId);
            m_goalsGrid.set(FIELD_GOAL_OID, goal.getOid());
            m_goalsGrid.set(FIELD_GOAL_FOCUS, goal.getFocus());
            m_goalsGrid.set(FIELD_GOAL_DESCRIPTION, goalDescription);
            m_goalsGrid.set(FIELD_DATA_GRID_GOAL_OBJECTIVES, m_objectivesGrid);
        }

        m_goalsGrid.beforeTop();

        addParameter(DATASOURCE_GOALS, m_goalsGrid);
    }

    /**
     * This method is called when the goals sub report is loaded.
     * This method sets the objectives input stream.
     *
     * @return void
     */
    private void getObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportObjectives = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_OBJECTIVES, new ByteArrayInputStream(m_reportObjectives.getCompiledFormat()));
    }

    /**
     * This method loads the sped and related services sub report.
     */
    private void loadSpedRelServices() {
        getSvcsSubRptCriteria();
        getSvcsCriteria();
    }

    /**
     * This method sets the input stream for the services sub report, which in turn
     * has sub reports for sped services and related services.
     *
     * @return void
     */
    private void getSvcsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SERVICES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportServices = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_SERVICES, new ByteArrayInputStream(m_reportServices.getCompiledFormat()));

        getInnerSubRptCriteria();
    }

    /**
     * This method sets the input stream for both the sped and related services sub report.
     *
     * @return void
     */
    private void getInnerSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SERVICES_SPED);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportSpedServices = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_SPED, new ByteArrayInputStream(m_reportSpedServices.getCompiledFormat()));

        criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SERVICES_REL);
        query = new QueryByCriteria(Report.class, criteria);

        m_reportRelatedServices = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_RELATED, new ByteArrayInputStream(m_reportRelatedServices.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the services.
     * The services grid in turn has a data grid for both sped and related services.
     *
     * @return void
     */
    private void getSvcsCriteria() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        m_servicesGrid = new ReportDataGrid();

        // Sped Services service mode
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, m_currentIep.getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, PARAM_SERVICE_NAME_SPECIAL_ED);
        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_TYPE);

        Collection<IepService> spedServices = getBroker().getCollectionByQuery(servicesQuery);

        m_spedServicesGrid = new ReportDataGrid();
        if (spedServices.isEmpty()) {
            spedServices.add(X2BaseBean.newInstance(IepService.class, getBroker().getPersistenceKey()));
        }

        for (IepService service : spedServices) {
            m_spedServicesGrid.append();
            m_spedServicesGrid.set(FIELD_SERVICE_OID, service.getOid());

            // retrieve and write service type to grid
            String serviceType = service.getServiceType();
            m_spedServicesGrid.set(FIELD_SERVICE_TYPE, serviceType);

            if (service.getStartDate() != null && service.getEndDate() != null) {
                m_spedServicesGrid.set(FIELD_SERVICE_DATE, dateFormat.format(service.getStartDate())
                        + " - " + dateFormat.format(service.getEndDate()));
                if (service.getCycle() != null) {
                    m_spedServicesGrid.set(FIELD_SERVICE_FREQUENCY,
                            service.getFrequency().toString() + "/" + service.getCycle());
                } else {
                    m_spedServicesGrid.set(FIELD_SERVICE_FREQUENCY, service.getFrequency().toString());
                }
            } else {
                m_spedServicesGrid.set(FIELD_SERVICE_DATE, EMPTY_STRING);
                m_spedServicesGrid.set(FIELD_SERVICE_FREQUENCY, EMPTY_STRING);
            }

            m_spedServicesGrid.set(FIELD_SERVICE_LOCATION,
                    service.getFieldValueByAlias(ALIAS_LOCATION_CODE, getDictionary()));
            if (service.getStudent() != null) {
                m_spedServicesGrid.set(FIELD_SERVICE_DURATION, Integer.toString(service.getDuration())); // returns
                                                                                                         // int
            }
        }
        m_spedServicesGrid.beforeTop();

        // Related Services service mode
        criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, m_currentIep.getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, PARAM_SERVICE_NAME_RELATED);
        servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_TYPE);

        Collection<IepService> relatedServices = getBroker().getCollectionByQuery(servicesQuery);

        m_relServicesGrid = new ReportDataGrid();
        if (relatedServices.isEmpty()) {
            relatedServices.add(X2BaseBean.newInstance(IepService.class, getBroker().getPersistenceKey()));
        }

        for (IepService service : relatedServices) {
            m_relServicesGrid.append();
            String serviceType = service.getServiceType();

            m_relServicesGrid.set(FIELD_SERVICE_TYPE, serviceType);
            m_relServicesGrid.set(FIELD_SERVICE_OID, service.getOid());
            if (service.getStartDate() != null && service.getEndDate() != null) {
                m_relServicesGrid.set(FIELD_SERVICE_DATE, dateFormat.format(service.getStartDate())
                        + " - " + dateFormat.format(service.getEndDate()));
                if (service.getCycle() != null) {
                    m_relServicesGrid.set(FIELD_SERVICE_FREQUENCY,
                            service.getFrequency().toString() + "/" + service.getCycle());
                } else {
                    m_relServicesGrid.set(FIELD_SERVICE_FREQUENCY, service.getFrequency().toString());
                }
            } else {
                m_relServicesGrid.set(FIELD_SERVICE_DATE, EMPTY_STRING);
                m_relServicesGrid.set(FIELD_SERVICE_FREQUENCY, EMPTY_STRING);
            }

            m_relServicesGrid.set(FIELD_SERVICE_LOCATION,
                    service.getFieldValueByAlias(ALIAS_LOCATION_CODE, getDictionary()));
            if (service.getStudent() != null) {
                m_relServicesGrid.set(FIELD_SERVICE_DURATION, Integer.toString(service.getDuration())); // returns
                                                                                                        // int
            }
        }
        m_relServicesGrid.beforeTop();

        m_servicesGrid.append();
        m_servicesGrid.set(FIELD_DATA_GRID_SPED, m_spedServicesGrid);
        m_servicesGrid.set(FIELD_DATA_GRID_RELATED, m_relServicesGrid);
        m_servicesGrid.beforeTop();

        addParameter(DATASOURCE_SERVICES, m_servicesGrid);
    }

    /**
     * This method loads the assessment sub report.
     */
    private void loadAssessments() {
        getAssessmentsSubRptCriteria();
        getAssessmentsCriteria();
    }

    /**
     * This method sets the input stream for the assessments sub report.
     *
     * @return void
     */
    private void getAssessmentsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_ASSESSMENTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportAssessment = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_ASSESSMENT, new ByteArrayInputStream(m_reportAssessment.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the assessments sub report.
     *
     * @return void
     */
    private void getAssessmentsCriteria() {
        Collection<IepAccommodation> accommodations = m_currentIep.getAccommodations();

        // Sort Accommodations
        TreeMap<String, IepAccommodation> iepAccommodations = new TreeMap<String, IepAccommodation>();
        for (IepAccommodation accommodation : accommodations) {
            String key = accommodation.getCategory() + "-" + accommodation.getOid();
            iepAccommodations.put(key, accommodation);
        }
        accommodations = iepAccommodations.values();

        String distAssessment = (String) m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT, getDictionary());
        String distAssessmentModif =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT_MODIFICATION, getDictionary());
        String distAssessmentNotAppr = (String) m_currentIep
                .getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT_NOT_APPROPRIATE_REASON, getDictionary());
        String assessmentType = (String) m_currentIep.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE, getDictionary());

        m_assessmentGrid = new ReportDataGrid();
        for (IepAccommodation accommodation : accommodations) {
            String grade = accommodation.getCategory();
            String isStateArts = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE1, getDictionary());
            String isStateMaths = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE2, getDictionary());
            String isStateScience = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE3, getDictionary());
            String alternateAssessmentType =
                    (String) accommodation.getFieldValueByAlias(ALIAS_ALT_ASSESS_TYPE, getDictionary());
            String isApaArts = (String) accommodation.getFieldValueByAlias(ALIAS_APA_COURSE1, getDictionary());
            String isApaMaths = (String) accommodation.getFieldValueByAlias(ALIAS_APA_COURSE2, getDictionary());
            String isApaScience = (String) accommodation.getFieldValueByAlias(ALIAS_APA_COURSE3, getDictionary());
            String modifications = (String) accommodation.getFieldValueByAlias(ALIAS_MODIFICATIONS, getDictionary());
            String notApprReason = (String) accommodation.getFieldValueByAlias(ALIAS_REASON_NOT_APPR, getDictionary());
            String otherReason =
                    (String) accommodation.getFieldValueByAlias(ALIAS_MODIFICATIONS_OTHER, getDictionary());

            if (modifications != null && modifications.contains("Other") && otherReason != null) {
                modifications = modifications.replaceAll("Other", "Other: " + otherReason);
            }

            m_assessmentGrid.append();
            m_assessmentGrid.set(FIELD_STATE_CODE, grade);
            m_assessmentGrid.set(FIELD_STATE_ARTS, isStateArts);
            m_assessmentGrid.set(FIELD_STATE_MATHS, isStateMaths);
            m_assessmentGrid.set(FIELD_STATE_SCIENCE, isStateScience);
            m_assessmentGrid.set(FIELD_MODIF, modifications);
            m_assessmentGrid.set(FIELD_NOT_APPR, notApprReason);
            m_assessmentGrid.set(FIELD_ALT_ASSESS_TYPE, alternateAssessmentType);
            m_assessmentGrid.set(FIELD_APA_ARTS, isApaArts);
            m_assessmentGrid.set(FIELD_APA_MATHS, isApaMaths);
            m_assessmentGrid.set(FIELD_APA_SCIENCE, isApaScience);
        }

        // If these are no assessment (IepAccommodation) records still add one so that the Page
        // Header information is displayed.
        if (accommodations.size() == 0) {
            m_assessmentGrid.append();
        }

        m_assessmentGrid.beforeTop();

        addParameter(PARAM_DISTRICT_ASSESSMENT, distAssessment);
        addParameter(PARAM_ASSESSMENT_MODIFICATIONS, distAssessmentModif);
        addParameter(PARAM_ASSESSMENT_NOT_APPR_REASON, distAssessmentNotAppr);
        addParameter(PARAM_ASSESSMENT_TYPE, assessmentType);
        addParameter(DATASOURCE_ASSESSMENT, m_assessmentGrid);
    }

    /**
     * This method loads the assessment sub report.
     */
    private void loadSpeechAssessments() {
        getSpeechAssessmentsSubRptCriteria();
        // getSpeechAssessmentsCriteria();
        getAssessmentsCriteria();
    }

    /**
     * This method sets the input stream for the assessments sub report.
     *
     * @return void
     */
    private void getSpeechAssessmentsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_ASSESSMENTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportAssessment = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_ASSESSMENT, new ByteArrayInputStream(m_reportAssessment.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the assessments sub report.
     */
    /*
     * private void getSpeechAssessmentsCriteria()
     * {
     * Collection<IepAccommodation> accommodations = m_currentIep.getAccommodations();
     * m_assessmentGrid = new ReportDataGrid();
     * m_assessmentGrid.append();
     *
     * String distAssessment = (String) m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT,
     * getDictionary());
     * String distAssessmentModif = (String)
     * m_currentIep.getFieldValueByAlias(ALIAS_DISTRICT_ASSESSMENT_MODIFICATION, getDictionary());
     *
     * String assessmentType = (String) m_currentIep.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE,
     * getDictionary());
     * m_assessmentGrid.set(FIELD_DISTRICT_ASSESSMENT, distAssessment);
     * m_assessmentGrid.set(FIELD_DIST_ASSESSMENT_MODIFICATIONS, distAssessmentModif);
     * m_assessmentGrid.set(FIELD_ASSESSMENT_TYPE, assessmentType);
     *
     * if (!accommodations.isEmpty())
     * {
     * Iterator<IepAccommodation> iter = accommodations.iterator();
     * IepAccommodation accommodation = iter.next();
     * String grade = accommodation.getCategory();
     * String isStateArts = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE1,
     * getDictionary());
     * String isStateMaths = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE2,
     * getDictionary());
     * String isStateScience = (String) accommodation.getFieldValueByAlias(ALIAS_STATE_COURSE3,
     * getDictionary());
     * String modifications = (String) accommodation.getFieldValueByAlias(ALIAS_MODIFICATIONS,
     * getDictionary());
     * String otherReason = (String) accommodation.getFieldValueByAlias(ALIAS_MODIFICATIONS_OTHER,
     * getDictionary());
     *
     * if (modifications != null && modifications.contains("Other") && otherReason != null)
     * {
     * modifications = modifications.replaceAll("Other", "Other: " + otherReason);
     * }
     *
     * if (REFCODE_GRADE3.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE3_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE3_MATHS, isStateMaths);
     * }
     * else if (REFCODE_GRADE4.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_MATHS, isStateMaths);
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_SCIENCE, isStateScience);
     * }
     * else if (REFCODE_GRADE5.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE5_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE5_MATHS, isStateMaths);
     * }
     * else if (REFCODE_GRADE6.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE6_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE6_MATHS, isStateMaths);
     * }
     * else if (REFCODE_GRADE7.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE7_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE7_MATHS, isStateMaths);
     * }
     * else if (REFCODE_GRADE8.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_MATHS, isStateMaths);
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_SCIENCE, isStateScience);
     * }
     * else if (REFCODE_HSPA.equalsIgnoreCase(grade) || REFCODE_SRA.equalsIgnoreCase(grade) ||
     * REFCODE_PARCC.equalsIgnoreCase(grade))
     * {
     * if (REFCODE_HSPA.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_HSPA, grade);
     * m_assessmentGrid.set(FIELD_SRA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_PARCC, EMPTY_STRING);
     * }
     * else if (REFCODE_SRA.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_HSPA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_SRA, grade);
     * m_assessmentGrid.set(FIELD_PARCC, EMPTY_STRING);
     * }
     * else if (REFCODE_PARCC.equalsIgnoreCase(grade))
     * {
     * m_assessmentGrid.set(FIELD_HSPA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_SRA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_PARCC, grade);
     * }
     * m_assessmentGrid.set(FIELD_HSPA_SRA_ARTS, isStateArts);
     * m_assessmentGrid.set(FIELD_HSPA_SRA_MATHS, isStateMaths);
     * m_assessmentGrid.set(FIELD_HSPA_SRA_SCIENCE, isStateScience);
     * }
     * m_assessmentGrid.set(FIELD_STATE_ASSESSMENT_MODIFICATIONS, modifications);
     * }
     * else
     * {
     * m_assessmentGrid.set(FIELD_STATE_GRADE3_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE3_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE4_SCIENCE, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE5_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE5_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE6_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE6_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE7_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE7_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_GRADE8_SCIENCE, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_HSPA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_SRA, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_HSPA_SRA_ARTS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_HSPA_SRA_MATHS, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_HSPA_SRA_SCIENCE, EMPTY_STRING);
     * m_assessmentGrid.set(FIELD_STATE_ASSESSMENT_MODIFICATIONS, EMPTY_STRING);
     * }
     * m_assessmentGrid.beforeTop();
     *
     * addParameter(DATASOURCE_ASSESSMENT, m_assessmentGrid);
     * }
     */
    /**
     * This method loads the goals and objectives sub report.
     */
    private void loadSpeechGoalsAndObjectives() {
        getSpeechGoalsAndObjectivesSubRptCriteria();
        // getSpeechGoalsAndObjectivesCriteria();
        getGoalsAndObjectivesCriteria();
    }

    /**
     * This method sets the input stream for the goals sub report.
     * It then sets the input stream for the objectives sub report.
     *
     * @return void
     */
    private void getSpeechGoalsAndObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_GOALS_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportGoals = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_GOALS, new ByteArrayInputStream(m_reportGoals.getCompiledFormat()));

        getSpeechObjectivesSubRptCriteria();
    }

    /**
     * This method sets the criteria for the goals and objectives.
     *
     * @return void
     */
    /*
     * private void getSpeechGoalsAndObjectivesCriteria()
     * {
     * Criteria goalsCriteria = new Criteria();
     * goalsCriteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, m_currentIep.getOid());
     * QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, goalsCriteria);
     * goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
     * goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);
     * Collection<IepGoal> goals = getBroker().getCollectionByQuery(goalsQuery);
     *
     * m_goalsGrid = new ReportDataGrid();
     * if (goals.isEmpty())
     * {
     * goals.add((IepGoal) X2BaseBean.newInstance(IepGoal.class, getBroker().getPersistenceKey()));
     * }
     * for (IepGoal goal : goals)
     * {
     * String goalId = goal.getId();
     * String goalDescription = goal.getGoal();
     *
     * Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives(getBroker());
     * m_objectivesGrid = new ReportDataGrid();
     * for (IepGoalObjective objective : objectives)
     * {
     * String shortTermBenchmark = (String)
     * objective.getFieldValueByAlias(ALIAS_OBJECTIVE_BENCHMARK, getDictionary());
     * String criteria = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_CRITERIA,
     * getDictionary());
     * String evalProc = (String) objective.getFieldValueByAlias(ALIAS_OBJECTIVE_EVAL_PROC,
     * getDictionary());
     *
     * m_objectivesGrid.append();
     * m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, shortTermBenchmark);
     * m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, criteria);
     * m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, evalProc);
     * }
     * if (objectives.size() < 4)
     * {
     * for (int i=0; i<(4-objectives.size()); i++)
     * {
     * m_objectivesGrid.append();
     * m_objectivesGrid.set(FIELD_OBJECTIVE_BENCHMARK, EMPTY_STRING);
     * m_objectivesGrid.set(FIELD_OBJECTIVE_CRITERIA, EMPTY_STRING);
     * m_objectivesGrid.set(FIELD_OBJECTIVE_EVALUATION_PROC, EMPTY_STRING);
     * }
     * }
     * m_objectivesGrid.beforeTop();
     *
     * m_goalsGrid.append();
     * m_goalsGrid.set(FIELD_GOAL_ID, goalId);
     * m_goalsGrid.set(FIELD_GOAL_OID, goal.getOid());
     * m_goalsGrid.set(FIELD_GOAL_FOCUS, goal.getFocus());
     * m_goalsGrid.set(FIELD_GOAL_DESCRIPTION, goalDescription);
     * m_goalsGrid.set(FIELD_DATA_GRID_GOAL_OBJECTIVES, m_objectivesGrid);
     * }
     * m_goalsGrid.beforeTop();
     * addParameter(DATASOURCE_GOALS, m_goalsGrid);
     * }
     */

    /**
     * This method is called when the goals sub report is loaded.
     * This method sets the objectives input stream.
     */
    private void getSpeechObjectivesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_OBJECTIVES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportObjectives = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_OBJECTIVES, new ByteArrayInputStream(m_reportObjectives.getCompiledFormat()));
    }

    /**
     * This method loads the present levels sub report.
     */
    private void loadSpeechPresentLevels() {
        getSpeechPresentLevelsSubRptCriteria();
        // getSpeechPresentLevelsCriteria();
        getPresentLevelsCriteria();
    }

    /**
     * This method sets the input stream for the present levels sub report.
     *
     * @return void
     */
    private void getSpeechPresentLevelsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_PRESENT_LEVELS);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportPresentLevels = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_PRESENT_LEVELS,
                new ByteArrayInputStream(m_reportPresentLevels.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the present levels sub report.
     *
     * @throws Exception exception
     */
    /*
     * private void getSpeechPresentLevelsCriteria()
     * {
     * String developmentSrc = null;
     * String presentLevel = null;
     * String accommodationModification = null;
     * String educationalNeeds = null;
     * String specialFactors = null;
     *
     * developmentSrc = (String) m_currentIep.getFieldValueByAlias(ALIAS_DEVELOPMENT_SRC,
     * getDictionary());
     * presentLevel = (String) m_currentIep.getFieldValueByAlias(ALIAS_PRESENT_LEVEL,
     * getDictionary());
     * accommodationModification = (String)
     * m_currentIep.getFieldValueByAlias(ALIAS_ACCOMMODATION_MODIFICATION, getDictionary());
     * educationalNeeds = (String) m_currentIep.getFieldValueByAlias(ALIAS_EDUCATIONAL_NEEDS,
     * getDictionary());
     * specialFactors = (String) m_currentIep.getFieldValueByAlias(ALIAS_SPECIAL_FACTORS,
     * getDictionary());
     *
     * m_presentLevelsGrid = new ReportDataGrid();
     * m_presentLevelsGrid.append();
     * m_presentLevelsGrid.set(FIELD_DEVELOPMENT_SRC, developmentSrc);
     * m_presentLevelsGrid.set(FIELD_PRESENT_LEVEL, presentLevel);
     * m_presentLevelsGrid.set(FIELD_ACCOMMODATION_MODIFICATION, accommodationModification);
     * m_presentLevelsGrid.set(FIELD_EDUCATIONAL_NEEDS, educationalNeeds);
     * m_presentLevelsGrid.set(FIELD_SPECIAL_FACTORS, specialFactors);
     * m_presentLevelsGrid.beforeTop();
     *
     * addParameter(DATASOURCE_PRESENT_LEVELS, m_presentLevelsGrid);
     * }
     */

    /**
     * This method loads the rights sub report.
     */
    private void loadSpeechRights() throws Exception {
        getSpeechRightsRptCriteria();
        getSpeechRightsCriteria();
    }

    /**
     * This method sets the input stream for the rights sub report.
     *
     * @return void
     */
    private void getSpeechRightsRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_RIGHTS);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportRights = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_RIGHTS, new ByteArrayInputStream(m_reportRights.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the rights sub report.
     *
     * @return void
     * @throws Exception exception
     */
    private void getSpeechRightsCriteria() throws Exception {
        String option1Line1 = null;
        String option1Line2 = null;
        String option1Line3 = null;
        String option1Line4 = null;
        String option1Line5 = null;
        String option1Line6 = null;
        String option2Line1 = null;
        String option2Line2 = null;

        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd");
        SisStudent student = m_currentIep.getStudent();
        String stdFullName = new StringBuilder(student.getPerson().getFirstName()).append(" ")
                .append(student.getPerson().getLastName()).toString();
        PlainDate dob = student.getPerson().getDob();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(dob);
        calendar.add(Calendar.YEAR, 18);
        String dobPlus18Yrs = dateFormat.format(calendar.getTime());

        option1Line1 = new StringBuilder("On ")
                .append(dobPlus18Yrs)
                .append(", ")
                .append(stdFullName)
                .append("  will turn age 18 and become an adult student. " +
                        "The following rights will transfer to ")
                .append(stdFullName)
                .append(":").toString();

        option1Line2 = new StringBuilder(">> The school district must receive written permission from ")
                .append(stdFullName)
                .append(" before it conducts any assessments as part of an evaluation " +
                        "or reevaluation and before implementing an IEP for the first " +
                        "time.")
                .toString();

        option1Line3 = new StringBuilder(">> The school must send a written notice to ")
                .append(stdFullName)
                .append(" whenever it wishes to change or refuses to change the " +
                        "evaluation, eligibility, individualized education program (IEP), " +
                        "placement, or the provision of a free appropriate public  " +
                        "education (FAPE).")
                .toString();

        option1Line4 = new StringBuilder(">> You, the parent(s) may not have access to ")
                .append(stdFullName)
                .append("\'s educational records without the student's consent, unless the student continues to be financially "
                        +
                        "dependent on you.")
                .toString();

        option1Line5 = new StringBuilder(">> Any time ")
                .append(stdFullName)
                .append(" disagrees with the special education program, the student is the only " +
                        "one who can request mediation or a due process hearing to resolve any " +
                        "disputes arising in those areas.")
                .toString();

        option1Line6 = new StringBuilder("If ")
                .append(stdFullName)
                .append(" wishes, the student may write a letter to the school giving you, the " +
                        "parent(s), the right to continue to act on " + student.getPerson().getFirstName()
                        + "'s behalf in these " +
                        "matters.")
                .toString();

        String studentNotified = "";
        String parentNotified = "";
        String studentNotificationDate = DASHED_STRING;
        String parentNotificationDate = DASHED_STRING;
        String contactName = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_CONTACT_NAME, getDictionary());
        String optionSelected =
                (String) m_currentIep.getFieldValueByAlias(ALIAS_TRANSFER_OF_RIGHTS_OPTION, getDictionary());

        if (TRANSFER_OF_RIGHTS_OPTION2.equalsIgnoreCase(optionSelected)) {
            studentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED, getDictionary());
            parentNotified = (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED, getDictionary());
            try {
                String notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_STUDENT_NOTIFIED_DATE, getDictionary());
                Date tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    studentNotificationDate = dateFormat.format(tempDate);
                }

                notificationDate =
                        (String) m_currentIep.getFieldValueByAlias(ALIAS_PARENT_NOTIFIED_DATE, getDictionary());
                tempDate = null;
                if (notificationDate != null) {
                    tempDate = inputFormat.parse(notificationDate);
                    parentNotificationDate = dateFormat.format(tempDate);
                } else {
                    parentNotificationDate = DASHED_STRING;
                }
            } catch (ParseException e) {
                throw e;
            }
        } else {
            studentNotified = "";
            parentNotified = "";
            contactName = DASHED_STRING;
            studentNotificationDate = DASHED_STRING;
            parentNotificationDate = DASHED_STRING;
        }

        option2Line1 = new StringBuilder().append(stdFullName).append(" was informed in writing on ")
                .append(studentNotificationDate)
                .append(" of the rights that will transfer to the student at age eighteen.").toString();

        option2Line2 = new StringBuilder().append(contactName).append(" was/were informed in writing on ")
                .append(parentNotificationDate).append(" of the rights that will transfer at age eighteen.").toString();

        m_rightsGrid = new ReportDataGrid();
        m_rightsGrid.append();
        m_rightsGrid.set(FIELD_OPTION1_LINE1, option1Line1);
        m_rightsGrid.set(FIELD_OPTION1_LINE2, option1Line2);
        m_rightsGrid.set(FIELD_OPTION1_LINE3, option1Line3);
        m_rightsGrid.set(FIELD_OPTION1_LINE4, option1Line4);
        m_rightsGrid.set(FIELD_OPTION1_LINE5, option1Line5);
        m_rightsGrid.set(FIELD_OPTION1_LINE6, option1Line6);
        m_rightsGrid.set(FIELD_OPTION2_LINE1, option2Line1);
        m_rightsGrid.set(FIELD_OPTION2_LINE2, option2Line2);
        m_rightsGrid.set(FIELD_STUDENT_NOTIFIED, studentNotified);
        m_rightsGrid.set(FIELD_PARENT_NOTIFIED, parentNotified);
        m_rightsGrid.set(FIELD_OPTION_SELECTED, optionSelected);

        m_rightsGrid.beforeTop();

        addParameter(DATASOURCE_RIGHTS, m_rightsGrid);
    }

    /**
     * This method loads the sped and related services sub report.
     */
    private void loadSpeechSpedRelServices() {
        getSpeechSvcsSubRptCriteria();
        getSpeechSvcsCriteria();
    }

    /**
     * This method sets the input stream for the services sub report.
     *
     * @return void
     */
    private void getSpeechSvcsSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_SPEECH_SERVICES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_reportServices = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_SUBREPORT_SERVICES, new ByteArrayInputStream(m_reportServices.getCompiledFormat()));
    }

    /**
     * This method sets the criteria for the services.
     *
     * @return void
     */
    private void getSpeechSvcsCriteria() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yy");
        m_servicesGrid = new ReportDataGrid();

        // Related Services service mode
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, m_currentIep.getOid());
        criteria.addEqualTo(IepService.COL_SERVICE_MODE, PARAM_SERVICE_NAME_SPEECH);
        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_TYPE);

        Collection<IepService> speechServices = getBroker().getCollectionByQuery(servicesQuery);

        if (speechServices.isEmpty()) {
            speechServices.add(X2BaseBean.newInstance(IepService.class, getBroker().getPersistenceKey()));
        }

        for (IepService service : speechServices) {
            m_servicesGrid.append();
            String serviceType = service.getServiceType();

            m_servicesGrid.set(FIELD_SERVICE_TYPE, serviceType);
            m_servicesGrid.set(FIELD_SERVICE_OID, service.getOid());
            if (service.getStartDate() != null && service.getEndDate() != null) {
                m_servicesGrid.set(FIELD_SERVICE_DATE,
                        dateFormat.format(service.getStartDate()) + " - " + dateFormat.format(service.getEndDate()));
                if (service.getCycle() != null) {
                    m_servicesGrid.set(FIELD_SERVICE_FREQUENCY,
                            service.getFrequency().toString() + "/" + service.getCycle());
                } else {
                    m_servicesGrid.set(FIELD_SERVICE_FREQUENCY, service.getFrequency().toString());
                }
            } else {
                m_servicesGrid.set(FIELD_SERVICE_DATE, EMPTY_STRING);
                m_servicesGrid.set(FIELD_SERVICE_FREQUENCY, EMPTY_STRING);
            }

            m_servicesGrid.set(FIELD_SERVICE_LOCATION,
                    service.getFieldValueByAlias(ALIAS_LOCATION_CODE, getDictionary()));

            if (service.getStudent() != null) {
                m_servicesGrid.set(FIELD_SERVICE_DURATION, Integer.toString(service.getDuration())); // returns
                                                                                                     // int
            }
        }

        m_servicesGrid.beforeTop();

        addParameter(DATASOURCE_SERVICES, m_servicesGrid);
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }

    /**
     * This method is provided as a way for subclasses to save session state information. The
     * default implementation does nothing.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        m_currentIep = userData.getCurrentRecord(IepData.class);
    }
}
