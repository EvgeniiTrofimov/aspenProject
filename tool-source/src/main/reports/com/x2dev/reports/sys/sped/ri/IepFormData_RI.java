/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2017 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.ri.RiMultipleGridReportJavaSource;
import com.x2dev.procedures.sys.sped.ri.RiSpedHelper;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.collections.Predicate;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java Source for the Rhode Island IEP report.
 *
 * @author X2 Development Corporation
 */
public class IepFormData_RI extends RiMultipleGridReportJavaSource {

    /*
     * Aliases
     */
    private static final String ALIAS_MEETING_PURPOSE = "meetingPurpose";

    /*
     * -------- Input parameter constants -------
     */
    private static final String PARAM_REPORT_TYPE = "reportType";
    private static final String REPORT_TYPE_ELEMENTARY_SCHOOL = "6t13";
    private static final String REPORT_TYPE_SECONDARY_TRANSITION = "o13";

    /*
     * -------- Constants for the main report -------
     */
    private static final String COL_IEP = "iep";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";

    private static final String EMPTY = "";
    private static final int IEP_INITIAL_CAPACITY = 50;
    private static final String LANGUAGE_REF_TABLE_OID = "rtbLanguage";

    private static final String ALIAS_CONTACT_INTERGRETER = "contact-interpreter";
    private static final String ALIAS_CONTACT_LANGUAGE = "contact-language";
    private static final String ALIAS_IEP_SERVICE_TRANSITION_AREA = "transition-area";
    private static final String ALIAS_IEP_STUDENT_GRAD_DATE = "img-student-grad-date";
    private static final String ALIAS_ITM_IEP_TEAM = "itm-iep-tm";
    private static final String ALIAS_IMA_ADDITIONAL_PRESENT = "all-ima-additionalPresent";
    private static final String ALIAS_MTG_IEP_DEVELOPMENT = "meeting-iep-development";
    private static final String ALIAS_PARENTAL_CONSENT = "iep-parental-consent";

    private static final String PARAM_MEETING_DATES = "meetingDates";

    /*
     * --------------- Format IDs: 3-13 ---------------
     */
    private static final String OVERFLOW_FORMAT_ID = "SYS-SPED-RI-OVERFLOW";

    // First page of IEP
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-RI-IEP1";

    // Meeting page of IEP
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-RI-IEP2";

    // Present Levels of Functional Performance & Academic Achievement
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-RI-IEP3";

    // Areas to be Addressed & Academic Standards Program Will Address
    private static final String PAGE_4_FORMAT_ID = "SYS-SPED-RI-IEP4";

    // Special case for Preschoolers
    private static final String PAGE_4P_FORMAT_ID = "SYS-SPED-RI-IEP4P";

    // Goals (Same as Secondary Transition)
    private static final String PAGE_5_FORMAT_ID = "SYS-SPED-RI-IEP5";
    private static final String PAGE_5_OBJ_FORMAT_ID = "SYS-SPED-RI-IEP5-OBJ";

    // Considerations (Same as Secondary Transition)
    private static final String PAGE_6_FORMAT_ID = "SYS-SPED-RI-IEP6";

    // Services (Same as Secondary Transition)
    private static final String PAGE_7_FORMAT_ID = "SYS-SPED-RI-IEP7";

    // Educational Environments, Explanation of Nonparticipation, & Placement
    private static final String PAGE_8_FORMAT_ID = "SYS-SPED-RI-IEP8";

    // Assessment Accommodations (Same as Secondary Transition)
    private static final String PAGE_9_FORMAT_ID = "SYS-SPED-RI-IEP9";

    // Information for parents
    private static final String PAGE_13_FORMAT_ID = "SYS-SPED-RI-IEP13";

    /*
     * ------- Format IDs: Secondary Transition -------
     */

    // First page of IEP
    private static final String PAGE_1T_FORMAT_ID = "SYS-SPED-RI-IEP1T";

    // Meeting page of IEP
    private static final String PAGE_2T_FORMAT_ID = "SYS-SPED-RI-IEP2T";

    // Transition Assessments & Measurable Post-school Goals
    private static final String PAGE_3T_FORMAT_ID = "SYS-SPED-RI-IEP3T";

    // Present Levels of Functional Performance & Academic Achievement
    private static final String PAGE_4T_FORMAT_ID = "SYS-SPED-RI-IEP4T";

    // Areas to be Addressed & Transition Services
    private static final String PAGE_5T_FORMAT_ID = "SYS-SPED-RI-IEP5T";

    // Program of Study, Assurance of Transition Services, & Academic Standards
    private static final String PAGE_6T_FORMAT_ID = "SYS-SPED-RI-IEP6T";

    // Goals (Same as 3-13)
    private static final String PAGE_7T_FORMAT_ID = "SYS-SPED-RI-IEP5";

    // Considerations (Same as 3-13)
    private static final String PAGE_8T_FORMAT_ID = "SYS-SPED-RI-IEP6";

    // Services (Same as 3-13)
    private static final String PAGE_9T_FORMAT_ID = "SYS-SPED-RI-IEP7";

    // Educational Environments, Explanation of Non-participation, & Placement
    private static final String PAGE_10T_FORMAT_ID = "SYS-SPED-RI-IEP10T";

    // Assessment Accommodations (Same as Secondary Transition)
    private static final String PAGE_11T_FORMAT_ID = "SYS-SPED-RI-IEP9";

    // Early Childhood Data Collection
    private static final String PAGE_12_FORMAT_ID = "SYS-SPED-RI-IEP12";

    // Early Childhood Transition Data Collection
    private static final String PAGE_12P_FORMAT_ID = "SYS-SPED-RI-IEP12P";

    // Information for parents
    private static final String PAGE_13T_FORMAT_ID = "SYS-SPED-RI-IEP13T";

    // Collection of all report ID constants
    private static final Collection<String> FORMAT_IDS = Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
            PAGE_2_FORMAT_ID,
            PAGE_3_FORMAT_ID,
            PAGE_4_FORMAT_ID,
            PAGE_4P_FORMAT_ID,
            PAGE_5_FORMAT_ID,
            PAGE_5_OBJ_FORMAT_ID,
            PAGE_6_FORMAT_ID,
            PAGE_7_FORMAT_ID,
            PAGE_8_FORMAT_ID,
            PAGE_9_FORMAT_ID,
            PAGE_1T_FORMAT_ID,
            PAGE_2T_FORMAT_ID,
            PAGE_3T_FORMAT_ID,
            PAGE_4T_FORMAT_ID,
            PAGE_5T_FORMAT_ID,
            PAGE_6T_FORMAT_ID,
            PAGE_7T_FORMAT_ID,
            PAGE_8T_FORMAT_ID,
            PAGE_9T_FORMAT_ID,
            PAGE_10T_FORMAT_ID,
            PAGE_11T_FORMAT_ID,
            PAGE_12_FORMAT_ID,
            PAGE_12P_FORMAT_ID,
            PAGE_13_FORMAT_ID,
            PAGE_13T_FORMAT_ID,
            OVERFLOW_FORMAT_ID});
    /*
     * --------- Parameters for subreports -------------
     */
    public static final String PARAM_CONTACT_0 = "contact0";
    public static final String PARAM_CONTACT_1 = "contact1";
    public static final String PARAM_CONTACT_0_INTERPRETER = "contact0Interpreter";
    public static final String PARAM_CONTACT_1_INTERPRETER = "contact1Interpreter";
    public static final String PARAM_CONTACT_0_LANGUAGE = "contact0Language";
    public static final String PARAM_CONTACT_1_LANGUAGE = "contact1Language";
    public static final String PARAM_FORMATED_NUMBER = "formatedNumber";
    public static final String PARAM_IEP = COL_IEP;
    public static final String PARAM_INTERPRETER = "interpreter";
    public static final String PARAM_GRAD_DATE = "gradDate";
    public static final String PARAM_PARENT_OBTAINED_DATE = "parentObtainedDate";
    public static final String PARAM_SCHOOL = "school";
    public static final String PARAM_SERVICES_MODE_A = "SERVICES_MODE_A";
    public static final String PARAM_SERVICES_MODE_B = "SERVICES_MODE_B";
    public static final String PARAM_SERVICES_MODE_C1 = "SERVICES_MODE_C1";
    public static final String PARAM_SERVICES_MODE_C2 = "SERVICES_MODE_C2";
    public static final String PARAM_SERVICES_MODE_C3 = "SERVICES_MODE_C3";
    public static final String PARAM_STUDENT_AGE = "studentAge";
    public static final String PARAM_STUDENT_GRADE_LEVEL = "studentGradeLevel";
    public static final String PARAM_STUDENT_LANGUAGE = "studentLanguage";
    public static final String PARAM_SURROGATE = "surrogate";
    public static final String PARAM_TODAY = "today";

    /*
     * -------- Constants for the contact info subreport -------
     */
    public static final String RELATIONSHIP_FAMILY_TEACHER_CODE = "Family Teacher";
    public static final String RELATIONSHIP_FATHER_CODE = "Father";
    public static final String RELATIONSHIP_FOSTER_PARENTS_CODE = "Foster Parents";
    public static final String RELATIONSHIP_GUARDIAN_CODE = "Guardian";
    public static final String RELATIONSHIP_LEGAL_GUARDIAN_CODE = "Legal Guardian";
    public static final String RELATIONSHIP_MOTHER_CODE = "Mother";
    public static final String RELATIONSHIP_PARENTS_CODE = "Parents";
    public static final String RELATIONSHIP_SURROGATE_CODE = "Surrogate";

    private static final String RI_STATE_LANG_CODE_ENG = "1290";

    /*
     * -------- Constants for the goals subreport -------
     */
    private static final String COL_GOAL = "goal";
    private static final int MAX_GOALS_PER_PAGE = 2;

    /*
     * -------- Constants for the services sub-report -------
     */

    // These must correspond with the Rhode Island "Service Modes" ref table
    public static final String SERVICES_MODE_A = "Special Education";
    public static final String SERVICES_MODE_B = "Related Services";
    public static final String SERVICES_MODE_C1 = "Supplementary Aids";
    public static final String SERVICES_MODE_C2 = "Program Modifications";
    public static final String SERVICES_MODE_C3 = "Personnel Supports";


    private static final String COL_PRINT_HEADER = "printHeader";
    private static final String COL_SERVICE = "service";

    private static final int MAX_SERVICES_PER_PAGE = 6;

    /*
     * -------- Constants for the transition services sub-report -------
     */

    // This must correspond with the Rhode Island "Service Mode Types" ref table
    public static final String SERVICES_MODE_TRANSITION = "Transition Services";

    public static final String TRANSITION_SERVICES_AREA_EDU = "Education and Training";
    public static final String TRANSITION_SERVICES_AREA_EMP = "Employment";
    public static final String TRANSITION_SERVICES_AREA_IND = "Independent Living";

    private static final int MIN_SERVICES_PER_AREA = 3;

    /*
     * -------- Constants for the placement subreport -------
     *
     * These must correspond with the reference tables for Educational Environments
     * and Special Ed. Placement. They are used to populate the checkboxes in the
     * page 8 and 10T formats.
     */

    // Educational Environments
    public static final String ED_ENVIRONMENT_CODE_LOWER = "Regular class 39% or less";
    public static final String ED_ENVIRONMENT_CODE_MIDDLE = "Regular class 79% - 40%";
    public static final String ED_ENVIRONMENT_CODE_UPPER = "Regular class 80% or more";

    // Placement 3-5
    public static final String PLACEMENT_EC_0 = "Temporary placement (30 days or less)";
    public static final String PLACEMENT_EC_1 = "General EC w/ on site consultation";
    public static final String PLACEMENT_EC_2 = "Integrated preschool";
    public static final String PLACEMENT_EC_3 = "Home-based special ed. and related services";
    public static final String PLACEMENT_EC_4 = "Home w/ supplementary EC special ed. setting";
    public static final String PLACEMENT_EC_5 = "Full time EC special ed.";
    public static final String PLACEMENT_EC_6 = "Special ed. day school";
    public static final String PLACEMENT_EC_7 = "Residential special ed. school";

    // Placement 6+
    public static final String PLACEMENT_0 = "General ed. class w/ special ed. consultation";
    public static final String PLACEMENT_1 = "Special class in a school district building";
    public static final String PLACEMENT_2 = "Home or hospitalized instruction";
    public static final String PLACEMENT_3 = "Special education day school program";
    public static final String PLACEMENT_4 = "Special education residential school";

    // -------- Constant for the Accommodations subreport --------
    private static final String COL_ACCOMMODATION = "accommodation";
    private static final int MIN_ACCOMMODATIONS = 6;

    // -------- Constants for the Transition Assessments subreport --------
    private static final String ALIAS_TRANS_ASSESS_DATE = "transAssess-date";
    private static final String ALIAS_TRANS_ASSESS_TOOL = "transAssess-tool";
    private static final String COL_ASSESSMENT = "assessment";
    private static final int MIN_ASSESSMENTS = 5;

    // -------- Member Fields ------
    private IepData m_currentIep = null;
    private Collection<IepPerformanceLevel> m_assessmentData = null;
    private SimpleDateFormat m_formatterFrom = new SimpleDateFormat("yyyy-MM-dd");
    private SimpleDateFormat m_formatterTo = new SimpleDateFormat("M/d/yyyy");
    private Collection<IepGoal> m_goalData = null;
    private Map<String, Collection<IepService>> m_servicesData = null;
    private Map m_subReports = null;
    private RiSpedHelper m_rihlp = null;
    private String m_meeting_purpose;


    /*
     * --------------------------------------------------------------------------------------------
     * General Report Methods
     * --------------------------------------------------------------------------------------------
     */

    /**
     *
     * @see com.x2dev.procedures.sys.sped.ri.RiMultipleGridReportJavaSource#gatherDataList()
     */
    @Override
    protected List<ReportData> gatherDataList() throws Exception {
        List<ReportData> grids = new ArrayList<ReportData>();
        IepData iep = getIep();
        m_rihlp = new RiSpedHelper(getBroker(), getOrganization());
        m_rihlp.setDataDictionary(getDictionary());
        m_meeting_purpose = DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_MEETING_PURPOSE).getJavaName();
        SisStudent student = iep.getStudent();
        setContacts(iep, student);

        loadAssessments();
        loadGoals();
        loadServices();
        loadSubReports();

        SisSchool currentSchool = student.getSchool();
        String currentGrade = student.getGradeLevel();
        if (iep.getStatusCodeEnum() != StatusCode.ACTIVE &&
                iep.getStatusCodeEnum() != StatusCode.DRAFT &&
                iep.getStatusCodeEnum() != StatusCode.AMENDMENT_DRAFT) {
            List<String> enrollmentTypes =
                    new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE));
            PlainDate meetingDate = getMeetingDate(iep);
            if (meetingDate == null) {
                meetingDate = (getFormInstance() != null && getFormInstance().getCreatedTime() > 0
                        ? new PlainDate(getFormInstance().getCreatedTime())
                        : new PlainDate());
            }
            StudentEnrollment currentEnr = m_rihlp.getNearLastStudentEnrollment(student, meetingDate, enrollmentTypes);
            currentSchool = currentEnr.getSchool();
            currentGrade = m_rihlp.getGradeLevel(meetingDate, currentEnr);
        }
        String homeLangCode = EMPTY;
        String unFormatedNumber = EMPTY;
        if (iep != null && iep.getStudent() != null) {
            homeLangCode = iep.getStudent().getHomeLanguageCode();
        }
        if (currentSchool != null && currentSchool.getAddress() != null) {
            unFormatedNumber = currentSchool.getAddress().getPhone01();
        }

        addParameter(PARAM_IEP, iep);
        addParameter(PARAM_FORMATED_NUMBER, getFormatedPhoneNumber(unFormatedNumber));

        List<IepMeeting> meetings = new ArrayList<IepMeeting>(iep.getIepMeeting());
        List<IepMeeting> sortedMeetings = new ArrayList<IepMeeting>(iep.getIepMeeting().size());

        for (int i = meetings.size() - 1; i >= 0; i--) {
            sortedMeetings.add(meetings.get(i));
        }

        for (IepMeeting iepMeeting : sortedMeetings) {
            String gradeDate = (String) iepMeeting.getFieldValueByAlias(ALIAS_IEP_STUDENT_GRAD_DATE,
                    DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                            getBroker().getPersistenceKey()));
            if (gradeDate != null) {
                gradeDate = formatDate(gradeDate);
                addParameter(PARAM_GRAD_DATE, gradeDate);
                break;
            }
        }

        addParameter(PARAM_MEETING_DATES, getMeetingDates(iep));
        addParameter(PARAM_REPORT_TYPE, getParameter(PARAM_REPORT_TYPE).toString());
        addParameter(PARAM_SERVICES_MODE_A, SERVICES_MODE_A);
        addParameter(PARAM_SERVICES_MODE_B, SERVICES_MODE_B);
        addParameter(PARAM_SERVICES_MODE_C1, SERVICES_MODE_C1);
        addParameter(PARAM_SERVICES_MODE_C2, SERVICES_MODE_C2);
        addParameter(PARAM_SERVICES_MODE_C3, SERVICES_MODE_C3);
        addParameter(PARAM_STUDENT_AGE, getAgeAsOfDate(iep));
        addParameter(PARAM_STUDENT_GRADE_LEVEL, currentGrade);
        addParameter(PARAM_SCHOOL, currentSchool);
        addParameter(PARAM_STUDENT_LANGUAGE, getLanguageDescription(homeLangCode));
        addParameter(PARAM_PARENT_OBTAINED_DATE, getParentObtainedDate(iep));
        addParameter(PARAM_TODAY, DateUtils.convertPlainDateToShortFormat(new PlainDate(), getLocale()));

        if (isSecondaryTransition()) {
            setStudentInterpreter(iep.getStudent());
            preparePage(grids, iep, PAGE_1T_FORMAT_ID);
            preparePage(grids, iep, PAGE_2T_FORMAT_ID);
            preparePage(grids, iep, PAGE_3T_FORMAT_ID);
            preparePage(grids, iep, PAGE_4T_FORMAT_ID);
            preparePage(grids, iep, PAGE_5T_FORMAT_ID);
            preparePage(grids, iep, PAGE_6T_FORMAT_ID);
            preparePage(grids, iep, PAGE_7T_FORMAT_ID);
            preparePage(grids, iep, PAGE_8T_FORMAT_ID);
            preparePage(grids, iep, PAGE_9T_FORMAT_ID);
            preparePage(grids, iep, PAGE_10T_FORMAT_ID);
            preparePage(grids, iep, PAGE_11T_FORMAT_ID);
            preparePage(grids, iep, PAGE_13T_FORMAT_ID);
        } else if (isElementarySchool()) {
            preparePage(grids, iep, PAGE_1_FORMAT_ID);
            preparePage(grids, iep, PAGE_2_FORMAT_ID);
            preparePage(grids, iep, PAGE_3_FORMAT_ID);
            preparePage(grids, iep, PAGE_4_FORMAT_ID);
            preparePage(grids, iep, PAGE_5_FORMAT_ID);
            preparePage(grids, iep, PAGE_6_FORMAT_ID);
            preparePage(grids, iep, PAGE_7_FORMAT_ID);
            preparePage(grids, iep, PAGE_8_FORMAT_ID);
            preparePage(grids, iep, PAGE_9_FORMAT_ID);
            preparePage(grids, iep, PAGE_13_FORMAT_ID);
        } else {

            preparePage(grids, iep, PAGE_1_FORMAT_ID);
            preparePage(grids, iep, PAGE_2_FORMAT_ID);
            preparePage(grids, iep, PAGE_3_FORMAT_ID);
            preparePage(grids, iep, PAGE_4P_FORMAT_ID);
            preparePage(grids, iep, PAGE_5_FORMAT_ID);
            preparePage(grids, iep, PAGE_6_FORMAT_ID);
            preparePage(grids, iep, PAGE_7_FORMAT_ID);
            preparePage(grids, iep, PAGE_8_FORMAT_ID);
            preparePage(grids, iep, PAGE_9_FORMAT_ID);
            preparePage(grids, iep, PAGE_12_FORMAT_ID);
            preparePage(grids, iep, PAGE_12P_FORMAT_ID);
            preparePage(grids, iep, PAGE_13_FORMAT_ID);
        }

        return grids;
    }



    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
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
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Returns the age of the student as of the iep meeting date.
     *
     * @param iep IepData
     * @return String
     */
    private String getAgeAsOfDate(IepData iep) {
        int ageAsOfdate = 0;
        if (iep != null &&
                iep.getStudent() != null &&
                iep.getStudent().getPerson() != null &&
                iep.getStudent().getPerson().getDob() != null) {
            if ((iep.getIepAmendment() != null) && (iep.getIepAmendment().getDate() != null)) {
                ageAsOfdate = iep.getStudent().getPerson().getAgeAsOfDate(iep.getIepAmendment().getDate());
            } else {
                if (iep.getMeetingDate() != null) {
                    ageAsOfdate = iep.getStudent().getPerson().getAgeAsOfDate(iep.getMeetingDate());
                } else {
                    ageAsOfdate = iep.getStudent().getPerson().getAgeAsOfDate(new PlainDate());
                }
            }
        }

        return ageAsOfdate + EMPTY;
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        IepData iep = null;

        if (isBlank()) {
            IepData ownerIep = (IepData) getFormOwner();

            iep = new IepData(getBroker().getPersistenceKey());
            iep.setStudentOid(ownerIep.getStudentOid());
            iep.setStaffOid(ownerIep.getStaffOid());
        } else {
            iep = (IepData) getFormStorage();
        }

        return iep;
    }

    /**
     * Returns a SimpleBeanDataSource for the passed IEP that supports an overflow page.
     *
     * @param iep IepData
     * @param grid ReportDataGrid
     * @return Simple form data source
     */
    private SimpleFormDataSource getDataSource(IepData iep, ReportDataGrid grid) {
        Map overflowFields = new HashMap<String, Object>();
        overflowFields.put(COL_IEP, iep);

        return new SimpleFormDataSource(iep,
                iep,
                grid,
                getSubreportFormat(OVERFLOW_FORMAT_ID),
                overflowFields,
                getDictionary(),
                getLocale());
    }


    /**
     * Returns a formated phone number based on US standards.
     *
     * @param unFormatedNumber String
     * @return String
     */
    private String getFormatedPhoneNumber(String unFormatedNumber) {
        String unorganizedformatedNumber = EMPTY;
        int unFormatedNumberLength = unFormatedNumber.length();

        for (int i = 0; i <= unFormatedNumberLength - 1; i++) {
            String charAtIndex = String.valueOf(unFormatedNumber.charAt(i));
            if (StringUtils.isNumeric(charAtIndex)) {
                unorganizedformatedNumber += charAtIndex;
            }
        }

        String organizedFormatedNumber = EMPTY;
        if (unorganizedformatedNumber.length() == 11) {
            for (int i = 0; i <= unorganizedformatedNumber.length() - 1; i++) {
                String charAtIndex = String.valueOf(unorganizedformatedNumber.charAt(i));

                if (i == 1 || i == 4 || i == 7) {
                    organizedFormatedNumber += "-";
                }
                organizedFormatedNumber += charAtIndex;
            }
        } else if (unorganizedformatedNumber.length() == 10) {
            for (int i = 0; i <= unorganizedformatedNumber.length() - 1; i++) {
                String charAtIndex = String.valueOf(unorganizedformatedNumber.charAt(i));

                if (i == 0) {
                    organizedFormatedNumber += "(";
                } else if (i == 3) {
                    organizedFormatedNumber += ") ";
                } else if (i == 6) {
                    organizedFormatedNumber += "-";
                }

                organizedFormatedNumber += charAtIndex;
            }
        }

        return organizedFormatedNumber;
    }

    /**
     * Gets the iterator for relation ship code.
     *
     * @param stdOid String
     * @param relationShipCode String
     * @return Query iterator
     */
    private QueryIterator getIteratorForRelationShipCode(String stdOid, String relationShipCode) {
        X2Criteria relationShipCriteria = new X2Criteria();
        X2Criteria relationShipCodeCriteria = new X2Criteria();
        X2Criteria relationShipCodeOrCriteria0 = new X2Criteria();
        // create criteria
        relationShipCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, stdOid);


        relationShipCodeCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, relationShipCode);
        relationShipCodeOrCriteria0.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, relationShipCode.toUpperCase());

        relationShipCodeCriteria.addOrCriteria(relationShipCodeOrCriteria0);
        relationShipCriteria.addAndCriteria(relationShipCodeCriteria);

        QueryByCriteria familyStaffQuery = new QueryByCriteria(StudentContact.class, relationShipCriteria);
        familyStaffQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        return getBroker().getIteratorByQuery(familyStaffQuery);
    }

    /**
     * Returns the home language description based off of the language code.
     *
     * @param langCode String
     * @return String
     */
    private String getLanguageDescription(String langCode) {
        ReferenceDescriptionLookup referenceLookup;
        referenceLookup = new ReferenceDescriptionLookup(getBroker(), getOrganization());

        String languageDescription = null;
        if (!StringUtils.isEmpty(langCode)) {
            languageDescription = referenceLookup.getDescription(LANGUAGE_REF_TABLE_OID, langCode);
        }

        return languageDescription;
    }

    /**
     * Returns meeting date. If meeting date is null then method returns last meeting date from
     * IepMeeting
     *
     * @param iepData IepData
     * @return String
     */
    private PlainDate getMeetingDate(IepData iepData) {
        PlainDate meetingDate = iepData.getMeetingDate();
        if (meetingDate == null) {
            for (IepMeeting iepMeeting : iepData.getIepMeeting()) {
                PlainDate date = iepMeeting.getDate();
                if (meetingDate == null || date.after(meetingDate)) {
                    meetingDate = date;
                }
            }
        }
        return meetingDate;
    }

    /**
     * Returns a String representation of meeting dates for the passed iep.
     *
     * @param iep IepData
     * @return String
     */
    private String getMeetingDates(IepData iep) {
        String dates = EMPTY;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, iep.getOid());

        DataDictionaryField meetingPurposeRefField =
                DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                        getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_MEETING_PURPOSE);
        ReferenceTable meetingPurposeRefTable = meetingPurposeRefField.getReferenceTable();

        Criteria criteriaOr = new Criteria();
        for (ReferenceCode code : meetingPurposeRefTable.getReferenceCodes()) {
            if (code.getDisabledIndicator() == false) {
                Criteria purposeCriteria = new Criteria();
                purposeCriteria.addLike(m_meeting_purpose, "%" + code.getCode() + "%");
                criteriaOr.addOrCriteria(purposeCriteria);
            }
        }

        criteria.addAndCriteria(criteriaOr);
        SubQuery query = new SubQuery(IepMeeting.class, IepMeeting.COL_DATE, criteria);
        query.addOrderByAscending(IepMeeting.COL_DATE);
        DateFormat format = new SimpleDateFormat("MM/dd/yyyy");
        PlainDate today = new PlainDate();
        for (PlainDate date : (Collection<PlainDate>) getBroker().getSubQueryCollectionByQuery(query)) {
            if (date != null && !today.before(date)) {
                dates += format.format(date) + ", ";
            }
        }

        return dates.substring(0, Math.max(dates.length() - 2, 0));
    }

    /**
     * return date when parent give agree.
     * Information get from "Receive Permission to Evaluate" phase -> "Parent Agrees" outcome.
     * if workflow hasn't this outcome or it is not choosed, or not filled - info get data from iep
     * by "ritr-consent-obtained-date" alias
     *
     * @param iep IepData
     * @return String
     */
    private String getParentObtainedDate(IepData iep) {
        String returnDate = null;
        if (iep != null && !StringUtils.isEmpty(iep.getOid())) {

            String obtainDate = null;
            obtainDate = (String) iep.getFieldValueByAlias(ALIAS_PARENTAL_CONSENT, getDictionary());

            returnDate = formatDate(obtainDate);
        }
        return returnDate;
    }


    /**
     * Returns most recent meeting of passed IEP.
     *
     * @param iep IepData
     * @return Iep meeting
     */
    private IepMeeting getRecentMeeting(IepData iep) {
        List<IepMeeting> meetings = new ArrayList(iep.getIepMeeting());

        if (meetings != null && !meetings.isEmpty()) {
            Collections.sort(meetings, new Comparator<IepMeeting>() {
                @Override
                public int compare(IepMeeting meet0, IepMeeting meet1) {
                    int result = 0;

                    PlainDate meet0Date = meet0.getDate();
                    PlainDate meet1Date = meet1.getDate();

                    if (meet0Date == null && meet1Date == null) {
                        result = 0;
                    } else if (meet0Date == null) {
                        result = -1;
                    } else if (meet1Date == null) {
                        result = 1;
                    } else {
                        result = meet0Date.compareTo(meet1Date);
                    }

                    return result;
                }
            });
        }

        return meetings.isEmpty() ? null : meetings.get(meetings.size() - 1);
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);
        return report.getCompiledFormat();
    }


    /**
     * Returns true if the report selected to print is the Elementary Transition report.
     *
     * @return boolean
     */
    private boolean isElementarySchool() {
        return getParameter(PARAM_REPORT_TYPE).toString()
                .equals(REPORT_TYPE_ELEMENTARY_SCHOOL);
    }

    /**
     * Returns true if the report selected to print is the Secondary Transition report.
     *
     * @return boolean
     */
    private boolean isSecondaryTransition() {
        return getParameter(PARAM_REPORT_TYPE).toString()
                .equals(REPORT_TYPE_SECONDARY_TRANSITION);
    }


    /**
     * Assigns the three necessary contacts (2 guardians and 1 education surrogate) to their
     * parameters.
     *
     * @param student void
     */
    private void setContacts(Student student) {
        Criteria guardianCriteria = new Criteria();
        Criteria guardianOrCriteria0 = new Criteria();
        Criteria guardianOrCriteria1 = new Criteria();
        Criteria guardianOrCriteria2 = new Criteria();
        Criteria guardianOrCriteria3 = new Criteria();
        Criteria guardianOrCriteria4 = new Criteria();
        Criteria guardianOrCriteria5 = new Criteria();
        Criteria guardianOrCriteria6 = new Criteria();
        Criteria guardianOrCriteria7 = new Criteria();
        Criteria guardianOrCriteria8 = new Criteria();

        guardianCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_FATHER_CODE);
        guardianOrCriteria0.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_FOSTER_PARENTS_CODE);
        guardianOrCriteria1.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_GUARDIAN_CODE);
        guardianOrCriteria2.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_MOTHER_CODE);
        guardianOrCriteria3.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_PARENTS_CODE);
        guardianOrCriteria4.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_FATHER_CODE.toUpperCase());
        guardianOrCriteria5.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE,
                RELATIONSHIP_FOSTER_PARENTS_CODE.toUpperCase());
        guardianOrCriteria6.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_GUARDIAN_CODE.toUpperCase());
        guardianOrCriteria7.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_MOTHER_CODE.toUpperCase());
        guardianOrCriteria8.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_PARENTS_CODE.toUpperCase());

        guardianCriteria.addOrCriteria(guardianOrCriteria0);
        guardianCriteria.addOrCriteria(guardianOrCriteria1);
        guardianCriteria.addOrCriteria(guardianOrCriteria2);
        guardianCriteria.addOrCriteria(guardianOrCriteria3);
        guardianCriteria.addOrCriteria(guardianOrCriteria4);
        guardianCriteria.addOrCriteria(guardianOrCriteria5);
        guardianCriteria.addOrCriteria(guardianOrCriteria6);
        guardianCriteria.addOrCriteria(guardianOrCriteria7);
        guardianCriteria.addOrCriteria(guardianOrCriteria8);

        guardianCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, student.getOid());

        QueryByCriteria guardianQuery = new QueryByCriteria(StudentContact.class, guardianCriteria);
        guardianQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        QueryIterator guardianIt = getBroker().getIteratorByQuery(guardianQuery);

        // S-31255 Please update the RI IEP Report to include student contacts when the Contact
        // relationship equals "Legal Guardian" and there is no Parent Relationship
        // key word and there is no Parent Relationship
        // try find "Legal Guardian" only in case if Parent Relationship doesn't exist
        if (!guardianIt.hasNext()) {
            guardianIt.close();
            guardianIt = getIteratorForRelationShipCode(student.getOid(), RELATIONSHIP_LEGAL_GUARDIAN_CODE);
        }


        // S-30961 Please update the RI IEP Report to include student contacts when the Contact
        // relationship equals "Family Teacher" and there is no Parent Relationship
        // key word and there is no Parent Relationship
        // try find "Family Teacher" only in case if Parent Relationship doesn't exist
        if (!guardianIt.hasNext()) {
            guardianIt.close();
            guardianIt = getIteratorForRelationShipCode(student.getOid(), RELATIONSHIP_FAMILY_TEACHER_CODE);
        }

        try {
            if (guardianIt.hasNext()) {
                StudentContact contact0 = (StudentContact) guardianIt.next();
                addParameter(PARAM_CONTACT_0, contact0);

                if (contact0 != null) {
                    addParameter(PARAM_CONTACT_0_LANGUAGE,
                            contact0.getFieldValueByAlias(ALIAS_CONTACT_INTERGRETER));
                    addParameter(PARAM_CONTACT_0_INTERPRETER,
                            contact0.getFieldValueByAlias(ALIAS_CONTACT_LANGUAGE));
                }

                if (guardianIt.hasNext()) {
                    StudentContact contact1 = (StudentContact) guardianIt.next();
                    addParameter(PARAM_CONTACT_1, contact1);

                    if (contact1 != null) {
                        addParameter(PARAM_CONTACT_1_LANGUAGE,
                                contact1.getFieldValueByAlias(ALIAS_CONTACT_INTERGRETER));
                        addParameter(PARAM_CONTACT_1_INTERPRETER,
                                contact1.getFieldValueByAlias(ALIAS_CONTACT_LANGUAGE));
                    }
                }
            }
        } finally {
            guardianIt.close();
        }

        Criteria surrogateCriteria = new Criteria();
        Criteria surrogateCodeCriteria = new Criteria();
        Criteria surrogateCodeOrCriteria0 = new Criteria();
        surrogateCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, student.getOid());
        surrogateCodeCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_SURROGATE_CODE);
        surrogateCodeOrCriteria0.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE,
                RELATIONSHIP_SURROGATE_CODE.toUpperCase());

        surrogateCodeCriteria.addOrCriteria(surrogateCodeOrCriteria0);

        surrogateCriteria.addAndCriteria(surrogateCodeCriteria);
        QueryByCriteria surrogateQuery = new QueryByCriteria(StudentContact.class, surrogateCriteria);
        surrogateQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);
        addParameter(PARAM_SURROGATE, getBroker().getBeanByQuery(surrogateQuery));
    }

    /**
     * Assigns the three necessary contacts (2 guardians and 1 education surrogate) to their
     * parameters.
     *
     * @param student void
     */
    private void setContacts(IepData iep, SisStudent student) {
        StudentContact contact1 = null;
        StudentContact contact2 = null;
        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 2);
        if (contacts.size() > 0) {
        	contact1 = contacts.get(0);
            if (contacts.size() > 1) {
            	contact2 = contacts.get(1);
            }
        }

        addParameter(PARAM_CONTACT_0, contact1);
        if (contact1 != null) {
            addParameter(PARAM_CONTACT_0_LANGUAGE,
                    contact1.getFieldValueByAlias(ALIAS_CONTACT_INTERGRETER));
            addParameter(PARAM_CONTACT_0_INTERPRETER,
                    contact1.getFieldValueByAlias(ALIAS_CONTACT_LANGUAGE));
        }

        addParameter(PARAM_CONTACT_1, contact2);
        if (contact2 != null) {
            addParameter(PARAM_CONTACT_1_LANGUAGE,
                    contact2.getFieldValueByAlias(ALIAS_CONTACT_INTERGRETER));
            addParameter(PARAM_CONTACT_1_INTERPRETER,
                    contact2.getFieldValueByAlias(ALIAS_CONTACT_LANGUAGE));
        }

        Criteria surrogateCriteria = new Criteria();
        Criteria surrogateCodeCriteria = new Criteria();
        Criteria surrogateCodeOrCriteria0 = new Criteria();
        surrogateCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, student.getOid());
        surrogateCodeCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, RELATIONSHIP_SURROGATE_CODE);
        surrogateCodeOrCriteria0.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE,
                RELATIONSHIP_SURROGATE_CODE.toUpperCase());

        surrogateCodeCriteria.addOrCriteria(surrogateCodeOrCriteria0);

        surrogateCriteria.addAndCriteria(surrogateCodeCriteria);
        QueryByCriteria surrogateQuery = new QueryByCriteria(StudentContact.class, surrogateCriteria);
        surrogateQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);
        addParameter(PARAM_SURROGATE, getBroker().getBeanByQuery(surrogateQuery));
    }

    /**
     * Checks the state code of the language and if it is equal to the RI English code (1290)
     * then set the interpreter parameter to "No", if not, set it to the language code.
     *
     * @param student void
     */
    private void setStudentInterpreter(Student student) {
        String homeLanguage = student.getHomeLanguageCode();
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_CODE, homeLanguage);

        SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_STATE_CODE, criteria);

        String interpreter = null;
        for (String stateCode : (Collection<String>) getBroker().getSubQueryCollectionByQuery(query)) {
            if (RI_STATE_LANG_CODE_ENG.equals(stateCode)) {
                interpreter = "No";
                break;
            }
        }

        if (RI_STATE_LANG_CODE_ENG.equals(homeLanguage)) {
            interpreter = "No";
        } else if (interpreter == null) {
            interpreter = getLanguageDescription(homeLanguage);
        }

        addParameter(PARAM_INTERPRETER, interpreter);
    }

    /*
     * --------------------------------------------------------------------------------------------
     * Load Methods
     * --------------------------------------------------------------------------------------------
     */

    /**
     * Loads assessment data into map for fast retrieval. The map loaded is keyed on IEP OID and
     * each
     * value contains a collection of sorted IepPerformanceLevel objects.
     */
    private void loadAssessments() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria query = new QueryByCriteria(IepPerformanceLevel.class, criteria);
        query.addOrderByAscending(getDictionary().findDataDictionaryFieldByAlias(ALIAS_TRANS_ASSESS_DATE)
                .getJavaName());
        query.addOrderByAscending(getDictionary().findDataDictionaryFieldByAlias(ALIAS_TRANS_ASSESS_TOOL)
                .getJavaName());

        m_assessmentData = getBroker().getCollectionByQuery(query);
    }

    /**
     * Loads goal data into a map for fast retrieval. The map loaded is keyed on IEP OID and each
     * value contains a collection of sorted IepGoal objects.
     */
    private void loadGoals() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, criteria);

        goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
        goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);

        m_goalData = getBroker().getCollectionByQuery(goalsQuery);
    }

    /**
     * Loads service data into map for fast retrieval. The map loaded is keyed on IEP OID and each
     * value contains a collection of sorted IepService objects.
     */
    private void loadServices() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);

        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_MODE);
        servicesQuery.addOrderByAscending(IepService.COL_START_DATE);
        servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW); // Focus on goal number

        m_servicesData = getBroker().getGroupedCollectionByQuery(
                servicesQuery, IepService.COL_SERVICE_MODE, IEP_INITIAL_CAPACITY);
    }


    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, FORMAT_IDS);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }



    /*
     * --------------------------------------------------------------------------------------------
     * Subreport Methods
     * --------------------------------------------------------------------------------------------
     */

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     * @param reportFormat String
     */
    private void preparePage(List<ReportData> grids, IepData iep, String reportFormat) {
        ReportDataGrid repotDataGrid = new ReportDataGrid();
        Object grid = null;
        InputStream format = null;
        if (reportFormat.equals(PAGE_2_FORMAT_ID) || reportFormat.equals(PAGE_2T_FORMAT_ID)) {
            prepareMeetingPage(repotDataGrid, iep, reportFormat);
        } else if (reportFormat.equals(PAGE_3T_FORMAT_ID)) {
            prepareTransitionAssessmentsPage(repotDataGrid, iep);
        } else if (reportFormat.equals(PAGE_5_FORMAT_ID)) {
            prepareGoalsPage(repotDataGrid, iep);
        } else if (reportFormat.equals(PAGE_5T_FORMAT_ID)) {
            prepareTransitionServicesPage(repotDataGrid, iep);
        } else if (reportFormat.equals(PAGE_7_FORMAT_ID)) {
            prepareServicesPage(repotDataGrid, iep);
        } else if (reportFormat.equals(PAGE_9_FORMAT_ID)) {
            prepareAccommodationsPage(repotDataGrid, iep);
        } else if (reportFormat.equals(PAGE_12_FORMAT_ID) || reportFormat.equals(PAGE_12P_FORMAT_ID)) {
            grid = m_rihlp.new SimpleFormDataSourceExtends(iep, iep, getDictionary(), getLocale());
            format = new ByteArrayInputStream(getSubreportFormat(reportFormat));
        } else {
            repotDataGrid.append();
            repotDataGrid.set(COL_IEP, iep);
            repotDataGrid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, repotDataGrid));
            repotDataGrid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(reportFormat));
        }

        if (format == null) {
            format = getFormat();
        }

        if (grid == null) {
            repotDataGrid.beforeTop();
            grid = repotDataGrid;
        }
        ReportData reportData = new ReportData(grid, format, getParameters());
        grids.add(reportData);
    }

    /**
     * Prepares the second IEP page for the IEP team meeting.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     * @param reportFormat String
     */
    private void prepareMeetingPage(ReportDataGrid grid, IepData iep, String reportFormat) {
        Collection<IepMeetingAttendance> meetings = null;
        if (isBlank()) {
            IepMeetingAttendance blankAttendance = new IepMeetingAttendance(getBroker().getPersistenceKey());
            blankAttendance.setStudentOid(iep.getStudentOid());

            meetings = new ArrayList<IepMeetingAttendance>(16);
            for (int i = 0; i < 16; i++) {
                meetings.add(blankAttendance);
            }
            grid.append();
            grid.set(COL_IEP, iep);
            grid.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(meetings, getDictionary(), getLocale()));
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(reportFormat));
        } else {
            IepMeeting iepMeeting = getRecentMeeting(iep);
            if (iepMeeting != null) {
                meetings = iepMeeting.getMeetingAttendance();

                if (meetings != null && !meetings.isEmpty()) {
                    // filter by "itm-iep-tm" == true if "meeting-iep-development" is true
                    DataDictionaryField mtgIepDevField =
                            getDictionary().findDataDictionaryFieldByAlias(ALIAS_MTG_IEP_DEVELOPMENT);
                    final DataDictionaryField itmIepTeam =
                            getDictionary().findDataDictionaryFieldByAlias(ALIAS_ITM_IEP_TEAM);
                    DataDictionaryField imaOtherPresentField =
                            getDictionary().findDataDictionaryFieldByAlias(ALIAS_IMA_ADDITIONAL_PRESENT);
                    boolean canBeFiltered = mtgIepDevField != null && itmIepTeam != null;
                    if (canBeFiltered) {
                        boolean isIepTeamOnly = BooleanAsStringConverter.TRUE
                                .equals(iepMeeting.getFieldValueByBeanPath(mtgIepDevField.getJavaName()));
                        if (isIepTeamOnly) {
                            org.apache.commons.collections.CollectionUtils.filter(meetings, new Predicate() {

                                @Override
                                public boolean evaluate(Object object) {

                                    IepMeetingAttendance attendance = (IepMeetingAttendance) object;
                                    IepTeamMember teamMember = attendance.getTeamMember();
                                    String otherIndicator =
                                            (String) attendance
                                                    .getFieldValueByBeanPath(imaOtherPresentField.getJavaName());
                                    if (teamMember != null) {
                                        String teamMemberIndicator =
                                                (String) teamMember.getFieldValueByBeanPath(itmIepTeam.getJavaName());
                                        if (BooleanAsStringConverter.TRUE.equals(teamMemberIndicator)) {
                                            return true;
                                        }
                                    } else if (BooleanAsStringConverter.TRUE.equals(otherIndicator)) {
                                        return true;
                                    }

                                    return false;
                                }
                            });
                        }
                    }
                }

                if (meetings == null || meetings.isEmpty()) {
                    IepMeetingAttendance blankAttendance = new IepMeetingAttendance(getBroker().getPersistenceKey());
                    blankAttendance.setStudentOid(iep.getStudentOid());

                    meetings = new ArrayList<IepMeetingAttendance>(16);
                    for (int i = 0; i < 16; i++) {
                        meetings.add(blankAttendance);
                    }
                }
                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE,
                        new BeanCollectionDataSource(meetings, getDictionary(), getLocale()));
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(reportFormat));
            }
        }

    }

    /**
     * Prepares IEP page 5 and 7T for goal data.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void prepareGoalsPage(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 5 page can contain 2 goals. If there are more than two goals, we must
         * insert additional pages for IEP 5.
         */
        Collection goals = null;
        if (isBlank()) {
            IepGoal blankGoal = new IepGoal(getBroker().getPersistenceKey());
            blankGoal.setStudentOid(iep.getStudentOid());

            goals = new ArrayList<IepGoal>(2);
            goals.add(blankGoal);
            goals.add(blankGoal);
        } else {
            goals = m_goalData;
        }

        if (goals == null) {
            IepGoal blankGoal1 = new IepGoal(getBroker().getPersistenceKey());
            IepGoal blankGoal2 = new IepGoal(getBroker().getPersistenceKey());
            goals = new ArrayList<IepGoal>(2);
            goals.add(blankGoal1);
            goals.add(blankGoal2);
        }

        int goalCount = 0;
        ReportDataGrid currentPage = null;
        Iterator iterator = goals.iterator();
        while (iterator.hasNext()) {
            IepGoal goal = (IepGoal) iterator.next();

            if (goalCount % MAX_GOALS_PER_PAGE == 0) {
                if (currentPage != null) {
                    currentPage.beforeTop();
                }

                currentPage = new ReportDataGrid(2, 1);

                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5_FORMAT_ID));
            }

            prepareGoalPage_addGoal(currentPage, goal);

            goalCount++;
        }

        if (currentPage != null) {
            currentPage.beforeTop();
        }
    }

    /**
     * Adds the passed goal to the subreport data grid used on the IEP's goal page.
     *
     * @param currentPage ReportDataGrid
     * @param goal IepGoal
     */
    private void prepareGoalPage_addGoal(ReportDataGrid currentPage, IepGoal goal) {
        currentPage.append();
        currentPage.set(COL_GOAL, goal);
        currentPage.set(COL_SUBREPORT_DATA_SOURCE, new BeanCollectionDataSource(
                goal.getIepGoalObjectives(getBroker()), getDictionary(), getLocale()));
        currentPage.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5_OBJ_FORMAT_ID));
    }

    /**
     * Prepares IEP page 7 and 9T for service data.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void prepareServicesPage(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 7 page can contain 6 services. If there are more of any of these types, we must
         * insert additional pages for IEP 7.
         */
        Collection<IepService> servicesA =
                prepareServicesPage_addBlankServices(m_servicesData.get(SERVICES_MODE_A), SERVICES_MODE_A);
        Collection<IepService> servicesB =
                prepareServicesPage_addBlankServices(m_servicesData.get(SERVICES_MODE_B), SERVICES_MODE_B);
        Collection<IepService> servicesC = m_servicesData.get(SERVICES_MODE_C1);

        if (m_servicesData.get(SERVICES_MODE_C2) != null) {
            servicesC.addAll(m_servicesData.get(SERVICES_MODE_C2));
        }

        if (m_servicesData.get(SERVICES_MODE_C3) != null) {
            servicesC.addAll(m_servicesData.get(SERVICES_MODE_C3));
        }

        servicesC = prepareServicesPage_addBlankServices(servicesC, SERVICES_MODE_C1);

        Collection<IepService> allServices = new LinkedList(servicesA);
        allServices.addAll(servicesB);
        allServices.addAll(servicesC);

        ReportDataGrid currentPage = null;
        int serviceCount = 0;
        for (IepService service : allServices) {
            if (serviceCount % MAX_SERVICES_PER_PAGE == 0) {
                if (currentPage != null) {
                    currentPage.beforeTop();
                }

                currentPage = new ReportDataGrid(2, 1);

                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_7_FORMAT_ID));
            }
            prepareServicesPage_addService(currentPage, service,
                    Boolean.valueOf(serviceCount == 0 ||
                            serviceCount == servicesA.size() ||
                            serviceCount == (servicesA.size() + servicesB.size())));

            serviceCount++;
        }
        currentPage.beforeTop();
    }

    /**
     * Adds the passed service to the data grid for the IEP service subreport's current page.
     *
     * @param currentPage ReportDataGrid
     * @param service IepService
     * @param printHeader Boolean
     */
    private void prepareServicesPage_addService(ReportDataGrid currentPage, IepService service, Boolean printHeader) {
        currentPage.append();
        currentPage.set(COL_SERVICE, service);
        currentPage.set(COL_PRINT_HEADER, printHeader.toString());
    }

    /**
     * Adds the appropriate amount of blank services to fill the rest of a half page.
     *
     * @param services Collection<IepService>
     * @param serviceMode String
     * @return Collection<IepService>
     */
    private Collection<IepService> prepareServicesPage_addBlankServices(Collection<IepService> services,
                                                                        String serviceMode) {
        if (services == null) {
            services = new LinkedList<IepService>();
        }

        while (services.size() % (MAX_SERVICES_PER_PAGE / 2) != 0 || services.size() < 3) {
            IepService tempService = new IepService(getBroker().getPersistenceKey());
            tempService.setServiceMode(serviceMode);
            services.add(tempService);
        }

        return services;
    }

    /**
     * Prepares IEP page 9 and 11T for accommodations data.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void prepareAccommodationsPage(ReportDataGrid grid, IepData iep) {
        ReportDataGrid currentPage = new ReportDataGrid(MIN_ACCOMMODATIONS, 1);

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_9_FORMAT_ID));

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, iep.getOid());

        QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);

        QueryIterator accIt = getBroker().getIteratorByQuery(query);
        int i = 0;
        try {
            while (accIt.hasNext() || i < MIN_ACCOMMODATIONS) {
                currentPage.append();
                if (accIt.hasNext()) {
                    currentPage.set(COL_ACCOMMODATION, accIt.next());
                } else {
                    currentPage.set(COL_ACCOMMODATION, new IepAccommodation(getBroker().getPersistenceKey()));
                }

                currentPage.set(COL_IEP, iep);
                i++;
            }
        } finally {
            accIt.close();
        }

        currentPage.beforeTop();
    }

    /**
     * Prepares IEP page 3T for transition assessments.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void prepareTransitionAssessmentsPage(ReportDataGrid grid, IepData iep) {
        ReportDataGrid assessmentGrid = new ReportDataGrid(MIN_ASSESSMENTS, 1);

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, assessmentGrid);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3T_FORMAT_ID));

        int assessmentCount = 0;
        for (IepPerformanceLevel assessment : m_assessmentData) {
            assessmentGrid.append();
            assessmentGrid.set(COL_ASSESSMENT, assessment);
            assessmentGrid.set(COL_IEP, iep);

            assessmentCount++;
        }

        while (assessmentCount < MIN_ASSESSMENTS) {
            assessmentGrid.append();
            assessmentGrid.set(COL_ASSESSMENT, new IepPerformanceLevel(getBroker().getPersistenceKey()));
            assessmentGrid.set(COL_IEP, iep);

            assessmentCount++;
        }

        assessmentGrid.beforeTop();
    }

    /**
     * Prepares IEP page 5T for transition services.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void prepareTransitionServicesPage(ReportDataGrid grid, IepData iep) {
        ReportDataGrid servicesGrid = new ReportDataGrid(MIN_SERVICES_PER_AREA * 3, 1);

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, servicesGrid);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5T_FORMAT_ID));

        Collection<IepService> services = m_servicesData.get(SERVICES_MODE_TRANSITION);

        // add blanks if the list is empty
        if (services == null || services.isEmpty()) {
            services = new ArrayList<IepService>(MIN_SERVICES_PER_AREA * 3);

            IepService service;

            for (int i = 0; i < MIN_SERVICES_PER_AREA; i++) {
                service = new IepService(getBroker().getPersistenceKey());
                service.setFieldValueByAlias(ALIAS_IEP_SERVICE_TRANSITION_AREA,
                        TRANSITION_SERVICES_AREA_EDU, getDictionary());
                services.add(service);
            }

            for (int i = 0; i < MIN_SERVICES_PER_AREA; i++) {
                service = new IepService(getBroker().getPersistenceKey());
                service.setFieldValueByAlias(ALIAS_IEP_SERVICE_TRANSITION_AREA,
                        TRANSITION_SERVICES_AREA_EMP, getDictionary());
                services.add(service);
            }

            for (int i = 0; i < MIN_SERVICES_PER_AREA; i++) {
                service = new IepService(getBroker().getPersistenceKey());
                service.setFieldValueByAlias(ALIAS_IEP_SERVICE_TRANSITION_AREA,
                        TRANSITION_SERVICES_AREA_IND, getDictionary());
                services.add(service);
            }
        } else {
            services = CollectionUtils.sortBeans(services,
                    getDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_SERVICE_TRANSITION_AREA)
                            .getJavaName(),
                    false);
        }

        for (IepService service : services) {
            servicesGrid.append();
            servicesGrid.set(COL_SERVICE, service);
            servicesGrid.set(COL_IEP, iep);
        }

        servicesGrid.beforeTop();
    }

    /**
     * Format date.
     *
     * @param paramDate Object
     * @return String
     */
    private String formatDate(Object paramDate) {
        String returnValue = null;
        Date date = null;
        if (paramDate != null && paramDate instanceof String) {
            try {
                date = m_formatterFrom.parse((String) paramDate);
            } catch (ParseException e) {
                // nothing to do

            }
        } else if (paramDate != null && paramDate instanceof Date) {
            date = (Date) paramDate;
        }

        if (date != null) {
            returnValue = m_formatterTo.format(date);
        }

        return returnValue;
    }
}
