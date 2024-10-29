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
package com.x2dev.procedures.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.jdom.Element;

/**
 * State reporting export module for UK School Census export.
 *
 * @author Follett Software Company
 */
public class SchoolCensus extends XMLStateReportData {

    /**
     * General Constants
     */
    private static final String STRING_EMPTY = "";
    private static final String STRING_ZERO = "0";
    private static final String STRING_ONE = "1";

    /**
     * Input Definition Parameters
     */
    private static final String PARAM_CENSUS_DATE = "censusDate";
    private static final String PARAM_FSM_START_DATE = "fsmStartDate";
    private static final String PARAM_FSM_END_DATE = "fsmEndDate";
    private static final String PARAM_PERIOD_NUM = "periodNumber";
    private static final String PARAM_PREV_TERM_START_DATE = "previousTermStartDate";
    private static final String PARAM_SCHOOL_OID = "schoolOid";
    private static final String PARAM_TERM = "term";
    private static final String PARAM_TERM_END_DATE = "termEndDate";
    private static final String PARAM_TERMLY_ATTENDANCE_START_DATE = "termlyAttendanceStartDate";
    private static final String PARAM_TERMLY_ATTENDANCE_END_DATE = "termlyAttendanceEndDate";
    private static final String PARAM_TERMLY_EXCLUSION_START_DATE = "termlyExclusionStartDate";
    private static final String PARAM_TERMLY_EXCLUSION_END_DATE = "termlyExclusionEndDate";

    /**
     * Model Definition Parameters
     */
    private static final String MODEL_PARAM_DATE_FORMAT = "dateFormat";
    private static final String MODEL_PARAM_CBDS = "CBDS";
    private static final String MODEL_PARAM_LOAD_DATES = "loadDates";
    private static final String MODEL_PARAM_REPORT = "report";
    private static final String MODEL_PARAM_SCHOOL_YEAR = "schoolYear";
    private static final String MODEL_PARAM_TERM = "term";

    /**
     * Model Retrievers
     */
    private static final String MODEL_RETRIEVER_ATTENDANCE = "ATTENDANCE";
    private static final String MODEL_RETRIEVER_CLASS = "CLASS";
    private static final String MODEL_RETRIEVER_EXCLUSION = "EXCLUSION";
    private static final String MODEL_RETRIEVER_FSMELIGIBLE = "FSMELIGIBLE";
    private static final String MODEL_RETRIEVER_FUNDEDHOURS = "FUNDEDHOURS";
    private static final String MODEL_RETRIEVER_HOURSATSETTING = "HOURSATSETTING";
    private static final String MODEL_RETRIEVER_PUPILSTATUS = "PUPILSTATUS";
    private static final String MODEL_RETRIEVER_RESOURCEDPROVISIONID = "RESOURCEDPROVISIONID";
    private static final String MODEL_RETRIEVER_SENPROVISION = "SENPROVISION";
    private static final String MODEL_RETRIEVER_SENUNITMEMBER = "SENUNITMEMBER";
    private static final String MODEL_RETRIEVER_SYSTEM = "SYSTEM";
    private static final String MODEL_RETRIEVER_UNITCONTACTTIME = "UNITCONTACTTIME";


    /**
     * Query Names
     */
    private static final String QUERY_ANNUAL_EXCLUSIONS = "annualExclusions";
    private static final String QUERY_CLASSES = "classes";
    private static final String QUERY_DISABILITIES = "disabilities";
    private static final String QUERY_FSM = "fsm";
    private static final String QUERY_LEARNER_SUPPORT_ITEMS = "learnerSupportItems";
    private static final String QUERY_PUPILS_NO_LONGER_ON_ROLL = "pupilsNoLongerOnRoll";
    private static final String QUERY_PUPILS_ON_ROLL = "pupilsOnRoll";
    private static final String QUERY_SCHOOL = "school";
    private static final String QUERY_SEN_NEEDS = "senNeeds";
    private static final String QUERY_TERMLY_EXCLUSIONS = "termlyExclusions";

    /**
     * Element Names
     */
    // private static final String ELEMENT_LEARNING_AIMS = "learningAims";
    private static final String ELEMENT_ADMISSION_APPEALS = "admissionAppeals";
    private static final String ELEMENT_ANNUAL_ATTENDANCE = "annualAttendance";
    private static final String ELEMENT_ANNUAL_EXCLUSIONS = "annualExclusions";
    private static final String ELEMENT_ASC_ACTIVITY = "ascActivity";
    private static final String ELEMENT_ATTENDANCE = "attendance";
    private static final String ELEMENT_CATEGORY_DETAILS = "CategoryDetails";
    private static final String ELEMENT_CENSUS_TEACHER_CATEGORY = "CensusTeacherCategory";
    private static final String ELEMENT_CHILD_CARE_PLACES = "childCarePlaces";
    private static final String ELEMENT_CHILD_MOTHERS = "childMothers";
    private static final String ELEMENT_CLASS_TYPE = "classType";
    private static final String ELEMENT_CLASSES = QUERY_CLASSES;
    private static final String ELEMENT_CONNEXIONS = "connexions";
    private static final String ELEMENT_DISABILITIES = "disabilities";
    private static final String ELEMENT_ETHNICITY = "ethnicity";
    private static final String ELEMENT_EXCLUSIONS = "exclusions";
    private static final String ELEMENT_FE_COLLEGE = "feCollege";
    private static final String ELEMENT_FULL_TIME = "FullTime";
    private static final String ELEMENT_GENDER = "Gender";
    private static final String ELEMENT_HEAD_COUNT = "HeadCount";
    private static final String ELEMENT_HOURS = "Hours";
    private static final String ELEMENT_INFANT_ADMISSIONS_APPEALS = "infantAdmissionsAppeals";
    private static final String ELEMENT_KEY_STAGE = "keyStage";
    private static final String ELEMENT_LANGUAGE = "language";
    private static final String ELEMENT_LEARNER_SUPPORT = "learnerSupport";
    private static final String ELEMENT_LEARNER_SUPPORT_NO_LONGER = "learnerSupportNoLonger";
    private static final String ELEMENT_MISCELLANEOUS = "miscellaneous";
    private static final String ELEMENT_NC_YEAR_LEAVING = "ncYearLeaving";
    private static final String ELEMENT_NO_LONGER_ANNUAL_ATTENDANCE = "annualAttendanceNoLonger";
    private static final String ELEMENT_NO_LONGER_ANNUAL_EXCLUSIONS = "annualExlusionNoLongerPupil";
    private static final String ELEMENT_NO_LONGER_ATTENDANCE = "attendanceNoLonger";
    private static final String ELEMENT_NO_LONGER_SUMMER_HALF_TERM2_ATTENDANCE = "summerHalfTerm2AttendanceNoLonger";
    private static final String ELEMENT_NO_LONGER_TERMLY_ATTENDANCE = "termlyAttendanceNoLonger";
    private static final String ELEMENT_NO_LONGER_TERMLY_EXCLUSIONS = "termlyExlusionNoLongerPupil";
    private static final String ELEMENT_PART_TIME = "PartTime";
    private static final String ELEMENT_PART_TIME_HOURS = "PartTimeHours";
    private static final String ELEMENT_PUPIL_NO_LONGER_ON_ROLL_HOME_INFORMATION = "pupilNoLongerOnRollHomeInformation";
    private static final String ELEMENT_PUPILS_NO_LONGER_ON_ROLL = QUERY_PUPILS_NO_LONGER_ON_ROLL;
    private static final String ELEMENT_RECONCILIATION = "reconciliation";
    private static final String ELEMENT_REFERENCE_DATE = "ReferenceDate";
    private static final String ELEMENT_RESOURCED_PROVISION_INDICATOR = "resourcedProvisionIndicator";
    private static final String ELEMENT_SCHOOL_LOCATION = "schoolLocation";
    private static final String ELEMENT_SEN_NEEDS = QUERY_SEN_NEEDS;
    private static final String ELEMENT_SEN_UNIT_INDICATOR = "senUnitIndicator";
    private static final String ELEMENT_SERVICE_CHILD = "serviceChild";
    private static final String ELEMENT_SPECIAL_SCHOOL = "specialSchool";
    private static final String ELEMENT_STAFF_INFORMATION = "staffInformation";
    private static final String ELEMENT_SUMMER_HALF_TERM2_ATTENDANCE = "summerHalfTerm2Attendance";
    private static final String ELEMENT_SUPPORT_CATEGORY = "SupportCategory";
    private static final String ELEMENT_SUPPORT_CATEGORY_DETAILS = "supportCategoryDetails";
    private static final String ELEMENT_SUPPORT_STAFF = "supportStaff";
    private static final String ELEMENT_SUPPORT_STAFF_2 = "SupportStaff";
    private static final String ELEMENT_TEACHER_CATEGORY_DETAILS = "teacherCategoryDetails";
    private static final String ELEMENT_TEEN_MOTHER_PLACES = "teenMotherPlaces";
    private static final String ELEMENT_TERMLY_ATTENDANCE = "termlyAttendance";
    private static final String ELEMENT_TERMLY_EXCLUSIONS = "termlyExclusions";
    private static final String ELEMENT_TYPE_OF_CLASS = "typeOfClass";
    private static final String ELEMENT_ULN = "uln";
    private static final String ELEMENT_UNIT_CONTACT_TIME_PUPIL = "unitContactTime";
    private static final String ELEMENT_WORK_EXPERIENCE = "workExperience";
    private static final String ELEMENT_YSSA = "yssa";

    /**
     * Alias Names
     */
    private static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    private static final String ALIAS_EXCLUSION_CATEGORY = "DFE EXCLUSION CATEGORY";
    private static final String ALIAS_HOURS_PER_WEEK = "DFE HOURS PER WEEK";
    private static final String ALIAS_SCHOOL_CENSUS_HIDE = "DFE SCHOOL CENSUS HIDE";
    private static final String ALIAS_SEN_PROVISION = "DFE SEN PROVISION";
    private static final String ALIAS_STUDENT_CENSUS_HIDE = "DFE STUDENT CENSUS HIDE";
    private static final String ALIAS_TENURE = "DFE TENURE";

    /**
     * School Codes
     */
    protected static final String SCHOOL_MIDDLE_PRIMARY = "MP";
    protected static final String SCHOOL_MIDDLE_SECONDARY = "MS";
    protected static final String SCHOOL_NURSERY = "NS";
    protected static final String SCHOOL_PRIMARY = "PS";
    protected static final String SCHOOL_SECONDARY = "SS";
    protected static final String SCHOOL_SPECIAL = "SP";

    /**
     * Term Codes
     */
    protected static final String TERM_AUTUMN = "AUT";
    protected static final String TERM_SPRING = "SPR";
    protected static final String TERM_SUMMER = "SUM";
    protected static final String TERM_PRU = "PRU";

    /**
     * Student Program Codes
     */
    protected static final String STUDENT_PROGRAM_CODE_FSM = "Free/Reduced Lunch";
    protected static final String STUDENT_PROGRAM_CODE_SE = "Special Education";

    /**
     * Helper class for handling student-related data querying
     */
    private StudentHistoryHelper m_helper;

    /**
     * Model with re-usable code, retrievers, validators
     */
    private StateReportModel m_model;

    /**
     * Map of full time teacher keyed on staff type
     */
    private Map<String, String> m_fullTimeTeacher;

    /**
     * Map of part time teachers keyed on staff type
     */
    private Map<String, String> m_partTimeTeacher;

    /**
     * Map of part time teacher's hours worked keyed on staff type
     */
    private Map<String, String> m_partTimeTeacherHours;

    /**
     * Map of support staff hours worked keyed on staff type
     */
    private Map<String, String> m_supportStaffHours;

    /**
     * Count of the active full time support staff
     */
    private String m_fullTimeSupport;

    /**
     * Count of the active part time support staff
     */
    private String m_partTimeSupport;

    /**
     * Count of support staff keyed on staff type
     */
    private Map<String, String> m_headCountSupport;

    /**
     * Collection of Teacher Category Codes
     */
    private Collection<String> m_teacherCategoryCodes;

    /**
     * Collection of Support Staff Category Codes
     */
    private Collection<String> m_supportCategoryCodes;

    /**
     * Category details Element for teachers
     */
    private Element m_teacherCategoryDetails;

    /**
     * Category details Element for support staff
     */
    private Element m_supportCategoryDetails;

    /**
     * Support staff Element
     */
    private Element m_supportStaff;

    /**
     * Map of a list of Master Schedule oids keyed on student oids
     */
    private Map<String, List<MasterSchedule>> m_rawStudentSchedulesMap;

    /**
     * Map of a list of Master Schedule oids keyed on staff oids
     */
    private Map<String, List<MasterSchedule>> m_rawTeacherSchedulesMap;

    /**
     * List of all section Oids at the selected time
     */
    private ArrayList<String> m_sectionOids;

    /**
     * Grouped collection of Conduct action records keyed on studentOid
     */
    private Map<String, Collection<ConductAction>> m_actionStudentMap;

    /**
     * List of StudentOids having a Active Attendance
     */
    private Set<String> m_attendanceStudentOids = new HashSet<String>();

    /**
     * List of StudentOids having a Conduct Action
     */
    private Set<String> m_conductActionStudentOids = new HashSet<String>();

    /**
     * List of StudentOids having a Learning Support Item
     */
    private Set<String> m_learnerSupportItemStudentOids = new HashSet<String>();

    /**
     * Model Parameters
     */
    private PlainDate m_censusDate;
    private PlainDate m_fsmStartDate;
    private PlainDate m_fsmEndDate;
    private PlainDate m_termEndDate;
    private String m_term;
    private PlainDate m_termlyAttendanceStartDate;
    private PlainDate m_termlyAttendanceEndDate;
    private PlainDate m_prevTermStartDate;
    private SimpleDateFormat m_dateFormat;
    private PlainDate m_ageAtDate;

    /**
     * Initialize the export.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        /*
         * Set up the PlainDate instance variables depending on the term
         */
        m_model = loadModel(MODEL_PARAM_CBDS);

        String term = (String) getParameter(PARAM_TERM);
        m_model.set(MODEL_PARAM_REPORT, "SchoolCensus");
        m_model.set(MODEL_PARAM_TERM, term);
        m_model.set(MODEL_PARAM_SCHOOL_YEAR, Integer.valueOf(getCurrentContext().getSchoolYear()));
        m_model.execute(MODEL_PARAM_LOAD_DATES);
        m_dateFormat = (SimpleDateFormat) m_model.get(MODEL_PARAM_DATE_FORMAT);

        m_censusDate = (PlainDate) m_model.get(PARAM_CENSUS_DATE);
        setElement(ELEMENT_REFERENCE_DATE, new Element(ELEMENT_REFERENCE_DATE).setText(m_censusDate.toString()));
        m_termEndDate = (PlainDate) m_model.get(PARAM_TERM_END_DATE);
        m_fsmStartDate = (PlainDate) m_model.get(PARAM_FSM_START_DATE);
        m_fsmEndDate = (PlainDate) m_model.get(PARAM_FSM_END_DATE);
        m_term = (String) m_model.get(PARAM_TERM);
        m_prevTermStartDate = (PlainDate) m_model.get(PARAM_PREV_TERM_START_DATE);
        m_termlyAttendanceStartDate = (PlainDate) m_model.get(PARAM_TERMLY_ATTENDANCE_START_DATE);
        m_termlyAttendanceEndDate = (PlainDate) m_model.get(PARAM_TERMLY_ATTENDANCE_END_DATE);
        m_ageAtDate = (PlainDate) m_model.get("ageAtDate");

        /*
         * Retrieve the DFE SCHOOL PHASE value from the school
         */
        SisSchool school = (SisSchool) getSchool();
        setBeanToQuery(QUERY_SCHOOL, school);

        String schoolPhase = getFieldValue(school, "200006");
        // String highestNcYearAsString = getFieldValue(school, "200013");
        if (StringUtils.isEmpty(schoolPhase)) {
            addSetupError("No school phase", String.format("%s does not have a school phase set!", school.getName()));
            return;
        }

        /*
         * set up the criteria and queries
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
        // m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, termStartDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_termEndDate);

        /*
         * Student-level stuff
         */
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        // Check for Student School Census Hide field
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField studentCensusHideField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_CENSUS_HIDE);
        X2Criteria studentCensusHide = new X2Criteria();
        X2Criteria studentCensusHideNull = new X2Criteria();
        if (studentCensusHideField != null) {
            String studentCensusHideJavaName = studentCensusHideField.getJavaName();
            studentCensusHide.addEqualTo(studentCensusHideJavaName, BooleanAsStringConverter.FALSE);
            studentCensusHideNull.addIsNull(studentCensusHideJavaName);
            studentCensusHide.addOrCriteria(studentCensusHideNull);
            studentCriteria.addAndCriteria(studentCensusHide);
        }

        X2Criteria studentCriteria2 = studentCriteria;
        Criteria withdrawalCriteria = new Criteria();
        withdrawalCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        // withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
        // startDate);
        withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_prevTermStartDate);
        withdrawalCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_censusDate);
        SubQuery enrQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria);
        Criteria withdrawnCriteria = new Criteria();
        withdrawnCriteria.addIn(X2BaseBean.COL_OID, enrQuery);
        withdrawnCriteria.addAndCriteria(studentCensusHide); // census hide for withdrawn
        studentCriteria2.addOrCriteria(withdrawnCriteria);

        Map<String, Collection<ConductAction>> actionMap = loadExclusions(studentCriteria2);
        m_conductActionStudentOids = m_actionStudentMap.keySet();

        Criteria withdrawalCriteria2 = new Criteria();
        withdrawalCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        withdrawalCriteria2.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_prevTermStartDate);
        withdrawalCriteria2.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_censusDate);

        if (m_conductActionStudentOids != null && !m_conductActionStudentOids.isEmpty()) {
            Criteria withdrawalCriteria3 = new Criteria();
            withdrawalCriteria3.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            withdrawalCriteria3.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_censusDate);
            withdrawalCriteria3.addIn(StudentEnrollment.COL_STUDENT_OID, m_conductActionStudentOids);
            withdrawalCriteria2.addOrCriteria(withdrawalCriteria3);
        }

        SubQuery enrQuery2 =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria2);

        Criteria withdrawnCriteria2 = new Criteria();
        withdrawnCriteria2.addIn(X2BaseBean.COL_OID, enrQuery2);
        withdrawnCriteria2.addAndCriteria(studentCensusHide); // census hide for withdrawn
        studentCriteria.addOrCriteria(withdrawnCriteria2);

        Map<String, Collection<StudentProgramParticipation>> learnerSupportItems = loadLearnerSupport(studentCriteria);
        m_learnerSupportItemStudentOids = learnerSupportItems.keySet();

        Map<String, Collection<StudentProgramParticipation>> fsmMap = loadFreeMeals(studentCriteria);
        Map<String, Collection<StudentProgramParticipation>> senMap = loadSens(studentCriteria);

        // HashSet activeSchedules = loadActiveSchedulesOids();
        /*
         * if (!StringUtils.isEmpty(highestNcYearAsString) &&
         * StringUtils.isNumeric(highestNcYearAsString))
         * {
         * int highestNcYearAsInt = Integer.parseInt(highestNcYearAsString);
         * if (highestNcYearAsInt > 11)
         * {
         * loadLearningAims(studentCriteria, activeSchedules);
         * }
         * else
         * {
         * removeElement("learningAims");
         * }
         * }
         * else
         * {
         * removeElement("learningAims");
         * }
         */

        // builds the maps for all students and teachers who are in a class during the selected time
        buildPersonScheduleMaps();
        loadSectionList(studentCriteria);

        BeanQuery studentQuery = new BeanQuery(SisStudent.class, studentCriteria);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        Map<String, Collection<IepDisability>> idbMap = loadDisabilities(studentCriteria);
        updateIdbMap(students, idbMap);

        loadAttendanceStudentOids(students);

        /*
         * Pupils
         */
        loadPupils(students);

        /*
         * School-level stuff
         */
        loadClasses(school);

        // Teacher Staff Types
        m_teacherCategoryCodes = new ArrayList<String>();
        m_teacherCategoryCodes.add("ET");
        m_teacherCategoryCodes.add("HL");
        m_teacherCategoryCodes.add("LQ");
        m_teacherCategoryCodes.add("LT");
        m_teacherCategoryCodes.add("NQ");
        m_teacherCategoryCodes.add("QA");
        m_teacherCategoryCodes.add("QT");
        m_teacherCategoryCodes.add("TA");
        m_teacherCategoryCodes.add("TE");
        m_teacherCategoryCodes.add("UA");

        // Support Staff Types
        m_supportCategoryCodes = new ArrayList<String>();
        m_supportCategoryCodes.add("AC");
        m_supportCategoryCodes.add("AO");
        m_supportCategoryCodes.add("BU");
        m_supportCategoryCodes.add("CQ");
        m_supportCategoryCodes.add("CU");
        m_supportCategoryCodes.add("EB");
        m_supportCategoryCodes.add("EO");
        m_supportCategoryCodes.add("IT");
        m_supportCategoryCodes.add("LI");
        m_supportCategoryCodes.add("ME");
        m_supportCategoryCodes.add("SN");
        m_supportCategoryCodes.add("SS");

        m_fullTimeTeacher = new HashMap<String, String>();
        m_partTimeTeacher = new HashMap<String, String>();
        m_partTimeTeacherHours = new HashMap<String, String>();
        m_supportStaffHours = new HashMap<String, String>();
        m_fullTimeSupport = STRING_EMPTY;
        m_partTimeSupport = STRING_EMPTY;
        m_headCountSupport = new HashMap<String, String>();
        m_teacherCategoryDetails = new Element(ELEMENT_CATEGORY_DETAILS);
        m_supportCategoryDetails = new Element(ELEMENT_CATEGORY_DETAILS);
        m_supportStaff = new Element(ELEMENT_SUPPORT_STAFF_2);

        loadPartTimeStaffHours();
        loadStaffCounts();
        loadStaffElements();

        setElement(ELEMENT_TEACHER_CATEGORY_DETAILS, m_teacherCategoryDetails);
        setElement(ELEMENT_SUPPORT_CATEGORY_DETAILS, m_supportCategoryDetails);
        setElement(ELEMENT_SUPPORT_STAFF, m_supportStaff);

        /*
         * Initialize instance variables, retrievers, validators, and what not.
         */
        if (getSetupErrors().isEmpty()) {
            m_model.initializeRetriever(MODEL_RETRIEVER_SYSTEM);
            m_model.initializeRetriever(MODEL_RETRIEVER_PUPILSTATUS, m_helper);
            m_model.initializeRetriever(MODEL_RETRIEVER_SENPROVISION, senMap);
            m_model.initializeRetriever(MODEL_RETRIEVER_SENUNITMEMBER, m_helper);
            m_model.initializeRetriever(MODEL_RETRIEVER_EXCLUSION, actionMap);
            m_model.initializeRetriever(MODEL_RETRIEVER_ATTENDANCE, students, m_helper);
            if (!TERM_PRU.equals(m_term)) {
                m_model.initializeRetriever(MODEL_RETRIEVER_CLASS, students, m_helper, m_rawStudentSchedulesMap);
            }
            m_model.initializeRetriever(MODEL_RETRIEVER_FSMELIGIBLE, fsmMap);
            // m_model.initializeRetriever("LEARNINGAIM", studentCriteria, activeSchedules);
            m_model.initializeRetriever(MODEL_RETRIEVER_FUNDEDHOURS);
            m_model.initializeRetriever(MODEL_RETRIEVER_HOURSATSETTING);
            m_model.initializeRetriever(MODEL_RETRIEVER_RESOURCEDPROVISIONID, senMap);
            m_model.initializeRetriever(MODEL_RETRIEVER_UNITCONTACTTIME);
            super.addCalcs(m_model.getRetrievers());
        }

        /*
         * Change any elements in the template.
         * Some elements in the template should not show depending on the term and school phase
         */
        setConditionalElements(term, schoolPhase);
    }

    /**
     * Populates a map of all sections students are attending during the selection period on the
     * Census Date.
     */
    private void buildPersonScheduleMaps() {
        SisSchool school = (SisSchool) getSchool();

        String censusDayOid = null;
        if (getDayBean() != null) {
            censusDayOid = getDayBean().getOid();
        }

        String selectedPeriodOid = null;
        if (getPeriodBean() != null) {
            selectedPeriodOid = getPeriodBean().getOid();
        }

        List<String> schoolTermOids = null;
        if (getSchoolTermOids() != null) {
            schoolTermOids = getSchoolTermOids();
        }


        // finds all StudentSchedule records based on the censusDay bean, selectedPeriod bean, and
        // schoolTermOids
        Criteria criteria = new X2Criteria();

        criteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField censusHideField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_CENSUS_HIDE);
        X2Criteria censusHide = new X2Criteria();
        X2Criteria censusHideNull = new X2Criteria();
        if (censusHideField != null) {
            String censusHideJavaName = censusHideField.getJavaName();
            censusHide.addEqualTo(
                    StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName,
                    BooleanAsStringConverter.FALSE);
            censusHideNull.addIsNull(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName);
            censusHide.addOrCriteria(censusHideNull);
            criteria.addAndCriteria(censusHide);
        }


        criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, school.getOid());

        if (!schoolTermOids.isEmpty()) {
            criteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                    MasterTerm.COL_SCHEDULE_TERM_OID,
                    schoolTermOids);
        }

        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_DAY_OID, censusDayOid);

        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_PERIOD_OID, selectedPeriodOid);

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, criteria);

        // Build a map of a list of student schedules.
        m_rawStudentSchedulesMap = new HashMap<String, List<MasterSchedule>>();

        QueryIterator studentScheduleIterator = getBroker().getIteratorByQuery(studentScheduleQuery);
        List<MasterSchedule> schedules = new ArrayList<MasterSchedule>();
        try {
            SisStudent lastStudent = null;
            while (studentScheduleIterator.hasNext()) {
                StudentSchedule currentSchedule = (StudentSchedule) studentScheduleIterator.next();
                SisStudent student = currentSchedule.getStudent();
                if (!ObjectUtils.match(student, lastStudent)) {
                    schedules = new ArrayList<MasterSchedule>();
                    lastStudent = student;
                    if (lastStudent != null) {
                        m_rawStudentSchedulesMap.put(lastStudent.getOid(), schedules);
                    }
                }

                schedules.add(currentSchedule.getSection());
            }

            if (lastStudent != null) {
                m_rawStudentSchedulesMap.put(lastStudent.getOid(), schedules);
            }
        } finally {
            studentScheduleIterator.close();
        }

        /*
         * Next, lookup all StudentScheduleChange records that may apply and update the schedule
         * list with them.
         * Pick up the schedule change records after report date in reverse date order.
         * Work back through them and adjust the schedule list:
         * 1. find a Drop, insert the dropped section into the student schedule list.
         * 2. find an Add, remove the added section from the student schedule list.
         */
        criteria = new X2Criteria();

        criteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        criteria.addGreaterOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, m_censusDate);
        criteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);
        criteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, getSchool().getOid());
        censusHide = new X2Criteria();
        censusHideNull = new X2Criteria();
        if (censusHideField != null) {
            String censusHideJavaName = censusHideField.getJavaName();
            censusHide.addEqualTo(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                            + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName,
                    BooleanAsStringConverter.FALSE);
            censusHideNull.addIsNull(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                            + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName);
            censusHide.addOrCriteria(censusHideNull);
            criteria.addAndCriteria(censusHide);
        }

        if (!schoolTermOids.isEmpty()) {
            criteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                    MasterTerm.COL_SCHEDULE_TERM_OID,
                    schoolTermOids);
        }

        criteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_DAY_OID, censusDayOid);

        criteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_PERIOD_OID, selectedPeriodOid);

        QueryByCriteria studentScheduleChangeQuery = new QueryByCriteria(StudentScheduleChange.class, criteria);

        // Order by student and descending change date.
        studentScheduleChangeQuery.addOrderByAscending(StudentScheduleChange.COL_STUDENT_OID);
        studentScheduleChangeQuery.addOrderByDescending(StudentScheduleChange.COL_EFFECTIVE_DATE);

        QueryIterator studentScheduleChangeIterator = getBroker().getIteratorByQuery(studentScheduleChangeQuery);
        try {
            while (studentScheduleChangeIterator.hasNext()) {
                StudentScheduleChange scheduleChange = (StudentScheduleChange) studentScheduleChangeIterator.next();
                SisStudent student = scheduleChange.getStudent();

                schedules = m_rawStudentSchedulesMap.get(student.getOid());
                if (schedules == null) {
                    schedules = new ArrayList<MasterSchedule>();
                    m_rawStudentSchedulesMap.put(student.getOid(), schedules);
                }

                if (StudentScheduleChange.CODE_ADD.equals(scheduleChange.getChangeTypeCode())) {
                    // Search for a matching section Oid. Remove it.
                    Iterator<MasterSchedule> schedIterator = schedules.iterator();
                    while (schedIterator.hasNext()) {
                        MasterSchedule mst = schedIterator.next();
                        if (mst.getOid().equals(scheduleChange.getMasterScheduleOid())) {
                            schedIterator.remove();
                            break;
                        }
                    }
                } else if (StudentScheduleChange.CODE_DROP.equals(scheduleChange.getChangeTypeCode())) {
                    // Search for a matching section Oid to verify that it is not already there.
                    boolean exists = false;
                    Iterator<MasterSchedule> schedIterator = schedules.iterator();
                    while (schedIterator.hasNext()) {
                        MasterSchedule mst = schedIterator.next();
                        if (mst.getOid().equals(scheduleChange.getMasterScheduleOid())) {
                            exists = true;
                            break;
                        }
                    }
                    // Add a the Master Schedule.
                    if (!exists) {
                        schedules.add(scheduleChange.getMasterSchedule());
                    }
                }
            }
        } finally {
            studentScheduleChangeIterator.close();
        }

        // finds all ScheduleTeacher records based on the dayBean, periodBean, and schoolTermOids
        criteria = new X2Criteria();

        criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        censusHide = new X2Criteria();
        censusHideNull = new X2Criteria();
        if (censusHideField != null) {
            String censusHideJavaName = censusHideField.getJavaName();
            censusHide.addEqualTo(
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName,
                    BooleanAsStringConverter.FALSE);
            censusHideNull.addIsNull(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + censusHideJavaName);
            censusHide.addOrCriteria(censusHideNull);
            criteria.addAndCriteria(censusHide);
        }

        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, school.getOid());

        if (!schoolTermOids.isEmpty()) {
            criteria.addIn(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                    MasterTerm.COL_SCHEDULE_TERM_OID,
                    schoolTermOids);
        }

        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_DAY_OID, censusDayOid);

        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER +
                MasterTerm.REL_MASTER_MATRICES + PATH_DELIMITER +
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER +
                ScheduleMatrix.COL_SCHEDULE_PERIOD_OID, selectedPeriodOid);

        QueryByCriteria teacherScheduleQuery = new QueryByCriteria(ScheduleTeacher.class, criteria);

        // Build a map of a list of teacher schedules.
        m_rawTeacherSchedulesMap = new HashMap<String, List<MasterSchedule>>();

        QueryIterator teacherScheduleIterator = getBroker().getIteratorByQuery(teacherScheduleQuery);
        List<MasterSchedule> teacherSchedules = new ArrayList<MasterSchedule>();
        try {
            SisStaff lastStaff = null;
            while (teacherScheduleIterator.hasNext()) {
                ScheduleTeacher currentSchedule = (ScheduleTeacher) teacherScheduleIterator.next();
                SisStaff staff = currentSchedule.getStaff();
                if (!ObjectUtils.match(staff, lastStaff)) {
                    teacherSchedules = new ArrayList<MasterSchedule>();
                    lastStaff = staff;
                    if (lastStaff != null) {
                        m_rawTeacherSchedulesMap.put(lastStaff.getOid(), teacherSchedules);
                    }
                }
                teacherSchedules.add(currentSchedule.getSection());
            }

            if (lastStaff != null) {
                m_rawTeacherSchedulesMap.put(lastStaff.getOid(), teacherSchedules);
            }
        } finally {
            teacherScheduleIterator.close();
        }
    }

    /**
     * Gets the ScheduleDay Bean based on the census date on the SchoolCalendarDate table.
     *
     * @return ScheduleDay bean
     */
    private ScheduleDay getDayBean() {
        SisSchool school = (SisSchool) getSchool();

        // Get a map of school schedule day oid on report date.
        // Go through the school calendars to find the day number on report date.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCalendarDate.COL_DATE, m_censusDate);
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + X2BaseBean.COL_OID, school.getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        ReportQueryByCriteria query = new ReportQueryByCriteria(SchoolCalendarDate.class,
                new String[] {SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                        SchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER},
                criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        ScheduleDay dayBean = null;
        try {
            while (iterator.hasNext()) {
                Object[] objects = (Object[]) iterator.next();
                String schoolOid = (String) objects[0];
                Number dayNum = (Number) objects[2];

                // finds the Schedule Day bean based on the Schedule Day Number on the
                // SchoolCalendarDate table
                criteria = new Criteria();
                criteria.addEqualTo(ScheduleDay.COL_NUMBER, dayNum);
                criteria.addEqualTo(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID,
                        schoolOid);
                criteria.addEqualToField(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        ScheduleDay.COL_SCHEDULE_OID);
                QueryByCriteria dayQuery = new QueryByCriteria(ScheduleDay.class, criteria);
                dayBean = (ScheduleDay) getBroker().getBeanByQuery(dayQuery);
            }
        } finally {
            iterator.close();
        }

        return dayBean;
    }

    /**
     * Gets the SchedulePeriod bean based the on inputed Period Number.
     *
     * @return SchedulePeriod bean
     */
    private SchedulePeriod getPeriodBean() {
        SchedulePeriod periodBean = null;
        String periodNum = (String) getParameter(PARAM_PERIOD_NUM);

        if (periodNum != null) {
            if (STRING_EMPTY.equals(periodNum)) {
                // This is a workaround to a bug with Aspen Jobs and using the selector with "1"
                periodNum = STRING_ONE;
            }
            Integer periodNumber = Integer.valueOf(periodNum);
            SisSchool school = (SisSchool) getSchool();

            // find the Schedule Period bean based on the selected period number and school selected
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    school.getOid());
            // criteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_INDICATOR, Boolean.TRUE);
            criteria.addEqualToField(SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    SchedulePeriod.COL_SCHEDULE_OID);
            criteria.addEqualTo(SchedulePeriod.COL_NUMBER, periodNumber);

            QueryByCriteria periodQuery = new QueryByCriteria(SchedulePeriod.class, criteria);

            periodBean = (SchedulePeriod) getBroker().getBeanByQuery(periodQuery);
        }

        return periodBean;
    }

    /**
     * Get a Student Age as of Aug 31.
     *
     * @param student Student
     * @return int
     */
    private int getStudentAgeAsOfAug31(Student student) {
        // PlainDate aug31 = (PlainDate) get(VAR_AGE_AT_DATE);
        int ageAsOfAug31 = 0;
        if (student.getPerson() != null && student.getPerson().getDob() != null) {
            ageAsOfAug31 = student.getPerson().getAgeAsOfDate(m_ageAtDate);
        }

        return ageAsOfAug31;
    }

    /**
     * Builds a list of all SchoolTerm oids based on the select school and census date.
     *
     * @return List
     */
    private List<String> getSchoolTermOids() {
        SisSchool school = (SisSchool) getSchool();

        // Get a map of school schedule term oids on the census date for the selected school.
        Criteria termCriteria = new Criteria();
        termCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, school.getOid());
        termCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_censusDate);
        termCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, m_censusDate);
        termCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.COL_SCHEDULE_OID);

        ReportQueryByCriteria termQuery = new ReportQueryByCriteria(ScheduleTermDate.class,
                new String[] {ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID,
                        ScheduleTermDate.COL_SCHEDULE_TERM_OID},
                termCriteria);

        List<String> schoolTermOids = new ArrayList<String>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(termQuery);
        try {
            while (iterator.hasNext()) {
                Object[] objects = (Object[]) iterator.next();
                String termOid = (String) objects[1];

                schoolTermOids.add(termOid);
            }
        } finally {
            iterator.close();
        }

        return schoolTermOids;
    }

    /**
     * Returns the selected school's active staff members.
     *
     * @return criteria
     */
    private Criteria getStaffCriteria() {
        String schoolOid = (String) getParameter(PARAM_SCHOOL_OID);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Staff.COL_SCHOOL_OID, schoolOid);
        criteria.addEqualTo(Staff.COL_STATUS, "Active");

        String excludeStaffBeanPath = translateAliasToJavaName(ALIAS_EXCLUDE_STF, false);
        if (!StringUtils.isEmpty(excludeStaffBeanPath)) {
            criteria.addNotEqualTo(excludeStaffBeanPath, BooleanAsStringConverter.TRUE);
        }

        return criteria;
    }

    /**
     * Loads the active schedules since the start of previous academic year
     *
     * return HashSet<String>.
     *
     * @param students Collection<SisStudent>
     */
    /*
     * private HashSet<String> loadActiveSchedulesOids()
     * {
     * HashSet<String> schoolYearsOids = new HashSet<String>();
     * 
     * //Get the current school year.
     * DistrictSchoolYearContext currentSchoolYear = getOrganization().getCurrentContext();
     * schoolYearsOids.add(currentSchoolYear.getOid());
     * 
     * //Get the previous school year.
     * int currentSchoolYearInt = currentSchoolYear.getSchoolYear();
     * X2Criteria previousSchoolYearCriteria = new X2Criteria();
     * int previousSchoolYearInt = currentSchoolYearInt - 1;
     * previousSchoolYearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, new
     * Integer(previousSchoolYearInt));
     * QueryByCriteria previousSchoolYearQuery = new
     * QueryByCriteria(DistrictSchoolYearContext.class, previousSchoolYearCriteria);
     * DistrictSchoolYearContext previousSchoolYear = (DistrictSchoolYearContext)
     * getBroker().getBeanByQuery(previousSchoolYearQuery);
     * if (previousSchoolYear != null)
     * {
     * schoolYearsOids.add(previousSchoolYear.getOid());
     * }
     * 
     * //Get all active school schedule Oid form the current and previous School Years
     * X2Criteria schoolScheduleContextCriteria = new X2Criteria();
     * schoolScheduleContextCriteria.addIn(SchoolScheduleContext.REL_DISTRICT_CONTEXT + "." +
     * X2BaseBean.COL_OID, schoolYearsOids);
     * schoolScheduleContextCriteria.addEqualTo(SchoolScheduleContext.REL_SCHOOL + "." +
     * School.COL_ARCHIVE_INDICATOR, Boolean.valueOf(false));
     * schoolScheduleContextCriteria.addNotEqualTo(SchoolScheduleContext.REL_SCHOOL + "." +
     * School.COL_SCHOOL_ID, "CTF School");
     * SubQuery schoolScheduleContextSubQuery = new SubQuery(SchoolScheduleContext.class,
     * SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, schoolScheduleContextCriteria);
     * Collection<String> scheduleOids =
     * getBroker().getSubQueryCollectionByQuery(schoolScheduleContextSubQuery);
     * HashSet uniqueScheduleOids = new HashSet(scheduleOids);
     * 
     * return uniqueScheduleOids;
     * }
     */

    /**
     * Load a list of StudentOids of Students that have a Attendance elements.
     * 
     * @param school the school
     */
    private void loadAttendanceStudentOids(Collection<SisStudent> students) {
        for (SisStudent student : students) {
            StudentEnrollment entryEnrollment = m_helper.getEnrollmentForDate(student.getOid(), m_termEndDate, "E");
            List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);

            int studentAgeAsOfAug31 = getStudentAgeAsOfAug31(student);

            if (4 <= studentAgeAsOfAug31 && studentAgeAsOfAug31 <= 15) // 18
            {
                if (entryEnrollment != null && entryEnrollment.getEnrollmentDate().before(m_termlyAttendanceEndDate)) {
                    PlainDate entryDate = entryEnrollment.getEnrollmentDate();
                    for (StudentEnrollmentSpan span : spans) {
                        Collection<PlainDate> inSessionDates =
                                CalendarManager.getInSessionDates(m_termlyAttendanceStartDate,
                                        m_termlyAttendanceEndDate, student, getBroker());

                        PlainDate lastActiveDate = span.getLastActiveDate();
                        if (lastActiveDate == null) {
                            lastActiveDate = m_termlyAttendanceStartDate;
                        }
                        int sessionsPossible = 0;
                        for (PlainDate date : inSessionDates) {
                            if (!date.before(entryDate) && !date.after(lastActiveDate)) {
                                sessionsPossible += 2;
                            }
                        }

                        if (sessionsPossible > 0) {
                            m_attendanceStudentOids.add(student.getOid());
                        }
                    }
                }
            }
        }
    }

    /**
     * Load all the master schedules that are within the school and school's active schedule.
     *
     * @param school the school
     */
    private void loadClasses(SisSchool school) {
        X2Criteria classesCriteria = new X2Criteria();
        classesCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                school.getOid());
        classesCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, school.getActiveScheduleOid());
        if (!m_sectionOids.isEmpty()) {
            classesCriteria.addIn(X2BaseBean.COL_OID, m_sectionOids);
        }

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField censusHideField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_CENSUS_HIDE);
        X2Criteria censusHide = new X2Criteria();
        X2Criteria censusHideNull = new X2Criteria();
        if (censusHideField != null) {
            String censusHideJavaName = censusHideField.getJavaName();
            censusHide.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + PATH_DELIMITER + censusHideJavaName, BooleanAsStringConverter.FALSE);
            censusHideNull.addIsNull(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + PATH_DELIMITER + censusHideJavaName);
            censusHide.addOrCriteria(censusHideNull);
        }
        classesCriteria.addAndCriteria(censusHide);

        BeanQuery classesQuery = new BeanQuery(MasterSchedule.class, classesCriteria);

        setCollectionToQuery(QUERY_CLASSES, getBroker().getCollectionByQuery(classesQuery));
    }

    /**
     * Load all the student disabilities, limiting the results to <tt>studentCriteria</tt>.
     *
     * @param studentCriteria student criteria
     * @return Map
     */
    private Map<String, Collection<IepDisability>> loadDisabilities(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

        X2Criteria idbCriteria = new X2Criteria();
        idbCriteria.addIn(IepDisability.COL_STUDENT_OID, studentSubQuery);
        BeanQuery idbQuery = new BeanQuery(IepDisability.class, idbCriteria);
        Map<String, Collection<IepDisability>> idbMap =
                getBroker().getGroupedCollectionByQuery(idbQuery, IepDisability.COL_STUDENT_OID, 64);

        setGroupedCollectionToQuery(QUERY_DISABILITIES, idbMap);
        return idbMap;
    }

    /**
     * Loads students' exclusions (a.k.a. 'conduct incidents'), actions, and offenses,
     * limiting the results to <tt>studentCriteria</tt>
     *
     * @param studentCriteria student criteria
     * @return map[stdOid] = Collection of actions
     */
    private Map<String, Collection<ConductAction>> loadExclusions(X2Criteria studentCriteria) {
        PlainDate termlyExclusionStartDate = null;
        PlainDate termlyExclusionEndDate = null;
        if (!TERM_PRU.equals(m_term)) {
            termlyExclusionStartDate = (PlainDate) getParameter(PARAM_TERMLY_EXCLUSION_START_DATE);
            termlyExclusionEndDate = (PlainDate) getParameter(PARAM_TERMLY_EXCLUSION_END_DATE);
        } else {
            String twoYearsAgo = "2012";
            String lastYear = "2013";
            try {
                if (getCurrentContext() != null) {
                    int schoolYear = getCurrentContext().getSchoolYear();
                    twoYearsAgo = String.valueOf(schoolYear - 2);
                    lastYear = String.valueOf(schoolYear - 1);
                }
                termlyExclusionStartDate = new PlainDate(m_dateFormat.parse(twoYearsAgo + "-09-01"));
                termlyExclusionEndDate = new PlainDate(m_dateFormat.parse(lastYear + "-08-31"));
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

        // find the incident code field, get the ref table, get non-empty state code. those are the
        // offensive codes
        DataDictionaryField incidentCodeField =
                getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        ReferenceTable incidentCodeRefTable = incidentCodeField.getReferenceTable();
        Map<String, ReferenceCode> incidentCodeRefCodeMap = incidentCodeRefTable.getCodeMap();
        HashSet<String> offensiveIncidentCodeSet = new HashSet<String>();
        for (Entry<String, ReferenceCode> entry : incidentCodeRefCodeMap.entrySet()) {
            ReferenceCode offensiveRefCode = entry.getValue();
            String offensiveStateCode = offensiveRefCode.getStateCode();
            if (!StringUtils.isEmpty(offensiveStateCode)) {
                String offensiveCode = entry.getKey();
                offensiveIncidentCodeSet.add(offensiveCode);
            }
        }

        X2Criteria incidentCriteria = new X2Criteria();
        incidentCriteria.addIn(ConductIncident.COL_STUDENT_OID, studentSubQuery);
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, termlyExclusionStartDate);
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, termlyExclusionEndDate);
        X2Criteria endDateCriteriaB = new X2Criteria();
        endDateCriteriaB.addIsNull(ConductIncident.COL_INCIDENT_DATE);
        endDateCriteria.addOrCriteria(endDateCriteriaB);
        incidentCriteria.addAndCriteria(endDateCriteria);
        incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, offensiveIncidentCodeSet);

        BeanQuery incidentQuery = new BeanQuery(ConductIncident.class, incidentCriteria);
        Map groupedCollectionByQuery =
                getBroker().getGroupedCollectionByQuery(incidentQuery, ConductIncident.COL_STUDENT_OID, 128);
        Map nullGroupedCollectionByQuery = new HashMap<String, Collection<Object>>();

        if (!TERM_PRU.equals(m_term)) {
            setGroupedCollectionToQuery(QUERY_TERMLY_EXCLUSIONS, groupedCollectionByQuery);
            setGroupedCollectionToQuery(QUERY_ANNUAL_EXCLUSIONS, nullGroupedCollectionByQuery);
        } else {
            setGroupedCollectionToQuery(QUERY_TERMLY_EXCLUSIONS, nullGroupedCollectionByQuery);
            setGroupedCollectionToQuery(QUERY_ANNUAL_EXCLUSIONS, groupedCollectionByQuery);
        }

        SubQuery conductSubquery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

        X2Criteria actionCriteria = new X2Criteria();
        actionCriteria.addIn(ConductAction.COL_INCIDENT_OID, conductSubquery);

        // Check for Conduct Action Exclusion Category field
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField exclusionCategoryField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUSION_CATEGORY);
        if (exclusionCategoryField != null) {
            String exclusionCategoryJavaName = exclusionCategoryField.getJavaName();
            actionCriteria.addNotEqualTo(exclusionCategoryJavaName, "N/A");
        }

        BeanQuery actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
        Map<String, Collection<ConductAction>> actionMap =
                getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_INCIDENT_OID, 128);

        m_actionStudentMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);

        return actionMap;
    }

    /**
     * Loads all 'free meals' programs, limiting the results to <tt>studentCriteria</tt>.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> loadFreeMeals(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

        X2Criteria freeMealsCriteria = new X2Criteria();
        freeMealsCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery); // reporting
                                                                                               // students
        freeMealsCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, STUDENT_PROGRAM_CODE_FSM); // meals

        if (!TERM_PRU.equals(m_term)) {
            // Valid FSM date scenarios
            X2Criteria scenariosCriteria = new X2Criteria();

            // Scenario 1
            X2Criteria scenario1Criteria = new X2Criteria();
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario1Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario1Criteria);

            // Scenario 2
            X2Criteria scenario2Criteria = new X2Criteria();
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario2Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario2Criteria);

            // Scenario 3
            X2Criteria scenario3Criteria = new X2Criteria();
            scenario3Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario3Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario3Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario3Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario3Criteria);

            // Scenario 4
            X2Criteria scenario4Criteria = new X2Criteria();
            scenario4Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario4Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario4Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario4Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario4Criteria);

            // Scenario 5
            X2Criteria scenario5Criteria = new X2Criteria();
            scenario5Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario5Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario5Criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            scenariosCriteria.addOrCriteria(scenario5Criteria);

            // Scenario 6
            X2Criteria scenario6Criteria = new X2Criteria();
            scenario6Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario6Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario6Criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            scenariosCriteria.addOrCriteria(scenario6Criteria);

            freeMealsCriteria.addAndCriteria(scenariosCriteria);
        } else {
            // Valid FSM date scenarios
            X2Criteria scenariosCriteria = new X2Criteria();

            // Scenario 1
            X2Criteria scenario1Criteria = new X2Criteria();
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario1Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario1Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario1Criteria);

            // Scenario 2
            X2Criteria scenario2Criteria = new X2Criteria();
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario2Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario2Criteria);

            // Scenario 3
            X2Criteria scenario3Criteria = new X2Criteria();
            scenario3Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario3Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario3Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario3Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario3Criteria);

            // Scenario 4
            X2Criteria scenario4Criteria = new X2Criteria();
            scenario4Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario4Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario4Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmStartDate);
            scenario4Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_fsmEndDate);
            scenariosCriteria.addOrCriteria(scenario4Criteria);

            // Scenario 5
            X2Criteria scenario5Criteria = new X2Criteria();
            scenario5Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario5Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario5Criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            scenariosCriteria.addOrCriteria(scenario5Criteria);

            // Scenario 6
            X2Criteria scenario6Criteria = new X2Criteria();
            scenario6Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmStartDate);
            scenario6Criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_fsmEndDate);
            scenario6Criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            scenariosCriteria.addOrCriteria(scenario6Criteria);

            freeMealsCriteria.addAndCriteria(scenariosCriteria);
        }

        // Valid Census Date criteria
        X2Criteria startCensusCriteria = new X2Criteria();
        startCensusCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_censusDate);
        // X2Criteria endCensusDateCriteria = new X2Criteria();
        // endCensusDateCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE,
        // m_censusDate);
        // X2Criteria endCensusDateNullCriteria = new X2Criteria();
        // endCensusDateNullCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
        // endCensusDateCriteria.addOrCriteria(endCensusDateNullCriteria);
        // startCensusCriteria.addAndCriteria(endCensusDateCriteria);

        freeMealsCriteria.addAndCriteria(startCensusCriteria);

        BeanQuery freeMealsQuery = new BeanQuery(StudentProgramParticipation.class, freeMealsCriteria);
        freeMealsQuery.addOrderByAscending(StudentProgramParticipation.COL_START_DATE); // most
                                                                                        // recent
        freeMealsQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);

        Map<String, Collection<StudentProgramParticipation>> fsmMap =
                getBroker().getGroupedCollectionByQuery(freeMealsQuery, StudentProgramParticipation.COL_STUDENT_OID,
                        64);
        setGroupedCollectionToQuery(QUERY_FSM, fsmMap);

        return fsmMap;
    }

    /**
     * Loads all learner support codes into a grouped collection map.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> loadLearnerSupport(X2Criteria studentCriteria) {
        Map<String, Collection<StudentProgramParticipation>> learnerSupportItems =
                new HashMap<String, Collection<StudentProgramParticipation>>();

        X2Criteria criteria = null;
        // X2Criteria orCriteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addIn(ReferenceCode.COL_LOCAL_CODE, Arrays.asList("55", "56"));

            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            // Student subquery for active students this year.
            SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_censusDate);

            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_termlyAttendanceStartDate);
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);

            criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                    Student.COL_SCHOOL_OID, getSchool().getOid());

            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
            query.addOrderByAscending(StudentProgramParticipation.COL_STUDENT_OID);

            learnerSupportItems =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);

            setGroupedCollectionToQuery(QUERY_LEARNER_SUPPORT_ITEMS, learnerSupportItems);
        }

        return learnerSupportItems;
    }

    /**
     * Loads all leaning Aims into a grouped collection map.
     */
    /*
     * private Map<String, Collection<ExamEntry>> loadLearningAims(X2Criteria studentCriteria)
     * {
     * DataDictionaryField gradeLevelField = getDataDictionaryField(SisStudent.class,
     * SisStudent.COL_GRADE_LEVEL);
     * ReferenceTable gradeLevelReferenceTable = gradeLevelField.getReferenceTable();
     * Map<String, ReferenceCode> gradeLevelReferenceCodeMap =
     * gradeLevelReferenceTable.getCodeMap();
     * Set<String> gradeLevelsGreaterThanYr12 = new HashSet<String>();
     * Map<String, Integer> gradeNumMap = new HashMap<String, Integer>();
     * for (Entry<String, ReferenceCode> gradeLevelReferenceCodeEntry :
     * gradeLevelReferenceCodeMap.entrySet())
     * {
     * String gradeLevel = gradeLevelReferenceCodeEntry.getKey();
     * ReferenceCode gradeLevelReferenceCode = gradeLevelReferenceCodeEntry.getValue();
     * String numericLevel = gradeLevelReferenceCode.getFieldA005();
     * 
     * if (!StringUtils.isEmpty(numericLevel))
     * {
     * gradeNumMap.put(gradeLevel, Integer.valueOf(numericLevel));
     * }
     * 
     * if (!StringUtils.isEmpty(numericLevel) &&
     * StringUtils.isNumeric(numericLevel) &&
     * Integer.parseInt(numericLevel) >= 12)
     * {
     * gradeLevelsGreaterThanYr12.add(gradeLevel);
     * }
     * }
     * 
     * X2Criteria stdCriteria = studentCriteria.copy();
     * stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, gradeLevelsGreaterThanYr12);
     * SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);
     * 
     * GradeLevelHistory glh = new GradeLevelHistory(studentSubQuery, 10, getOrganization(),
     * getBroker());
     * 
     * String qualStatusFieldAsString = translateAliasToJavaName("DFE QUAL STATUS", true);
     * DataDictionaryField qualStatusField = getDataDictionaryField(ExamEntry.class,
     * qualStatusFieldAsString);
     * ReferenceTable qualStatusRefTable = qualStatusField.getReferenceTable();
     * Map<String, ReferenceCode> qualStatusCodeMap = qualStatusRefTable.getCodeMap();
     * Collection<ReferenceCode> qualStatuses = qualStatusCodeMap.values();
     * String errorCode = "Entered in error";
     * for (ReferenceCode qualStatus : qualStatuses)
     * {
     * String stateCode = qualStatus.getStateCode();
     * if ("X".equals(stateCode))
     * {
     * errorCode = qualStatus.getCode();
     * break;
     * }
     * }
     * 
     * int currentYear = getOrganization().getCurrentContext().getSchoolYear();
     * 
     * String examSeasonOid = (String) getParameter("examSeason");
     * Set<String> seasonOids = new HashSet(StringUtils.convertDelimitedStringToList(examSeasonOid,
     * ','));
     * X2Criteria entCriteria = new X2Criteria();
     * entCriteria.addIn(ExamEntry.COL_STUDENT_OID, studentSubQuery);
     * entCriteria.addNotEqualTo(qualStatusFieldAsString, errorCode);
     * entCriteria.addGreaterOrEqualThan(ExamEntry.REL_OPTION + PATH_DELIMITER +
     * ExamOption.REL_SERIES + PATH_DELIMITER +
     * ExamSeries.REL_SEASON + PATH_DELIMITER +
     * ExamSeason.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
     * DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(currentYear - 1));
     * BeanQuery entQuery = new BeanQuery(ExamEntry.class, entCriteria);
     * 
     * 
     * Map<String, Collection<ExamEntry>> stdEntMap = new HashMap<String, Collection<ExamEntry>>();
     * Collection<ExamEntry> examEntries = getBroker().getCollectionByQuery(entQuery);
     * for (ExamEntry examEntry : examEntries)
     * {
     * try
     * {
     * SisStudent student = examEntry.getStudent();
     * String studentOid = student.getOid();
     * String gradeLevel = student.getGradeLevel();
     * ExamOption option = examEntry.getOption();
     * ExamSeries series = option.getSeries();
     * ExamSeason season = series.getSeason();
     * SisDistrictSchoolYearContext districtContext = season.getDistrictContext();
     * int examYear = districtContext.getSchoolYear();
     * 
     * if (examYear <= currentYear)
     * {
     * if (!stdEntMap.containsKey(studentOid))
     * {
     * stdEntMap.put(studentOid, new LinkedList<ExamEntry>());
     * }
     * 
     * if ((seasonOids.isEmpty() && examYear == currentYear) ||
     * (!seasonOids.isEmpty() && seasonOids.contains(season.getOid()) && examYear == currentYear))
     * {
     * // "exam entry is this year" OR
     * // "if a season was specified and "exam entry is this year""
     * stdEntMap.get(studentOid).add(examEntry);
     * }
     * else if (gradeNumMap.get(gradeLevel).intValue() > 12 &&
     * examYear == (currentYear - 1))
     * {
     * // "student is greater than year 12 and exam is from last year"
     * stdEntMap.get(studentOid).add(examEntry);
     * }
     * else if (gradeLevelsGreaterThanYr12.contains(glh.getGradeLevel(studentOid, examYear)) &&
     * examYear == (currentYear - 1))
     * {
     * // "student was greater than yr12 at exam date" and
     * // "exam year was last year"
     * stdEntMap.get(studentOid).add(examEntry);
     * }
     * }
     * }
     * catch (NullPointerException npe)
     * {
     * // TODO: figure it out later.
     * }
     * }
     * 
     * setGroupedCollectionToQuery("learningAims", stdEntMap);
     * return stdEntMap;
     * }
     */

    /*
     * private Map<String, Collection<StudentSchedule>> loadLearningAims(X2Criteria studentCriteria,
     * HashSet activeScheduleOids)
     * {
     * DataDictionaryField gradeLevelField = getDataDictionaryField(SisStudent.class,
     * SisStudent.COL_GRADE_LEVEL);
     * ReferenceTable gradeLevelReferenceTable = gradeLevelField.getReferenceTable();
     * Map<String, ReferenceCode> gradeLevelReferenceCodeMap =
     * gradeLevelReferenceTable.getCodeMap();
     * Set<String> gradeLevelsGreaterThanYr12 = new HashSet<String>();
     * Map<String, Integer> gradeNumMap = new HashMap<String, Integer>();
     * for (Entry<String, ReferenceCode> gradeLevelReferenceCodeEntry :
     * gradeLevelReferenceCodeMap.entrySet())
     * {
     * String gradeLevel = gradeLevelReferenceCodeEntry.getKey();
     * ReferenceCode gradeLevelReferenceCode = gradeLevelReferenceCodeEntry.getValue();
     * String numericLevel = gradeLevelReferenceCode.getFieldA005();
     * 
     * if (!StringUtils.isEmpty(numericLevel))
     * {
     * gradeNumMap.put(gradeLevel, Integer.valueOf(numericLevel));
     * }
     * 
     * if (!StringUtils.isEmpty(numericLevel) &&
     * StringUtils.isNumeric(numericLevel) &&
     * Integer.parseInt(numericLevel) >= 11)
     * {
     * gradeLevelsGreaterThanYr12.add(gradeLevel);
     * }
     * }
     * 
     * X2Criteria stdCriteria = studentCriteria.copy();
     * stdCriteria.addIn(SisStudent.COL_GRADE_LEVEL, gradeLevelsGreaterThanYr12);
     * SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);
     * 
     * //Get a map of StudentSchedules with a QAN Code by StudentOid
     * X2Criteria studentScheduleCriteria = new X2Criteria();
     * studentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);
     * studentScheduleCriteria.addIn(StudentSchedule.COL_SCHEDULE_OID, activeScheduleOids);
     * studentScheduleCriteria.addNotNull(StudentSchedule.COL_QAN_CODE_OID);
     * QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class,
     * studentScheduleCriteria);
     * Map<String, Collection<StudentSchedule>> studentScheduleMap =
     * getBroker().getGroupedCollectionByQuery(studentScheduleQuery,
     * StudentSchedule.COL_STUDENT_OID, 128);
     * 
     * setGroupedCollectionToQuery("learningAims", studentScheduleMap);
     * return studentScheduleMap;
     * }
     */
    /**
     * Load the staff hours worked counts
     */
    private void loadPartTimeStaffHours() {
        Criteria criteria = new Criteria();
        SubQuery staffQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, getStaffCriteria());
        criteria.addIn(StaffPosition.COL_STAFF_OID, staffQuery);
        criteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_censusDate);

        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
        query.addOrderByAscending(StaffPosition.REL_STAFF + PATH_DELIMITER + SisStaff.COL_STAFF_TYPE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        double totalPartTimeHours = 0;
        double totalSupportHours = 0;
        String lastStaffType = null;
        int hours = 0;
        try {
            while (iterator.hasNext()) {
                StaffPosition staffPosition = (StaffPosition) iterator.next();
                PlainDate endDate = staffPosition.getEndDate();
                SisStaff staff = staffPosition.getStaff();
                String staffType = staff.getStaffType();
                String tenure = (String) staff.getFieldValueByAlias(ALIAS_TENURE);
                String hoursPerWeekString = (String) staffPosition.getFieldValueByAlias(ALIAS_HOURS_PER_WEEK);
                double hoursPerWeek = 0;

                if (StringUtils.isNumeric(hoursPerWeekString)) {
                    hoursPerWeek = Double.valueOf(hoursPerWeekString).doubleValue();
                }

                if (!StringUtils.isEmpty(lastStaffType) && !lastStaffType.equals(staffType)) {
                    if (m_teacherCategoryCodes.contains(lastStaffType)) {
                        totalPartTimeHours = Math.round(totalPartTimeHours);
                        hours = (int) totalPartTimeHours;
                        m_partTimeTeacherHours.put(lastStaffType, String.valueOf(hours));

                        totalPartTimeHours = 0;
                    }
                    if (m_supportCategoryCodes.contains(lastStaffType)) {
                        totalSupportHours = Math.round(totalSupportHours);
                        hours = (int) totalSupportHours;
                        m_supportStaffHours.put(lastStaffType, String.valueOf(hours));

                        totalSupportHours = 0;
                    }
                }

                PlainDate today = new PlainDate();
                if (endDate == null || endDate.after(today)) {
                    if (!StringUtils.isEmpty(staffType) && m_teacherCategoryCodes.contains(staffType)
                            && "Part Time".equals(tenure)) {
                        totalPartTimeHours += hoursPerWeek;
                    }

                    if (!StringUtils.isEmpty(staffType) && m_supportCategoryCodes.contains(staffType)) {
                        totalSupportHours += hoursPerWeek;
                    }
                }

                lastStaffType = staffType;
            }
        } finally {
            iterator.close();
        }

        if (!StringUtils.isEmpty(lastStaffType)) {
            if (m_teacherCategoryCodes.contains(lastStaffType)) {
                totalPartTimeHours = Math.round(totalPartTimeHours);
                hours = (int) totalPartTimeHours;
                m_partTimeTeacherHours.put(lastStaffType, String.valueOf(hours));
            }
            if (m_supportCategoryCodes.contains(lastStaffType)) {
                totalSupportHours = Math.round(totalSupportHours);
                hours = (int) totalSupportHours;
                m_supportStaffHours.put(lastStaffType, String.valueOf(hours));
            }
        }
    }

    /**
     * Load pupils on roll and pupils no longer on roll
     * <p>
     * Pupils are considered on roll if their most recent student enrollment has an status code.
     *
     * @param students Collection<SisStudent>
     */
    private void loadPupils(Collection<SisStudent> students) {
        Collection<X2BaseBean> pupilsOnRoll = new ArrayList<X2BaseBean>();
        Collection<X2BaseBean> pupilsNoLongerOnRoll = new ArrayList<X2BaseBean>();
        for (SisStudent student : students) {
            StudentEnrollment recentEnrollment = m_helper.getEnrollmentForDate(student.getOid(),
                    m_censusDate,
                    "EWS");

            String studentOid = student.getOid();

            if (recentEnrollment != null) {
                String statusCode = recentEnrollment.getStatusCode();
                if (StudentManager.isActiveStudent(getOrganization(), statusCode)) {
                    pupilsOnRoll.add(student);
                } else {
                    if (m_attendanceStudentOids.contains(studentOid) || m_conductActionStudentOids.contains(studentOid)
                            || m_learnerSupportItemStudentOids.contains(studentOid)) {
                        pupilsNoLongerOnRoll.add(student);
                    }
                }
            }

        }

        setCollectionToQuery(QUERY_PUPILS_ON_ROLL, pupilsOnRoll);
        setCollectionToQuery(QUERY_PUPILS_NO_LONGER_ON_ROLL, pupilsNoLongerOnRoll);
    }

    /**
     * Gets a list of the all classes occurring the selected period time.
     *
     * @param studentCriteria Criteria
     * @return List of MasterScheduleOids
     */
    private void loadSectionList(Criteria studentCriteria) {
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        m_sectionOids = new ArrayList<String>();
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                String studentOid = student.getOid();
                Collection<MasterSchedule> sections = m_rawStudentSchedulesMap.get(studentOid);

                if (sections != null) {
                    for (MasterSchedule section : sections) {
                        if (!m_sectionOids.contains(section.getOid())) {
                            m_sectionOids.add(section.getOid());
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads special education needs, limiting the results to <tt>studentCriteria</tt>.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> loadSens(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria.copy());

        String senProvisionBeanPath = translateAliasToJavaName(ALIAS_SEN_PROVISION, true);
        DataDictionaryField senProvisionField =
                getDataDictionaryField(StudentProgramParticipation.class, senProvisionBeanPath);
        List<String> senCodes = new ArrayList<String>();
        if (senProvisionField != null && !StringUtils.isEmpty(senProvisionField.getReferenceTableOid())) {
            Map<String, ReferenceCode> senCodesMap = getReferenceCodes(senProvisionField.getReferenceTableOid());
            for (ReferenceCode referenceCode : senCodesMap.values()) {
                String code = referenceCode.getCode();
                String stateCode = referenceCode.getStateCode();
                if (!"N".equals(stateCode)) {
                    senCodes.add(code);
                }
            }
        }

        X2Criteria senCriteria = new X2Criteria();
        senCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        senCriteria.addIn(senProvisionBeanPath, senCodes);
        senCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, STUDENT_PROGRAM_CODE_SE);
        BeanQuery senQuery = new BeanQuery(StudentProgramParticipation.class, senCriteria);
        Map<String, Collection<StudentProgramParticipation>> senMap =
                getBroker().getGroupedCollectionByQuery(senQuery, IepData.COL_STUDENT_OID, 128);

        setGroupedCollectionToQuery(QUERY_SEN_NEEDS, senMap);

        return senMap;
    }

    /**
     * Creates the Staff counts for Teachers/Support Staff.
     */
    private void loadStaffCounts() {
        QueryByCriteria query = new QueryByCriteria(SisStaff.class, getStaffCriteria());
        query.addOrderByAscending(Staff.COL_STAFF_TYPE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        int fullTimeTeacherCount = 0;
        int partTimeTeacherCount = 0;
        int fullTimeSupportCount = 0;
        int partTimeSupportCount = 0;
        int headCount = 0;
        String lastStaffType = null;
        try {
            while (iterator.hasNext()) {
                Staff staff = (Staff) iterator.next();
                String staffType = staff.getStaffType();
                String tenure = (String) staff.getFieldValueByAlias(ALIAS_TENURE);

                if (lastStaffType != null && !staffType.equals(lastStaffType)) {
                    if (m_teacherCategoryCodes.contains(lastStaffType)) {
                        m_fullTimeTeacher.put(lastStaffType, String.valueOf(fullTimeTeacherCount));
                        m_partTimeTeacher.put(lastStaffType, String.valueOf(partTimeTeacherCount));

                        fullTimeTeacherCount = 0;
                        partTimeTeacherCount = 0;
                    }

                    if (m_supportCategoryCodes.contains(lastStaffType)) {
                        m_headCountSupport.put(lastStaffType, String.valueOf(headCount));

                        headCount = 0;
                    }
                }

                if ("Full Time".equals(tenure)) {
                    if (m_teacherCategoryCodes.contains(staffType)) {
                        fullTimeTeacherCount++;
                    }
                    if (m_rawTeacherSchedulesMap.containsKey(staff.getOid())
                            && m_supportCategoryCodes.contains(staffType)) {
                        fullTimeSupportCount++;
                        headCount++;
                    }
                }
                if ("Part Time".equals(tenure)) {
                    if (m_teacherCategoryCodes.contains(staffType)) {
                        partTimeTeacherCount++;
                    }
                    if (m_rawTeacherSchedulesMap.containsKey(staff.getOid())
                            && m_supportCategoryCodes.contains(staffType)) {
                        partTimeSupportCount++;
                        headCount++;
                    }
                }

                lastStaffType = staffType;
            }
        } finally {
            iterator.close();
        }

        if (lastStaffType != null) {
            if (m_teacherCategoryCodes.contains(lastStaffType)) {
                m_fullTimeTeacher.put(lastStaffType, String.valueOf(fullTimeTeacherCount));
                m_partTimeTeacher.put(lastStaffType, String.valueOf(partTimeTeacherCount));

                fullTimeTeacherCount = 0;
                partTimeTeacherCount = 0;
            }

            m_fullTimeSupport = String.valueOf(fullTimeSupportCount);
            m_partTimeSupport = String.valueOf(partTimeSupportCount);

            if (m_supportCategoryCodes.contains(lastStaffType)) {
                m_headCountSupport.put(lastStaffType, String.valueOf(headCount));
            }
        }
    }

    /**
     * Creates a <CategoryDetails> Element and <SupportStaff> Element.
     *
     * @return <CategoryDetails>
     */
    private void loadStaffElements() {
        String[] columns = {Staff.COL_STAFF_TYPE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(Staff.class, columns, getStaffCriteria());
        query.addGroupBy(Staff.COL_STAFF_TYPE);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        boolean setSupportStaff = false;
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String staffType = (String) row[0];
                String fullTimeStaffCount = STRING_EMPTY;
                String partTimeStaffCount = STRING_EMPTY;
                String partTimeStaffHoursCount = STRING_EMPTY;
                String headCountCount = STRING_EMPTY;
                String supportStaffHours = STRING_EMPTY;

                Element censusTeacherCategory = new Element(ELEMENT_CENSUS_TEACHER_CATEGORY);
                Element gender = new Element(ELEMENT_GENDER);
                Element fullTimeStaff = new Element(ELEMENT_FULL_TIME);
                Element partTimeStaff = new Element(ELEMENT_PART_TIME);
                Element partTimeStaffHours = new Element(ELEMENT_PART_TIME_HOURS);
                Element supportCategory = new Element(ELEMENT_SUPPORT_CATEGORY);
                Element headCount = new Element(ELEMENT_HEAD_COUNT);
                Element hours = new Element(ELEMENT_HOURS);

                if (m_teacherCategoryCodes.contains(staffType)) {
                    if (m_fullTimeTeacher.containsKey(staffType)) {
                        fullTimeStaffCount = m_fullTimeTeacher.get(staffType);
                    } else {
                        fullTimeStaffCount = STRING_ZERO;
                    }

                    if (m_partTimeTeacher.containsKey(staffType)) {
                        partTimeStaffCount = m_partTimeTeacher.get(staffType);
                    } else {
                        partTimeStaffCount = STRING_ZERO;
                    }

                    if (m_partTimeTeacherHours.containsKey(staffType)) {
                        partTimeStaffHoursCount = m_partTimeTeacherHours.get(staffType);
                    } else {
                        partTimeStaffHoursCount = STRING_ZERO;
                    }

                    censusTeacherCategory.setText(staffType);
                    gender.setText("F");
                    fullTimeStaff.setText(fullTimeStaffCount);
                    partTimeStaff.setText(partTimeStaffCount);
                    partTimeStaffHours.setText(partTimeStaffHoursCount);
                    m_teacherCategoryDetails.addContent(censusTeacherCategory);
                    m_teacherCategoryDetails.addContent(gender); // TODO Find out what Gender code
                                                                 // means in this instance
                    m_teacherCategoryDetails.addContent(fullTimeStaff);
                    m_teacherCategoryDetails.addContent(partTimeStaff);
                    m_teacherCategoryDetails.addContent(partTimeStaffHours);
                }
                if (m_supportCategoryCodes.contains(staffType)) {
                    if (!StringUtils.isNumeric(m_fullTimeSupport)) {
                        fullTimeStaffCount = m_fullTimeSupport;
                    } else {
                        fullTimeStaffCount = STRING_ZERO;
                    }

                    if (!StringUtils.isNumeric(m_partTimeSupport)) {
                        partTimeStaffCount = m_partTimeSupport;
                    } else {
                        partTimeStaffCount = STRING_ZERO;
                    }

                    if (m_headCountSupport.containsKey(staffType)) {
                        headCountCount = m_headCountSupport.get(staffType);
                    } else {
                        headCountCount = STRING_ZERO;
                    }

                    if (m_supportStaffHours.containsKey(staffType)) {
                        supportStaffHours = m_supportStaffHours.get(staffType);
                    } else {
                        supportStaffHours = STRING_ZERO;
                    }

                    supportCategory.setText(staffType);
                    headCount.setText(headCountCount);
                    hours.setText(supportStaffHours);
                    m_supportCategoryDetails.addContent(supportCategory);
                    m_supportCategoryDetails.addContent(headCount);
                    m_supportCategoryDetails.addContent(hours);

                    if (!setSupportStaff) {
                        fullTimeStaff.setText(fullTimeStaffCount);
                        partTimeStaff.setText(partTimeStaffCount);
                        m_supportStaff.addContent(fullTimeStaff);
                        m_supportStaff.addContent(partTimeStaff);
                        setSupportStaff = true;
                    }
                }

            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Since the &lt;Disabilities&gt; tag is required (where if students who don't have a
     * disability,
     * there should be a placeholder &lt;Disability&gt;None&lt;/Disability&gt; element.
     * <p>
     * Therefore... </br>
     * Give students who don't have disabilities a "None" disability with no type
     * so that the export format can default it to "NONE".
     *
     * @param students Collection<SisStudent>
     * @param idbMap Map<String,Collection<IepDisability>>
     */
    private void updateIdbMap(Collection<SisStudent> students,
                              Map<String, Collection<IepDisability>> idbMap) {
        Collection<IepDisability> idbs = new ArrayList<IepDisability>(1);
        IepDisability emptyIdb = X2BaseBean.newInstance(IepDisability.class, getBroker().getPersistenceKey());
        emptyIdb.setDisabilityCode("None");
        idbs.add(emptyIdb);
        for (SisStudent student : students) {
            if (!idbMap.containsKey(student.getOid())) {
                idbMap.put(student.getOid(), idbs);
            }
        }
    }

    /**
     * Remove certain elements from the template depending on the <tt>term</tt>
     * and the <tt>schoolPhase</tt>.
     *
     * @param term String
     * @param schoolPhase String
     */
    private void setConditionalElements(String term, String schoolPhase) {
        /*
         * School XML Structure
         */
        if (TERM_PRU.equals(term) || !(TERM_SPRING.equals(term) && SCHOOL_SPECIAL.equals(schoolPhase))) {
            removeElement(ELEMENT_SPECIAL_SCHOOL);
        }
        if (TERM_SUMMER.equals(term) || TERM_AUTUMN.equals(term)) {
            removeElement(ELEMENT_SCHOOL_LOCATION);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term) || (SCHOOL_NURSERY.equals(schoolPhase) ||
                SCHOOL_SPECIAL.equals(schoolPhase))) {
            removeElement(ELEMENT_ADMISSION_APPEALS);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term) || !SCHOOL_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_INFANT_ADMISSIONS_APPEALS);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term) || (SCHOOL_NURSERY.equals(schoolPhase) ||
                SCHOOL_SPECIAL.equals(schoolPhase))) {
            removeElement(ELEMENT_RECONCILIATION);
        }
        if (!TERM_SPRING.equals(term) || !(SCHOOL_MIDDLE_SECONDARY.equals(schoolPhase) ||
                SCHOOL_SECONDARY.equals(schoolPhase))) {
            removeElement(ELEMENT_WORK_EXPERIENCE);
            removeElement(ELEMENT_FE_COLLEGE);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term) || (SCHOOL_NURSERY.equals(schoolPhase) ||
                SCHOOL_SPECIAL.equals(schoolPhase))) {
            removeElement(ELEMENT_CLASSES);
        }
        if (!TERM_SPRING.equals(term) || !SCHOOL_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_CLASS_TYPE);
            removeElement(ELEMENT_KEY_STAGE);
        }
        if (!TERM_SPRING.equals(term) || !(SCHOOL_PRIMARY.equals(schoolPhase) ||
                SCHOOL_MIDDLE_PRIMARY.equals(schoolPhase))) {
            removeElement(ELEMENT_ASC_ACTIVITY);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term)) {
            removeElement(ELEMENT_MISCELLANEOUS);
        }

        /*
         * Pupil XML Structure
         */
        if (SCHOOL_NURSERY.equals(schoolPhase) ||
                SCHOOL_PRIMARY.equals(schoolPhase) ||
                SCHOOL_MIDDLE_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_ULN);
        }
        if ((!TERM_PRU.equals(term) && !TERM_SPRING.equals(term))
                || (SCHOOL_NURSERY.equals(schoolPhase) && !TERM_PRU.equals(term))) {
            removeElement(ELEMENT_ETHNICITY);
        }
        if (!TERM_SPRING.equals(term)) {
            removeElement(ELEMENT_DISABILITIES);
        }
        if (SCHOOL_NURSERY.equals(schoolPhase) || SCHOOL_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_CONNEXIONS);
            removeElement(ELEMENT_YSSA);
        }
        if (SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_LANGUAGE);
        }
        if (!TERM_PRU.equals(term) && !TERM_SPRING.equals(term)) {
            removeElement(ELEMENT_SERVICE_CHILD);
        }
        if (TERM_AUTUMN.equals(term) || SCHOOL_NURSERY.equals(schoolPhase)
                || SCHOOL_PRIMARY.equals(schoolPhase) || SCHOOL_MIDDLE_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_LEARNER_SUPPORT);
            removeElement(ELEMENT_LEARNER_SUPPORT_NO_LONGER);
        }
        if (TERM_PRU.equals(term) || !SCHOOL_PRIMARY.equals(schoolPhase)) {
            removeElement(ELEMENT_TYPE_OF_CLASS);
        }
        if (!TERM_SPRING.equals(term)) {
            removeElement(ELEMENT_SEN_NEEDS);
        }
        if (TERM_PRU.equals(term) || !TERM_SPRING.equals(term) || SCHOOL_SPECIAL.equals(schoolPhase)) {
            removeElement(ELEMENT_SEN_UNIT_INDICATOR);
            removeElement(ELEMENT_RESOURCED_PROVISION_INDICATOR);
        }
        if (!TERM_PRU.equals(term) && SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_EXCLUSIONS);
            removeElement(ELEMENT_ATTENDANCE);
            removeElement(ELEMENT_NO_LONGER_ATTENDANCE);
        }
        if (TERM_PRU.equals(term) || SCHOOL_SPECIAL.equals(schoolPhase) || SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_TERMLY_ATTENDANCE);
        }
        if (TERM_PRU.equals(term) || !TERM_AUTUMN.equals(term) || SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_SUMMER_HALF_TERM2_ATTENDANCE);
            removeElement(ELEMENT_NO_LONGER_SUMMER_HALF_TERM2_ATTENDANCE);
        }
        if (!TERM_PRU.equals(term) && (!TERM_AUTUMN.equals(term) || !SCHOOL_SPECIAL.equals(schoolPhase))) {
            removeElement(ELEMENT_ANNUAL_ATTENDANCE);
            removeElement(ELEMENT_NO_LONGER_ANNUAL_ATTENDANCE);
        }
        /*
         * if (TERM_PRU.equals(term)|| !TERM_AUTUMN.equals(term) ||
         * !(SCHOOL_MIDDLE_SECONDARY.equals(schoolPhase) ||
         * SCHOOL_SECONDARY.equals(schoolPhase)))
         * {
         * removeElement(ELEMENT_LEARNING_AIMS);
         * }
         */
        if (SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_PUPILS_NO_LONGER_ON_ROLL);
        }
        if (TERM_PRU.equals(term) || !TERM_AUTUMN.equals(term) || !(SCHOOL_MIDDLE_SECONDARY.equals(schoolPhase) ||
                SCHOOL_SECONDARY.equals(schoolPhase))) {
            removeElement(ELEMENT_NC_YEAR_LEAVING);
        }
        if (TERM_PRU.equals(term) || !TERM_AUTUMN.equals(term) || !(SCHOOL_MIDDLE_SECONDARY.equals(schoolPhase) ||
                SCHOOL_SECONDARY.equals(schoolPhase))) {
            removeElement(ELEMENT_PUPIL_NO_LONGER_ON_ROLL_HOME_INFORMATION);
        }
        if (TERM_PRU.equals(term) || SCHOOL_NURSERY.equals(schoolPhase)) {
            removeElement(ELEMENT_TERMLY_EXCLUSIONS);
            removeElement(ELEMENT_NO_LONGER_TERMLY_EXCLUSIONS);
        }
        if (!TERM_PRU.equals(term)) {
            removeElement(ELEMENT_ANNUAL_EXCLUSIONS);
            removeElement(ELEMENT_NO_LONGER_ANNUAL_EXCLUSIONS);
            removeElement(ELEMENT_UNIT_CONTACT_TIME_PUPIL);
            removeElement(ELEMENT_STAFF_INFORMATION);
            removeElement(ELEMENT_CHILD_MOTHERS);
            removeElement(ELEMENT_TEEN_MOTHER_PLACES);
            removeElement(ELEMENT_CHILD_CARE_PLACES);
        }
        if (TERM_PRU.equals(term)) {
            removeElement(ELEMENT_NO_LONGER_TERMLY_ATTENDANCE);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        Element rootElement = getRootElement();
        List<Element> elementChildren = rootElement.getChildren();
        for (Element element : elementChildren) {
            System.out.println(element.getName());
        }
        return super.postProcess();
    }
}
