/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health.ma;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the MA CSHS Annual Status Report.
 *
 * @author X2 Development Corporation
 */
public class MACSHSStatusReportData extends ReportJavaSourceNet {
    /**
     * State Report Data class for gathering data, using StudentHistoryHelper, calculating
     * enrollment history.
     * This export should report a row for each student/school combination.
     * It should report positive attendance, and report positive codes for each day in membership.
     * Days not in membership should be reported as empty.
     *
     * @author Follett Development Corporation
     */
    class AttendanceStatistics extends StateReportData {

        /*
         * Instance variables.
         */
        private StudentHistoryHelper m_helper;
        private Map<String, ReferenceCode> m_raceCodes;
        private Map<String, String> m_raceMap;
        private Collection<SisStudent> m_students;
        private Collection<String> m_studentsOids;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_dateStart);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_dateEnd);
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STUDENT_ACTIVE_CODE);
            m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            if (isSchoolContext()) {
                m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }
            setQuery(m_helper.getStudentQuery(true));
            DataDictionaryField raceCodeField = getDataDictionaryField(Race.class, Race.COL_RACE_CODE);
            if (raceCodeField != null && !StringUtils.isEmpty(raceCodeField.getReferenceTableOid())) {
                m_raceCodes = getReferenceCodes(raceCodeField.getReferenceTableOid());
            }
        }

        /**
         * Retrieve race for given student.
         *
         * @param stdOid String
         * @return race
         */
        protected String getRace(String stdOid) {
            if (m_raceMap == null) {
                m_raceMap = new HashMap<>();
                for (SisStudent std : m_students) {
                    String raceCode = "";
                    if (std.getPerson().getHispanicLatinoIndicator()) {
                        raceCode = "11";
                    } else {
                        Collection<Race> races = m_helper.getRaces(std);
                        if (races != null) {
                            if (races.size() > 1) {
                                raceCode = "17";
                            } else {
                                for (Race race : races) {
                                    ReferenceCode refCode = m_raceCodes.get(race.getRaceCode());
                                    raceCode = refCode.getFederalCode() != null ? refCode.getFederalCode() : "";
                                }
                            }
                        }
                    }
                    if (!StringUtils.isEmpty(raceCode)) {
                        m_raceMap.put(std.getOid(), raceCode);
                    }
                }
            }
            return m_raceMap.get(stdOid);
        }

        /**
         * Return collection of students.
         *
         * @return m_students
         */
        protected Collection<SisStudent> getStudents() {
            if (m_students == null) {
                m_students = getBroker().getCollectionByQuery(m_helper.getStudentQuery(true));
            }
            return m_students;
        }

        protected Collection<String> getStudentsOids() {
            if (m_studentsOids == null) {
                m_studentsOids = getBroker()
                        .getGroupedCollectionByQuery(m_helper.getStudentQuery(true), X2BaseBean.COL_OID, 1024).keySet();
            }
            return m_studentsOids;
        }
    }

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Common report constants
     */
    private static final String DB_TRUE = "1";
    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";
    private static final String REPORT_FIELD_IN_504 = "in_504_stds";
    private static final String REPORT_FIELD_SPECIAl_CARE = "special_health_care_stds";
    private static final int PAGES = 16;
    private static final String SUBREPORT_ID_PREFIX = "SYS-HTH-MA-CSHS-SB";

    /**
     * Input parameters.
     */
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";

    /**
     * Report params
     */
    private static final String REPORT_PARAM_CURRENT_SCHOOL_YEAR = "currentSchoolYear";
    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_PERSON_RUN_REPORT_NAME = "personRunReportName";
    private static final String REPORT_PARAM_START_DATE = "startDate";

    /**
     * Dictionaries IDs.
     */
    private static final String DDX_HSC_GENERAL_OID = "ddxHscGeneral";
    private static final String DDX_HSC_HEARING_OID = "ddxHscHearing";
    private static final String DDX_HSC_PHYSICAL_OID = "ddxHscPhysical";
    private static final String DDX_HSC_POSTURAL_OID = "ddxHscPostural";
    private static final String DDX_HSC_VISION_OID = "ddxHscVision";
    /**
     * Aliases
     */
    private static final String ALIAS_GENERAL_BMI_PERCENT = "hsc-general-bmi-percent";
    private static final String ALIAS_GENERAL_BMI_OPTION = "hsc-general-bmi-opt";
    private static final String ALIAS_GENERAL_IS_RESCREENING = "hsc-general-rescreening";
    private static final String ALIAS_GENERAL_IS_REFERRAL_COMPLETE = "hsc-general-referral-complete";
    private static final String ALIAS_REPORT_LOCATION = "report-location";
    private static final String ALIAS_STD_DOE_39_IN_504_PLAN = "DOE 39";
    private static final String ALIAS_STD_ELL = "DOE 26";
    private static final String ALIAS_STD_HOMELESS = "SIF STD Homeless";
    private static final String ALIAS_STD_INSURANCE_TYPE = "all-std-hcInsType";

    /**
     * RTB Oids
     */
    private static final String RTB_HEALTH_MED_TYPE = "rtbHthMedType";
    private static final String RTB_HEALTH_VISIT_COMPLIANTSS = "rtbHthCompCds";
    private static final String RTB_HEALTH_VISIT_TREATMENTS = "rtbHthTreatCds";
    private static final String RTB_MED_ACT = "rtbHthActCds";
    private static final String RTB_MEDICAL_CONDITIONS = "rtbConditions";
    private static final String RTB_CONDITION_TYPES = "rtbCndtnTypes";
    private static final String RTB_CONDITION_HLG_COMPLIANTS = "rtbHthCompCds";

    /**
     * Grades
     */
    private static final String GRADE_PREFIX = "G";
    private static final String GRADE_LEVEL_01 = "01";
    private static final String GRADE_LEVEL_04 = "04";
    private static final String GRADE_LEVEL_07 = "07";
    private static final String GRADE_LEVEL_10 = "10";
    private static final String GRADE_LEVEL_KF = "KF";
    private static final String GRADE_LEVEL_KP = "KP";
    private static final String GRADE_LEVEL_KT = "KT";
    private static final String GENDER_MALE = "M";
    private static final String GENDER_FEMALE = "F";

    /**
     * BMI Categories.
     */
    private static final String BMI_CATEGORY_NORMAL = "N";
    private static final String BMI_CATEGORY_OBESE = "B";
    private static final String BMI_CATEGORY_OVERWEIGHT = "O";
    private static final String BMI_CATEGORY_UNDERWEIGHT = "U";
    private static final int BMI_PRECENTILE_5 = 5;
    private static final int BMI_PRECENTILE_85 = 85;
    private static final int BMI_PRECENTILE_95 = 95;

    /**
     * Report Locations
     */
    private static final String REPORT_LOCATION_BMI_INITIAL_SCREENING = "16.a.1";
    private static final String REPORT_LOCATION_BMI_RESCREENING = "16.a.2";
    private static final String REPORT_LOCATION_BMI_OPTION_OUT_COUNT = "29.a.1";
    private static final String REPORT_LOCATION_BMI_REFERRALS = "16.a.3";
    private static final String REPORT_LOCATION_BMI_COMPLETE_REFERRALS = "16.a.4";
    private static final String REPORT_LOCATION_HEARING_INITIAL_SCREENING = "17.a.1";
    private static final String REPORT_LOCATION_HEARING_RESCREENING = "17.a.2";
    private static final String REPORT_LOCATION_HEARING_REFERRALS = "17.a.3";
    private static final String REPORT_LOCATION_HEARING_COMPLETE_REFERRALS = "17.a.4";
    private static final String REPORT_LOCATION_POSTURAL_INITIAL_SCREENING = "18.a.1";
    private static final String REPORT_LOCATION_POSTURAL_RESCREENING = "18.a.2";
    private static final String REPORT_LOCATION_POSTURAL_REFERRALS = "18.a.3";
    private static final String REPORT_LOCATION_POSTURAL_COMPLETE_REFERRALS = "18.a.4";
    private static final String REPORT_LOCATION_VISION_INITIAL_SCREENING = "19.a.1";
    private static final String REPORT_LOCATION_VISION_RESCREENING = "19.a.2";
    private static final String REPORT_LOCATION_VISION_REFERRALS = "19.a.3";
    private static final String REPORT_LOCATION_VISION_COMPLETE_REFERRALS = "19.a.4";

    /**
     * Other.
     */
    private static final String CODE_IN_504 = "Active";
    private static final String CODE_SECTION_504_EXITED = "Exited";
    private static final String CONSTANT_ALL_ACTIVE_SCHOOLS = "All Active Schools";
    private static final String EMPTY = "";
    private static final String ENROLLMENT_SUFFIX = "ENR";
    private static final String[] GRADE_EVELS_ARRAY =
            new String[] {GRADE_LEVEL_01, GRADE_LEVEL_04, GRADE_LEVEL_07, GRADE_LEVEL_10};
    private static final String INSURANCE_STATE_TYPE_NO = "NI";
    private static final String INSURANCE_STATE_TYPE_PRIVATE = "PRI";
    private static final String INSURANCE_STATE_TYPE_PUBLIC = "PUB";
    private static final String INSURANCE_STATE_TYPE_UNKNOWN = "UNK";
    private static final String PARAM_SCHOOL = "school";
    private static final String POSTFIX_COUNTS = "_counts";
    private static final String POSTFIX_STDS = "_stds";
    private static final String POSTFIX_STFS = "_stfs";
    private static final String REPORT_PARAM_SCHOOL_NAME_AND_DATE = "schoolNameAndDate";
    private static final String SPACE = " ";
    private static final String SUFFIX_PRN_ADMIN_SCH = "_prn_admin_sch";
    private static final String SUFFIX_PRN_ADMIN_SCH_NON = "_prn_admin_sch_non";
    private static final String SUFFIX_PRN_SCH = "_prn_sch";
    private static final String SUFFIX_PRN_SCH_NON = "_prn_sch_non";
    private static final String TREATMENT_CODE_DENTAL_SEALANT = "Dental Sealant";
    private static final String TREATMENT_CODE_FLUORIDE_RINSE = "Flouride Rinse";
    private static final String VISIT_TYPE_SCREENING = "Screening";

    /**
     * Field members.
     */
    private HashMap<String, String> m_bMIKeyToLocation;
    private DistrictSchoolYearContext m_ctxByDates;
    private PlainDate m_dateEnd;
    private PlainDate m_dateStart;
    private DataDictionary m_ddxGeneral;
    private DataDictionary m_ddxHearing;
    private DataDictionary m_ddxPhysical;
    private DataDictionary m_ddxPostural;
    private DataDictionary m_ddxVision;
    private String m_fieldIn504Plan;
    private String m_fieldStdEll;
    private String m_fieldStdHomeless;
    private String m_fieldStdInsuranceType;
    private Collection<HealthCondition> m_healthConditions;
    private Map<String, Collection<HealthLog>> m_healthLogs;
    private Map<String, Collection<HealthLog>> m_healthLogsStfs;
    private ArrayList<String> m_kindergartenGradeLevels;
    private Map<String, List<String>> m_locationsByCode;
    private Person m_personRunReport;
    private Collection<String> m_referenceTableOids;
    private ArrayList<String> m_selectedGradeLevels;
    private ArrayList<String> m_selectedTreatmentCodes;
    private SimpleDateFormat m_simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private AttendanceStatistics m_statistics;
    private Collection<String> m_stdValidELLCodes = new ArrayList<>();


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Get Input parameters
        ReportDataGrid grid = new ReportDataGrid(100);
        grid.append();
        populateHealthInsuranceGrid(grid);
        populateHealthConditionGrid(grid);
        populateHlcAndHltGrid(grid);
        populateHealthScreeningBMIGrid(grid);
        populateHealthScreeningMiscGrid(grid);
        populateMedicationAdminGrid(grid);
        populateMedicationOrderGrid(grid);
        populateDemographicsGrid(grid);
        grid.beforeTop();
        // Set Report parameters
        addParameter(REPORT_PARAM_START_DATE, m_dateStart);
        addParameter(REPORT_PARAM_END_DATE, m_dateEnd);
        addParameter(REPORT_PARAM_PERSON_RUN_REPORT_NAME, getPersonRunReprtName());
        addParameter(REPORT_PARAM_CURRENT_SCHOOL_YEAR, m_ctxByDates.getContextId());
        addParameter(REPORT_PARAM_SCHOOL_NAME_AND_DATE, getSchoolNameAndDate());
        // Create Report Grid from Counts
        ReportDataGrid outerGrid = createFinalReportGrid(grid);
        return outerGrid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_dateStart = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_dateEnd = (PlainDate) getParameter(INPUT_PARAM_END_DATE);
        m_fieldIn504Plan = translateAliasToJavaName(ALIAS_STD_DOE_39_IN_504_PLAN);
        m_fieldStdInsuranceType = translateAliasToJavaName(ALIAS_STD_INSURANCE_TYPE);
        m_fieldStdEll = translateAliasToJavaName(ALIAS_STD_ELL);
        m_fieldStdHomeless = translateAliasToJavaName(ALIAS_STD_HOMELESS);
        m_locationsByCode = new HashMap<String, List<String>>(4096);
        m_referenceTableOids = new ArrayList<String>(5);
        m_referenceTableOids.add(RTB_MEDICAL_CONDITIONS);
        m_referenceTableOids.add(RTB_HEALTH_VISIT_TREATMENTS);
        m_referenceTableOids.add(RTB_HEALTH_VISIT_COMPLIANTSS);
        m_referenceTableOids.add(RTB_HEALTH_MED_TYPE);
        m_referenceTableOids.add(RTB_MED_ACT);
        m_referenceTableOids.add(RTB_CONDITION_TYPES);
        m_referenceTableOids.add(RTB_CONDITION_HLG_COMPLIANTS);
        m_kindergartenGradeLevels = new ArrayList();
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KF);
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KP);
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KT);
        m_selectedTreatmentCodes = new ArrayList();
        m_selectedTreatmentCodes.add(TREATMENT_CODE_DENTAL_SEALANT);
        m_selectedTreatmentCodes.add(TREATMENT_CODE_FLUORIDE_RINSE);
        // Only grades 1, 4, 7 and 10
        m_selectedGradeLevels = new ArrayList();
        for (int i = 0; i < GRADE_EVELS_ARRAY.length; i++) {
            m_selectedGradeLevels.add(GRADE_EVELS_ARRAY[i]);
        }
        // translate BMIKey to Location Code
        m_bMIKeyToLocation = new HashMap();
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_MALE + BMI_CATEGORY_UNDERWEIGHT, "21.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_MALE + BMI_CATEGORY_NORMAL, "21.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_MALE + BMI_CATEGORY_OVERWEIGHT, "21.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_MALE + BMI_CATEGORY_OBESE, "21.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_MALE + ENROLLMENT_SUFFIX, "21.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_FEMALE + BMI_CATEGORY_UNDERWEIGHT, "22.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_FEMALE + BMI_CATEGORY_NORMAL, "22.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_FEMALE + BMI_CATEGORY_OVERWEIGHT, "22.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_FEMALE + BMI_CATEGORY_OBESE, "22.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_01 + GENDER_FEMALE + ENROLLMENT_SUFFIX, "22.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_MALE + BMI_CATEGORY_UNDERWEIGHT, "23.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_MALE + BMI_CATEGORY_NORMAL, "23.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_MALE + BMI_CATEGORY_OVERWEIGHT, "23.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_MALE + BMI_CATEGORY_OBESE, "23.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_MALE + ENROLLMENT_SUFFIX, "23.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_FEMALE + BMI_CATEGORY_UNDERWEIGHT, "24.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_FEMALE + BMI_CATEGORY_NORMAL, "24.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_FEMALE + BMI_CATEGORY_OVERWEIGHT, "24.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_FEMALE + BMI_CATEGORY_OBESE, "24.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_04 + GENDER_FEMALE + ENROLLMENT_SUFFIX, "24.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_MALE + BMI_CATEGORY_UNDERWEIGHT, "25.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_MALE + BMI_CATEGORY_NORMAL, "25.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_MALE + BMI_CATEGORY_OVERWEIGHT, "25.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_MALE + BMI_CATEGORY_OBESE, "25.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_MALE + ENROLLMENT_SUFFIX, "25.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_FEMALE + BMI_CATEGORY_UNDERWEIGHT, "26.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_FEMALE + BMI_CATEGORY_NORMAL, "26.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_FEMALE + BMI_CATEGORY_OVERWEIGHT, "26.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_FEMALE + BMI_CATEGORY_OBESE, "26.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_07 + GENDER_FEMALE + ENROLLMENT_SUFFIX, "26.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_MALE + BMI_CATEGORY_UNDERWEIGHT, "27.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_MALE + BMI_CATEGORY_NORMAL, "27.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_MALE + BMI_CATEGORY_OVERWEIGHT, "27.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_MALE + BMI_CATEGORY_OBESE, "27.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_MALE + ENROLLMENT_SUFFIX, "27.a.5");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_FEMALE + BMI_CATEGORY_UNDERWEIGHT, "28.a.1");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_FEMALE + BMI_CATEGORY_NORMAL, "28.a.2");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_FEMALE + BMI_CATEGORY_OVERWEIGHT, "28.a.3");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_FEMALE + BMI_CATEGORY_OBESE, "28.a.4");
        m_bMIKeyToLocation.put(GRADE_PREFIX + GRADE_LEVEL_10 + GENDER_FEMALE + ENROLLMENT_SUFFIX, "28.a.5");

        m_ddxGeneral = getDdxByOid(DDX_HSC_GENERAL_OID);
        m_ddxHearing = getDdxByOid(DDX_HSC_HEARING_OID);
        m_ddxPhysical = getDdxByOid(DDX_HSC_PHYSICAL_OID);
        m_ddxPostural = getDdxByOid(DDX_HSC_POSTURAL_OID);
        m_ddxVision = getDdxByOid(DDX_HSC_VISION_OID);

        initCtxByDates();
        loadHealthLogs();
        loadHealthLogsStfs();
        loadReferenceCodes();
        loadReferenceCodesELL();
        loadStudentAttendanceStatistics();
        loadHealthConditions();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_personRunReport = userData.getUser() == null ? null : userData.getUser().getPerson();
    }

    /**
     * Create Report Grid from Counts.
     *
     * @param grid ReportDataGrid
     * @return ReportDataGrid
     */
    private ReportDataGrid createFinalReportGrid(ReportDataGrid grid) {
        ReportDataGrid outerGrid = new ReportDataGrid();
        for (int i = 1; i <= PAGES; i++) {
            Report report =
                    ReportUtils.getReport(SUBREPORT_ID_PREFIX + String.format("%02d", new Integer(i)), getBroker());
            if (report != null) {
                outerGrid.append();
                outerGrid.set(FIELD_DATA, grid);
                outerGrid.set(FIELD_FORMAT, new ByteArrayInputStream(report.getCompiledFormat()));
            }
        }
        outerGrid.beforeTop();
        return outerGrid;
    }

    /**
     * Gets the ext ddx attributes by oid.
     *
     * @param ddxOid String
     * @return Extended dictionary attributes
     */
    private DataDictionary getDdxByOid(Object ddxOid) {
        Criteria ddxExtCriteria = new Criteria();
        ddxExtCriteria.addEqualTo(X2BaseBean.COL_OID, ddxOid);
        Collection<ExtendedDictionaryAttributes> attributes =
                getBroker().getCollectionByQuery(new QueryByCriteria(ExtendedDataDictionary.class, ddxExtCriteria));
        ExtendedDictionaryAttributes extendedDictionaryAttributes =
                attributes.size() == 1 ? attributes.iterator().next() : null;
        return DataDictionary.getDistrictDictionary(extendedDictionaryAttributes, getBroker().getPersistenceKey());
    }

    /**
     * Returns a collection of General (BMI) HealthScreening records for the dates specified.
     * Only include students as the grade levels 1, 4, 7 and 10
     *
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> getHealthScreeningBMIs() {
        Criteria healthScreeningCriteria = new Criteria();
        healthScreeningCriteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER +
                X2BaseBean.COL_OID, DDX_HSC_GENERAL_OID);
        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, m_dateStart);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, m_dateEnd);
        healthScreeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, m_statistics.getStudentsOids());
        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);
        // sort records by Student OID, then newest record first so that latest screening is used
        // for report classification
        healthScreeningQuery.addOrderByAscending(HealthScreening.COL_STUDENT_OID);
        healthScreeningQuery.addOrderByDescending(HealthScreening.COL_DATE);
        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);
        // Get Student grade levels
        SubQuery studentSubQuery =
                new SubQuery(HealthScreening.class, HealthScreening.COL_STUDENT_OID, healthScreeningCriteria);
        GradeLevelHistory gradeLevelHistory = new GradeLevelHistory(studentSubQuery, 20,
                OrganizationManager.getRootOrganization(getOrganization()), getBroker());
        // Only include students that were in the grade levels 01, 04, 07 and 10
        Collection<HealthScreening> healthScreeningsList = new ArrayList<HealthScreening>();
        for (HealthScreening healthScreening : healthScreenings) {
            String studentOid = healthScreening.getStudentOid();
            String gradeLevel = gradeLevelHistory.getGradeLevel(studentOid, m_ctxByDates.getSchoolYear());
            if (GRADE_LEVEL_01.equals(gradeLevel) || GRADE_LEVEL_04.equals(gradeLevel)
                    || GRADE_LEVEL_07.equals(gradeLevel) || GRADE_LEVEL_10.equals(gradeLevel)) {
                healthScreeningsList.add(healthScreening);
            }
        }
        return healthScreeningsList;
    }

    /**
     * Returns a collection of HealthScreening records for the dates specified.
     *
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> getHealthScreeningMiscGroup() {
        // Get Health Screens for BMI (General), Hearing, Postural and Vision
        ArrayList healthScreeningMiscGroup = new ArrayList();
        healthScreeningMiscGroup.add(DDX_HSC_GENERAL_OID);
        healthScreeningMiscGroup.add(DDX_HSC_HEARING_OID);
        healthScreeningMiscGroup.add(DDX_HSC_POSTURAL_OID);
        healthScreeningMiscGroup.add(DDX_HSC_VISION_OID);
        Criteria extDDxCriteria = new Criteria();
        extDDxCriteria.addIn(X2BaseBean.COL_OID, healthScreeningMiscGroup);
        SubQuery extDDXsubQuery = new SubQuery(ExtendedDataDictionary.class, X2BaseBean.COL_OID, extDDxCriteria);
        Criteria healthScreeningCriteria = new Criteria();
        healthScreeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, m_statistics.getStudentsOids());
        healthScreeningCriteria.addIn(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, extDDXsubQuery);
        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, m_dateStart);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, m_dateEnd);
        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);
        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);
        return healthScreenings;
    }

    /**
     * Returns a collection of HealthScreening records for Student Physicals for the dates
     * specified.
     *
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> getHealthScreeningPhysicals() {
        Criteria healthScreeningCriteria = new Criteria();
        healthScreeningCriteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER +
                X2BaseBean.COL_OID, DDX_HSC_PHYSICAL_OID);
        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, m_dateStart);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, m_dateEnd);
        healthScreeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, m_statistics.getStudentsOids());
        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);
        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);
        return healthScreenings;
    }

    /**
     * Returns a collection of HealthLogTreatment records for Student who had Oral Treatments for
     * the dates specified.
     *
     * @return Collection<HealthLogTreatment>
     */
    private Collection<HealthLogTreatment> getHealthTreatmentOral() {
        // Load Student Health Log Treatments.
        Criteria healthLogTreatmentCriteria = new Criteria();
        healthLogTreatmentCriteria.addGreaterOrEqualThan(
                HealthLogTreatment.REL_HEALTH_LOG + ModelProperty.PATH_DELIMITER + HealthLog.COL_DATE,
                m_dateStart);
        healthLogTreatmentCriteria.addLessOrEqualThan(
                HealthLogTreatment.REL_HEALTH_LOG + ModelProperty.PATH_DELIMITER + HealthLog.COL_DATE,
                m_dateEnd);
        healthLogTreatmentCriteria.addEqualTo(
                HealthLogTreatment.REL_HEALTH_LOG + ModelProperty.PATH_DELIMITER + HealthLog.REL_PERSON
                        + ModelProperty.PATH_DELIMITER + Person.COL_STUDENT_INDICATOR,
                new Boolean(true));
        healthLogTreatmentCriteria.addIn(HealthLogTreatment.COL_TREATMENT_CODE, m_selectedTreatmentCodes);
        if (isSchoolContext()) {
            healthLogTreatmentCriteria.addEqualTo(HealthLogTreatment.REL_HEALTH_LOG + ModelProperty.PATH_DELIMITER +
                    HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthLogTreatmentQuery =
                new QueryByCriteria(HealthLogTreatment.class, healthLogTreatmentCriteria);
        Collection<HealthLogTreatment> healthLogTreatments = getBroker().getCollectionByQuery(healthLogTreatmentQuery);
        return healthLogTreatments;
    }

    /**
     * Gets the person run reprt name.
     *
     * @return String
     */
    private String getPersonRunReprtName() {
        return m_personRunReport == null ? EMPTY : m_personRunReport.getNameView();
    }

    /**
     * Gets the school name and date.
     *
     * @return String
     */
    private String getSchoolNameAndDate() {
        StringBuilder builder = new StringBuilder();
        if (isSchoolContext()) {
            builder.append(getSchool() == null ? EMPTY : getSchool().getName());
        } else {
            builder.append(CONSTANT_ALL_ACTIVE_SCHOOLS);
        }
        builder.append(SPACE + SPACE + SPACE + SPACE);
        PlainDate date = new PlainDate(new Date());
        builder.append(m_simpleDateFormat.format(date));
        return builder.toString();
    }

    /**
     * Look for school year by selected in input dates.
     *
     */
    private void initCtxByDates() {
        X2Criteria schoolYearCriteria = new X2Criteria();
        schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_dateStart);
        schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_dateEnd);
        QueryByCriteria schoolYearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
        schoolYearQuery.addOrderByAscending(DistrictSchoolYearContext.COL_START_DATE);
        m_ctxByDates = getBroker().getBeanByQuery(schoolYearQuery);
    }

    /**
     * Returns a collection of HealthCondition records for enrolled students within the dates
     * specified.
     *
     */
    private void loadHealthConditions() {
        Criteria healthConditionCriteria = new Criteria();
        if (isSchoolContext()) {
            healthConditionCriteria.addEqualTo(HealthCondition.REL_STUDENT + "." +
                    SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        healthConditionCriteria.addIn(HealthCondition.COL_STUDENT_OID, m_statistics.getStudentsOids());
        healthConditionCriteria.addEqualTo(
                HealthCondition.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                "Active");
        QueryByCriteria healthConditionQuery = new QueryByCriteria(HealthCondition.class, healthConditionCriteria);
        m_healthConditions = getBroker().getCollectionByQuery(healthConditionQuery);
    }

    /**
     * Load health logs.
     */
    private void loadHealthLogs() {
        Criteria healthLogCriteria = new Criteria();
        healthLogCriteria.addGreaterOrEqualThan(HealthLog.COL_DATE, m_dateStart);
        healthLogCriteria.addLessOrEqualThan(HealthLog.COL_DATE, m_dateEnd);
        healthLogCriteria.addNotEqualTo(HealthLog.REL_PERSON + PATH_DELIMITER + SisPerson.COL_STAFF_INDICATOR,
                Boolean.TRUE);
        if (isSchoolContext()) {
            healthLogCriteria.addEqualTo(HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthLogQuery = new QueryByCriteria(HealthLog.class, healthLogCriteria);
        m_healthLogs = getBroker().getGroupedCollectionByQuery(healthLogQuery,
                HealthLog.REL_PERSON + PATH_DELIMITER + SisPerson.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID,
                512);
    }

    /**
     * Load health logs for STFS.
     */
    private void loadHealthLogsStfs() {
        Criteria healthLogCriteria = new Criteria();
        healthLogCriteria.addGreaterOrEqualThan(HealthLog.COL_DATE, m_dateStart);
        healthLogCriteria.addLessOrEqualThan(HealthLog.COL_DATE, m_dateEnd);
        healthLogCriteria.addNotEqualTo(HealthLog.REL_PERSON + PATH_DELIMITER + SisPerson.COL_STUDENT_INDICATOR,
                Boolean.TRUE);
        if (isSchoolContext()) {
            healthLogCriteria.addEqualTo(HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthLogQuery = new QueryByCriteria(HealthLog.class, healthLogCriteria);
        m_healthLogsStfs = getBroker().getGroupedCollectionByQuery(healthLogQuery,
                HealthLog.REL_PERSON + PATH_DELIMITER + SisPerson.REL_STAFF + PATH_DELIMITER + X2BaseBean.COL_OID,
                512);
    }


    /**
     * Loads the reference codes into a map keyed to the codes' report location.
     */
    private void loadReferenceCodes() {
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField reportLocationField = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_REPORT_LOCATION);
        if (reportLocationField != null) {
            String reportLocationAttribute = reportLocationField.getJavaName();

            X2Criteria criteria = new X2Criteria();
            criteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
            criteria.addIn(ReferenceCode.COL_REFERENCE_TABLE_OID, m_referenceTableOids);
            criteria.addNotEmpty(reportLocationAttribute, getBroker().getPersistenceKey());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ReferenceCode referenceCode = (ReferenceCode) iterator.next();
                    String code = referenceCode.getCode();
                    List<String> reportLocations = StringUtils.convertDelimitedStringToList(
                            (String) referenceCode.getFieldValueByBeanPath(reportLocationAttribute), ',');
                    for (String location : reportLocations) {
                        location = location.trim();
                        if (m_locationsByCode.keySet().contains(code)) {
                            List<String> locations = m_locationsByCode.get(code);
                            if (!locations.contains(location)) {
                                locations.add(location);
                                m_locationsByCode.put(code, locations);
                            }
                        } else {
                            List<String> locations = new ArrayList<String>();
                            locations.add(location);
                            m_locationsByCode.put(code, locations);
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Loads the reference codes for valid ell codes.
     */
    private void loadReferenceCodesELL() {
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField ddf = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_STD_ELL);
        if (ddf != null && ddf.hasReferenceTable()) {
            Map<String, ReferenceCode> codesMap = ddf.getReferenceTable().getCodeMap();
            for (String code : codesMap.keySet()) {
                if (!codesMap.get(code).getStateCode().equals("00")) {
                    m_stdValidELLCodes.add(code);
                }
            }
        }
    }

    /**
     * Load Student Attendance Statistics.
     */
    private void loadStudentAttendanceStatistics() {
        Map<String, Object> parametersMap = new HashMap<String, Object>();

        m_statistics = new AttendanceStatistics();
        m_statistics.setBroker(getBroker());
        m_statistics.setCurrentContext(m_ctxByDates);
        m_statistics.setPrivilegeSet(getPrivilegeSet());
        m_statistics.setOrganization(OrganizationManager.getRootOrganization(getOrganization()));

        Object school = getParameter(PARAM_SCHOOL);
        boolean isSchoolContext = false;
        if (school instanceof School) {

            isSchoolContext = true;
            m_statistics.setSchool((School) school);
        }
        m_statistics.setSchoolContext(isSchoolContext);

        m_statistics.setParameters(parametersMap);
        try {
            m_statistics.initializeExport();
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                m_dateStart);
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                m_dateEnd);
    }

    /**
     * Returns a collection of StudentEnrollment records for the dates specified.
     * Only include students as the grade levels 1, 4, 7 and 10
     *
     * @return Collection<StudentEnrollment>
     */
    private Collection<StudentEnrollment> loadStudentEnrollments() {
        Criteria studentEnrollmentCriteria = new Criteria();
        studentEnrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                m_dateStart);
        studentEnrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                m_dateEnd);
        if (isSchoolContext()) {
            studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria studentEnrollmentQuery =
                new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
        Collection<StudentEnrollment> studentEnrollments = getBroker().getCollectionByQuery(studentEnrollmentQuery);

        SubQuery studentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, studentEnrollmentCriteria);
        GradeLevelHistory gradeLevelHistory = new GradeLevelHistory(studentSubQuery, 20,
                OrganizationManager.getRootOrganization(getOrganization()), getBroker());

        // Only include students that were in the grade levels 01, 04, 07 and 10
        ArrayList<StudentEnrollment> studentEnrollmentsList = new ArrayList<StudentEnrollment>();
        for (StudentEnrollment studentEnrollment : studentEnrollments) {
            String studentOid = studentEnrollment.getStudentOid();
            String gradeLevel = gradeLevelHistory.getGradeLevel(studentOid, m_ctxByDates.getSchoolYear());
            if (GRADE_LEVEL_01.equals(gradeLevel) || GRADE_LEVEL_04.equals(gradeLevel)
                    || GRADE_LEVEL_07.equals(gradeLevel) || GRADE_LEVEL_10.equals(gradeLevel)) {
                studentEnrollmentsList.add(studentEnrollment);
            }
        }

        return studentEnrollmentsList;
    }

    /**
     * Populates student demographics statistic.
     *
     * @param grid ReportDataGrid
     */
    private void populateDemographicsGrid(ReportDataGrid grid) {
        Map<String, Integer> raceByStdCounts = new HashMap<>();
        Map<String, Integer> raceByVisitsCounts = new HashMap<>();
        Map<String, Integer> raceByEnrCounts = new HashMap<>();
        Map<String, Integer> otherDemoMap = new HashMap<>();
        Map<String, SisStudent> stdByOidHMLSMap = new HashMap<>();
        Map<String, SisStudent> stdByOidELLMap = new HashMap<>();
        for (SisStudent std : m_statistics.m_students) {
            String raceCode = m_statistics.getRace(std.getOid());
            if (!StringUtils.isEmpty(raceCode)) {
                Integer valueToPutEnrCounts =
                        raceByEnrCounts.containsKey(raceCode)
                                ? Integer.valueOf(raceByEnrCounts.get(raceCode).intValue() + 1)
                                : Integer.valueOf(1);
                raceByEnrCounts.put(raceCode, valueToPutEnrCounts);
                if (m_fieldStdHomeless != null
                        && BooleanAsStringConverter.TRUE.equals(std.getFieldValueByBeanPath(m_fieldStdHomeless))) {
                    String keyHMLSENR = "HMLS_ENR";
                    Integer valueToHMLSEnrCounts =
                            otherDemoMap.containsKey(keyHMLSENR)
                                    ? Integer.valueOf(otherDemoMap.get(keyHMLSENR).intValue() + 1)
                                    : Integer.valueOf(1);
                    otherDemoMap.put(keyHMLSENR, valueToHMLSEnrCounts);
                    stdByOidHMLSMap.put(std.getOid(), std);
                }
                if (m_fieldStdEll != null && std.getFieldValueByBeanPath(m_fieldStdEll) != null &&
                        m_stdValidELLCodes.contains(
                                std.getFieldValueByBeanPath(m_fieldStdEll))) {
                    String keyELLENR = "ELL_ENR";
                    Integer valueToELLEnrCounts =
                            otherDemoMap.containsKey(keyELLENR)
                                    ? Integer.valueOf(otherDemoMap.get(keyELLENR).intValue() + 1)
                                    : Integer.valueOf(1);
                    otherDemoMap.put(keyELLENR, valueToELLEnrCounts);
                    stdByOidELLMap.put(std.getOid(), std);
                }
            }
        }
        int enrTotal = 0;
        if (!raceByEnrCounts.isEmpty()) {
            for (String race : raceByEnrCounts.keySet()) {
                enrTotal += raceByEnrCounts.get(race).intValue();
                grid.set(race + "_ENR", raceByEnrCounts.get(race));
            }
        }
        grid.set("total_ENR", Integer.valueOf(enrTotal));
        String keyELLStds = "ELL" + POSTFIX_STDS;
        String keyELLCounts = "ELL" + POSTFIX_COUNTS;
        String keyHMLSStds = "HMLS" + POSTFIX_STDS;
        String keyHMLSCounts = "HMLS" + POSTFIX_COUNTS;
        HashSet<String> operatedStds = new HashSet<>();
        for (String stdOid : m_healthLogs.keySet()) {
            String raceCode = m_statistics.getRace(stdOid);
            if (!StringUtils.isEmpty(raceCode)) {
                if (!operatedStds.contains(stdOid)) {
                    Integer valueToPutStdCounts =
                            raceByStdCounts.containsKey(raceCode)
                                    ? Integer.valueOf(raceByStdCounts.get(raceCode).intValue() + 1)
                                    : Integer.valueOf(1);
                    raceByStdCounts.put(raceCode, valueToPutStdCounts);
                    operatedStds.add(stdOid);
                }
                Collection<HealthLog> logs = m_healthLogs.get(stdOid);
                if (logs != null) {
                    int count = logs.size();
                    Integer valueToPutVisitsCounts =
                            raceByVisitsCounts.containsKey(raceCode)
                                    ? Integer.valueOf(raceByVisitsCounts.get(raceCode).intValue()
                                            + count)
                                    : Integer.valueOf(count);
                    raceByVisitsCounts.put(raceCode, valueToPutVisitsCounts);
                }
            }
            if (stdByOidELLMap.containsKey(stdOid)) {
                Integer valueToPutELLStdCounts =
                        otherDemoMap.containsKey(keyELLStds)
                                ? Integer.valueOf(otherDemoMap.get(keyELLStds).intValue() + 1)
                                : Integer.valueOf(1);
                otherDemoMap.put(keyELLStds, valueToPutELLStdCounts);
                Integer valueToPutELLVisits =
                        otherDemoMap.containsKey(keyELLCounts)
                                ? Integer.valueOf(otherDemoMap.get(keyELLCounts).intValue()
                                        + m_healthLogs.get(stdOid).size())
                                : Integer.valueOf(m_healthLogs.get(stdOid).size());
                otherDemoMap.put(keyELLCounts, valueToPutELLVisits);
            }
            if (stdByOidHMLSMap.containsKey(stdOid)) {
                Integer valueToPutHMLSStdCounts =
                        otherDemoMap.containsKey(keyHMLSStds)
                                ? Integer.valueOf(otherDemoMap.get(keyHMLSStds).intValue() + 1)
                                : Integer.valueOf(1);
                otherDemoMap.put(keyHMLSStds, valueToPutHMLSStdCounts);
                Integer valueToPutHMLSVisits =
                        otherDemoMap.containsKey(keyHMLSCounts)
                                ? Integer.valueOf(otherDemoMap.get(keyHMLSCounts).intValue()
                                        + m_healthLogs.get(stdOid).size())
                                : Integer.valueOf(m_healthLogs.get(stdOid).size());
                otherDemoMap.put(keyHMLSCounts, valueToPutHMLSVisits);
            }
        }
        Map<String, Map<String, Integer>> pdcByRaces = new HashMap<>();
        Map<String, Map<String, Integer>> becByRaces = new HashMap<>();
        Map<String, Integer> ellPdc = new HashMap<>();
        Map<String, Integer> ellBec = new HashMap<>();
        Map<String, Integer> hmlsPdc = new HashMap<>();
        Map<String, Integer> hmlsBec = new HashMap<>();
        for (HealthCondition healthCondition : m_healthConditions) {
            String stdOid = healthCondition.getStudentOid();
            String raceCode = m_statistics.getRace(stdOid);
            if (!StringUtils.isEmpty(raceCode)) {
                String conditionCode = healthCondition.getConditionCode();
                List<String> locationsByCode = m_locationsByCode.get(conditionCode);
                if (locationsByCode != null) {
                    for (String location : locationsByCode) {
                        if (location.startsWith("PDC.")) {
                            if (pdcByRaces.containsKey(raceCode + "_PDC")) {
                                Map<String, Integer> pdcCodesMap = pdcByRaces.get(raceCode + "_PDC");
                                Integer valueToPutPDCCounts =
                                        pdcCodesMap.containsKey(conditionCode)
                                                ? Integer.valueOf(pdcCodesMap.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                pdcCodesMap.put(conditionCode, valueToPutPDCCounts);
                            } else {
                                Map<String, Integer> pdcCodesMap = new HashMap<>();
                                pdcCodesMap.put(conditionCode, new Integer(1));
                                pdcByRaces.put(raceCode + "_PDC", pdcCodesMap);
                            }
                        } else if (location.startsWith("BEC.")) {
                            if (becByRaces.containsKey(raceCode + "_BEC")) {
                                Map<String, Integer> becCodesMap = becByRaces.get(raceCode + "_BEC");
                                Integer valueToPutPDCCounts =
                                        becCodesMap.containsKey(conditionCode)
                                                ? Integer.valueOf(becCodesMap.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                becCodesMap.put(conditionCode, valueToPutPDCCounts);
                            } else {
                                Map<String, Integer> becCodesMap = new HashMap<>();
                                becCodesMap.put(conditionCode, new Integer(1));
                                becByRaces.put(raceCode + "_BEC", becCodesMap);
                            }
                        }
                        if (stdByOidHMLSMap.containsKey(stdOid)) {
                            if (location.startsWith("PDC.")) {
                                Integer valueToPutHMLSPDCCounts =
                                        hmlsPdc.containsKey(conditionCode)
                                                ? Integer.valueOf(hmlsPdc.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                hmlsPdc.put(conditionCode, valueToPutHMLSPDCCounts);
                            } else if (location.startsWith("BEC.")) {
                                Integer valueToPutHMLSBECCounts =
                                        hmlsBec.containsKey(conditionCode)
                                                ? Integer.valueOf(hmlsBec.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                hmlsBec.put(conditionCode, valueToPutHMLSBECCounts);
                            }
                        }
                        if (stdByOidELLMap.containsKey(stdOid)) {
                            if (location.startsWith("PDC.")) {
                                Integer valueToPutEllPDCCounts =
                                        ellPdc.containsKey(conditionCode)
                                                ? Integer.valueOf(ellPdc.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                ellPdc.put(conditionCode, valueToPutEllPDCCounts);
                            } else if (location.startsWith("BEC.")) {
                                Integer valueToPutEllBECCounts =
                                        ellBec.containsKey(conditionCode)
                                                ? Integer.valueOf(ellBec.get(conditionCode).intValue() + 1)
                                                : Integer.valueOf(1);
                                ellBec.put(conditionCode, valueToPutEllBECCounts);
                            }
                        }
                    }
                }
            }
        }
        if (!hmlsPdc.isEmpty()) {
            String codeToPut = null;
            int maxValue = -1;
            for (String code : hmlsPdc.keySet()) {
                if (maxValue < hmlsPdc.get(code).intValue()) {
                    codeToPut = code;
                }
            }
            grid.set("HMLS_PDC", codeToPut);
        }
        if (!hmlsBec.isEmpty()) {
            String codeToPut = null;
            int maxValue = -1;
            for (String code : hmlsBec.keySet()) {
                if (maxValue < hmlsBec.get(code).intValue()) {
                    codeToPut = code;
                }
            }
            grid.set("HMLS_BEC", codeToPut);
        }
        if (!ellPdc.isEmpty()) {
            String codeToPut = null;
            int maxValue = -1;
            for (String code : ellPdc.keySet()) {
                if (maxValue < ellPdc.get(code).intValue()) {
                    codeToPut = code;
                }
            }
            grid.set("ELL_PDC", codeToPut);
        }
        if (!ellBec.isEmpty()) {
            String codeToPut = null;
            int maxValue = -1;
            for (String code : ellBec.keySet()) {
                if (maxValue < ellBec.get(code).intValue()) {
                    codeToPut = code;
                }
            }
            grid.set("ELL_BEC", codeToPut);
        }
        if (pdcByRaces != null && !pdcByRaces.isEmpty()) {
            for (String key : pdcByRaces.keySet()) {
                Map<String, Integer> pdcCodesMap = pdcByRaces.get(key);
                String codeToPut = null;
                int maxValue = -1;
                for (String code : pdcCodesMap.keySet()) {
                    int codeCount = pdcCodesMap.get(code).intValue();
                    if (maxValue < codeCount) {
                        codeToPut = code;
                        maxValue = codeCount;
                    }
                }
                grid.set(key, codeToPut);
            }
        }
        if (becByRaces != null && !becByRaces.isEmpty()) {
            for (String key : becByRaces.keySet()) {
                Map<String, Integer> becCodesMap = becByRaces.get(key);
                String codeToPut = null;
                int maxValue = -1;
                for (String code : becCodesMap.keySet()) {
                    int codeCount = becCodesMap.get(code).intValue();
                    if (maxValue < codeCount) {
                        codeToPut = code;
                        maxValue = codeCount;
                    }
                }
                grid.set(key, codeToPut);
            }
        }
        if (!raceByStdCounts.isEmpty()) {
            int demoTotal = 0;
            for (String race : raceByStdCounts.keySet()) {
                grid.set(race + POSTFIX_STDS, raceByStdCounts.get(race));
                demoTotal += raceByStdCounts.get(race).intValue();
            }
            grid.set("demo_total" + POSTFIX_STDS, Integer.valueOf(demoTotal));
        }
        if (!raceByVisitsCounts.isEmpty()) {
            int demoTotal = 0;
            for (String race : raceByVisitsCounts.keySet()) {
                grid.set(race + POSTFIX_COUNTS, raceByVisitsCounts.get(race));
                demoTotal += raceByVisitsCounts.get(race).intValue();
            }
            grid.set("demo_total" + POSTFIX_COUNTS, Integer.valueOf(demoTotal));
        }
        if (!otherDemoMap.isEmpty()) {
            for (String key : otherDemoMap.keySet()) {
                grid.set(key, otherDemoMap.get(key));
            }
        }
    }

    /**
     * Populates grid with PDC and BEC report locations.
     *
     * @param grid ReportDataGrid
     */
    private void populateHealthConditionGrid(ReportDataGrid grid) {
        Map<String, Integer> stdCounts = new HashMap<>();
        Map<String, Integer> visitsCounts = new HashMap<>();
        HashSet<String> specialCareStds = new HashSet<>();
        HashSet<String> operatedStdsFor504 = new HashSet<>();
        HashSet<String> operatedStdsForLocation = new HashSet<>();
        int in504PlanStds = 0;
        for (String stdOid : m_healthLogs.keySet()) {
            if (!operatedStdsFor504.contains(stdOid)) {
                SisStudent std = getBroker().getBeanByOid(SisStudent.class, stdOid);
                if (std != null && !CODE_SECTION_504_EXITED.equals(std.getSection504StatusCode())
                        && CODE_IN_504.equals(std.getFieldValueByBeanPath(m_fieldIn504Plan))) {
                    in504PlanStds += 1;
                }
            }
        }
        for (HealthCondition healthCondition : m_healthConditions) {
            String studentOid = healthCondition.getStudentOid();
            if (!operatedStdsFor504.contains(studentOid)) {
                if (!CODE_SECTION_504_EXITED.equals(healthCondition.getStudent().getSection504StatusCode())
                        && CODE_IN_504.equals(healthCondition.getStudent().getFieldValueByBeanPath(m_fieldIn504Plan))) {
                    in504PlanStds += 1;
                    operatedStdsFor504.add(studentOid);
                }
            }
            String conditionCode = healthCondition.getConditionCode();
            List<String> locationsByCode = m_locationsByCode.get(conditionCode);
            String conditionType = healthCondition.getConditionType();
            List<String> locationsByType = m_locationsByCode.get(conditionType);
            HashSet<String> locations = new HashSet<>();
            if (locationsByCode != null) {
                locations.addAll(locationsByCode);
            }
            if (locationsByType != null) {
                locations.addAll(locationsByType);
            }
            if (locations != null && !locations.isEmpty()) {
                for (String location : locations) {
                    if (location.contains("PDC") || location.contains("BEC")) {
                        specialCareStds.add(studentOid);
                        if (!operatedStdsForLocation.contains(studentOid + location)) {
                            Integer valueToPut = stdCounts.containsKey(location)
                                    ? Integer.valueOf(stdCounts.get(location).intValue() + 1)
                                    : Integer.valueOf(1);
                            stdCounts.put(location, valueToPut);
                            Collection<HealthLog> logs = m_healthLogs.get(studentOid);
                            if (logs != null && !logs.isEmpty()) {
                                valueToPut = visitsCounts.containsKey(location)
                                        ? Integer.valueOf(visitsCounts.get(location).intValue() + logs.size())
                                        : Integer.valueOf(logs.size());
                                visitsCounts.put(location, valueToPut);
                                operatedStdsForLocation.add(studentOid + location);
                            }
                        }
                    }
                }
            }
        }
        if (!stdCounts.isEmpty()) {
            for (String location : stdCounts.keySet()) {
                Integer stdCountsForLocation = stdCounts.get(location);
                grid.set(location + POSTFIX_STDS, stdCountsForLocation);
            }
        }
        grid.set(REPORT_FIELD_IN_504, Integer.valueOf(in504PlanStds));
        // @formatter:off
        /*
        HashSet<String> operatedCareStds = new HashSet<>();
        for (HealthCondition healthCondition : m_healthConditions) {
            String studentOid = healthCondition.getStudentOid();
            String conditionType = healthCondition.getConditionType();
            List<String> locationsByType = m_locationsByCode.get(conditionType);
            String conditionCode = healthCondition.getConditionCode();
            List<String> locationsByCode = m_locationsByCode.get(conditionCode);
            HashSet<String> locations = new HashSet<String>();
            if (locationsByType != null) {
                locations.addAll(locationsByType);
            }
            if (locationsByCode != null) {
                locations.addAll(locationsByCode);
            }
            if (locations != null && !locations.isEmpty()) {
                for (String location : locations) {
                    if (location.contains("PDC") || location.contains("BEC")) {
                        specialCareStds.add(studentOid);
                        if (!operatedCareStds.contains(studentOid + location)) {
                            Collection<HealthLog> logs = m_healthLogs.get(studentOid);
                            if (logs != null && !logs.isEmpty()) {
                                Integer valueToPut = visitsCounts.containsKey(location)
                                        ? Integer.valueOf(visitsCounts.get(location).intValue() + logs.size())
                                        : Integer.valueOf(logs.size());
                                visitsCounts.put(location, valueToPut);
                                operatedCareStds.add(studentOid + location);
                            }
                        }
                    }
                }
            }
        }
        */
        // @formatter:on
        if (!visitsCounts.isEmpty()) {
            for (String location : visitsCounts.keySet()) {
                Integer stdVisitsForLocation = visitsCounts.get(location);
                grid.set(location + POSTFIX_COUNTS, stdVisitsForLocation);
            }
        }
        grid.set(REPORT_FIELD_SPECIAl_CARE, Integer.valueOf(specialCareStds.size()));
    }

    /**
     * Populates grid with counts of Private/Public/No Insurance/Unknown insurance types.
     *
     * @param grid ReportDataGrid
     */
    private void populateHealthInsuranceGrid(ReportDataGrid grid) {
        Collection<SisStudent> students = m_statistics.getStudents();
        Map<String, Integer> typesMap = new HashMap<>();
        if (m_fieldStdInsuranceType != null && students != null) {
            for (SisStudent std : students) {
                String type = (String) std.getFieldValueByBeanPath(m_fieldStdInsuranceType);
                if (!StringUtils.isEmpty(type)) {
                    String stateType = m_statistics.lookupStateValue(SisStudent.class, m_fieldStdInsuranceType, type);
                    if (!StringUtils.isEmpty(stateType)) {
                        switch (stateType) {
                            case INSURANCE_STATE_TYPE_PRIVATE:
                                if (typesMap.containsKey(INSURANCE_STATE_TYPE_PRIVATE)) {
                                    typesMap.put(INSURANCE_STATE_TYPE_PRIVATE,
                                            Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_PRIVATE).intValue() + 1));
                                } else {
                                    typesMap.put(INSURANCE_STATE_TYPE_PRIVATE, Integer.valueOf(1));
                                }
                                break;
                            case INSURANCE_STATE_TYPE_PUBLIC:
                                if (typesMap.containsKey(INSURANCE_STATE_TYPE_PUBLIC)) {
                                    typesMap.put(INSURANCE_STATE_TYPE_PUBLIC,
                                            Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_PUBLIC).intValue() + 1));
                                } else {
                                    typesMap.put(INSURANCE_STATE_TYPE_PUBLIC, Integer.valueOf(1));
                                }
                                break;
                            case INSURANCE_STATE_TYPE_NO:
                                if (typesMap.containsKey(INSURANCE_STATE_TYPE_NO)) {
                                    typesMap.put(INSURANCE_STATE_TYPE_NO,
                                            Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_NO).intValue() + 1));
                                } else {
                                    typesMap.put(INSURANCE_STATE_TYPE_NO, Integer.valueOf(1));
                                }
                                break;
                            case INSURANCE_STATE_TYPE_UNKNOWN:
                                if (typesMap.containsKey(INSURANCE_STATE_TYPE_UNKNOWN)) {
                                    typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN,
                                            Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_UNKNOWN).intValue() + 1));
                                } else {
                                    typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN, Integer.valueOf(1));
                                }
                                break;

                            default:
                                break;
                        }
                    } else {
                        if (typesMap.containsKey(INSURANCE_STATE_TYPE_UNKNOWN)) {
                            typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN,
                                    Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_UNKNOWN).intValue() + 1));
                        } else {
                            typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN, Integer.valueOf(1));
                        }
                    }
                } else {
                    if (typesMap.containsKey(INSURANCE_STATE_TYPE_UNKNOWN)) {
                        typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN,
                                Integer.valueOf(typesMap.get(INSURANCE_STATE_TYPE_UNKNOWN).intValue() + 1));
                    } else {
                        typesMap.put(INSURANCE_STATE_TYPE_UNKNOWN, Integer.valueOf(1));
                    }
                }
            }
        }
        if (!typesMap.isEmpty()) {
            for (String type : typesMap.keySet()) {
                grid.set(type, typesMap.get(type));
            }
        }
    }

    /**
     * Populates grid with BMI counts.
     *
     * @param grid ReportDataGrid
     */
    private void populateHealthScreeningBMIGrid(ReportDataGrid grid) {
        Collection<HealthScreening> healthScreeningsBMI = getHealthScreeningBMIs();
        HashMap<String, Integer> counts = new HashMap<String, Integer>();
        int totalBMIOptionOutCount = 0;
        // Get field name for BMI Percent
        DataDictionaryField bMIPercentField =
                m_ddxGeneral.findDataDictionaryFieldByAlias(ALIAS_GENERAL_BMI_PERCENT);
        DataDictionaryField bMIOptionField =
                m_ddxGeneral.findDataDictionaryFieldByAlias(ALIAS_GENERAL_BMI_OPTION);
        if (bMIPercentField != null && bMIOptionField != null) {
            HashSet loggedStudentOids = new HashSet();
            for (HealthScreening healthScreening : healthScreeningsBMI) {
                String studentOid = healthScreening.getStudentOid();
                // Count each student only once.
                if (!loggedStudentOids.contains(studentOid)) {
                    boolean bMIOption = false;
                    String bMIOptionStr =
                            (String) healthScreening.getFieldValueByAlias(ALIAS_GENERAL_BMI_OPTION, m_ddxGeneral);
                    if (bMIOptionStr != null && (DB_TRUE.equalsIgnoreCase(bMIOptionStr))) {
                        bMIOption = true;
                    }
                    // Don't count the student if they have opted out the BMI screening
                    if (bMIOption) {
                        totalBMIOptionOutCount++;
                    } else {
                        String bMIPercentageStr = (String) healthScreening
                                .getFieldValueByAlias(ALIAS_GENERAL_BMI_PERCENT, m_ddxGeneral);
                        double bMIPercentage = 0;
                        // skip the record if BMI percent is non-numeric
                        if (bMIPercentageStr == null || !StringUtils.isNumeric(bMIPercentageStr)) {
                            continue;
                        }
                        bMIPercentage = Double.valueOf(bMIPercentageStr).doubleValue();
                        String gradeLevel = healthScreening.getStudent().getGradeLevel();
                        String gender = healthScreening.getStudent().getPerson().getGenderCode();
                        String percentileGroup = null;
                        if (bMIPercentage < BMI_PRECENTILE_5) {
                            percentileGroup = BMI_CATEGORY_UNDERWEIGHT;
                        } else if (bMIPercentage >= BMI_PRECENTILE_5 && bMIPercentage < BMI_PRECENTILE_85) {
                            percentileGroup = BMI_CATEGORY_NORMAL;
                        } else if (bMIPercentage >= BMI_PRECENTILE_85 && bMIPercentage < BMI_PRECENTILE_95) {
                            percentileGroup = BMI_CATEGORY_OVERWEIGHT;
                        } else {
                            percentileGroup = BMI_CATEGORY_OBESE;
                        }
                        String bMIKey = GRADE_PREFIX + gradeLevel + gender + percentileGroup;
                        String bmiEnrKey = GRADE_PREFIX + gradeLevel + gender + ENROLLMENT_SUFFIX;
                        String location = m_bMIKeyToLocation.get(bMIKey);
                        String locationEnr = m_bMIKeyToLocation.get(bmiEnrKey);
                        if (!counts.containsKey(location)) {
                            counts.put(location, new Integer(1));
                        } else {
                            Integer count = counts.get(location);
                            counts.put(location, new Integer(count.intValue() + 1));
                        }
                        if (!counts.containsKey(locationEnr)) {
                            counts.put(locationEnr, new Integer(1));
                        } else {
                            Integer count = counts.get(locationEnr);
                            counts.put(locationEnr, new Integer(count.intValue() + 1));
                        }
                    }
                    loggedStudentOids.add(studentOid);
                }
            }
            if (!counts.isEmpty()) {
                for (String key : counts.keySet()) {
                    grid.set(key, counts.get(key));
                }
            }
            grid.set(REPORT_LOCATION_BMI_OPTION_OUT_COUNT, new Integer(totalBMIOptionOutCount));
        }
    }

    /**
     * Populate the Student Health Screening Misc Group Count.
     * Groups are (BMI, Hearing, Postural, Vision)
     *
     * @param grid ReportDataGrid
     */
    private void populateHealthScreeningMiscGrid(ReportDataGrid grid) {
        Collection<HealthScreening> healthScreeningsMsicGroups = getHealthScreeningMiscGroup();
        // Get the field names of the two extended data dictionary fields isRescreening and
        // isReferralComplete
        int totalHealthScreeningsBMIInitialCount = 0;
        int totalHealthScreeningsBMIReScreenInitCount = 0;
        int totalHealthScreeningsBMIReferralsCount = 0;
        int totalHealthScreeningsBMICompletedCount = 0;
        int totalHealthScreeningsHearingInitialCount = 0;
        int totalHealthScreeningsHearingReScreenInitCount = 0;
        int totalHealthScreeningsHearingReferralsCount = 0;
        int totalHealthScreeningsHearingCompletedCount = 0;
        int totalHealthScreeningsPosturalInitialCount = 0;
        int totalHealthScreeningsPosturalReScreenInitCount = 0;
        int totalHealthScreeningsPosturalReferralsCount = 0;
        int totalHealthScreeningsPosturalCompletedCount = 0;
        int totalHealthScreeningsVisionInitialCount = 0;
        int totalHealthScreeningsVisionReScreenInitCount = 0;
        int totalHealthScreeningsVisionReferralsCount = 0;
        int totalHealthScreeningsVisionCompletedCount = 0;
        String ddxOid = null;
        // Cycle though all Health Screening and create total counts
        for (HealthScreening healthScreening : healthScreeningsMsicGroups) {
            ddxOid = healthScreening.getExtendedDataDictionary().getOid();
            ddxOid = ddxOid.trim();
            boolean isFollowUp = healthScreening.getFollowUpIndicator();
            boolean isReferral = healthScreening.getReferralIndicator();

            boolean isReferralComplete = false;
            String isReferralCompleteStr = (String) healthScreening
                    .getFieldValueByAlias(ALIAS_GENERAL_IS_REFERRAL_COMPLETE, m_ddxGeneral);
            if (isReferralCompleteStr != null && (DB_TRUE.equalsIgnoreCase(isReferralCompleteStr))) {
                isReferralComplete = true;
            }
            if (DDX_HSC_GENERAL_OID.equals(ddxOid)) {
                String bMIOptionStr =
                        (String) healthScreening.getFieldValueByAlias(ALIAS_GENERAL_BMI_OPTION, m_ddxGeneral);
                if (bMIOptionStr != null && (BooleanAsStringConverter.TRUE.equalsIgnoreCase(bMIOptionStr))) {
                    continue;
                }
                boolean isGeneralRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING,
                                m_ddxGeneral))
                                        ? true
                                        : false;
                if (!isFollowUp && !isReferral && !isGeneralRescreen && !isReferralComplete) {
                    totalHealthScreeningsBMIInitialCount++;
                } else if (isGeneralRescreen) {
                    totalHealthScreeningsBMIReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsBMIReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsBMICompletedCount++;
                }
            } else if (DDX_HSC_HEARING_OID.equals(ddxOid)) {
                boolean isHearingRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING, m_ddxHearing))
                                ? true
                                : false;
                if (!isFollowUp && !isReferral && !isHearingRescreen && !isReferralComplete) {
                    totalHealthScreeningsHearingInitialCount++;
                } else if (isHearingRescreen) {
                    totalHealthScreeningsHearingReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsHearingReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsHearingCompletedCount++;
                }
            } else if (DDX_HSC_POSTURAL_OID.equals(ddxOid)) {
                boolean isPosturalRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING, m_ddxPostural))
                                ? true
                                : false;
                if (!isFollowUp && !isReferral && !isPosturalRescreen && !isReferralComplete) {
                    totalHealthScreeningsPosturalInitialCount++;
                } else if (isPosturalRescreen) {
                    totalHealthScreeningsPosturalReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsPosturalReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsPosturalCompletedCount++;
                }
            } else if (DDX_HSC_VISION_OID.equals(ddxOid)) {
                boolean isVisionRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING, m_ddxVision))
                                ? true
                                : false;
                if (!isFollowUp && !isReferral && !isVisionRescreen && !isReferralComplete) {
                    totalHealthScreeningsVisionInitialCount++;
                } else if (isVisionRescreen) {
                    totalHealthScreeningsVisionReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsVisionReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsVisionCompletedCount++;
                }
            }
        }
        grid.set(REPORT_LOCATION_BMI_INITIAL_SCREENING, new Integer(totalHealthScreeningsBMIInitialCount));
        grid.set(REPORT_LOCATION_BMI_RESCREENING, new Integer(totalHealthScreeningsBMIReScreenInitCount));
        grid.set(REPORT_LOCATION_BMI_REFERRALS, new Integer(totalHealthScreeningsBMIReferralsCount));
        grid.set(REPORT_LOCATION_BMI_COMPLETE_REFERRALS, new Integer(totalHealthScreeningsBMICompletedCount));
        grid.set(REPORT_LOCATION_HEARING_INITIAL_SCREENING, new Integer(totalHealthScreeningsHearingInitialCount));
        grid.set(REPORT_LOCATION_HEARING_RESCREENING, new Integer(totalHealthScreeningsHearingReScreenInitCount));
        grid.set(REPORT_LOCATION_HEARING_REFERRALS, new Integer(totalHealthScreeningsHearingReferralsCount));
        grid.set(REPORT_LOCATION_HEARING_COMPLETE_REFERRALS,
                new Integer(totalHealthScreeningsHearingCompletedCount));
        grid.set(REPORT_LOCATION_POSTURAL_INITIAL_SCREENING,
                new Integer(totalHealthScreeningsPosturalInitialCount));
        grid.set(REPORT_LOCATION_POSTURAL_RESCREENING, new Integer(totalHealthScreeningsPosturalReScreenInitCount));
        grid.set(REPORT_LOCATION_POSTURAL_REFERRALS, new Integer(totalHealthScreeningsPosturalReferralsCount));
        grid.set(REPORT_LOCATION_POSTURAL_COMPLETE_REFERRALS,
                new Integer(totalHealthScreeningsPosturalCompletedCount));
        grid.set(REPORT_LOCATION_VISION_INITIAL_SCREENING, new Integer(totalHealthScreeningsVisionInitialCount));
        grid.set(REPORT_LOCATION_VISION_RESCREENING, new Integer(totalHealthScreeningsVisionReScreenInitCount));
        grid.set(REPORT_LOCATION_VISION_REFERRALS, new Integer(totalHealthScreeningsVisionReferralsCount));
        grid.set(REPORT_LOCATION_VISION_COMPLETE_REFERRALS, new Integer(totalHealthScreeningsVisionCompletedCount));
    }

    /**
     * Populates grid with Assessments/ Interventions/ Procedures/Treatments.
     *
     * @param grid ReportDataGrid
     */
    private void populateHlcAndHltGrid(ReportDataGrid grid) {
        Map<String, Integer> codesMapStd = new HashMap<>();
        Map<String, Integer> codesMapStf = new HashMap<>();
        for (String stdOid : m_healthLogs.keySet()) {
            Collection<HealthLog> visits = m_healthLogs.get(stdOid);
            for (HealthLog visit : visits) {
                HashSet<String> treatmentCodes = new HashSet<String>();
                treatmentCodes.add(visit.getPrimaryTreatmentCode());
                for (HealthLogTreatment treatment : visit.getTreatments(getBroker())) {
                    treatmentCodes.add(treatment.getTreatmentCode());
                }
                for (String treatmentCode : treatmentCodes) {
                    List<String> treatmentLocations = m_locationsByCode.get(treatmentCode);
                    if (treatmentLocations != null) {
                        for (String location : treatmentLocations) {
                            if (location.startsWith("8.")) {
                                Integer valueToPut = codesMapStd.containsKey(location)
                                        ? Integer.valueOf(codesMapStd.get(location).intValue() + 1)
                                        : Integer.valueOf(1);
                                codesMapStd.put(location, valueToPut);
                            }
                        }
                    }
                }
            }
        }
        for (String stfOid : m_healthLogsStfs.keySet()) {
            Collection<HealthLog> visits = m_healthLogsStfs.get(stfOid);
            for (HealthLog visit : visits) {
                HashSet<String> treatmentCodes = new HashSet<String>();
                treatmentCodes.add(visit.getPrimaryTreatmentCode());
                for (HealthLogTreatment treatment : visit.getTreatments(getBroker())) {
                    treatmentCodes.add(treatment.getTreatmentCode());
                }
                for (String treatmentCode : treatmentCodes) {
                    List<String> treatmentLocations = m_locationsByCode.get(treatmentCode);
                    if (treatmentLocations != null) {
                        for (String location : treatmentLocations) {
                            if (location.startsWith("8.")) {
                                Integer valueToPut = codesMapStf.containsKey(location)
                                        ? Integer.valueOf(codesMapStf.get(location).intValue() + 1)
                                        : Integer.valueOf(1);
                                codesMapStf.put(location, valueToPut);
                            }
                        }
                    }
                }
            }
        }
        if (!codesMapStd.isEmpty()) {
            int totalStds = 0;
            for (String location : codesMapStd.keySet()) {
                if (location.startsWith("8")) {
                    Integer valueToPut = codesMapStd.get(location);
                    totalStds += valueToPut.intValue();
                    grid.set(location + POSTFIX_STDS, valueToPut);
                }
            }
            grid.set("total" + POSTFIX_STDS, Integer.valueOf(totalStds));
        }
        if (!codesMapStf.isEmpty()) {
            int totalStfs = 0;
            for (String location : codesMapStf.keySet()) {
                if (location.startsWith("8")) {
                    Integer valueToPut = codesMapStf.get(location);
                    totalStfs += valueToPut.intValue();
                    grid.set(location + POSTFIX_STFS, valueToPut);
                }
            }
            grid.set("total" + POSTFIX_STFS, Integer.valueOf(totalStfs));
        }
    }

    /**
     * Counts the medication orders administered.
     *
     * @param grid ReportDataGrid
     */
    private void populateMedicationAdminGrid(ReportDataGrid grid) {
        X2Criteria medicationAdminCriteria = new X2Criteria();
        medicationAdminCriteria.addGreaterOrEqualThan(
                HealthInventoryTransaction.REL_HEALTH_JOB + PATH_DELIMITER + HealthJob.COL_DATE,
                m_dateStart);
        medicationAdminCriteria.addLessOrEqualThan(
                HealthInventoryTransaction.REL_HEALTH_JOB + PATH_DELIMITER + HealthJob.COL_DATE,
                m_dateEnd);
        SubQuery personOidSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_statistics.m_helper.getStudentCriteria());
        medicationAdminCriteria.addIn(HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                HealthMedicationOrder.REL_STUDENT + PATH_DELIMITER + Student.COL_PERSON_OID, personOidSubQuery);
        Map<String, Integer> adminPrnMap = new HashMap<>();
        String[] columns = {HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                HealthMedicationOrder.COL_MEDICATION_TYPE,
                HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                        HealthMedicationOrder.COL_AS_NEEDED_INDICATOR,
                HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                        HealthMedicationOrder.COL_HMO_OID};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(HealthInventoryTransaction.class, columns, medicationAdminCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                boolean asNeededIndicator = "1".equals(record[1]);
                String suffix = null;
                if (asNeededIndicator) {
                    suffix = SUFFIX_PRN_ADMIN_SCH_NON;
                } else {
                    suffix = SUFFIX_PRN_ADMIN_SCH;
                }
                List<String> locations = m_locationsByCode.get(code);
                if (!CollectionUtils.isEmpty(locations)) {
                    for (String location : locations) {
                        String key = location + suffix;
                        int usage = adminPrnMap.containsKey(key) ? adminPrnMap.get(key).intValue() : 0;
                        adminPrnMap.put(key, new Integer(usage + 1));
                    }
                }
            }
        } finally {
            iterator.close();
        }
        if (!adminPrnMap.isEmpty()) {
            int totalAdminSch = 0;
            int totalAdminSchNon = 0;
            for (String type : adminPrnMap.keySet()) {
                if (type.endsWith(SUFFIX_PRN_ADMIN_SCH)) {
                    totalAdminSch += adminPrnMap.get(type).intValue();
                } else if (type.endsWith(SUFFIX_PRN_ADMIN_SCH_NON)) {
                    totalAdminSchNon += adminPrnMap.get(type).intValue();
                }
                grid.set(type, adminPrnMap.get(type));
            }
            grid.set("total" + SUFFIX_PRN_ADMIN_SCH, Integer.valueOf(totalAdminSch));
            grid.set("total" + SUFFIX_PRN_ADMIN_SCH_NON, Integer.valueOf(totalAdminSchNon));
        }
    }

    /**
     * Counts the medication order that are open.
     *
     * @param grid ReportDataGrid
     */
    private void populateMedicationOrderGrid(ReportDataGrid grid) {
        X2Criteria medicationCriteria = new X2Criteria();
        medicationCriteria.addLessOrEqualThan(HealthMedicationOrder.COL_START_DATE, m_dateEnd);
        medicationCriteria.addGreaterOrEqualThan(HealthMedicationOrder.COL_STOP_DATE,
                m_dateStart);
        medicationCriteria.addEmpty(HealthMedicationOrder.COL_HMO_OID, getBroker().getPersistenceKey());
        SubQuery personOidSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_statistics.m_helper.getStudentCriteria());
        medicationCriteria.addIn(HealthMedicationOrder.REL_STUDENT + PATH_DELIMITER +
                SisStudent.COL_PERSON_OID, personOidSubQuery);
        Map<String, Integer> prnMap = new HashMap<>();

        String[] columns = {HealthMedicationOrder.COL_MEDICATION_TYPE,
                HealthMedicationOrder.COL_AS_NEEDED_INDICATOR};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(HealthMedicationOrder.class, columns, medicationCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                boolean asNeededIndicator = "1".equals(record[1]);
                List<String> locations = m_locationsByCode.get(code);
                if (!CollectionUtils.isEmpty(locations)) {
                    for (String location : locations) {
                        String scheduledSuffix =
                                asNeededIndicator ? SUFFIX_PRN_SCH_NON : SUFFIX_PRN_SCH;
                        String key = location + scheduledSuffix;
                        int usage = prnMap.containsKey(key) ? prnMap.get(key).intValue() : 0;
                        prnMap.put(key, new Integer(usage + 1));
                    }
                }
            }
        } finally {
            iterator.close();
        }
        if (!prnMap.isEmpty()) {
            int totalSch = 0;
            int totalSchNon = 0;
            for (String type : prnMap.keySet()) {
                if (type.endsWith(SUFFIX_PRN_SCH)) {
                    totalSch += prnMap.get(type).intValue();
                } else if (type.endsWith(SUFFIX_PRN_SCH_NON)) {
                    totalSchNon += prnMap.get(type).intValue();
                }
                grid.set(type, prnMap.get(type));
            }
            grid.set("total" + SUFFIX_PRN_SCH, Integer.valueOf(totalSch));
            grid.set("total" + SUFFIX_PRN_SCH_NON, Integer.valueOf(totalSchNon));
        }
    }

    /**
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;
        DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }
        return javaName;
    }
}
