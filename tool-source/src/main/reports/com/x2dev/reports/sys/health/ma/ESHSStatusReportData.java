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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.HealthLogTreatment;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
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

/**
 * Prepares the data for the MA Annual District BMI Summary Report.
 *
 * @author X2 Development Corporation
 */
public class ESHSStatusReportData extends ReportJavaSourceNet {


    private static final String IN_504_CODE = "504";

    private static final String SECTION_504_EXITED_CODE = "Exited";

    private static final String ALIAS_DOE_39_IN_504_PLAN = "DOE 39";

    /**
     * The Class StdHelperForIncludeStatusCode40.
     */
    public class StdHelperForIncludeStatusCode40 extends StudentHistoryHelper {

        private static final String ENROLLMENT_CODE_40_NOT_ENROLLED_SPED_ONLY = "40";

        /**
         * Instantiates a new std helper for include status code 40.
         *
         * @param data StateReportData
         */
        public StdHelperForIncludeStatusCode40(StateReportData data) {
            super(data);
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentCriteriaForActiveStudents(java.lang.Boolean)
         */
        @Override
        protected X2Criteria buildStudentCriteriaForActiveStudents(Boolean applySchool) {
            X2Criteria activeCriteria = new X2Criteria();
            Collection<String> activeStatus = StudentManager.getActiveStudentCodeList(getData().getOrganization());
            String enrollmentCode40 = getEnrollmentStatusCode40();
            if (!StringUtils.isEmpty(enrollmentCode40)) {
                activeStatus.add(enrollmentCode40);
            }

            activeCriteria.addIn(Student.COL_ENROLLMENT_STATUS, activeStatus);
            // Apply school selection criteria.
            if (applySchool.booleanValue() && getData().isSchoolContext()) {
                activeCriteria.addEqualTo(Student.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                activeCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                activeCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            return activeCriteria;
        }

        /**
         * Gets the enrollment status code 40.
         *
         * @return String
         */
        private String getEnrollmentStatusCode40() {
            String code = null;

            DataDictionaryField field = getData().getDataDictionaryField(Student.class, Student.COL_ENROLLMENT_STATUS);
            Map<String, ReferenceCode> codes = getData().getReferenceCodes(field.getReferenceTableOid());
            for (String currentCode : codes.keySet()) {
                ReferenceCode refCode = codes.get(currentCode);
                String stateCode = refCode.getStateCode();
                if (!StringUtils.isEmpty(stateCode)) {
                    if (stateCode.equals(ENROLLMENT_CODE_40_NOT_ENROLLED_SPED_ONLY)) {
                        code = currentCode;
                        break;
                    }
                }
            }

            return code;
        }
    }

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";
    private static final String SUBREPORT_ID_PREFIX = "SYS-HTH-MA-ESHS-SB";
    private static final int PAGES = 9;
    private static final String DB_TRUE = "1";

    private static final String INPUT_PARAM_START_DATE = "startDate";
    private static final String INPUT_PARAM_END_DATE = "endDate";

    private static final String REPORT_PARAM_CURRENT_SCHOOL_YEAR = "currentSchoolYear";
    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_PERSON_RUN_REPORT_NAME = "personRunReportName";
    private static final String REPORT_PARAM_START_DATE = "startDate";

    // use id instead oid because sql server added space in oid for make length 14
    // in the result query don't find records because oid with space and without space - different
    // oid
    // my sql don't add space ...
    private static final String DDX_HSC_GENERAL_OID = "ddxHscGeneral";
    // private static final String DDX_HSC_PHYSICAL_OID = "ddxHscPhysical";
    private static final String DDX_HSC_HEARING_OID = "ddxHscHearing";
    private static final String DDX_HSC_POSTURAL_OID = "ddxHscPostural";
    private static final String DDX_HSC_VISION_OID = "ddxHscVision";
    // use id instead oid because sql server added space in oid for make length 14
    // in the result query don't find records because oid with space and without space - different
    // oid
    // my sql don't add space ...
    private static final String DDX_HSC_GENERAL_ID = "HSC-GENERAL";
    private static final String DDX_HSC_PHYSICAL_ID = "HSC-PHYSICAL";
    private static final String DDX_HSC_HEARING_ID = "HSC-HEARING";
    private static final String DDX_HSC_POSTURAL_ID = "HSC-POSTURAL";
    private static final String DDX_HSC_VISION_ID = "HSC-VISION";

    private static final String ALIAS_GENERAL_BMI_PERCENT = "hsc-general-bmi-percent";
    private static final String ALIAS_GENERAL_BMI_OPTION = "hsc-general-bmi-opt";
    private static final String ALIAS_GENERAL_IS_RESCREENING = "hsc-general-rescreening";
    private static final String ALIAS_GENERAL_IS_REFERRAL_COMPLETE = "hsc-general-referral-complete";
    private static final String ALIAS_REPORT_LOCATION = "report-location";
    private static final String REC_CODE_MEDICAL_CONDITIONS = "rtbConditions";
    private static final String REC_CODE_HEALTH_VISIT_TREATMENTS = "rtbHthTreatCds";

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
    private static final String BMI_CATEGORY_UNDERWEIGHT = "U";
    private static final String BMI_CATEGORY_NORMAL = "N";
    private static final String BMI_CATEGORY_OVERWEIGHT = "O";
    private static final String BMI_CATEGORY_OBESE = "B";
    private static final String ENROLLMENT_SUFFIX = "ENR";

    private static final String[] m_gradeLevelsArray =
            new String[] {GRADE_LEVEL_01, GRADE_LEVEL_04, GRADE_LEVEL_07, GRADE_LEVEL_10};

    private static final int BMI_PRECENTILE_5 = 5;
    private static final int BMI_PRECENTILE_85 = 85;
    private static final int BMI_PRECENTILE_95 = 95;

    private static final String PARAM_SCHOOL = "school";
    private static final String CONSTANT_ALL_ACTIVE_SCHOOLS = "All Active Schools";
    private static final String SPACE = " ";
    private static final String REPORT_PARAM_SCHOOL_NAME_AND_DATE = "schoolNameAndDate";

    private static final String TREATMENT_CODE_DENTAL_SEALANT = "Dental Sealant";
    private static final String TREATMENT_CODE_FLUORIDE_RINSE = "Flouride Rinse";
    private static final String VISIT_TYPE_SCREENING = "Screening";
    private static final String REPORT_LOCATION_STUDENT_CONDITIONS_COUNT = "10.a.1";
    private static final String REPORT_LOCATION_STUDENT_504_COUNT = "10.a.3";
    private static final String REPORT_LOCATION_TOTAL_STUDENT_VISITS = "12.a.1";
    private static final String REPORT_LOCATION_STUDENT_KINDERGARTEN_COUNT = "14.a.1";
    private static final String REPORT_LOCATION_PHYSICAL_SCREENINGS_COUNT = "20.a.1";
    private static final String REPORT_LOCATION_BMI_OPTION_OUT_COUNT = "29.a.1";

    private static final String REPORT_LOCATION_BMI_INITIAL_SCREENING = "16.a.1";
    private static final String REPORT_LOCATION_BMI_RESCREENING = "16.a.2";
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

    // other

    private static final String EMPTY = "";
    private static final String POINT = ".";


    private HashMap<String, Integer> m_counts = new HashMap<String, Integer>();
    private Map<String, List<String>> m_locationsByCode;
    private Collection<String> m_referenceTableOids;
    private ArrayList<String> m_kindergartenGradeLevels;
    private ArrayList<String> m_selectedTreatmentCodes;
    private ArrayList<String> m_selectedGradeLevels;
    private HashMap<String, String> m_bMIKeyToLocation;
    private AttendanceStatistics m_statistics;
    private Person m_person_run_report;
    private int m_schoolYear;
    private SimpleDateFormat m_simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
    private String m_fieldIn504Plan;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Get Input parameters
        PlainDate startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        m_schoolYear = getSchoolYear(startDate, endDate);

        loadStudentAttendanceStatistics(startDate, endDate);

        // Load Report counts
        // Update Sections 8, 9, 10.a.1, 10.a.6, 10.a.7 and 11.a.1
        updateHealthConditionCounts(startDate, endDate);

        // Update Section 10.a.3
        updateStudentSection504Counts(startDate, endDate);

        // Update Section 12
        updateTotalStudentHealthVisitCounts(startDate, endDate);

        // Update Section 14
        updateStudentKindergartenCounts(startDate, endDate);

        // Update Sections 16 - 19
        updateHealthScreeningMiscGroupCounts(startDate, endDate);

        // Update Section 20
        updateHealthScreeningPhysicalCounts(startDate, endDate);

        // Update Sections 21 - 28
        updateHealthScreeningBMICounts(startDate, endDate);
        updateStudentEnrollmentCounts(startDate, endDate);

        // Update Section 31
        updateHealthTreatmentOralCounts(startDate, endDate);

        // Set Report parameters
        addParameter(REPORT_PARAM_START_DATE, startDate);
        addParameter(REPORT_PARAM_END_DATE, endDate);
        addParameter(REPORT_PARAM_PERSON_RUN_REPORT_NAME, getPersonRunReprtName());
        addParameter(REPORT_PARAM_CURRENT_SCHOOL_YEAR, getCurrentContext().getContextId());
        addParameter(REPORT_PARAM_SCHOOL_NAME_AND_DATE, getSchoolNameAndDate());



        // Create Report Grid from Counts
        ReportDataGrid outerGrid = createReportGridFromCounts();

        return outerGrid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_fieldIn504Plan = translateAliasToJavaName(ALIAS_DOE_39_IN_504_PLAN);
        m_locationsByCode = new HashMap<String, List<String>>(4096);

        m_referenceTableOids = new ArrayList<String>(5);
        m_referenceTableOids.add(REC_CODE_MEDICAL_CONDITIONS);
        m_referenceTableOids.add(REC_CODE_HEALTH_VISIT_TREATMENTS);

        loadReferenceCodes();

        m_kindergartenGradeLevels = new ArrayList();
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KF);
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KP);
        m_kindergartenGradeLevels.add(GRADE_LEVEL_KT);

        m_selectedTreatmentCodes = new ArrayList();
        m_selectedTreatmentCodes.add(TREATMENT_CODE_DENTAL_SEALANT);
        m_selectedTreatmentCodes.add(TREATMENT_CODE_FLUORIDE_RINSE);

        // Only grades 1, 4, 7 and 10
        m_selectedGradeLevels = new ArrayList();
        for (int i = 0; i < m_gradeLevelsArray.length; i++) {
            m_selectedGradeLevels.add(m_gradeLevelsArray[i]);
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
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_person_run_report = userData.getUser() == null ? null : userData.getUser().getPerson();
    }

    /**
     * Create Report Grid from Counts.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid createReportGridFromCounts() {
        // Save to Report Grid
        ReportDataGrid grid = new ReportDataGrid(100);
        grid.append();
        for (String key : m_counts.keySet()) {
            Integer value = m_counts.get(key);
            grid.set(key, value);
        }

        // Append the counts to the grid and set the grid for each page.
        ReportDataGrid outerGrid = new ReportDataGrid();
        for (int i = 1; i <= PAGES; i++) {
            outerGrid.append();
            outerGrid.set(FIELD_DATA, grid);

            Report report =
                    ReportUtils.getReport(SUBREPORT_ID_PREFIX + String.format("%02d", Integer.valueOf(i)), getBroker());
            if (report != null) {
                outerGrid.set(FIELD_FORMAT, new ByteArrayInputStream(report.getCompiledFormat()));
            }
        }

        outerGrid.beforeTop();

        return outerGrid;
    }

    /**
     * Gets the ext ddx attributes by id.
     *
     * @param ddxId String
     * @return Extended dictionary attributes
     */
    private ExtendedDictionaryAttributes getExtDdxAttributesById(String ddxId) {
        Criteria ddxExtCriteria = new Criteria();
        ddxExtCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
        Collection<ExtendedDictionaryAttributes> attributes =
                getBroker().getCollectionByQuery(new QueryByCriteria(ExtendedDataDictionary.class, ddxExtCriteria));

        ExtendedDictionaryAttributes extendedDictionaryAttributes =
                attributes.size() == 1 ? attributes.iterator().next() : null;
        return extendedDictionaryAttributes;
    }

    /**
     * Returns a collection of Students in Kindergarten who enrolled between the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return studentCount
     */
    private int getStudentsInKindergartenCount(PlainDate startDate, PlainDate endDate) {
        // Select students enrolled in Kindergarten between the data range.
        Selection studentSelection = m_statistics.getActiveStudentsSelection();
        Criteria selectionCriteria = new Criteria();
        selectionCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        SubQuery studentOidSubQuery =
                new SubQuery(SelectionObject.class, SelectionObject.COL_OBJECT_OID, selectionCriteria);

        GradeLevelHistory gradeLevelHistory = new GradeLevelHistory(studentOidSubQuery, 20,
                OrganizationManager.getRootOrganization(getOrganization()), getBroker());

        // Only count students that were in Kindergarten in the selected school Year
        int studentCount = 0;
        List<String> activeStudents = m_statistics.getActiveStudents();
        for (String studentOid : activeStudents) {
            String gradeLevel = gradeLevelHistory.getGradeLevel(studentOid, m_schoolYear);
            if (GRADE_LEVEL_KF.equals(gradeLevel) || GRADE_LEVEL_KP.equals(gradeLevel)
                    || GRADE_LEVEL_KT.equals(gradeLevel)) {
                studentCount++;
            }
        }

        // Delete the selection object
        getBroker().deleteBean(studentSelection);

        return studentCount;
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
     * Returns School Year between date range.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return int
     */
    private int getSchoolYear(PlainDate startDate, PlainDate endDate) {
        X2Criteria schoolYearCriteria = new X2Criteria();
        schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
        schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, endDate);
        QueryByCriteria schoolYearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
        schoolYearQuery.addOrderByAscending(DistrictSchoolYearContext.COL_START_DATE);
        Collection<DistrictSchoolYearContext> districtSchoolYearContexts =
                getBroker().getCollectionByQuery(schoolYearQuery);

        int schoolYear = 0;
        for (DistrictSchoolYearContext districtSchoolYearContext : districtSchoolYearContexts) {
            schoolYear = districtSchoolYearContext.getSchoolYear();
            break;
        }

        return schoolYear;
    }

    /**
     * Returns a count of Section 504 Students who enrolled between the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return studentCount
     */
    private int getStudentsSection504Count(PlainDate startDate, PlainDate endDate) {
        // Get Students with Section 504 Ed Plan active in the date range

        Criteria studentCriteria = new Criteria();

        studentCriteria.addNotNull(Student.COL_SECTION504_STATUS_CODE);
        studentCriteria.addNotEqualTo(Student.COL_SECTION504_STATUS_CODE, SECTION_504_EXITED_CODE);
        studentCriteria.addEqualTo(m_fieldIn504Plan, IN_504_CODE);

        Selection studentSelection = m_statistics.getActiveStudentsSelection();

        Criteria studentSubCriteria = new Criteria();
        studentSubCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        studentSubCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
        SubQuery studentOidSubQuery =
                new SubQuery(SelectionObject.class, SelectionObject.COL_OBJECT_OID, studentSubCriteria);

        studentCriteria.addIn(X2BaseBean.COL_OID, studentOidSubQuery);

        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

        int studentCount = getBroker().getCount(studentQuery);
        getBroker().deleteBean(studentSelection);
        return studentCount;
    }

    /**
     * Returns a collection of HealthCondition records for enrolled students within the dates
     * specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthCondition>
     */
    private Collection<HealthCondition> loadHealthConditions(PlainDate startDate, PlainDate endDate) {
        // Load Health Conditions.
        Criteria healthConditionCriteria = new Criteria();

        // Select the target group of students that were enrolled between the selected date range
        // Build student criteria.
        Selection studentSelection = m_statistics.getActiveStudentsSelection();
        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        studentCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + HealthCondition.COL_STUDENT_OID);



        // Build HealthCondition criteria
        healthConditionCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, studentCriteria));
        /*
         * if (isSchoolContext())
         * {
         * healthConditionCriteria.addEqualTo(HealthCondition.REL_STUDENT +
         * ModelProperty.PATH_DELIMITER +
         * Student.COL_SCHOOL_OID, getSchool().getOid());
         * //21 with this criteria
         * //21check without this criteria - must be the same;
         *
         * }
         */
        QueryByCriteria healthConditionQuery = new QueryByCriteria(HealthCondition.class, healthConditionCriteria);

        Collection<HealthCondition> healthConditions = getBroker().getCollectionByQuery(healthConditionQuery);

        // Delete the selection object
        getBroker().deleteBean(studentSelection);

        return healthConditions;
    }

    /**
     * Returns a collection of HealthLog records for Student Visits.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthLog>
     */
    private Collection<HealthLog> loadHealthLogs(PlainDate startDate, PlainDate endDate) {
        // Select the target group of students that were enrolled between the selected date range
        Selection studentSelection = m_statistics.getActiveStudentsSelection();

        Criteria selectionCriteria = new Criteria();
        selectionCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        selectionCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);
        SubQuery studentOidSubQuery =
                new SubQuery(SelectionObject.class, SelectionObject.COL_OBJECT_OID, selectionCriteria);

        Criteria studentCriteria = new Criteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, studentOidSubQuery);

        SubQuery personOidSubQuery = new SubQuery(Student.class, Student.COL_PERSON_OID, studentCriteria);


        // Load Health Logs.
        Criteria healthLogCriteria = new Criteria();
        healthLogCriteria.addGreaterOrEqualThan(HealthLog.COL_DATE, startDate);
        healthLogCriteria.addLessOrEqualThan(HealthLog.COL_DATE, endDate);
        healthLogCriteria.addEqualTo(HealthLog.REL_PERSON + POINT + Person.COL_STUDENT_INDICATOR, Boolean.valueOf(true));

        // logic must work without isSchoolContext block because studentSelection contain student
        // belong to selected school
        // but I stay this criteria:
        // 1) student can move from one school to another and in each school student will have
        // HealthLog
        // in this case can appear record belong to another school. It is theory without checking
        // real data
        // 2) broker with this criteria provide data less than one minute
        // without this criteria broker try get data very very very very... very very very long
        // time.
        // I do not get the results in 20 minutes and broke process
        if (isSchoolContext()) {
            healthLogCriteria.addEqualTo(HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        }

        healthLogCriteria.addIn(HealthLog.COL_PERSON_OID, personOidSubQuery);

        QueryByCriteria healthLogQuery = new QueryByCriteria(HealthLog.class, healthLogCriteria);

        Collection<HealthLog> healthLogs = getBroker().getCollectionByQuery(healthLogQuery);


        // Delete the selection object
        getBroker().deleteBean(studentSelection);

        return healthLogs;
    }

    /**
     * Returns a collection of General (BMI) HealthScreening records for the dates specified.
     * Only include students as the grade levels 1, 4, 7 and 10
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> loadHealthScreeningBMIs(PlainDate startDate, PlainDate endDate) {
        // Select BMI Health Screening records
        Criteria healthScreeningCriteria = new Criteria();
        healthScreeningCriteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_HSC_GENERAL_ID);

        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, startDate);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, endDate);

        Selection studentSelection = m_statistics.getActiveStudentsSelection();
        Criteria studentSubCriteria = new Criteria();
        studentSubCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        studentSubCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + HealthScreening.COL_STUDENT_OID);
        SubQuery studentOidSubQuery =
                new SubQuery(SelectionObject.class, SelectionObject.COL_OBJECT_OID, studentSubCriteria);
        healthScreeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, studentOidSubQuery);

        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    Student.COL_SCHOOL_OID, getSchool().getOid());
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
            String gradeLevel = gradeLevelHistory.getGradeLevel(studentOid, m_schoolYear);
            if (GRADE_LEVEL_01.equals(gradeLevel) || GRADE_LEVEL_04.equals(gradeLevel)
                    || GRADE_LEVEL_07.equals(gradeLevel) || GRADE_LEVEL_10.equals(gradeLevel)) {
                healthScreeningsList.add(healthScreening);
            }
        }
        getBroker().deleteBean(studentSelection);
        return healthScreeningsList;
    }

    /**
     * Returns a collection of HealthScreening records for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> loadHealthScreeningMiscGroup(PlainDate startDate, PlainDate endDate) {
        // Get Health Screens for BMI (General), Hearing, Postural and Vision
        ArrayList healthScreeningMiscGroup = new ArrayList();
        healthScreeningMiscGroup.add(DDX_HSC_GENERAL_ID);
        healthScreeningMiscGroup.add(DDX_HSC_HEARING_ID);
        healthScreeningMiscGroup.add(DDX_HSC_POSTURAL_ID);
        healthScreeningMiscGroup.add(DDX_HSC_VISION_ID);
        Criteria extDDxCriteria = new Criteria();
        extDDxCriteria.addIn(ExtendedDataDictionary.COL_ID, healthScreeningMiscGroup);
        SubQuery extDDXsubQuery = new SubQuery(ExtendedDataDictionary.class, X2BaseBean.COL_OID, extDDxCriteria);
        Criteria healthScreeningCriteria = new Criteria();
        // healthScreeningCriteria.addIn(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID,
        // healthScreeningMiscGroup);
        healthScreeningCriteria.addIn(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, extDDXsubQuery);
        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, startDate);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, endDate);

        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    Student.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);

        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);

        return healthScreenings;
    }

    /**
     * Returns a collection of HealthScreening records for Student Physicals for the dates
     * specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> loadHealthScreeningPhysicals(PlainDate startDate, PlainDate endDate) {
        Criteria healthScreeningCriteria = new Criteria();
        healthScreeningCriteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_HSC_PHYSICAL_ID);
        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, startDate);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, endDate);

        if (isSchoolContext()) {
            healthScreeningCriteria.addEqualTo(HealthScreening.REL_STUDENT +
                    ModelProperty.PATH_DELIMITER + Student.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);

        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);

        return healthScreenings;
    }

    /**
     * Returns a collection of HealthLogTreatment records for Student who had Oral Treatments for
     * the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthLogTreatment>
     */
    private Collection<HealthLogTreatment> loadHealthTreatmentOral(PlainDate startDate, PlainDate endDate) {
        // Load Student Health Log Treatments.
        Criteria healthLogTreatmentCriteria = new Criteria();
        healthLogTreatmentCriteria.addGreaterOrEqualThan(HealthLogTreatment.REL_HEALTH_LOG + POINT + HealthLog.COL_DATE,
                startDate);
        healthLogTreatmentCriteria.addLessOrEqualThan(HealthLogTreatment.REL_HEALTH_LOG + POINT + HealthLog.COL_DATE,
                endDate);
        healthLogTreatmentCriteria.addEqualTo(
                HealthLogTreatment.REL_HEALTH_LOG + POINT + HealthLog.REL_PERSON + POINT + Person.COL_STUDENT_INDICATOR,
                Boolean.valueOf(true));
        healthLogTreatmentCriteria.addIn(HealthLogTreatment.COL_TREATMENT_CODE, m_selectedTreatmentCodes);

        if (isSchoolContext()) {
            healthLogTreatmentCriteria.addEqualTo(HealthLogTreatment.REL_HEALTH_LOG + POINT +
                    HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria healthLogTreatmentQuery =
                new QueryByCriteria(HealthLogTreatment.class, healthLogTreatmentCriteria);

        Collection<HealthLogTreatment> healthLogTreatments = getBroker().getCollectionByQuery(healthLogTreatmentQuery);

        return healthLogTreatments;
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
            criteria.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.FALSE);
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
     * Returns a collection of StudentEnrollment records for the dates specified.
     * Only include students as the grade levels 1, 4, 7 and 10
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<StudentEnrollment>
     */
    private Collection<StudentEnrollment> loadStudentEnrollments(PlainDate startDate, PlainDate endDate) {
        Criteria studentEnrollmentCriteria = new Criteria();
        studentEnrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        studentEnrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
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
            String gradeLevel = gradeLevelHistory.getGradeLevel(studentOid, m_schoolYear);
            if (GRADE_LEVEL_01.equals(gradeLevel) || GRADE_LEVEL_04.equals(gradeLevel)
                    || GRADE_LEVEL_07.equals(gradeLevel) || GRADE_LEVEL_10.equals(gradeLevel)) {
                studentEnrollmentsList.add(studentEnrollment);
            }
        }

        return studentEnrollmentsList;
    }

    /**
     * Load Student Attendance Statistics.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void loadStudentAttendanceStatistics(PlainDate startDate, PlainDate endDate) {
        Map<String, Object> parametersMap = new HashMap<String, Object>();

        m_statistics = new AttendanceStatistics();
        m_statistics.setBroker(getBroker());
        // XXX m_statistics.setCurrentContext(getCurrentContext());
        m_statistics.setPrivilegeSet(getPrivilegeSet());
        m_statistics.setOrganization(OrganizationManager.getRootOrganization(getOrganization()));
        m_statistics.setStartDate(startDate);
        m_statistics.setEndDate(endDate);

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
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, startDate);
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, endDate);
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

    /**
     * Gets the person run reprt name.
     *
     * @return String
     */
    private String getPersonRunReprtName() {
        return m_person_run_report == null ? EMPTY : m_person_run_report.getNameView();
    }


    /**
     * Updates the Health Condition Counts from the HealthCondition records for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateHealthConditionCounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthCondition> healthConditions = loadHealthConditions(startDate, endDate);

        HashSet loggedStudentOids = new HashSet();

        int totalHealthConditionCount = 0;

        for (HealthCondition healthCondition : healthConditions) {
            String studentOid = healthCondition.getStudentOid();
            String conditionCode = healthCondition.getConditionCode();

            // Only include a student once
            if (!loggedStudentOids.contains(studentOid)) {
                loggedStudentOids.add(studentOid);
                totalHealthConditionCount++;
            }

            List<String> locations = m_locationsByCode.get(conditionCode);

            if (!CollectionUtils.isEmpty(locations)) {
                for (String location : locations) {
                    if (!m_counts.containsKey(location)) {
                        m_counts.put(location, Integer.valueOf(1));
                    } else {
                        Integer count = m_counts.get(location);
                        m_counts.put(location, Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        }

        m_counts.put(REPORT_LOCATION_STUDENT_CONDITIONS_COUNT, Integer.valueOf(totalHealthConditionCount));
    }

    /**
     * Updates the BMI Values from the HealthScreenings records for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateHealthScreeningBMICounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthScreening> healthScreeningsBMI = loadHealthScreeningBMIs(startDate, endDate);

        int totalBMIOptionOutCount = 0;

        // Get field name for BMI Percent
        ExtendedDictionaryAttributes extendedDictionaryAttributes = getExtDdxAttributesById(DDX_HSC_GENERAL_ID);

        DataDictionary extendedDictionary =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributes, getBroker().getPersistenceKey());
        DataDictionaryField bMIPercentField =
                extendedDictionary.findDataDictionaryFieldByAlias(ALIAS_GENERAL_BMI_PERCENT);
        DataDictionaryField bMIOptionField =
                extendedDictionary.findDataDictionaryFieldByAlias(ALIAS_GENERAL_BMI_OPTION);
        if (bMIPercentField != null && bMIOptionField != null) {
            HashSet loggedStudentOids = new HashSet();

            for (HealthScreening healthScreening : healthScreeningsBMI) {
                String studentOid = healthScreening.getStudentOid();

                // Count each student only once.
                if (!loggedStudentOids.contains(studentOid)) {
                    boolean bMIOption = false;
                    String bMIOptionStr =
                            (String) healthScreening.getFieldValueByAlias(ALIAS_GENERAL_BMI_OPTION, extendedDictionary);
                    if (bMIOptionStr != null && (DB_TRUE.equalsIgnoreCase(bMIOptionStr))) {
                        bMIOption = true;
                    }

                    // Don't count the student if they have opted out the BMI screening
                    if (bMIOption) {
                        totalBMIOptionOutCount++;
                    } else {
                        String bMIPercentageStr = (String) healthScreening
                                .getFieldValueByAlias(ALIAS_GENERAL_BMI_PERCENT, extendedDictionary);
                        int bMIPercentage = 0;

                        // skip the record if BMI percent is non-numeric
                        if (bMIPercentageStr == null || !StringUtils.isNumeric(bMIPercentageStr)) {
                            continue;
                        }

                        bMIPercentage = Integer.valueOf(bMIPercentageStr).intValue();

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
                        String location = m_bMIKeyToLocation.get(bMIKey);

                        if (!m_counts.containsKey(location)) {
                            m_counts.put(location, Integer.valueOf(1));
                        } else {
                            Integer count = m_counts.get(location);
                            m_counts.put(location, Integer.valueOf(count.intValue() + 1));
                        }
                    }

                    loggedStudentOids.add(studentOid);
                }
            }

            m_counts.put(REPORT_LOCATION_BMI_OPTION_OUT_COUNT, Integer.valueOf(totalBMIOptionOutCount));
        }
    }

    /**
     * Updates the Student Health Screening Misc Group Count for the dates specified.
     * Groups are (BMI, Hearing, Postural, Vision)
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateHealthScreeningMiscGroupCounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthScreening> healthScreeningsMsicGroups = loadHealthScreeningMiscGroup(startDate, endDate);

        // Get the field names of the two extended data dictionary fields isRescreening and
        // isReferralComplete
        ExtendedDictionaryAttributes extendedDictionaryAttributesGeneral = getExtDdxAttributesById(DDX_HSC_GENERAL_ID);
        DataDictionary extendedDictionaryGeneral =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributesGeneral,
                        getBroker().getPersistenceKey());

        ExtendedDictionaryAttributes extendedDictionaryAttributesVision = getExtDdxAttributesById(DDX_HSC_VISION_ID);
        DataDictionary extendedDictionaryVision =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributesVision,
                        getBroker().getPersistenceKey());

        ExtendedDictionaryAttributes extendedDictionaryAttributesHearing = getExtDdxAttributesById(DDX_HSC_HEARING_ID);
        DataDictionary extendedDictionaryHearing =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributesHearing,
                        getBroker().getPersistenceKey());

        ExtendedDictionaryAttributes extendedDictionaryAttributesPostural =
                getExtDdxAttributesById(DDX_HSC_POSTURAL_ID);
        DataDictionary extendedDictionaryPostural =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributesPostural,
                        getBroker().getPersistenceKey());

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

        String extendedDataDictionaryOid = null;

        // Cycle though all Health Screening and create total counts
        for (HealthScreening healthScreening : healthScreeningsMsicGroups) {
            extendedDataDictionaryOid = healthScreening.getExtendedDataDictionaryOid();
            extendedDataDictionaryOid = extendedDataDictionaryOid.trim();
            boolean isFollowUp = healthScreening.getFollowUpIndicator();
            boolean isReferral = healthScreening.getReferralIndicator();

            boolean isReferralComplete = false;
            String isReferralCompleteStr = (String) healthScreening
                    .getFieldValueByAlias(ALIAS_GENERAL_IS_REFERRAL_COMPLETE, extendedDictionaryGeneral);
            if (isReferralCompleteStr != null && (DB_TRUE.equalsIgnoreCase(isReferralCompleteStr))) {
                isReferralComplete = true;
            }

            if (DDX_HSC_GENERAL_OID.equals(extendedDataDictionaryOid)) {
                boolean isGeneralRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING,
                                extendedDictionaryGeneral))
                                        ? true : false;
                if (!isFollowUp && !isReferral && !isGeneralRescreen && !isReferralComplete) {
                    totalHealthScreeningsBMIInitialCount++;
                } else if (isGeneralRescreen) {
                    totalHealthScreeningsBMIReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsBMIReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsBMICompletedCount++;
                }
            } else if (DDX_HSC_HEARING_OID.equals(extendedDataDictionaryOid)) {
                boolean isHearingRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING,
                                extendedDictionaryHearing))
                                        ? true : false;
                if (!isFollowUp && !isReferral && !isHearingRescreen && !isReferralComplete) {
                    totalHealthScreeningsHearingInitialCount++;
                } else if (isHearingRescreen) {
                    totalHealthScreeningsHearingReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsHearingReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsHearingCompletedCount++;
                }
            } else if (DDX_HSC_POSTURAL_OID.equals(extendedDataDictionaryOid)) {
                boolean isPosturalRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING,
                                extendedDictionaryPostural))
                                        ? true : false;
                if (!isFollowUp && !isReferral && !isPosturalRescreen && !isReferralComplete) {
                    totalHealthScreeningsPosturalInitialCount++;
                } else if (isPosturalRescreen) {
                    totalHealthScreeningsPosturalReScreenInitCount++;
                } else if (isReferral) {
                    totalHealthScreeningsPosturalReferralsCount++;
                } else if (isReferralComplete) {
                    totalHealthScreeningsPosturalCompletedCount++;
                }
            } else if (DDX_HSC_VISION_OID.equals(extendedDataDictionaryOid)) {
                boolean isVisionRescreen = BooleanAsStringConverter.TRUE
                        .equals(healthScreening.getFieldValueByAlias(ALIAS_GENERAL_IS_RESCREENING,
                                extendedDictionaryVision))
                                        ? true : false;
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

        m_counts.put(REPORT_LOCATION_BMI_INITIAL_SCREENING, Integer.valueOf(totalHealthScreeningsBMIInitialCount));
        m_counts.put(REPORT_LOCATION_BMI_RESCREENING, Integer.valueOf(totalHealthScreeningsBMIReScreenInitCount));
        m_counts.put(REPORT_LOCATION_BMI_REFERRALS, Integer.valueOf(totalHealthScreeningsBMIReferralsCount));
        m_counts.put(REPORT_LOCATION_BMI_COMPLETE_REFERRALS, Integer.valueOf(totalHealthScreeningsBMICompletedCount));
        m_counts.put(REPORT_LOCATION_HEARING_INITIAL_SCREENING, Integer.valueOf(totalHealthScreeningsHearingInitialCount));
        m_counts.put(REPORT_LOCATION_HEARING_RESCREENING, Integer.valueOf(totalHealthScreeningsHearingReScreenInitCount));
        m_counts.put(REPORT_LOCATION_HEARING_REFERRALS, Integer.valueOf(totalHealthScreeningsHearingReferralsCount));
        m_counts.put(REPORT_LOCATION_HEARING_COMPLETE_REFERRALS,
                Integer.valueOf(totalHealthScreeningsHearingCompletedCount));
        m_counts.put(REPORT_LOCATION_POSTURAL_INITIAL_SCREENING,
                Integer.valueOf(totalHealthScreeningsPosturalInitialCount));
        m_counts.put(REPORT_LOCATION_POSTURAL_RESCREENING, Integer.valueOf(totalHealthScreeningsPosturalReScreenInitCount));
        m_counts.put(REPORT_LOCATION_POSTURAL_REFERRALS, Integer.valueOf(totalHealthScreeningsPosturalReferralsCount));
        m_counts.put(REPORT_LOCATION_POSTURAL_COMPLETE_REFERRALS,
                Integer.valueOf(totalHealthScreeningsPosturalCompletedCount));
        m_counts.put(REPORT_LOCATION_VISION_INITIAL_SCREENING, Integer.valueOf(totalHealthScreeningsVisionInitialCount));
        m_counts.put(REPORT_LOCATION_VISION_RESCREENING, Integer.valueOf(totalHealthScreeningsVisionReScreenInitCount));
        m_counts.put(REPORT_LOCATION_VISION_REFERRALS, Integer.valueOf(totalHealthScreeningsVisionReferralsCount));
        m_counts.put(REPORT_LOCATION_VISION_COMPLETE_REFERRALS, Integer.valueOf(totalHealthScreeningsVisionCompletedCount));
    }

    /**
     * Updates the Student Health Screening Physicals Count for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateHealthScreeningPhysicalCounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthScreening> healthScreeningsPhysical = loadHealthScreeningPhysicals(startDate, endDate);

        HashSet loggedStudentOids = new HashSet();

        int totalHealthScreeningsPhysicalCount = 0;

        for (HealthScreening healthScreening : healthScreeningsPhysical) {
            String studentOid = healthScreening.getStudentOid();

            // Count each student only once
            if (!loggedStudentOids.contains(studentOid)) {
                loggedStudentOids.add(studentOid);
                totalHealthScreeningsPhysicalCount++;
            }
        }

        m_counts.put(REPORT_LOCATION_PHYSICAL_SCREENINGS_COUNT, Integer.valueOf(totalHealthScreeningsPhysicalCount));
    }

    /**
     * Updates the Student Oral Treatment Count for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateHealthTreatmentOralCounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthLogTreatment> healthLogTreatments = loadHealthTreatmentOral(startDate, endDate);

        for (HealthLogTreatment healthLogTreatment : healthLogTreatments) {
            String treatmentCode = healthLogTreatment.getTreatmentCode();

            List<String> locations = m_locationsByCode.get(treatmentCode);

            if (!CollectionUtils.isEmpty(locations)) {
                for (String location : locations) {
                    if (!m_counts.containsKey(location)) {
                        m_counts.put(location, Integer.valueOf(1));
                    } else {
                        Integer count = m_counts.get(location);
                        m_counts.put(location, Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        }
    }

    /**
     * Updates the Enrollment Values from the StudentEnrollments records for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateStudentEnrollmentCounts(PlainDate startDate, PlainDate endDate) {
        Collection<StudentEnrollment> studentEnrollments = loadStudentEnrollments(startDate, endDate);

        HashSet loggedStudentOids = new HashSet();

        for (StudentEnrollment studentEnrollment : studentEnrollments) {
            String studentOid = studentEnrollment.getStudentOid();

            // Only count the student once
            if (!loggedStudentOids.contains(studentOid)) {
                String gradeLevel = studentEnrollment.getStudent().getGradeLevel();
                String gender = studentEnrollment.getStudent().getPerson().getGenderCode();
                String enrollmentKey = GRADE_PREFIX + gradeLevel + gender + ENROLLMENT_SUFFIX;
                String location = m_bMIKeyToLocation.get(enrollmentKey);

                if (!m_counts.containsKey(location)) {
                    m_counts.put(location, Integer.valueOf(1));
                } else {
                    Integer count = m_counts.get(location);
                    m_counts.put(location, Integer.valueOf(count.intValue() + 1));
                }
            }

            loggedStudentOids.add(studentOid);
        }
    }

    /**
     * Updates the Student Kindergarten Counts for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateStudentKindergartenCounts(PlainDate startDate, PlainDate endDate) {
        int totalStudentKindergartenCount = getStudentsInKindergartenCount(startDate, endDate);

        m_counts.put(REPORT_LOCATION_STUDENT_KINDERGARTEN_COUNT, Integer.valueOf(totalStudentKindergartenCount));
    }

    /**
     * Updates the Special Counts for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateStudentSection504Counts(PlainDate startDate, PlainDate endDate) {
        int totalStudent504Count = getStudentsSection504Count(startDate, endDate);

        m_counts.put(REPORT_LOCATION_STUDENT_504_COUNT, Integer.valueOf(totalStudent504Count));
    }

    /**
     * Updates the Total Student Health Visit Counts for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void updateTotalStudentHealthVisitCounts(PlainDate startDate, PlainDate endDate) {
        Collection<HealthLog> healthLogs = loadHealthLogs(startDate, endDate);

        HashSet loggedPersonOids = new HashSet();

        int totalStudentVisitsCount = 0;

        for (HealthLog healthLog : healthLogs) {
            String personOid = healthLog.getPersonOid();

            if (!loggedPersonOids.contains(personOid)) {
                String visitType = healthLog.getVisitType();
                if (!VISIT_TYPE_SCREENING.equalsIgnoreCase(visitType)) {
                    loggedPersonOids.add(personOid);
                    totalStudentVisitsCount++;
                }
            }
        }

        m_counts.put(REPORT_LOCATION_TOTAL_STUDENT_VISITS, Integer.valueOf(totalStudentVisitsCount));
    }

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
         * Constants: Parameters, Constants
         */
        protected static final String PARAM_END_DATE = "endDate";
        protected static final String PARAM_START_DATE = "startDate";

        /*
         * Instance variables.
         */
        protected PlainDate m_endDate;
        protected EnrollmentManager m_enrollmentManager;
        protected StudentHistoryHelper m_helper;
        protected PlainDate m_startDate;
        protected ArrayList<String> m_students;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {

            m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

            m_helper = new StdHelperForIncludeStatusCode40(this);

            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.valueOf(true));
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
            setQuery(m_helper.getStudentQuery(false));

        }

        /**
         * Get Active Students.
         *
         * @return List<String>
         */
        protected List<String> getActiveStudents() {
            if (m_students == null) {
                m_students = new ArrayList<String>();
                QueryIterator iterator = getBroker().getIteratorByQuery(getQuery());
                try {
                    while (iterator.hasNext()) {
                        SisStudent student = (SisStudent) iterator.next();
                        // if (!m_helper.getStudentEnrollmentSpans(student, true).isEmpty())
                        if (hasMembershipDays(student)) {

                            m_students.add(student.getOid());
                        }
                    }
                } finally {
                    iterator.close();
                }
            }

            return m_students;
        }

        /**
         * Checks for membership days.
         *
         * @param student SisStudent
         * @return true, if successful
         */
        protected boolean hasMembershipDays(SisStudent student) {
            return m_enrollmentManager.getMembershipDays(student, m_startDate, m_endDate, true) > 1 ? true : false;
        }

        /**
         * Gets the active students selection.
         *
         * @return Selection
         */
        protected Selection getActiveStudentsSelection() {
            // in test site compilation show error:
            // Selection selection = X2BaseBean.newInstance(Selection.class,
            // getBroker().getPersistenceKey());
            // required: Selection
            // found: X2BaseBean
            Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());

            List<String> activeStudents = getActiveStudents();
            for (String oid : activeStudents) {
                // same situation like with selection
                SelectionObject selectedObject = X2BaseBean.newInstance(SelectionObject.class,
                        getBroker().getPersistenceKey());
                selectedObject.setObjectOid(oid);
                selection.addToSelectionObjects(selectedObject);
            }

            selection.setTimestamp(System.currentTimeMillis());
            getBroker().saveBeanForced(selection);

            return selection;
        }

        /**
         * Set Start Date.
         *
         * @param startDate void
         */
        public void setStartDate(PlainDate startDate) {
            m_startDate = startDate;
        }

        /**
         * Set End Date.
         *
         * @param endDate void
         */
        public void setEndDate(PlainDate endDate) {
            m_endDate = endDate;
        }

    }

}
