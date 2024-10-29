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
package com.x2dev.reports.sys.health.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentJournal;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class HealthHistoryAndAppraisalData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class HealthHistoryAndAppraisalData extends ReportJavaSourceNet {
    // aliases used

    private static final String ALIAS_GS_BLOOD_PR = "hsc-blood-pr";
    private static final String ALIAS_GS_BMI = "hsc-general-bmi";
    private static final String ALIAS_GS_HEIGHT = "hsc-general-height-in";
    private static final String ALIAS_GS_WEIGHT = "hsc-general-weight-lbs";

    private static final String ALIAS_HS_LAI = "hsc-hearing-lai";
    private static final String ALIAS_HS_RAI = "hsc-hearing-rai";

    private static final String ALIAS_HSC_PHYSICIAN = "hsc-physical-physician";
    private static final String ALIAS_REPORT_LOCATION = "report-location";

    private static final String ALIAS_TB_IGRA = "hsc-tb-igra";
    private static final String ALIAS_TB_MANTOUX = "hsc-tb-mantoux";
    private static final String ALIAS_TB_READDATE = "hsc-tb-readdate";
    private static final String ALIAS_TB_TESTDATE = "hsc-tb-testdate";

    private static final String ALIAS_VS_BVA = "hsc-vision-bva";
    private static final String ALIAS_VS_BVAL = "hsc-vision-bval";
    private static final String ALIAS_VS_CB = "hsc-vision-cb";
    private static final String ALIAS_VS_LVA = "hsc-vision-lva";
    private static final String ALIAS_VS_LVAL = "hsc-vision-lval";
    private static final String ALIAS_VS_MB = "hsc-vision-mb";
    private static final String ALIAS_VS_RVA = "hsc-vision-rva";
    private static final String ALIAS_VS_RVAL = "hsc-vision-rval";

    private static final String ALIAS_XR_DATE = "hsc-tb-cxray";
    private static final String ALIAS_XR_RESULT = "hsc-tb-result";

    private static final String ALIAS_YEAR_DIAGNOSED = "year-diagnosed";

    private static final String DEFAULT_TYPE_OF_EXAM = "PE";

    // report fields
    private static final String FIELD_COLOR_PERCEPTION = "color-perception";
    private static final String FIELD_CONDITIONS = "health-conditions";
    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";

    private static final String FIELD_GS_AGE = "gs-age";
    private static final String FIELD_GS_BLOOD_PR = "gs-blood-pr";
    private static final String FIELD_GS_BMI = "gs-bmi";
    private static final String FIELD_GS_DATE = "gs-date";
    private static final String FIELD_GS_GRADE = "gs-grade";
    private static final String FIELD_GS_HEIGHT = "gs-height";
    private static final String FIELD_GS_WEIGHT = "gs-weight";

    private static final String FIELD_HS_DATE = "hs-date";
    private static final String FIELD_HS_LAI = "hs-lai";
    private static final String FIELD_HS_RAI = "hs-rai";

    private static final String FIELD_IMMUNIZATION = "immunization";
    private static final String FIELD_PARENT = "parent";

    private static final String FIELD_PHY_EXAMINATIONS_AGE = "pe-age";
    private static final String FIELD_PHY_EXAMINATIONS_DATE = "pe-date";
    private static final String FIELD_PHY_EXAMINATIONS_GRADE = "pe-grade";
    private static final String FIELD_PHY_EXAMINATIONS_MED_PROVIDER = "pe-medicalProvider";
    private static final String FIELD_PHY_EXAMINATIONS_SIGNIF_FINDING = "pe-significantFinding";
    private static final String FIELD_PHY_EXAMINATIONS_TYPE_OF_EXAM = "pe-typeOfExam";

    private static final String FIELD_SCREENING_GENERAL = "general-screening";
    private static final String FIELD_SCREENING_HEARING = "hearing-screening";
    private static final String FIELD_SCREENING_LEAD = "lead-screening";
    private static final String FIELD_SCREENING_SCOLIOSIS = "scoliosis-screening";
    private static final String FIELD_SCREENING_TB = "tb-screening";
    private static final String FIELD_SCREENING_VISION = "vision-screening";
    private static final String FIELD_SCREENING_XR = "xr-screening";

    private static final String FIELD_SS_DATE = "ss-date";
    private static final String FIELD_SS_RESULT = "ss-result";

    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_JOURNAL_DATE = "sj-date";
    private static final String FIELD_STUDENT_JOURNAL_COMMENT = "sj-comment";

    private static final String FIELD_TB_IGRA = "tb-igra";
    private static final String FIELD_TB_MANTOUX = "tb-mantoux";
    private static final String FIELD_TB_READDATE = "tb-readdate";
    private static final String FIELD_TB_TESTDATE = "tb-testdate";

    private static final String FIELD_VS_BVA = "vs-bva";
    private static final String FIELD_VS_BVAL = "vs-bval";
    private static final String FIELD_VS_CB = "vs-cb";
    private static final String FIELD_VS_COLOR_PERCEPTION_DATE = "vs-cp-date";
    private static final String FIELD_VS_DATE = "vs-date";
    private static final String FIELD_VS_LVA = "vs-lva";
    private static final String FIELD_VS_LVAL = "vs-lval";
    private static final String FIELD_VS_MB = "vs-mb";
    private static final String FIELD_VS_RVA = "vs-rva";
    private static final String FIELD_VS_RVAL = "vs-rval";

    private static final String FIELD_XR_DATE = "tb-cxray";
    private static final String FIELD_XR_RESULT = "tb-result";

    private static final int FIRST_EMERGENCY_PRIORITY = 1;
    private static final String HS_RESULT_CODE_RT = "Health Screening Result Codes";

    private static final String IM_DOSE_DATE = "dose-date";
    private static final String IM_SERIES_ID = "series-id";

    // field keys that are mainly used in datasource maps
    private static final String LEAD_DATE = "lead-date";
    private static final String LEAD_RESULT = "lead-result";

    // indicators to base different report parts on same datasource
    private static final String PART_INDICATOR_HEALTH_STUDENT = "HEALTH_STUDENT_JOURNAL";
    private static final String PART_INDICATOR_PHY_EXEMPTIONS = "PHY_EXAMINATIONS_INDICATOR";
    private static final String PART_INDICATOR_PHYSICAL_EXEMPTIONS_FIELD = "INDICATOR";

    // input parameters
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";

    // record limits for the different report parts (according report template)
    private static int RECORDS_LIMIT_PHY_EXEMPTIONS_PART = 17;
    private static int RECORDS_LIMIT_HEALTH_STUDENT_PART = 27;
    private static int RECORDS_LIMIT_SCREENING = 15;
    private static int RECORDS_LIMIT_LEAD_SCREENING = 4;
    private static int RECORDS_LIMIT_TB_SCREENING = 2;
    private static int RECORDS_LIMIT_XRAY_SCREENING = 4;
    private static int RECORDS_LIMIT_SCOLIOSIS_SCREENING = 5;
    private static int RECORDS_LIMIT_IMMUNIZATION = 5;

    // extended data dictionaries used
    private static final String SCREENING_TYPE_PHYSICAL = "HSC-PHYSICAL";
    private static final String SCREENING_TYPE_GENERAL = "HSC-GENERAL";
    private static final String SCREENING_TYPE_VISION = "HSC-VISION";
    private static final String SCREENING_TYPE_HEARING = "HSC-HEARING";
    private static final String SCREENING_TYPE_SCOLIOSIS = "HSC-POSTURAL";
    private static final String SCREENING_TYPE_LEAD = "HSC-LEAD";
    private static final String SCREENING_TYPE_TB_XRAY = "HSC-TB";

    private static final String STUDENT_JOURNAL_TYPE_HEALTH = "Health";

    // subreport format parameters
    private static final String SUBREPORT_ID_FIRST_PAGE = "SYS-HTH-APR-NJ-P1";
    private static final String SUBREPORT_ID_PARAMETER_SUB_P1 = "P1_FORMAT";
    private static final String SUBREPORT_ID_PARAMETER_SUB_P2 = "P2_FORMAT";
    private static final String SUBREPORT_ID_SECOND_PAGE = "SYS-HTH-APR-NJ-P2";


    DataDictionary m_dictionary = null;
    // immunization dose records keyed by student id and immunization category
    Map<String, Map<String, List<HealthImmunizationDose>>> m_healthImmunizationDoseRecords;
    // screening code reference table
    Map<String, ReferenceCode> m_healthScreeningResultCodes = new HashMap<String, ReferenceCode>();
    SimpleDateFormat m_parseDate = new SimpleDateFormat("yyyy-MM-dd");
    // report wide variables used to compose data source
    Map<String, SisStudent> m_students = new HashMap<String, SisStudent>();
    // health condition map keyed by student oid and report location (ref. map alias)
    Map<String, Map<String, Integer>> m_studentsHealthConditionMap = null;
    // screening records keyed by student oid and screening type
    Map<String, Map<String, List<HealthScreening>>> m_studentsHealthScreening =
            new HashMap<String, Map<String, List<HealthScreening>>>();


    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        addReportFormatParameter(SUBREPORT_ID_FIRST_PAGE, SUBREPORT_ID_PARAMETER_SUB_P1);
        addReportFormatParameter(SUBREPORT_ID_SECOND_PAGE, SUBREPORT_ID_PARAMETER_SUB_P2);

        ReportDataGrid reportGrid = new ReportDataGrid();

        for (SisStudent student : m_students.values()) {
            ReportDataGrid firstPageGrid = getFirstPageData(student);
            ReportDataGrid secondPageGrid = getSecondPageData(student);

            prepareReportPage(reportGrid, firstPageGrid, FIELD_DATA, SUBREPORT_ID_PARAMETER_SUB_P1, FIELD_FORMAT);
            prepareReportPage(reportGrid, secondPageGrid, FIELD_DATA, SUBREPORT_ID_PARAMETER_SUB_P2, FIELD_FORMAT);
        }

        addParameter(FIELD_CONDITIONS, m_studentsHealthConditionMap);

        reportGrid.beforeTop();
        return reportGrid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        initializeStudents();

        initializeHealthScreening();

        initializeImmunizationDoses();

        initializeHealthConditionsMap();
    }

    /**
     * prepare subreport format.
     *
     * @param reportFormatID String
     * @param formatParameterID String
     */
    private void addReportFormatParameter(String reportFormatID,
                                          String formatParameterID) {
        Report pageSubreportFormat = ReportUtils.getReport(reportFormatID, getBroker());
        if (pageSubreportFormat != null) {
            addParameter(formatParameterID, new ByteArrayInputStream(pageSubreportFormat.getCompiledFormat()));
        }
    }

    /**
     * Adds criteria based on user input.
     *
     * @param criteria Criteria
     */
    private void applyUserCriteria(Criteria criteria) {
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        switch (queryBy) {
            case 0: // All
                break;

            case 1: // State ID
                criteria.addEqualTo(SisStudent.COL_STATE_ID, getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // Grade level
                criteria.addEqualTo(SisStudent.COL_GRADE_LEVEL, getParameter(QUERY_STRING_PARAM));
                break;

            case 3: // Current selection
                criteria = getCurrentCriteria();
                break;

            case 4: // Snapshot
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }
    }

    /**
     * Get xray screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of xray screening parameters for particular student
     */
    private List<Map<String, Object>> getChestXRayScreening(SisStudent student) {
        List<Map<String, Object>> chestXRayScreeningList = new ArrayList<Map<String, Object>>();

        String resultCode;

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_TB_XRAY)) {

            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_TB_XRAY)) {

                // limitations like on report sample. Clarify if we need it?
                if (chestXRayScreeningList.size() > RECORDS_LIMIT_XRAY_SCREENING) {
                    break;
                }

                DataDictionary xrScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                // keep Chest X-Ray records only
                if (screening.getFieldValueByAlias(ALIAS_XR_DATE, xrScreeningDictionary) != null) {

                    Map<String, Object> chestXRayScreeningRecord = new HashMap<String, Object>();
                    PlainDate xrayDate = null;

                    try {
                        if (screening.getFieldValueByAlias(ALIAS_XR_DATE, xrScreeningDictionary) != null) {
                            xrayDate = new PlainDate(m_parseDate
                                    .parse((String) screening.getFieldValueByAlias(ALIAS_XR_DATE,
                                            xrScreeningDictionary)));
                        }
                    } catch (ParseException e) {
                        // do nothing
                    }

                    chestXRayScreeningRecord.put(FIELD_XR_DATE, xrayDate);
                    resultCode =
                            (String) screening.getFieldValueByAliasExtended(ALIAS_XR_RESULT, xrScreeningDictionary);
                    chestXRayScreeningRecord.put(FIELD_XR_RESULT, getHealthScreeningResultCodeLocal(resultCode));
                    chestXRayScreeningList.add(chestXRayScreeningRecord);
                }

            }
        }

        // limitations like on report sample. Clarify if we need it?
        while (chestXRayScreeningList.size() < RECORDS_LIMIT_XRAY_SCREENING + 1) {
            Map<String, Object> chestXRayScreeningRecord = new HashMap<String, Object>();
            chestXRayScreeningList.add(chestXRayScreeningRecord);
        }

        return chestXRayScreeningList;
    }

    /**
     * Get color perception screening data for particular student.
     *
     * @param student SisStudent
     * @return color perception screening parameters for particular student
     */
    private Map<String, Object> getColorPerceptionScreening(SisStudent student) {
        List<Map<String, Object>> colorPerceptionList = new ArrayList<Map<String, Object>>();

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_VISION)
                && !m_studentsHealthScreening.get(student.getOid()).get(SCREENING_TYPE_VISION).isEmpty()) {


            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_VISION)) {

                DataDictionary visionScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                if (screening.getFieldValueByAliasExtended(ALIAS_VS_CB, visionScreeningDictionary) != null) {
                    Map<String, Object> colorPerceptionRecord = new HashMap<String, Object>();
                    colorPerceptionRecord.put(FIELD_VS_COLOR_PERCEPTION_DATE, screening.getDate());
                    String colorPerceptionCode =
                            (String) screening.getFieldValueByAliasExtended(ALIAS_VS_CB, visionScreeningDictionary);
                    colorPerceptionRecord.put(FIELD_VS_CB, getHealthScreeningResultCodeLocal(colorPerceptionCode));
                    colorPerceptionList.add(colorPerceptionRecord);
                }
            }
        }
        // get latest result
        Collections.sort(colorPerceptionList, getScreeningComparatorByDate(FIELD_VS_COLOR_PERCEPTION_DATE));

        return ((!colorPerceptionList.isEmpty()) ? (colorPerceptionList.get(0)) : (new HashMap<String, Object>()));
    }

    /**
     * populate report first page subreport data.
     *
     * @param student SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getFirstPageData(SisStudent student) {
        ReportDataGrid firstPageGrid = new ReportDataGrid();
        firstPageGrid.append();
        firstPageGrid.set(FIELD_STUDENT, student);
        firstPageGrid.set(FIELD_PARENT, student.getContact(FIRST_EMERGENCY_PRIORITY));
        firstPageGrid.set(FIELD_SCREENING_GENERAL, getGeneralScreening(student));
        firstPageGrid.set(FIELD_SCREENING_VISION, getVisionScreening(student));
        firstPageGrid.set(FIELD_SCREENING_HEARING, getHearingScreening(student));
        firstPageGrid.set(FIELD_COLOR_PERCEPTION, getColorPerceptionScreening(student));
        firstPageGrid.set(FIELD_SCREENING_SCOLIOSIS, getScoliosisScreening(student));
        firstPageGrid.set(FIELD_IMMUNIZATION, getImmunizationData(student));
        firstPageGrid.set(FIELD_SCREENING_LEAD, getLeadScreening(student));
        firstPageGrid.set(FIELD_SCREENING_TB, getTBScreening(student));
        firstPageGrid.set(FIELD_SCREENING_XR, getChestXRayScreening(student));

        return firstPageGrid;
    }

    /**
     * Get general screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of general screening parameters for particular student
     */
    private List<Map<String, Object>> getGeneralScreening(SisStudent student) {

        List<Map<String, Object>> generalScreeningList = new ArrayList<Map<String, Object>>();

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_GENERAL)) {
            // General screening processing
            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_GENERAL)) {

                // limitations like on report sample. Clarify if we need it?
                if (generalScreeningList.size() > RECORDS_LIMIT_SCREENING) {
                    break;
                }

                DataDictionary generalScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                Map<String, Object> generalScreeningRecord = new HashMap<String, Object>();

                generalScreeningRecord.put(FIELD_GS_DATE, screening.getDate());
                generalScreeningRecord.put(FIELD_GS_GRADE, student.getGradeLevel());
                generalScreeningRecord.put(FIELD_GS_AGE, String.valueOf(student.getPerson().getAge()));

                generalScreeningRecord.put(FIELD_GS_HEIGHT,
                        screening.getFieldValueByAliasExtended(ALIAS_GS_HEIGHT, generalScreeningDictionary));
                generalScreeningRecord.put(FIELD_GS_WEIGHT,
                        screening.getFieldValueByAliasExtended(ALIAS_GS_WEIGHT, generalScreeningDictionary));
                generalScreeningRecord.put(FIELD_GS_BMI,
                        screening.getFieldValueByAliasExtended(ALIAS_GS_BMI, generalScreeningDictionary));
                generalScreeningRecord.put(FIELD_GS_BLOOD_PR,
                        screening.getFieldValueByAliasExtended(ALIAS_GS_BLOOD_PR, generalScreeningDictionary));

                generalScreeningList.add(generalScreeningRecord);
            }
        }

        Collections.sort(generalScreeningList, getScreeningComparatorByDate(FIELD_GS_DATE));

        // limitations like on report sample. Clarify if we need it?
        while (generalScreeningList.size() < RECORDS_LIMIT_SCREENING + 1) {
            Map<String, Object> generalScreeningRecord = new HashMap<String, Object>();
            generalScreeningList.add(generalScreeningRecord);
        }

        return generalScreeningList;
    }

    /**
     * Get local ref. code for Health Screening Result Code ref. table.
     *
     * @param code String
     * @return local code if exist, original code otherwise
     */
    private String getHealthScreeningResultCodeLocal(String code) {
        String localCode = code;
        if (m_healthScreeningResultCodes.containsKey(code)) {
            localCode = m_healthScreeningResultCodes.get(code).getLocalCode();
        }

        return localCode;
    }

    /**
     * Prepare datasource for the second page. There will be datagrid for both tables on report
     * separated by INDICATOR field.
     *
     * @param student SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getHealthStudentJournal(SisStudent student) {
        X2Criteria studentJournalCriteria = new X2Criteria();
        studentJournalCriteria.addEqualTo(StudentJournal.COL_STUDENT_OID, student.getOid());
        studentJournalCriteria.addEqualToIgnoreCase(StudentJournal.COL_TYPE, STUDENT_JOURNAL_TYPE_HEALTH);

        QueryByCriteria healthStudentJournalQuery = new QueryByCriteria(StudentJournal.class, studentJournalCriteria);
        Collection<StudentJournal> journalEntries = getBroker().getCollectionByQuery(healthStudentJournalQuery);

        ReportDataGrid healthStudentJournalGrid = new ReportDataGrid();

        for (StudentJournal journalEntry : journalEntries) {
            // limitations like on report sample. Clarify if we need it?
            if (healthStudentJournalGrid.rowCount() > RECORDS_LIMIT_HEALTH_STUDENT_PART + 1) {
                break;
            }
            healthStudentJournalGrid.append();
            healthStudentJournalGrid.set(FIELD_STUDENT_JOURNAL_DATE, journalEntry.getDate());
            healthStudentJournalGrid.set(FIELD_STUDENT_JOURNAL_COMMENT, journalEntry.getComment());
            healthStudentJournalGrid.set(PART_INDICATOR_PHYSICAL_EXEMPTIONS_FIELD, PART_INDICATOR_HEALTH_STUDENT);

        }

        // limitations like on report sample. Clarify if we need it?
        while (healthStudentJournalGrid.rowCount() < RECORDS_LIMIT_HEALTH_STUDENT_PART + 1) {
            healthStudentJournalGrid.append();
            healthStudentJournalGrid.set(PART_INDICATOR_PHYSICAL_EXEMPTIONS_FIELD, PART_INDICATOR_HEALTH_STUDENT);
        }

        healthStudentJournalGrid.beforeTop();
        return healthStudentJournalGrid;
    }

    /**
     * Get hearing screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of hearing screening parameters for particular student
     */
    private List<Map<String, Object>> getHearingScreening(SisStudent student) {
        List<Map<String, Object>> studentHearingScreeningList = new ArrayList<Map<String, Object>>();
        String hearingRightLocalCode;
        String hearingLeftLocalCode;

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_HEARING)) {


            // Hearing screening processing
            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_HEARING)) {

                // limitations like on report sample. Clarify if we need it?
                if (studentHearingScreeningList.size() > RECORDS_LIMIT_SCREENING) {
                    break;
                }

                DataDictionary hearingScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                Map<String, Object> hearingScreeningRecord = new HashMap<String, Object>();
                hearingScreeningRecord.put(FIELD_HS_DATE, screening.getDate());

                hearingRightLocalCode =
                        getHealthScreeningResultCodeLocal((String) screening.getFieldValueByAliasExtended(ALIAS_HS_RAI,
                                hearingScreeningDictionary));
                hearingScreeningRecord.put(FIELD_HS_RAI, hearingRightLocalCode);
                hearingLeftLocalCode =
                        getHealthScreeningResultCodeLocal((String) screening.getFieldValueByAliasExtended(ALIAS_HS_LAI,
                                hearingScreeningDictionary));
                hearingScreeningRecord.put(FIELD_HS_LAI, hearingLeftLocalCode);
                studentHearingScreeningList.add(hearingScreeningRecord);
            }
        }

        Collections.sort(studentHearingScreeningList, getScreeningComparatorByDate(FIELD_HS_DATE));

        // limitations like on report sample. Clarify if we need it?
        while (studentHearingScreeningList.size() < RECORDS_LIMIT_SCREENING + 1) {
            Map<String, Object> studentScreeningRecord = new HashMap<String, Object>();
            studentHearingScreeningList.add(studentScreeningRecord);
        }

        return studentHearingScreeningList;
    }

    /**
     * Get immunicatoin data for particular student.
     *
     * @param student SisStudent
     * @return Map<Health Immunization category, List of series with parameters>
     */
    private Map<String, List<Map<String, Object>>> getImmunizationData(SisStudent student) {

        Map<String, List<Map<String, Object>>> immunizationDosesMap = new HashMap<String, List<Map<String, Object>>>();

        if (m_healthImmunizationDoseRecords.containsKey(student.getOid())
                && !m_healthImmunizationDoseRecords.get(student.getOid()).keySet().isEmpty()) {
            for (String immunizationCategory : m_healthImmunizationDoseRecords.get(student.getOid()).keySet()) {
                List<Map<String, Object>> healthImmunizationDoses = new ArrayList<Map<String, Object>>();
                if (!m_healthImmunizationDoseRecords.get(student.getOid()).get(immunizationCategory).isEmpty()) {
                    for (HealthImmunizationDose immunizationDose : m_healthImmunizationDoseRecords.get(student.getOid())
                            .get(immunizationCategory)) {
                        Map<String, Object> immunizationDoseRecord = new HashMap<String, Object>();
                        immunizationDoseRecord.put(IM_DOSE_DATE, immunizationDose.getDate());
                        if (immunizationDose.getImmunizationSeries() != null
                                && immunizationDose.getImmunizationSeries().getImmunizationDefinition() != null) {
                            String seriesId =
                                    immunizationDose.getImmunizationSeries().getImmunizationDefinition().getSeriesId();
                            if (seriesId.equalsIgnoreCase("TD") || seriesId.equalsIgnoreCase("DT")
                                    || seriesId.equalsIgnoreCase("OPV")) {
                                immunizationDoseRecord.put(IM_SERIES_ID, seriesId);
                            } else {
                                immunizationDoseRecord.put(IM_SERIES_ID, null);
                            }
                        } else {
                            immunizationDoseRecord.put(IM_SERIES_ID, null);
                        }
                        healthImmunizationDoses.add(immunizationDoseRecord);
                        // limitations like on report sample. Clarify if we need it?
                        if (healthImmunizationDoses.size() > RECORDS_LIMIT_SCREENING) {
                            break;
                        }
                    }
                }
                // limitations like on report sample. Clarify if we need it?
                while (healthImmunizationDoses.size() < RECORDS_LIMIT_IMMUNIZATION + 1) {
                    Map<String, Object> immunizationDoseRecord = new HashMap<String, Object>();
                    immunizationDoseRecord.put(IM_DOSE_DATE, null);
                    immunizationDoseRecord.put(IM_SERIES_ID, null);
                    healthImmunizationDoses.add(immunizationDoseRecord);
                }

                immunizationDosesMap.put(immunizationCategory, healthImmunizationDoses);
            }
        }

        return immunizationDosesMap;
    }

    /**
     * Get lead screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of lead screening parameters for particular student
     */
    private List<Map<String, Object>> getLeadScreening(SisStudent student) {
        List<Map<String, Object>> leadScreeningList = new ArrayList<Map<String, Object>>();
        String leadScreeningResult;

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_LEAD)) {

            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_LEAD)) {

                // limitations like on report sample. Clarify if we need it?
                if (leadScreeningList.size() > RECORDS_LIMIT_LEAD_SCREENING) {
                    break;
                }

                Map<String, Object> leadScreeningRecord = new HashMap<String, Object>();
                leadScreeningRecord.put(LEAD_DATE, screening.getDate());
                leadScreeningResult = screening.getResultCode();
                leadScreeningRecord.put(LEAD_RESULT, getHealthScreeningResultCodeLocal(leadScreeningResult));
                leadScreeningList.add(leadScreeningRecord);
            }

        }

        Collections.sort(leadScreeningList, getScreeningComparatorByDate(LEAD_DATE));

        // limitations like on report sample. Clarify if we need it?
        while (leadScreeningList.size() < RECORDS_LIMIT_SCREENING + 1) {
            Map<String, Object> leadScreeningRecord = new HashMap<String, Object>();
            leadScreeningList.add(leadScreeningRecord);
        }

        return leadScreeningList;
    }

    /**
     * get physical examination records.
     *
     * @param student SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getPhysicalExaminations(SisStudent student) {
        ReportDataGrid physicalExaminationsGrid = new ReportDataGrid();

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_PHYSICAL)) {
            Collection<HealthScreening> screenings =
                    m_studentsHealthScreening.get(student.getOid()).get(SCREENING_TYPE_PHYSICAL);

            for (HealthScreening screening : screenings) {

                // limitations like on report sample. Clarify if we need it?
                if (physicalExaminationsGrid.rowCount() > RECORDS_LIMIT_PHY_EXEMPTIONS_PART + 1) {
                    break;
                }

                DataDictionary physicalScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                physicalExaminationsGrid.append();
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_DATE, screening.getDate());
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_GRADE, screening.getStudent().getGradeLevel());
                // seems that grade should be calculated as of screening date here
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_AGE,
                        Integer.toString(screening.getStudent().getPerson().getAge()));
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_TYPE_OF_EXAM, DEFAULT_TYPE_OF_EXAM);
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_SIGNIF_FINDING, screening.getSummaryView());
                physicalExaminationsGrid.set(FIELD_PHY_EXAMINATIONS_MED_PROVIDER,
                        screening.getFieldValueByAliasExtended(ALIAS_HSC_PHYSICIAN, physicalScreeningDictionary));
                physicalExaminationsGrid.set(PART_INDICATOR_PHYSICAL_EXEMPTIONS_FIELD, PART_INDICATOR_PHY_EXEMPTIONS);
            }
        }

        // limitations like on report sample. Clarify if we need it?
        while (physicalExaminationsGrid.rowCount() < RECORDS_LIMIT_PHY_EXEMPTIONS_PART + 1) {
            physicalExaminationsGrid.append();
            physicalExaminationsGrid.set(PART_INDICATOR_PHYSICAL_EXEMPTIONS_FIELD, PART_INDICATOR_PHY_EXEMPTIONS);
        }

        physicalExaminationsGrid.beforeTop();
        return physicalExaminationsGrid;
    }

    /**
     * Get scoliosis screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of scoliosis screening parameters for particular student
     */
    private List<Map<String, Object>> getScoliosisScreening(SisStudent student) {
        List<Map<String, Object>> studentScoliosisScreeningList = new ArrayList<Map<String, Object>>();
        String scoliosisResultCode;
        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_SCOLIOSIS)) {

            // Scoliosis screening processing
            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_SCOLIOSIS)) {

                // limitations like on report sample. Clarify if we need it?
                if (studentScoliosisScreeningList.size() > RECORDS_LIMIT_SCOLIOSIS_SCREENING) {
                    break;
                }

                Map<String, Object> scoliosisScreeningRecord = new HashMap<String, Object>();
                scoliosisScreeningRecord.put(FIELD_SS_DATE, screening.getDate());
                scoliosisResultCode = screening.getResultCode();
                scoliosisScreeningRecord.put(FIELD_SS_RESULT, getHealthScreeningResultCodeLocal(scoliosisResultCode));
                studentScoliosisScreeningList.add(scoliosisScreeningRecord);
            }
        }

        // limitations like on report sample. Clarify if we need it?
        while (studentScoliosisScreeningList.size() < RECORDS_LIMIT_SCREENING + 1) {
            Map<String, Object> scoliosisScreeningRecord = new HashMap<String, Object>();
            studentScoliosisScreeningList.add(scoliosisScreeningRecord);
        }

        return studentScoliosisScreeningList;

    }

    /**
     * Get comparator for Map (that's general data structure for report fields.
     *
     * @param dateField - date field name that will be used for sorting
     * @return comparator for collections sorting
     */
    private Comparator<Map<String, Object>> getScreeningComparatorByDate(final String dateField) {
        return new Comparator<Map<String, Object>>() {

            @Override
            public int compare(Map<String, Object> item1, Map<String, Object> item2) {
                PlainDate date1 = (PlainDate) item1.get(dateField);
                PlainDate date2 = (PlainDate) item2.get(dateField);
                int result = 0;
                if (date1 != null && date2 != null) {
                    result = date1.compareTo(date2);
                }
                return result;
            }
        };
    }

    /**
     * populate report second page subreport data for particular student.
     *
     * @param student SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getSecondPageData(SisStudent student) {
        ReportDataGrid secondPageGrid = new ReportDataGrid(10);

        secondPageGrid.append(getPhysicalExaminations(student));
        secondPageGrid.append(getHealthStudentJournal(student));

        return secondPageGrid;
    }

    /**
     * Get TB screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of TB screening parameters for particular student
     */
    private List<Map<String, Object>> getTBScreening(SisStudent student) {
        List<Map<String, Object>> TBScreeningList = new ArrayList<Map<String, Object>>();

        String tbMantouxResultCode, tbIgraResultCode;

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_TB_XRAY)) {

            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_TB_XRAY)) {

                DataDictionary tbScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                // limitations like on report sample. Clarify if we need it?
                if (TBScreeningList.size() > RECORDS_LIMIT_TB_SCREENING) {
                    break;
                }

                Map<String, Object> tbScreeningRecord = new HashMap<String, Object>();

                PlainDate testDate = null;
                PlainDate readDate = null;

                try {
                    if (screening.getFieldValueByAlias(ALIAS_TB_TESTDATE, tbScreeningDictionary) != null) {
                        testDate = new PlainDate(m_parseDate
                                .parse((String) screening.getFieldValueByAlias(ALIAS_TB_TESTDATE,
                                        tbScreeningDictionary)));
                    }
                    if (screening.getFieldValueByAlias(ALIAS_TB_READDATE, tbScreeningDictionary) != null) {
                        readDate = new PlainDate(m_parseDate
                                .parse((String) screening.getFieldValueByAlias(ALIAS_TB_READDATE,
                                        tbScreeningDictionary)));
                    }
                } catch (ParseException e) {
                    // do nothing
                }

                tbScreeningRecord.put(FIELD_TB_TESTDATE, testDate);
                tbScreeningRecord.put(FIELD_TB_READDATE, readDate);
                tbMantouxResultCode =
                        (String) screening.getFieldValueByAliasExtended(ALIAS_TB_MANTOUX, tbScreeningDictionary);
                tbIgraResultCode =
                        (String) screening.getFieldValueByAliasExtended(ALIAS_TB_IGRA, tbScreeningDictionary);
                tbScreeningRecord.put(FIELD_TB_MANTOUX, getHealthScreeningResultCodeLocal(tbMantouxResultCode));
                tbScreeningRecord.put(FIELD_TB_IGRA, getHealthScreeningResultCodeLocal(tbIgraResultCode));
                if (tbScreeningRecord.get(FIELD_TB_TESTDATE) != null
                        || tbScreeningRecord.get(FIELD_TB_READDATE) != null
                        || tbScreeningRecord.get(ALIAS_TB_MANTOUX) != null
                        || tbScreeningRecord.get(FIELD_TB_IGRA) != null) {
                    TBScreeningList.add(tbScreeningRecord);
                }
            }
        }

        // limitations like on report sample. Clarify if we need it?
        while (TBScreeningList.size() < RECORDS_LIMIT_TB_SCREENING + 1) {
            Map<String, Object> tbScreeningRecord = new HashMap<String, Object>();
            TBScreeningList.add(tbScreeningRecord);
        }

        return TBScreeningList;
    }

    /**
     * Get vision screening data for particular student.
     *
     * @param student SisStudent
     * @return collection of vision screening parameters for particular student
     */
    private List<Map<String, Object>> getVisionScreening(SisStudent student) {
        List<Map<String, Object>> visionScreeningList = new ArrayList<Map<String, Object>>();
        String muscleBalanceCode;

        if (m_studentsHealthScreening.containsKey(student.getOid())
                && m_studentsHealthScreening.get(student.getOid()).containsKey(SCREENING_TYPE_VISION)) {

            // Vision screening processing
            for (HealthScreening screening : m_studentsHealthScreening.get(student.getOid())
                    .get(SCREENING_TYPE_VISION)) {

                // limitations like on report sample. Clarify if we need it?
                if (visionScreeningList.size() > RECORDS_LIMIT_SCREENING) {
                    break;
                }

                DataDictionary visionScreeningDictionary =
                        DataDictionary.getDistrictDictionary(screening.getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());

                Map<String, Object> visionScreeningRecord = new HashMap<String, Object>();
                visionScreeningRecord.put(FIELD_VS_DATE, screening.getDate());
                visionScreeningRecord.put(FIELD_VS_RVA,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_RVA, visionScreeningDictionary));
                visionScreeningRecord.put(FIELD_VS_LVA,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_LVA, visionScreeningDictionary));
                visionScreeningRecord.put(FIELD_VS_BVA,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_BVA, visionScreeningDictionary));
                visionScreeningRecord.put(FIELD_VS_RVAL,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_RVAL, visionScreeningDictionary));
                visionScreeningRecord.put(FIELD_VS_LVAL,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_LVAL, visionScreeningDictionary));
                visionScreeningRecord.put(FIELD_VS_BVAL,
                        screening.getFieldValueByAliasExtended(ALIAS_VS_BVAL, visionScreeningDictionary));

                muscleBalanceCode =
                        (String) screening.getFieldValueByAliasExtended(ALIAS_VS_MB, visionScreeningDictionary);
                visionScreeningRecord.put(FIELD_VS_MB, getHealthScreeningResultCodeLocal(muscleBalanceCode));

                visionScreeningList.add(visionScreeningRecord);
            }
        }

        Collections.sort(visionScreeningList, getScreeningComparatorByDate(FIELD_VS_DATE));

        // limitations like on report sample. Clarify if we need it?
        while (visionScreeningList.size() < RECORDS_LIMIT_SCREENING + 1) {
            Map<String, Object> visionScreeningRecord = new HashMap<String, Object>();
            visionScreeningList.add(visionScreeningRecord);
        }

        return visionScreeningList;

    }

    /**
     * Initialize health condition map.
     */
    private void initializeHealthConditionsMap() {

        // initialize health conditions ref. codes and appropriate report location map
        // report location map used to group health condition codes on report
        DataDictionaryField conditionCodeField =
                m_dictionary.findDataDictionaryField(HealthCondition.class.getName(),
                        HealthCondition.COL_CONDITION_CODE);
        String reportLocationFieldName =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_REPORT_LOCATION).getJavaName();
        X2Criteria medicalConditionCriteria = new X2Criteria();
        medicalConditionCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                conditionCodeField.getReferenceTableOid());
        medicalConditionCriteria.addNotEmpty(reportLocationFieldName, getBroker().getPersistenceKey());
        BeanQuery medicalConditionQuery = new BeanQuery(ReferenceCode.class, medicalConditionCriteria, false);
        Map<String, ReferenceCode> medicalConditionCodes =
                getBroker().getMapByQuery(medicalConditionQuery, ReferenceCode.COL_CODE, 50);
        Map<String, ReferenceCode> reportLocationCodes =
                getBroker().getMapByQuery(medicalConditionQuery, reportLocationFieldName, 20);

        // select students health condition codes
        X2Criteria healthConditionCriteria = new X2Criteria();
        healthConditionCriteria.addIn(HealthCondition.COL_STUDENT_OID, m_students.keySet());
        healthConditionCriteria.addIn(HealthCondition.COL_CONDITION_CODE, medicalConditionCodes.keySet());
        // healthConditionCriteria.
        String yearDiagnosedFieldName =
                m_dictionary.findDataDictionaryFieldByAlias(ALIAS_YEAR_DIAGNOSED).getDatabaseName();
        healthConditionCriteria.addNotEmpty(yearDiagnosedFieldName, getBroker().getPersistenceKey());
        QueryByCriteria healthConditionQuery =
                new QueryByCriteria(HealthCondition.class, healthConditionCriteria);

        Map<String, List<HealthCondition>> healthConditions =
                getBroker().getGroupedCollectionByQuery(healthConditionQuery, HealthCondition.COL_STUDENT_OID, 10);

        // process student health condition values into map that would be used in report
        // student -> medical condition code -> most recent year
        m_studentsHealthConditionMap = new HashMap<String, Map<String, Integer>>();

        for (SisStudent student : m_students.values()) {
            if (!m_studentsHealthConditionMap.containsKey(student.getOid())) {
                m_studentsHealthConditionMap.put(student.getOid(), new HashMap<String, Integer>());
            }
            for (String reportLocation : reportLocationCodes.keySet()) {
                if (!m_studentsHealthConditionMap.get(student.getOid()).containsKey(reportLocation)) {
                    m_studentsHealthConditionMap.get(student.getOid()).put(reportLocation, null);
                }

                if (healthConditions.containsKey(student.getOid())) {
                    // we need to keep most recent year for the same medical code
                    for (HealthCondition healthCondition : healthConditions.get(student.getOid())) {

                        if (medicalConditionCodes.get(healthCondition.getConditionCode())
                                .getFieldValueByAlias(ALIAS_REPORT_LOCATION).toString()
                                .equalsIgnoreCase(reportLocation)) {

                            Integer yearDiagnosed = (healthCondition.getFieldValueByAlias(ALIAS_YEAR_DIAGNOSED) != null)
                                    ? (Integer.valueOf((String) healthCondition.getFieldValueByAlias(ALIAS_YEAR_DIAGNOSED)))
                                    : (Integer.valueOf(0));
                            Integer previousYearDiagnosed =
                                    ((m_studentsHealthConditionMap.get(student.getOid()).get(reportLocation) != null)
                                            ? ((Integer) m_studentsHealthConditionMap.get(student.getOid())
                                                    .get(reportLocation))
                                            : (Integer.valueOf(0)));
                            if (previousYearDiagnosed.intValue() < yearDiagnosed.intValue()) {
                                m_studentsHealthConditionMap.get(student.getOid()).put(reportLocation, yearDiagnosed);
                            }
                        }
                    }
                }
            }
        }

        return;
    }

    /**
     * Initialize health screening map that subject of the report.
     */
    private void initializeHealthScreening() {
        // initialize student health screening
        X2Criteria screeningCriteria = new X2Criteria();
        screeningCriteria.addIn(HealthScreening.COL_STUDENT_OID, m_students.keySet());

        QueryByCriteria studentsHealthScreeningQuery = new QueryByCriteria(HealthScreening.class, screeningCriteria);

        Map<String, List<HealthScreening>> healthScreeningRecords =
                getBroker().getGroupedCollectionByQuery(studentsHealthScreeningQuery,
                        HealthScreening.REL_EXTENDED_DATA_DICTIONARY + PATH_DELIMITER + ExtendedDataDictionary.COL_ID,
                        m_students.size() * 4);

        // student oid -> screening type -> health screening records
        for (String screeningType : healthScreeningRecords.keySet()) {
            for (HealthScreening healthScreening : healthScreeningRecords.get(screeningType)) {
                if (!m_studentsHealthScreening.containsKey(healthScreening.getStudentOid())) {
                    m_studentsHealthScreening.put(healthScreening.getStudentOid(),
                            new HashMap<String, List<HealthScreening>>());
                }
                if (!m_studentsHealthScreening.get(healthScreening.getStudentOid()).containsKey(screeningType)) {
                    m_studentsHealthScreening.get(healthScreening.getStudentOid()).put(screeningType,
                            new ArrayList<HealthScreening>());
                }
                m_studentsHealthScreening.get(healthScreening.getStudentOid()).get(screeningType).add(healthScreening);
            }
        }

        // initialize health screening result codes map
        X2Criteria healthScreeningResultsCodeCriteria = new X2Criteria();
        healthScreeningResultsCodeCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, HS_RESULT_CODE_RT);
        QueryByCriteria healthScreeningResultsCodeQuery =
                new QueryByCriteria(ReferenceTable.class, healthScreeningResultsCodeCriteria);
        ReferenceTable refTable =
                (ReferenceTable) getBroker().getBeanByQuery(healthScreeningResultsCodeQuery);
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                m_healthScreeningResultCodes.put(code.getCode(), code);
            }
        }
    }

    /**
     * Initialize immunization doses map for all students that are subject of the report.
     */
    private void initializeImmunizationDoses() {
        // initialize immunization doses
        X2Criteria immunizationCriteria = new X2Criteria();
        immunizationCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, m_students.keySet());
        QueryByCriteria healthImmunizationDoseQuery =
                new QueryByCriteria(HealthImmunizationDose.class, immunizationCriteria);

        m_healthImmunizationDoseRecords =
                getBroker().getGroupedCollectionByQuery(healthImmunizationDoseQuery,
                        new String[] {HealthImmunizationDose.COL_STUDENT_OID,
                                HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER
                                        + HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER
                                        + HealthImmunizationDefinition.COL_CATEGORIES},
                        new int[] {m_students.size() * 4, 8});
    }

    /**
     * Initialize student map used across the report.
     */
    private void initializeStudents() {
        // initialize students
        X2Criteria studentCriteria = new X2Criteria();
        applyUserCriteria(studentCriteria);

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        m_students = getBroker().getMapByQuery(studentQuery, X2BaseBean.COL_OID, 100);
    }

    /**
     * prepare subreport datasources and formats.
     *
     * @param reportGrid ReportDataGrid
     * @param subreportGrid ReportDataGrid
     * @param dataFieldID String
     * @param reportFormatType String
     * @param formatFieldID String
     */
    private void prepareReportPage(ReportDataGrid reportGrid,
                                   ReportDataGrid subreportGrid,
                                   String dataFieldID,
                                   String reportFormatType,
                                   String formatFieldID) {
        reportGrid.append();
        subreportGrid.beforeTop();
        reportGrid.set(dataFieldID, subreportGrid);
        reportGrid.set(formatFieldID, reportFormatType);
    }

}
