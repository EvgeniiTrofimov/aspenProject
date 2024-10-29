/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2023 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on.register;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.JasperEngine5MultyReportFiller;
import com.x2dev.procedures.statereporting.common.PublishReportsManagerMultyPerBeanJe5;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.procedures.statereporting.on.register.original.OntarioToolHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.struts.util.MessageResources;

import java.io.InputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static com.x2dev.reports.statereporting.on.register.EnrRegByCourseDataConstants.*;
import static com.x2dev.reports.statereporting.on.register.EnrRegByCourseDataConstants.FIELD_MASTER_SCHEDULE;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * The Class EnrRegByCourseDataReportConEdBase.
 *
 * @author Follett Software Company
 * @copyright 2023
 */
public class EnrRegByCourseDataReportConEdBase extends EnrRegByCourseDataHelper {
    private String m_rptIdPart1;

    private String m_rptIdPart2;

    private ReportDataGrid m_grid;

    private ReportDataGrid gridForMstForPage2;

    private ReportDataGrid gridForMstForPage3;

    private ReportDataGrid gridForMstForPage4;

    private ReportDataGrid gridForMstForPage5;

    private ReportDataGrid gridForMstForPage6;

    private int assignStdTotCnt;

    private int assignCtRegFt;

    private int assignCtRegPt;

    private int assignCtSummer;

    private String regTypeOct;

    private String regTypeMar;

    private static final int REPORT_DATA_GRID_PAGE2 = 2;

    private static final int REPORT_DATA_GRID_PAGE3 = 3;

    private static final int REPORT_DATA_GRID_PAGE4 = 4;

    private static final int REPORT_DATA_GRID_PAGE5 = 5;

    private static final int REPORT_DATA_GRID_PAGE6 = 6;

    /**
     * Gather data.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        String reportName = (String) getParameter(PARAM_REPORT_NAME);

        // get report type, in its own method to allow overrride
        getReportType();

        // get current context start and end dates
        DistrictSchoolYearContext ctx = getCurrentContext();
        String ctxOid = ctx.getOid();
        PlainDate fromDate = ctx.getStartDate();
        PlainDate asOfDate = ctx.getEndDate();

        // load schedule term dates
        loadScheduleTermDatesMap(ctxOid, fromDate, asOfDate);

        // load course delivery type list to onsis code map
        loadCourseDeliveryTypeListToOnsisCodeMap();

        // build course criteria
        loadMstSelection();

        // load student oids to master schedule (using student schedule)
        loadStdOidsToMstOid();

        // load enrolment fields for students in these master schedule courses
        m_enrInfoToStdOidTypeMap = OntarioToolHelper.getEnrInfoBasedOnFteMthlyToStdOidMapRegTypeSplit(m_stdOids, null,
                fromDate, asOfDate, m_dictionary, getBroker(), getLocale());
        m_sskOpIndToSklMthToSklOidToStdOidMap =
                OntarioToolHelper.getSskOpIndToSklMthToSklOidToStdOidMap(m_stdOids, null, getCurrentContext(),
                        m_dictionary, getBroker(), getLocale());

        // load assignment totals, assignment count to student for each master schedule
        if (m_byAssign) {
            loadAssignTotalCtToMstOid();
            loadAssignCtToMstOidStdOid();
        }

        // load class and attendance dates for each master schedule
        if (m_byDate) {
            // get input parameter
            boolean courseIncludeDays = getBooleanParameter(PARAM_COURSE_INCLUDE_DAYS);
            boolean courseIncludeTime = getBooleanParameter(PARAM_COURSE_INCLUDE_TIMES);

            // load bell periods, class dates for selected sections and add parameter
            if (courseIncludeTime) {
                loadBpeToBelOidPeriodNm();
            }
            loadClassDateDayTimeListsToMstOid(ctxOid, courseIncludeDays, courseIncludeTime);
            addParameter(REPORT_CLASS_DATE_LIST_TO_MST_MAP, m_classDtListToMstOidMap);
            addParameter(REPORT_CLASS_DAY_LIST_TO_MST_MAP, m_classDayListToMstOidMap);
            addParameter(REPORT_CLASS_TIME_LIST_TO_MST_MAP, m_classStartEndTimeToMstOidMap);

            // load student class attendance for selected sections and add parameters
            loadClassAttListToStdOidToMstOid();
        }

        // load output grid
        buildOutputGrid(reportName);

        // delete selection objects
        getBroker().deleteBean(m_selectionMst);

        // return null as grid used in publish results
        return null;
    }

    /**
     * Build output grid
     */
    protected void buildOutputGrid(String reportName) {
        // loop through master schedule/student and put output grid records
        Set<String> mstOidsSorted = m_stdOidListToMstOid.keySet();

        // initialize variables
        m_assignCtNonOpRegToMstOidMap = new HashMap<>();
        m_assignCtNonOpSummerToMstOidMap = new HashMap<>();
        m_assignCtNonOpFtToMstOidMap = new HashMap<>();
        m_pupilCtMaleOctToMstOidMap = new HashMap<>();
        m_pupilCtFemaleOctToMstOidMap = new HashMap<>();
        m_pupilCtMaleJunToMstOidMap = new HashMap<>();
        m_pupilCtFemaleJunToMstOidMap = new HashMap<>();
        m_pupilCtMaleAugToMstOidMap = new HashMap<>();
        m_pupilCtFemaleAugToMstOidMap = new HashMap<>();
        m_attFundedCtNonOpToMstOidMap = new HashMap<>();

        // loop through sections
        for (String mstOid : mstOidsSorted) {
            MasterSchedule mst = m_mstToMstOid.get(mstOid);
            PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
            List<String> stdOids = m_stdOidListToMstOid.get(mstOid);
            EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = m_stdStartEndDatesToMstOidMap.get(mstOid);

            int stdCnt = 0;

            int assignCtNonOpRegMst = CONST_ZERO;
            int assignCtNonOpSummerMst = CONST_ZERO;
            int assignCtNonOpFtMst = CONST_ZERO;
            int attFundedCtNonOpMst = CONST_ZERO;

            AtomicInteger pupilCtMaleOctMst = new AtomicInteger(CONST_ZERO);
            AtomicInteger pupilCtFemaleOctMst = new AtomicInteger(CONST_ZERO);
            AtomicInteger pupilCtMaleJunMst = new AtomicInteger(CONST_ZERO);
            AtomicInteger pupilCtFemaleJunMst = new AtomicInteger(CONST_ZERO);
            AtomicInteger pupilCtMaleAugMst = new AtomicInteger(CONST_ZERO);
            AtomicInteger pupilCtFemaleAugMst = new AtomicInteger(CONST_ZERO);

            // write detail records twice if days met go over 45
            gridForMstForPage2 = new ReportDataGrid();
            // write detail records thrice if days met go over 120
            gridForMstForPage3 = new ReportDataGrid();
            // write detail records thrice if days met go over 195
            gridForMstForPage4 = new ReportDataGrid();
            // write detail records thrice if days met go over 270
            gridForMstForPage5 = new ReportDataGrid();
            // write detail records thrice if days met go over 345
            gridForMstForPage6 = new ReportDataGrid();

            // get class dates, number of class sessions, student drop (saved in effective
            // end date) dates if report is by date
            List<PlainDate> classDtListForMstOid = new ArrayList<>();

            int classSessionsCtForMstOid = 0;

            if (m_byDate && m_classDtListToMstOidMap.get(mstOid) != null) {
                classDtListForMstOid = m_classDtListToMstOidMap.get(mstOid);
                classSessionsCtForMstOid = classDtListForMstOid.size();
            }

            // loop through students
            for (String stdOid : stdOids) {
                SisStudent std = getBroker().getBeanByOid(SisStudent.class, stdOid);
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;

                int selectedSchoolYear = mst.getSchoolCourse().getCourse().getDistrictContext().getSchoolYear();
                String schoolName = mst.getSchoolCourse().getSchool().getName();
                Optional<String> arrivalStatus = getArrivalStatus(std, selectedSchoolYear, schoolName);

                if (arrivalStatus.isPresent() &&
                        arrivalStatus.get().equals(OntarioAlias.CONST_ARRIVAL_STATUS_ARRIVED)) {
                    stdCnt++;

                    // get op indicator from enr info, registration type, first enrolment date as string
                    String opInd = CONST_EMPTY;
                    if (m_enrInfoToStdOidTypeMap.containsKey(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX)) {
                        opInd = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX);
                    }

                    SisSchool stdSchool = std.getSchool();

                    boolean isConedSchool = stdSchool != null
                            && stdSchool.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_SPECIAL_CONDITION) != null
                            && OntarioAlias.CONST_SKL_SPECIAL_CONDITION_CON_ED_LIST
                            .contains(stdSchool.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_SPECIAL_CONDITION));

                    processRegistrationTypes(std, mst);

                    // get school month for student (first) start date to see if op
                    PlainDate effStartDateForStd = stdEnrStartEndDatesForMst.getEffStartDateForStdForMst(stdOid);
                    m_calendar.setTime(effStartDateForStd);
                    int classStartDateForStdSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP
                            .get(m_calendar.get(Calendar.MONTH) + 1);

                    // update op indicator based on student start date and school association for student/section school
                    String opIndSsk = null;
                    String mstSklOid = mst.getSchoolCourse().getSchoolOid();

                    if (m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid) != null
                            && m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid).get(mstSklOid) != null) {
                        Map<Integer, String> sskOpIndToSklMthForSklOid =
                                m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid).get(mstSklOid);
                        opIndSsk = sskOpIndToSklMthForSklOid.get(classStartDateForStdSklMonth);
                    }

                    if (opIndSsk != null) {
                        opInd = opIndSsk;
                    }

                    calculateStdAssign(mstOidStdOid, classStartDate, regTypeOct);

                    if (hasAllAssignedLessonsUncompleted(assignStdTotCnt, reportName)) {
                        continue;
                    }

                    // start set grid fields
                    m_grid.append();
                    m_grid.set(FIELD_MASTER_SCHEDULE, mst);
                    m_grid.set(FIELD_STUDENT, std);
                    m_grid.set(FIELD_OP_IND, opInd);
                    m_grid.set(FIELD_REG_TYPE_OCT, isConedSchool ? "CE" : regTypeOct);
                    m_grid.set(FIELD_REG_TYPE_MAR, isConedSchool ? "CE" : regTypeMar);
                    m_grid.set(FIELD_REASON, isNotBlank(m_mstReasons.get(mstOid)) ? m_mstReasons.get(mstOid) : "");

                    // if report is by assignment - process assignments at section/student level
                    if (m_byAssign) {
                        // set assignment counts to op indicator and register type
                        int assignCtNonOpReg = CONST_ZERO;
                        int assignCtNonOpFtReg = CONST_ZERO;
                        int assignCtNonOpSummer = CONST_ZERO;

                        if (StringUtils.isEmpty(opInd) || !opInd.equals(OntarioAlias.CONST_ENR_OP_TXT)) {
                            assignCtNonOpReg = assignCtRegFt + assignCtRegPt;
                            assignCtNonOpFtReg = assignCtRegFt;
                            assignCtNonOpSummer = assignCtSummer;
                        }

                        // accumulate assignment counts for master schedule
                        assignCtNonOpRegMst += assignCtNonOpReg;
                        assignCtNonOpSummerMst += assignCtNonOpSummer;
                        assignCtNonOpFtMst += assignCtNonOpFtReg;

                        // set assignment grid fields
                        m_grid.set(FIELD_ASSIGN_CT, assignStdTotCnt);
                        m_grid.set(FIELD_ASSIGN_CT_NON_OP_REG, assignCtNonOpReg);
                        m_grid.set(FIELD_ASSIGN_CT_NON_OP_SUMMER, assignCtNonOpSummer);
                        m_grid.set(FIELD_ASSIGN_CT_FUNDED_REG, assignCtNonOpReg - assignCtNonOpFtReg);
                        m_grid.set(FIELD_ASSIGN_CT_FUNDED_SUMMER, assignCtNonOpSummer);
                    }

                    // if report is by date - process attendance at section/student level
                    if (m_byDate) {
                        processAttendanceByDate(mst, std, mstOidStdOid, classDtListForMstOid,
                                classSessionsCtForMstOid, attFundedCtNonOpMst, opInd, stdCnt, stdEnrStartEndDatesForMst);
                    }

                    // add student to pupil counts for master schedule
                    String stdGender = std.getPerson().getGenderCode();

                    switch (classStartDateForStdSklMonth) {
                        case OntarioAlias.CONST_SKL_MONTH_JUL:
                        case OntarioAlias.CONST_SKL_MONTH_AUG:
                            processGenderCount(stdGender, pupilCtFemaleAugMst, pupilCtMaleAugMst);
                            break;
                        case OntarioAlias.CONST_SKL_MONTH_SEP:
                        case OntarioAlias.CONST_SKL_MONTH_OCT:
                            processGenderCount(stdGender, pupilCtFemaleJunMst, pupilCtMaleJunMst);
                            processGenderCount(stdGender, pupilCtFemaleOctMst, pupilCtMaleOctMst);
                            break;
                        case OntarioAlias.CONST_SKL_MONTH_NOV:
                        case OntarioAlias.CONST_SKL_MONTH_DEC:
                        case OntarioAlias.CONST_SKL_MONTH_JAN:
                        case OntarioAlias.CONST_SKL_MONTH_FEB:
                        case OntarioAlias.CONST_SKL_MONTH_MAR:
                        case OntarioAlias.CONST_SKL_MONTH_APR:
                        case OntarioAlias.CONST_SKL_MONTH_MAY:
                        case OntarioAlias.CONST_SKL_MONTH_JUN:
                            processGenderCount(stdGender, pupilCtFemaleJunMst, pupilCtMaleJunMst);
                            break;
                    }
                } // end student processing
            }

            // adjust for summer class, to shift enrollment to August instead of June
            boolean courseSummer = false;

            if (m_classSummerCourseIndToMstOid.get(mstOid) != null) {
                courseSummer = m_classSummerCourseIndToMstOid.get(mstOid);
            }

            if (courseSummer) {
                pupilCtMaleOctMst.set(CONST_ZERO);
                pupilCtFemaleOctMst.set(CONST_ZERO);
                pupilCtMaleJunMst.set(CONST_ZERO);
                pupilCtFemaleJunMst.set(CONST_ZERO);
            }

            // if report is by assignment - process assignments at section level
            if (m_byAssign) {
                // save assignment total counts to mst oid
                m_assignCtNonOpRegToMstOidMap.put(mstOid, assignCtNonOpRegMst);
                m_assignCtNonOpSummerToMstOidMap.put(mstOid, assignCtNonOpSummerMst);
                m_assignCtNonOpFtToMstOidMap.put(mstOid, assignCtNonOpFtMst);
            }

            // if report is by date - process attendance at section level
            if (m_byDate) {
                // save attendance total counts to mst oid
                m_attFundedCtNonOpToMstOidMap.put(mstOid, attFundedCtNonOpMst);
                gridForMstForPage2.beforeTop();
                m_grid.append(gridForMstForPage2);
                gridForMstForPage3.beforeTop();
                m_grid.append(gridForMstForPage3);
            }

            // save pupil total counts to mst oid
            m_pupilCtMaleOctToMstOidMap.put(mstOid, pupilCtMaleOctMst.get());
            m_pupilCtFemaleOctToMstOidMap.put(mstOid, pupilCtFemaleOctMst.get());
            m_pupilCtMaleJunToMstOidMap.put(mstOid, pupilCtMaleJunMst.get());
            m_pupilCtFemaleJunToMstOidMap.put(mstOid, pupilCtFemaleJunMst.get());
            m_pupilCtMaleAugToMstOidMap.put(mstOid, pupilCtMaleAugMst.get());
            m_pupilCtFemaleAugToMstOidMap.put(mstOid, pupilCtFemaleAugMst.get());
        } // end mst processing

        // add parameters for assignment/pupil maps
        addParameter(REPORT_ASSIGN_CT_NON_OP_REG_TO_MST_MAP, m_assignCtNonOpRegToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_NON_OP_SUMMER_TO_MST_MAP, m_assignCtNonOpSummerToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_NON_OP_FT_TO_MST_MAP, m_assignCtNonOpFtToMstOidMap);
        addParameter(REPORT_PUPIL_CT_MALE_OCT_TO_MST_MAP, m_pupilCtMaleOctToMstOidMap);
        addParameter(REPORT_PUPIL_CT_FEMALE_OCT_TO_MST_MAP, m_pupilCtFemaleOctToMstOidMap);
        addParameter(REPORT_PUPIL_CT_MALE_JUN_TO_MST_MAP, m_pupilCtMaleJunToMstOidMap);
        addParameter(REPORT_PUPIL_CT_FEMALE_JUN_TO_MST_MAP, m_pupilCtFemaleJunToMstOidMap);
        addParameter(REPORT_PUPIL_CT_MALE_AUG_TO_MST_MAP, m_pupilCtMaleAugToMstOidMap);
        addParameter(REPORT_PUPIL_CT_FEMALE_AUG_TO_MST_MAP, m_pupilCtFemaleAugToMstOidMap);
        addParameter(REPORT_ATT_FUNDED_CT_NON_OP_TO_MST_MAP, m_attFundedCtNonOpToMstOidMap);
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // Initialize variables
        m_grid = new ReportDataGrid();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_calendar = Calendar.getInstance(getLocale());
        m_calHalf1EndDt = PlainDate.fromString(getCurrentContext().getSchoolYear() + CONST_CAL_HALF1_END);
        m_calSummerEnrStartDt = PlainDate.fromString(getCurrentContext().getSchoolYear() + CONST_CAL_SUMMER_ENR_START);
        m_calSummerAssignAfterDt = PlainDate.fromString(getCurrentContext().getSchoolYear() + CONST_CAL_SUMMER_ASSIGN_AFTER);

        // get report ids (for ireport parts)
        m_rptIdPart1 = (String) getParameter(PARAM_RPT_ID_PART1);
        m_rptIdPart2 = (String) getParameter(PARAM_RPT_ID_PART2);

        Map<String, Object> locVarMap = new HashMap<>();
        MessageResources resourcesOut = OntarioToolHelper.initializeLocalized(getOrganization(), null, locVarMap, getBroker());
        m_locForLang = (OrganizationLocale) locVarMap.get(OrganizationLocale.class.getName());

        // get prefixes for parts of both reports and add to separate parameters
        // cannot use the same prefix as they are different reports
        addParameter(REPORT_PREFIX1, getUserMessageKeyPrefix(getReportOidRptId(m_rptIdPart1)));
        addParameter(REPORT_PREFIX2, getUserMessageKeyPrefix(getReportOidRptId(m_rptIdPart2)));

        addParameter(REPORT_SCHOOL_LOCALE, resourcesOut);

        // add extra report parameters
        Map<String, String> ontarioAliasForRptMap = OntarioAlias.buildStaticValuesMap(CONST_ONTARIO_ALIAS_FOR_RPT_LIST);
        addParameter(REPORT_ONTARIO_ALIAS_FOR_RPT_MAP, ontarioAliasForRptMap);
        addParameter(REPORT_LOGO, OntarioToolHelper.getBase64ImageString(OntarioAlias.ON_TRILLIUM_CODE_LOGO, getBroker()));

        // add tool version
        addParameter(REPORT_VERSION, getJob().getTool().getComment());

        DecimalFormat decNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC);
        decNumFormatOut.setGroupingUsed(false);
        DecimalFormat dec5NumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC5);
        dec5NumFormatOut.setGroupingUsed(false);

        if (m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG_MMM_DD,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT_MMM_YY, dateFormatOut);
        } else {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR_DD_MMM, m_locForLang.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT_MMM_YY, dateFormatOut);
            DecimalFormatSymbols decNumFormatOutDecimalFormatSymbols = decNumFormatOut.getDecimalFormatSymbols();
            decNumFormatOutDecimalFormatSymbols.setDecimalSeparator(CONST_NUM_FMT_FR_CHAR_DECIMAL);
            decNumFormatOut.setDecimalFormatSymbols(decNumFormatOutDecimalFormatSymbols);
            DecimalFormatSymbols dec5NumFormatOutDecimalFormatSymbols = dec5NumFormatOut.getDecimalFormatSymbols();
            dec5NumFormatOutDecimalFormatSymbols.setDecimalSeparator(CONST_NUM_FMT_FR_CHAR_DECIMAL);
            dec5NumFormatOut.setDecimalFormatSymbols(dec5NumFormatOutDecimalFormatSymbols);
        }
        addParameter(REPORT_DEC_FMT_OUTPUT, decNumFormatOut);
        addParameter(REPORT_DEC5_FMT_OUTPUT, dec5NumFormatOut);

        // loads reference table for building codes map, add it as parameter
        // should return an empty map if there are no entries with this reference table oid
        Map<String, String> refCodeBuildingCodeMap = loadReferenceCodeDescMap(OntarioAlias.REF_OID_BUILDING_CODES);
        addParameter(REPORT_BUILDING_CODES_RCD_MAP, refCodeBuildingCodeMap);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     *
     *      Override to combine portrait and landscape reports
     */
    @Override
    protected void publishResults() throws Exception {
        // if grid is empty run the super publishResults and return
        if (m_grid.isEmpty()) {
            super.publishResults();
        }

        else {
            // assumes jasper reports 5, if different version then add and call another filler method
            JasperEngine5MultyReportFiller filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(), getBroker());
            boolean publishRpt = false;
            // publishing will be done for combined report using customized publishing
            // manager
            PublishReportsManagerMultyPerBeanJe5 publishManager = null;
            /*
             * If the job has been aborted then don't bother filling the format or exporting
             * the results.
             */
            if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
                if (PublishReportsManager.isPublishing(getJob(), m_grid, this)) {
                    publishRpt = true;

                    // data source, format are null as report filler is not created/populated in
                    // publishing manager
                    publishManager = new PublishReportsManagerMultyPerBeanJe5(getJob(), null, getParameters(), null,
                            this, getSchool(), getOrganization(), getBroker());
                }

                // get report formats for both portrait and landscape reports
                InputStream rptFmtPart1 = null;
                if (!StringUtils.isEmpty(m_rptIdPart1)) {
                    rptFmtPart1 = getFormatRptId(m_rptIdPart1);
                }
                InputStream rptFmtPart2 = null;
                if (!StringUtils.isEmpty(m_rptIdPart2)) {
                    rptFmtPart2 = getFormatRptId(m_rptIdPart2);
                }

                // loop through grid and sends records for each course to report 1 (portrait)
                // and report 2 (landscape)
                MasterSchedule mstPrev = null;
                String mstOidPrev = CONST_EMPTY;
                ReportDataGrid mstGrid = new ReportDataGrid();
                m_grid.beforeTop();

                while (m_grid.next()) {
                    MasterSchedule mst = (MasterSchedule) m_grid.get(FIELD_MASTER_SCHEDULE);
                    String mstOid = mst.getOid();

                    // if new MasterSchedule send grid records for previous master schedule to
                    // reports
                    if ((!StringUtils.isEmpty(mstOidPrev)) && (!mstOidPrev.equals(mstOid))) {
                        mstGrid.beforeTop();
                        filler5.addReport(rptFmtPart1, getParameters(), mstGrid);
                        mstGrid.beforeTop();
                        filler5.addReport(rptFmtPart2, getParameters(), mstGrid);

                        // create publisher and publish mst report (one per master schedule) if publishRpt
                        if (publishRpt) {
                            // publish report for master schedule
                            publishManager.publishReportEachBean(filler5, mstPrev);

                            // reset jasper report filler (as publish is only for one master schedule)
                            filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(), getBroker());
                        }

                        // reset formats
                        rptFmtPart1.reset();
                        rptFmtPart2.reset();

                        // initialize master schedule grid
                        mstGrid = new ReportDataGrid();
                    }
                    mstPrev = mst;
                    mstOidPrev = mstOid;

                    // add current grid record to master schedule grid
                    mstGrid.append(m_grid.getCurrentRow());
                }

                // process last master schedule
                mstGrid.beforeTop();
                filler5.addReport(rptFmtPart1, getParameters(), mstGrid);
                mstGrid.beforeTop();
                filler5.addReport(rptFmtPart2, getParameters(), mstGrid);

                // create publisher and publish mst report (last master schedule) if publishRpt
                if (publishRpt) {
                    // publish report for master scheddule
                    publishManager.publishReportEachBean(filler5, mstPrev);

                    // reset jasper report filler (as publish is only for one master schedule)
                    filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(), getBroker());
                } else {
                    // else write concatenated reports to output stream
                    filler5.fillReport();
                }
            }
        }
    }

    /**
     * Processes attendance for a specific date in the master schedule.
     *
     * @param mst                     The master schedule for the processing.
     * @param std                     The Student for whom the attendance is being processed.
     * @param mstOidStdOid            The Oid for the master schedule and student.
     * @param classDtListForMstOid    List of class dates for the given master schedule.
     * @param classSessionsCtForMstOid Number of class sessions for the given master schedule.
     * @param attFundedCtNonOpMst     Accumulated funded count for the master schedule.
     * @param opInd                    Operational indicator for enrollment.
     * @param stdCnt                   Student count.
     * @param stdEnrStartEndDatesForMst Start and end dates for the student's enrollment in the master schedule.
     */
    private void processAttendanceByDate(MasterSchedule mst, SisStudent std, String mstOidStdOid,
                                         List<PlainDate> classDtListForMstOid, int classSessionsCtForMstOid, int attFundedCtNonOpMst,
                                         String opInd, int stdCnt, EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst) {

        List<String> attListLocalized = initializeAttendanceList(mstOidStdOid, classSessionsCtForMstOid);

        int stdNotEnrCt = countStudentNotEnrolled(mst, std, mstOidStdOid,
                classDtListForMstOid, attListLocalized,stdEnrStartEndDatesForMst);

        int absentUnfundedCt = calculateAbsentUnfundedCount(mst, mstOidStdOid, classSessionsCtForMstOid, stdNotEnrCt);

        int fundedCt = calculateFundedCount(classSessionsCtForMstOid, absentUnfundedCt, stdNotEnrCt);

        int attFundedCtNonOp = calculateFundedCountNonOp(opInd, fundedCt);

        accumulateFundedCountForMasterSchedule(attFundedCtNonOpMst, attFundedCtNonOp);

        setAttendanceGridFields(1, 0, attListLocalized, attFundedCtNonOp, stdCnt);

        createAdditionalPagesIfNecessary(attListLocalized, stdCnt);
    }

    /**
     * Checks if the given master schedule is a dual credit course.
     *
     * @param mst The master schedule to check.
     * @return True if the master schedule is a dual credit course, false otherwise.
     */
    private boolean isCskDualCredit(MasterSchedule mst) {
        boolean cskDualCredit = false;
        String cskCourseDeliveryType = (String) mst.getSchoolCourse()
                .getFieldValueByAlias(OntarioAlias.ALIAS_CSK_COURSE_DELIVERY_TYPE);

        if (!StringUtils.isEmpty(cskCourseDeliveryType)
                && m_courseDeliveryTypeListForDualCredit.contains(cskCourseDeliveryType)) {
            cskDualCredit = true;
        }
        return cskDualCredit;
    }

    /**
     * Initializes the attendance list for a specific master schedule and student.
     *
     * @param mstOidStdOid           The Oid for the master schedule and student.
     * @param classSessionsCtForMstOid Number of class sessions for the given master schedule.
     * @return The initialized attendance list.
     */
    private List<String> initializeAttendanceList(String mstOidStdOid, int classSessionsCtForMstOid) {
        List<String> attListLocalized = new ArrayList<>();
        // get saved list if class att was found
        if (m_attListToMstOidStdOidMap.get(mstOidStdOid) != null) {
            attListLocalized = m_attListToMstOidStdOidMap.get(mstOidStdOid);
        } else {
            // else initialize class att list with spaces
            for (int i = 0; i < classSessionsCtForMstOid; i++) {
                attListLocalized.add(CONST_EMPTY);
            }
        }
        return attListLocalized;
    }

    /**
     * Counts the number of class dates where a student is not enrolled in the master schedule.
     *
     * @param mst                     The master schedule for the processing.
     * @param std                     The student for whom the attendance is being processed.
     * @param mstOidStdOid            The Oid for the master schedule and student.
     * @param classDtListForMstOid    List of class dates for the given master schedule.
     * @param attListLocalized        The localized attendance list.
     * @param stdEnrStartEndDatesForMst Start and end dates for the student's enrollment in the master schedule.
     * @return The count of class dates where the student is not enrolled.
     */
    private int countStudentNotEnrolled(MasterSchedule mst, SisStudent std, String mstOidStdOid,
                                        List<PlainDate> classDtListForMstOid, List<String> attListLocalized,
                                        EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst) {
        int stdNotEnrCt = CONST_ZERO;

        for (PlainDate classDt : classDtListForMstOid) {
            if (!isStudentEnrolledOnClassDate(std, mst, mstOidStdOid, classDt, stdEnrStartEndDatesForMst)) {
                int indexClassDt = classDtListForMstOid.indexOf(classDt);
                if (StringUtils.isEmpty(attListLocalized.get(indexClassDt))) {
                    attListLocalized.set(indexClassDt, CONST_ATT_NOT_ENROLLED);
                    stdNotEnrCt++;
                }
            }
        }

        return stdNotEnrCt;
    }

    /**
     * Checks if a student is enrolled on a specific class date in the master schedule.
     *
     * @param std                     The student for whom the attendance is being processed.
     * @param mst                     The master schedule for the processing.
     * @param mstOidStdOid            The Oid for the master schedule and student.
     * @param classDt                  The class date to check.
     * @param stdEnrStartEndDatesForMst Start and end dates for the student's enrollment in the master schedule.
     * @return True if the student is enrolled, false otherwise.
     */
    private boolean isStudentEnrolledOnClassDate(SisStudent std, MasterSchedule mst,
                                                 String mstOidStdOid, PlainDate classDt,
                                                 EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst) {
        Optional<String> notArrivedAsOfDateValueByAlias = std.getStudentSchools().stream()
                .filter(studentSchool -> studentSchool.getSchoolOid().equals(mst.getSchoolCourse().getSchoolOid()))
                .map(e -> (String) e.getFieldValueByAlias(OntarioAlias.ALIAS_SSK_NOT_ARRIVED_AS_OF_DATE))
                .filter(Objects::nonNull)
                .findFirst();

        PlainDate parseNotArrivedAsOfDate = notArrivedAsOfDateValueByAlias.map(value ->
                new PlainDate(PlainDate.valueOf(value))).orElse(null);

        return stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(mstOidStdOid, classDt, parseNotArrivedAsOfDate);
    }

    /**
     * Calculates the count of absent unfunded sessions for a specific master schedule and student.
     *
     * @param mst                     The master schedule for the processing.
     * @param mstOidStdOid            The Oid for the master schedule and student.
     * @param classSessionsCtForMstOid Number of class sessions for the given master schedule.
     * @param stdNotEnrCt             Count of class dates where the student is not enrolled.
     * @return The count of absent unfunded sessions.
     */
    private int calculateAbsentUnfundedCount(MasterSchedule mst, String mstOidStdOid,
                                             int classSessionsCtForMstOid, int stdNotEnrCt) {
        int absentUnfundedCt = CONST_ZERO;

        if (isCskDualCredit(mst)) {
            // if dual credit, all sessions except when the student is not enrolled
            // (which would not be funded anyway) are unfunded
            absentUnfundedCt = classSessionsCtForMstOid - stdNotEnrCt;
        } else if (m_attAbsentUnfundedCtToMstOidStdOid.get(mstOidStdOid) != null) {
            absentUnfundedCt = m_attAbsentUnfundedCtToMstOidStdOid.get(mstOidStdOid);
        }

        return absentUnfundedCt;
    }

    /**
     * Calculates the count of funded sessions for a specific master schedule and student.
     *
     * @param classSessionsCtForMstOid Number of class sessions for the given master schedule.
     * @param absentUnfundedCt        Count of absent unfunded sessions.
     * @param stdNotEnrCt             Count of class dates where the student is not enrolled.
     * @return The count of funded sessions.
     */
    private int calculateFundedCount(int classSessionsCtForMstOid, int absentUnfundedCt, int stdNotEnrCt) {
        return classSessionsCtForMstOid - absentUnfundedCt - stdNotEnrCt;
    }

    /**
     * Calculates the count of funded sessions for non-operational enrollment.
     *
     * @param opInd     Operational indicator for enrollment.
     * @param fundedCt  Count of funded sessions.
     * @return The count of funded sessions for non-operational enrollment.
     */
    private int calculateFundedCountNonOp(String opInd, int fundedCt) {
        return (StringUtils.isEmpty(opInd) || !opInd.equals(OntarioAlias.CONST_ENR_OP_TXT)) ? fundedCt : CONST_ZERO;
    }

    /**
     * Accumulates the count of funded sessions for the master schedule.
     *
     * @param attFundedCtNonOpMst Accumulated funded count for the master schedule.
     * @param attFundedCtNonOp    Count of funded sessions for the current processing.
     */
    private void accumulateFundedCountForMasterSchedule(int attFundedCtNonOpMst, int attFundedCtNonOp) {
        attFundedCtNonOpMst = attFundedCtNonOpMst + attFundedCtNonOp;
    }

    /**
     * Sets the fields in the attendance grid for a specific page.
     *
     * @param page                 The page number for the attendance grid.
     * @param pageStart            The starting index for the page.
     * @param attListLocalized     The localized attendance list.
     * @param attFundedCtNonOp     Count of funded sessions for non-operational enrollment.
     * @param stdCnt               Student count.
     */
    private void setAttendanceGridFields(int page, int pageStart, List<String> attListLocalized,
                                         int attFundedCtNonOp, int stdCnt) {
        m_grid.set(FIELD_CLASS_DTL_PAGE, page);
        m_grid.set(FIELD_CLASS_DTL_PAGE_START, pageStart);
        m_grid.set(FIELD_CLASS_ATT_LIST, attListLocalized);
        m_grid.set(FIELD_ATT_FUNDED_CT_NON_OP, attFundedCtNonOp);
        m_grid.set(FIELD_STUDENT_NUMBER, stdCnt);
    }

    /**
     * Creates additional pages in the attendance grid if necessary.
     *
     * @param attListLocalized The localized attendance list.
     * @param stdCnt           Student count.
     */
    private void createAdditionalPagesIfNecessary(List<String> attListLocalized, int stdCnt) {
        createAdditionalPage(attListLocalized, 2, CONST_CLASS_DTL_PAGE1_CT, stdCnt);
        createAdditionalPage(attListLocalized, 3, CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT, stdCnt);
        createAdditionalPage(attListLocalized, 4, CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 2, stdCnt);
        createAdditionalPage(attListLocalized, 5, CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 3, stdCnt);
        createAdditionalPage(attListLocalized, 6, CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 4, stdCnt);
    }

    /**
     * Creates an additional page in the attendance grid.
     *
     * @param attListLocalized The localized attendance list.
     * @param page             The page number for the attendance grid.
     * @param pageStart        The starting index for the page.
     * @param stdCnt           Student count.
     */
    private void createAdditionalPage(List<String> attListLocalized, int page, int pageStart, int stdCnt) {
        if (attListLocalized.size() > pageStart) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            ReportDataGrid gridForMstForPage = getGridForPage(page);
            gridForMstForPage.append(gridForMstCurrRowClone);
            gridForMstForPage.set(FIELD_STUDENT_NUMBER, stdCnt);
            gridForMstForPage.set(FIELD_CLASS_DTL_PAGE, page);
            gridForMstForPage.set(FIELD_CLASS_DTL_PAGE_START, pageStart);
        }
    }

    /**
     * Retrieves the ReportDataGrid for a specific page.
     *
     * @param page The page number.
     * @return The ReportDataGrid for the specified page.
     */
    private ReportDataGrid getGridForPage(int page) {
        switch (page) {
            case REPORT_DATA_GRID_PAGE2:
                return gridForMstForPage2;
            case REPORT_DATA_GRID_PAGE3:
                return gridForMstForPage3;
            case REPORT_DATA_GRID_PAGE4:
                return gridForMstForPage4;
            case REPORT_DATA_GRID_PAGE5:
                return gridForMstForPage5;
            case REPORT_DATA_GRID_PAGE6:
                return gridForMstForPage6;
            default:
                return m_grid;
        }
    }

    /**
     * Checks if all assigned lessons are uncompleted for a given report.
     *
     * @param assignTotCnt The total count of assigned lessons.
     * @param reportName   The name of the report.
     * @return true if all assigned lessons are uncompleted and false otherwise.
     */
    private boolean hasAllAssignedLessonsUncompleted(int assignTotCnt, String reportName) {
        boolean isAllLessonsUncomplited = false;
        // currently we don't need to include the student in register 002a only
        if (reportName.equals(EnrRegReport.OBC_ENR_REG002a.getValue())) {
            if (assignTotCnt == 0) {
                isAllLessonsUncomplited = true;
            }
        }

        return isAllLessonsUncomplited;
    }

    /**
     * Calculates the total assignment count for a student.
     *
     * @param mstOidStdOid    The combined identifier for master and student.
     * @param classStartDate  The start date of the class.
     * @param regTypeOct      The reg. type in Oct format.
     */
    private void calculateStdAssign(String mstOidStdOid, PlainDate classStartDate, String regTypeOct) {
        if (m_assignCtRegToRegTypeToMstOidStdOid.get(mstOidStdOid) != null) {
            Map<String, Integer> assignCtRegByRegType =
                    m_assignCtRegToRegTypeToMstOidStdOid.get(mstOidStdOid);
            assignCtRegFt = assignCtRegByRegType.get(OntarioAlias.ENR_REG_TYPE_FT);
            assignCtRegPt = assignCtRegByRegType.get(OntarioAlias.ENR_REG_TYPE_PT);
        }

        if (m_assignCtSummerToMstOidStdOid.get(mstOidStdOid) != null) {
            assignCtSummer = m_assignCtSummerToMstOidStdOid.get(mstOidStdOid);
        }

        // if summer class set all assignments to summer
        if (!classStartDate.before(m_calSummerEnrStartDt)) {
            assignCtSummer += assignCtRegFt;
            assignCtRegFt = CONST_ZERO;
            assignCtSummer += assignCtRegPt;
            assignCtRegPt = CONST_ZERO;
        }

        // if school is cont. ed. set assignment count to PT if FT
        if (regTypeOct.equals(CONST_REG_TYPE_SKL_CON_ED)) {
            assignCtRegPt += assignCtRegFt;
            assignCtRegFt = CONST_ZERO;
        }

        assignStdTotCnt = assignCtRegFt + assignCtRegPt + assignCtSummer;
    }

    /**
     * Process registration types for a given student, considering secondary grades.
     *
     * @param std The student for whom registration types are being processed.
     * @param mst The master schedule to check.
     */
    private void processRegistrationTypes(SisStudent std, MasterSchedule mst) {
        regTypeOct = CONST_EMPTY;
        regTypeMar = CONST_EMPTY;

        String stdOid = std.getOid();
        Optional<String> stdGradeLevelOnSisCode = getStdGradeLevelOnSisCode(std);

        if (stdGradeLevelOnSisCode.isPresent() &&
                OnsisConstants.VALUE_GRADES_SECONDARY_STATE_CODES.contains(stdGradeLevelOnSisCode.get())) {
            if (m_enrInfoToStdOidTypeMap.containsKey(
                    stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_OCT)) {
                regTypeOct = m_enrInfoToStdOidTypeMap.get(
                        stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_OCT);
            }
            if (m_enrInfoToStdOidTypeMap.containsKey(
                    stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_MAR)) {
                regTypeMar = m_enrInfoToStdOidTypeMap.get(
                        stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_MAR);
            }
        } else {
            // as elementary students do not have FTE Records/FTE Monthly data,
            // the information must be collected from the alias 'all-enr-EnrolmentRegister'
            // when student is an elementary student.
            processElementaryRegistrationTypes(std, mst);
        }
    }

    /**
     * Process registration types for an elementary student by iterating through their enrollments.
     *
     * @param std The elementary student for whom registration types are being processed.
     * @param mst The master schedule to check.
     */
    private void processElementaryRegistrationTypes(SisStudent std, MasterSchedule mst) {
        Collection<StudentEnrollment> stdEnrollments = std.getEnrollments();

        for (StudentEnrollment stdEnrollment : stdEnrollments) {
            int schoolYear = mst.getSchoolCourse().getCourse().getDistrictContext().getSchoolYear();
            PlainDate dateOct = ToolBean.getPlainDateValue(schoolYear - 1, Calendar.OCTOBER, 31);
            PlainDate dateMar = ToolBean.getPlainDateValue(schoolYear, Calendar.MARCH, 31);

            if (stdEnrollment.getEnrollmentDate() != null && stdEnrollment.getEnrollmentDate().before(dateOct)) {
                if (OntarioAlias.ENR_TYPE_CODES.contains(stdEnrollment.getEnrollmentType())) {
                    regTypeOct = (String) stdEnrollment.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);
                }
            }
            if (stdEnrollment.getEnrollmentDate() != null && stdEnrollment.getEnrollmentDate().before(dateMar)) {
                if (OntarioAlias.ENR_TYPE_CODES.contains(stdEnrollment.getEnrollmentType())) {
                    regTypeMar = (String) stdEnrollment.getFieldValueByAlias(OntarioAlias.ALIAS_ENR_REGISTER);
                    regTypeMar = isNotEmpty(regTypeMar) ? regTypeMar : regTypeOct;
                }
            }
        }
    }

}
