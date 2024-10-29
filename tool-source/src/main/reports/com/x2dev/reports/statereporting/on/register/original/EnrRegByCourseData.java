/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.on.register.original;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.common.JasperEngine5MultyReportFiller;
import com.x2dev.procedures.statereporting.common.PublishReportsManagerMultyPerBeanJe5;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.procedures.statereporting.on.register.original.OntarioToolHelper;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Enrolment Register By Course for Ontario is used to print by course.
 *
 * Combines a portrait summary page and landscape list of students page per
 * course.
 *
 * @author Follett Software Company
 */
public class EnrRegByCourseData extends ReportJavaSourceNet implements Publishable {
    private static final long serialVersionUID = 1L;

    // Input parameters
    protected static final String PARAM_CSK_CON_ED_PROGRAM_TYPES = "cskConEdProgramTypes";
    protected static final String PARAM_MST_OIDS = "sectionOids";
    protected static final String PARAM_MST_OIDS_STAFF_VIEW = "sectionOidsStaffView";
    protected static final String PARAM_COURSE_SORT = "courseSort";
    protected static final String PARAM_STUDENT_INFO_BY = "studentInfoBy";
    protected static final String PARAM_COURSE_INCLUDE_DAYS = "courseIncludeDays";
    protected static final String PARAM_COURSE_INCLUDE_TIMES = "courseIncludeTimes";
    protected static final String PARAM_RPT_ID_PART1 = "rptIdPart1";
    protected static final String PARAM_RPT_ID_PART2 = "rptIdPart2";

    // Report parameters
    protected static final String REPORT_ONTARIO_ALIAS_FOR_RPT_MAP = "ontarioAliasForRptMap";
    protected static final String REPORT_PREFIX1 = "prefix1";
    protected static final String REPORT_PREFIX2 = "prefix2";
    protected static final String REPORT_SCHOOL_LOCALE = "schoolLocale";
    protected static final String REPORT_LOGO = "logoOntario";
    protected static final String REPORT_DATE_FMT_OUTPUT_MMM_YY = "formatDateMmmYy";
    protected static final String REPORT_DEC_FMT_OUTPUT = "decFmtOutput";
    protected static final String REPORT_DEC5_FMT_OUTPUT = "dec5FmtOutput";
    protected static final String REPORT_VERSION = "version";

    // building codes - reference codes map
    protected static final String REPORT_BUILDING_CODES_RCD_MAP = "buildingCodesRcdMap";
    // course related
    protected static final String REPORT_COURSE_START_DATE_TO_MST_MAP = "classStartDateToMstOid";
    protected static final String REPORT_COURSE_END_DATE_TO_MST_MAP = "classEndDateToMstOid";
    protected static final String REPORT_COURSE_SUMMER_COURSE_iND_TO_MST_MAP = "classSummerCourseIndToMstOid";
    protected static final String REPORT_COURSE_DELIVERY_TYPE_LIST_BY_ONSIS_CODE_MAP =
            "courseDeliveryTypeListByOnsisCodeMap";
    // for assignment counts
    protected static final String REPORT_ASSIGN_TOT_TO_MST_MAP = "assignTotToMstOidMap";
    protected static final String REPORT_ASSIGN_CT_NON_OP_REG_TO_MST_MAP = "assignCtNonOpRegToMstOidMap";
    protected static final String REPORT_ASSIGN_CT_NON_OP_SUMMER_TO_MST_MAP = "assignCtNonOpSummerToMstOidMap";
    protected static final String REPORT_ASSIGN_CT_NON_OP_FT_TO_MST_MAP = "assignCtNonOpFtToMstOidMap";
    // for pupil counts
    protected static final String REPORT_PUPIL_CT_MALE_OCT_TO_MST_MAP = "pupilCtMaleOctToMstOidMap";
    protected static final String REPORT_PUPIL_CT_FEMALE_OCT_TO_MST_MAP = "pupilCtFemaleOctToMstOidMap";
    protected static final String REPORT_PUPIL_CT_MALE_JUN_TO_MST_MAP = "pupilCtMaleJunToMstOidMap";
    protected static final String REPORT_PUPIL_CT_FEMALE_JUN_TO_MST_MAP = "pupilCtFemaleJunToMstOidMap";
    protected static final String REPORT_PUPIL_CT_MALE_AUG_TO_MST_MAP = "pupilCtMaleAugToMstOidMap";
    protected static final String REPORT_PUPIL_CT_FEMALE_AUG_TO_MST_MAP = "pupilCtFemaleAugToMstOidMap";
    // for class attendance dates
    protected static final String REPORT_CLASS_DAY_LIST_TO_MST_MAP = "classDayListToMstOidMap";
    protected static final String REPORT_CLASS_TIME_LIST_TO_MST_MAP = "classStartEndTimeToMstOidMap";
    protected static final String REPORT_CLASS_DATE_LIST_TO_MST_MAP = "classDtListToMstOidMap";
    protected static final String REPORT_ATT_FUNDED_CT_NON_OP_TO_MST_MAP = "attFundedCtNonOpToMstOidMap";

    // Grid fields
    protected static final String FIELD_MASTER_SCHEDULE = "masterSchedule";
    protected static final String FIELD_STUDENT = "student";
    protected static final String FIELD_STUDENT_NUMBER = "studentNumber";
    protected static final String FIELD_OP_IND = "opInd";
    protected static final String FIELD_REG_TYPE_OCT = "regTypOct";
    protected static final String FIELD_REG_TYPE_MAR = "regTypMar";

    // for assignment counts
    protected static final String FIELD_ASSIGN_CT = "assignCt";
    protected static final String FIELD_ASSIGN_CT_NON_OP_REG = "assignCtNonOpReg";
    protected static final String FIELD_ASSIGN_CT_NON_OP_SUMMER = "assignCtNonOpSummer";
    protected static final String FIELD_ASSIGN_CT_FUNDED_REG = "assignCtFundedReg";
    protected static final String FIELD_ASSIGN_CT_FUNDED_SUMMER = "assignCtFundedSummer";

    // for class attendance dates
    protected static final String FIELD_CLASS_DTL_PAGE = "classDtlPage";
    protected static final String FIELD_CLASS_DTL_PAGE_START = "classDtlPageStart";
    protected static final String FIELD_CLASS_ATT_LIST = "classAttList";
    protected static final String FIELD_ATT_FUNDED_CT_NON_OP = "attFundedCtNonOp";

    // Constant values
    protected static final List<String> CONST_ONTARIO_ALIAS_FOR_RPT_LIST = new ArrayList<String>(Arrays.asList(
            "IMAGE_FILE_NAME_ONTARIO", "ALIAS_PSN_LEGAL_LAST_NAME", "ALIAS_PSN_LEGAL_FIRST_NAME",
            "ALIAS_CSK_COURSE_DELIVERY_TYPE", "CONST_CRS_DELIVERY_TYPE_CORR", "CONST_CRS_DELIVERY_TYPE_ELEARN_NOTLMS",
            "CONST_CRS_DELIVERY_TYPE_ELEARN_LMS", "CONST_CRS_DELIVERY_TYPE_IND_STUDY", "ALIAS_CSK_INTL_LANGUAGE",
            "ALIAS_MST_CON_ED_OFFERING_TYPE", "CONST_MST_CON_ED_OFFERING_TYPE_01_NIGHT_WEEKEND",
            "CONST_MST_CON_ED_OFFERING_TYPE_02_DAY", "CONST_MST_CON_ED_OFFERING_TYPE_03_AFTER_SKL",
            "CONST_MST_CON_ED_OFFERING_TYPE_04_CREDIT_ON_MATH", "CONST_MST_CON_ED_OFFERING_TYPE_05_ADDITIONAL_PREP",
            "CONST_MST_CON_ED_OFFERING_TYPE_13_7_8_REMEDIAL_LIT_NUM",
            "CONST_MST_CON_ED_OFFERING_TYPE_14_NON_CREDIT_9_10_REMEDIAL_LIT_NUM",
            "CONST_MST_CON_ED_OFFERING_TYPE_15_LIT_NUM_PARENTS_GUARDIANS", "ALIAS_MST_TIME_OF_DAY_CODE",
            "CONST_MST_TIME_OF_DAY_CODE_1_REG_DAY", "CONST_MST_TIME_OF_DAY_CODE_2_SUMMER",
            "CONST_MST_TIME_OF_DAY_CODE_3_WEEKENDS", "CONST_MST_TIME_OF_DAY_CODE_4_AFTER_SKL",
            "CONST_MST_TIME_OF_DAY_CODE_5_EXTENDED_SKL_DAY", "CONST_MST_TIME_OF_DAY_CODE_6_BEF_SKL_OR_LUNCH",
            "CONST_MST_TIME_OF_DAY_CODE_7_LATE_AFT_OR_EVE"));
    protected static final String CONST_EMPTY = "";
    protected static final String CONST_SPACE = " ";
    protected static final String CONST_SPLIT_STR = "---";
    protected static final String CONST_HYPHEN = "-";
    protected static final String CONST_PERIOD = ".";
    protected static final String CONST_COLON = ":";
    protected static final String CONST_COMMA = ",";
    protected static final String CONST_TRUE = "1";
    protected static final Integer CONST_ZERO = Integer.valueOf(0);
    protected static final Double CONST_ZERO_DBL = Double.valueOf(0.0);
    protected static final String CONST_AM = "AM";
    protected static final String CONST_PM = "PM";
    protected static final int CONST_PM_HOUR = 12;
    protected static final double CONST_HOUR_TO_MINS_DBL = 60.00;
    protected static final int CONST_LIST_NOT_FOUND = -2;
    protected static final DecimalFormat CONST_FORMAT_INT_2 = new DecimalFormat("00");
    protected static final DecimalFormat CONST_FORMAT_DBL_2_CALC_ONLY = new DecimalFormat("#0.00");
    protected static final DateFormat CONST_FORMAT_DATE_MST_IN = new SimpleDateFormat("yyyy-MM-dd");

    // Constant values - Localization
    protected static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    protected static final String CONST_DATE_FMT_STR_ENG_MMM_DD = "MMM d";
    protected static final String CONST_DATE_FMT_STR_FR_DD_MMM = "d MMM";
    protected static final char CONST_NUM_FMT_FR_CHAR_DECIMAL = ',';
    protected static final String CONST_NUM_FMT_STR_DEC = "#,##0.00";
    protected static final String CONST_NUM_FMT_STR_DEC5 = "#,##0.00000";

    // Constant values - Courses
    protected static final Collection<String> CONST_CRS_DELIVERY_TYPE_DUAL_CRED_ONSIS_CODE_LIST = new ArrayList<String>(
            Arrays.asList("12", "13", "21"));

    // Constant values - Reg type
    protected static final String CONST_REG_TYPE_SKL_CON_ED = "CE";

    // Constant values - Attendance codes/Absent counts
    protected static final int CONST_CLASS_DTL_PAGE1_CT = 45;
    protected static final int CONST_CLASS_DTL_PAGE2_CT = 100;
    protected static final String CONST_ATT_NOT_ENROLLED = "-";
    protected static final int CONST_ABS_CONSEC_CT_MIN = 3;

    // Constant values - Input parameters
    protected static final String CONST_SORT_CRS_NUM_SECTION = "courseNumber, section";
    protected static final String CONST_SORT_CRS_DESC_SECTION = "courseDesc, section";
    protected static final String CONST_STD_INFO_BY_ASSIGN = "byAssign";
    protected static final String CONST_STD_INFO_BY_DATE = "byDate";

    // Constant values - Cal 1st and 2nd half years
    protected static final String CONST_CAL_HALF1_END = "-01-31";

    // Constant values - Cal Summer Course Start Date
    protected static final String CONST_CAL_SUMMER_ENR_START = "-06-25";

    // Constant values - Cal Summer Assignment Start Date
    protected static final String CONST_CAL_SUMMER_ASSIGN_AFTER = "-06-30";

    // Constant values - Days
    protected static final int CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP = 10;

    // Variables - general
    protected String m_rptIdPart1;
    protected String m_rptIdPart2;
    protected ReportDataGrid m_grid;
    protected DataDictionary m_dictionary;
    protected Calendar m_calendar = null;
    protected PlainDate m_calHalf1EndDt = null;
    protected PlainDate m_calSummerEnrStartDt = null;
    protected PlainDate m_calSummerAssignAfterDt = null;
    protected OrganizationLocale m_locForLang = null;

    // Variables - input
    protected boolean m_byAssign = false;
    protected boolean m_byDate = false;

    // Variables - selection, mst, std
    protected Staff m_staffCurrent;
    protected Selection m_selectionMst;
    protected Map<String, MasterSchedule> m_mstToMstOid;
    protected Map<String, PlainDate> m_classStartDateToMstOid;
    protected Map<String, PlainDate> m_classEndDateToMstOid;
    protected Map<String, Boolean> m_classSummerCourseIndToMstOid;
    protected Map<String, List<String>> m_stdOidListToMstOid;
    protected List<String> m_stdOids;
    protected Map<String, String> m_stdCalIdToMstOid;// assumes same for all students

    // Variables - maps
    protected Map<String, String> m_enrInfoToStdOidTypeMap;
    protected Map<String, Map<String, Map<Integer, String>>> m_sskOpIndToSklMthToSklOidToStdOidMap;
    // for course info
    protected Map<String, ScheduleTermDate> m_courseTrmDtToTrmOidMap;
    protected Map<String, List<String>> m_courseDeliveryTypeListByOnsisCodeMap;
    protected List<String> m_courseDeliveryTypeListForDualCredit;
    // for assignment counts - for grid
    protected Map<String, Map<String, Integer>> m_assignCtRegToRegTypeToMstOidStdOid;
    protected Map<String, Integer> m_assignCtSummerToMstOidStdOid;
    // for assignment counts - parameter
    protected Map<String, Integer> m_assignTotToMstOidMap;
    protected Map<String, Integer> m_assignCtNonOpRegToMstOidMap;
    protected Map<String, Integer> m_assignCtNonOpSummerToMstOidMap;
    protected Map<String, Integer> m_assignCtNonOpFtToMstOidMap;
    // for pupil counts - parameter
    protected Map<String, Integer> m_pupilCtMaleOctToMstOidMap;
    protected Map<String, Integer> m_pupilCtFemaleOctToMstOidMap;
    protected Map<String, Integer> m_pupilCtMaleJunToMstOidMap;
    protected Map<String, Integer> m_pupilCtFemaleJunToMstOidMap;
    protected Map<String, Integer> m_pupilCtMaleAugToMstOidMap;
    protected Map<String, Integer> m_pupilCtFemaleAugToMstOidMap;
    // for class attendance dates - parameter or grid
    protected Map<String, List<PlainDate>> m_classDtListToMstOidMap;
    protected Map<String, List<String>> m_classDayListToMstOidMap;
    protected Map<String, String> m_classStartEndTimeToMstOidMap;
    protected Map<String, EnrRegStudentStartEndDatesForSection> m_stdStartEndDatesToMstOidMap;
    protected Map<String, Integer> m_attFundedCtNonOpToMstOidMap;
    // for class attendance dates - for grid
    protected Map<String, List<String>> m_attListToMstOidStdOidMap;
    protected Map<String, Integer> m_attAbsentUnfundedCtToMstOidStdOid;
    // for class attendance dates - other
    protected Map<String, ScheduleBellPeriod> m_bpeTimeToBelOidPeriodNmMap;

    // Variables - to prevent java error from inner classes
    protected static final String NO_PERFMON4J_INSTRUMENTATION = "";

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
        buildOutputGrid();

        // delete selection objects
        getBroker().deleteBean(m_selectionMst);

        // return null as grid used in publish results
        return null;
    }

    /**
     * Gets report type based on input parameters
     */
    protected void getReportType() {
        String studentInfoBy = (String) getParameter(PARAM_STUDENT_INFO_BY);
        m_byAssign = (studentInfoBy.equals(CONST_STD_INFO_BY_ASSIGN)) ? true : false;
        m_byDate = (studentInfoBy.equals(CONST_STD_INFO_BY_DATE)) ? true : false;
    }

    /**
     * Loads the schedule term dates to schedule term oid map for selected year
     *
     * @param ctxOid
     * @param asOfDate
     * @param fromDate
     */
    protected void loadScheduleTermDatesMap(String ctxOid, PlainDate fromDate, PlainDate asOfDate) {
        m_courseTrmDtToTrmOidMap = new HashMap<String, ScheduleTermDate>();

        Criteria tmdCriteria = new Criteria();
        tmdCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID, ctxOid);
        tmdCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_START_DATE, fromDate);
        tmdCriteria.addLessOrEqualThan(ScheduleTermDate.COL_END_DATE, asOfDate);

        QueryByCriteria tmdQuery = new QueryByCriteria(ScheduleTermDate.class, tmdCriteria);

        // load map of schedule term date to schedule term oid
        try (QueryIterator tmdIterator = getBroker().getIteratorByQuery(tmdQuery)) {
            // iterates through and saves into map
            while (tmdIterator.hasNext()) {
                ScheduleTermDate tmd = (ScheduleTermDate) tmdIterator.next();
                String trmOid = tmd.getScheduleTermOid();

                m_courseTrmDtToTrmOidMap.put(trmOid, tmd);
            }
        }
    }

    /**
     * Loads the course delivery type reference table codes to Onsis code maps
     */
    protected void loadCourseDeliveryTypeListToOnsisCodeMap() {
        m_courseDeliveryTypeListByOnsisCodeMap = new HashMap<String, List<String>>();
        m_courseDeliveryTypeListForDualCredit = new ArrayList<String>();

        String[] requestColumns = {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + X2BaseBean.COL_OID,
                OntarioAlias.REF_OID_ON_SIS_COURSE_DELIVERY_TYPE);

        ColumnQuery query = new ColumnQuery(ReferenceCode.class, requestColumns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String code = (String) row[0];
                String onsisCode = (String) row[1];

                List<String> courseDeliveryTypeListForOnsisCode = m_courseDeliveryTypeListByOnsisCodeMap.get(onsisCode);
                if (courseDeliveryTypeListForOnsisCode == null) {
                    courseDeliveryTypeListForOnsisCode = new ArrayList<String>();
                }

                if (!courseDeliveryTypeListForOnsisCode.contains(code)) {
                    courseDeliveryTypeListForOnsisCode.add(code);
                }

                m_courseDeliveryTypeListByOnsisCodeMap.put(onsisCode, courseDeliveryTypeListForOnsisCode);

                if (CONST_CRS_DELIVERY_TYPE_DUAL_CRED_ONSIS_CODE_LIST.contains(onsisCode)) {
                    m_courseDeliveryTypeListForDualCredit.add(code);
                }
            }
        }

        addParameter(REPORT_COURSE_DELIVERY_TYPE_LIST_BY_ONSIS_CODE_MAP, m_courseDeliveryTypeListByOnsisCodeMap);
    }

    /**
     * Loads the master schedule selection objects based on input selection
     */
    protected void loadMstSelection() {
        /*
         * General set up of input parameters, master schedule selection objects,
         * initialization
         */
        // Get master schedule oids based on input selection
        Collection<String> mstOidsInput = new ArrayList<String>();
        if (getParameter(PARAM_MST_OIDS) != null) {
            mstOidsInput.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS), ","));
        }
        if (getParameter(PARAM_MST_OIDS_STAFF_VIEW) != null) {
            mstOidsInput.addAll(
                    StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS_STAFF_VIEW), ","));
        }

        // initialize selection
        m_selectionMst = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
        m_selectionMst.setTimestamp(System.currentTimeMillis());

        // initialize other variables
        m_mstToMstOid = new HashMap<String, MasterSchedule>();
        m_classStartDateToMstOid = new HashMap<String, PlainDate>();
        m_classEndDateToMstOid = new HashMap<String, PlainDate>();
        m_classSummerCourseIndToMstOid = new HashMap<String, Boolean>();

        /*
         * Add to selection if sections have been selected
         */
        // loop through, save selection objects if sections have been selected
        try {
            for (String mstOid : mstOidsInput) {
                // create SelectionObject for each mst oid
                SelectionObject selectionObj = X2BaseBean.newInstance(SelectionObject.class,
                        getBroker().getPersistenceKey());
                selectionObj.setObjectOid(mstOid);
                m_selectionMst.addToSelectionObjects(selectionObj);

                // get master schedule for mstOid
                MasterSchedule mst = getBroker().getBeanByOid(MasterSchedule.class, mstOid);
                m_mstToMstOid.put(mstOid, mst);

                // save master schedule start and end dates
                saveMstStartEndDates(mst, mstOid);

                // save master schedule summer class indicator
                saveMstSummerClassInd(mst, mstOid);
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        /*
         * Add to selection if all sections that meet other criteria
         */
        // select all sections based on school/staff if particular sections not selected
        if (mstOidsInput.isEmpty()) {
            Collection<String> cskConEdProgramTypesInput = new ArrayList<String>();
            if (getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES) != null) {
                cskConEdProgramTypesInput.addAll(StringUtils
                        .convertDelimitedStringToList((String) getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES), ";"));
            }

            // create master schedule criteria and add sections to selection object
            X2Criteria mstCriteria = new X2Criteria();

            // add school condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());

            // add school year condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            // add staff condition (would be from staff view)
            if (m_staffCurrent != null) {
                mstCriteria.addEqualTo(
                        MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER + TeacherSection.COL_STAFF_OID,
                        m_staffCurrent.getOid());
            }

            // add course con ed program condition
            String fieldCourseConEdProgramType = getBeanPathFromAlias(OntarioAlias.ALIAS_CSK_CON_ED_PROGRAM_TYPE,
                    false);
            mstCriteria.addIn(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + fieldCourseConEdProgramType,
                    cskConEdProgramTypesInput);

            // create query for master schedule
            QueryByCriteria mstQuery = new QueryByCriteria(MasterSchedule.class, mstCriteria);

            // load master schedules
            try (QueryIterator mstIterator = getBroker().getIteratorByQuery(mstQuery)) {
                // iterates through and saves
                while (mstIterator.hasNext()) {
                    MasterSchedule mst = (MasterSchedule) mstIterator.next();
                    String mstOid = mst.getOid();

                    // create SelectionObject for each mst oid
                    SelectionObject selectionObj = X2BaseBean.newInstance(SelectionObject.class,
                            getBroker().getPersistenceKey());
                    selectionObj.setObjectOid(mstOid);
                    m_selectionMst.addToSelectionObjects(selectionObj);

                    // save master schedule for mstOid
                    m_mstToMstOid.put(mstOid, mst);

                    // save master schedule start and end dates
                    saveMstStartEndDates(mst, mstOid);

                    // save master schedule summer class indicator
                    saveMstSummerClassInd(mst, mstOid);
                }
            }
        }

        // Save the selection as criteria for use on other queries or show errors
        Collection<ValidationError> errors = getBroker().saveBean(m_selectionMst);
        if (errors.size() > 0) {
            StringBuilder errorCause = new StringBuilder();
            for (ValidationError err : errors) {
                errorCause.append(WebUtils.getMessage(err, getBroker().getPersistenceKey()));
            }
            throw new RuntimeException(errorCause.toString());
        }

        // Save course start/end dates
        addParameter(REPORT_COURSE_START_DATE_TO_MST_MAP, m_classStartDateToMstOid);
        addParameter(REPORT_COURSE_END_DATE_TO_MST_MAP, m_classEndDateToMstOid);
        addParameter(REPORT_COURSE_SUMMER_COURSE_iND_TO_MST_MAP, m_classSummerCourseIndToMstOid);
    }

    /**
     * Save the master schedule start end dates in global maps
     *
     * @param mst - MasterSchedule
     * @param mstOid
     */
    protected void saveMstStartEndDates(MasterSchedule mst, String mstOid) {
        // get school year start/end date in case not found (data issue)
        PlainDate classStartDate = getCurrentContext().getStartDate();
        PlainDate classEndDate = getCurrentContext().getEndDate();

        // get master schedule start/end dates based on section (if overriding values
        // saved)
        String classStartDateMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_START_DATE);
        String classEndDateMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_END_DATE);

        // get master schedule start and end dates from schedule term if not on section
        String trmOid = mst.getScheduleTermOid();
        ScheduleTermDate tmd = m_courseTrmDtToTrmOidMap.get(trmOid);
        if (!StringUtils.isEmpty(classStartDateMstStr)) {
            classStartDate = getDate(mst, OntarioAlias.ALIAS_MST_CON_ED_START_DATE);
        } else if (tmd != null) {
            classStartDate = tmd.getStartDate();
        }
        if (!StringUtils.isEmpty(classEndDateMstStr)) {
            classEndDate = getDate(mst, OntarioAlias.ALIAS_MST_CON_ED_END_DATE);
        } else if (tmd != null) {
            classEndDate = tmd.getEndDate();
        }

        m_classStartDateToMstOid.put(mstOid, classStartDate);
        m_classEndDateToMstOid.put(mstOid, classEndDate);
    }

    /**
     * Save the master schedule summer class indicator in global map
     *
     * @param mst - MasterSchedule
     * @param mstOid
     */
    protected void saveMstSummerClassInd(MasterSchedule mst, String mstOid) {
        // initialize summer indicator to false
        boolean courseSummer = false;

        // get master schedule cont ed offering type
        String mstConEdOfferingType = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_OFFERING_TYPE);
        String mstTimeOfDayCode = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_TIME_OF_DAY_CODE);

        // if either value above is for summer set indicator to true
        if (((!StringUtils.isEmpty(mstConEdOfferingType))
                && (OntarioAlias.CONST_MST_CON_ED_OFFERING_SUMMER.contains(mstConEdOfferingType)))
                || ((!StringUtils.isEmpty(mstTimeOfDayCode))
                        && (OntarioAlias.CONST_MST_TIME_OF_DAY_SUMMER.contains(mstTimeOfDayCode)))) {
            courseSummer = true;
        }

        m_classSummerCourseIndToMstOid.put(mstOid, Boolean.valueOf(courseSummer));
    }

    /**
     * Loads the student oids who are taking the master schedule selection courses
     */
    protected void loadStdOidsToMstOid() {
        // create student schedule criteria
        X2Criteria sccCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(StudentScheduleChange.COL_MASTER_SCHEDULE_OID);
        sccCriteria.addExists(subQuery);

        // get sort parameter
        String courseSort = (String) getParameter(PARAM_COURSE_SORT);

        // create query for load students to master schedule
        // not doing column query in case date is returned as time stamp
        QueryByCriteria sccQuery = new QueryByCriteria(StudentScheduleChange.class, sccCriteria);
        if (courseSort.equals(CONST_SORT_CRS_NUM_SECTION)) {
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        } else if (courseSort.equals(CONST_SORT_CRS_DESC_SECTION)) {
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_DESCRIPTION);
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_SECTION_NUMBER);
        }
        sccQuery.addOrderByAscending(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        sccQuery.addOrderByAscending(StudentScheduleChange.COL_TIMESTAMP);

        // load map of student oid to mst oid, map should be ordered by entry
        // (LinkedHashMap) and
        // list of student oids
        m_stdOidListToMstOid = new LinkedHashMap<String, List<String>>();
        m_stdCalIdToMstOid = new HashMap<String, String>();
        m_stdOids = new ArrayList<String>();
        m_stdStartEndDatesToMstOidMap = new HashMap<String, EnrRegStudentStartEndDatesForSection>();
        String mstOidPrev = CONST_EMPTY;
        String stdOidPrev = CONST_EMPTY;
        List<String> stdOidListForMstOid = new ArrayList<String>();
        EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = null;
        try (QueryIterator sccIterator = getBroker().getIteratorByQuery(sccQuery)) {
            while (sccIterator.hasNext()) {
                // get new values
                StudentScheduleChange scc = (StudentScheduleChange) sccIterator.next();
                String mstOid = scc.getMasterScheduleOid();
                String stdOid = scc.getStudentOid();
                PlainDate sccEffDate = scc.getEffectiveDate();
                String sccChgType = scc.getChangeTypeCode();
                String stdCalId = scc.getStudent().getCalendarCode();

                // if new mstOid, write stdOids for previous mstOid including last stdOid
                if ((!StringUtils.isEmpty(mstOidPrev) && (!mstOidPrev.equals(mstOid)))) {
                    // add previous stdOid to list for previous mstOid
                    if (!StringUtils.isEmpty(stdOidPrev)) {
                        // if student was enrolled after class started add to lists for mst
                        if ((stdEnrStartEndDatesForMst != null)
                                && (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev))) {
                            stdOidListForMstOid.add(stdOidPrev);

                            // add previous stdOid to overall list
                            if (!m_stdOids.contains(stdOidPrev)) {
                                m_stdOids.add(stdOidPrev);
                            }
                        }
                    }

                    // save student enrolment dates for previous mstOid
                    m_stdStartEndDatesToMstOidMap.put(mstOidPrev, stdEnrStartEndDatesForMst);

                    // add list of stdOids for previous mstOid
                    m_stdOidListToMstOid.put(mstOidPrev, stdOidListForMstOid);

                    // reinitialize variables (mst and std)
                    stdOidListForMstOid = new ArrayList<String>();
                }
                // if new stdOid (but not new mstOid), add previous stdOid to list
                else if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    // if student was enrolled after class started add to lists for mst
                    if ((stdEnrStartEndDatesForMst != null)
                            && (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev))) {
                        stdOidListForMstOid.add(stdOidPrev);
                    }

                    // add previous stdOid to overall list
                    if (!m_stdOids.contains(stdOidPrev)) {
                        m_stdOids.add(stdOidPrev);
                    }
                }

                // if new mst oid fetch variables saved for new mst oid
                if (!mstOidPrev.equals(mstOid)) {
                    PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
                    PlainDate classEndDate = m_classEndDateToMstOid.get(mstOid);
                    stdEnrStartEndDatesForMst =
                            new EnrRegStudentStartEndDatesForSection(mstOid, classStartDate, classEndDate);
                }

                // set mst/std previous to current values
                mstOidPrev = mstOid;
                stdOidPrev = stdOid;

                // check if student was enrolled when class started, changes come in order so
                // check date/type
                if (!sccChgType.equals(StudentScheduleChange.CODE_DROP)) {
                    // add student start date
                    stdEnrStartEndDatesForMst.addStdMstStartDate(stdOid, sccEffDate);

                    // save calendar id if not saved for section
                    if (m_stdCalIdToMstOid.get(mstOid) == null) {
                        m_stdCalIdToMstOid.put(mstOid, stdCalId);
                    }
                }

                else {
                    // add student drop date
                    m_calendar.setTime(sccEffDate);
                    PlainDate sccEffDateLastClassDate = addDays(m_calendar, -1);
                    stdEnrStartEndDatesForMst.addStdMstEndDate(stdOid, sccEffDateLastClassDate);
                }
            }
        } catch (

        Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write stdOids/drop dates for last section including last std oid
        if (!StringUtils.isEmpty(mstOidPrev)) {
            // add last student
            if (!StringUtils.isEmpty(stdOidPrev)) {
                // if student was enrolled after class started add to lists for mst
                if (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev)) {
                    stdOidListForMstOid.add(stdOidPrev);

                    // add previous stdOid to overall list
                    if (!m_stdOids.contains(stdOidPrev)) {
                        m_stdOids.add(stdOidPrev);
                    }
                }

                // save student enrolment dates for previous mstOid
                m_stdStartEndDatesToMstOidMap.put(mstOidPrev, stdEnrStartEndDatesForMst);

                // add list of stdOids for previous mstOid
                m_stdOidListToMstOid.put(mstOidPrev, stdOidListForMstOid);
            }
        }
    }

    /**
     * Loads assignment total count to master schedule oid
     */
    protected void loadAssignTotalCtToMstOid() {
        // create student schedule criteria
        X2Criteria gbcCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);
        gbcCriteria.addExists(subQuery);

        // add condition based on system indicator
        gbcCriteria.addEqualTo(GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        // add condition based on indicator to include in these registers
        String fieldAssignConEdIncl = getBeanPathFromAlias(OntarioAlias.ALIAS_GCD_CON_ED_REGISTER_INCL_ON, false);
        if (fieldAssignConEdIncl != null) {
            gbcCriteria.addEqualTo(fieldAssignConEdIncl, CONST_TRUE);
        }

        // load assignments to master schedule/student
        String[] columns = new String[] {GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, X2BaseBean.COL_OID};
        ColumnQuery gbcQuery = new ColumnQuery(GradebookColumnDefinition.class, columns, gbcCriteria);
        gbcQuery.addOrderByAscending(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);

        // load map of assignment counts to master schedule oid/student oid
        m_assignTotToMstOidMap = new HashMap<String, Integer>();
        String mstOidPrev = CONST_EMPTY;
        int assignTotCt = CONST_ZERO.intValue();
        // loop through assignments
        try (ReportQueryIterator gbcIterator = getBroker().getReportQueryIteratorByQuery(gbcQuery)) {
            while (gbcIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) gbcIterator.next();
                String mstOid = data[0].toString();
                String gbcOid = data[1].toString();

                // if new section, write assignment total count for prev
                if ((!StringUtils.isEmpty(mstOidPrev) && (!mstOidPrev.equals(mstOid)))) {
                    m_assignTotToMstOidMap.put(mstOidPrev, Integer.valueOf(assignTotCt));
                    assignTotCt = CONST_ZERO.intValue();
                }
                mstOidPrev = mstOid;

                // accumulate assignments
                assignTotCt++;
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write assignment total count for last section
        if (!StringUtils.isEmpty(mstOidPrev)) {
            m_assignTotToMstOidMap.put(mstOidPrev, Integer.valueOf(assignTotCt));
        }

        addParameter(REPORT_ASSIGN_TOT_TO_MST_MAP, m_assignTotToMstOidMap);
    }

    /**
     * Loads assignment count to master schedule oid/student oid
     */
    protected void loadAssignCtToMstOidStdOid() {
        // create student schedule criteria
        X2Criteria gbsCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER
                + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);
        gbsCriteria.addExists(subQuery);

        // add condition based on system indicator
        gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER
                + GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        // add condition to only include scores with a completion date (to bypass excluded
        // assignments)
        gbsCriteria.addNotNull(GradebookScore.COL_COMPLETED_DATE);

        // add condition based on indicator to include in these registers
        String fieldAssignConEdIncl = getBeanPathFromAlias(OntarioAlias.ALIAS_GCD_CON_ED_REGISTER_INCL_ON, false);
        if (fieldAssignConEdIncl != null) {
            gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER + fieldAssignConEdIncl,
                    CONST_TRUE);
        }

        // load assignments to master schedule/student
        String[] columns = new String[] {
                GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER
                        + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID,
                GradebookScore.COL_STUDENT_OID, GradebookScore.COL_SCORE, GradebookScore.COL_COMPLETED_DATE};
        ColumnQuery gbsQuery = new ColumnQuery(GradebookScore.class, columns, gbsCriteria);
        gbsQuery.addOrderByAscending(GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER
                + GradebookColumnDefinition.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        gbsQuery.addOrderByAscending(GradebookScore.REL_COLUMN_DEFINITION + PATH_DELIMITER
                + GradebookColumnDefinition.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.COL_DESCRIPTION);
        gbsQuery.addOrderByAscending(GradebookScore.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

        // load map of assignment counts to master schedule oid/student oid
        m_assignCtRegToRegTypeToMstOidStdOid = new HashMap<String, Map<String, Integer>>();
        m_assignCtSummerToMstOidStdOid = new HashMap<String, Integer>();
        String mstOidStdOidPrev = CONST_EMPTY;
        int assignCtRegFt = CONST_ZERO.intValue();
        int assignCtRegPt = CONST_ZERO.intValue();
        int assignCtSummer = CONST_ZERO.intValue();
        String regTypeOct = CONST_EMPTY;
        String regTypeMar = CONST_EMPTY;
        // loop through scores
        try (ReportQueryIterator gbsIterator = getBroker().getReportQueryIteratorByQuery(gbsQuery)) {
            while (gbsIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) gbsIterator.next();
                String mstOid = data[0].toString();
                String stdOid = data[1].toString();
                String score = data[2].toString();
                Timestamp scoreDateTs = (Timestamp) data[3];
                PlainDate scoreDate = null;
                if (scoreDateTs != null) {
                    scoreDate = PlainDate.fromString(scoreDateTs.toString().substring(0, 10));
                }
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;

                // process only if student is in list of std oids with enrolment info saved
                // one student had assignments but was not on roster so is necessary
                if (!m_stdOids.contains(stdOid)) {
                    continue;
                }

                // if new section/student, write assignment count for prev
                if ((!StringUtils.isEmpty(mstOidStdOidPrev) && (!mstOidStdOidPrev.equals(mstOidStdOid)))) {
                    Map<String, Integer> assignCtRegByRegType = new HashMap<String, Integer>();
                    assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_FT, Integer.valueOf(assignCtRegFt));
                    assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_PT, Integer.valueOf(assignCtRegPt));
                    m_assignCtRegToRegTypeToMstOidStdOid.put(mstOidStdOidPrev, assignCtRegByRegType);
                    m_assignCtSummerToMstOidStdOid.put(mstOidStdOidPrev, assignCtSummer);
                    assignCtRegFt = CONST_ZERO.intValue();
                    assignCtRegPt = CONST_ZERO.intValue();
                    assignCtSummer = CONST_ZERO.intValue();
                    regTypeOct = CONST_EMPTY;
                    regTypeMar = CONST_EMPTY;
                }
                mstOidStdOidPrev = mstOidStdOid;

                // accumulate assignment if score is populated
                if (!StringUtils.isEmpty(score)) {
                    if (StringUtils.isEmpty(regTypeOct)) {
                        regTypeOct = m_enrInfoToStdOidTypeMap.get(
                                stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_OCT);
                        regTypeMar = m_enrInfoToStdOidTypeMap.get(
                                stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_MAR);
                    }

                    // if score date != null then check if summer otherwise check if PT
                    // assignments are FT if not summer and not PT
                    if (scoreDate != null) {
                        // check if summer assignment
                        if (scoreDate.after(m_calSummerAssignAfterDt)) {
                            assignCtSummer++;
                        }
                        // assignment counts if PT
                        // first half yr assignment and PT in Oct
                        else if (((regTypeOct != null) && (regTypeOct.equals(OntarioAlias.ENR_REG_TYPE_PT))
                                && (!scoreDate.after(m_calHalf1EndDt)))
                                // second half yr assignment and PT in Mar
                                || ((regTypeMar != null) && (regTypeMar.equals(OntarioAlias.ENR_REG_TYPE_PT))
                                        && (scoreDate.after(m_calHalf1EndDt)))) {
                            assignCtRegPt++;
                        } else {
                            assignCtRegFt++;
                        }
                    } else {
                        assignCtRegFt++;
                    }
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write assignment count for last section/student
        if (!StringUtils.isEmpty(mstOidStdOidPrev)) {
            Map<String, Integer> assignCtRegByRegType = new HashMap<String, Integer>();
            assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_FT, Integer.valueOf(assignCtRegFt));
            assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_PT, Integer.valueOf(assignCtRegPt));
            m_assignCtRegToRegTypeToMstOidStdOid.put(mstOidStdOidPrev, assignCtRegByRegType);
            m_assignCtSummerToMstOidStdOid.put(mstOidStdOidPrev, assignCtSummer);
        }
    }

    /**
     * Loads bell periods to bel oid, period name map
     */
    protected void loadBpeToBelOidPeriodNm() {
        m_bpeTimeToBelOidPeriodNmMap = new HashMap<String, ScheduleBellPeriod>();

        // create criteria/query to get bell schedule information - fetches for all
        // schools though
        // may not be needed
        X2Criteria bpeCriteria = new X2Criteria();

        QueryByCriteria bpeQuery = new QueryByCriteria(ScheduleBellPeriod.class, bpeCriteria);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.COL_BELL_SCHEDULE_OID);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + PATH_DELIMITER + SchedulePeriod.COL_NAME);

        // load map of schedule bell period to schedule bell oid/period name
        try (QueryIterator bpeIterator = getBroker().getIteratorByQuery(bpeQuery)) {
            // iterates through and saves into map
            while (bpeIterator.hasNext()) {
                ScheduleBellPeriod bpe = (ScheduleBellPeriod) bpeIterator.next();

                // get new values
                String belOid = bpe.getBellScheduleOid();
                String bpePeriodNm = bpe.getSchedulePeriod().getName();

                // load into map
                String keyField = belOid + CONST_HYPHEN + bpePeriodNm;
                m_bpeTimeToBelOidPeriodNmMap.put(keyField, bpe);
            }
        }
    }

    /**
     * Loads class dates/days/time lists in order of class number to master schedule
     * map
     *
     * @param ctxOid
     * @param courseIncludeDays
     * @param courseIncludeTime
     */
    protected void loadClassDateDayTimeListsToMstOid(String ctxOid,
                                                     boolean courseIncludeDays,
                                                     boolean courseIncludeTime) {
        m_classDtListToMstOidMap = new HashMap<String, List<PlainDate>>();
        m_classDayListToMstOidMap = new HashMap<String, List<String>>();
        m_classStartEndTimeToMstOidMap = new HashMap<String, String>();

        /*
         * get the master schedules to ScheduleDays that selected sections meet also
         * lists of school oids, day numbers to use in next query
         */
        Map<String, List<String>> mstOidsToSklOidDayNumMap = new HashMap<String, List<String>>();
        Map<String, List<String>> periodNmsToMstOidMap = new HashMap<String, List<String>>();
        List<String> mstSklOidsToRestrictSelection = new ArrayList<String>();
        List<Integer> mstDayNumsToRestrictSelection = new ArrayList<Integer>();

        // create criteria/query to get schedule days for mstOids
        X2Criteria mtxCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);
        mtxCriteria.addExists(subQuery);

        String[] mtxColumns = new String[] {
                // school oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                        + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                // schedule day number
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_DAY
                        + PATH_DELIMITER + ScheduleDay.COL_NUMBER,
                // schedule period name
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.REL_SCHEDULE_PERIOD
                        + PATH_DELIMITER + SchedulePeriod.COL_NAME,
                // master schedule oid
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID};
        ColumnQuery mtxQuery = new ColumnQuery(MasterScheduleMatrix.class, mtxColumns, mtxCriteria);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + PATH_DELIMITER
                        + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_DAY + PATH_DELIMITER + ScheduleDay.COL_NUMBER);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_PERIOD + PATH_DELIMITER + SchedulePeriod.COL_NAME);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);

        // load map of schedule days to mstOids
        String mtxQuerySklOidDayNumPrev = null;
        List<String> mstOidsForSklOidDayNum = new ArrayList<String>();
        try (ReportQueryIterator mtxIterator = getBroker().getReportQueryIteratorByQuery(mtxQuery)) {
            // iterates through using sort key which is school/day/period/mstOid
            while (mtxIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) mtxIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = Integer.valueOf(((BigDecimal) data[1]).intValue());
                String periodNm = ((String) data[2]);
                String mstOid = data[3].toString();
                String sklOidDayNum = sklOid + CONST_SPLIT_STR + dayNum.toString();

                // if new key school/day, write section list for prev key
                if ((!(mtxQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(mtxQuerySklOidDayNumPrev))) {
                    mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);

                    // initialize list
                    mstOidsForSklOidDayNum = new ArrayList<String>();
                }
                mtxQuerySklOidDayNumPrev = sklOidDayNum;

                // save sklOids, day numbers in list
                if (!mstSklOidsToRestrictSelection.contains(sklOid)) {
                    mstSklOidsToRestrictSelection.add(sklOid);
                }
                if (!mstDayNumsToRestrictSelection.contains(dayNum)) {
                    mstDayNumsToRestrictSelection.add(dayNum);
                }

                // add section to list for school/day
                // may be duplicates including if multiple periods so saved only once
                if (!mstOidsForSklOidDayNum.contains(mstOid)) {
                    mstOidsForSklOidDayNum.add(mstOid);
                }

                // add period names by mstOid
                List<String> periodNmsForMst = periodNmsToMstOidMap.get(mstOid);
                if (periodNmsForMst == null) {
                    periodNmsForMst = new ArrayList<String>();
                }
                if (!periodNmsForMst.contains(periodNm)) {
                    periodNmsForMst.add(periodNm);
                }
                periodNmsToMstOidMap.put(mstOid, periodNmsForMst);
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write section list for last school/day
        if (!(mtxQuerySklOidDayNumPrev == null)) {
            mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);
        }

        /*
         * get SchoolCalendarDates(full year)/days-of-week lists to selected sections
         * also get bell schedule by school/day number (assuming 1 to 1 relation)
         */
        Map<String, List<PlainDate>> classDtFullYrToMstOidMap = new HashMap<String, List<PlainDate>>();
        Map<String, String> belOidToMstOidMap = new HashMap<String, String>();

        // create criteria/query to get school calendar dates for mstOids
        X2Criteria csdCriteria = new X2Criteria();

        // condition for school year/schools/day numbers for selected sections
        csdCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                ctxOid);
        csdCriteria.addIn(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                mstSklOidsToRestrictSelection);
        csdCriteria.addIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, mstDayNumsToRestrictSelection);

        String[] csdColumns = new String[] {
                // school oid
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                // calendar id
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                // schedule day number
                SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER,
                // date
                SisSchoolCalendarDate.COL_DATE,
                // schedule bell oid
                SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID};
        ColumnQuery csdQuery = new ColumnQuery(SisSchoolCalendarDate.class, csdColumns, csdCriteria);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        // load map of schedule days to mstOids
        String csdQuerySklOidCalIdDayNumPrev = null;
        Collection<PlainDate> csdDatesForSklOidCalIdDayNum = new ArrayList<PlainDate>();
        Collection<String> csdDaysForSklOidCalIdDayNum = new ArrayList<String>();
        String belOidForSklOidCalIdDayNum = CONST_EMPTY;
        try (ReportQueryIterator csdIterator = getBroker().getReportQueryIteratorByQuery(csdQuery)) {
            while (csdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) csdIterator.next();
                String sklOid = data[0].toString();
                String calId = CONST_EMPTY;
                if (data[1] != null) {
                    calId = data[1].toString();
                }
                Integer dayNum = Integer.valueOf(((BigDecimal) data[2]).intValue());
                PlainDate csdDate = new PlainDate((Timestamp) data[3]);
                String sklOidCalIdDayNum = sklOid + CONST_SPLIT_STR + calId + CONST_SPLIT_STR + dayNum.toString();
                String belOid = CONST_EMPTY;
                if (data[4] != null) {
                    belOid = data[3].toString();
                }

                // if new key school/cal id/day, write class date list for prev key
                if ((!(csdQuerySklOidCalIdDayNumPrev == null))
                        && (!sklOidCalIdDayNum.equals(csdQuerySklOidCalIdDayNumPrev))) {
                    // add school dates/days to lists to mstOid
                    // get calendar id and key without calendar id
                    String calIdPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[1];
                    String csdQuerySklOidDayNumPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[0]
                            + CONST_SPLIT_STR + csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[2];
                    List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
                    for (String mstOid : mstOidsForSklOidDayNumLoop) {
                        // check if calendar Id is one for section or null if no students
                        String calIdMst = null;
                        if (m_stdCalIdToMstOid.containsKey(mstOid)) {
                            calIdMst = m_stdCalIdToMstOid.get(mstOid);
                        }
                        if ((calIdMst == null) || (calIdPrev.equals(calIdMst))) {
                            // add school dates (for full year)
                            List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                            if (csdDates == null) {
                                csdDates = new ArrayList<PlainDate>();
                            }
                            csdDates.addAll(csdDatesForSklOidCalIdDayNum);
                            classDtFullYrToMstOidMap.put(mstOid, csdDates);

                            // add days of week
                            if (courseIncludeDays) {
                                List<String> csdDays = m_classDayListToMstOidMap.get(mstOid);
                                if (csdDays == null) {
                                    csdDays = new ArrayList<String>();
                                }
                                csdDays.addAll(csdDaysForSklOidCalIdDayNum);
                                m_classDayListToMstOidMap.put(mstOid, csdDays);
                            }

                            // add bell schedule oid if not populated already
                            // - taking first, only one set of times on report
                            if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                                belOidToMstOidMap.put(mstOid, belOidForSklOidCalIdDayNum);
                            }
                        }
                    }

                    // initialize lists
                    csdDatesForSklOidCalIdDayNum = new ArrayList<PlainDate>();
                    csdDaysForSklOidCalIdDayNum = new ArrayList<String>();
                    belOidForSklOidCalIdDayNum = CONST_EMPTY;
                }
                csdQuerySklOidCalIdDayNumPrev = sklOidCalIdDayNum;

                // add class date to list for school/day
                if (!csdDatesForSklOidCalIdDayNum.contains(csdDate)) {
                    csdDatesForSklOidCalIdDayNum.add(csdDate);

                    // save the day-of-week if several have not been been saved for the day number
                    if ((courseIncludeDays)
                            && (csdDatesForSklOidCalIdDayNum.size() < CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP)) {
                        m_calendar.setTime(csdDate);
                        int csdDateDayOfWeek = m_calendar.get(Calendar.DAY_OF_WEEK);
                        csdDaysForSklOidCalIdDayNum.add(Integer.valueOf(csdDateDayOfWeek).toString());
                    }
                }

                // save bell schedule oid for school/day
                if ((courseIncludeTime) && (!StringUtils.isEmpty(belOid))) {
                    belOidForSklOidCalIdDayNum = belOid;
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write class date list for last school/day
        if (!(csdQuerySklOidCalIdDayNumPrev == null)) {
            // add school dates/days to lists to mstOid
            // get calendar id and key without calendar id
            String calIdPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[1];
            String csdQuerySklOidDayNumPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[0] + CONST_SPLIT_STR
                    + csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[2];
            List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
            for (String mstOid : mstOidsForSklOidDayNumLoop) {
                // check if calendar Id is one for section or null if no students
                String calIdMst = null;
                if (m_stdCalIdToMstOid.containsKey(mstOid)) {
                    calIdMst = m_stdCalIdToMstOid.get(mstOid);
                }
                if ((calIdMst == null) || (calIdPrev.equals(calIdMst))) {
                    // add school dates (for full year)
                    List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                    if (csdDates == null) {
                        csdDates = new ArrayList<PlainDate>();
                    }
                    csdDates.addAll(csdDatesForSklOidCalIdDayNum);
                    classDtFullYrToMstOidMap.put(mstOid, csdDates);

                    // add days of week
                    if (courseIncludeDays) {
                        List<String> csdDays = m_classDayListToMstOidMap.get(mstOid);
                        if (csdDays == null) {
                            csdDays = new ArrayList<String>();
                        }
                        csdDays.addAll(csdDaysForSklOidCalIdDayNum);
                        m_classDayListToMstOidMap.put(mstOid, csdDays);
                    }

                    // add bell schedule oid if not populated already
                    // - taking first, only one set of times on report
                    if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                        belOidToMstOidMap.put(mstOid, belOidForSklOidCalIdDayNum);
                    }
                }
            }
        }

        /*
         * clean class date lists to only leave for dates within schedule term sort
         * class date lists saved to mstOids save class time (start/end) by mstOids
         */
        // get sections
        Set<String> mstOids = classDtFullYrToMstOidMap.keySet();

        // get class date list
        // get unsorted list, sort for each section and add to output map
        // for class time
        // get period names for mst, get bell schedule times
        for (String mstOid : mstOids) {
            // get class date list
            List<PlainDate> classDtsFullYr = classDtFullYrToMstOidMap.get(mstOid);
            PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
            PlainDate classEndDate = m_classEndDateToMstOid.get(mstOid);

            // process class date list for each section
            List<PlainDate> classDts = new ArrayList<PlainDate>();
            if (classDtsFullYr != null) {
                // create class date list for section's schedule term
                for (PlainDate classDt : classDtsFullYr) {
                    if ((classDt.compareTo(classStartDate) >= 0) && (classDt.compareTo(classEndDate) <= 0)) {
                        classDts.add(classDt);
                    }
                }

                // sort list of dates
                Collections.sort(classDts, new Comparator<PlainDate>() {
                    @Override
                    public int compare(PlainDate o1, PlainDate o2) {
                        return o1.compareTo(o2);
                    }
                });
            }

            // save sorted list
            m_classDtListToMstOidMap.put(mstOid, classDts);

            if (courseIncludeTime) {
                // initialize section start/end times
                PlainTime startTimeMst = null;
                PlainTime endTimeMst = null;
                Double durationMst = null;

                // get start/end time and class length if overriding values saved for section
                MasterSchedule mst = m_mstToMstOid.get(mstOid);
                String startTimeMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_START_TIME);
                if (startTimeMstStr != null) {
                    String[] startTimeMstStrSplit = startTimeMstStr.split(CONST_COLON);
                    if (startTimeMstStrSplit.length >= 2) {
                        startTimeMst = new PlainTime();
                        int startTimeHourInt = Integer.valueOf(startTimeMstStrSplit[0]).intValue();
                        int startTimeMinInt = Integer.valueOf(startTimeMstStrSplit[1]).intValue();
                        m_calendar.setTime(startTimeMst);
                        m_calendar.set(Calendar.HOUR_OF_DAY, startTimeHourInt);
                        m_calendar.set(Calendar.MINUTE, startTimeMinInt);
                        startTimeMst.setTime(m_calendar.getTimeInMillis());
                    }
                }
                String endTimeMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_END_TIME);
                if (endTimeMstStr != null) {
                    String[] endTimeMstStrSplit = endTimeMstStr.split(CONST_COLON);
                    if (endTimeMstStrSplit.length >= 2) {
                        endTimeMst = new PlainTime();
                        int endTimeHourInt = Integer.valueOf(endTimeMstStrSplit[0]).intValue();
                        int endTimeMinInt = Integer.valueOf(endTimeMstStrSplit[1].split(CONST_SPACE)[0]).intValue();
                        m_calendar.setTime(endTimeMst);
                        m_calendar.set(Calendar.HOUR_OF_DAY, endTimeHourInt);
                        m_calendar.set(Calendar.MINUTE, endTimeMinInt);
                        endTimeMst.setTime(m_calendar.getTimeInMillis());
                    }
                }
                String durationMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_LENGTH_MINS);
                if (durationMstStr != null) {
                    durationMst = Double.valueOf(Integer.valueOf(durationMstStr).doubleValue());
                }

                // get start/end times from periods if overrides do not exist for either
                if ((startTimeMst == null) || (endTimeMst == null)) {
                    // get period names list
                    List<String> periodNms = periodNmsToMstOidMap.get(mstOid);

                    // sort period names
                    Collections.sort(periodNms);

                    // get class start/end time
                    if (belOidToMstOidMap.get(mstOid) != null) {
                        String belOid = belOidToMstOidMap.get(mstOid);
                        // look for lowest start time and highest end time for class periods
                        // -(if bell schedule is found)
                        if (!StringUtils.isEmpty(belOid)) {
                            for (String periodNm : periodNms) {
                                String keyField = belOid + CONST_HYPHEN + periodNm;
                                PlainTime startTimeBpe = null;
                                PlainTime endTimeBpe = null;

                                ScheduleBellPeriod bpe = m_bpeTimeToBelOidPeriodNmMap.get(keyField);
                                if (bpe != null) {
                                    startTimeBpe = bpe.getStartTime();
                                    endTimeBpe = bpe.getEndTime();
                                }

                                if ((startTimeBpe != null)
                                        && ((startTimeMst == null) || (startTimeBpe.before(startTimeMst)))) {
                                    startTimeMst = startTimeBpe;
                                }
                                if ((endTimeBpe != null) && ((endTimeMst == null) || (endTimeBpe.after(endTimeMst)))) {
                                    endTimeMst = endTimeBpe;
                                }
                            }
                        }
                    }
                }

                // format resulting times
                String classStartTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;
                if (startTimeMst != null) {
                    m_calendar.setTime(startTimeMst);
                    int classStartHourInt = m_calendar.get(Calendar.HOUR_OF_DAY);
                    int classStartMinInt = m_calendar.get(Calendar.MINUTE);
                    String classStartAmPm = CONST_AM;
                    if (classStartHourInt == CONST_ZERO.intValue()) {
                        classStartHourInt = CONST_PM_HOUR;
                        classStartAmPm = CONST_AM;
                    } else if (classStartHourInt > CONST_PM_HOUR) {
                        classStartHourInt = classStartHourInt - CONST_PM_HOUR;
                        classStartAmPm = CONST_PM;
                    } else if (classStartHourInt == CONST_PM_HOUR) {
                        classStartAmPm = CONST_PM;
                    }
                    classStartTime = CONST_FORMAT_INT_2.format(classStartHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classStartMinInt) + CONST_COMMA + classStartAmPm;
                }
                String classEndTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;
                if (endTimeMst != null) {
                    m_calendar.setTime(endTimeMst);
                    int classEndHourInt = m_calendar.get(Calendar.HOUR_OF_DAY);
                    int classEndMinInt = m_calendar.get(Calendar.MINUTE);
                    String classEndAmPm = CONST_AM;
                    if (classEndHourInt == CONST_ZERO.intValue()) {
                        classEndHourInt = CONST_PM_HOUR;
                        classEndAmPm = CONST_AM;
                    } else if (classEndHourInt > CONST_PM_HOUR) {
                        classEndHourInt = classEndHourInt - CONST_PM_HOUR;
                        classEndAmPm = CONST_PM;
                    } else if (classEndHourInt == CONST_PM_HOUR) {
                        classEndAmPm = CONST_PM;
                    }
                    classEndTime = CONST_FORMAT_INT_2.format(classEndHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classEndMinInt) + CONST_COMMA + classEndAmPm;
                }
                // get class duration
                String classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(CONST_ZERO_DBL);
                if (durationMst != null) {
                    double classDurationDbl = durationMst.doubleValue() / CONST_HOUR_TO_MINS_DBL;
                    classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(classDurationDbl);
                } else {
                    if ((startTimeMst != null) && (endTimeMst != null)) {
                        LocalDateTime startTimeMstLocal = new java.sql.Timestamp(startTimeMst.getTime())
                                .toLocalDateTime();
                        LocalDateTime endTimeMstLocal = new java.sql.Timestamp(endTimeMst.getTime()).toLocalDateTime();
                        Duration classDurationObject = Duration.between(startTimeMstLocal, endTimeMstLocal);
                        long classDurationMins = classDurationObject.toMinutes();
                        double classDurationDbl = Double.valueOf(classDurationMins).doubleValue()
                                / CONST_HOUR_TO_MINS_DBL;
                        classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(classDurationDbl);
                    }
                }

                // save result by mstOid
                m_classStartEndTimeToMstOidMap.put(mstOid,
                        classStartTime + CONST_COMMA + classEndTime + CONST_COMMA + classDuration);
            }
        }
    }

    /**
     * Loads class attendance list in order of class number to student to master
     * schedule map
     */
    protected void loadClassAttListToStdOidToMstOid() {
        // create criteria/query to get class attendance for selected sections
        X2Criteria patCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patCriteria.addExists(subQuery);

        String[] patColumns = new String[] {StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID,
                StudentPeriodAttendance.COL_STUDENT_OID, StudentPeriodAttendance.COL_DATE,
                StudentPeriodAttendance.COL_CODE_VIEW, StudentPeriodAttendance.COL_OTHER_CODE,
                StudentPeriodAttendance.COL_OTHER_CODE02};

        ColumnQuery patQuery = new ColumnQuery(StudentPeriodAttendance.class, patColumns, patCriteria);

        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_DATE);

        // load map of class attendance to master schedule/student
        m_attListToMstOidStdOidMap = new HashMap<String, List<String>>();
        m_attAbsentUnfundedCtToMstOidStdOid = new HashMap<String, Integer>();
        String mstOidPrev = CONST_EMPTY;
        String mstOidStdOidPrev = CONST_EMPTY;
        List<String> attList = null;
        int attAbsentConsecCtAbs = CONST_ZERO.intValue();
        int attAbsentUnfundedCt = CONST_ZERO.intValue();
        // these will be initialized based on class dates for section
        List<PlainDate> classDtListForMstOid = null;
        int classDtListSize = CONST_ZERO.intValue();
        // loop through class attendance
        try (ReportQueryIterator patIterator = getBroker().getReportQueryIteratorByQuery(patQuery)) {
            while (patIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) patIterator.next();
                String mstOid = data[0].toString();
                String stdOid = data[1].toString();
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;
                PlainDate patDate = new PlainDate((Timestamp) data[2]);
                String patCodeIn = data[3].toString();
                String patOtherCode = OntarioAlias.CONST_EMPTY;
                if (data[4] != null) {
                    patOtherCode = data[4].toString();
                }
                String patOtherCode02 = OntarioAlias.CONST_EMPTY;
                if (data[5] != null) {
                    patOtherCode02 = data[5].toString();
                }

                // continue to next iteration if patDate is not between start and end date for
                // std for mst
                EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = m_stdStartEndDatesToMstOidMap
                        .get(mstOid);
                if ((stdEnrStartEndDatesForMst == null)
                        || (!stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(stdOid, patDate))) {
                    continue;
                }

                // if new section/student, write attendance count for prev
                if ((!StringUtils.isEmpty(mstOidStdOidPrev) && (!mstOidStdOidPrev.equals(mstOidStdOid)))) {
                    if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                        attAbsentUnfundedCt += attAbsentConsecCtAbs;
                    }

                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        attList = setAttCodeListToFrench(attList);
                    }
                    m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
                    m_attAbsentUnfundedCtToMstOidStdOid.put(mstOidStdOidPrev, Integer.valueOf(attAbsentUnfundedCt));
                    attList = null;
                    attAbsentConsecCtAbs = CONST_ZERO.intValue();
                    attAbsentUnfundedCt = CONST_ZERO.intValue();
                }
                mstOidStdOidPrev = mstOidStdOid;

                // if new section get class dates and size of class dates list
                if (!mstOid.equals(mstOidPrev)) {
                    classDtListForMstOid = m_classDtListToMstOidMap.get(mstOid);
                    if (classDtListForMstOid != null) {
                        classDtListSize = classDtListForMstOid.size();
                    } else {
                        classDtListSize = CONST_ZERO.intValue();
                    }
                }
                mstOidPrev = mstOid;

                // get index of att date in class date list
                int indexAttDt = CONST_LIST_NOT_FOUND;
                if ((classDtListForMstOid != null) && (classDtListForMstOid.contains(patDate))) {
                    indexAttDt = classDtListForMstOid.indexOf(patDate);
                }

                // save class attendance if att date is found in class date list
                if (!(indexAttDt == CONST_LIST_NOT_FOUND)) {
                    // initialize class attendance list for student/section if first attendance for
                    // them
                    if (attList == null) {
                        attList = new ArrayList<String>(classDtListSize);
                        for (int i = 0; i < classDtListSize; i++) {
                            attList.add(CONST_EMPTY);
                        }
                    }

                    // save class attendance
                    String patCodeEnlish = patCodeIn;
                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        if (OntarioAlias.ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn) != null) {
                            patCodeEnlish = OntarioAlias.ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn);
                        }
                    }

                    if (patCodeEnlish.contains(OntarioAlias.ATT_CODE_GENERAL_ABSENCE)) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_GENERAL_ABSENCE;
                    } else if (
                    // if other codes show cancelled unfunded move corresponding report code
                    // other code
                    ((patOtherCode != null) && (patOtherCode.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED)))
                            // other code 02
                            || ((patOtherCode02 != null)
                                    && (patOtherCode02.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED)))) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT;
                    } else if (
                    // if other codes show cancelled funded move corresponding report code
                    // other code
                    ((patOtherCode != null) && (patOtherCode.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED)))
                            // other code 02
                            || ((patOtherCode02 != null)
                                    && (patOtherCode02.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED)))) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT;
                    }

                    // set pat code in output list
                    attList.set(indexAttDt, patCodeEnlish.trim());

                    // check if attendance code is not present or tardy
                    if ((!StringUtils.isEmpty(patCodeEnlish))
                            && (!patCodeEnlish.contains(OntarioAlias.ATT_CODE_TARDY))) {
                        // accumulate/reset consecutive count of absences
                        // if previous value counts as consecutive
                        if ((indexAttDt > CONST_ZERO.intValue()) && (OntarioAlias.ATT_CODE_CON_ED_CONSECUTIVE_INCL
                                .contains(attList.get(indexAttDt - 1)))) {
                            if (OntarioAlias.ATT_CODE_ABSENT.equals(attList.get(indexAttDt))) {
                                attAbsentConsecCtAbs++;
                            }
                        } else
                        // if previous value does not count as consecutive
                        {
                            if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                                attAbsentUnfundedCt += attAbsentConsecCtAbs;
                            }
                            attAbsentConsecCtAbs = CONST_ZERO.intValue();
                            if (OntarioAlias.ATT_CODE_ABSENT.equals(attList.get(indexAttDt))) {
                                attAbsentConsecCtAbs++;
                            }
                        }

                        // if consecutive absences (including unfunded) are greater than 3 and
                        // attendance code is not funded add to days absent unfunded
                        if (patCodeEnlish.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT)) {
                            attAbsentUnfundedCt++;
                        }
                    }
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write class attendance for students for last section/student
        if (!StringUtils.isEmpty(mstOidStdOidPrev)) {
            // write class attendance for students for previous section
            if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                attAbsentUnfundedCt += attAbsentConsecCtAbs;
            }

            if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                attList = setAttCodeListToFrench(attList);
            }
            m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
            m_attAbsentUnfundedCtToMstOidStdOid.put(mstOidStdOidPrev, Integer.valueOf(attAbsentUnfundedCt));
        }
    }

    /**
     * Build output grid
     */
    protected void buildOutputGrid() {
        // loop through master schedule/student and put output grid records
        Set<String> mstOidsSorted = m_stdOidListToMstOid.keySet();

        // initialize variables
        m_assignCtNonOpRegToMstOidMap = new HashMap<String, Integer>();
        m_assignCtNonOpSummerToMstOidMap = new HashMap<String, Integer>();
        m_assignCtNonOpFtToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtMaleOctToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtFemaleOctToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtMaleJunToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtFemaleJunToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtMaleAugToMstOidMap = new HashMap<String, Integer>();
        m_pupilCtFemaleAugToMstOidMap = new HashMap<String, Integer>();
        m_attFundedCtNonOpToMstOidMap = new HashMap<String, Integer>();

        // loop through sections
        for (String mstOid : mstOidsSorted) {
            MasterSchedule mst = m_mstToMstOid.get(mstOid);
            PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
            List<String> stdOids = m_stdOidListToMstOid.get(mstOid);
            EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = m_stdStartEndDatesToMstOidMap.get(mstOid);
            int stdCt = 0;

            int assignCtNonOpRegMst = CONST_ZERO.intValue();
            int assignCtNonOpSummerMst = CONST_ZERO.intValue();
            int assignCtNonOpFtMst = CONST_ZERO.intValue();
            int pupilCtMaleOctMst = CONST_ZERO.intValue();
            int pupilCtFemaleOctMst = CONST_ZERO.intValue();
            int pupilCtMaleJunMst = CONST_ZERO.intValue();
            int pupilCtFemaleJunMst = CONST_ZERO.intValue();
            int pupilCtMaleAugMst = CONST_ZERO.intValue();
            int pupilCtFemaleAugMst = CONST_ZERO.intValue();
            int attFundedCtNonOpMst = CONST_ZERO.intValue();

            // write detail records twice if days met go over 45
            ReportDataGrid gridForMstForPage2 = new ReportDataGrid();
            // write detail records thrice if days met go over 145
            ReportDataGrid gridForMstForPage3 = new ReportDataGrid();

            // get class dates, number of class sessions, student drop (saved in effective
            // end date) dates if report is by date
            List<PlainDate> classDtListForMstOid = new ArrayList<PlainDate>();
            int classSessionsCtForMstOid = 0;
            if ((m_byDate) && (m_classDtListToMstOidMap.get(mstOid) != null)) {
                classDtListForMstOid = m_classDtListToMstOidMap.get(mstOid);
                classSessionsCtForMstOid = classDtListForMstOid.size();
            }

            // loop through students
            for (String stdOid : stdOids) {
                SisStudent std = getBroker().getBeanByOid(SisStudent.class, stdOid);
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;
                stdCt++;

                // get op indicator from enr info, registration type, first enrolment date as
                // string
                String opInd = CONST_EMPTY;
                if (m_enrInfoToStdOidTypeMap.containsKey(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX)) {
                    opInd = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX);
                }
                String regTypeOct = CONST_EMPTY;
                String regTypeMar = CONST_EMPTY;

                SisSchool stdSchool = std.getSchool();
                if ((stdSchool != null)
                        && (stdSchool.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_SPECIAL_CONDITION) != null)
                        && (OntarioAlias.CONST_SKL_SPECIAL_CONDITION_CON_ED_LIST
                                .contains(stdSchool.getFieldValueByAlias(OntarioAlias.ALIAS_SKL_SPECIAL_CONDITION)))) {
                    regTypeOct = CONST_REG_TYPE_SKL_CON_ED;
                    regTypeMar = CONST_REG_TYPE_SKL_CON_ED;
                } else {
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
                }

                // get school month for student (first) start date to see if op
                PlainDate effStartDateForStd = stdEnrStartEndDatesForMst.getEffStartDateForStdForMst(stdOid);
                m_calendar.setTime(effStartDateForStd);
                int classStartDateForStdSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP
                        .get(m_calendar.get(Calendar.MONTH) + 1).intValue();

                // update op indicator based on student start date and school association for
                // student/section school
                String opIndSsk = null;
                String mstSklOid = mst.getSchoolCourse().getSchoolOid();
                if ((m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid) != null)
                        && (m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid).get(mstSklOid) != null)) {
                    Map<Integer, String> sskOpIndToSklMthForSklOid = m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid)
                            .get(mstSklOid);
                    opIndSsk = sskOpIndToSklMthForSklOid.get(Integer.valueOf(classStartDateForStdSklMonth));
                }
                if (opIndSsk != null) {
                    opInd = opIndSsk;
                }

                // start set grid fields
                m_grid.append();
                m_grid.set(FIELD_MASTER_SCHEDULE, mst);
                m_grid.set(FIELD_STUDENT, std);
                m_grid.set(FIELD_OP_IND, opInd);
                m_grid.set(FIELD_REG_TYPE_OCT, regTypeOct);
                m_grid.set(FIELD_REG_TYPE_MAR, regTypeMar);

                // if report is by assignment - process assignments at section/student level
                if (m_byAssign) {
                    // get assignment count
                    int assignCtRegFt = CONST_ZERO.intValue();
                    int assignCtRegPt = CONST_ZERO.intValue();
                    int assignCtSummer = CONST_ZERO.intValue();
                    if (m_assignCtRegToRegTypeToMstOidStdOid.get(mstOidStdOid) != null) {
                        Map<String, Integer> assignCtRegByRegType = m_assignCtRegToRegTypeToMstOidStdOid
                                .get(mstOidStdOid);
                        assignCtRegFt = assignCtRegByRegType.get(OntarioAlias.ENR_REG_TYPE_FT).intValue();
                        assignCtRegPt = assignCtRegByRegType.get(OntarioAlias.ENR_REG_TYPE_PT).intValue();
                    }
                    if (m_assignCtSummerToMstOidStdOid.get(mstOidStdOid) != null) {
                        assignCtSummer = m_assignCtSummerToMstOidStdOid.get(mstOidStdOid).intValue();
                    }

                    // if summer class set all assignments to summer
                    if (!classStartDate.before(m_calSummerEnrStartDt)) {
                        assignCtSummer += assignCtRegFt;
                        assignCtRegFt = CONST_ZERO.intValue();
                        assignCtSummer += assignCtRegPt;
                        assignCtRegPt = CONST_ZERO.intValue();
                    }

                    // if school is cont. ed. set assignment count to PT if FT
                    if (regTypeOct.equals(CONST_REG_TYPE_SKL_CON_ED)) {
                        assignCtRegPt += assignCtRegFt;
                        assignCtRegFt = CONST_ZERO.intValue();
                    }

                    // set assignment counts to op indicator and register type
                    int assignCtNonOpReg = CONST_ZERO.intValue();
                    int assignCtNonOpFtReg = CONST_ZERO.intValue();
                    int assignCtNonOpSummer = CONST_ZERO.intValue();
                    if ((StringUtils.isEmpty(opInd)) || (!opInd.equals(OntarioAlias.CONST_ENR_OP_TXT))) {
                        assignCtNonOpReg = assignCtRegFt + assignCtRegPt;
                        assignCtNonOpFtReg = assignCtRegFt;
                        assignCtNonOpSummer = assignCtSummer;
                    }

                    // accumulate assignment counts for master schedule
                    assignCtNonOpRegMst += assignCtNonOpReg;
                    assignCtNonOpSummerMst += assignCtNonOpSummer;
                    assignCtNonOpFtMst += assignCtNonOpFtReg;

                    // set assignment grid fields
                    m_grid.set(FIELD_ASSIGN_CT, Integer.valueOf(assignCtRegFt + assignCtRegPt + assignCtSummer));
                    m_grid.set(FIELD_ASSIGN_CT_NON_OP_REG, Integer.valueOf(assignCtNonOpReg));
                    m_grid.set(FIELD_ASSIGN_CT_NON_OP_SUMMER, Integer.valueOf(assignCtNonOpSummer));
                    m_grid.set(FIELD_ASSIGN_CT_FUNDED_REG, Integer.valueOf(assignCtNonOpReg - assignCtNonOpFtReg));
                    m_grid.set(FIELD_ASSIGN_CT_FUNDED_SUMMER, Integer.valueOf(assignCtNonOpSummer));
                }

                // if report is by date - process attendance at section/student level
                if (m_byDate) {
                    // check if dual credit course (if so all not funded)
                    boolean cskDualCredit = false;
                    String cskCourseDeliveryType = (String) mst.getSchoolCourse()
                            .getFieldValueByAlias(OntarioAlias.ALIAS_CSK_COURSE_DELIVERY_TYPE);
                    if ((!StringUtils.isEmpty(cskCourseDeliveryType))
                            && (m_courseDeliveryTypeListForDualCredit.contains(cskCourseDeliveryType))) {
                        cskDualCredit = true;
                    }

                    // get class att list
                    List<String> attListLocalized = new ArrayList<String>();
                    // get saved list if class att was found
                    if (m_attListToMstOidStdOidMap.get(mstOidStdOid) != null) {
                        attListLocalized = m_attListToMstOidStdOidMap.get(mstOidStdOid);
                    } else {
                        // else initialize class att list with spaces
                        for (int i = 0; i < classSessionsCtForMstOid; i++) {
                            attListLocalized.add(CONST_EMPTY);
                        }
                    }

                    // check if not enrolled on class date for student
                    int stdNotEnrCt = CONST_ZERO.intValue();
                    for (PlainDate classDt : classDtListForMstOid) {
                        if (!stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(stdOid, classDt)) {
                            int indexClassDt = classDtListForMstOid.indexOf(classDt);
                            if (StringUtils.isEmpty(attListLocalized.get(indexClassDt))) {
                                attListLocalized.set(indexClassDt, CONST_ATT_NOT_ENROLLED);
                                stdNotEnrCt++;
                            }
                        }
                    }

                    // get absent unfunded count and corresponding funded count
                    int absentUnfundedCt = CONST_ZERO.intValue();
                    if (cskDualCredit) {
                        // if dual credit all sessions except when student is not enrolled (which
                        // would not be funded anyways) are unfunded
                        absentUnfundedCt = classSessionsCtForMstOid - stdNotEnrCt;
                    } else if (m_attAbsentUnfundedCtToMstOidStdOid.get(mstOidStdOid) != null) {
                        absentUnfundedCt = m_attAbsentUnfundedCtToMstOidStdOid.get(mstOidStdOid).intValue();
                    }
                    int fundedCt = classSessionsCtForMstOid - absentUnfundedCt - stdNotEnrCt;

                    // set funded count to op indicator
                    int attFundedCtNonOp = CONST_ZERO.intValue();
                    if ((StringUtils.isEmpty(opInd)) || (!opInd.equals(OntarioAlias.CONST_ENR_OP_TXT))) {
                        attFundedCtNonOp = fundedCt;
                    }

                    // accumulate funded count for master schedule
                    attFundedCtNonOpMst += attFundedCtNonOp;

                    // set attendance grid/parameter fields
                    m_grid.set(FIELD_CLASS_DTL_PAGE, Integer.valueOf(1));
                    m_grid.set(FIELD_CLASS_DTL_PAGE_START, Integer.valueOf(0));
                    m_grid.set(FIELD_CLASS_ATT_LIST, attListLocalized);
                    m_grid.set(FIELD_ATT_FUNDED_CT_NON_OP, Integer.valueOf(attFundedCtNonOp));
                    m_grid.set(FIELD_STUDENT_NUMBER, Integer.valueOf(stdCt));
                    if (attListLocalized.size() > CONST_CLASS_DTL_PAGE1_CT) {
                        Map<String, Object> gridForMstCurrRowClone1 = new HashMap<String, Object>();
                        gridForMstCurrRowClone1.putAll(m_grid.getCurrentRow());
                        gridForMstForPage2.append(gridForMstCurrRowClone1);
                        gridForMstForPage2.set(FIELD_STUDENT_NUMBER, Integer.valueOf(stdCt));
                        gridForMstForPage2.set(FIELD_CLASS_DTL_PAGE, Integer.valueOf(2));
                        gridForMstForPage2.set(FIELD_CLASS_DTL_PAGE_START, Integer.valueOf(CONST_CLASS_DTL_PAGE1_CT));
                    }
                    if (attListLocalized.size() > (CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT)) {
                        Map<String, Object> gridForMstCurrRowClone2 = new HashMap<String, Object>();
                        gridForMstCurrRowClone2.putAll(m_grid.getCurrentRow());
                        gridForMstForPage3.append(gridForMstCurrRowClone2);
                        gridForMstForPage3.set(FIELD_STUDENT_NUMBER, Integer.valueOf(stdCt));
                        gridForMstForPage3.set(FIELD_CLASS_DTL_PAGE, Integer.valueOf(3));
                        gridForMstForPage3.set(FIELD_CLASS_DTL_PAGE_START, Integer.valueOf(CONST_CLASS_DTL_PAGE2_CT));
                    }
                }

                // add student to pupil counts for master schedule
                String stdGender = std.getPerson().getGenderCode();
                if (classStartDateForStdSklMonth <= OntarioAlias.CONST_SKL_MONTH_OCT) {
                    if (!stdGender.equals(OntarioAlias.CONST_GENDER_FEMALE)) {
                        pupilCtMaleOctMst++;
                    } else {
                        pupilCtFemaleOctMst++;
                    }
                } else if (classStartDateForStdSklMonth <= OntarioAlias.CONST_SKL_MONTH_JUN) {
                    if (!stdGender.equals(OntarioAlias.CONST_GENDER_FEMALE)) {
                        pupilCtMaleJunMst++;
                    } else {
                        pupilCtFemaleJunMst++;
                    }
                } else {
                    if (!stdGender.equals(OntarioAlias.CONST_GENDER_FEMALE)) {
                        pupilCtMaleAugMst++;
                    } else {
                        pupilCtFemaleAugMst++;
                    }
                }
            } // end student processing

            // adjust for summer class, to shift enrollment to August instead of June
            boolean courseSummer = false;
            if (m_classSummerCourseIndToMstOid.get(mstOid) != null) {
                courseSummer = m_classSummerCourseIndToMstOid.get(mstOid).booleanValue();
            }
            if (courseSummer) {
                int pupilCtMaleSummerMst = pupilCtMaleOctMst + pupilCtMaleJunMst + pupilCtMaleAugMst;
                int pupilCtFemaleSummerMst = pupilCtFemaleOctMst + pupilCtFemaleJunMst + pupilCtFemaleAugMst;
                pupilCtMaleOctMst = CONST_ZERO.intValue();
                pupilCtFemaleOctMst = CONST_ZERO.intValue();
                pupilCtMaleJunMst = CONST_ZERO.intValue();
                pupilCtFemaleJunMst = CONST_ZERO.intValue();
                pupilCtMaleAugMst = pupilCtMaleSummerMst;
                pupilCtFemaleAugMst = pupilCtFemaleSummerMst;
            }

            // if report is by assignment - process assignments at section level
            if (m_byAssign) {
                // save assignment total counts to mst oid
                m_assignCtNonOpRegToMstOidMap.put(mstOid, Integer.valueOf(assignCtNonOpRegMst));
                m_assignCtNonOpSummerToMstOidMap.put(mstOid, Integer.valueOf(assignCtNonOpSummerMst));
                m_assignCtNonOpFtToMstOidMap.put(mstOid, Integer.valueOf(assignCtNonOpFtMst));
            }

            // if report is by date - process attendance at section level
            if (m_byDate) {
                // save attendance total counts to mst oid
                m_attFundedCtNonOpToMstOidMap.put(mstOid, Integer.valueOf(attFundedCtNonOpMst));
                gridForMstForPage2.beforeTop();
                m_grid.append(gridForMstForPage2);
                gridForMstForPage3.beforeTop();
                m_grid.append(gridForMstForPage3);
            }

            // save pupil total counts to mst oid
            m_pupilCtMaleOctToMstOidMap.put(mstOid, Integer.valueOf(pupilCtMaleOctMst));
            m_pupilCtFemaleOctToMstOidMap.put(mstOid, Integer.valueOf(pupilCtFemaleOctMst));
            m_pupilCtMaleJunToMstOidMap.put(mstOid, Integer.valueOf(pupilCtMaleJunMst));
            m_pupilCtFemaleJunToMstOidMap.put(mstOid, Integer.valueOf(pupilCtFemaleJunMst));
            m_pupilCtMaleAugToMstOidMap.put(mstOid, Integer.valueOf(pupilCtMaleAugMst));
            m_pupilCtFemaleAugToMstOidMap.put(mstOid, Integer.valueOf(pupilCtFemaleAugMst));
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
        m_calHalf1EndDt = PlainDate
                .fromString(Integer.valueOf(getCurrentContext().getSchoolYear()) + CONST_CAL_HALF1_END);
        m_calSummerEnrStartDt = PlainDate
                .fromString(Integer.valueOf(getCurrentContext().getSchoolYear()) + CONST_CAL_SUMMER_ENR_START);
        m_calSummerAssignAfterDt = PlainDate
                .fromString(Integer.valueOf(getCurrentContext().getSchoolYear()) + CONST_CAL_SUMMER_ASSIGN_AFTER);

        // get report ids (for ireport parts)
        m_rptIdPart1 = (String) getParameter(PARAM_RPT_ID_PART1);
        m_rptIdPart2 = (String) getParameter(PARAM_RPT_ID_PART2);

        // add extra report parameters
        Map<String, String> ontarioAliasForRptMap = OntarioAlias.buildStaticValuesMap(CONST_ONTARIO_ALIAS_FOR_RPT_LIST);
        addParameter(REPORT_ONTARIO_ALIAS_FOR_RPT_MAP, ontarioAliasForRptMap);
        addParameter(REPORT_LOGO,
                OntarioToolHelper.getBase64ImageString(OntarioAlias.ON_TRILLIUM_CODE_LOGO, getBroker()));

        // add tool version
        addParameter(REPORT_VERSION, getJob().getTool().getComment());

        // Setting locale to English as localization not used yet
        initializeLocalized();

        DecimalFormat decNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC);
        decNumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());
        DecimalFormat dec5NumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC5);
        dec5NumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());
        if (m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG_MMM_DD,
                    m_locForLang.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT_MMM_YY, dateFormatOut);
        } else {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR_DD_MMM,
                    m_locForLang.getSystemLocale());
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
        // should return an empty map if there are no entries with this reference table
        // oid
        Map<String, String> refCodeBuildingCodeMap = loadReferenceCodeDescMap(OntarioAlias.REF_OID_BUILDING_CODES);
        addParameter(REPORT_BUILDING_CODES_RCD_MAP, refCodeBuildingCodeMap);
    }

    /**
     * Loads reference code description by code for reference table
     *
     * @param rtbOid
     *
     * @return Map<String, ReferenceCode>
     */
    protected Map<String, String> loadReferenceCodeDescMap(String rtbOid) {
        Map<String, String> rcdDescByCodeMap = new HashMap<String, String>();

        // create criteria for student contact
        X2Criteria rcdCriteria = new X2Criteria();

        // for students in student criteria
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

        // select query
        QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, rcdCriteria);

        // load map of reference code description to reference code
        try (QueryIterator rcdIterator = getBroker().getIteratorByQuery(rcdQuery)) {
            // iterates through and saves into map
            while (rcdIterator.hasNext()) {
                ReferenceCode rcd = (ReferenceCode) rcdIterator.next();
                String rcdCode = rcd.getCode();
                String rcdDesc = rcd.getDescription();

                rcdDescByCodeMap.put(rcdCode, rcdDesc);
            }
        }

        // return map
        return rcdDescByCodeMap;
    }

    /**
     * Sets each att code value in input string to french
     *
     * @param attListEng
     * @return List<String> - attListFr
     */
    protected List<String> setAttCodeListToFrench(List<String> attListEng) {
        List<String> attListFr = new ArrayList<String>();

        for (String attCodeEng : attListEng) {
            String attCodeFr = attCodeEng;
            if ((!StringUtils.isEmpty(attCodeEng))
                    && (OntarioAlias.ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.get(attCodeEng) != null)) {
                attCodeFr = OntarioAlias.ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.get(attCodeEng);
            }

            attListFr.add(attCodeFr);
        }

        return attListFr;
    }

    /**
     * Initializes for localization Language is set for organization or school
     *
     * Adds the localization parameters
     */
    protected void initializeLocalized() {
        Map<String, Object> locVarMap = new HashMap<String, Object>();
        MessageResources resourcesOut = OntarioToolHelper.initializeLocalized(getOrganization(), null, locVarMap,
                getBroker());
        m_locForLang = (OrganizationLocale) locVarMap.get(OrganizationLocale.class.getName());

        // get prefixes for parts of both reports and add to separate parameters
        // cannot use the same prefix as they are different reports
        addParameter(REPORT_PREFIX1, CONST_TOOLS_FOR_PREFIX + getReportOidRptId(m_rptIdPart1) + CONST_PERIOD);
        addParameter(REPORT_PREFIX2, CONST_TOOLS_FOR_PREFIX + getReportOidRptId(m_rptIdPart2) + CONST_PERIOD);

        addParameter(REPORT_SCHOOL_LOCALE, resourcesOut);
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();
        logToolMessage(Level.INFO, "Sample variable size BEFORE clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
        clearLocallyDeclaredVariables(this.getClass(), true);
        logToolMessage(Level.INFO, "Sample variable size AFTER clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
    }

    /**
     * Method to sift through all your declared member variables and sets them to
     * null for proper garbage collection.
     *
     * @param classToClear Class
     * @param recordOnToolLog boolean
     */
    protected void clearLocallyDeclaredVariables(Class classToClear, boolean recordOnToolLog) {
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used by tool: " + getTotalMemoryUsedByClass(classToClear), false);
        }
        for (java.lang.reflect.Field field : classToClear.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE && !field.getType().isPrimitive()) {
                field.setAccessible(true);
            }
            if (field.isAccessible()) {
                try {
                    field.set(this, null);
                } catch (IllegalArgumentException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalArgumentException" + "\n" + e,
                                false);
                    }
                } catch (IllegalAccessException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalAccessException" + "\n" + e,
                                false);
                    }
                }
            }
        }
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used after clearing: " + getTotalMemoryUsedByClass(classToClear), false);
        }
    }

    /**
     * Method that relies on reflection to gather the size of all locally declared
     * variables.
     *
     * @param classToMeasure
     * @return Easy to read memory size as a string
     */
    protected String getTotalMemoryUsedByClass(Class classToMeasure) {
        long totalMemoryUsed = 0;
        for (java.lang.reflect.Field field : classToMeasure.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE) {
                field.setAccessible(true);
            }
            try {
                totalMemoryUsed = totalMemoryUsed + getObjectSize(field.get(this), false);
            } catch (IllegalArgumentException e) {
                // TODO You could log exception to learn more if needed.
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                // TODO You could log expection to learn more if needed.
                e.printStackTrace();
            }
        }
        return com.x2dev.utils.MemoryUtils.formatMemoryUsed(totalMemoryUsed);
    }

    /**
     * Returns approximate footprint of an object in memory.
     *
     * @param obj Object
     * @param recordOnToolLog boolean
     * @return long
     */
    protected long getObjectSize(Object obj, boolean recordOnToolLog) {
        long memmoryUsed = 0;
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(obj);
            oos.close();
            if (recordOnToolLog) {
                logToolMessage(Level.INFO,
                        obj + " Object Data Size: " + com.x2dev.utils.MemoryUtils.formatMemoryUsed(baos.size()), false);
            }
            memmoryUsed += baos.size();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return memmoryUsed;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.reports.GradeReportJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        // check if staff view
        m_staffCurrent = null;
        if (userData.getSessionNavConfig().getApplicationContext().contextDisplayName(getBroker().getPersistenceKey())
                .equalsIgnoreCase(ApplicationContext.STAFF.toString())) {
            m_staffCurrent = userData.getStaff();
        }
    }

    /**
     * Returns the report oid based on report id
     *
     * @param reportId
     *
     * @return String - report oid
     */
    protected String getReportOidRptId(String reportId) {
        X2Criteria rptCriteria = new X2Criteria();
        rptCriteria.addEqualTo(Report.COL_ID, reportId);
        QueryByCriteria rptQuery = new QueryByCriteria(Report.class, rptCriteria);

        Report rpt = getBroker().getBeanByQuery(rptQuery);

        return rpt.getOid();
    }

    /**
     * Returns sub query of mst oids
     *
     * @param beanPath target mstoid column path
     * @return sub query
     */
    protected SubQuery getSubQueryFromMstSelection(String beanPath) {
        SubQuery mstSubQuery;

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, m_selectionMst.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + beanPath);

        mstSubQuery = new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria);

        return mstSubQuery;
    }

    /**
     * Takes an alias of field and returns either the java name or the database name
     * that field depending on the status of returnBbName.
     *
     * @param alias - String,
     * @param returnJavaName - boolean
     *
     * @return String the java name or field name of the object that is aliased
     */
    protected String getBeanPathFromAlias(String alias, boolean returnJavaName) {
        String foundField = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);

        if (field != null) {
            if (returnJavaName) {
                foundField = field.getJavaName();
            } else {
                foundField = field.getDatabaseName();
            }
        }

        return foundField;
    }

    /**
     * Takes a bean and an alias (for a udf) and returns the PlainDate in that udf.
     *
     * @param bean - X2BaseBean
     * @param alias - String
     *
     * @return PlainDate - value for alias in bean
     */
    protected PlainDate getDate(X2BaseBean bean, String alias) {
        PlainDate date = null;
        if (bean != null) {
            String value = (String) bean.getFieldValueByAlias(alias, m_dictionary);
            date = getDate(value);
        }

        return date;
    }

    /**
     * Takes a date as string and returns the PlainDate.
     *
     * @param dateStr - String in "yyyy-MM-dd" format
     *
     * @return PlainDate - value for alias in bean
     */
    protected PlainDate getDate(String dateStr) {
        PlainDate date = null;
        try {
            date = new PlainDate(CONST_FORMAT_DATE_MST_IN.parse(dateStr));
        } catch (ParseException parseExp) {
            logToolMessage(Level.WARNING, String.format("%s %s ", " Problem with date parsing", parseExp.getMessage()),
                    false);
        }

        return date;
    }

    /**
     * A utility method that adds a number of days to a calendar date and returns a
     * date value representing this new date.
     *
     * @param cal the calendar date to add days to
     * @param days the number of days to add
     *
     * @return the newly calculated date
     */
    protected PlainDate addDays(Calendar cal, int days) {
        cal.add(Calendar.DATE, days);
        PlainDate newDate = new PlainDate(cal.getTime().getTime());
        return newDate;
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
            // assumes jasper reports 5, if different version then add and call another
            // filler
            // method from ri MultipleGrid.. report
            JasperEngine5MultyReportFiller filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(),
                    getBroker());
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
                // and
                // report 2 (landscape)
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

                        // create publisher and publish mst report (one per master schedule) if
                        // publishRpt
                        if (publishRpt) {
                            // publish report for master schedule
                            publishManager.publishReportEachBean(filler5, mstPrev);

                            // reset jasper report filler (as publish is only for one master
                            // schedule)
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
     * Returns the Jasper format based on report id
     *
     * @param reportId
     *
     * @return InputStream An InputStream containing the compile Jasper format
     */
    protected InputStream getFormatRptId(String reportId) {
        InputStream format = null;

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Report.COL_ID, reportId);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report alternateFormat = (Report) getBroker().getBeanByQuery(query);
        if (alternateFormat != null) {
            format = new ByteArrayInputStream(alternateFormat.getCompiledFormat());
        }

        /*
         * If a custom format was not specified (or it was invalid) then use the
         * original report's format.
         */
        if (format == null) {
            Report report = (Report) getJob().getTool();
            format = new ByteArrayInputStream(report.getCompiledFormat());
        }

        return format;
    }

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return FIELD_MASTER_SCHEDULE;
    }

    /**
     * Gets the description.
     *
     * @param bean X2BaseBean
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDescription(com.x2dev.sis.model.beans.
     *      X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "Enrolment Register for " + ((MasterSchedule) bean).getCourseView();
    }

    /**
     * Gets the email address. Will never be published so returning null.
     *
     * @param person Person
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailAddress(SisPerson)
     */
    @Override
    public String getEmailAddress(Person person) {
        return null;
    }

    /**
     * Gets the email recipients. Will never be published so returning null.
     *
     * @param bean X2BaseBean
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        Collection<Person> recipients = null;

        return recipients;
    }

    /**
     * Returns the value of a boolean parameter or false if the parameter does not
     * exist in the input definition.
     *
     * @param key String
     * @return boolean
     */
    protected boolean getBooleanParameter(String key) {
        return (getParameter(key) != null) ? ((Boolean) getParameter(key)).booleanValue() : false;
    }

    /**
     * Inner class for saving and getting dates for student enrolment in one section
     *
     * @author Follett Software Company
     */
    protected class EnrRegStudentStartEndDatesForSection {
        // Variables - mst oid
        protected String m_mstOid;
        protected PlainDate m_mstStartDate;
        protected PlainDate m_mstEndDate;

        // Variables - effective start and end dates for each student
        protected Map<String, List<PlainDate>> m_stdStartDatesListToStdOid;
        protected Map<String, List<PlainDate>> m_stdEndDatesListToStdOid;

        /**
         * Constructor; initializes for the mst
         *
         * @param mstOid
         * @param mstStartDt
         * @param mstEndDt
         */
        protected EnrRegStudentStartEndDatesForSection(String mstOid, PlainDate mstStartDt, PlainDate mstEndDt) {
            m_mstOid = mstOid;
            m_mstStartDate = mstStartDt;
            m_mstEndDate = mstEndDt;

            m_stdStartDatesListToStdOid = new HashMap<String, List<PlainDate>>();
            m_stdEndDatesListToStdOid = new HashMap<String, List<PlainDate>>();
        }

        /**
         * Add start date for mst/std
         *
         * @param stdOid
         * @param stdMstStartDt
         */
        protected void addStdMstStartDate(String stdOid, PlainDate stdMstStartDt) {
            List<PlainDate> stdStartDatesList;
            List<PlainDate> stdEndDatesList;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            } else {
                stdStartDatesList = new ArrayList<PlainDate>();
                stdEndDatesList = new ArrayList<PlainDate>();
            }

            stdStartDatesList.add(stdMstStartDt);
            stdEndDatesList.add(m_mstEndDate);

            m_stdStartDatesListToStdOid.put(stdOid, stdStartDatesList);
            m_stdEndDatesListToStdOid.put(stdOid, stdEndDatesList);
        }

        /**
         * Add end date for mst/std
         *
         * @param stdOid
         * @param lateStartDt
         */
        protected void addStdMstEndDate(String stdOid, PlainDate stdMstEndDt) {
            List<PlainDate> stdStartDatesList;
            List<PlainDate> stdEndDatesList;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            } else {
                stdStartDatesList = new ArrayList<PlainDate>();
                stdEndDatesList = new ArrayList<PlainDate>();
            }

            if (!stdEndDatesList.isEmpty()) {
                stdEndDatesList.set(stdEndDatesList.size() - 1, stdMstEndDt);

                m_stdStartDatesListToStdOid.put(stdOid, stdStartDatesList);
                m_stdEndDatesListToStdOid.put(stdOid, stdEndDatesList);
            }
        }

        /**
         * Check if student is enrolled within class dates
         *
         * @param stdOid
         *
         * @return boolean
         */
        public boolean isStdEnrolledWithinClassDates(String stdOid) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            boolean stdEnrolledWithinClassDates = false;

            // return false if student dates not found
            if (stdStartDatesList == null) {
                return stdEnrolledWithinClassDates;
            }

            // loop thru date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (!stdEnrolledWithinClassDates)) {
                PlainDate stdStartDate = stdStartDatesList.get(indexDates);
                PlainDate stdEndDate = stdEndDatesList.get(indexDates);

                // verify student start and end date including that end date is after start (not
                // just equal)
                if ((stdEndDate.after(stdStartDate)) &&
                        ((!stdEndDate.before(m_mstStartDate)) || (!stdStartDate.after(m_mstEndDate)))) {
                    stdEnrolledWithinClassDates = true;
                }

                indexDates++;
            }

            return stdEnrolledWithinClassDates;
        }

        /**
         * Check if date is for when student enrolled in class
         *
         * @param stdOid - student oid
         * @param dateToCheck - input date to method
         *
         * @return boolean
         */
        protected boolean isDateInStdMstEnrDates(String stdOid, PlainDate dateToCheck) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            boolean dateToCheckInStdMstEnrDates = false;

            // return false if student dates not found
            if (stdStartDatesList == null) {
                return dateToCheckInStdMstEnrDates;
            }

            // loop thru date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (!dateToCheckInStdMstEnrDates)) {
                PlainDate stdStartDate = stdStartDatesList.get(indexDates);
                PlainDate stdEndDate = stdEndDatesList.get(indexDates);

                if ((!dateToCheck.before(stdStartDate)) && (!dateToCheck.after(stdEndDate))) {
                    dateToCheckInStdMstEnrDates = true;
                }

                indexDates++;
            }

            return dateToCheckInStdMstEnrDates;
        }

        /**
         * Get first (effective) start date for student for class -defaults to class
         * start date
         *
         * @param stdOid - student oid
         *
         * @return PlainDate
         */
        protected PlainDate getEffStartDateForStdForMst(String stdOid) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            PlainDate effStartDateStdMst = null;

            // return null if student dates not found
            if (stdStartDatesList == null) {
                return m_mstStartDate;
            }

            // loop thru date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (effStartDateStdMst == null)) {
                // verify that student not dropped before it starts
                PlainDate indexStdEndDate = stdEndDatesList.get(indexDates);
                if (indexStdEndDate.before(m_mstStartDate)) {
                    indexDates++;
                    continue;
                }

                effStartDateStdMst = stdStartDatesList.get(indexDates);

                indexDates++;
            }

            // if the effective start date is before class starts set to class start date
            if ((effStartDateStdMst == null) || (effStartDateStdMst.before(m_mstStartDate))) {
                effStartDateStdMst = m_mstStartDate;
            }

            // if the effective start date is before student enrolls change it to date of
            // enrollment
            PlainDate dtFirstStd = getSchool().getCurrentContext().getStartDate();
            if (m_enrInfoToStdOidTypeMap.containsKey(stdOid + OntarioAlias.CONST_ENR_DT_FIRST)) {
                String dtFirstStdStr = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_DT_FIRST);
                dtFirstStd = getDate(dtFirstStdStr);
            }
            if (dtFirstStd.after(effStartDateStdMst)) {
                effStartDateStdMst = dtFirstStd;
            }

            return effStartDateStdMst;
        }
    }
}
