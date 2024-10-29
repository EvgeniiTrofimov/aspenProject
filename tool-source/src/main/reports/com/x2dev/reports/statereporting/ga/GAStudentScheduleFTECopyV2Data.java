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
package com.x2dev.reports.statereporting.ga;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Student Schedule (Sheet)" report.
 *
 * @author X2 Development Corporation
 */
public class GAStudentScheduleFTECopyV2Data extends ReportJavaSourceNet {
    /**
     * A String comparator that returns the opposite sort order (greatest to least).
     *
     * @author X2 Development Corporation
     */
    protected static class KeyComparator implements Comparator {

        /**
         * Compare.
         *
         * @param obj1 Object
         * @param obj2 Object
         * @return int
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(Object obj1, Object obj2) {
            int comp = 0;
            if (obj1 instanceof String && obj2 instanceof String) {
                comp = -((String) obj1).compareTo((String) obj2);
            }
            return comp;
        }
    }

    /**
     * Indicator to update the student record with the schedule FTE values.
     */
    public static final String ADD_SIXTH_RECORD_PARAM = "addSixthRecord";

    /**
     * Indicator to update the student record with the schedule FTE values.
     */
    public static final String CLEAR_PROGRAM_OVERRIDE_PARAM = "clearOverride";

    /**
     * Indicator to only write the program and class information and preserve all other untouched.
     */
    public static final String SIMPLE_PRESERVE_PARAM = "simplePreserve";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * FTE Report cycle, used to determine how to find alternate schedule day for block scheduling.
     */
    public static final String REPORT_CYCLE_PARAM = "cycle";

    /**
     * FTE Report date, used to lookup class section meeting days.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the enumerated "schedule sort" report parameter. The value is an Integer.
     */
    public static final String SCHEDULE_SORT_PARAM = "scheduleSort";

    /**
     * Name for the student schedule attributes parameter. The value is a Map keyed on
     * student Oid containing student schedule attribute beans.
     */
    public static final String SCHEDULE_ATTRIBUTES_PARAM = "scheduleAttributes";

    /**
     * Name for the enumerated "student sort" report parameter. The value is an Integer.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    /**
     * Indicator to update the student record with the schedule FTE values.
     */
    public static final String UPDATE_STUDENT_PARAM = "updateStudent";

    /**
     * Indicator to update the student's GAA status from that student's IEP's GAA indicator
     */
    public static final String CALCULATE_SPED_PARAM = "calcSped";

    /**
     * Constants
     */
    private static final String CONSTANT_A = "A";
    private static final String CONSTANT_B = "B";
    private static final String CONSTANT_INCLUSION = "INCLUSION";
    private static final String CONSTANT_NO = "No";
    private static final String CONSTANT_ONE = "1";
    private static final String CONSTANT_SPACE = " ";
    private static final String CONSTANT_SPRING = "3";
    private static final String CONSTANT_YES = "Yes";
    private static final String CONSTANT_ZERO = "0";
    private static final String CRS_NUM_PATTERN = "\\d{2}\\.\\d{7}";
    private static final String EMPTY_STRING = "";
    private static final String MAP_KEY_SEPARATOR = "-";
    private static final int MAX_COURSE_NUMBER_LENGTH = 8;
    private static final String TYPE_CLASS = "Class";
    private static final String WEIGHT_ZERO = "0.0000";

    /**
     * Aliases for fields with the program code reference table.
     * Course program alias uses PROGRAM_ALIAS as is.
     * Student aliases must be appended with " 1" through " 6" for all student fields.
     */
    private static final String ALIAS_CRS_EXCLUDE = "DOE EXCLUDE CRS";
    private static final String ALIAS_CRS_MATCH_TEN_CHARS = "DOE MATCH 10 CHARACTERS";
    private static final String ALIAS_MST_INCL_DELIVERY_MODEL = "all-mst-SectionInclusionCode";
    private static final String ALIAS_SCC_CALC_CRS_STATE_CODE = "DOE CALCULATED STATE CRS CODE HISTORY";
    private static final String ALIAS_SCC_EXCLUDE = "all-scc-ExcludefromStateReporting";
    private static final String ALIAS_SCC_INCL_DELIVERY_CODE = "all-scc-StudentDeliveryModel";
    private static final String ALIAS_SSC_CALC_CRS_STATE_CODE = "DOE CALCULATED STATE CRS CODE";
    private static final String ALIAS_SSC_EXCLUDE = "all-ssc-ExcludefromStateReporting";
    private static final String ALIAS_SSC_INCL_DELIVERY_CODE = "all-scc-StudentInclusionCode";
    private static final String ALIAS_STD_ENR_TYPE = "DOE Enrollment Type";
    private static final String ALIAS_STD_FTE_CALC_CRS_STATE_CODE = "FTE CALCULATED STATE CRS CODE";
    private static final String ALIAS_STD_DOE_INCLUSION = "DOE Inclusion";
    private static final String IEP_GAA_FLAG_ALIAS = "iep-gaa-participant";
    private static final String ITINERANT_ALIAS = "DOE Itinerant";
    private static final String LOCATION_ALIAS = "DOE Location";
    private static final String PROGRAM_ALIAS = "DOE Program Code";
    private static final String PROGRAM_ORIG_ALIAS = "DOE Orig Prog Code";
    private static final String PROGRAM_OVERRIDE_ALIAS = "DOE Program Override";
    private static final String SECTION_ALIAS = "DOE Section View";
    private static final String SPEECH_ALIAS = "DOE Speech";
    private static final String STD_GAA_FLAG_ALIAS = "DOE GAA Flag";
    private static final String STUDENT_AREA_SERVED = "DOE Area Served";
    private static final String STUDENT_DISABILITY_ALIAS = "DOE Primary Exceptionality";
    private static final String TRANSPORT_ALIAS = "DOE Transport";

    /**
     * Fields in grid for iReport.
     */
    private static final String ERROR_FIELD_CALC_CODE = "CalcSchStateCode";
    private static final String ERROR_FIELD_LOCAL_ID = "StdLocalId";
    private static final String ERROR_FIELD_NAME = "StdNameView";
    private static final String FIELD_DESCRIPTION = "descr";
    private static final String FIELD_GAA_PARTICIPANT = "gaa";
    private static final String FIELD_CALC_CRS_CODE = "calcCrsCode";
    private static final String FIELD_INCLUDED = "included";
    private static final String FIELD_PRIMARY_EXCEPTIONALITY = "primaryExceptionality";
    private static final String FIELD_PROGRAM = "program";
    private static final String FIELD_REPORT_ERROR_FORMAT = "errorFormat";
    private static final String FIELD_REPORT_ERROR_GRID = "errorGrid";
    private static final String FIELD_SCHEDULE = "schedule";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_WEIGHT = "weight";
    private static final String FORMAT_ID_ERROR = "GA-SCH-V2-SUB-1";
    private static final String PARAMETER_ORGANIZATION = "organization";

    /**
     * The Class CalendarKeySet.
     */
    public class CalendarKeySet extends HashSet<String> {

        /**
         * To string.
         *
         * @return String
         * @see java.util.AbstractCollection#toString()
         */
        @Override
        public String toString() {
            List<String> list = new LinkedList();
            for (String item : this) {
                SisSchool school = null;
                String calendarCode = "";
                int separatorPos = item.indexOf(MAP_KEY_SEPARATOR);
                if (separatorPos > 0) {
                    String schoolOid = item.substring(0, separatorPos);
                    school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                    if (school == null) {
                        throw new IllegalStateException("School not found for oid [" + schoolOid + "]");
                    }
                    calendarCode = item.substring(separatorPos + MAP_KEY_SEPARATOR.length());
                }
                if (school == null) {
                    list.add("Decoding error: " + item);
                } else {
                    list.add("School: " + school.getName() + " Calendar: " + calendarCode);
                }
            }

            Collections.sort(list);
            StringBuilder out = new StringBuilder();
            for (String item : list) {
                out.append(item);
                out.append("\n");
            }

            return out.toString();
        }

    }

    /**
     * Instance variables.
     */
    private Boolean m_addSixthRecord;
    private List<String> m_aliasList = new ArrayList<String>();
    private Map<String, Course> m_allCrsNumbers = new HashMap();
    private Boolean m_calcSped;
    private Map<String, String[]> m_calendarDayNums;
    private Set<String> m_calendarDayNumsNotFound = new CalendarKeySet();
    private Boolean m_clearOverride;
    private String m_courseExcludeField;
    private String m_courseProgramField;
    private String m_crsMatchTenCharsField;
    private Map<String, ReferenceCode> m_enrCodesRcds = new HashMap<>();
    private ReportDataGrid m_errorGrid = new ReportDataGrid();
    private String m_fieldEnrCode;
    private String m_fieldStdEnrType;
    private DataDictionary m_iepDictionary;
    private Map<String, IepData> m_iepMap;
    private Map<String, Map<String, Collection<MasterScheduleMatrix>>> m_masterSchedulesMap = new HashMap();
    private String m_mstInclusionDeliveryModelField;
    private DataDictionaryField m_mstInclusionDeliveryModelDdf;
    private Map<String, ReferenceCode> m_programCodeMap = new HashMap<String, ReferenceCode>();
    private String m_programOverrideField;
    private PlainDate m_reportDate;
    private ScheduleReportHelper m_reportHelper;
    private String m_reportingCycle;
    private String m_sccCalcStateCode;
    private String m_sccExclude;
    private String m_sccInclusionDeliveryCode;
    private DataDictionaryField m_sccInclusionDeliveryDdf;
    private Map<String, List<String>> m_schoolTermOids;
    private Boolean m_simplePreserve;
    private String m_sscCalcStateCode;
    private String m_sscExclude;
    private String m_sscInclusionDeliveryCode;
    private DataDictionaryField m_sscInclusionDeliveryDdf;
    private Map<String, ReferenceCode> m_stdEnrTypeRcdsByStateCode = new HashMap<>();
    private Map<String, LinkedList<StudentEnrollment>> m_stdtEnrMap = new HashMap<>();
    private Map<String, String>[] m_studentAliasMap;
    private String m_studentAreaServedField;
    private String m_studentDisabilityField;
    private Map<String, Map<String, Object>> m_studentSchsMap;
    private Report m_subReportError;
    private boolean m_updateStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_aliasList.add(PROGRAM_ALIAS);
        m_aliasList.add(PROGRAM_ORIG_ALIAS);
        m_aliasList.add(LOCATION_ALIAS);
        m_aliasList.add(TRANSPORT_ALIAS);
        m_aliasList.add(SPEECH_ALIAS);
        m_aliasList.add(ITINERANT_ALIAS);
        m_aliasList.add(ALIAS_STD_DOE_INCLUSION);
        m_aliasList.add(SECTION_ALIAS);
        m_aliasList.add(ALIAS_STD_FTE_CALC_CRS_STATE_CODE);
        // get input parameters.
        m_reportingCycle = (String) getParameter(REPORT_CYCLE_PARAM);
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_updateStudent = ((Boolean) getParameter(UPDATE_STUDENT_PARAM)).booleanValue();
        m_addSixthRecord = (Boolean) getParameter(ADD_SIXTH_RECORD_PARAM);
        if (m_addSixthRecord == null) {
            m_addSixthRecord = Boolean.FALSE;
        }
        m_clearOverride = (Boolean) getParameter(CLEAR_PROGRAM_OVERRIDE_PARAM);
        if (m_clearOverride == null) {
            m_clearOverride = Boolean.FALSE;
        }
        m_simplePreserve = (Boolean) getParameter(SIMPLE_PRESERVE_PARAM);
        if (m_simplePreserve == null) {
            m_simplePreserve = Boolean.FALSE;
        }
        m_calcSped = (Boolean) getParameter(CALCULATE_SPED_PARAM);
        if (m_calcSped == null) {
            m_calcSped = Boolean.FALSE;
        }

        // prepare report data.
        initializeReport();
        addParameter(PARAMETER_ORGANIZATION, getOrganization());
        // Build and execute the query for student schedules.
        ReportDataGrid grid = new ReportDataGrid();
        buildScheduleMaps();
        if (m_calcSped.booleanValue()) {
            buildIepMap();
        }
        Iterator<String> iterator = m_studentSchsMap.keySet().iterator();
        Map<String, Object> schedules = null;
        IepData iep = null;
        while (iterator.hasNext()) {
            String studentOid = iterator.next();
            SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
            schedules = m_studentSchsMap.get(studentOid);
            if (m_iepMap != null) {
                iep = m_iepMap.get(studentOid);
            }
            if (m_updateStudent) {
                updateStudent(student, schedules, iep);
            }
            outputStudent(student, schedules, grid);
        }
        addParameter("calendarErrorSet", m_calendarDayNumsNotFound.toString());

        if (!m_calendarDayNumsNotFound.isEmpty() && grid.isEmpty()) {
            grid.append();
        }
        m_errorGrid.beforeTop();
        grid.set(FIELD_REPORT_ERROR_FORMAT, new ByteArrayInputStream(m_subReportError.getCompiledFormat()));
        grid.set(FIELD_REPORT_ERROR_GRID, m_errorGrid);


        grid.beforeTop();


        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Build a map of student ieps
     *
     * It will retrieve a student's active IEP, otherwise most recent exited IEP.
     */
    private void buildIepMap() {
        // initialize map with PREVIOUS ieps
        X2Criteria isExitedCriteria = new X2Criteria();
        isExitedCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()));
        BeanQuery isExitedQuery = new BeanQuery(IepData.class, isExitedCriteria);
        isExitedQuery.addOrderByAscending(IepData.COL_START_DATE);
        isExitedQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        m_iepMap = getBroker().getMapByQuery(isExitedQuery, IepData.COL_STUDENT_OID, 1024);
        // then, put (or override) with ACTIVE ieps
        // this is so a key-value entry is the student's active iep, otherwise most recent exited
        X2Criteria isActiveCriteria = new X2Criteria();
        isActiveCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        BeanQuery isActiveQuery = new BeanQuery(IepData.class, isActiveCriteria);
        Collection<IepData> ieps = getBroker().getCollectionByQuery(isActiveQuery);
        for (IepData iep : ieps) {
            m_iepMap.put(iep.getStudentOid(), iep);
        }
    }

    /**
     * Build a map of student schedules in effect on report date.
     * Include user specified selection criteria and sorting.
     */
    private void buildScheduleMaps() {
        // Identify current active student schedules.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_MASTER_TYPE, TYPE_CLASS);
        criteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_courseExcludeField, BooleanAsStringConverter.TRUE);
        if (!StringUtils.isEmpty(m_sscExclude)) {
            criteria.addNotEqualTo(m_sscExclude, BooleanAsStringConverter.TRUE);
        }

        // If student criteria was specified, include the student criteria.
        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentSection.COL_STUDENT_OID, m_reportHelper.getStudentOid());
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }
            int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    criteria.addEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                            getParameter(QUERY_STRING_PARAM));
                    break;
                case 2: // LASID
                    criteria.addEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                            getParameter(QUERY_STRING_PARAM));
                    break;
                case 3: // SASID
                    criteria.addEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                            getParameter(QUERY_STRING_PARAM));
                    break;
                case 4: // Record set
                    criteria.addIn(StudentSection.COL_STUDENT_OID, getStudentOidSubQuery());
                    break;
                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }
        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);
        /*
         * Build the sort based on user input.
         */
        int sort = ((Integer) getParameter(STUDENT_SORT_PARAM)).intValue();
        List<String> sortArrayList = new ArrayList<String>();
        switch (sort) {
            case 0: // Name view
                sortArrayList.add(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                sortArrayList.add(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                sortArrayList.add(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 2: // School
                sortArrayList.add(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                        + SisSchool.COL_NAME);
                sortArrayList.add(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            default: // No sort specified
                break;
        }

        for (String sortValue : sortArrayList) {
            query.addOrderByAscending(sortValue);
        }

        /*
         * The student sort order always has to end with the student OID so that schedules are
         * grouped properly.
         */
        query.addOrderByAscending(StudentSection.COL_STUDENT_OID);
        // Build a map of lists of student schedules.
        Map<String, List<Object>> rawStudentSchedulesMap = new HashMap<String, List<Object>>();
        List<StudentSchedule> studentSchedules = new ArrayList<>(getBroker().getCollectionByQuery(query));
        List<Object> schedulesToOperate = null;
        SisStudent lastStudent = null;
        Map<String, SisStudent> tempStdMap = new HashMap<>();
        for (StudentSchedule currentSchedule : studentSchedules) {
            SisStudent student = currentSchedule.getStudent();
            if (!student.equals(lastStudent)) {
                schedulesToOperate = new ArrayList<Object>();
                lastStudent = student;
                rawStudentSchedulesMap.put(lastStudent.getOid(), schedulesToOperate);
                if (!tempStdMap.containsKey(student.getOid())) {
                    tempStdMap.put(student.getOid(), student);
                }
            }
            schedulesToOperate.add(currentSchedule);
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
        criteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_MASTER_TYPE, TYPE_CLASS);
        criteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_courseExcludeField, BooleanAsStringConverter.TRUE);
        criteria.addGreaterOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, m_reportDate);
        criteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);
        if (!StringUtils.isEmpty(m_sccExclude)) {
            criteria.addNotEqualTo(m_sccExclude, BooleanAsStringConverter.TRUE);
        }
        // If student criteria was specified, include the student criteria.
        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentScheduleChange.COL_STUDENT_OID, m_reportHelper.getStudentOid());
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }

            int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 2: // LASID
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 3: // SASID
                    criteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 4: // Record set
                    criteria.addIn(StudentScheduleChange.COL_STUDENT_OID, getStudentOidSubQuery());
                    break;

                default:
                    // No additional criteria (this is the case for "All")
                    break;
            }
        }
        // Order by student and descending change date.
        query.addOrderByAscending(StudentScheduleChange.COL_STUDENT_OID);
        query.addOrderByDescending(StudentScheduleChange.COL_EFFECTIVE_DATE);
        query = new QueryByCriteria(StudentScheduleChange.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentScheduleChange scheduleChange = (StudentScheduleChange) iterator.next();
                SisStudent student = scheduleChange.getStudent();
                schedulesToOperate = rawStudentSchedulesMap.get(student.getOid());
                if (schedulesToOperate == null) {
                    schedulesToOperate = new ArrayList<Object>();
                    rawStudentSchedulesMap.put(student.getOid(), schedulesToOperate);
                    if (!tempStdMap.containsKey(student.getOid())) {
                        tempStdMap.put(student.getOid(), student);
                    }
                }
                if (StudentScheduleChange.CODE_ADD.equals(scheduleChange.getChangeTypeCode())) {
                    // Search for a matching section Oid. Remove it.
                    Iterator<Object> schedIterator = schedulesToOperate.iterator();
                    while (schedIterator.hasNext()) {
                        Object iter = schedIterator.next();
                        MasterSchedule mstToOperate = getProperSection(iter);
                        if (mstToOperate.getOid().equals(scheduleChange.getMasterScheduleOid())) {
                            schedIterator.remove();
                            break;
                        }
                    }
                } else if (StudentScheduleChange.CODE_DROP.equals(scheduleChange.getChangeTypeCode())) {
                    // Search for a matching section Oid to verify that it is not already there.
                    boolean exists = false;
                    Iterator<Object> schedIterator = schedulesToOperate.iterator();
                    while (schedIterator.hasNext()) {
                        Object iter = schedIterator.next();
                        MasterSchedule mstToOperate = getProperSection(iter);
                        if (mstToOperate.getOid().equals(scheduleChange.getMasterScheduleOid())) {
                            exists = true;
                            break;
                        }
                    }
                    // Add a the Master Schedule.
                    if (!exists) {
                        schedulesToOperate.add(scheduleChange);
                    }
                }
            }
        } finally {
            iterator.close();
        }
        /*
         * With a list of each students master sections, for each student look up the weights of all
         * classes.
         */
        m_studentSchsMap = new HashMap<String, Map<String, Object>>();
        Iterator<String> stdIterator = rawStudentSchedulesMap.keySet().iterator();
        while (stdIterator.hasNext()) {
            String studentOid = stdIterator.next();
            SisStudent student = tempStdMap.get(studentOid);
            Map<String, Object> weightedSchedule = new TreeMap<String, Object>(new KeyComparator());
            m_studentSchsMap.put(studentOid, weightedSchedule);
            boolean blockScheduled = false;
            int fteCountDay = 0;
            int fteAltDay = 1;
            schedulesToOperate = rawStudentSchedulesMap.get(studentOid);
            Iterator<Object> sectIterator = schedulesToOperate.iterator();
            while (sectIterator.hasNext()) {
                Object schIterator = sectIterator.next();
                MasterSchedule section = getProperSection(schIterator);
                String calendarKey =
                        section.getSchedule().getSchoolOid() + MAP_KEY_SEPARATOR + student.getCalendarCode();
                String[] schoolDayOid = getCalendarDayNums(calendarKey);
                if (schoolDayOid != null && schoolDayOid.length > 1) {
                    blockScheduled = true;
                    if (CONSTANT_SPRING.equals(m_reportingCycle)) {
                        fteCountDay = 1;
                        fteAltDay = 0;
                    }
                }
                // Get the weight of the class program.
                String[] weights = getScheduleAndWeight(section, student, schIterator, fteCountDay);
                for (int i = 0; i < weights.length; i++) {
                    String weight = weights[i];
                    String mapKey = weight + MAP_KEY_SEPARATOR + section.getOid() + MAP_KEY_SEPARATOR + CONSTANT_A
                            + Integer.toString(i);
                    weightedSchedule.put(mapKey, schIterator);
                }
            }
            // If the student has block scheduled classes, look on the alternate day for two more
            // classes.
            if (blockScheduled) {
                // Do alt scheduling.
                Map<String, Object> altWeightedSchedule =
                        new TreeMap<String, Object>(new KeyComparator());
                sectIterator = schedulesToOperate.iterator();
                for (Object schInstance : schedulesToOperate) {
                    MasterSchedule section = getProperSection(schInstance);
                    String calendarKey =
                            section.getSchedule().getSchoolOid() + MAP_KEY_SEPARATOR + student.getCalendarCode();
                    String[] schoolDayOid = getCalendarDayNums(calendarKey);
                    if (schoolDayOid != null && schoolDayOid.length > 1) {
                        // Get the weight of the class program.
                        String[] weights = getScheduleAndWeight(section, student, schInstance, fteAltDay);
                        for (int i = 0; i < weights.length; i++) {
                            String weight = weights[i];
                            String mapKey = weight + MAP_KEY_SEPARATOR + section.getOid() + MAP_KEY_SEPARATOR
                                    + CONSTANT_B + Integer.toString(i);
                            altWeightedSchedule.put(mapKey, schInstance);
                        }
                    }
                }
                // Find the highest weighted section that is not already in the regular day list.
                boolean found = false;
                for (String key : altWeightedSchedule.keySet()) {
                    Object schInstance = altWeightedSchedule.get(key);
                    if (!key.startsWith(WEIGHT_ZERO)) {
                        weightedSchedule.put(key, schInstance);
                        found = true;
                        break;
                    }
                }
                if (!found && altWeightedSchedule.size() > 0) {
                    String key = altWeightedSchedule.keySet().iterator().next();
                    Object schInstance = altWeightedSchedule.get(key);
                    weightedSchedule.put(key, schInstance);
                }
                // find the lowest non-zero weight section that is not already in the regular day
                // list.
                found = false;
                String lastKey = null;
                for (String key : altWeightedSchedule.keySet()) {
                    if (!key.startsWith(WEIGHT_ZERO)) {
                        lastKey = key;
                    }
                }
                if (lastKey == null) {
                    // Did not find a section that was not already there, now just find the lowest
                    // non-zero weight section.
                    for (String key : altWeightedSchedule.keySet()) {
                        if (!key.startsWith(WEIGHT_ZERO)) {
                            lastKey = key;
                        }
                    }
                }
                if (lastKey != null) {
                    Object schInstance = altWeightedSchedule.get(lastKey);
                    weightedSchedule.put(lastKey, schInstance);
                }
            }

            if (m_addSixthRecord.booleanValue() && weightedSchedule.size() < 6 && !weightedSchedule.isEmpty()) {
                Map<String, Integer> countMap = new HashMap();
                Map<String, String> keyMap = new HashMap();
                for (String key : weightedSchedule.keySet()) {
                    String[] values = key.split(MAP_KEY_SEPARATOR);
                    String subKey = values[0];
                    Integer previous = countMap.get(subKey);
                    countMap.put(subKey,
                            previous == null ? Integer.valueOf(0) : Integer.valueOf(previous.intValue() + 1));
                    keyMap.put(subKey, key);
                }
                int frequency = 0;
                String matchKey = null;
                for (Entry<String, Integer> entry : countMap.entrySet()) {
                    if (entry.getValue().intValue() > frequency) {
                        frequency = entry.getValue().intValue();
                        matchKey = entry.getKey();
                    }
                }
                if (matchKey != null) {
                    String key = keyMap.get(matchKey);
                    Object value = weightedSchedule.get(key);
                    weightedSchedule.put(key.replace(MAP_KEY_SEPARATOR + CONSTANT_A, MAP_KEY_SEPARATOR + CONSTANT_B),
                            value);
                }
            }
        }
    }

    /**
     * Gets the calculated state crs code.
     *
     * @param obj Object
     * @return String
     */
    private String getCalculatedStateCrsCode(Object obj) {
        String value = null;
        if (obj instanceof StudentSchedule) {
            value = (String) ((StudentSchedule) obj).getFieldValueByBeanPath(m_sscCalcStateCode);
        } else {
            value = (String) ((StudentScheduleChange) obj).getFieldValueByBeanPath(m_sccCalcStateCode);
        }
        return value;
    }

    /**
     * Gets the calendar day nums.
     *
     * @param calendarKey String
     * @return String[]
     */
    private String[] getCalendarDayNums(String calendarKey) {
        String[] schoolDayOid = m_calendarDayNums.get(calendarKey);
        if (schoolDayOid == null) {
            m_calendarDayNumsNotFound.add(calendarKey);
        }
        return schoolDayOid;
    }

    /**
     * Return course by calculated state course code of the schedule.
     *
     * @param calcStateCode String
     * @return Course
     */
    private Course getCourseByCalcCode(String calcStateCode) {
        Course crs = null;
        // first try match on all 10 characters
        if (calcStateCode.length() > 10) {
            calcStateCode = calcStateCode.substring(0, 10);
        }
        crs = m_allCrsNumbers.get(calcStateCode);

        if (crs == null) {
            if (calcStateCode.length() > MAX_COURSE_NUMBER_LENGTH) {
                calcStateCode = calcStateCode.substring(0, MAX_COURSE_NUMBER_LENGTH);
            }
            crs = m_allCrsNumbers.get(calcStateCode);
        }
        return crs;
    }

    /**
     * Return the most recent enrollment record for the student of the specified types that
     * exists on or before the specified date.
     *
     * @param enrollments List<StudentEnrollment>
     * @param date PlainDate
     * @param types String
     * @return Student enrollment
     */
    private StudentEnrollment getEnrollmentForDate(List<StudentEnrollment> enrollments, PlainDate date, String types) {
        StudentEnrollment lastEnrollment = null;
        if (enrollments != null) {
            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentDate() != null && !enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                        break;
                    }
                }
            }
        }
        return lastEnrollment;
    }

    /**
     * Gets the student enrollments.
     *
     * @param student SisStudent
     * @return Array list
     */
    private LinkedList<StudentEnrollment> getStudentEnrollments(SisStudent student) {
        LinkedList<StudentEnrollment> enrollments = m_stdtEnrMap.get(student.getOid());
        if (enrollments == null) {
            enrollments = new LinkedList<>(student.getEnrollments().stream()
                    .filter(enr -> enr.getSchoolOid().equals(student.getSchoolOid()))
                    .sorted(Comparator.comparing(StudentEnrollment::getEnrollmentDate)
                            .thenComparing(StudentEnrollment::getTimestamp)
                            .reversed())
                    .collect(Collectors.toList()));
            m_stdtEnrMap.put(student.getOid(), enrollments);
        }
        return enrollments;
    }

    /**
     * This method takes a List and returns the value of the index passed. If the index
     * passed does not exist in the List, a null value will be returned.
     *
     * @param enteredValues List<Object>
     * @param key String
     * @return String
     */
    private String getEnteredValues(Map<String, Object> enteredValues, String key) {
        String value = null;
        if (enteredValues != null && enteredValues.containsKey(key)) {
            value = (String) enteredValues.get(key);
        }
        return value;
    }

    /**
     * Get the IEP dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getIepDictionary() {
        if (m_iepDictionary == null) {
            String ddxOid = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.SPED_DEFAULT_IEP_DEFINITION);
            ExtendedDataDictionary edd =
                    (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, ddxOid);
            m_iepDictionary = DataDictionary.getDistrictDictionary(edd, getBroker().getPersistenceKey());
        }
        return m_iepDictionary;
    }

    /**
     * Gets the proper section according to the SSC or SCC.
     *
     * @param obj Object
     * @return Master schedule
     */
    private MasterSchedule getProperSection(Object obj) {
        MasterSchedule mstToOperate = null;
        if (obj instanceof StudentSchedule) {
            mstToOperate = ((StudentSchedule) obj).getSection();
        } else {
            mstToOperate = ((StudentScheduleChange) obj).getMasterSchedule();
        }
        return mstToOperate;
    }

    /**
     * This method determines two things:
     * 1. How many times the section meets on the report date according to the
     * school calendar and schedule matrix. If a section meets on two or
     * more periods in the day, return that many values in the array.
     * 2. Return the day and weight of each instance as a string that can be
     * sorted for priority:
     * A. meet one period on report date: -> [1.000]
     * B. meet two periods on report date: -> [1.000, 1.000]
     * C. meet once on report date and once not: -> [1.000]
     *
     * @param section MasterSchedule
     * @param student SisStudent
     * @param instanceOfStdSchedule Object
     * @param dayIdx int
     * @return weight[]
     */
    private String[] getScheduleAndWeight(MasterSchedule section,
                                          SisStudent student,
                                          Object instanceOfStdSchedule,
                                          int dayIdx) {
        String[] weights = new String[0];
        /*
         * Find out how many times it meets on report day.
         */
        String calendarKey = section.getSchedule().getSchoolOid() + MAP_KEY_SEPARATOR + student.getCalendarCode();
        String[] schoolDayOid = getCalendarDayNums(calendarKey);
        if (schoolDayOid != null && dayIdx < schoolDayOid.length) {
            int count = getScheduleMatrixCount(section, schoolDayOid[dayIdx]);
            // Find the weight of each instance of the section.
            // 1. Course has program code.
            // 2. If course has overrideProgram flag, use student primary disability instead.
            // 2a. If student has Area Served, use that to override primary disability.
            String schStateCalcCode = null;
            MasterSchedule mst = null;
            if (instanceOfStdSchedule instanceof StudentSchedule) {
                schStateCalcCode =
                        (String) ((StudentSchedule) instanceOfStdSchedule).getFieldValueByBeanPath(m_sscCalcStateCode);
                mst = ((StudentSchedule) instanceOfStdSchedule).getSection();
            } else {
                schStateCalcCode = (String) ((StudentScheduleChange) instanceOfStdSchedule)
                        .getFieldValueByBeanPath(m_sccCalcStateCode);
                mst = ((StudentScheduleChange) instanceOfStdSchedule).getMasterSchedule();
            }
            Course crsToOperate = null;
            if (!StringUtils.isEmpty(schStateCalcCode)) {
                crsToOperate = getCourseByCalcCode(schStateCalcCode);
            }
            if (crsToOperate == null && mst.getSchoolCourse() != null) {
                crsToOperate = mst.getSchoolCourse().getCourse();
            }
            if (crsToOperate != null) {
                String program = (String) crsToOperate.getFieldValueByBeanPath(m_courseProgramField);
                String programOverride = (String) crsToOperate.getFieldValueByBeanPath(m_programOverrideField);
                if (CONSTANT_ONE.equals(programOverride)) {
                    program = getStudentEquivelantProgramCode(student, crsToOperate);
                }
                if (!StringUtils.isEmpty(program) || CONSTANT_ONE.equals(programOverride)) {
                    ReferenceCode refCode = m_programCodeMap.get(program);
                    String baseWeight = WEIGHT_ZERO;
                    if (refCode != null && !StringUtils.isEmpty(refCode.getLocalCode())) {
                        baseWeight = refCode.getLocalCode();
                    }
                    weights = new String[count];
                    for (int i = 0; i < count; i++) {
                        weights[i] = baseWeight;
                    }
                }
            } else {
                if (m_errorGrid == null) {
                    m_errorGrid = new ReportDataGrid();
                }
                m_errorGrid.append();
                m_errorGrid.set(ERROR_FIELD_LOCAL_ID, student.getLocalId());
                m_errorGrid.set(ERROR_FIELD_NAME, student.getNameView());
                m_errorGrid.set(ERROR_FIELD_CALC_CODE,
                        schStateCalcCode + mst == null ? "" : "-[" + mst.getCourseView() + "]");
            }
        }
        return weights;
    }

    /**
     * Return the number of periods a section meets on a particular scheduled day. Results are
     * cached by schedule
     *
     * @param section MasterSchedule
     * @param schoolDayOid String
     * @return int
     */
    private int getScheduleMatrixCount(MasterSchedule section, String schoolDayOid) {
        String key = section.getSchedule().getSchoolOid() + schoolDayOid;
        Map<String, Collection<MasterScheduleMatrix>> matrixMap = m_masterSchedulesMap.get(key);
        if (matrixMap == null) {
            List<String> termOids = m_schoolTermOids.get(section.getSchedule().getSchoolOid());
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.COL_SCHEDULE_DAY_OID,
                    schoolDayOid);
            criteria.addIn(MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER +
                    MasterTerm.COL_SCHEDULE_TERM_OID, termOids);
            QueryByCriteria query = new QueryByCriteria(MasterScheduleMatrix.class, criteria);
            matrixMap = getBroker().getGroupedCollectionByQuery(query,
                    MasterScheduleMatrix.REL_MASTER_TERM + PATH_DELIMITER +
                            MasterTerm.COL_MASTER_SCHEDULE_OID,
                    1000);
            m_masterSchedulesMap.put(key, matrixMap);
        }
        int count = 0;
        Collection<MasterScheduleMatrix> matrices = matrixMap.get(section.getOid());
        if (matrices != null && !matrices.isEmpty()) {
            count = matrices.size();
        }
        return count;
    }

    /**
     * When the course program code must be overridden by the student primary disability.
     * These use different reference tables and codes, but have common state code values.
     * 1. Find the Area Served.
     * 2. If this is empty,
     * 2a. find the student disability code ReferenceCode and get the state code from it.
     * 2b. Search through program codes reference table, look for a program code with the same state
     * value.
     * 2c. Return the program code.
     * 2d. If no matching program code can be found, return the disability code.
     * It won't match validation but it will be informative enough to allow user correction.
     * 3. If still blank, return value from DOE Program Code from the course
     *
     * @param student SisStudent
     * @param course Course
     * @return String program code.
     */
    private String getStudentEquivelantProgramCode(SisStudent student, Course course) {
        String newProgram = (String) student.getFieldValueByBeanPath(m_studentAreaServedField);
        if (StringUtils.isEmpty(newProgram)) {
            newProgram = (String) student.getFieldValueByBeanPath(m_studentDisabilityField);
        }
        // if newProgram is still empty, use the value from "DOE Program Code"
        if (StringUtils.isEmpty(newProgram)) {
            newProgram = (String) course.getFieldValueByBeanPath(m_courseProgramField);
        }
        return newProgram;
    }

    /**
     * Returns the Subquery for retrieving student oids from a record set.
     *
     * @return SubQuery
     */
    private SubQuery getStudentOidSubQuery() {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                getParameter(QUERY_STRING_PARAM));

        SubQuery studentOidSubQuery = new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria);
        return studentOidSubQuery;
    }

    /**
     * Gets the value for inclusion delivery.
     *
     * @param schInstance Object
     * @param section MasterSchedule
     * @param schCalcValue String
     * @param matchDecimal String
     * @return String
     */
    private String getValueForInclusionDelivery(Object schInstance,
                                                MasterSchedule section,
                                                String schCalcValue,
                                                String matchDecimal) {
        String value = null;
        switch (matchDecimal) {
            case "9":
                if (!StringUtils.isEmpty(schCalcValue) && schCalcValue.matches(CRS_NUM_PATTERN)
                        && (matchDecimal.equals(String.valueOf(schCalcValue.charAt(3)))
                                || "0".equals(String.valueOf(schCalcValue.charAt(3)))
                                || "1".equals(String.valueOf(schCalcValue.charAt(3)))
                                || "2".equals(String.valueOf(schCalcValue.charAt(3)))
                                || "3".equals(String.valueOf(schCalcValue.charAt(3)))
                                || "4".equals(String.valueOf(schCalcValue.charAt(3))))) {
                    if (schInstance instanceof StudentSchedule) {
                        if (m_sscInclusionDeliveryDdf != null && m_sscInclusionDeliveryDdf.hasReferenceTable()) {
                            Map<String, ReferenceCode> codeMap =
                                    m_sscInclusionDeliveryDdf.getReferenceTable().getCodeMap();
                            String keyValue = (String) ((StudentSchedule) schInstance)
                                    .getFieldValueByBeanPath(m_sscInclusionDeliveryCode);
                            if (codeMap.containsKey(keyValue)
                                    && CONSTANT_INCLUSION.equalsIgnoreCase(codeMap.get(keyValue).getFederalCode())) {
                                value = keyValue;

                            }
                        }
                    } else {
                        if (m_sccInclusionDeliveryDdf != null && m_sccInclusionDeliveryDdf.hasReferenceTable()) {
                            Map<String, ReferenceCode> codeMap =
                                    m_sccInclusionDeliveryDdf.getReferenceTable().getCodeMap();
                            String keyValue = (String) ((StudentScheduleChange) schInstance)
                                    .getFieldValueByBeanPath(m_sccInclusionDeliveryCode);
                            if (codeMap.containsKey(keyValue)
                                    && CONSTANT_INCLUSION.equalsIgnoreCase(codeMap.get(keyValue).getFederalCode())) {
                                value = keyValue;
                            }
                        }
                    }
                    if (StringUtils.isEmpty(value)) {
                        if (m_mstInclusionDeliveryModelDdf != null
                                && m_mstInclusionDeliveryModelDdf.hasReferenceTable()) {
                            Map<String, ReferenceCode> codeMap =
                                    m_mstInclusionDeliveryModelDdf.getReferenceTable().getCodeMap();
                            String keyValue =
                                    (String) section.getFieldValueByBeanPath(m_mstInclusionDeliveryModelField);
                            if (codeMap.containsKey(keyValue)
                                    && CONSTANT_INCLUSION.equalsIgnoreCase(codeMap.get(keyValue).getFederalCode())) {
                                value = keyValue;
                            }
                        }
                    }
                }
                break;
            default:
                break;
        }
        return value;
    }

    /**
     * Prepare report data, alias lookups, datamaps.
     */
    private void initializeReport() {
        m_subReportError = ReportUtils.getReport(FORMAT_ID_ERROR, getBroker());
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(PROGRAM_OVERRIDE_ALIAS);
        if (field != null) {
            m_programOverrideField = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(PROGRAM_ALIAS);
        if (field != null) {
            m_courseProgramField = field.getJavaName();
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                m_programCodeMap = refTable.getCodeMap(getBroker());
            }
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CRS_EXCLUDE);
        if (field != null) {
            m_courseExcludeField = field.getJavaName();
        }

        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_MST_INCL_DELIVERY_MODEL);
        if (field != null) {
            m_mstInclusionDeliveryModelDdf = field;
            m_mstInclusionDeliveryModelField = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(STUDENT_AREA_SERVED);
        if (field != null) {
            m_studentAreaServedField = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(STUDENT_DISABILITY_ALIAS);
        if (field != null) {
            m_studentDisabilityField = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_CALC_CRS_STATE_CODE);
        if (field != null) {
            m_sccCalcStateCode = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_CALC_CRS_STATE_CODE);
        if (field != null) {
            m_sscCalcStateCode = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_INCL_DELIVERY_CODE);
        if (field != null) {
            m_sscInclusionDeliveryDdf = field;
            m_sscInclusionDeliveryCode = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_INCL_DELIVERY_CODE);
        if (field != null) {
            m_sccInclusionDeliveryDdf = field;
            m_sccInclusionDeliveryCode = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CRS_MATCH_TEN_CHARS);
        if (field != null && Course.class.equals(field.getDataTable().getDataClass())) {
            m_crsMatchTenCharsField = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SSC_EXCLUDE);
        if (field != null) {
            m_sscExclude = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCC_EXCLUDE);
        if (field != null) {
            m_sccExclude = field.getJavaName();
        }
        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_ENR_TYPE);
        if (field != null) {
            m_fieldStdEnrType = field.getJavaName();
            if (field.hasReferenceTable()) {
                X2Criteria refCodesCriteria = new X2Criteria();
                refCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                refCodesCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
                QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, refCodesCriteria);
                m_stdEnrTypeRcdsByStateCode =
                        getBroker().getMapByQuery(rcdQuery, ReferenceCode.COL_STATE_CODE, 128);
            }
        }
        field = dictionary.findDataDictionaryField(StudentEnrollment.class.getName(),
                StudentEnrollment.COL_ENROLLMENT_CODE);
        if (field != null) {
            m_fieldEnrCode = field.getJavaName();
            if (field.hasReferenceTable()) {
                m_enrCodesRcds = field.getReferenceTable().getCodeMap();
            }
        }
        /*
         * Prepare the student alias maps.
         */
        m_studentAliasMap = new Map[6];
        field = null;
        for (int i = 0; i < 6; i++) {
            m_studentAliasMap[i] = new HashMap<String, String>();
            String postFix = CONSTANT_SPACE + Integer.toString(i + 1).trim();

            for (String aliasRoot : m_aliasList) {
                field = dictionary.findDataDictionaryFieldByAlias(aliasRoot + postFix);
                if (field != null) {
                    m_studentAliasMap[i].put(aliasRoot, field.getJavaName());
                }
            }
        }
        loadAllCoursesForContext();
        // Get a map of school schedule day oid on report date.
        // Go through the school calendars to find the day number on report date.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCalendarDate.COL_DATE, m_reportDate);
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        ReportQueryByCriteria query = new ReportQueryByCriteria(SchoolCalendarDate.class,
                new String[] {SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                        SchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER},
                criteria);
        m_calendarDayNums = new HashMap<String, String[]>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] objects = (Object[]) iterator.next();
                String schoolOid = (String) objects[0];
                String calendarId = (String) objects[1];
                Number dayNum = (Number) objects[2];
                // Step 1. For the school active schedule, retrieve the ScheduleDayOID for the day
                // number on report date.
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
                ScheduleDay dayBean = (ScheduleDay) getBroker().getBeanByQuery(dayQuery);
                // Step 2. count the number of schedulable periods in the schedule structure for the
                // school. Used to determine block scheduling.
                criteria = new Criteria();
                criteria.addEqualTo(SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID,
                        schoolOid);
                criteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_INDICATOR, Boolean.TRUE);
                criteria.addEqualToField(SchedulePeriod.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        SchedulePeriod.COL_SCHEDULE_OID);
                dayQuery = new QueryByCriteria(SchedulePeriod.class, criteria);
                int periodCount = getBroker().getCount(dayQuery);
                // If there are fewer than 6 schedule periods,
                ScheduleDay dayBean2 = null;
                if (periodCount < 6) {
                    // Step 3. For the school-calendar, find the day number for the day prior to
                    // report date.
                    criteria = new Criteria();
                    boolean sortAscending = false;
                    // Find schedule day before report date.
                    criteria.addLessThan(SchoolCalendarDate.COL_DATE, m_reportDate);
                    criteria.addEqualTo(
                            SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                            schoolOid);
                    criteria.addEqualTo(
                            SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                            calendarId);
                    criteria.addGreaterThan(SchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, Integer.valueOf(0));
                    dayQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);
                    dayQuery.addOrderBy(SchoolCalendarDate.COL_DATE, sortAscending);
                    SchoolCalendarDate calDateBean = (SchoolCalendarDate) getBroker().getBeanByQuery(dayQuery);
                    if (calDateBean != null) {
                        Integer dayNum2 = Integer.valueOf(calDateBean.getScheduleDayNumber());
                        // Step 4. For the school active schedule, retrieve the ScheduleDayOID for
                        // the day prior to report date.
                        criteria = new Criteria();
                        criteria.addEqualTo(ScheduleDay.COL_NUMBER, dayNum2);
                        criteria.addEqualTo(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                                schoolOid);
                        criteria.addEqualToField(ScheduleDay.REL_SCHEDULE + PATH_DELIMITER +
                                Schedule.REL_SCHOOL + PATH_DELIMITER +
                                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                                ScheduleDay.COL_SCHEDULE_OID);
                        dayQuery = new QueryByCriteria(ScheduleDay.class, criteria);
                        dayBean2 = (ScheduleDay) getBroker().getBeanByQuery(dayQuery);
                    }
                }
                // Add the calendar dates to the calendar day num map. Two parts: [ sched day OID on
                // report date, sched day OID on alt date
                if (dayBean != null) {
                    String key = schoolOid + MAP_KEY_SEPARATOR + calendarId;
                    if (dayBean2 == null) {
                        m_calendarDayNums.put(key, new String[] {dayBean.getOid()});
                    } else {
                        m_calendarDayNums.put(key, new String[] {dayBean.getOid(), dayBean2.getOid()});
                    }
                }
            }
        } finally {
            iterator.close();
        }
        // Get a map of school schedule term oids on report date for all schools.
        Criteria termCriteria = new Criteria();
        termCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);
        termCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, m_reportDate);
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
        m_schoolTermOids = new HashMap<String, List<String>>();
        iterator = getBroker().getReportQueryIteratorByQuery(termQuery);
        try {
            while (iterator.hasNext()) {
                Object[] objects = (Object[]) iterator.next();
                String oid = (String) objects[0];
                String termOid = (String) objects[1];

                List<String> schoolTerms = m_schoolTermOids.get(oid);
                if (schoolTerms == null) {
                    schoolTerms = new ArrayList<String>();
                    m_schoolTermOids.put(oid, schoolTerms);
                }
                schoolTerms.add(termOid);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load all courses for context.
     */
    private void loadAllCoursesForContext() {
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addEqualTo(Course.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        courseCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        courseCriteria.addNotEmpty(Course.COL_NUMBER, getBroker().getPersistenceKey());
        courseCriteria.addNotEmpty(m_courseProgramField, getBroker().getPersistenceKey());
        courseCriteria.addNotEqualTo(m_courseExcludeField, BooleanAsStringConverter.TRUE);
        QueryByCriteria crsQuery = new QueryByCriteria(Course.class, courseCriteria);

        try (QueryIterator iterator = getBroker().getIteratorByQuery(crsQuery)) {
            while (iterator.hasNext()) {
                Course course = (Course) iterator.next();
                String key = course.getNumber();
                if (!StringUtils.isEmpty(m_crsMatchTenCharsField) &&
                        BooleanAsStringConverter.TRUE.equals(course.getFieldValueByBeanPath(m_crsMatchTenCharsField))) {
                    if (course.getNumber().length() > 10) {
                        key = key.substring(0, 10);
                    }
                } else if (course.getNumber().length() > MAX_COURSE_NUMBER_LENGTH) {
                    key = key.substring(0, MAX_COURSE_NUMBER_LENGTH);
                }
                m_allCrsNumbers.put(key, course);
            }
        }
    }

    /**
     * For a given student and schedule records, collect reportable data and put it into the report
     * grid.
     *
     * @param student SisStudent
     * @param schs Map<String,Object>
     * @param grid ReportDataGrid
     */
    private void outputStudent(SisStudent student, Map<String, Object> schs, ReportDataGrid grid) {
        /*
         * The natural order of the schedules Map is by increasing weight.
         * We need to order by decreasing weight.
         */
        int count = 0;
        for (String weightKey : schs.keySet()) {
            count++;
            String calcStateCrsCode = getCalculatedStateCrsCode(schs.get(weightKey));
            MasterSchedule section = getProperSection(schs.get(weightKey));
            Course crsToOperate = null;
            if (!StringUtils.isEmpty(calcStateCrsCode)) {
                crsToOperate = getCourseByCalcCode(calcStateCrsCode);
            }
            if (crsToOperate == null && section.getSchoolCourse() != null) {
                crsToOperate = section.getSchoolCourse().getCourse();
            }

            if (crsToOperate != null) {
                SisSchool school = section.getSchedule().getSchool();
                // Get program code and override code if required.
                String program = (String) crsToOperate.getFieldValueByBeanPath(m_courseProgramField);
                String programOverride = (String) crsToOperate.getFieldValueByBeanPath(m_programOverrideField);
                if (BooleanAsStringConverter.TRUE.equals(programOverride)) {
                    program = getStudentEquivelantProgramCode(student, crsToOperate);
                }
                String weight = weightKey.substring(0, weightKey.indexOf(MAP_KEY_SEPARATOR));
                String gaa = (String) student.getFieldValueByAlias(STD_GAA_FLAG_ALIAS);
                String primaryDisability = (String) student.getFieldValueByAlias(STUDENT_DISABILITY_ALIAS);
                grid.append();
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_SCHOOL, school);
                grid.set(FIELD_SECTION, section.getCourseView());
                grid.set(FIELD_DESCRIPTION, section.getDescription());
                grid.set(FIELD_SCHEDULE, section.getScheduleDisplay());
                grid.set(FIELD_PROGRAM, program);
                grid.set(FIELD_WEIGHT, weight);
                grid.set(FIELD_INCLUDED, (count <= 6 ? Boolean.TRUE : Boolean.FALSE));
                grid.set(FIELD_GAA_PARTICIPANT, gaa);
                grid.set(FIELD_PRIMARY_EXCEPTIONALITY, primaryDisability);
                grid.set(FIELD_CALC_CRS_CODE, calcStateCrsCode);
            }
        }
    }

    /**
     * Update the student record with the program code, other flags and defaults
     * related to the FTE reporting requirements.
     *
     * @param student SisStudent
     * @param schs Map<String,Object>
     * @param iep IepData
     */
    private void updateStudent(SisStudent student, Map<String, Object> schs, IepData iep) {
        StudentEnrollment stdEnr = getEnrollmentForDate(getStudentEnrollments(student), m_reportDate,
                StudentEnrollment.ENTRY + StudentEnrollment.YOG_CHANGE + StudentEnrollment.STATUS_CHANGE);
        if (stdEnr != null) {
            if (m_fieldEnrCode != null) {
                String enrCode = (String) stdEnr.getFieldValueByBeanPath(m_fieldEnrCode);
                if (StringUtils.isEmpty(enrCode)) {
                    stdEnr = getEnrollmentForDate(getStudentEnrollments(student), m_reportDate,
                            StudentEnrollment.ENTRY);
                    enrCode = (String) stdEnr.getFieldValueByBeanPath(m_fieldEnrCode);
                }
                if (!StringUtils.isEmpty(enrCode) && m_enrCodesRcds.containsKey(enrCode)) {
                    String enrCodeState = m_enrCodesRcds.get(enrCode).getStateCode();
                    if (!StringUtils.isEmpty(enrCodeState)
                            && m_stdEnrTypeRcdsByStateCode.containsKey(enrCodeState) && m_fieldStdEnrType != null) {
                        String codeToSet = m_stdEnrTypeRcdsByStateCode.get(enrCodeState).getCode();
                        student.setFieldValueByBeanPath(m_fieldStdEnrType, codeToSet);
                    }
                }
            }
        }

        // If the student has an IEP...
        if (iep != null) {
            /*
             * Set the student's 'DOE GAA Flag' field based on that student's IEP
             * 'iep-gaa-participant' field
             *
             * If IEP > 'iep-gaa-participant' equals "Yes", STUDENT > DOE GAA Flag is "Yes",
             * otherwise...
             * STUDENT > DOE GAA Flag is "No"
             */
            String gaaFlagAsString = (String) iep.getFieldValueByAlias(IEP_GAA_FLAG_ALIAS, getIepDictionary());
            if (!StringUtils.isEmpty(gaaFlagAsString) && gaaFlagAsString.matches("Y(es)?")) {
                student.setFieldValueByAlias(STD_GAA_FLAG_ALIAS, CONSTANT_YES);
            } else {
                student.setFieldValueByAlias(STD_GAA_FLAG_ALIAS, CONSTANT_NO);
            }
            /*
             * Set the student's 'DOE PRIMARY EXCEPTIONALITY' field based on the IEP's primary
             * IepDisability's disability code
             */
            List<IepDisability> disabilities = new ArrayList<IepDisability>(iep.getIepDisability());
            IepDisability primaryDisability = null;
            if (disabilities != null && !disabilities.isEmpty()) {
                primaryDisability = disabilities.get(0);
                for (IepDisability disability : disabilities) {
                    if (disability.getPrimaryIndicator()) {
                        primaryDisability = disability;
                    }
                }
                if (primaryDisability != null) {
                    String primaryDisabilityCode = primaryDisability.getDisabilityCode();
                    student.setFieldValueByBeanPath(m_studentDisabilityField, primaryDisabilityCode);
                }
            }
        }
        int count = 0;
        List<KeyValuePair> enteredPrograms = new ArrayList<KeyValuePair>();
        for (count = 0; count < 6; count++) {
            if (!m_clearOverride.booleanValue()) {
                // Collect existing entered codes so they can be re-applied in case sections moved.
                String enteredCourse =
                        (String) student.getFieldValueByBeanPath(m_studentAliasMap[count].get(SECTION_ALIAS));
                if (!StringUtils.isEmpty(enteredCourse)) {
                    Map<String, Object> enteredValues = new HashMap();
                    // Object[] enteredValues = new Object[9];
                    enteredValues.put(PROGRAM_ALIAS,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ALIAS)));
                    enteredValues.put(LOCATION_ALIAS,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(LOCATION_ALIAS)));
                    enteredValues.put(TRANSPORT_ALIAS,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(TRANSPORT_ALIAS)));
                    enteredValues.put(SPEECH_ALIAS,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(SPEECH_ALIAS)));
                    enteredValues.put(ITINERANT_ALIAS,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(ITINERANT_ALIAS)));
                    enteredValues.put(ALIAS_STD_DOE_INCLUSION,
                            student.getFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_DOE_INCLUSION)));
                    enteredValues.put(ALIAS_STD_FTE_CALC_CRS_STATE_CODE, student
                            .getFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_FTE_CALC_CRS_STATE_CODE)));
                    KeyValuePair pair = new KeyValuePair(enteredCourse, enteredValues);
                    enteredPrograms.add(pair);
                }
            }
            student.setFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ALIAS), EMPTY_STRING);
            student.setFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ORIG_ALIAS), EMPTY_STRING);
            if (!m_simplePreserve.booleanValue()) {
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(LOCATION_ALIAS), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(TRANSPORT_ALIAS), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(SPEECH_ALIAS), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ITINERANT_ALIAS), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_DOE_INCLUSION), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(SECTION_ALIAS), EMPTY_STRING);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_FTE_CALC_CRS_STATE_CODE),
                        EMPTY_STRING);
            }
        }
        count = 0;
        for (String weightKey : schs.keySet()) {
            String calcStateCrsValue = getCalculatedStateCrsCode(schs.get(weightKey));
            MasterSchedule section = getProperSection(schs.get(weightKey));
            Course course = null;
            if (!StringUtils.isEmpty(calcStateCrsValue)) {
                course = getCourseByCalcCode(calcStateCrsValue);
            }
            if (course == null && section.getSchoolCourse() != null) {
                course = section.getSchoolCourse().getCourse();
            }
            if (course != null) {
                String inclusion = getValueForInclusionDelivery(schs.get(weightKey), section, calcStateCrsValue, "9");
                String program = (String) course.getFieldValueByBeanPath(m_courseProgramField);
                String programOverride = (String) course.getFieldValueByBeanPath(m_programOverrideField);
                if (CONSTANT_ONE.equals(programOverride)) {
                    program = getStudentEquivelantProgramCode(student, course);
                }
                String sectionView =
                        section.getCourseView() + CONSTANT_SPACE + CONSTANT_SPACE + section.getDescription();
                if (sectionView.length() > 50) {
                    sectionView = sectionView.substring(0, 50);
                }
                // Find a KeyValuePair for this sectionDescription.
                // Remove one instance in case there are multiple entries for a section.
                Map<String, Object> enteredValues = new HashMap();
                for (KeyValuePair pair : enteredPrograms) {
                    if (pair.getKey().equals(sectionView)) {
                        enteredValues = (Map<String, Object>) pair.getValue();
                        enteredPrograms.remove(pair);
                        break;
                    }
                }
                // Assign all fields.
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(SECTION_ALIAS), sectionView);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ORIG_ALIAS), program);
                student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_FTE_CALC_CRS_STATE_CODE),
                        calcStateCrsValue);
                if (enteredValues != null && !m_clearOverride.booleanValue()) {
                    student.setFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ALIAS),
                            getEnteredValues(enteredValues, PROGRAM_ALIAS));
                    if (!m_simplePreserve.booleanValue()) {
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(LOCATION_ALIAS),
                                getEnteredValues(enteredValues, LOCATION_ALIAS));
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(TRANSPORT_ALIAS),
                                getEnteredValues(enteredValues, TRANSPORT_ALIAS));
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(SPEECH_ALIAS),
                                getEnteredValues(enteredValues, SPEECH_ALIAS));
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ITINERANT_ALIAS),
                                getEnteredValues(enteredValues, ITINERANT_ALIAS));
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_DOE_INCLUSION),
                                getEnteredValues(enteredValues, ALIAS_STD_DOE_INCLUSION));
                    }
                } else {
                    student.setFieldValueByBeanPath(m_studentAliasMap[count].get(PROGRAM_ALIAS), EMPTY_STRING);
                    if (!m_simplePreserve.booleanValue()) {
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(LOCATION_ALIAS), EMPTY_STRING);
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(TRANSPORT_ALIAS), CONSTANT_ZERO);
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(SPEECH_ALIAS), CONSTANT_ZERO);
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ITINERANT_ALIAS), CONSTANT_ZERO);
                        student.setFieldValueByBeanPath(m_studentAliasMap[count].get(ALIAS_STD_DOE_INCLUSION),
                                inclusion);
                    }
                }
                count++;
                if (count >= 6) {
                    break;
                }
            }
        }
        getBroker().saveBeanForced(student);
    }
}
