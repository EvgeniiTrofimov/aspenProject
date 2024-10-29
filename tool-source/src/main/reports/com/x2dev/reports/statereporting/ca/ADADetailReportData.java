/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ca.ADADataHelper;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailItem;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailIterator;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADA_ENROLLMENT_TYPE;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "CA Average Daily Attendance (ADA) Detail" report .
 *
 * @author X2 Development Corporation
 */
public class ADADetailReportData extends ReportJavaSourceNet {

    /**
     * Helper class to save all necessary information for the given student.
     *
     * @author Follett Software Company
     */
    class DetailItem {

        private List<StudentEnrollmentSpan> m_stdSpans;
        private SisStudent m_student;
        private String[] m_codes = new String[20];
        private int m_numAbsent;
        private int m_numApportionment;
        private int m_numExcused;
        private int m_numUnExcused;
        private int m_numScheduled;
        private int m_numSuspended;
        private int m_numTruant;
        private int m_numNotEnrolled;
        private int m_numONA;

        private int m_numBegin;
        private int m_numGain;
        private int m_numNewGain;
        private int m_numLoss;
        private int m_numEnd;
        private int m_numFirst;
        private int m_numLast;

        private boolean m_isEnrolled;
        private boolean m_isEverEnrolled;

        private boolean m_multipleExits;
        private boolean m_multipleReenter;

        /**
         * Constants
         */
        private static final String NOT_ENROLLED_CODE = "NE";
        private static final String SCHOOL_CLOSED_CODE = "KC";

        /**
         * Constructor.
         *
         * @param student SisStudent
         * @param stdSpans List<StudentEnrollmentSpan>
         */
        public DetailItem(SisStudent student, List<StudentEnrollmentSpan> stdSpans) {
            this.m_student = student;
            this.m_stdSpans = stdSpans;
        }

        /**
         * Gets the codes.
         *
         * @return the m_codes
         */
        public String[] getCodes() {
            return m_codes;
        }

        /**
         * Gets the num absent.
         *
         * @return the m_numAbsent
         */
        public int getNumAbsent() {
            return m_numAbsent;
        }

        /**
         * Gets the num apportionment.
         *
         * @return the m_numApportionment
         */
        public int getNumApportionment() {
            return m_numApportionment;
        }

        /**
         * Gets the num begin.
         *
         * @return the m_numBegin
         */
        public int getNumBegin() {
            return m_numBegin;
        }

        /**
         * Gets the num end.
         *
         * @return the m_numEnd
         */
        public int getNumEnd() {
            return m_numEnd;
        }

        /**
         * Gets the num excused.
         *
         * @return the m_numExcused
         */
        public int getNumExcused() {
            return m_numExcused;
        }

        /**
         * Gets the num first.
         *
         * @return the m_numFirst
         */
        public int getNumFirst() {
            return m_numFirst;
        }

        /**
         * Gets the num gain.
         *
         * @return the m_numGain
         */
        public int getNumGain() {
            return m_numGain;
        }

        /**
         * Gets the num last.
         *
         * @return the m_numLast
         */
        public int getNumLast() {
            return m_numLast;
        }

        /**
         * Gets the num loss.
         *
         * @return the m_numLoss
         */
        public int getNumLoss() {
            return m_numLoss;
        }

        /**
         * Gets the num new gain.
         *
         * @return the m_numGain
         */
        public int getNumNewGain() {
            return m_numNewGain;
        }

        /**
         * Gets the num not enrolled.
         *
         * @return the m_numNotEnrolled
         */
        public int getNumNotEnrolled() {
            return m_numNotEnrolled;
        }

        /**
         * Gets the num ONA.
         *
         * @return the m_numONA
         */
        public int getNumONA() {
            return m_numONA;
        }

        /**
         * Gets the num scheduled.
         *
         * @return the m_numScheduled
         */
        public int getNumScheduled() {
            return m_numScheduled;
        }

        /**
         * Gets the num suspended.
         *
         * @return the m_numSuspended
         */
        public int getNumSuspended() {
            return m_numSuspended;
        }

        /**
         * Gets the num truant.
         *
         * @return the m_numTruant
         */
        public int getNumTruant() {
            return m_numTruant;
        }

        /**
         * Gets the num un excused.
         *
         * @return the m_numUnExcused
         */
        public int getNumUnExcused() {
            return m_numUnExcused;
        }

        /**
         * Gets the std spans.
         *
         * @return the m_stdSpans
         */
        public List<StudentEnrollmentSpan> getStdSpans() {
            return m_stdSpans;
        }

        /**
         * Gets the student.
         *
         * @return the m_student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Increment.
         *
         * @param item ADADetailItem
         * @param dayIndex int
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        public void increment(ADADetailItem item, int dayIndex, PlainDate startDate, PlainDate endDate) {
            if (!item.getAttendanceCode().equals(SCHOOL_CLOSED_CODE)
                    || m_dataHelper.isCycleDay(item.getAttendanceDate())) {
                if (item.getAttendanceDate().equals(startDate)) {
                    if (!NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                        ++m_numBegin;
                        m_isEnrolled = true;
                        m_isEverEnrolled = true;
                    } else {
                        m_isEnrolled = false;
                    }
                } else if (item.getAttendanceDate().equals(endDate)) {
                    if (NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                        if (m_isEnrolled) {
                            ++m_numLoss;
                            if (m_numLoss > 1) {
                                m_multipleExits = true;
                            }
                            m_isEnrolled = false;
                        }
                    } else {
                        ++m_numEnd;
                        if (!m_isEnrolled) {
                            ++m_numGain;
                            if (m_numGain > 1) {
                                m_multipleReenter = true;
                            }
                            if (!m_isEverEnrolled) {
                                ++m_numNewGain;
                            }
                            m_isEnrolled = true;
                            m_isEverEnrolled = true;
                        }
                    }
                } else {
                    if (NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                        if (m_isEnrolled) {
                            ++m_numLoss;
                            if (m_numLoss > 1) {
                                m_multipleExits = true;
                            }
                            m_isEnrolled = false;
                        }
                    } else {
                        if (!m_isEnrolled) {
                            ++m_numGain;
                            if (m_numGain > 1) {
                                m_multipleReenter = true;
                            }
                            if (!m_isEverEnrolled) {
                                ++m_numNewGain;
                            }
                            m_isEnrolled = true;
                            m_isEverEnrolled = true;
                        }
                    }
                }
            }
            if (!m_dataHelper.isCycleDay(item.getAttendanceDate())) {
                m_codes[dayIndex] = SCHOOL_CLOSED_CODE;
                return;
            }
            m_codes[dayIndex] = item.getAttendanceCode();
            m_numNotEnrolled += NOT_ENROLLED_CODE.equals(item.getAttendanceCode()) ? 1 : 0;
            m_numApportionment += item.isApportionment() ? 1 : 0;
            m_numAbsent += item.isAbsent() && !"NE".equals(item.getAttendanceCode()) ? 1 : 0;
            m_numExcused += item.isAbsent() && item.isExcused() && !"NE".equals(item.getAttendanceCode()) ? 1 : 0;
            m_numUnExcused += item.isAbsent() && !item.isExcused() && !"NE".equals(item.getAttendanceCode()) ? 1 : 0;
            m_numScheduled += item.isScheduled() ? 1 : 0;
            m_numSuspended += item.isSuspended() && !"NE".equals(item.getAttendanceCode()) ? 1 : 0;
            m_numTruant += item.isTruant() ? 1 : 0;
            m_numONA += item.isScheduled() && !item.isApportionment() && !item.isAbsent()
                    && !"NE".equals(item.getAttendanceCode()) ? 1 : 0;

            m_numFirst += item.getAttendanceDate().equals(startDate) &&
                    ADA_ENROLLMENT_TYPE.FIRST.equals(item.getEnrollmentType()) ? 1 : 0;
            m_numLast += item.getAttendanceDate().equals(endDate) &&
                    ADA_ENROLLMENT_TYPE.LAST.equals(item.getEnrollmentType()) ? 1 : 0;
        }

        /**
         * Checks if is multiple exit.
         *
         * @return true, if is multiple exit
         */
        public boolean isMultipleExit() {
            return m_multipleExits;
        }

        /**
         * Checks if is multiple reenter.
         *
         * @return true, if is multiple reenter
         */
        public boolean isMultipleReenter() {
            return m_multipleReenter;
        }
    }

    /**
     * Helper class to be the key for the data map.
     *
     * @author Follett Software Company
     */
    class DetailItemKey implements Comparable<DetailItemKey> {
        private SisStudent m_student;

        /**
         * Instantiates a new detail item key.
         *
         * @param student SisStudent
         */
        public DetailItemKey(SisStudent student) {
            this.m_student = student;
        }

        /**
         * Compare to.
         *
         * @param other DetailItemKey
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(DetailItemKey other) {
            int value = m_student.getNameView().compareTo(other.getStudent().getNameView());
            if (value == 0) {
                value = m_student.getOid().compareTo(other.getStudent().getOid());
            }
            return value;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((DetailItemKey) obj) == 0 ? true : false;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_student.getOid().hashCode();
        }

        /**
         * Gets the student.
         *
         * @return the m_student
         */
        public SisStudent getStudent() {
            return m_student;
        }
    }

    /**
     * This class is key for the data map .
     *
     * @author Follett Software Company
     */
    class GroupItem implements Comparable<GroupItem> {
        private String m_cycle;
        private String m_fund;
        private String m_grade;
        private List<String> m_messages;
        private PlainDate[] m_mondays;
        private SisSchool m_school;

        /**
         * Construct object of GroupItem (key for the data map).
         *
         * @param school SisSchool
         * @param fund String
         * @param grade String
         * @param cycle String
         * @param mondays PlainDate[]
         * @param messages
         */
        public GroupItem(SisSchool school, String fund, String grade, String cycle, PlainDate[] mondays,
                List<String> messages) {
            this.m_school = school;
            this.m_fund = fund;
            this.m_grade = grade;
            this.m_cycle = cycle;
            this.m_messages = messages;
            this.m_mondays = mondays;
        }

        /**
         * Compare to.
         *
         * @param other GroupItem
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(GroupItem other) {
            int value = m_school.getName().compareTo(other.m_school.getName());
            if (value == 0) {
                value = m_school.getOid().compareTo(other.m_school.getOid());
            }
            if (value == 0) {
                value = m_fund.compareTo(other.m_fund);
            }
            if (value == 0) {
                value = m_grade.compareTo(other.m_grade);
            }
            if (value == 0) {
                value = m_cycle.compareTo(other.m_cycle);
            }

            return value;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((GroupItem) obj) == 0 ? true : false;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_school.getOid().hashCode() + m_fund.hashCode() + m_grade.hashCode() + m_cycle.hashCode();
        }

        /**
         * Gets the cycle.
         *
         * @return the m_cycle
         */
        public String getCycle() {
            return m_cycle;
        }

        /**
         * Gets the fund.
         *
         * @return the m_fund
         */
        public String getFund() {
            return m_fund;
        }

        /**
         * Gets the grade.
         *
         * @return the m_grade
         */
        public String getGrade() {
            return m_grade;
        }

        /**
         * Gets the mondays.
         *
         * @return m_mondays
         */
        public List<String> getMessages() {
            return m_messages == null ? Collections.EMPTY_LIST : m_messages;
        }

        /**
         * Gets the mondays.
         *
         * @return m_mondays
         */
        public PlainDate[] getMondays() {
            return m_mondays;
        }

        /**
         * Gets the school.
         *
         * @return the m_school
         */
        public SisSchool getSchool() {
            return m_school;
        }
    }


    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    protected ADADataHelper m_dataHelper;

    private static final String FIELD_ABS = "absent";
    private static final String FIELD_APPORT = "apport";
    private static final String FIELD_BEGIN = "begin";
    private static final String FIELD_CODES = "codes";
    private static final String FIELD_CYCLE = "cycle";
    private static final String FIELD_DETAIL_TABLE = "stdDetailTable";
    private static final String FIELD_END = "end";
    private static final String FIELD_END_DATE = "endDate";
    private static final String FIELD_EXC_ABS = "exc_abs";
    private static final String FIELD_FIRST = "first";
    private static final String FIELD_FUND = "fund";
    private static final String FIELD_GAIN = "gain";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_LAST = "last";
    private static final String FIELD_LOSS = "loss";
    private static final String FIELD_MESSAGES = "messages";
    private static final String FIELD_MONDAYS = "mondays";
    private static final String FIELD_MULTIPLE_REENTERS = "multipleReenters";
    private static final String FIELD_MULTIPLE_EXITS = "multipleExits";
    private static final String FIELD_NOT_ENROLLED = "notEnrolled";
    private static final String FIELD_NUM_OF_IN_SESSION_DATES = "numOfInSessionDates";
    private static final String FIELD_ONA = "otherNotApport";
    private static final String FIELD_REPORT_FORMAT = "reportFormat";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHEDULED = "scheduled";
    private static final String FIELD_START_DATE = "startDate";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_SUB_STD_NAME = "stdName";
    private static final String FIELD_SUB_TYPE = "typeSTDTable";
    private static final String FIELD_SUB_DATE = "dateSTDTable";
    private static final String FIELD_SUSPENDED = "suspended";
    private static final String FIELD_TRUANT = "truant";
    private static final String FIELD_UNEX_ABS = "unex_abs";

    private static final String INPUT_PARAM_PROCEDURE_ID = "procedureId";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SUBREPORT_ID = "stdDetailSubReport";

    /**
     * Report params
     */
    private static final String REPORT_PARAM_CURRENT_DATE = "currentTime";
    private static final String REPORT_PARAM_GENERATED_BY = "generatedBy";
    private static final String REPORT_PARAM_ORGANIZATION = "organization";
    private static final String REPORT_PARAM_START_DATE = "startDate";
    private static final String REPORT_PARAM_END_DATE = "endDate";

    /**
     * Keys for the maps
     */
    private static final String KEY_START_DATE_KEY = "startDateKey";
    private static final String KEY_END_DATE_KEY = "endDateKey";


    PlainDate m_endDate = null;
    Collection<SisSchool> m_schools = null;
    PlainDate m_startDate = null;

    private TreeMap<GroupItem, ReportDataGrid> m_additionalStdTable = new TreeMap<GroupItem, ReportDataGrid>();
    private Map<PlainDate, String> m_cyclesMap = null;
    private TreeMap<GroupItem, TreeMap<DetailItemKey, DetailItem>> m_data =
            new TreeMap<GroupItem, TreeMap<DetailItemKey, DetailItem>>();
    private final SimpleDateFormat m_formatFull = new SimpleDateFormat("MMMMM dd, yyyy, h:mm a");
    private Map<PlainDate, PlainDate[]> m_mondaysMap = null;
    private Map<PlainDate, List<String>> m_mondaysMessageMap = null;
    private Map<String, Map<String, PlainDate>> m_periodMap = null;
    private Report m_subreport;

    /**
     * Gather data.
     *
     * @return ReportDataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        addParameter(REPORT_PARAM_ORGANIZATION, getOrganization());
        addParameter(REPORT_PARAM_CURRENT_DATE, m_formatFull.format(new Date()));

        ReportDataGrid grid = new ReportDataGrid();

        loadInputData();
        loadStdDetailTable();
        populateGrid(grid);

        grid.beforeTop();
        return grid;

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

        m_subreport = ReportUtils.getReport((String) getParameter(INPUT_PARAM_SUBREPORT_ID), getBroker());

        m_dataHelper = new ADADataHelper(getBroker(), getOrganization());

        /*
         * The mechanism used to test the report populated the days report using
         * four weeks starting with the first monday after the input date.
         * The mechanism used to set the report days for production selects dates
         * from the calendar based on the cycle field
         * Only one of these should be used.
         */
        PlainDate inputDate = (PlainDate) getParameters().get(ADADataHelper.INPUT_PARAM_START_DATE);
        ArrayList cycles =
                StringUtils.convertDelimitedStringToList((String) getParameter(ADADataHelper.INPUT_PARAM_START_CYCLE),
                        ",");
        X2Criteria cycleCodesCriteria = new X2Criteria();
        cycleCodesCriteria.addIn(X2BaseBean.COL_OID, cycles);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, cycleCodesCriteria);
        query.addOrderBy(ReferenceCode.COL_CODE, true);
        ArrayList<String> codes = new ArrayList<String>();
        codes.addAll(getBroker().getGroupedCollectionByQuery(query, ReferenceCode.COL_CODE, 1024).keySet());
        Collections.sort(codes,
                new Comparator<String>() {
                    @Override
                    public int compare(String str1, String str2) {
                        return str1.toString().compareTo(str2.toString());
                    }
                });
        m_dataHelper.setCycle(inputDate, null, null, codes, false);
        PlainDate[] days = m_dataHelper.getDays();

        initializeCycleMap(codes, days);
        initializePeriodMap();

        m_startDate = days[0];
        m_endDate = days[days.length - 1];

        if (m_startDate != null && m_endDate != null) {
            this.addParameter(REPORT_PARAM_START_DATE, m_startDate);
            this.addParameter(REPORT_PARAM_END_DATE, m_endDate);

        }

        m_schools = getSchools();

        // Lookup State report source data procedure
        String procedureId = (String) getParameter(INPUT_PARAM_PROCEDURE_ID);
        m_dataHelper.initialize(getPrivilegeSet(), isSchoolContext(), getSchool(), getParameters(), getUser(),
                procedureId);

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
        addParameter(REPORT_PARAM_GENERATED_BY, userData.getUser().getNameView());
    }

    /**
     * Add detail item to the data map and increment it.
     *
     * @param item ADADetailItem
     * @param dayIndex int
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    private void addDetailItem(ADADetailItem item, int dayIndex, PlainDate startDate, PlainDate endDate) {
        if (m_schools.contains(item.getSchool())) {
            GroupItem groupItem = new GroupItem(item.getSchool(), item.getFundingCategory(), item.getGradeLevel(),
                    m_cyclesMap.get(item.getAttendanceDate()), m_mondaysMap.get(item.getAttendanceDate()),
                    m_mondaysMessageMap.get(item.getAttendanceDate()));
            DetailItemKey detailItemKey = new DetailItemKey(item.getStudent());
            TreeMap<DetailItemKey, DetailItem> items = m_data.get(groupItem);
            if (items == null) {
                items = new TreeMap<DetailItemKey, DetailItem>();
                m_data.put(groupItem, items);
            }
            DetailItem detailItem = items.get(detailItemKey);
            if (detailItem == null) {
                detailItem = new DetailItem(item.getStudent(), item.getStdSpans());
                items.put(detailItemKey, detailItem);
            }
            detailItem.increment(item, dayIndex, startDate, endDate);
        }
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private Collection<SisSchool> getSchools() {
        X2Criteria criteria = new X2Criteria();

        String exclSchool = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getOrganization().getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_SCHOOL);
        if (field != null) {
            exclSchool = field.getJavaName();

            if (exclSchool != null) {
                criteria.addNotEqualTo(exclSchool, BooleanAsStringConverter.TRUE);
            }
        }

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
    }

    /**
     * Load cycles map keyed on dates .
     *
     * @param codes Collection<String>
     * @param days PlainDate[]
     */
    private void initializeCycleMap(Collection<String> codes, PlainDate[] days) {
        DateFormat shortDateFormat = (SimpleDateFormat) getParameter(SHORT_DATEFORMAT_KEY);
        PlainDate[][] mondays = new PlainDate[days.length / 20 + 1][4];
        List<String>[] messageLists = new List[days.length / 20 + 1];
        int cycleIndex = -1;
        for (int i = 0; i < days.length / 5; ++i) {
            if (i % 4 == 0) {
                cycleIndex += 1;
            }
            PlainDate monday = days[i * 5];
            PlainDate friday = days[i * 5 + 4];
            if (DateUtils.getDayDifference(monday, friday) > 4) {
                StringBuilder message = new StringBuilder();
                message.append("Monday ");
                message.append(shortDateFormat.format(monday));
                message.append(" represents dates ");
                message.append(shortDateFormat.format(monday));
                for (int j = 1; j <= 4; ++j) {
                    message.append(", ");
                    message.append(shortDateFormat.format(days[i * 5 + j]));
                }
                if (messageLists[cycleIndex] == null) {
                    messageLists[cycleIndex] = new ArrayList(5);
                }
                messageLists[cycleIndex].add(message.toString());
            }
            mondays[cycleIndex][i % 4] = monday;
        }

        m_cyclesMap = new LinkedHashMap<PlainDate, String>();
        m_mondaysMap = new HashMap<PlainDate, PlainDate[]>();
        m_mondaysMessageMap = new HashMap();
        ArrayList<String> codesList = new ArrayList<String>();
        codesList.addAll(codes);
        if (codesList != null && days != null) {
            cycleIndex = 0;
            int datesIndex = 0;
            for (PlainDate date : days) {

                datesIndex++;
                m_cyclesMap.put(date, codesList.get(cycleIndex));
                m_mondaysMap.put(date, mondays[cycleIndex]);
                m_mondaysMessageMap.put(date, messageLists[cycleIndex]);

                if (datesIndex % 20 == 0) {
                    cycleIndex += 1;
                }
            }
        }
    }

    /**
     * Initialize map keyed on cycle number which contains map keyed on START or END constant
     * and valued with START or END date of the cycles.
     *
     */
    private void initializePeriodMap() {
        m_periodMap = new HashMap<String, Map<String, PlainDate>>();

        String tempCycle = null;
        PlainDate tempStartDate = null;
        PlainDate tempEndDate = null;
        int counter = 0;
        for (Map.Entry<PlainDate, String> entry : m_cyclesMap.entrySet()) {
            counter++;
            if (!entry.getValue().equals(tempCycle) && tempEndDate == null && tempStartDate == null) {
                tempCycle = entry.getValue();
                tempStartDate = entry.getKey();
            }
            if (entry.getValue().equals(tempCycle)) {
                tempEndDate = entry.getKey();
                if (counter < m_cyclesMap.entrySet().size()) {
                    continue;
                }
            }
            Map<String, PlainDate> tempMap = new HashMap<String, PlainDate>();
            tempMap.put(KEY_START_DATE_KEY, tempStartDate);
            tempMap.put(KEY_END_DATE_KEY, tempEndDate);
            m_periodMap.put(tempCycle, tempMap);
            tempStartDate = entry.getKey();
            tempCycle = entry.getValue();

        }

    }

    /**
     * Load data and populate data map with necessary values.
     *
     * @throws X2BaseException exception
     */
    private void loadInputData() throws X2BaseException {
        ADADetailIterator iterator = m_dataHelper.iterator();
        if (iterator != null) {
            PlainDate startDate = null;
            PlainDate endDate = null;

            List<PlainDate> days = Arrays.asList(m_dataHelper.getDays());
            try {
                ADADetailItem item = null;
                while ((item = iterator.next()) != null) {
                    if (days.contains(item.getAttendanceDate())) {
                        startDate = (days.indexOf(item.getAttendanceDate()) % 20) == 0 ? item.getAttendanceDate()
                                : startDate;
                        PlainDate firstInSessionDate = m_dataHelper.getFirstInSessionDate(item.getSchool().getOid(),
                                item.getStudent().getCalendarCode());
                        if (firstInSessionDate != null && firstInSessionDate.after(startDate)) {
                            startDate = firstInSessionDate;
                        }
                        endDate = (days.indexOf(item.getAttendanceDate()) % 20) == 19 ? item.getAttendanceDate()
                                : endDate;
                        addDetailItem(item, days.indexOf(item.getAttendanceDate()) % 20, startDate, endDate);
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Fill detailed student table with data.
     */
    private void loadStdDetailTable() {

        for (Entry<GroupItem, TreeMap<DetailItemKey, DetailItem>> groupEntry : m_data.entrySet()) {
            PlainDate startDate = m_periodMap.get(groupEntry.getKey().getCycle()).get(KEY_START_DATE_KEY);
            PlainDate endDate = m_periodMap.get(groupEntry.getKey().getCycle()).get(KEY_END_DATE_KEY);
            GroupItem groupItemStdTable = groupEntry.getKey();
            for (DetailItem detail : groupEntry.getValue().values()) {
                ReportDataGrid stdTableGrid = m_additionalStdTable.get(groupItemStdTable);
                if (stdTableGrid == null) {
                    stdTableGrid = new ReportDataGrid();
                    m_additionalStdTable.put(groupItemStdTable, stdTableGrid);

                }

                PlainDate dateSTDTable = null;
                String typeSTDTable = null;
                List<StudentEnrollmentSpan> spans = detail.getStdSpans();
                String sklOid = detail.getStudent().getSchool().getOid();
                if (spans != null && !spans.isEmpty()) {
                    for (StudentEnrollmentSpan span : spans) {
                        StudentEnrollment activeEnr = span.getFirstActiveEnrollment();
                        StudentEnrollment inactiveEnr = span.getFirstInactiveEnrollment();

                        if (activeEnr != null && sklOid != null && sklOid.equals(activeEnr.getSchoolOid())
                                && startDate.before(activeEnr.getEnrollmentDate())) {
                            typeSTDTable = activeEnr.getEnrollmentType();
                            dateSTDTable = activeEnr.getEnrollmentDate();

                            stdTableGrid.append();
                            stdTableGrid.set(FIELD_SUB_DATE, dateSTDTable);
                            stdTableGrid.set(FIELD_SUB_TYPE, typeSTDTable);
                            stdTableGrid.set(FIELD_SUB_STD_NAME, detail.getStudent().getNameView());

                        }

                        if (inactiveEnr != null && sklOid != null && sklOid.equals(inactiveEnr.getSchoolOid())
                                && inactiveEnr.getEnrollmentDate().before(endDate)) {
                            typeSTDTable = inactiveEnr.getEnrollmentType();
                            dateSTDTable = inactiveEnr.getEnrollmentDate();

                            stdTableGrid.append();
                            stdTableGrid.set(FIELD_SUB_DATE, dateSTDTable);
                            stdTableGrid.set(FIELD_SUB_TYPE, typeSTDTable);
                            stdTableGrid.set(FIELD_SUB_STD_NAME, detail.getStudent().getNameView());

                        }
                    }
                }
            }
        }
    }

    /**
     * Populate data grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateGrid(ReportDataGrid grid) {
        for (Entry<GroupItem, TreeMap<DetailItemKey, DetailItem>> groupEntry : m_data.entrySet()) {

            for (DetailItem detail : groupEntry.getValue().values()) {
                grid.append();

                ReportDataGrid detailGrid = m_additionalStdTable.get(groupEntry.getKey());
                detailGrid.beforeTop();
                grid.set(FIELD_DETAIL_TABLE, detailGrid);
                grid.set(FIELD_REPORT_FORMAT, new ByteArrayInputStream(m_subreport.getCompiledFormat()));
                grid.set(FIELD_SCHOOL, groupEntry.getKey().getSchool());
                grid.set(FIELD_STUDENT, detail.getStudent());
                grid.set(FIELD_GRADE, groupEntry.getKey().getGrade());
                grid.set(FIELD_FUND, groupEntry.getKey().getFund());
                grid.set(FIELD_MESSAGES, groupEntry.getKey().getMessages().stream().collect(Collectors.joining("\n")));
                grid.set(FIELD_MONDAYS, groupEntry.getKey().getMondays());
                grid.set(FIELD_CYCLE, groupEntry.getKey().getCycle());
                grid.set(FIELD_START_DATE, m_periodMap.get(groupEntry.getKey().getCycle()).get(KEY_START_DATE_KEY));
                grid.set(FIELD_END_DATE, m_periodMap.get(groupEntry.getKey().getCycle()).get(KEY_END_DATE_KEY));
                grid.set(FIELD_CODES, detail.getCodes());
                grid.set(FIELD_APPORT, Integer.valueOf(detail.getNumApportionment()));
                grid.set(FIELD_ABS, Integer.valueOf(detail.getNumAbsent()));
                grid.set(FIELD_EXC_ABS, Integer.valueOf(detail.getNumExcused()));
                grid.set(FIELD_UNEX_ABS, Integer.valueOf(detail.getNumUnExcused()));
                grid.set(FIELD_SCHEDULED, Integer.valueOf(detail.getNumScheduled()));
                grid.set(FIELD_TRUANT, Integer.valueOf(detail.getNumTruant()));
                grid.set(FIELD_SUSPENDED, Integer.valueOf(detail.getNumSuspended()));
                grid.set(FIELD_NOT_ENROLLED, Integer.valueOf(detail.getNumNotEnrolled()));
                grid.set(FIELD_ONA, Integer.valueOf(detail.getNumONA()));

                grid.set(FIELD_BEGIN, Integer.valueOf(detail.getNumBegin()));
                grid.set(FIELD_GAIN, Integer.valueOf(detail.getNumGain()));
                grid.set(FIELD_LOSS, Integer.valueOf(detail.getNumLoss()));
                grid.set(FIELD_END, Integer.valueOf(detail.getNumEnd()));

                grid.set(FIELD_FIRST, Integer.valueOf(detail.getNumBegin() + detail.getNumNewGain()));
                grid.set(FIELD_LAST, Integer.valueOf(detail.getNumLast()));

                grid.set(FIELD_MULTIPLE_REENTERS, detail.isMultipleReenter() ? Integer.valueOf(1) : Integer.valueOf(0));
                grid.set(FIELD_MULTIPLE_EXITS, detail.isMultipleExit() ? Integer.valueOf(1) : Integer.valueOf(0));

                grid.set(FIELD_NUM_OF_IN_SESSION_DATES,
                        Integer.valueOf(m_dataHelper.getInSessionDaysForCycle(groupEntry.getKey().getCycle(),
                                groupEntry.getKey().getSchool().getOid()).size()));
            }
        }
    }



}
