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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ca.ADADataHelper;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailItem;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailIterator;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "CA Average Daily Attendance (ADA) Detail" report .
 *
 * @author X2 Development Corporation
 */
public class ADAWeeklyReportData extends ReportJavaSourceNet {

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Helper class to save all necessary information for the given student.
     *
     * @author Follett Software Company
     */
    class DetailItem implements Comparable<DetailItem> {
        private String[] m_codes = new String[5];
        private SisStudent m_student;
        @SuppressWarnings("hiding")
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Construct object of the DetailItem class.
         *
         * @param student SisStudent
         */
        public DetailItem(SisStudent student) {
            this.m_student = student;
        }

        /**
         * Add information to the member variables of the DetailItem
         * which contains all attendance codes of the student.
         *
         * @param item ADADetailItem
         * @param dayIndex int
         */
        public void increment(ADADetailItem item, int dayIndex) {
            String attCode = item.getAttendanceCode();
            m_codes[dayIndex] = attCode;
        }

        /**
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(DetailItem other) {
            int value = m_student.getNameView().compareTo(other.m_student.getNameView());
            if (value == 0) {
                value = m_student.getOid().compareTo(other.m_student.getOid());
            }
            return value;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((DetailItem) obj) == 0 ? true : false;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_student.getOid().hashCode();
        }

        /**
         * Gets the codes.
         *
         * @return the codes
         */
        public String[] getCodes() {
            return m_codes;
        }
    }

    /**
     * Contains all information of the teacher.
     *
     * @author Follett Software Company
     */
    class TeacherDetailItem implements Comparable<TeacherDetailItem> {

        /**
         * Member variables
         */
        private int m_femaleNumber;
        private Set<String> m_gradeLevels;
        private Boolean m_isPAT;
        private int m_maleNumber;
        private MasterSchedule m_mst;
        private Map<String, Map<String, Map<PlainDate, Collection<StudentPeriodAttendance>>>> m_patMap = new HashMap();
        private ArrayList<SisStudent> m_students;
        @SuppressWarnings("hiding")
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Constants
         */
        private static final String PERSON_CODE_MALE = "M";

        /**
         * Construct object of the TeacherDetailItem class.
         *
         * @param mst MasterSchedule
         */
        public TeacherDetailItem(MasterSchedule mst) {
            m_mst = mst;
        }

        /**
         * Gets the students.
         *
         * @return the stdOids
         */
        public ArrayList<SisStudent> getStudents() {
            return m_students;
        }

        /**
         * Gets the master schedule.
         *
         * @return the courseName
         */
        public MasterSchedule getMasterSchedule() {
            return m_mst;
        }

        /**
         * Gets the male number.
         *
         * @return the maleNumber
         */
        public int getMaleNumber() {
            return m_maleNumber;
        }

        /**
         * Gets the female number.
         *
         * @return the femaleNumber
         */
        public int getFemaleNumber() {
            return m_femaleNumber;
        }

        /**
         * Gets the grade levels.
         *
         * @return the gradeLevel
         */
        public Set<String> getGradeLevels() {
            return m_gradeLevels;
        }

        /**
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(TeacherDetailItem o) {
            // mst.getCourseView(), mst.getSectionNumber(), mst.getPrimaryRoom().getRoomNumber()
            int value = m_mst.getCourseView().compareTo(o.m_mst.getCourseView());
            if (value == 0) {
                value = m_mst.getSectionNumber().compareTo(o.m_mst.getSectionNumber());
            }
            if (value == 0) {
                value = m_mst.getRoomView().compareTo(o.m_mst.getRoomView());
            }

            return value;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((TeacherDetailItem) obj) == 0 ? true : false;
        }

        /**
         * Query all PAT records for the given master schedule and days on first invocation.</br>
         * The results of the query are stored in a Map<stdOid, Map<date, Collection<patBean>>.</br>
         * The codeView is returned if a PAT exists for this student on the given date for this
         * given master schedule.
         *
         * @param student SisStudent
         * @param masterSchedule MasterSchedule
         * @param plainDate PlainDate
         * @return String
         */
        public String getPeriodAttendanceCode(SisStudent student, MasterSchedule masterSchedule, PlainDate plainDate) {
            String value = ATT_CODE_PRESENT;
            Map<String, Map<PlainDate, Collection<StudentPeriodAttendance>>> patMap =
                    m_patMap.get(masterSchedule.getOid());
            if (patMap == null) {
                X2Criteria patCriteria = new X2Criteria();
                patCriteria.addEqualTo(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, masterSchedule.getOid());
                patCriteria.addIn(StudentPeriodAttendance.COL_DATE, Arrays.asList(m_days));

                QueryByCriteria patQuery = new QueryByCriteria(StudentPeriodAttendance.class, patCriteria);
                String[] columnKeys = new String[] {StudentPeriodAttendance.COL_STUDENT_OID,
                        StudentPeriodAttendance.COL_DATE};

                patMap = m_broker.getGroupedCollectionByQuery(patQuery, columnKeys, new int[] {1024, 1024});
                m_patMap.put(masterSchedule.getOid(), patMap);
            }

            if (!patMap.isEmpty()) {
                Map<PlainDate, Collection<StudentPeriodAttendance>> patsMap = patMap.get(student.getOid());

                if (patsMap != null) {
                    Collection<StudentPeriodAttendance> patsForDate = patsMap.get(plainDate);
                    if (patsForDate != null && !patsForDate.isEmpty()) {
                        StudentPeriodAttendance pat = (StudentPeriodAttendance) patsForDate.toArray()[0];
                        if (!StringUtils.isEmpty(pat.getOtherCode())) {
                            value = pat.getOtherCode();
                        } else if (!StringUtils.isEmpty(getReasonCode(pat))) {
                            value = getReasonCode(pat);
                        } else if (pat.getAbsentIndicator()) {
                            value = pat.getExcusedIndicator() ? ATT_CODE_ABSENT_EX : ATT_CODE_ABSENT;
                        } else if (pat.getTardyIndicator()) {
                            value = pat.getExcusedIndicator() ? ATT_CODE_TARDY_EX : ATT_CODE_TARDY;
                        }
                    }
                }
            }

            return value;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_mst.getCourseView().hashCode() + m_mst.getSectionNumber().hashCode();
        }

        /**
         * Add student and his gender the to the teacher detail item.
         *
         * @param std SisStudent
         */
        public void increment(SisStudent std) {
            if (m_students == null) {
                m_students = new ArrayList<SisStudent>();
            }
            m_students.add(std);

            if (m_gradeLevels == null) {
                m_gradeLevels = new HashSet<String>();
            }
            m_gradeLevels.add(std.getGradeLevel());

            if (PERSON_CODE_MALE.equals(std.getPerson().getGenderCode())) {
                m_maleNumber += 1;
            } else {
                m_femaleNumber += 1;
            }
        }

        /**
         * Method checks if SKL uses PAT.
         *
         * @return true, if is period attendance
         */
        public boolean isPeriodAttendance() {
            if (m_isPAT == null) {
                SisSchool skl = null;

                if (m_mst != null && m_mst.getSchedule() != null && (skl = m_mst.getSchedule().getSchool()) != null) {
                    m_isPAT = Boolean
                            .valueOf(BooleanAsStringConverter.TRUE.equals(skl.getFieldValueByAlias(ALIAS_SKL_USE_PAT)));
                } else {
                    m_isPAT = Boolean.FALSE;
                }
            }

            return m_isPAT.booleanValue();
        }

        /**
         * Returns reason code.
         */
        private String getReasonCode(StudentPeriodAttendance pat) {
            return getDataHelper().getData().lookupStateValue(StudentPeriodAttendance.class,
                    StudentPeriodAttendance.COL_REASON_CODE,
                    pat.getReasonCode());
        }
    }

    /**
     * The Class StudentScheduleSpan.
     */
    private class StudentScheduleSpan {
        /*
         * Instance variables
         */
        private StudentScheduleChange m_entryChange;
        private PlainDate m_entryDate;
        private StudentScheduleChange m_exitChange;
        private PlainDate m_exitDate;
        private MasterSchedule m_section;
        @SuppressWarnings("hiding")
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Constructor.
         *
         * @param section MasterSchedule
         */
        StudentScheduleSpan(MasterSchedule section) {
            m_section = section;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String output = m_section.getCourseView();

            output += " [" + m_section.getTermView() + "]: ";
            output += " " + m_entryDate + " to " + m_exitDate;
            output += " EntryChange: " + (m_entryChange != null ? "Yes" : "No");
            output += " ExitChange: " + (m_exitChange != null ? "Yes" : "No");

            return output;
        }

        /**
         * Returns the schedule begin date. This may be the term begin date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Returns the schedule end date. This may be the term end date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Returns the master section for this schedule span.
         *
         * @return MasterSchedule
         */
        MasterSchedule getSection() {
            return m_section;
        }

        /**
         * Set the student schedule change for add.
         *
         * @param entryChange void
         */
        void setEntryChange(StudentScheduleChange entryChange) {
            this.m_entryChange = entryChange;
        }

        /**
         * Sets the entry date for this student in this class.
         *
         * @param entryDate void
         */
        void setEntryDate(PlainDate entryDate) {
            m_entryDate = entryDate;
        }

        /**
         * Set the student schedule change for drop.
         *
         * @param exitChange void
         */
        void setExitChange(StudentScheduleChange exitChange) {
            this.m_exitChange = exitChange;
        }

        /**
         * Sets the exit date for this student in this class.
         *
         * @param exitDate void
         */
        void setExitDate(PlainDate exitDate) {
            m_exitDate = exitDate;
        }
    }

    private static final String ALIAS_SKL_USE_PAT = "DOE SKL USE PAT";

    private static final String ATT_CODE_ABSENT = "A";
    private static final String ATT_CODE_ABSENT_EX = "EX";
    private static final String ATT_CODE_PRESENT = "+";
    private static final String ATT_CODE_TARDY = "T-";
    private static final String ATT_CODE_TARDY_EX = "T+";
    private final List<String> ATT_REPLACEABLE_CODES = Arrays.asList(
            new String[] {ATT_CODE_ABSENT, ATT_CODE_ABSENT_EX, ATT_CODE_PRESENT, ATT_CODE_TARDY, ATT_CODE_TARDY_EX});

    private final String[] BLANK_CODES = {"", "", "", "", ""};

    private static final String FIELD_CODES = "codes";
    private static final String FIELD_FEMALE_NUMBER = "femaleNumber";
    private static final String FIELD_MALE_NUMBER = "maleNumber";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_SORT = "sklNameSort";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_SECTION_SORT = "sectionSort";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_SORT = "studentSort";
    private static final String FIELD_TEACHER = "teacher";
    private static final String FIELD_TEACHER_SORT = "teacherSort";

    private static final String INPUT_PARAM_ACTIVE_ONLY = "activeOnly";
    private static final String INPUT_PARAM_BLANKS_ALWAYS = "blanksAlways";
    private static final String INPUT_PARAM_HEADING = "reportHeading";
    private static final String INPUT_PARAM_HOMEROOM_ONLY = "applyFilter";
    private static final String INPUT_PARAM_PROCEDURE_ID = "procedureId";
    private static final String INPUT_PARAM_QUERY_BY = "queryBy";
    private static final String INPUT_PARAM_START_DATE = "startDate";
    private static final String INPUT_SINGLE_TEACHER = "singleTeacher";


    private static final String REPORT_PARAM_ADMIN_NAME = "adminName";
    private static final String REPORT_PARAM_CURRENT_DATE = "currentTime";
    private static final String REPORT_PARAM_DAYS = "days";
    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_GENERATED_BY = "generatedBy";
    private static final String REPORT_PARAM_HEADING = "reportHeading";
    private static final String REPORT_PARAM_ORGANIZATION = "organization";
    private static final String REPORT_PARAM_START_DATE = "startDate";

    private boolean m_homeroomOnly = false;
    private Map<String, DetailItem> m_data = new HashMap<String, ADAWeeklyReportData.DetailItem>();
    private ADADataHelper m_dataHelper;
    private PlainDate m_endDate = null;
    private final SimpleDateFormat m_formatFull = new SimpleDateFormat("MMMMM dd, yyyy, h:mm a");
    private PlainDate m_startDate = null;
    private X2Criteria m_staffCriteria = null;
    private Map<String, Set<MasterSchedule>> m_stdMap = null;
    private Map<SisStaff, ArrayList<TeacherDetailItem>> m_teacherMap = null;
    private String m_teacherOid = null;
    private Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    protected X2Broker m_broker;
    protected PlainDate[] m_days = new PlainDate[5];

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
        addParameter(REPORT_PARAM_START_DATE, m_startDate);
        addParameter(REPORT_PARAM_END_DATE, m_endDate);
        addParameter(REPORT_PARAM_CURRENT_DATE, m_formatFull.format(new Date()));
        addParameter(REPORT_PARAM_ADMIN_NAME, getUser().getNameView());
        addParameter(REPORT_PARAM_DAYS, daysFormattedParam());
        addParameter(REPORT_PARAM_HEADING, getParameter(INPUT_PARAM_HEADING));

        ReportDataGrid grid = new ReportDataGrid();

        if (m_teacherMap.entrySet().size() > 0) {
            loadInputData();

            populateGrid(grid);
        }

        grid.beforeTop();
        return grid;

    }

    protected ADADataHelper getDataHelper() {
        return m_dataHelper;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_broker = getBroker();

        Boolean applyFilter = (Boolean) getParameters().get(INPUT_PARAM_HOMEROOM_ONLY);
        if (applyFilter != null) {
            m_homeroomOnly = applyFilter.booleanValue();
        }

        m_dataHelper = new ADADataHelper(getBroker(), getOrganization());

        PlainDate inputDate = (PlainDate) getParameters().get(INPUT_PARAM_START_DATE);

        initializeEndDate(inputDate);

        if (m_startDate != null && m_endDate != null) {
            this.addParameter(REPORT_PARAM_START_DATE, m_startDate);
            this.addParameter(REPORT_PARAM_END_DATE, m_endDate);

        }

        // Lookup State report source data procedure
        String procedureId = (String) getParameter(INPUT_PARAM_PROCEDURE_ID);
        m_dataHelper.initialize(getPrivilegeSet(), isSchoolContext(), getSchool(),
                getParameters(), getUser(), procedureId);

        setMondays();

        m_staffCriteria = initializeStaffCriteria();

        m_stdMap = new HashMap<String, Set<MasterSchedule>>();
        buildStudentScheduleMap(m_stdMap);

        m_teacherMap = buildTeacherMap();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ReportJavaDataSource#saveState(com.follett.fsc.core.
     *      k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        if (userData.getSessionNavConfig().getApplicationContext().equals(ApplicationContext.STAFF)) {
            m_teacherOid = userData.getStaffOid();
        }
        addParameter(REPORT_PARAM_GENERATED_BY, userData.getUser().getNameView());
    }

    /**
     * Add detail item to the data map and increment it.
     *
     * @param item ADADetailItem
     * @param dayIndex int
     */
    private void addDetailItem(ADADetailItem item, int dayIndex) {
        String stdOid = item.getStudent().getOid();
        DetailItem detailItem = m_data.get(stdOid);
        if (detailItem == null) {
            detailItem = new DetailItem(item.getStudent());
            m_data.put(stdOid, detailItem);
        }
        detailItem.increment(item, dayIndex);
    }

    /**
     * Add section to the given student.
     *
     * @param studentOid String
     * @param masterScheduleOid String
     * @param studentMap Map
     */
    private void addMasterSchedule(String studentOid,
                                   String masterScheduleOid,
                                   Map<String, Set<MasterSchedule>> studentMap) {
        Set<MasterSchedule> set = studentMap.get(studentOid);
        if (set == null) {
            set = new HashSet<MasterSchedule>();
            studentMap.put(studentOid, set);
        }
        MasterSchedule bean = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, masterScheduleOid);
        set.add(bean);
    }

    /**
     * Builds student schedule map.
     *
     * @param studentMap Map<String,Set<MasterSchedule>>
     */
    private void buildStudentScheduleMap(Map<String, Set<MasterSchedule>> studentMap) {
        Map<String, List<StudentSchedule>> schedules = getStudentSchedules();
        Map<String, List<StudentScheduleChange>> changes = getStudentScheduleChanges();
        Set<String> students = new HashSet();
        students.addAll(schedules.keySet());
        students.addAll(changes.keySet());
        for (String student : students) {
            for (StudentScheduleSpan span : getStudentScheduleSpans(schedules.get(student), changes.get(student))) {
                if (getSchool().getOid().equals(span.getSection().getSchedule().getSchoolOid())
                        && !m_endDate.before(span.getEntryDate()) && !m_startDate.after(span.getExitDate())) {
                    addMasterSchedule(student, span.getSection().getOid(), studentMap);
                }
            }
        }
    }

    /**
     * Returns map keyed on SisStaff with list of teacher details.
     *
     * @return Map
     */
    private Map<SisStaff, ArrayList<TeacherDetailItem>> buildTeacherMap() {
        Map<SisStaff, ArrayList<TeacherDetailItem>> teacherMap = new HashMap<SisStaff, ArrayList<TeacherDetailItem>>();
        if (m_stdMap != null) {
            for (Entry<String, Set<MasterSchedule>> entry : m_stdMap.entrySet()) {
                String stdOid = entry.getKey();
                Set<MasterSchedule> schedules = entry.getValue();
                SisStudent std = (SisStudent) getBroker().getBeanByOid(SisStudent.class, stdOid);

                for (MasterSchedule mst : schedules) {
                    for (ScheduleTeacher teacher : mst.getTeacherSections()) {
                        SisStaff teacherToPut = teacher.getStaff();
                        ArrayList<TeacherDetailItem> stfDetails = teacherMap.get(teacherToPut);
                        if (stfDetails == null) {
                            stfDetails = new ArrayList<TeacherDetailItem>();
                            teacherMap.put(teacherToPut, stfDetails);
                        }

                        TeacherDetailItem teacherDetailNew = new TeacherDetailItem(mst);
                        if (stfDetails.contains(teacherDetailNew)) {
                            teacherDetailNew = stfDetails.get(stfDetails.indexOf(teacherDetailNew));
                        } else {
                            stfDetails.add(teacherDetailNew);
                        }

                        teacherDetailNew.increment(std);
                    }
                }
            }
        }
        return teacherMap;
    }

    /**
     * Search through schedule change records for alternate start dates and other
     * sections to report.
     *
     * Loop through schedule change records for each section, in date order. Add section
     * spans based on change drop or add record.s
     *
     * Some will have been dropped after the end of a term but should still be counted.
     *
     * @param scheduleChanges List<StudentScheduleChange>
     * @param scheduleSpanMap Map<String,StudentScheduleSpan>
     */
    private void checkScheduleChanges(List<StudentScheduleChange> scheduleChanges,
                                      Map<String, StudentScheduleSpan> scheduleSpanMap) {
        MasterSchedule lastSection = null;
        StudentScheduleChange lastChange = null;
        PlainDate termStart = null;
        PlainDate termEnd = null;

        /*
         * Work backward in time through schedule changes.
         * DROP will open a new section and the ADD before it will finish that section.
         * A DROP without a following ADD will be considered open at start of term.
         * Any activity entirely before start of term will be ignored.
         */
        for (StudentScheduleChange change : scheduleChanges) {
            // Check for a new section.
            if (lastSection == null || !lastSection.getOid().equals(change.getMasterScheduleOid())) {
                // Save the working section if necessary.
                if (lastChange != null) {
                    // The last change record for this section (in reverse chronological order)
                    // was a drop. Assume the section was scheduled from the beginning of the
                    // term/year.
                    StudentScheduleSpan info = new StudentScheduleSpan(lastSection);
                    info.setEntryDate(termStart);
                    if (lastChange.getEffectiveDate().after(termEnd)) {
                        info.setExitDate(termEnd);
                    } else {
                        info.setExitDate(lastChange.getEffectiveDate());
                    }
                    info.setExitChange(lastChange);
                    // Avoid recording sections scheduled out entirely
                    // before the start of it's term. This is just scheduling activity.
                    if (!info.getExitDate().before(termStart)) {
                        scheduleSpanMap.put(lastChange.getOid(), info);
                    }
                }

                // Initialize the new section
                lastChange = null;
                lastSection = change.getMasterSchedule();
                termStart = null;
                termEnd = null;
                Collection<ScheduleTermDate> termDates = getTermDates(lastSection.getScheduleTermOid());
                for (ScheduleTermDate termDate : termDates) {
                    if (termStart == null || termStart.after(termDate.getStartDate())) {
                        termStart = termDate.getStartDate();
                    }
                    if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                        termEnd = termDate.getEndDate();
                    }
                }
                // If a term is missing any dates, use school schedule dates or district calendar
                // dates.
                if (termStart == null) {
                    termStart = lastSection.getSchedule().getStartDate();
                    if (termStart == null) {
                        termStart = getOrganization().getCurrentContext().getStartDate();
                    }
                }
                if (termEnd == null) {
                    termEnd = lastSection.getSchedule().getEndDate();
                    if (termEnd == null) {
                        termEnd = getOrganization().getCurrentContext().getEndDate();
                    }
                }
            }

            // For a section, see if its dates compare with report dates or term dates.
            if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                lastChange = change;
            } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                if (lastChange == null) {
                    // No previous record, assume current student schedule. Find based on master
                    // OID.
                    StudentScheduleSpan info = scheduleSpanMap.get(change.getMasterScheduleOid());
                    if (info != null) {
                        info.setEntryDate(change.getEffectiveDate());
                        info.setEntryChange(change);
                        if (info.getEntryDate().before(termStart)) {
                            info.setEntryDate(termStart);
                        }
                    }
                } else {
                    StudentScheduleSpan info = new StudentScheduleSpan(change.getMasterSchedule());
                    info.setEntryDate(change.getEffectiveDate());
                    info.setEntryChange(change);
                    if (info.getEntryDate().before(termStart)) {
                        info.setEntryDate(termStart);
                    }
                    info.setExitDate(lastChange.getEffectiveDate());
                    // Avoid entering a change date that is after the term end date
                    if (info.getExitDate().after(termEnd)) {
                        info.setExitDate(termEnd);
                    }
                    info.setExitChange(lastChange);
                    // Avoid recording sections scheduled out entirely
                    // before the start of it's term. This is just scheduling activity.
                    if (!info.getExitDate().before(termStart)) {
                        scheduleSpanMap.put(change.getOid(), info);
                    }
                }
                lastChange = null;
            }
        }
        if (lastChange != null) {
            // The last change record for this section (in reverse chronological order)
            // was a drop. Assume the section was scheduled from the beginning of the term/year.
            StudentScheduleSpan info = new StudentScheduleSpan(lastSection);
            info.setEntryDate(termStart);
            if (lastChange.getEffectiveDate().after(termEnd)) {
                info.setExitDate(termEnd);
            } else {
                info.setExitDate(lastChange.getEffectiveDate());
            }
            info.setExitChange(lastChange);
            // Avoid recording sections scheduled out entirely
            // before the start of it's term. This is just scheduling activity.
            if (!info.getExitDate().before(termStart)) {
                scheduleSpanMap.put(lastChange.getOid(), info);
            }
        }
    }

    /**
     * Returns dates in required for the report format.
     *
     * @return String[]
     */
    private String[] daysFormattedParam() {
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd");
        String[] formattedDays = new String[m_days.length];
        for (int i = 0; i < m_days.length; ++i) {
            formattedDays[i] = sdf.format(m_days[i]);
        }

        return formattedDays;
    }

    /**
     * For all populated sections in the schedule span map, if the entry date or exit date is
     * missing, populate with term dates.
     *
     * @param scheduleSpanMap Map<String,StudentScheduleSpan>
     */
    private void fillTermDates(Map<String, StudentScheduleSpan> scheduleSpanMap) {
        PlainDate termStart = null;
        PlainDate termEnd = null;

        Iterator<StudentScheduleSpan> iterator = scheduleSpanMap.values().iterator();
        while (iterator.hasNext()) {
            StudentScheduleSpan info = iterator.next();
            if (info.getEntryDate() == null || info.getExitDate() == null) {
                termStart = null;
                termEnd = null;
                Collection<ScheduleTermDate> termDates = getTermDates(info.getSection().getScheduleTermOid());
                for (ScheduleTermDate termDate : termDates) {
                    if (termStart == null || termStart.after(termDate.getStartDate())) {
                        termStart = termDate.getStartDate();
                    }
                    if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                        termEnd = termDate.getEndDate();
                    }
                }

                if (info.getEntryDate() == null) {
                    info.setEntryDate(termStart);
                }
                if (info.getExitDate() == null) {
                    info.setExitDate(termEnd);
                }
            }

            /*
             * If the entry/exit dates are out of order, remove the info.
             * This can be caused by drop/re-add after the end of term.
             * The original entry will exist before the drop, so this record is extra.
             */
            if (info.getExitDate() != null &&
                    info.getEntryDate() != null &&
                    info.getExitDate().before(info.getEntryDate())) {
                iterator.remove();
            }
        }
    }

    /**
     * Construct an initial criteria for loading student schedule change records.
     * This should match the student criteria used for the student query as closely as possible.
     *
     * @return X2Criteria
     */
    private Map<String, List<StudentScheduleChange>> getStudentScheduleChanges() {
        X2Criteria studentScheduleChangeCriteria = new X2Criteria();

        // From Class type section
        studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        studentScheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);

        // section term started before report date.
        // Require section term to start before end/report date.
        studentScheduleChangeCriteria.addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_endDate);

        studentScheduleChangeCriteria.addGreaterOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_END_DATE, m_startDate);

        studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, getSchool().getOid());

        studentScheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria);

        studentScheduleChangeCriteria.addIn(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER +
                ScheduleTeacher.COL_STAFF_OID, staffSubQuery);

        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, studentScheduleChangeCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, false);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
        return getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 32);
    }

    /**
     * Construct an initial criteria for loading student schedule records.
     * This should match the student criteria used for the student query as closely as possible.
     *
     * @return X2Criteria
     */
    private Map<String, List<StudentSchedule>> getStudentSchedules() {

        X2Criteria studentScheduleCriteria = new X2Criteria();

        // Master type Class
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        studentScheduleCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_endDate);

        studentScheduleCriteria.addGreaterOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_END_DATE, m_startDate);

        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, m_staffCriteria);

        studentScheduleCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER +
                ScheduleTeacher.COL_STAFF_OID,
                staffSubQuery);

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
        query.addOrderBy(StudentSchedule.COL_STUDENT_OID, true);
        query.addOrderBy(StudentSchedule.COL_SECTION_OID, true);

        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 32);
    }

    /**
     * Returns student schedule spans by list schedules and schedule changes.
     *
     * @param schedules List<StudentSchedule>
     * @param changes List<StudentScheduleChange>
     * @return List
     */
    private List<StudentScheduleSpan> getStudentScheduleSpans(List<StudentSchedule> schedules,
                                                              List<StudentScheduleChange> changes) {
        Map<String, StudentScheduleSpan> scheduleSpanMap = new HashMap<String, StudentScheduleSpan>();


        if (schedules != null) {
            for (StudentSchedule schedule : schedules) {
                StudentScheduleSpan info = new StudentScheduleSpan(schedule.getSection());
                scheduleSpanMap.put(schedule.getSectionOid(), info);
            }
        }

        if (changes != null) {
            checkScheduleChanges(changes, scheduleSpanMap);
        }


        // Fill in any empty entry/exit dates with term dates for the section term.
        fillTermDates(scheduleSpanMap);

        List<StudentScheduleSpan> scheduleSpanList = new ArrayList<StudentScheduleSpan>(scheduleSpanMap.values());
        return scheduleSpanList;
    }

    /**
     * Load the schedule term dates for a schedule term oid.
     * Keep a map of existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    private Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;

        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }

        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Initialize start and end date of the period.
     *
     * @param start PlainDate
     */
    private void initializeEndDate(PlainDate start) {
        Calendar calStart = Calendar.getInstance();
        calStart.setTime(start);

        calStart.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
        m_startDate = new PlainDate(calStart.getTime());
        int counter = 0;
        while (4 != counter) {
            int dayOfWeek = calStart.get(Calendar.DAY_OF_WEEK);
            if (dayOfWeek != 1 && dayOfWeek != 7) {
                counter++;
            }
            calStart.add(Calendar.DAY_OF_MONTH, 1);
        }
        m_endDate = new PlainDate(calStart.getTime());
    }

    /**
     * Build the criteria based on user input.
     *
     * @return Criteria
     */
    private X2Criteria initializeStaffCriteria() {

        X2Criteria criteria = new X2Criteria();

        String queryBy = (String) getParameter(INPUT_PARAM_QUERY_BY);

        Boolean isSingle = ((Boolean) getParameter(INPUT_SINGLE_TEACHER));

        if (isSingle == null || !isSingle.booleanValue()) {
            if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
                criteria = getCurrentCriteria();
            } else {
                if (isSchoolContext()) {
                    criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());

                } else {
                    criteria.addAndCriteria(getOrganizationCriteria(SisStaff.class));
                }
            }

            if (((Boolean) getParameter(INPUT_PARAM_ACTIVE_ONLY)).booleanValue()) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
            }
        } else {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_teacherOid);
        }

        return criteria;
    }

    /**
     * Load data and populate data map with necessary values.
     *
     * @throws X2BaseException exception
     */
    private void loadInputData() throws X2BaseException {
        ADADetailIterator iterator = m_dataHelper.iterator();
        if (iterator != null) {
            List<PlainDate> days = Arrays.asList(m_days);
            try {
                ADADetailItem item = null;
                while ((item = iterator.next()) != null) {
                    if (item.getSchool().getOid().equals(getSchool().getOid())
                            && days.contains(item.getAttendanceDate())) {
                        addDetailItem(item, days.indexOf(item.getAttendanceDate()));
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Populate data grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateGrid(ReportDataGrid grid) {
        PlainDate today = new PlainDate();
        boolean isBlanksAlways = false;
        if (getParameter(INPUT_PARAM_BLANKS_ALWAYS) != null
                && ((Boolean) getParameter(INPUT_PARAM_BLANKS_ALWAYS)).booleanValue()) {
            isBlanksAlways = true;
        }

        for (SisStaff stf : m_teacherMap.keySet()) {
            ArrayList<TeacherDetailItem> stfDetailItem = m_teacherMap.get(stf);
            for (TeacherDetailItem item : stfDetailItem) {
                List<SisStudent> students = item.getStudents();
                Collections.sort(students, new Comparator<SisStudent>() {
                    @Override
                    public int compare(SisStudent s1, SisStudent s2) {
                        return s1.getNameView().compareTo(s2.getNameView());
                    }
                });
                for (SisStudent student : students) {
                    if (!(m_homeroomOnly && StringUtils
                            .isEmpty(item.getMasterSchedule().getSchoolCourse().getCourse().getShortDescription()))) {
                        if (m_data.keySet().contains(student.getOid())) {
                            grid.append();
                            grid.set(FIELD_SCHOOL, getSchool());
                            grid.set(FIELD_SCHOOL_SORT, getSchool().getName() + getSchool().getOid());
                            grid.set(FIELD_TEACHER, stf);
                            grid.set(FIELD_TEACHER_SORT, stf.getNameView() + stf.getOid());

                            grid.set(FIELD_STUDENT, student);
                            grid.set(FIELD_STUDENT_SORT, student.getNameView());
                            String codes[] = isBlanksAlways ? BLANK_CODES : m_data.get(student.getOid()).getCodes();

                            if (item.isPeriodAttendance()) {
                                boolean isCopy = false;
                                for (int iCode = 0; iCode < codes.length; ++iCode) {
                                    // if ADA code is candidate for replacement
                                    if (ATT_REPLACEABLE_CODES.contains(codes[iCode])) {
                                        String replacementCode = item.getPeriodAttendanceCode(student,
                                                item.getMasterSchedule(), m_days[iCode]);
                                        if (!StringUtils.isEmpty(replacementCode)) {
                                            if (!isCopy) {
                                                codes = Arrays.copyOf(codes, codes.length);
                                                isCopy = true;
                                            }
                                            codes[iCode] = isBlanksAlways ? "" : replacementCode;
                                        }
                                    }
                                }
                            }

                            // Remove codes for future days
                            for (int i = 0; i < codes.length; ++i) {
                                if (m_days.length > i && m_days[i].after(today)) {
                                    codes[i] = "";
                                }
                            }
                            grid.set(FIELD_CODES, codes);
                            grid.set(FIELD_SECTION_SORT, item.getMasterSchedule().getCourseView());
                            grid.set(FIELD_SECTION, item.getMasterSchedule());
                            grid.set(FIELD_MALE_NUMBER, Integer.valueOf(item.getMaleNumber()));
                            grid.set(FIELD_FEMALE_NUMBER, Integer.valueOf(item.getFemaleNumber()));
                        }
                    }
                }
            }
        }

        grid.sort(
                Arrays.asList(
                        new String[] {FIELD_SCHOOL_SORT, FIELD_TEACHER_SORT, FIELD_SECTION_SORT, FIELD_STUDENT_SORT}),
                false);
    }

    /**
     * Set dates.
     */
    private void setMondays() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(m_startDate);
        for (int i = 0; i < m_days.length / 5; ++i) {
            while (cal.get(Calendar.DAY_OF_WEEK) != Calendar.MONDAY) {
                cal.add(Calendar.DAY_OF_WEEK, 1);
            }
            m_days[i * 5] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 1] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 2] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 3] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 4] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
        }
    }
}
