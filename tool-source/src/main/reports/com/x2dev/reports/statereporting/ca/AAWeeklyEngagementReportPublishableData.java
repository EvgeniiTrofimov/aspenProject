/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportDeliveryJob;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "CA Average Daily Attendance (ADA) Detail" report .
 *
 * @author X2 Development Corporation
 */
public class AAWeeklyEngagementReportPublishableData extends ReportJavaSourceNet implements Publishable {

    private static final String ALIAS_RECEIVE_ATTENDANCE_CERTIFICATION = "stf-receive-attendance-cert";
    private static final String CERTIFY = "1";
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String REPORT_DESCRIPTION_PREFIX = "WER_";
    private static final String SKL_ID_OPPORTUNITY = "1964501";

    private static final String PARAM_CERTIFICATION = "certification";
    private static final String PARAM_PUBLISHED_INDICATOR = "publishedIndicator";

    private Map<String, Collection<SisStaff>> m_schoolToStaff;

    private boolean m_isCertified;

    /**
     * Enum containing publish type values.
     */
    private static enum PublishType {
        NONE, PUBLISH, PREVIEW, PRINT
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
         * Returns the schedule begin date. This may be the term begin date, or
         * other date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Returns the schedule end date. This may be the term end date, or
         * other date if a schedule change occurred during the term.
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

    private static final String ALIAS_DOE_DISTRICT_ID = "DOE DISTRICT ID";
    private static final String ALIAS_DOE_SCHOOL_ID = "DOE SCHOOL ID";
    private static final String CRS_SD700 = "SD700";
    private static final String CRS_SD800 = "SD800";

    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_ATT_MONDAY = "attMonday";
    private static final String FIELD_ATT_TUESDAY = "attTuesday";
    private static final String FIELD_ATT_WEDNESDAY = "attWednesday";
    private static final String FIELD_ATT_THURSDAY = "attThursday";
    private static final String FIELD_ATT_FRIDAY = "attFriday";
    private static final String FIELD_USER = "user";

    private static final String INPUT_PARAM_START_DATE = "startDate";

    private static final String REPORT_PARAM_SESSION_USERNAME = "sessionUserName";
    private static final String REPORT_PARAM_MONTH_FORMAT = "monthDateFormat";
    private static final String REPORT_PARAM_CURRENT_DATE = "today";
    private static final String REPORT_PARAM_START_DATE = "startDate";
    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_DISTRICT_ID = "districtId";
    private static final String REPORT_PARAM_SCHOOL_ID = "schoolId";
    private static final String REPORT_PARAM_MST_DESC = "mstDescription";
    private static final String REPORT_PARAM_MST_SECTION = "mstSection";
    private static final String REPORT_PARAM_INSTRUCTIONAL_MIN = "instructionalMin";
    private static final String REPORT_PARAM_MONDAY_NONSESSION_IND = "nonSessionIndMonday";
    private static final String REPORT_PARAM_TUESDAY_NONSESSION_IND = "nonSessionIndTuesday";
    private static final String REPORT_PARAM_WEDNESDAY_NONSESSION_IND = "nonSessionIndWednesday";
    private static final String REPORT_PARAM_THURSDAY_NONSESSION_IND = "nonSessionIndThursday";
    private static final String REPORT_PARAM_FRIDAY_NONSESSION_IND = "nonSessionIndFriday";
    private final String[] nonSessionIndDays = new String[] {REPORT_PARAM_MONDAY_NONSESSION_IND,
            REPORT_PARAM_TUESDAY_NONSESSION_IND, REPORT_PARAM_WEDNESDAY_NONSESSION_IND,
            REPORT_PARAM_THURSDAY_NONSESSION_IND, REPORT_PARAM_FRIDAY_NONSESSION_IND};

    private static final String REPORT_PARAM_MON_INST_INPERSON = "mondayInstInPerson";
    private static final String REPORT_PARAM_MON_INST_OLSYNC = "mondayInstOLSync";
    private static final String REPORT_PARAM_MON_INST_ASYNC = "mondayInstASync";
    private static final String REPORT_PARAM_TUE_INST_INPERSON = "tuesdayInstInPerson";
    private static final String REPORT_PARAM_TUE_INST_OLSYNC = "tuesdayInstOLSync";
    private static final String REPORT_PARAM_TUE_INST_ASYNC = "tuesdayInstASync";
    private static final String REPORT_PARAM_WED_INST_INPERSON = "wednesdayInstInPerson";
    private static final String REPORT_PARAM_WED_INST_OLSYNC = "wednesdayInstOLSync";
    private static final String REPORT_PARAM_WED_INST_ASYNC = "wednesdayInstASync";
    private static final String REPORT_PARAM_THU_INST_INPERSON = "thursdayInstInPerson";
    private static final String REPORT_PARAM_THU_INST_OLSYNC = "thursdayInstOLSync";
    private static final String REPORT_PARAM_THU_INST_ASYNC = "thursdayInstASync";
    private static final String REPORT_PARAM_FRI_INST_INPERSON = "fridayInstInPerson";
    private static final String REPORT_PARAM_FRI_INST_OLSYNC = "fridayInstOLSync";
    private static final String REPORT_PARAM_FRI_INST_ASYNC = "fridayInstASync";
    private final String[][] instDeliveryMethod = new String[][] {
            {REPORT_PARAM_MON_INST_INPERSON, REPORT_PARAM_MON_INST_OLSYNC, REPORT_PARAM_MON_INST_ASYNC},
            {REPORT_PARAM_TUE_INST_INPERSON, REPORT_PARAM_TUE_INST_OLSYNC, REPORT_PARAM_TUE_INST_ASYNC},
            {REPORT_PARAM_WED_INST_INPERSON, REPORT_PARAM_WED_INST_OLSYNC, REPORT_PARAM_WED_INST_ASYNC},
            {REPORT_PARAM_THU_INST_INPERSON, REPORT_PARAM_THU_INST_OLSYNC, REPORT_PARAM_THU_INST_ASYNC},
            {REPORT_PARAM_FRI_INST_INPERSON, REPORT_PARAM_FRI_INST_OLSYNC, REPORT_PARAM_FRI_INST_ASYNC}};

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;
    private Map<String, StudentScheduleSpan> m_stdMap = null;
    private String m_teacherOid = null;
    private String m_mtcOid = null;
    private Integer m_gradeLevel = null;
    private MasterSchedule m_section = null;
    private Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    protected X2Broker m_broker;
    protected PlainDate[] m_days = new PlainDate[5];
    protected Set<PlainDate> m_instructionalDays = new HashSet<PlainDate>();

    private Map<String, ReferenceCode> m_gradeLevels;
    private String m_fieldNumericGradeLevel;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return Person.REL_USER;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDescription(com.follett.fsc.core.k12.beans.X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return REPORT_DESCRIPTION_PREFIX + ((SisUser) bean).getNameView() + "_" + new PlainDate() + "_"
                + getSchool().getSchoolId();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailAddress(com.follett.fsc.core.k12.beans.Person)
     */
    @Override
    public String getEmailAddress(Person person) {
        return "";
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(com.follett.fsc.core.k12.beans.X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        Collection<Person> recipients = new ArrayList<>();
        Collection<SisStaff> schoolStaff = m_schoolToStaff.get(getSchool().getOid());

        for (SisStaff staff : schoolStaff) {
            recipients.add(staff.getPerson());
        }

        return recipients;
    }

    /**
     * Gather data.
     *
     * @return ReportDataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        boolean publishedIndicator = PublishReportsManager.PublishType.PUBLISH.toString()
                .equals(getParameter(PARAM_PUBLISHED_INDICATOR));
        if (m_isCertified && !publishedIndicator) {
            addCustomErrorMessage(
                    "You are attempting to certify the report without publishing it. Please be sure to publish the report when you certify it.");
            return new ReportDataGrid();
        } else if (!m_isCertified && publishedIndicator) {
            addCustomErrorMessage(
                    "You are attempting to publish the report without certifying it. Please certify the report whenever you attempt to publish.");
            return new ReportDataGrid();
        }

        if (PublishReportsManager.PublishType.PREVIEW.toString().equals(getParameter(PARAM_PUBLISHED_INDICATOR))) {
            addCustomErrorMessage(
                    "The Publish tab's Delivery Type of 'Preview' is disabled. To preview the report, please chose 'Preview' from the 'Teacher Certification' option on the 'General' tab and leave the Publish Delivery Type set to blank.");
            return new ReportDataGrid();
        }

        addParameter(REPORT_PARAM_SESSION_USERNAME, getUser().getNameView());
        addParameter(REPORT_PARAM_MONTH_FORMAT, new SimpleDateFormat("MM-yyyy"));
        addParameter(REPORT_PARAM_START_DATE, m_startDate);
        addParameter(REPORT_PARAM_END_DATE, m_endDate);
        addParameter(REPORT_PARAM_CURRENT_DATE, new PlainDate());
        addParameter(REPORT_PARAM_MST_DESC, m_section.getDescription());
        addParameter(REPORT_PARAM_MST_SECTION, m_section.getSectionNumber());
        addParameter(REPORT_PARAM_INSTRUCTIONAL_MIN, getInstructionalMinutes());
        addParameter(REPORT_PARAM_DISTRICT_ID, getOrganization().getFieldValueByAlias(ALIAS_DOE_DISTRICT_ID));
        addParameter(REPORT_PARAM_SCHOOL_ID,
                m_section.getSchedule().getSchool().getFieldValueByAlias(ALIAS_DOE_SCHOOL_ID));
        addReportParameters();
        ReportDataGrid grid = new ReportDataGrid();
        populateGrid(grid);
        grid.beforeTop();
        return grid;
    }

    // Override the publish cleanup date and end date to be the
    // DISTRICT_YEAR_END
    @SuppressWarnings("static-access")
    @Override
    protected void publishResults() throws Exception {
        super.publishResults();
        if (m_isCertified) {
            X2Criteria reportDeliveryJobCriteria = new X2Criteria();
            reportDeliveryJobCriteria.addEqualTo(ReportDeliveryJob.COL_USER_OID, getUser().getOid());
            reportDeliveryJobCriteria.addEqualTo(ReportDeliveryJob.COL_REPORT_OID, getJob().getTool().getOid());
            reportDeliveryJobCriteria.addEqualTo(ReportDeliveryJob.COL_VIEW_START_DATE,
                    getParameter(ReportDeliveryJob.COL_VIEW_START_DATE));

            QueryByCriteria query = new QueryByCriteria(ReportDeliveryJob.class, reportDeliveryJobCriteria);

            query.addOrderByDescending(ReportDeliveryJob.COL_OID);

            Collection<ReportDeliveryJob> rdjs = m_broker.getCollectionByQuery(query);

            PlainDate endDate = null;
            if (getCurrentContext() != null) {
                endDate = getCurrentContext().getEndDate();
            } else {
                endDate = getSchool().getCurrentContext().getEndDate();
            }

            for (ReportDeliveryJob rdj : rdjs) {
                if (!rdj.getCleanupDate().equals(endDate)) {
                    rdj.setCleanupDate(endDate);
                    rdj.setViewEndDate(endDate);
                }

                getBroker().saveBeanForced(rdj);
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_broker = getBroker();
        PlainDate inputDate = (PlainDate) getParameters().get(INPUT_PARAM_START_DATE);
        initializeEndDate(inputDate);

        if (m_startDate != null && m_endDate != null) {
            this.addParameter(REPORT_PARAM_START_DATE, m_startDate);
            this.addParameter(REPORT_PARAM_END_DATE, m_endDate);

        }

        setMondays();
        loadGradeLevelRefMap();

        ScheduleTeacher mtc = m_broker.getBeanByOid(ScheduleTeacher.class, m_mtcOid);
        m_section = mtc.getSection();

        ReferenceCode gradeLevelRefCode = m_gradeLevels.get(m_section.getSchoolCourse().getCourse().getGradeLevel());
        m_gradeLevel = new Integer((String) gradeLevelRefCode.getFieldValueByBeanPath(m_fieldNumericGradeLevel));

        loadInstructionalDays();

        m_stdMap = new HashMap<String, StudentScheduleSpan>();
        buildStudentScheduleMap();

        Integer certification = (Integer) getParameters().get(PARAM_CERTIFICATION);
        m_isCertified = CERTIFY.equals(certification.toString());

        loadSchoolToStaff();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaDataSource#saveState(com.follett.fsc.core.
     *      k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        if (userData.getSessionNavConfig().getApplicationContext().equals(ApplicationContext.STAFF)) {
            m_teacherOid = userData.getStaffOid();
        }
        ScheduleTeacher mtc = userData.getCurrentRecord(ScheduleTeacher.class);
        if (mtc != null) {
            m_mtcOid = mtc.getOid();
        }
        runOnApplicationServer();
    }

    /**
     * Set instructional method flags, in person not yet implemented 1 =
     * full day, 2 = partial, 0 means nothing This may vary by the day in
     * the future
     */
    private void addReportParameters() {
        if (m_gradeLevel != null) {
            if (SKL_ID_OPPORTUNITY
                    .equals(m_section.getSchedule().getSchool().getFieldValueByAlias(ALIAS_DOE_SCHOOL_ID))
                    || (m_gradeLevel >= -1 && m_gradeLevel <= 6)) {
                for (int i = 0; i < m_days.length; i++) {
                    // set non-instructional day flags
                    Boolean ind = Boolean.FALSE;
                    if (!m_instructionalDays.contains(m_days[i])) {
                        ind = Boolean.TRUE;
                    }
                    addParameter(nonSessionIndDays[i], ind);
                    addParameter(instDeliveryMethod[i][1], new Integer(2));
                    addParameter(instDeliveryMethod[i][2], new Integer(2));
                    if (i <= 1) {
                        addParameter(instDeliveryMethod[i][0], new Integer(2));
                    } else {
                        addParameter(instDeliveryMethod[i][0], new Integer(0));
                    }
                }
            } else if (m_gradeLevel == 7 || m_gradeLevel == 8) {
                String mstSchDisplay = m_section.getScheduleDisplay();
                String crsNumber = m_section.getSchoolCourse().getCourse().getNumber();
                if (!StringUtils.isEmpty(mstSchDisplay) && mstSchDisplay.length() > 1) {
                    String firstNumberOfSchDisplay = mstSchDisplay.substring(0, 2);
                    for (int i = 0; i < m_days.length; i++) {
                        // set non-instructional day flags
                        Boolean ind = Boolean.FALSE;
                        if (!m_instructionalDays.contains(m_days[i])) {
                            ind = Boolean.TRUE;
                        }
                        addParameter(nonSessionIndDays[i], ind);
                    }
                    if (!StringUtils.isEmpty(crsNumber)
                            && (crsNumber.startsWith(CRS_SD700) || crsNumber.startsWith(CRS_SD800))) {
                        for (int i = 0; i < m_days.length; i++) {
                            addParameter(instDeliveryMethod[i][0], new Integer(2));
                            addParameter(instDeliveryMethod[i][1], new Integer(2));
                            addParameter(instDeliveryMethod[i][2], new Integer(2));
                        }
                    } else {
                        if (firstNumberOfSchDisplay.contains("1") || firstNumberOfSchDisplay.contains("2")
                                || firstNumberOfSchDisplay.contains("3")) {
                            addParameter(instDeliveryMethod[0][0], new Integer(2));
                            addParameter(instDeliveryMethod[0][1], new Integer(2));
                            addParameter(instDeliveryMethod[0][2], new Integer(2));
                            addParameter(instDeliveryMethod[3][0], new Integer(2));
                            addParameter(instDeliveryMethod[3][1], new Integer(2));
                            addParameter(instDeliveryMethod[3][2], new Integer(2));
                        }
                        if (firstNumberOfSchDisplay.contains("4") || firstNumberOfSchDisplay.contains("5")
                                || firstNumberOfSchDisplay.contains("6")) {
                            addParameter(instDeliveryMethod[1][0], new Integer(2));
                            addParameter(instDeliveryMethod[1][1], new Integer(2));
                            addParameter(instDeliveryMethod[1][2], new Integer(2));
                            addParameter(instDeliveryMethod[4][0], new Integer(2));
                            addParameter(instDeliveryMethod[4][1], new Integer(2));
                            addParameter(instDeliveryMethod[4][2], new Integer(2));
                        }
                        if (firstNumberOfSchDisplay.contains("7") || firstNumberOfSchDisplay.contains("HR")) {
                            addParameter(instDeliveryMethod[2][1], new Integer(2));
                            addParameter(instDeliveryMethod[2][2], new Integer(2));
                        }
                    }
                }
            }
        }
    }

    /**
     * Builds student schedule map.
     *
     * @param studentMap Map<String,Set<MasterSchedule>>
     */
    private void buildStudentScheduleMap() {
        Map<String, List<StudentSchedule>> schedules = getStudentSchedules();
        Map<String, List<StudentScheduleChange>> changes = getStudentScheduleChanges();
        Set<String> students = new HashSet();
        students.addAll(schedules.keySet());
        students.addAll(changes.keySet());
        for (String student : students) {
            for (StudentScheduleSpan span : getStudentScheduleSpans(schedules.get(student), changes.get(student))) {
                if (!m_endDate.before(span.getEntryDate()) && !m_startDate.after(span.getExitDate())) {
                    m_stdMap.put(student, span);
                }
            }
        }
    }

    /**
     * Search through schedule change records for alternate start dates and
     * other sections to report.
     *
     * Loop through schedule change records for each section, in date order. Add
     * section spans based on change drop or add record.s
     *
     * Some will have been dropped after the end of a term but should still be
     * counted.
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
         * Work backward in time through schedule changes. DROP will open a new
         * section and the ADD before it will finish that section. A DROP
         * without a following ADD will be considered open at start of term. Any
         * activity entirely before start of term will be ignored.
         */
        for (StudentScheduleChange change : scheduleChanges) {
            // Check for a new section.
            if (lastSection == null || !lastSection.getOid().equals(change.getMasterScheduleOid())) {
                // Save the working section if necessary.
                if (lastChange != null) {
                    // The last change record for this section (in reverse
                    // chronological order)
                    // was a drop. Assume the section was scheduled from the
                    // beginning of the
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
                    // before the start of it's term. This is just scheduling
                    // activity.
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
                // If a term is missing any dates, use school schedule dates or
                // district calendar
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

            // For a section, see if its dates compare with report dates or term
            // dates.
            if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                lastChange = change;
            } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                if (lastChange == null) {
                    // No previous record, assume current student schedule. Find
                    // based on master
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
                    // Avoid entering a change date that is after the term end
                    // date
                    if (info.getExitDate().after(termEnd)) {
                        info.setExitDate(termEnd);
                    }
                    info.setExitChange(lastChange);
                    // Avoid recording sections scheduled out entirely
                    // before the start of it's term. This is just scheduling
                    // activity.
                    if (!info.getExitDate().before(termStart)) {
                        scheduleSpanMap.put(change.getOid(), info);
                    }
                }
                lastChange = null;
            }
        }
        if (lastChange != null) {
            // The last change record for this section (in reverse chronological
            // order)
            // was a drop. Assume the section was scheduled from the beginning
            // of the term/year.
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
     * For all populated sections in the schedule span map, if the entry date or
     * exit date is missing, populate with term dates.
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
             * If the entry/exit dates are out of order, remove the info. This
             * can be caused by drop/re-add after the end of term. The original
             * entry will exist before the drop, so this record is extra.
             */
            if (info.getExitDate() != null && info.getEntryDate() != null
                    && info.getExitDate().before(info.getEntryDate())) {
                iterator.remove();
            }
        }
    }

    /**
     * Gets the filtered attendances for grade 7-8 by days.
     *
     * @param attendancesToFilter Collection<Attendance>
     * @param isMonThur boolean
     * @param isTueFr boolean
     * @param isWed boolean
     * @return Collection of Attendance
     */
    private Collection<Attendance> getFilteredAttendances(Collection<Attendance> attendancesToFilter,
                                                          boolean isMonThur,
                                                          boolean isTueFr,
                                                          boolean isWed) {
        Calendar cal = Calendar.getInstance();
        Collection<Attendance> filteredAtt = new ArrayList<Attendance>();
        for (Attendance attToFilter : attendancesToFilter) {
            cal.setTime(attToFilter.getDate());
            int day = cal.get(Calendar.DAY_OF_WEEK);
            if ((day == Calendar.MONDAY || day == Calendar.THURSDAY) && isMonThur) {
                filteredAtt.add(attToFilter);
            } else if ((day == Calendar.TUESDAY || day == Calendar.FRIDAY) && isTueFr) {
                filteredAtt.add(attToFilter);
            } else if (day == Calendar.WEDNESDAY && isWed) {
                filteredAtt.add(attToFilter);
            }
        }
        return filteredAtt;
    }

    /**
     * Gets the instructional minutes.
     *
     * @return String
     */
    private String getInstructionalMinutes() {
        String minutes = "240";
        String gradeLevel = m_section.getSchoolCourse().getCourse().getGradeLevel();
        if (!StringUtils.isEmpty(gradeLevel)) {
            Integer grade;
            if (gradeLevel.contains("TK")) {
                grade = new Integer(-1);
            } else {
                grade = Integer.parseInt(gradeLevel.trim());
            }
            if (grade != null) {
                if (grade < 1) {
                    minutes = "180";
                } else if (grade < 4) {
                    minutes = "230";
                } else {
                    minutes = "240";
                }
            }
        }

        return minutes;
    }

    /**
     * Construct an initial criteria for loading student schedule change
     * records. This should match the student criteria used for the student
     * query as closely as possible.
     *
     * @return X2Criteria
     */
    private Map<String, List<StudentScheduleChange>> getStudentScheduleChanges() {
        X2Criteria studentScheduleChangeCriteria = new X2Criteria();

        // From Class type section
        studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        studentScheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER
                + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentScheduleChange.COL_SCHEDULE_OID);

        // section term started before report date.
        // Require section term to start before end/report date.
        studentScheduleChangeCriteria.addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES
                + PATH_DELIMITER + ScheduleTermDate.COL_START_DATE, m_endDate);

        studentScheduleChangeCriteria.addGreaterOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES
                + PATH_DELIMITER + ScheduleTermDate.COL_END_DATE, m_startDate);

        studentScheduleChangeCriteria.addEqualTo(
                StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());

        studentScheduleChangeCriteria.addEqualTo(
                StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + X2BaseBean.COL_OID, m_section.getOid());

        studentScheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, studentScheduleChangeCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, false);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
        return getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 32);
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

        // Fill in any empty entry/exit dates with term dates for the section
        // term.
        fillTermDates(scheduleSpanMap);

        List<StudentScheduleSpan> scheduleSpanList = new ArrayList<StudentScheduleSpan>(scheduleSpanMap.values());
        return scheduleSpanList;
    }

    /**
     * Construct an initial criteria for loading student schedule records. This
     * should match the student criteria used for the student query as closely
     * as possible.
     *
     * @return X2Criteria
     */
    private Map<String, List<StudentSchedule>> getStudentSchedules() {

        X2Criteria studentScheduleCriteria = new X2Criteria();

        // Master type Class
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);

        studentScheduleCriteria.addLessOrEqualThan(
                StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER + ScheduleTermDate.COL_START_DATE,
                m_endDate);

        studentScheduleCriteria.addGreaterOrEqualThan(
                StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER + ScheduleTermDate.COL_END_DATE,
                m_startDate);

        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                getSchool().getOid());

        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + X2BaseBean.COL_OID,
                m_section.getOid());

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
        query.addOrderBy(StudentSchedule.COL_STUDENT_OID, true);
        query.addOrderBy(StudentSchedule.COL_SECTION_OID, true);

        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 32);
    }

    /**
     * Load the schedule term dates for a schedule term oid. Keep a map of
     * existing codes for lookup.
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
     * Load instructional days.
     */
    private void loadInstructionalDays() {
        Map<String, List<SchoolCalendarDate>> calDates = CalendarManager
                .getSchoolCalendarDays(m_section.getSchedule().getSchool(), m_startDate, m_endDate, m_broker, true);
        for (List<SchoolCalendarDate> dates : calDates.values()) {
            for (SchoolCalendarDate date : dates) {
                m_instructionalDays.add(date.getDate());
            }
        }
    }

    /**
     * Load grade level ref map.
     */
    private void loadGradeLevelRefMap() {
        ReferenceTable gradeLevelRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_GRADE_LEVEL);
        m_gradeLevels = gradeLevelRefTable.getCodeMap();

        ExtendedDataDictionary gradeLevelDdx = gradeLevelRefTable.getExtendedDataDictionary();
        DataDictionary gradeLevelDdxDictionary = DataDictionary.getDistrictDictionary(gradeLevelDdx,
                getBroker().getPersistenceKey());
        DataDictionaryField field = gradeLevelDdxDictionary.findDataDictionaryFieldByAlias("NumericGradeLevel");
        m_fieldNumericGradeLevel = field.getJavaName();
    }

    /**
     * @param certification
     */
    private void loadSchoolToStaff() {
        if (m_isCertified) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(((SisUser) getUser()).getPersistenceKey());
            DataDictionaryField certificationField = dictionary
                    .findDataDictionaryFieldByAlias(ALIAS_RECEIVE_ATTENDANCE_CERTIFICATION);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(certificationField.getJavaName(), Boolean.TRUE);

            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addEqualTo(X2BaseBean.COL_OID, m_teacherOid);
            criteria.addOrCriteria(orCriteria);
            BeanQuery query = new BeanQuery(SisStaff.class, criteria);

            m_schoolToStaff = m_broker.getGroupedCollectionByQuery(query, SisStaff.COL_SCHOOL_OID, 1);

        }
    }

    /**
     * Populate data grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateGrid(ReportDataGrid grid) {
        for (String stdOid : m_stdMap.keySet()) {
            Calendar cal = Calendar.getInstance();
            Student student = m_broker.getBeanByOid(Student.class, stdOid);
            X2Criteria attCriteria = new X2Criteria();
            QueryByCriteria query = null;
            attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, stdOid);
            attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
            attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);
            query = new QueryByCriteria(StudentAttendance.class, attCriteria);
            Collection<Attendance> attendance = m_broker.getCollectionByQuery(query);
            if (m_gradeLevel != null && (m_gradeLevel == 7 || m_gradeLevel == 8)) {
                X2Criteria patCriteria = new X2Criteria();
                patCriteria.addEqualTo(StudentPeriodAttendance.COL_STUDENT_OID, stdOid);
                patCriteria.addEqualTo(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID,
                        m_section.getOid());
                patCriteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, m_startDate);
                patCriteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, m_endDate);
                query = new QueryByCriteria(StudentPeriodAttendance.class, patCriteria);
                attendance.addAll(m_broker.getCollectionByQuery(query));
            }
            boolean isMonThur = false;
            boolean isTueFr = false;
            boolean isWed = false;
            grid.append();
            if (SKL_ID_OPPORTUNITY.equals(m_section.getSchedule().getSchool().getFieldValueByAlias(ALIAS_DOE_SCHOOL_ID))
                    || (m_gradeLevel >= -1 && m_gradeLevel <= 6)) {
                grid.set(FIELD_ATT_MONDAY, "200");
                grid.set(FIELD_ATT_TUESDAY, "200");
                grid.set(FIELD_ATT_WEDNESDAY, "200");
                grid.set(FIELD_ATT_THURSDAY, "200");
                grid.set(FIELD_ATT_FRIDAY, "200");
            } else if (m_gradeLevel == 7 || m_gradeLevel == 8) {
                String crsNumber = m_section.getSchoolCourse().getCourse().getNumber();
                if (!StringUtils.isEmpty(crsNumber)
                        && (crsNumber.startsWith(CRS_SD700) || crsNumber.startsWith(CRS_SD800))) {
                    grid.set(FIELD_ATT_MONDAY, "200");
                    grid.set(FIELD_ATT_TUESDAY, "200");
                    grid.set(FIELD_ATT_WEDNESDAY, "200");
                    grid.set(FIELD_ATT_THURSDAY, "200");
                    grid.set(FIELD_ATT_FRIDAY, "200");
                } else {
                    String mstSchDisplay = m_section.getScheduleDisplay();
                    if (!StringUtils.isEmpty(mstSchDisplay) && mstSchDisplay.length() > 1) {
                        String firstNumberOfSchDisplay = mstSchDisplay.substring(0, 2);
                        if (firstNumberOfSchDisplay.contains("1") || firstNumberOfSchDisplay.contains("2")
                                || firstNumberOfSchDisplay.contains("3")) {
                            grid.set(FIELD_ATT_MONDAY, "200");
                            grid.set(FIELD_ATT_THURSDAY, "200");
                            isMonThur = true;
                        }
                        if (firstNumberOfSchDisplay.contains("4") || firstNumberOfSchDisplay.contains("5")
                                || firstNumberOfSchDisplay.startsWith("6")) {
                            grid.set(FIELD_ATT_TUESDAY, "200");
                            grid.set(FIELD_ATT_FRIDAY, "200");
                            isTueFr = true;
                        }
                        if (firstNumberOfSchDisplay.contains("7") || firstNumberOfSchDisplay.contains("HR")) {
                            grid.set(FIELD_ATT_WEDNESDAY, "200");
                            isWed = true;
                        }
                    }
                    attendance = getFilteredAttendances(attendance, isMonThur, isTueFr, isWed);
                }
            }
            for (Attendance att : attendance) {
                cal.setTime(att.getDate());
                int day = cal.get(Calendar.DAY_OF_WEEK);
                String otherCode = att.getOtherCode();
                String otherCode2 = att.getOtherCode02();
                Boolean absent = att.getAbsentIndicator();
                String value = "";
                if ("500".equals(otherCode)) {
                    value = "500";
                } else if ("ASY".equals(otherCode)) {
                    value = "400";
                    if ("SYN".equals(otherCode2)) {
                        value = "300+400";
                    }
                } else if ("SYN".equals(otherCode)) {
                    value = "300";
                    if ("ASY".equals(otherCode2)) {
                        value = "300+400";
                    }
                } else if ("ASY+SYN".equals(otherCode)) {
                    value = "300+400";
                } else if (att.getTardyIndicator() || att.getDismissedIndicator()) {
                    value = "200";
                } else if (absent) {
                    value = "100";
                }
                setDayIndicators(grid, day, value);
            }
            // blank out report checkboxes for days a student wasn't in the
            // class
            // (midweek add, midweek drop)
            StudentScheduleSpan span = m_stdMap.get(stdOid);
            for (PlainDate date : m_days) {
                if (date.before(span.getEntryDate()) || date.after(span.getExitDate())
                        || !m_instructionalDays.contains(date)) {
                    cal.setTime(date);
                    int day = cal.get(Calendar.DAY_OF_WEEK);
                    setDayIndicators(grid, day, "");
                }
            }

            grid.set(FIELD_STUDENT_NAME, student.getNameView());
            grid.set(FIELD_USER, getUser());
        }

        grid.sort(Arrays.asList(new String[] {FIELD_STUDENT_NAME}), false);
    }

    /**
     * Sets the day indicators.
     *
     * @param grid ReportDataGrid
     * @param day int
     * @param value String
     */
    private void setDayIndicators(ReportDataGrid grid, int day, String value) {
        switch (day) {
            case Calendar.MONDAY:
                grid.set(FIELD_ATT_MONDAY, value);
                break;
            case Calendar.TUESDAY:
                grid.set(FIELD_ATT_TUESDAY, value);
                break;
            case Calendar.WEDNESDAY:
                grid.set(FIELD_ATT_WEDNESDAY, value);
                break;
            case Calendar.THURSDAY:
                grid.set(FIELD_ATT_THURSDAY, value);
                break;
            case Calendar.FRIDAY:
                grid.set(FIELD_ATT_FRIDAY, value);
                break;
        }
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
