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

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Helper class to get proper sections.
 *
 * @author Follett Software Company
 *
 */
public class TNClassSectionHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Comparator to sort sections by MasterSchedule.courseView
     *
     * @author Follett Software Company
     *
     */
    protected class SectionByCourseViewComparator implements Comparator<MasterSchedule> {

        /**
         * Compare.
         *
         * @param o1 MasterSchedule
         * @param o2 MasterSchedule
         * @return int
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(MasterSchedule o1, MasterSchedule o2) {
            int icmp = o2.getCourseView().compareTo(o1.getCourseView());
            if (icmp == 0) {
                icmp = o2.getOid().compareTo(o1.getOid());
            }
            return icmp;
        }
    }

    /**
     * Simple Container that allow null values.
     */
    protected class TNScheduleSpan {
        private PlainDate m_endDate;
        private String m_instrPgm;
        private PlainDate m_startDate;
        private MasterSchedule m_masterSchedule;
        private String m_nameView;
        private SisSchool m_schoolOverride;
        private Object m_outsideIep;

        /**
         * Constructor.
         *
         * @param scheduleSpan TNStudentScheduleSpan
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param school SisSchool
         * @param beanPathOutsideIep String
         * @param instrPgm String
         */
        TNScheduleSpan(TNStudentScheduleSpan scheduleSpan, PlainDate startDate, PlainDate endDate, SisSchool school,
                String beanPathOutsideIep, String instrPgm) {
            m_instrPgm = instrPgm;
            m_startDate = startDate;
            m_endDate = endDate;
            m_masterSchedule = scheduleSpan.getSection();
            m_schoolOverride = school;

            if (scheduleSpan.getSchedule() != null) {
                m_outsideIep = scheduleSpan.getSchedule().getFieldValueByBeanPath(beanPathOutsideIep);
            }
            if (scheduleSpan.getSchedule() != null && scheduleSpan.getSchedule().getStudent() != null) {
                m_nameView = scheduleSpan.getSchedule().getStudent().getNameView();
            }
        }

        /**
         * Gets the course view.
         *
         * @return courseView from MST
         */
        public String getCourseView() {
            return m_masterSchedule.getCourseView();
        }

        /**
         * Gets the end date.
         *
         * @return the m_endDate
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the instr pgm.
         *
         * @return the m_instrPgm
         */
        public String getInstrPgm() {
            return m_instrPgm;
        }

        /**
         * Returns key when key is school oid concatenated with mst oid and with instr program.
         *
         * @return String
         */
        public String getKey() {
            return getSchool().getOid() + m_masterSchedule.getOid() + m_instrPgm;
        }

        /**
         * Returns nameView for student generating this record.
         *
         * @return String
         */
        public String getNameView() {
            return m_nameView;
        }

        /**
         * Gets the outside iep.
         *
         * @return m_outsideIep
         */
        public Object getOutsideIep() {
            return m_outsideIep;
        }

        /**
         * Returns current school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getSchool() == null ? null : getSchool().getOid();
        }

        /**
         * Returns MST.
         *
         * @return MasterSchedule
         */
        public MasterSchedule getSection() {
            return m_masterSchedule;
        }

        /**
         * Gets the start date.
         *
         * @return the m_startDate
         */
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Returns shools's state id by giving bean path.
         *
         * @param beanPath String
         * @return Object
         */
        public Object getStateSchoolId(String beanPath) {
            Object value = null;
            if (getSchool() != null) {
                value = getSchool().getFieldValueByBeanPath(beanPath);
            }
            return value;
        }

        /**
         * Returns term view getting from MST.
         *
         * @return String
         */
        public String getTermView() {
            return m_masterSchedule.getTermView();
        }

        /**
         * Set end date.
         *
         * @param endDate PlainDate
         */
        public void setEndDate(PlainDate endDate) {
            m_endDate = endDate;
        }

        /**
         * Returns SisSchool.
         *
         * @return Sis school
         */
        private SisSchool getSchool() {
            SisSchool value = null;
            if (m_schoolOverride != null) {
                value = m_schoolOverride;
            } else if (m_masterSchedule != null && m_masterSchedule.getSchoolCourse() != null) {
                value = m_masterSchedule.getSchoolCourse().getSchool();
            }
            return value;
        }
    }

    public static final String INPUT_PARAM_ENROLLMENT_HELPER = "enrollmentHelper";
    public static final String INPUT_PARAM_BYPASS_DUP_SECT_TEST = "bypassDupSectionTest";
    public static final String INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY = "loadStudentSectionsOnly";

    private static final String ALIAS_ENR_CAL_CODE = "all-enr-StudentCalendar";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_SCC = "all-scc-ExcludeFromStateReporting";
    private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_OUTSIDEIEP = "DOE VOCATIONAL OUTSIDE IEP";

    protected static final String CALENDAR_ID_STANDARD = "Standard";

    /**
     * Input parameters.
     */
    private static final String INPUT_PARAM_CRS_SECTION = "courseSections";
    private static final String INPUT_PARAM_CSK_OID = "schoolOidCourse";
    private static final String INPUT_PARAM_STF_OIDS = "staffsOids";

    private static final String PARAM_QUERY_BY_FIELD = "queryBy";
    private static final String PARAM_QUERY_BY_CRITERIA = "queryString";
    /**
     * Class members
     */
    public Map<String, String> m_defaultInstProgMap = new HashMap<String, String>();
    public Map<String, String> m_instProgMap = new HashMap<String, String>();
    public Map<String, String> m_sectionToCalendarCode = new HashMap<String, String>();

    private Set<MasterSchedule> m_allReportableSections;
    private boolean m_bypassDuplicateSectionTest;
    private TNStateReportData m_data;
    private TNEnrollmentHelper m_enrollmentHelper;
    private String m_fieldExcludeCrs;
    private String m_fieldExcludeScc;
    private String m_fieldExcludeStd;
    private String m_fieldOutsideIEP;
    private TNStudentMultiYearHelper m_multiYearHelper;
    private String m_paramCourseSections;
    private Set<String> m_paramSchoolOidsCourse = new HashSet();
    private Boolean m_paramWholeSchool;
    private ScheduleManager m_scheduleMgr;
    private School m_school;
    private Map<String, Map<String, MasterSchedule>> m_schoolAllSections = new HashMap();
    private Map<String, Set<MasterSchedule>> m_schoolReportableSections = new HashMap();
    private Map<String, ArrayList<SisSchool>> m_schoolsByReportableSection;
    private Map<String, Collection<StudentSchool>> m_secondarySchools;
    private Collection<Collection<String>> m_sectionsOidLists = null;
    private TNStudentHistoryHelper m_studentHelper;
    private Map<String, List<TNScheduleSpan>> m_studentSchedules = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    /**
     * Constructor.
     *
     * @param data TNStateReportData
     * @param school School
     * @param applyStdStandardSelection is used to determine if run section or student based
     *        reports.
     * @param stdOids Collection<String>
     * @param bypassDuplicateSectionTest boolean
     */
    public TNClassSectionHelper(TNStateReportData data, School school, boolean applyStdStandardSelection,
            Collection<String> stdOids, boolean bypassDuplicateSectionTest) {
        m_data = data;
        m_bypassDuplicateSectionTest = bypassDuplicateSectionTest;
        if (m_data.getParameter(INPUT_PARAM_BYPASS_DUP_SECT_TEST) != null
                && m_data.getParameter(INPUT_PARAM_BYPASS_DUP_SECT_TEST) instanceof Boolean) {
            m_bypassDuplicateSectionTest =
                    ((Boolean) m_data.getParameter(INPUT_PARAM_BYPASS_DUP_SECT_TEST)).booleanValue();
        }
        m_fieldExcludeCrs = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
        m_fieldExcludeStd = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);
        m_fieldOutsideIEP = m_data.translateAliasToJavaName(ALIAS_OUTSIDEIEP, true);
        m_fieldExcludeScc = m_data.translateAliasToJavaName(ALIAS_EXCLUDE_SCC, false);
        m_school = school;
        if (m_data.getParameter(INPUT_PARAM_ENROLLMENT_HELPER) != null) {
            m_enrollmentHelper = (TNEnrollmentHelper) m_data.getParameter(INPUT_PARAM_ENROLLMENT_HELPER);
        } else {
            m_enrollmentHelper = new TNEnrollmentHelper(data);
        }
        m_studentHelper = m_enrollmentHelper.getStudentHistoryHelper();
        m_multiYearHelper = m_enrollmentHelper.getStudentMultiYearHelper();

        if (m_data.getSetupErrors().size() == 0) {
            m_paramCourseSections = (String) m_data.getParameter(INPUT_PARAM_CRS_SECTION);
            if (m_paramCourseSections != null && StringUtils.isEmpty(m_paramCourseSections)) {
                m_paramWholeSchool = Boolean.TRUE;
            }
            if (!applyStdStandardSelection) {
                if (!StringUtils.isEmpty((String) m_data.getParameter(INPUT_PARAM_CSK_OID))) {
                    m_paramSchoolOidsCourse
                            .addAll(Arrays.asList(((String) m_data.getParameter(INPUT_PARAM_CSK_OID)).split(",")));
                }
                boolean loadStudentSectionsOnly = false;
                if (m_data.getParameter(INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY) != null
                        && m_data.getParameter(INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY) instanceof Boolean) {
                    loadStudentSectionsOnly =
                            ((Boolean) m_data.getParameter(INPUT_PARAM_BYPASS_DUP_SECT_TEST)).booleanValue();
                }
                if (!loadStudentSectionsOnly) {
                    loadReportableSections();
                }
            }
            if (m_data.getBeanClass() == null || !m_data.getBeanClass().equals(ScheduleTeacher.class)) {
                if (m_data.getParameter(INPUT_PARAM_ENROLLMENT_HELPER) == null) {
                    prepareStudentSectionSelection(applyStdStandardSelection, stdOids);
                }
                loadSecondarySchools(applyStdStandardSelection);
                loadStudents();
            }
        }
    }

    /**
     * Adds map of all sections of the school to the map of all possible sections of all selected
     * schools and returns it.
     *
     * @return Collection
     */
    public Collection<MasterSchedule> getReportableSections() {
        if (m_allReportableSections == null) {
            m_allReportableSections = new TreeSet(new SectionByCourseViewComparator());
            for (Entry<String, Set<MasterSchedule>> entry : m_schoolReportableSections.entrySet()) {
                m_allReportableSections.addAll(entry.getValue());
            }
        }
        return m_allReportableSections;
    }

    /**
     * Gets the reportable sections oid lists.
     *
     * @return Collection
     */
    public Collection<Collection<String>> getReportableSectionsOidLists() {
        if (m_sectionsOidLists == null) {
            m_sectionsOidLists = new LinkedList();
            Collection<String> sectionOids = null;
            int index = 0;
            for (MasterSchedule mst : getReportableSections()) {
                if (index % 1000 == 0) {
                    sectionOids = new ArrayList(1000);
                    m_sectionsOidLists.add(sectionOids);
                }
                ++index;
                sectionOids.add(mst.getOid());
            }
        }
        return m_sectionsOidLists;
    }

    /**
     * Returns set of needed MSTs for given school.
     *
     * @param schoolOid String
     * @return Set of MasterSchedule
     */
    public Set<MasterSchedule> getReportableSectionForSchool(String schoolOid) {
        Set<MasterSchedule> values = m_schoolReportableSections.get(schoolOid);
        return values == null ? new HashSet() : values;
    }

    /**
     * Retrieve the most common calendar code for a section.
     *
     * @param section MasterSchedule
     * @return String
     */
    public String getSectionCalendarCode(MasterSchedule section) {
        String calendarCode;
        if (m_sectionToCalendarCode.containsKey(section.getOid())) {
            calendarCode = m_sectionToCalendarCode.get(section.getOid());
        } else {
            if (m_scheduleMgr == null) {
                m_scheduleMgr = new ScheduleManager(m_data.getBroker());
            }
            calendarCode = m_scheduleMgr.getMostCommonCalendar(section.getSchedule(), null);
        }
        return calendarCode;
    }

    /**
     * get the schoolOidCourse parameter array.
     *
     * @return Sets the
     */
    public Set<String> getSchoolOidsCourse() {
        return m_paramSchoolOidsCourse;
    }

    /**
     * Gets the student enrollment spans.
     *
     * @param student SisStudent
     * @return List
     */
    public List<TNStudentEnrollmentSpan> getStudentEnrollmentSpans(SisStudent student) {
        return m_studentHelper.getTNStudentEnrollmentSpans(student, true);
    }

    /**
     * Returns set of needed Schools for given section.
     *
     * @param mstOid String
     * @return List of SisSchool
     */
    public ArrayList<SisSchool> getSchoolsForSection(String mstOid) {
        if (m_schoolsByReportableSection == null) {
            loadSchoolsBySection();
        }

        ArrayList<SisSchool> values = m_schoolsByReportableSection.get(mstOid);

        return values == null ? new ArrayList() : values;
    }

    /**
     * Returns list of TNScheduleSpan (the same as schedules) form map by student oid.
     *
     * @param stdOid String
     * @return List of TNScheduleSpan
     */
    public List<TNScheduleSpan> getStudentSchedules(String stdOid) {
        List<TNScheduleSpan> values = m_studentSchedules.get(stdOid);
        return values == null ? new ArrayList<TNClassSectionHelper.TNScheduleSpan>() : values;
    }

    /**
     * Get student query by criteria from student helper.
     *
     * @param distinct boolean
     * @return QueryByCriteria
     */
    public QueryByCriteria getStudentQuery(boolean distinct) {
        return m_studentHelper.getStudentQuery(distinct);
    }

    /**
     * Load the default school based instructional program map.
     */
    public void loadDefaultInstProgMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(m_multiYearHelper.getActiveStudentCriteria());
        criteria.addAndCriteria(m_multiYearHelper.getNotEmptyCalendarCriteria());

        String[] columns = new String[] {m_multiYearHelper.getSchoolOidField(),
                m_multiYearHelper.getCalendarCodeField(), "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(m_multiYearHelper.getSchoolOidField());
        query.addGroupBy(m_multiYearHelper.getCalendarCodeField());
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                String key = schoolOid;

                if (!m_defaultInstProgMap.containsKey(key)) {
                    String prgmKey =
                            TNStateReportData.makeCalendarLookupKey(m_data.m_contextOid, schoolOid, calendarCode);
                    String prgm = m_data.m_calendarOids.get(prgmKey);
                    m_defaultInstProgMap.put(key, prgm);
                }
            }
        } finally {
            iterator.close();
        }

        // Add any additional schools without students, using first calendar
        for (Entry<String, String> entry : m_data.m_calendarOids.entrySet()) {
            String[] keys = entry.getKey().split(TNStateReportData.KEY_DELIMITER);
            String sklOid = keys[1];
            if (!m_defaultInstProgMap.containsKey(sklOid)) {
                m_defaultInstProgMap.put(sklOid, entry.getValue());
            }
        }
    }

    /**
     * Build maps of instructional program numbers.
     */
    public void loadInstProgMap() {
        for (Collection<String> list : getReportableSectionsOidLists()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addAndCriteria(m_multiYearHelper.getNotEmptyCalendarCriteria(StudentSchedule.REL_STUDENT));
            criteria.addIn(StudentSchedule.COL_SECTION_OID, list);

            String[] columns = new String[] {
                    StudentSchedule.COL_SECTION_OID,
                    StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                            + m_multiYearHelper.getCalendarCodeField(),
                    StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_SCHOOL_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, criteria);
            query.addGroupBy(StudentSchedule.COL_SECTION_OID);
            query.addGroupBy(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + m_multiYearHelper.getCalendarCodeField());
            query.addGroupBy(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_SCHOOL_OID);
            query.addOrderByDescending("count(" + X2BaseBean.COL_OID + ")");

            ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String sectionOid = (String) row[0];
                    String calendarCode = (String) row[1];
                    String schoolOid = (String) row[2];

                    String key = sectionOid;

                    if (!m_instProgMap.containsKey(key)) {
                        String prgmKey =
                                TNStateReportData.makeCalendarLookupKey(m_data.m_contextOid, schoolOid, calendarCode);
                        String prgm = m_data.m_calendarOids.get(prgmKey);
                        if (prgm != null && calendarCode != null) {
                            m_instProgMap.put(key, prgm);
                        }
                    }

                    if (!m_sectionToCalendarCode.containsKey(key)) {
                        m_sectionToCalendarCode.put(key, calendarCode);
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Adds span using method @see
     * com.x2dev.procedures.statereporting.tn.TNClassSectionHelper#loadReportableSection(String,
     * MasterSchedule)
     *
     * @param span TNScheduleSpan
     */
    private void addSpan(TNScheduleSpan span) {
        loadReportableSection(span.getSchoolOid(), span.getSection());
    }

    /**
     * Adds span to the map of the sections of the school if it is no duplicated.
     *
     * @param spans List<TNScheduleSpan>
     */
    private void addSpanToSections(List<TNScheduleSpan> spans) {
        for (TNScheduleSpan span : spans) {
            if (m_bypassDuplicateSectionTest
                    || !isDuplicateSpan(span) && m_paramWholeSchool == null) {
                addSpan(span);
            }
        }
    }

    /**
     * Check if previous TNScheduleSpan end date is the same as next TNScheduleSpan start date.</br>
     * If dates are adjacent we will return true.
     *
     * @param priorDate PlainDate
     * @param nextDate PlainDate
     * @return true, if successful
     */
    private boolean datesAdjacent(PlainDate priorDate, PlainDate nextDate) {
        if (priorDate.before(nextDate)) {
            Calendar cal = Calendar.getInstance();
            cal.setTime(priorDate);
            cal.add(Calendar.DATE, 1);
            priorDate = new PlainDate(cal.getTime());
        }
        return !priorDate.before(nextDate);
    }

    /**
     * Gets map of MST keyed on Course View for the given school.
     *
     * @param schoolOid String
     * @return Map
     */
    private Map<String, MasterSchedule> getAllSections(String schoolOid) {
        Map<String, MasterSchedule> map = m_schoolAllSections.get(schoolOid);
        if (map == null) {
            map = new HashMap();
            QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, getMasterScheduleCriteria(schoolOid));
            QueryIterator iterator = m_data.getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    MasterSchedule section = (MasterSchedule) iterator.next();
                    if (map.containsKey(section.getCourseView())) {
                        m_data.addSetupError("Duplicate Course View Found",
                                "School: " + section.getSchedule().getSchool().getName() +
                                        " Course View: " + section.getCourseView() + " [" + section.getOid() + ","
                                        + map.get(section.getCourseView()).getOid() + "]");
                    } else {
                        map.put(section.getCourseView(), section);
                    }
                }
            } finally {
                iterator.close();
            }
            m_schoolAllSections.put(schoolOid, map);
        }
        return map;
    }

    /**
     * Returns instr program by student calendar and span.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return String
     */
    private String getInstProgram(SisStudent student, TNStudentEnrollmentSpan span) {
        String calendarCode = null;
        if (span.getFirstInactiveEnrollment() != null) {
            calendarCode = (String) span.getFirstInactiveEnrollment().getFieldValueByAlias(ALIAS_ENR_CAL_CODE);
        }

        // If there is no calendar, use student's calendar.
        if (StringUtils.isEmpty(calendarCode)) {
            calendarCode = (String) m_enrollmentHelper.getStudentValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE);
            calendarCode = !StringUtils.isEmpty(calendarCode) ? calendarCode : CALENDAR_ID_STANDARD;
        }

        String calendarKey = TNStateReportData.makeCalendarLookupKey(m_data.m_contextOid,
                span.getFirstActiveEnrollment().getSchoolOid(), calendarCode);

        if (m_data.m_calendarOids == null) {
            m_data.getCalendarsForContextOid(m_data.m_contextOid);
        }

        return m_data.m_calendarOids.get(calendarKey);
    }

    /**
     * Function for building custom MasterSchedule criteria.
     *
     * @param schoolOid String
     * @return X2Criteria
     */
    private X2Criteria getMasterScheduleCriteria(String schoolOid) {

        X2Criteria masterScheduleCriteria = new X2Criteria();

        // Filter by current year context
        masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID,
                m_data.m_contextOid);

        // Include Active Schedules only
        // All queries using REL_ACTIVE_SCHOOL_SCHED can only be used when the selected context is
        // the current context.
        // If this is not the case, Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS must be used.
        if (m_data.m_contextOid.equals(m_data.getOrganization().getCurrentContextOid())) {
            masterScheduleCriteria.addEqualToField(MasterSchedule.REL_SCHEDULE +
                    ModelProperty.PATH_DELIMITER +
                    Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    MasterSchedule.COL_SCHEDULE_OID);
        } else {
            masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_data.m_contextOid);
        }

        // Must include state course code
        masterScheduleCriteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                PATH_DELIMITER + m_data.m_fieldStateCourseCode, m_data.getBroker().getPersistenceKey());

        // Exclude courses with exclude flag
        masterScheduleCriteria
                .addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                        PATH_DELIMITER + m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);

        // If school != null filter by school
        if (schoolOid != null) {
            masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, schoolOid);
        }

        if (m_paramWholeSchool != null && m_paramWholeSchool.booleanValue() && !m_paramSchoolOidsCourse.isEmpty()) {
            masterScheduleCriteria.addIn(
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    m_paramSchoolOidsCourse);
        } else if (!StringUtils.isEmpty(m_paramCourseSections)) {
            String[] courseSectionOids = m_paramCourseSections.split(",");
            Collection<String> collection = Arrays.asList(courseSectionOids);

            masterScheduleCriteria.addIn(X2BaseBean.COL_OID, collection);
        }

        return masterScheduleCriteria;
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
            dates = m_data.getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Determines if the standard input criteria will adjust the criteria.
     *
     * @return true, if successful
     */
    private boolean inputCriteriaUsed() {
        boolean used = false;
        int queryCount = 1;

        String queryBy = (String) m_data.getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
        String queryString = (String) m_data.getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        while (!StringUtils.isEmpty(queryBy) && !StringUtils.isEmpty(queryString)) {
            if (!queryBy.equals("##all")) {
                used = true;
            }
            queryCount++;
            queryBy = (String) m_data.getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
            queryString = (String) m_data.getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        }
        return used;
    }

    /**
     * Checks if given TNScheduleSpan exists in map of spans by school.</br>
     * School is passed from the given span to get map.</br>
     * Also checks if secondary section is not the same as primary will return setup error.
     *
     * @param span TNScheduleSpan
     * @return true, if is duplicate span
     */
    private boolean isDuplicateSpan(TNScheduleSpan span) {
        boolean value = false;
        Map<String, MasterSchedule> map = getAllSections(span.getSchoolOid());
        if (map.containsKey(span.getSection().getCourseView())) {
            MasterSchedule priorSection = map.get(span.getSection().getCourseView());
            if (!priorSection.getOid().equals(span.getSection().getOid())) {
                m_data.addSetupError("Secondary Course View Conflict",
                        "Prior Section: " + priorSection.getSchedule().getSchool().getName() +
                                " Course View: " + priorSection.getCourseView() + " [" + priorSection.getOid() + "]" +
                                " Secondary Section: " + span.getSection().getSchedule().getSchool().getName() +
                                " Course View: " + span.getSection().getCourseView() + " [" + span.getSection().getOid()
                                + "]" +
                                " Student Name: " + span.getNameView());
            }
            value = true;
        }
        return value;
    }

    /**
     * Populates map of master schedules keyed on school oids with given schoolOid and MST.
     *
     * @param sklOid String
     * @param section MasterSchedule
     */
    private void loadReportableSection(String sklOid, MasterSchedule section) {
        Set<MasterSchedule> sections = m_schoolReportableSections.get(sklOid);
        if (sections == null) {
            sections = new TreeSet<MasterSchedule>(new SectionByCourseViewComparator());
            m_schoolReportableSections.put(sklOid, sections);
        }
        sections.add(section);
    }

    /**
     * Query all possible MSTs for the current school.</br>
     * Pass every MST and schoolOid as parameters to populate MSTs map keyed on schoolOid.
     */
    private void loadReportableSections() {
        String schoolOid = m_school == null ? null : m_school.getOid();

        X2Criteria criteria = getMasterScheduleCriteria(schoolOid);
        if (m_data.getBeanClass() != null && m_data.getBeanClass().equals(ScheduleTeacher.class)) {
            Boolean paramEntireSchool = Boolean.FALSE;
            String paramStaffsOids = (String) m_data.getParameter(INPUT_PARAM_STF_OIDS);
            if (StringUtils.isEmpty(paramStaffsOids)) {
                paramEntireSchool = Boolean.TRUE;
            }
            if ((paramEntireSchool != null && paramEntireSchool.booleanValue() == true) ||
                    paramStaffsOids != null || inputCriteriaUsed()) {
                SubQuery subQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_SECTION_OID,
                        m_data.getQuery().getCriteria());
                criteria.addIn(X2BaseBean.COL_OID, subQuery);
            }
        } else {
            m_data.applyInputCriteria(criteria, false, null);
        }

        QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);
        QueryIterator iterator = m_data.getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                MasterSchedule section = (MasterSchedule) iterator.next();
                Schedule schedule = section.getSchedule();
                if (schedule != null) {
                    String sklOid = schedule.getSchoolOid();
                    if (sklOid != null) {
                        loadReportableSection(sklOid, section);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Populates map of all possible schools student participate during given school year keyed on
     * student oid.
     *
     * @param applyStudents boolean
     */
    private void loadSecondarySchools(boolean applyStudents) {
        X2Criteria criteria = new X2Criteria();

        if (m_data.getParameter(INPUT_PARAM_ENROLLMENT_HELPER) == null && applyStudents) {
            m_data.applyInputCriteria(m_studentHelper.getStudentCriteria(), false, null);
        }
        criteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        criteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, m_data.getCurrentContext().getOid());

        PlainDate startDate = m_data.getCurrentContext().getStartDate();
        PlainDate endDate = m_data.getCurrentContext().getEndDate();

        criteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, endDate);
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addIsNull(StudentSchool.COL_END_DATE);
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, startDate);
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        QueryByCriteria studentSchoolQuery =
                m_studentHelper.getStudentSelectionQuery(StudentSchool.class, criteria, StudentSchool.COL_STUDENT_OID);
        m_secondarySchools =
                m_data.getBroker().getGroupedCollectionByQuery(studentSchoolQuery, StudentSchool.COL_STUDENT_OID, 1024);
    }

    /**
     * Populates map of possible schools keyed on Section Oids.
     */
    private void loadSchoolsBySection() {
        Map<String, Collection<SisSchool>> schoolsMap = null;
        if (m_schoolsByReportableSection == null) {
            m_schoolsByReportableSection = new HashMap<String, ArrayList<SisSchool>>();
            schoolsMap = m_data.getBroker().getMapByQuery(new BeanQuery(SisSchool.class), X2BaseBean.COL_OID, 1024);
        }

        for (Entry<String, Set<MasterSchedule>> entry : m_schoolReportableSections.entrySet()) {

            for (MasterSchedule mst : entry.getValue()) {
                String sklOid = entry.getKey();
                SisSchool schoolToAdd = (SisSchool) schoolsMap.get(sklOid);

                if (schoolToAdd != null) {
                    ArrayList<SisSchool> schoolsBySection = m_schoolsByReportableSection.get(mst.getOid());

                    if (schoolsBySection == null) {
                        schoolsBySection = new ArrayList<SisSchool>();
                        schoolsBySection.add(schoolToAdd);
                        m_schoolsByReportableSection.put(mst.getOid(), schoolsBySection);
                    } else {
                        schoolsBySection.add(schoolToAdd);
                    }
                }
            }
        }
    }

    /**
     * Populates map of TNScheduleSpans keyed on student oids.
     */
    private void loadStudents() {
        QueryIterator iterator = m_data.getBroker().getIteratorByQuery(m_studentHelper.getStudentQuery(false));
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                List<TNStudentScheduleSpan> studentSchedules = m_studentHelper.getTNStudentScheduleSpans(student);
                List<TNStudentEnrollmentSpan> studentEnrollments =
                        m_studentHelper.getTNStudentEnrollmentSpans(student, true);

                List<TNScheduleSpan> spans = setTNScheduleSpans(student, studentSchedules, studentEnrollments);
                removeAdjacentTNScheduleSpans(spans);
                addSpanToSections(spans);

                m_studentSchedules.put(student.getOid(), spans);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * The student history helper for this variant of the export must select all students that
     * are matched by the input criteria. Since the MODE_SCHEDULE_SPANS selects students based on
     * the
     * StudentSchedule and StudentScheduleChange criteria, adjustments are made to these criteria.
     * For the school input criteria, we cannot use the typical apply schools, since we need
     * to match any students in the selected school that are scheduled in other schools.
     * This is accomplished by adding an additional enrollment/active based criteria for the
     * school.
     *
     * @param applyStdSelection boolean
     * @param stdOids Collection<String>
     */
    private void prepareStudentSectionSelection(boolean applyStdSelection, Collection<String> stdOids) {
        // Student criteria based on StudentSchedule and StudentScheduleChange
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                m_data.getOrganization().getCurrentContext().getEndDate());

        if (m_data.getParameter(INPUT_PARAM_ENROLLMENT_HELPER) == null) {
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT,
                    Boolean.valueOf(applyStdSelection));

            if (!m_paramSchoolOidsCourse.isEmpty()) {
                m_studentHelper.getStudentCriteria().addAndCriteria(m_multiYearHelper.getWithAttributesCriteria());
                m_studentHelper.getStudentCriteria().addIn(m_multiYearHelper.getSchoolOidField(),
                        m_paramSchoolOidsCourse);
            }
        }
        // Run sections based exports.
        if (!applyStdSelection) {
            // Must not apply school filter, since we must return students from this school in other
            // schedules
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.FALSE);

            // restrict to students with sections selected in input criteria
            if (m_data.getBeanClass() == null || !m_data.getBeanClass().equals(ScheduleTeacher.class)) {
                m_data.applyInputCriteria(m_studentHelper.getStudentScheduleCriteria(), false,
                        StudentSchedule.REL_SECTION);
                m_data.applyInputCriteria(m_studentHelper.getStudentScheduleChangeCriteria(), false,
                        StudentScheduleChange.REL_MASTER_SCHEDULE);
                if (m_paramCourseSections != null && !m_paramCourseSections.isEmpty()) {
                    String[] courseSectionOids = m_paramCourseSections.split(",");
                    Collection<String> collection = Arrays.asList(courseSectionOids);

                    m_studentHelper.getStudentScheduleCriteria().addIn(StudentSchedule.COL_SECTION_OID, collection);
                    m_studentHelper.getStudentScheduleChangeCriteria()
                            .addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, collection);
                }
            }
        } else {
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            if (m_paramCourseSections != null && !m_paramCourseSections.isEmpty()) {
                // limit student selection to students in selected sections

                String[] courseSectionOids = m_paramCourseSections.split(",");
                Collection<String> collection = Arrays.asList(courseSectionOids);

                X2Criteria sscCriteria = new X2Criteria();
                sscCriteria.addIn(StudentSchedule.COL_SECTION_OID, collection);

                X2Criteria sccCriteria = new X2Criteria();
                sccCriteria.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, collection);
                if (m_fieldExcludeScc != null) {
                    sccCriteria.addNotEqualTo(m_fieldExcludeScc, BooleanAsStringConverter.TRUE);
                }
                X2Criteria scheduledCriteria = new X2Criteria();
                scheduledCriteria.addIn(X2BaseBean.COL_OID,
                        new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, sscCriteria));
                X2Criteria scheduledCriteria2 = new X2Criteria();
                scheduledCriteria2.addIn(X2BaseBean.COL_OID,
                        new SubQuery(StudentScheduleChange.class, StudentScheduleChange.COL_STUDENT_OID, sccCriteria));
                scheduledCriteria.addOrCriteria(scheduledCriteria2);

                m_studentHelper.getStudentCriteria().addAndCriteria(scheduledCriteria);
            }
        }

        m_multiYearHelper = m_enrollmentHelper.getStudentMultiYearHelper();

        // If school != null filter by students enrolled in the school during the proper context
        if (m_school != null || !m_paramSchoolOidsCourse.isEmpty()) {
            Set<String> sklOids = m_paramSchoolOidsCourse;
            if (m_school != null) {
                sklOids = new HashSet();
                sklOids.add(m_school.getOid());
            }

            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    m_data.getCurrentContext().getStartDate());
            enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                    m_data.getCurrentContext().getEndDate());
            enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, sklOids);
            SubQuery enrollmentSubQuery =
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);

            X2Criteria enrCriteria = new X2Criteria();
            enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

            // Select students who are active.
            X2Criteria activeCriteria = new X2Criteria();
            activeCriteria.addAndCriteria(m_multiYearHelper.getActiveStudentCriteria());
            activeCriteria.addIn(m_multiYearHelper.getSchoolOidField(), sklOids);

            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(enrCriteria);
            orCriteria.addOrCriteria(activeCriteria);


            // Build the final student criteria, including exclude criteria.
            X2Criteria criteria = m_studentHelper.getStudentCriteria();
            criteria.addAndCriteria(orCriteria);
            criteria.addNotEqualTo(m_fieldExcludeStd, BooleanAsStringConverter.TRUE);
        }
        if (stdOids != null && !stdOids.isEmpty()) {
            m_studentHelper.getStudentCriteria().addIn(X2BaseBean.COL_OID, stdOids);
        }
    }

    /**
     * Check if new TNScheduleSpan from given TNStudentScheduleSpan is match to dates and adds it to
     * the given List of TNScheduleSpan.
     *
     * @param scheduleSpans List of TNScheduleSpan
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param studentScheduleSpan TNStudentScheduleSpan
     * @param overrideSchool SisSchool
     * @param instrPgm String
     */
    private void processSpans(List<TNScheduleSpan> scheduleSpans,
                              PlainDate startDate,
                              PlainDate endDate,
                              TNStudentScheduleSpan studentScheduleSpan,
                              SisSchool overrideSchool,
                              String instrPgm) {
        // Do intervals intersect?
        if (!startDate.after(studentScheduleSpan.getExitDate())
                || !endDate.before(studentScheduleSpan.getEntryDate())) {
            PlainDate termStart = null;
            PlainDate termEnd = null;
            Collection<ScheduleTermDate> termDates =
                    getTermDates(studentScheduleSpan.getSection().getScheduleTermOid());
            for (ScheduleTermDate termDate : termDates) {
                if (termStart == null || termStart.after(termDate.getStartDate())) {
                    termStart = termDate.getStartDate();
                }
                if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                    termEnd = termDate.getEndDate();
                }
            }

            if (startDate.before(studentScheduleSpan.getEntryDate())) {
                startDate = studentScheduleSpan.getEntryDate();
            }

            if (endDate.after(studentScheduleSpan.getExitDate())) {
                endDate = studentScheduleSpan.getExitDate();
                if (studentScheduleSpan.getExitChange() != null &&
                        termEnd != null && endDate.before(termEnd) &&
                        termStart != null && endDate.after(termStart)) {
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(endDate);
                    cal.add(Calendar.DATE, -1);
                    endDate = new PlainDate(cal.getTime());
                }
            }

            if (startDate.before(endDate)) {
                TNScheduleSpan newSpan = new TNScheduleSpan(studentScheduleSpan, startDate, endDate, overrideSchool,
                        m_fieldOutsideIEP, instrPgm);
                if (!StringUtils.isEmpty(newSpan.getSchoolOid()) && newSpan.getSection() != null) {
                    scheduleSpans.add(newSpan);
                }
            }
        }
    }

    /**
     * Leave only not adjacent TNScheduleSpan from given list of TNScheduleSpan.
     *
     * @param spans List of TNScheduleSpan
     */
    private void removeAdjacentTNScheduleSpans(List<TNScheduleSpan> spans) {
        Collections.sort(spans, new Comparator<TNScheduleSpan>() {

            @Override
            public int compare(TNScheduleSpan o1, TNScheduleSpan o2) {
                int result = o1.getSchoolOid().compareTo(o2.getSchoolOid());
                if (result == 0) {
                    result = o1.getSection().getOid().compareTo(o2.getSection().getOid());
                }
                if (result == 0) {
                    result = o1.getStartDate().compareTo(o2.getStartDate());
                }
                return result;
            }
        });


        Iterator<TNScheduleSpan> iter = spans.iterator();
        if (iter.hasNext()) {
            TNScheduleSpan previousSpan = iter.next();
            while (iter.hasNext()) {
                TNScheduleSpan span = iter.next();
                if (span.getKey().equals(previousSpan.getKey())
                        && datesAdjacent(previousSpan.getEndDate(), span.getStartDate())) {
                    previousSpan.setEndDate(span.getEndDate());
                    iter.remove();
                } else {
                    previousSpan = span;
                }
            }
        }
    }


    /**
     * This @SuppressWarning need because .
     *
     * @param student SisStudent
     * @param studentSchedules List<TNStudentScheduleSpan>
     * @param enrollments List<TNStudentEnrollmentSpan>
     * @return List
     */
    private List<TNScheduleSpan> setTNScheduleSpans(SisStudent student,
                                                    List<TNStudentScheduleSpan> studentSchedules,
                                                    List<TNStudentEnrollmentSpan> enrollments) {
        List<TNScheduleSpan> scheduleSpans = new LinkedList<TNScheduleSpan>();
        for (TNStudentScheduleSpan scheduleSpan : studentSchedules) {
            // initial school validation
            if (scheduleSpan.getSection() != null &&
                    scheduleSpan.getSection().getSchedule() != null &&
                    scheduleSpan.getSection().getSchedule().getSchool() != null &&
                    scheduleSpan.getEntryDate() != null &&
                    scheduleSpan.getExitDate() != null) {
                for (TNStudentEnrollmentSpan span : enrollments) {
                    Collection<TNStudentEnrollmentSpan> splitSpans = m_data.splitSpanByProgram(span, student);
                    for (TNStudentEnrollmentSpan enrollmentSpan : splitSpans) {
                        String instrProgram = getInstProgram(student, enrollmentSpan);

                        // search for secondary school
                        Collection<StudentSchool> secondarySchools = m_secondarySchools.get(student.getOid());
                        if (secondarySchools != null && !secondarySchools.isEmpty()) {
                            for (StudentSchool secondarySchool : secondarySchools) {
                                if (secondarySchool.getSchoolOid() != null &&
                                        secondarySchool.getSchoolOid()
                                                .equals(scheduleSpan.getSection().getSchedule().getSchoolOid())) {
                                    PlainDate startDate = secondarySchool.getStartDate();
                                    PlainDate endDate = secondarySchool.getEndDate() == null
                                            ? m_data.getCurrentContext().getEndDate()
                                            : secondarySchool.getEndDate();
                                    processSpans(scheduleSpans, startDate, endDate, scheduleSpan, null, instrProgram);
                                    if (startDate.before(enrollmentSpan.getFirstActiveDate())) {
                                        startDate = enrollmentSpan.getFirstActiveDate();
                                    }
                                    if (enrollmentSpan.getLastActiveDate() != null
                                            && enrollmentSpan.getLastActiveDate().before(endDate)) {
                                        endDate = enrollmentSpan.getLastActiveDate();
                                    }
                                    if (!startDate.after(endDate) && enrollmentSpan.getSchool() != null) {
                                        processSpans(scheduleSpans, startDate, endDate, scheduleSpan,
                                                enrollmentSpan.getSchool(), instrProgram);
                                    }
                                }
                            }
                        }

                        // validate that primary school exists
                        if (enrollmentSpan.getSchool() != null &&
                                (scheduleSpan.getSection().getSchedule().getSchoolOid()
                                        .equals(enrollmentSpan.getSchool().getOid())
                                        || (enrollmentSpan.getFirstEntryEnrollment() != null &&
                                                scheduleSpan.getSection().getSchedule().getSchoolOid()
                                                        .equals(enrollmentSpan.getFirstEntryEnrollment()
                                                                .getSchoolOid())))) {
                            PlainDate startDate = enrollmentSpan.getFirstActiveEnrollment().getEnrollmentDate();
                            PlainDate endDate = enrollmentSpan.getFirstInactiveEnrollment() == null
                                    ? m_data.getCurrentContext().getEndDate()
                                    : enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentDate();
                            processSpans(scheduleSpans, startDate, endDate, scheduleSpan, null, instrProgram);
                        }
                    }
                }
            }
        }
        return scheduleSpans;
    }
}

