/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Used to prepare data for CA CSR reports.
 *
 * @author Follett Software Company
 */
public class CSRDataHelper extends StateReportData {
    private static final String[] CSR_GRADES = {"K", "TK", "PK", "KF", "KP", "KN", "01", "02", "03", "04", "05", "06",
            "07", "08"};
    private static final String PARAM_ALL_SCHOOLS = "allSchools";
    private static final String PARAM_CYCLES = "cycle";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_INCLUDE_SECONDARY = "includeSecondary";
    private static final String PARAM_SCHOOLS = "schoolOids";
    private static final String PARAM_START_DATE = "startDate";

    private static final String KEY_DATE_START = "DATE_BEGIN";
    private static final String KEY_DATE_END = "DATE_END";

    Map<String, ReferenceCode> m_gradeRefTable;

    private Map<String, Map<String, Set<String>>> m_countOfStudentsAtGrade;
    private Map<String, Map<String, PlainDate>> m_cycEdgeDatesMap;
    private StudentHistoryHelper m_helper;
    private Map<String, Map<String, Set<PlainDate>>> m_schoolInSessionDays;
    private Map<School, Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>>> m_schools;
    private Map<String, Map<String, Integer>> m_sectionCalendarMap;
    private Map<String, Set<String>> m_sectionGradeListMap;

    /**
     * Returns the count of students for a grade of a section.
     *
     * @param section String
     * @param grade String
     * @return Integer
     */
    public Integer getCountOfStudent(String section, String grade) {
        Integer value;

        Map<String, Set<String>> gradesMap = m_countOfStudentsAtGrade.get(section);
        if (gradesMap != null) {
            Set<String> students = gradesMap.get(grade);
            if (students != null) {
                value = Integer.valueOf(students.size());
            } else {
                value = Integer.valueOf(0);
            }
        } else {
            value = Integer.valueOf(0);
        }
        return value;
    }

    /**
     * Determine the number of students enrolled for a particular section and date.
     *
     * @param section MasterSchedule
     * @param date PlainDate
     * @param grade String
     * @return Integer
     */
    public Integer getDayCount(MasterSchedule section, PlainDate date, String grade) {
        Integer value = null;
        School school = section.getSchedule().getSchool();
        Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>> sectionMap = m_schools.get(school);
        if (sectionMap != null) {
            int total = 0;
            Map<PlainDate, Map<String, Integer>> dateMap = sectionMap.get(section);
            if (dateMap != null) {
                Map<String, Integer> sectionGradeMap = dateMap.get(date);
                if (sectionGradeMap != null && "all".equals(grade)) {
                    for (Entry<String, Integer> sectionGrade : sectionGradeMap.entrySet()) {
                        total += sectionGrade.getValue().intValue();
                    }
                } else if (sectionGradeMap != null && sectionGradeMap.get(grade) != null && !"all".equals(grade)) {
                    total += sectionGradeMap.get(grade).intValue();
                }
            }
            value = Integer.valueOf(total);
        }
        return value == null ? Integer.valueOf(1) : value;
    }

    /**
     * Determine the number of student days enrolled for a particular section and date range.
     *
     * @param section MasterSchedule
     * @param startDate null indicates from beginning of year
     * @param endDate null indicates to end of year
     * @param grade String
     * @return Integer
     */
    public Integer getDayRangeCount(MasterSchedule section, PlainDate startDate, PlainDate endDate, String grade) {
        Integer value = null;

        School school = section.getSchedule().getSchool();
        Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>> sectionMap = m_schools.get(school);
        if (sectionMap != null) {
            int total = 0;
            Map<PlainDate, Map<String, Integer>> dateMap = sectionMap.get(section);
            for (Entry<PlainDate, Map<String, Integer>> entry : dateMap.entrySet()) {
                if ((startDate == null || !entry.getKey().before(startDate)) &&
                        (endDate == null || !entry.getKey().after(endDate))) {
                    Map<String, Integer> sectionGradeMap = entry.getValue();
                    if (sectionGradeMap != null && "all".equals(grade)) {
                        for (Entry<String, Integer> sectionGrade : sectionGradeMap.entrySet()) {
                            total += sectionGrade.getValue().intValue();
                        }
                    } else if (sectionGradeMap != null && sectionGradeMap.get(grade) != null && !"all".equals(grade)) {
                        total += sectionGradeMap.get(grade).intValue();
                    }
                }
            }
            value = Integer.valueOf(total);
        }
        return value == null ? Integer.valueOf(0) : value;
    }

    /**
     * Get a list of schools in the data set.
     *
     * @return Sets the
     */
    public Set<School> getSchools() {
        return m_schools == null ? new HashSet<School>() : m_schools.keySet();
    }

    /**
     * Get the in-session dates for a particular section.
     *
     * @param section MasterSchedule
     * @return Sets the
     */
    public Set<PlainDate> getSectionDates(MasterSchedule section) {
        Set<PlainDate> dates = new TreeSet<PlainDate>();
        if (m_sectionCalendarMap != null && m_sectionCalendarMap.containsKey(section.getOid())) {
            int max = 0;
            String calendarId = null;
            for (Entry<String, Integer> entry : m_sectionCalendarMap.get(section.getOid()).entrySet()) {
                if (entry.getValue().intValue() > max) {
                    calendarId = entry.getKey();
                    max = entry.getValue().intValue();
                }
            }
            Set<PlainDate> sessionDates = null;
            if (calendarId != null) {
                if (m_schoolInSessionDays.containsKey(section.getSchedule().getSchoolOid())) {
                    Map<String, Set<PlainDate>> calendarMap =
                            m_schoolInSessionDays.get(section.getSchedule().getSchoolOid());
                    if (calendarMap.containsKey(calendarId)) {
                        sessionDates = calendarMap.get(calendarId);
                    }
                }
            }
            if (sessionDates != null) {
                for (PlainDate date : sessionDates) {
                    for (ScheduleTermDate termDate : section.getScheduleTerm().getScheduleTermDates()) {
                        if (!termDate.getStartDate().after(date) && !termDate.getEndDate().before(date)) {
                            dates.add(date);
                        }
                    }
                }
            }
        }
        return dates;
    }

    /**
     * Gets the section dates for range.
     *
     * @param section MasterSchedule
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Sets the
     */
    public Set<PlainDate> getSectionDatesForRange(MasterSchedule section, PlainDate startDate, PlainDate endDate) {
        Set<PlainDate> dates = new HashSet<PlainDate>();

        for (PlainDate date : getSectionDates(section)) {
            if ((startDate == null || !date.before(startDate)) &&
                    (endDate == null || !date.after(endDate))) {
                dates.add(date);
            }
        }

        return dates;
    }

    /**
     * Return a reference to a list contining sections and the grades incldued in the section.
     *
     * @return Map
     */
    public Map<String, Set<String>> getSectionGradeList() {
        return m_sectionGradeListMap;
    }

    /**
     * Get the sections loaded for a particular school.
     *
     * @param school School
     * @return Sets the
     */
    public Set<MasterSchedule> getSections(School school) {
        Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>> sectionMap = m_schools.get(school);
        return sectionMap == null ? new HashSet<MasterSchedule>() : sectionMap.keySet();
    }

    /**
     * Return the current student query.
     *
     * @param distinct boolean
     * @return Query
     */
    public Query getStudentQuery(boolean distinct) {
        List<String> gradeCodes = new LinkedList<String>();
        gradeCodes.add("__dummy__");
        if (m_gradeRefTable != null) {
            List<String> stateGrades = Arrays.asList(CSR_GRADES);
            for (ReferenceCode code : m_gradeRefTable.values()) {
                if (stateGrades.contains(code.getStateCode())) {
                    gradeCodes.add(code.getCode());
                }
            }
        }
        X2Criteria gradeSelection = new X2Criteria();
        gradeSelection.addIn(SisStudent.COL_GRADE_LEVEL, gradeCodes);
        m_helper.getStudentCriteria().addAndCriteria(gradeSelection);
        return m_helper.getStudentQuery(distinct);
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY,
                getParameter(PARAM_INCLUDE_SECONDARY));
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);

        loadDataStructures();
    }

    /**
     * Get a local reference to a map of grade level reference codes.
     */
    void loadGradeRefTable() {
        DataDictionaryField dictionaryField = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            m_gradeRefTable = getReferenceCodes(dictionaryField.getReferenceTableOid());
        }
    }

    /**
     * Add a grade level to the list of grade levels for a section.
     *
     * @param section MasterSchedule
     * @param gradeLevel String
     */
    private void addSectionGradeLevel(MasterSchedule section, String gradeLevel) {
        Set<String> grades = null;
        if (m_sectionGradeListMap.containsKey(section.getOid())) {
            grades = m_sectionGradeListMap.get(section.getOid());
        } else {
            grades = new TreeSet(new Comparator<String>() {
                @Override
                public int compare(String a, String b) {
                    int value = a.compareTo(b);
                    ReferenceCode aCode = m_gradeRefTable.get(a);
                    ReferenceCode bCode = m_gradeRefTable.get(b);
                    if (aCode != null && bCode != null) {
                        try {
                            // compare by number
                            int aInt = Integer.parseInt(aCode.getFieldA005());
                            int bInt = Integer.parseInt(bCode.getFieldA005());
                            if ((aInt - bInt) != 0) {
                                value = aInt - bInt;
                            }
                        } catch (NumberFormatException e) {
                            // use string compare
                        }
                    }
                    return value;
                }
            });
            m_sectionGradeListMap.put(section.getOid(), grades);
        }
        grades.add(gradeLevel);
    }

    /**
     * Get he enrolled days corresponding to a particular enrollment span.
     *
     * @param days Set<PlainDate>
     * @param enrollmentSpans List<StudentEnrollmentSpan>
     * @param schoolOid String
     * @return Sets the
     */
    private Set<PlainDate> getEnrolledDays(Set<PlainDate> days,
                                           List<StudentEnrollmentSpan> enrollmentSpans,
                                           String schoolOid) {
        Set<PlainDate> dates = new HashSet<PlainDate>();
        for (PlainDate date : days) {
            boolean found = false;
            Iterator<StudentEnrollmentSpan> spans = enrollmentSpans.iterator();
            while (spans.hasNext() && !found) {
                StudentEnrollmentSpan span = spans.next();
                if (span.getSchool() != null
                        && span.getSchool().getOid().equals(schoolOid)
                        && span.getFirstActiveEnrollment() != null
                        && span.getFirstActiveEnrollment().getEnrollmentDate() != null
                        && !span.getFirstActiveEnrollment().getEnrollmentDate().after(date)
                        && (span.getFirstInactiveEnrollment() == null
                                || span.getFirstInactiveEnrollment().getEnrollmentDate() == null || !span
                                        .getFirstInactiveEnrollment().getEnrollmentDate().before(date))) {
                    found = true;
                }
            }
            if (found) {
                dates.add(date);
            }
        }
        return dates;
    }

    /**
     * Get the in-session days for a particular student at a particular school.
     *
     * @param school School
     * @param student Student
     * @return Sets the
     */
    private Set<PlainDate> getInSessionDays(School school, Student student) {
        Set<PlainDate> dates = new HashSet<PlainDate>();
        if (m_schoolInSessionDays.containsKey(school.getOid())) {
            Map<String, Set<PlainDate>> calendarMap = m_schoolInSessionDays.get(school.getOid());
            if (calendarMap.containsKey(student.getCalendarCode())) {
                dates.addAll(calendarMap.get(student.getCalendarCode()));
            } else {
                dates.addAll(calendarMap.values().iterator().next());
            }
        }
        return dates;
    }

    /**
     * Get the active days for a particular StudentScheduleSpan.
     *
     * @param days Set<PlainDate>
     * @param scheduleSpan StudentScheduleSpan
     * @return Sets the
     */
    private Set<PlainDate> getSectionDays(Set<PlainDate> days, StudentScheduleSpan scheduleSpan) {
        Set<PlainDate> dates = new HashSet<PlainDate>();
        for (PlainDate date : days) {
            if (!scheduleSpan.getEntryDate().after(date) && !scheduleSpan.getExitDate().before(date)) {
                dates.add(date);
            }
        }
        return dates;
    }

    /**
     * Increment the count of students in a section with each calendar for a particular section and
     * student.
     *
     * @param section MasterSchedule
     * @param student SisStudent
     */
    private void incrementCalendarCount(MasterSchedule section, SisStudent student) {
        if (m_schoolInSessionDays.containsKey(section.getSchedule().getSchoolOid())) {
            String calendarId = null;
            Map<String, Set<PlainDate>> calendarMap = m_schoolInSessionDays.get(section.getSchedule().getSchoolOid());
            if (calendarMap.containsKey(student.getCalendarCode())) {
                calendarId = student.getCalendarCode();
            } else {
                calendarId = calendarMap.keySet().iterator().next();
            }
            Map<String, Integer> calendarCountMap = null;
            if (m_sectionCalendarMap.containsKey(section.getOid())) {
                calendarCountMap = m_sectionCalendarMap.get(section.getOid());
            } else {
                calendarCountMap = new HashMap<String, Integer>();
                m_sectionCalendarMap.put(section.getOid(), calendarCountMap);
            }
            Integer newCount = null;
            if (calendarCountMap.containsKey(calendarId)) {
                newCount = Integer.valueOf(calendarCountMap.get(calendarId).intValue() + 1);
            } else {
                newCount = Integer.valueOf(1);
            }
            calendarCountMap.put(calendarId, newCount);
        }
    }

    /**
     * Increment the count for a particular section and set of days.
     *
     * @param section MasterSchedule
     * @param sectionDays Set<PlainDate>
     * @param student SisStudent
     */
    private void incrementCounts(MasterSchedule section, Set<PlainDate> sectionDays, SisStudent student) {
        School school = section.getSchedule().getSchool();
        Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>> sectionMap = m_schools.get(school);
        if (sectionMap != null) {
            Map<PlainDate, Map<String, Integer>> dateMap = sectionMap.get(section);
            if (dateMap == null) {
                dateMap = new TreeMap<PlainDate, Map<String, Integer>>();
                sectionMap.put(section, dateMap);
            }
            for (PlainDate date : sectionDays) {
                if (dateMap.containsKey(date)) {
                    Map<String, Integer> gradeForSection = dateMap.get(date);
                    if (gradeForSection != null && student.getGradeLevel() != null
                            && gradeForSection.containsKey(student.getGradeLevel())) {
                        gradeForSection.put(student.getGradeLevel(),
                                Integer.valueOf(gradeForSection.get(student.getGradeLevel()).intValue() + 1));
                        dateMap.put(date, gradeForSection);
                    } else if (gradeForSection != null && student.getGradeLevel() != null
                            && !gradeForSection.containsKey(student.getGradeLevel())) {
                        gradeForSection.put(student.getGradeLevel(), Integer.valueOf(1));
                        dateMap.put(date, gradeForSection);
                    }

                } else {
                    Map<String, Integer> gradeForSection = new TreeMap<String, Integer>();
                    if (student.getGradeLevel() != null) {
                        gradeForSection.put(student.getGradeLevel(), Integer.valueOf(1));
                        dateMap.put(date, gradeForSection);
                    }
                }
            }
        }
    }

    /**
     * Add a number of students to the map of grade levels for a section.
     *
     * @param section String
     * @param student SisStudent
     */
    private void incrementCountOfStudentsAtGrade(String section, SisStudent student) {
        Map<String, Set<String>> gradeCountMap = m_countOfStudentsAtGrade.get(section);
        if (gradeCountMap == null) {
            gradeCountMap = new HashMap();
            m_countOfStudentsAtGrade.put(section, gradeCountMap);
        }

        Set<String> students = gradeCountMap.get(student.getGradeLevel());
        if (students == null) {
            students = new HashSet();
            gradeCountMap.put(student.getGradeLevel(), students);
        }

        students.add(student.getOid());
    }

    /**
     * Load the required data structures.
     */
    private void loadDataStructures() {
        m_sectionCalendarMap = new HashMap<String, Map<String, Integer>>();
        m_sectionGradeListMap = new HashMap<String, Set<String>>();
        m_countOfStudentsAtGrade = new HashMap<String, Map<String, Set<String>>>();

        loadGradeRefTable();
        loadSchools();
        loadEdgeDatesForCycles();
        loadSchoolCalendars();

        QueryIterator students = getBroker().getIteratorByQuery(getStudentQuery(false));

        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<StudentScheduleSpan> scheduleSpans = m_helper.getStudentScheduleSpans(student);
                List<StudentEnrollmentSpan> enrollmentSpans = m_helper.getStudentEnrollmentSpans(student, false);

                for (StudentScheduleSpan scheduleSpan : scheduleSpans) {
                    MasterSchedule section = scheduleSpan.getSection();

                    if (m_schools.containsKey(section.getSchedule().getSchool())) {
                        Set<PlainDate> days = getInSessionDays(section.getSchedule().getSchool(), student);
                        Set<PlainDate> enrolledDays =
                                getEnrolledDays(days, enrollmentSpans, section.getSchedule().getSchoolOid());
                        Set<PlainDate> sectionDays = getSectionDays(days, scheduleSpan);
                        sectionDays.retainAll(enrolledDays);
                        if (!sectionDays.isEmpty()) {
                            incrementCounts(section, sectionDays, student);
                            incrementCalendarCount(section, student);
                            addSectionGradeLevel(section, student.getGradeLevel());
                            incrementCountOfStudentsAtGrade(section.getOid(), student);
                        }
                    }
                }
            }
        } finally {
            students.close();
        }
    }

    /**
     * Calculate days of the given attendance period.
     */
    private void loadEdgeDatesForCycles() {
        ArrayList<String> cycles = new ArrayList<String>();
        String cyclesOids = (String) getParameter(PARAM_CYCLES);
        if (!StringUtils.isEmpty(cyclesOids)) {
            cycles.addAll(StringUtils.convertDelimitedStringToList(cyclesOids, ","));
        }
        X2Criteria cycleCodesCriteria = new X2Criteria();
        cycleCodesCriteria.addIn(X2BaseBean.COL_OID, cycles);
        QueryByCriteria cycleCodesQuery = new QueryByCriteria(ReferenceCode.class, cycleCodesCriteria);
        cycleCodesQuery.addOrderBy(ReferenceCode.COL_CODE, true);
        ArrayList<String> codes = new ArrayList<String>();
        codes.addAll(getBroker().getGroupedCollectionByQuery(cycleCodesQuery, ReferenceCode.COL_CODE, 1024).keySet());
        Collections.sort(codes,
                new Comparator<String>() {
                    @Override
                    public int compare(String str1, String str2) {
                        return str1.toString().compareTo(str2.toString());
                    }
                });

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addIn(DistrictCalendar.COL_CYCLE, codes);

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderByAscending(DistrictCalendar.COL_DATE);

        String[] columns = {DistrictCalendar.COL_CYCLE, DistrictCalendar.COL_DATE};
        int[] sizes = {1024, 1024};
        Map<String, Map<PlainDate, DistrictCalendar>> cyclesDaysMap =
                getBroker().getGroupedCollectionByQuery(query, columns, sizes);

        Set<PlainDate> daysTemp = new TreeSet<PlainDate>();

        m_cycEdgeDatesMap = new HashMap<String, Map<String, PlainDate>>();

        for (Entry<String, Map<PlainDate, DistrictCalendar>> entry : cyclesDaysMap.entrySet()) {
            ArrayList<PlainDate> cycleDates = new ArrayList<PlainDate>(entry.getValue().keySet());

            Collections.sort(cycleDates);

            daysTemp.addAll(cycleDates);

            Map<String, PlainDate> cycleDatesMap = new HashMap<String, PlainDate>();

            cycleDatesMap.put(KEY_DATE_END, cycleDates.get(cycleDates.size() - 1));
            int FIRST_DATE_OF_CYCLE = 0;
            cycleDatesMap.put(KEY_DATE_START, cycleDates.get(FIRST_DATE_OF_CYCLE));

            m_cycEdgeDatesMap.put(entry.getKey(), cycleDatesMap);
        }
    }

    /**
     * Load school calendars.
     */
    private void loadSchoolCalendars() {
        m_schoolInSessionDays = new HashMap<String, Map<String, Set<PlainDate>>>();

        List<String> schoolOids = new LinkedList<String>();
        for (School school : m_schools.keySet()) {
            schoolOids.add(school.getOid());
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);
        criteria.addIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                schoolOids);

        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

        if (m_cycEdgeDatesMap != null && !m_cycEdgeDatesMap.isEmpty()) {
            X2Criteria andCriteria = new X2Criteria();
            for (String cycle : m_cycEdgeDatesMap.keySet()) {
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE,
                        m_cycEdgeDatesMap.get(cycle).get(KEY_DATE_START));
                orCriteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE,
                        m_cycEdgeDatesMap.get(cycle).get(KEY_DATE_END));

                andCriteria.addOrCriteria(orCriteria);
            }

            criteria.addAndCriteria(andCriteria);
        } else if (startDate != null && endDate != null) {
            criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, startDate);
            criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, endDate);
        }

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        QueryIterator dates = getBroker().getIteratorByQuery(query);
        try {
            while (dates.hasNext()) {
                SchoolCalendarDate date = (SchoolCalendarDate) dates.next();

                String key = date.getSchoolCalendar().getSchoolOid();
                Map<String, Set<PlainDate>> calendarMap = null;
                if (m_schoolInSessionDays.containsKey(key)) {
                    calendarMap = m_schoolInSessionDays.get(key);
                } else {
                    calendarMap = new HashMap<String, Set<PlainDate>>();
                    m_schoolInSessionDays.put(key, calendarMap);
                }

                key = date.getSchoolCalendar().getCalendarId();
                Set<PlainDate> dateSet = null;
                if (calendarMap.containsKey(key)) {
                    dateSet = calendarMap.get(key);
                } else {
                    dateSet = new HashSet<PlainDate>();
                    calendarMap.put(key, dateSet);
                }
                dateSet.add(date.getDate());
            }
        } finally {
            dates.close();
        }
    }

    /**
     * Load the schools.
     */
    private void loadSchools() {
        class SectionComparator implements Comparator<MasterSchedule> {
            @Override
            public int compare(MasterSchedule a, MasterSchedule b) {
                return a.getCourseView().compareTo(b.getCourseView());
            }
        }

        String schoolOids = (String) getParameter(PARAM_SCHOOLS);
        Boolean isAllSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);

        m_schools = new TreeMap<School, Map<MasterSchedule, Map<PlainDate, Map<String, Integer>>>>(
                new Comparator<School>() {
                    @Override
                    public int compare(School a, School b) {
                        return a.getName().compareTo(b.getName());
                    }
                });

        if (isSchoolContext()) {
            m_schools.put(getSchool(),
                    new TreeMap<MasterSchedule, Map<PlainDate, Map<String, Integer>>>(new SectionComparator()));
        } else {
            QueryIterator schools = null;
            X2Criteria criteria = new X2Criteria();
            if (isAllSchools != null && isAllSchools.booleanValue()) {
                criteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            } else if (!StringUtils.isEmpty(schoolOids)) {
                Set<String> setSchoolOids = new HashSet<String>();
                setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
                criteria.addIn(X2BaseBean.COL_OID, setSchoolOids);
            }
            // By default all active schools.
            else {
                criteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }
            QueryByCriteria query = new QueryByCriteria(School.class, criteria);
            schools = getBroker().getIteratorByQuery(query);
            try {
                while (schools.hasNext()) {
                    School school = (School) schools.next();
                    m_schools.put(school,
                            new TreeMap<MasterSchedule, Map<PlainDate, Map<String, Integer>>>(new SectionComparator()));
                }
            } finally {
                schools.close();
            }
        }

    }
}

