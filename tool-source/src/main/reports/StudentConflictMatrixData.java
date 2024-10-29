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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.BuildStudentSchedule;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.web.SisUserDataContainer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Student Conflict Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class StudentConflictMatrixData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "conflicts only" report parameter. The value is a Boolean
     */
    public static final String CONFLICTS_ONLY_PARAM = "conflictsOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    // Grid fields
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_COURSE = "course";
    private static final String FIELD_COMMENT = "comment";

    // Report parameter name constants
    private static final String PARAMETER_PERIOD_ID_LOOKUP = "periodIdLookup";

    private SisSchool m_currentSchool = null;
    private SisStudent m_currentStudent = null;
    private Map m_masterMap = null;
    private Map m_periodIdLookup = null;
    private Schedule m_schedule = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria requestCriteria = new Criteria();
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        if (m_currentStudent != null) {
            requestCriteria.addEqualTo(CourseRequest.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            if ("course".equals(queryBy)) {
                Criteria subCriteria = new Criteria();
                subCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_schedule.getDistrictContextOid());
                subCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_currentSchool.getOid());
                subCriteria.addEqualTo(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.COL_NUMBER, queryString);

                SubQuery subQuery = new SubQuery(CourseRequest.class, CourseRequest.COL_STUDENT_OID, subCriteria);
                requestCriteria.addIn(CourseRequest.COL_STUDENT_OID, subQuery);
            } else {
                addUserCriteria(requestCriteria, queryBy, queryString, null, null);
            }
        }

        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_schedule.getDistrictContextOid());
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_currentSchool.getOid());

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);
        applyUserSort(requestQuery, (String) getParameter(STUDENT_SORT_PARAM));

        requestQuery.addOrderByAscending(CourseRequest.COL_ALTERNATE_INDICATOR);
        requestQuery.addOrderByAscending(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        boolean conflictsOnly = ((Boolean) getParameter(CONFLICTS_ONLY_PARAM)).booleanValue();

        QueryIterator requestIterator = null;
        try {
            Collection primaryRequests;
            boolean allScheduled;
            String lastOid = null;
            String comment = "";
            SisStudent currentStudent;
            CourseRequest request;
            BuildStudentSchedule schedule;
            Map scheduleMap = null;

            requestIterator = getBroker().getIteratorByQuery(requestQuery);
            try {
                populateMasterMap();

                while (requestIterator.hasNext()) {
                    request = (CourseRequest) requestIterator.next();
                    currentStudent = request.getStudent();

                    if (lastOid == null || !currentStudent.getOid().equals(lastOid)) {
                        scheduleMap = getStudentScheduleMap(currentStudent);
                    }

                    lastOid = currentStudent.getOid();

                    // Checks if student has any conflicts if input parameter was set to true
                    if (conflictsOnly) {
                        primaryRequests = getPrimaryRequests(currentStudent);
                        allScheduled = isAllScheduled(scheduleMap, primaryRequests);
                    } else {
                        allScheduled = false;
                    }

                    if (!allScheduled) {
                        grid.append();
                        grid.set(FIELD_STUDENT, currentStudent);
                        grid.set(FIELD_COURSE, request.getSchoolCourse());

                        schedule = (BuildStudentSchedule) scheduleMap.get(request.getSchoolCourseOid());
                        if (schedule != null) {
                            if (request.getAlternateIndicator()) {
                                comment = "Alternate request scheduled";
                            } else {
                                comment = "S:" + schedule.getSection().getSectionNumber();
                            }
                            grid.set(FIELD_COMMENT, comment);

                            populatePeriodInfo(request.getSchoolCourse(), grid, schedule);
                        } else {
                            if (m_masterMap.containsKey(request.getSchoolCourseOid())) {
                                if (request.getAlternateIndicator()) {
                                    comment = "Alternate request not scheduled";
                                } else {
                                    comment = "Request not scheduled";
                                }
                                grid.set(FIELD_COMMENT, comment);

                                // Get Open Sections
                                populatePeriodInfo(request.getSchoolCourse(), grid, null);
                            } else {
                                comment = "Course not scheduled";
                                grid.set(FIELD_COMMENT, comment);
                            }
                        }
                    }
                }
            } finally {
                requestIterator.close();
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        }

        grid.beforeTop();

        addParameter(PARAMETER_PERIOD_ID_LOOKUP, m_periodIdLookup);

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentSchool = (SisSchool) getSchool();

        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            m_schedule = ((SisUserDataContainer) userData).getBuildSchedule();
            StudentScheduleAttributes attributes =
                    userData.getCurrentRecord(StudentScheduleAttributes.class);
            if (attributes != null) {
                m_currentStudent = attributes.getStudent();
            }
        } else {
            m_schedule = m_currentSchool.getActiveSchedule();
            m_currentStudent = userData.getCurrentRecord(SisStudent.class);
        }

        m_periodIdLookup = new HashMap(15);
        Iterator periods = m_schedule.getSchedulePeriods(getBroker()).iterator();
        while (periods.hasNext()) {
            SchedulePeriod period = (SchedulePeriod) periods.next();
            m_periodIdLookup.put(Integer.valueOf(period.getNumber()), period.getId());
        }
    }

    /**
     * Returns a collection of primary CourseRequests for a given student.
     *
     * @param student SisStudent
     * @return collection of primary course requests
     */
    private Collection getPrimaryRequests(SisStudent student) {
        Criteria requestCriteria = new Criteria();
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, m_currentSchool.getOid());
        requestCriteria.addNotEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, "1");
        requestCriteria.addEqualTo(CourseRequest.COL_STUDENT_OID, student.getOid());
        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_schedule.getDistrictContextOid());

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);

        return getBroker().getCollectionByQuery(requestQuery);
    }

    /**
     * Takes a map of schedules keyed to SchoolCourseOid and a collection of requests and returns
     * if all the requests have been scheduled.
     *
     * @param schedules Map
     * @param primaries Collection
     * @return boolean
     */
    private boolean isAllScheduled(Map schedules, Collection primaries) {
        Iterator iterator = primaries.iterator();
        boolean allScheduled = true;

        while (iterator.hasNext()) {
            CourseRequest request = (CourseRequest) iterator.next();
            if (!schedules.containsKey(request.getSchoolCourseOid())) {
                allScheduled = false;
                break;
            }
        }

        return allScheduled;
    }

    /**
     * Queries for the given student's schedule and returns it as a map keyed to the course Oid.
     *
     * @param student SisStudent
     * @return Map
     */
    private Map getStudentScheduleMap(SisStudent student) {
        HashMap scheduleMap = new HashMap(20);
        Criteria scheduleCriteria = new Criteria();
        scheduleCriteria.addEqualTo(BuildStudentSchedule.COL_SCHEDULE_OID, m_schedule.getOid());
        scheduleCriteria.addEqualTo(BuildStudentSchedule.COL_STUDENT_OID, student.getOid());

        QueryByCriteria scheduleQuery = new QueryByCriteria(BuildStudentSchedule.class, scheduleCriteria);

        QueryIterator scheduleIterator = getBroker().getIteratorByQuery(scheduleQuery);

        try {
            BuildStudentSchedule schedule;
            String courseOid;
            BuildMasterSchedule master;

            while (scheduleIterator.hasNext()) {
                schedule = (BuildStudentSchedule) scheduleIterator.next();
                master = schedule.getSection();
                if (master != null) {
                    courseOid = master.getSchoolCourseOid();
                    scheduleMap.put(courseOid, schedule);
                }
            }
        } finally {
            scheduleIterator.close();
        }

        return scheduleMap;
    }

    /**
     * Sets a Map of the master schedule based on courseOid.
     *
     * @return A map of courseOids to list of sections of course
     */
    private void populateMasterMap() {
        Collection sections = null;

        Criteria masterCriteria = new Criteria();
        masterCriteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_schedule.getOid());

        QueryByCriteria masterQuery = new QueryByCriteria(BuildMasterSchedule.class, masterCriteria);
        masterQuery.addOrderByAscending(BuildMasterSchedule.COL_SCHOOL_COURSE_OID);

        QueryIterator masterIterator = null;

        try {
            masterIterator = getBroker().getIteratorByQuery(masterQuery);
            sections = new ArrayList(20);
            m_masterMap = new HashMap(1000);

            String lastOid = null;
            SchoolCourse course = null;

            while (masterIterator.hasNext()) {
                BuildMasterSchedule schedule = (BuildMasterSchedule) masterIterator.next();
                course = schedule.getSchoolCourse();

                if (lastOid == null || !course.getOid().equals(lastOid)) {
                    if (lastOid != null) {
                        m_masterMap.put(lastOid, sections);
                    }

                    sections = new ArrayList(20);
                }

                if (schedule.getScheduleDisplay() != null) {
                    sections.add(schedule);
                }

                lastOid = course.getOid();
            }

            m_masterMap.put(lastOid, sections);
        } finally {
            if (masterIterator != null) {
                masterIterator.close();
            }
        }
    }

    /**
     * Creates and sets the availability of sections for each period in the day.
     *
     * @param course SchoolCourse
     * @param grid ReportDataGrid
     * @param schedule BuildStudentSchedule
     */
    private void populatePeriodInfo(SchoolCourse course, ReportDataGrid grid, BuildStudentSchedule schedule) {
        ScheduleMap studentScheduleMap = null;
        String scheduledSection = "";
        String display;
        String displayPrefix;
        String displaySuffix;

        Collection sections = (Collection) m_masterMap.get(course.getOid());

        if (schedule != null) {
            studentScheduleMap = schedule.getScheduleMap();
            scheduledSection = schedule.getSection().getSectionNumber();
        }

        for (int i = 1; i <= m_schedule.getPeriods(); i++) {
            String thisPeriod = String.valueOf(i);

            // Class is schedule this period.
            if (schedule != null && studentScheduleMap.isScheduledForPeriod(i)) {
                displayPrefix = "S:";
                if (studentScheduleMap.getBaseTerms() != studentScheduleMap.getCoveredTerms()) {
                    displaySuffix = "[" + schedule.getTermView() + "]";
                } else {
                    displaySuffix = "";
                }
                display = displayPrefix + scheduledSection + displaySuffix;

                grid.set(thisPeriod, display);
            } else {
                Iterator sectionIterator = sections.iterator();
                BuildMasterSchedule master;
                ScheduleMap masterScheduleMap;
                String preExisting;

                while (sectionIterator.hasNext()) {
                    master = (BuildMasterSchedule) sectionIterator.next();
                    masterScheduleMap = new ScheduleMap(master.getTermMap(), master.getScheduleMatrix());

                    if (masterScheduleMap.isScheduledForPeriod(i)) {
                        if (master.getEnrollmentMax() > master.getEnrollmentTotal()) {
                            displayPrefix = "O:";
                        } else {
                            displayPrefix = "C:";
                        }

                        if (masterScheduleMap.getBaseTerms() != masterScheduleMap.getCoveredTerms()) {
                            displaySuffix = "[" + master.getTermView() + "]";
                        } else {
                            displaySuffix = "";
                        }

                        preExisting = (String) grid.get(thisPeriod);
                        if (preExisting != null) {
                            grid.set(thisPeriod, preExisting + " " + displayPrefix + master.getSectionNumber());
                        } else {
                            grid.set(thisPeriod, displayPrefix + master.getSectionNumber());
                        }
                    }
                }
            }
        }
    }
}
