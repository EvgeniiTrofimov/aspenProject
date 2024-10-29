/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.reports.bc.SchoolTranscriptData.BcGraduationManager;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Diploma Information report.
 *
 * @author X2 Development Corporation
 */
public class DiplomaInformationData extends ReportJavaSourceNet {
    // Grid fields
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_DIST_NAME = "districtName";
    private static final String FIELD_CURRENT_TIME = "currentTime";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_GRQ_MAP = "grqMap";
    private static final String FIELD_TOTAL_SUM = "totalSum";
    private static final String FIELD_12_SUM = "gr12Sum";
    private static final String FIELD_TOTAL_POTENTIAL = "totalPotential";
    private static final String FIELD_12_POTENTIAL = "gr12Potential";
    private static final String FIELD_TOTAL_EARNED = "totalEarned";
    private static final String FIELD_12_EARNER = "gr12Earned";

    /*
     * Other constants
     */
    private static final String PARAM_PROGRAM_OID = "programStudiesOid";
    private static final String[] GRADE_LEVELS = {"8", "08", "9", "09", "10", "11", "12", "SU"};
    private static final String GRADE_LEVEL_12 = "12";

    private BcGraduationManager m_graduationManager;
    private StudentContextReportHelper m_helper;
    private UserDataContainer m_userData;
    private String m_programOid;
    private Collection m_schoolOids;

    /*
     * Tracking grade 12 tallies
     */
    private Map<String, Collection<Transcript>> m_studentG12TranscriptMap;
    private Map<String, Collection<StudentSchedule>> m_currentScheduleG12Map;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, buildStudentCriteria());
        query.addOrderByAscending(m_helper.getSchoolRelationship() + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(m_helper.getSchoolOidField());
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            List<GraduationRequirement> grqList = m_graduationManager.getAllRequirements(m_programOid);

            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                HashMap<String, String> grqTotals = new HashMap<String, String>();
                HashMap<String, List<SchoolCourse>> coursesGainedCredit = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaken = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaking = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<GraduationCourseRequirement>> partialCourseRequirments =
                        new HashMap<String, List<GraduationCourseRequirement>>();
                HashMap<String, Double> creditsGained = new HashMap<String, Double>();
                HashMap<String, Double> rawCreditsGained = new HashMap<String, Double>();
                HashMap<String, Double> creditsWaived = new HashMap<String, Double>();
                HashMap<String, Double> creditsRequired = new HashMap<String, Double>();
                HashMap<String, Double> creditsByCourse = new HashMap<String, Double>();
                HashMap<String, Double> creditsInProgress = new HashMap<String, Double>();
                HashMap<String, String> gradeLevelByCourse = new HashMap<String, String>();
                Map<String, Map<String, Object>> otherRequirementValues = new HashMap<String, Map<String, Object>>();
                List<String> satisfiedOtherRequirementOids = new ArrayList<String>();

                m_graduationManager.determineGraduationStatus(student,
                        m_userData,
                        m_programOid,
                        coursesGainedCredit,
                        coursesTaken,
                        coursesTaking,
                        new HashMap<String, List<SchoolCourse>>(),
                        new HashMap<String, List<String>>(),
                        creditsGained,
                        rawCreditsGained,
                        creditsWaived,
                        creditsRequired,
                        creditsByCourse,
                        creditsInProgress,
                        new HashMap<String, Double>(),
                        gradeLevelByCourse,
                        false,
                        partialCourseRequirments,
                        new HashMap<String, Map<String, String>>(),
                        otherRequirementValues,
                        satisfiedOtherRequirementOids,
                        null,
                        new ArrayList<String>());

                String totalCompleted = "";
                double totalEarned = 0;
                double totalProgress = 0;

                for (GraduationRequirement requirement : grqList) {
                    String requirementCode = requirement.getCode();

                    totalCompleted = String.valueOf(
                            m_graduationManager.getTotalCreditsGained(null, requirement.getOid(), rawCreditsGained));
                    grqTotals.put(requirementCode, totalCompleted);
                    totalEarned +=
                            m_graduationManager.getTotalCreditsGained(null, requirement.getOid(), rawCreditsGained);
                }

                totalProgress += m_graduationManager.getTotalCreditsInProgress(creditsInProgress);

                /*
                 * Load the grade 12 tallies based on the current schedule for grade 12 students and
                 * transcript
                 * records for grade 12
                 */
                double totalGr12Earned = 0;
                double totalGr12Progress = 0;

                Collection<Transcript> transcripts = m_studentG12TranscriptMap.get(student.getOid());
                if (!CollectionUtils.isEmpty(transcripts)) {
                    Collection<String> courseOids = parseCourseOids(coursesGainedCredit);
                    for (Transcript transcript : transcripts) {
                        if (courseOids.contains(transcript.getSchoolCourseOid())
                                && transcript.getTotalCredit() != null) {
                            totalGr12Earned += transcript.getTotalCredit().doubleValue();
                        }
                    }
                }

                Collection<StudentSchedule> schedules = m_currentScheduleG12Map.get(student.getOid());
                if (!CollectionUtils.isEmpty(schedules)) {
                    Collection<String> courseOids = parseCourseOids(coursesTaking);

                    Set<String> mstOids = new HashSet<String>();
                    if (!CollectionUtils.isEmpty(transcripts)) {
                        for (Transcript transcript : transcripts) {
                            if (transcript.getMasterScheduleOid() != null) {
                                mstOids.add(transcript.getMasterScheduleOid());
                            }
                        }
                    }

                    for (StudentSchedule schedule : schedules) {
                        if (courseOids.contains(schedule.getSection().getSchoolCourseOid())
                                && !mstOids.contains(schedule.getSectionOid())) {
                            totalGr12Progress += schedule.getSection().getSchoolCourse().getCredit().doubleValue();
                        }
                    }
                }

                double sumGr12 = totalGr12Earned + totalGr12Progress;

                grid.append();
                grid.set(FIELD_SCHOOL, student.getSchool(getCurrentContext().getOid(), getBroker()));
                grid.set(FIELD_DIST_NAME, getOrganization().getName());
                grid.set(FIELD_CURRENT_TIME, getCurrentTimeStamp());
                grid.set(FIELD_STUDENT, student);
                grid.set(FIELD_GRQ_MAP, grqTotals);

                grid.set(FIELD_12_EARNER, String.valueOf(totalGr12Earned));
                grid.set(FIELD_TOTAL_EARNED, String.valueOf(totalEarned));
                grid.set(FIELD_12_POTENTIAL, String.valueOf(totalGr12Progress));
                grid.set(FIELD_TOTAL_POTENTIAL, String.valueOf(totalProgress));
                grid.set(FIELD_12_SUM, String.valueOf(sumGr12));
                grid.set(FIELD_TOTAL_SUM, String.valueOf(totalEarned + totalProgress));
            }
        } finally {
            students.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        m_schoolOids = getSchoolOids();

        SchoolTranscriptData reportData = new SchoolTranscriptData();
        m_graduationManager = reportData.new BcGraduationManager(getBroker());

        m_programOid = (String) getParameter(PARAM_PROGRAM_OID);

        getStudentsG12TransciptMap();
        getCurrentScheduleG12Map();
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
        m_userData = userData;
    }

    /**
     * Builds the criteria for the students to include in the report.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_GRADE_LEVEL, Arrays.asList(GRADE_LEVELS));
        criteria.addEqualTo(SisStudent.REL_PROGRAM_STUDIES + ModelProperty.PATH_DELIMITER +
                GraduationStudentProgram.COL_PROGRAM_STUDIES_OID, m_programOid);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(SisStudent.COL_SCHOOL_OID, m_schoolOids);
        studentCriteria.addAndCriteria(m_helper.getActiveStudentCriteria());

        if (isSchoolContext()) {
            studentCriteria.addOrCriteria(StudentManager.getSecondaryStudentCriteria(
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER, getCurrentContext().getOid(),
                    getSchool().getOid(), null, null, getBroker().getPersistenceKey()));
        }

        criteria.addAndCriteria(studentCriteria);

        return criteria;
    }

    /**
     * Prepare student grade 12 current schedule map.
     *
     * @return void
     */
    private void getCurrentScheduleG12Map() {
        // Active Schedule sub query
        Criteria activeScheduleCriteria = new Criteria();
        activeScheduleCriteria.addIn(SchoolScheduleContext.COL_SCHOOL_OID, m_schoolOids);
        activeScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        SubQuery activeScheduleSubquery = new SubQuery(SchoolScheduleContext.class,
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, activeScheduleCriteria);
        activeScheduleSubquery.addGroupBy(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);

        /*
         * Build student schedule criteria
         */
        Criteria studentScheduleCriteria = new Criteria();
        studentScheduleCriteria.addIn(StudentSchedule.COL_SCHEDULE_OID, activeScheduleSubquery);
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.COL_GRADE_LEVEL, GRADE_LEVEL_12);

        BeanQuery studentScheduleQuery = new BeanQuery(StudentSchedule.class, studentScheduleCriteria);

        m_currentScheduleG12Map =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery, StudentSchedule.COL_STUDENT_OID, 20000);
    }

    /**
     * Returns the timestamp for 'now'.
     *
     * @return String
     */
    private String getCurrentTimeStamp() {
        SimpleDateFormat sdfDate = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
        Date now = new Date();
        String strDate = sdfDate.format(now);

        return strDate;
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        }

        return getBroker().getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Prepare student grade 12 transcript map.
     *
     * @return void
     */
    private void getStudentsG12TransciptMap() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Transcript.COL_GRADE_LEVEL, GRADE_LEVEL_12);
        criteria.addIn(Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField(), m_schoolOids);

        BeanQuery query = new BeanQuery(Transcript.class, criteria);

        m_studentG12TranscriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 200);
    }

    /**
     * Iterate over all school courses in the map and track the OID.
     *
     * @param courseMap HashMap<String,List<SchoolCourse>>
     * @return Collection<String> of OIDs
     */
    private Collection<String> parseCourseOids(HashMap<String, List<SchoolCourse>> courseMap) {
        Collection<String> oids = new LinkedList<>();

        for (String key : courseMap.keySet()) {
            List<SchoolCourse> courses = courseMap.get(key);
            if (!CollectionUtils.isEmpty(courses)) {
                for (SchoolCourse course : courses) {
                    oids.add(course.getOid());
                }
            }
        }

        return oids;
    }
}
