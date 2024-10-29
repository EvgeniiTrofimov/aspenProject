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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.reports.bc.SchoolTranscriptData.BcGraduationManager;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Diploma Information export.
 *
 * @author X2 Development Corporation
 */
public class DiplomaInformationExport extends ExportJavaSource {
    // Extract fields
    private static final String FIELD_SD = "SD";
    private static final String FIELD_SCHOOL = "School";
    private static final String FIELD_PUPIL = "Pupil";
    private static final String FIELD_PEN = "PEN";
    private static final String FIELD_STUDENT_USUAL_NAME = "Usual Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_DOB = "DOB";
    private static final String FIELD_PROJ_GR_YR = "Proj Gr Yr";
    private static final String FIELD_DIPL_MET = "Dipl Met";
    private static final String FIELD_LA0 = "LA0";
    private static final String FIELD_LA1 = "LA1";
    private static final String FIELD_LA2 = "LA2";
    private static final String FIELD_SS0 = "SS0";
    private static final String FIELD_SS1 = "SS1";
    private static final String FIELD_SC0 = "SC0";
    private static final String FIELD_SC1 = "SC1";
    private static final String FIELD_MA0 = "MA0";
    private static final String FIELD_MA1 = "MA1";
    private static final String FIELD_PE0 = "PE0";
    private static final String FIELD_PL0 = "PL0";
    private static final String FIELD_FAS = "FAS";
    private static final String FIELD_PA = "PA";
    private static final String FIELD_OTR = "Otr";
    private static final String FIELD_EARNED_GR_12 = "Earned GR 12";
    private static final String FIELD_EARNED_TOTAL = "Earned Total";
    private static final String FIELD_POTENTIAL_GR_12 = "Potential GR 12";
    private static final String FIELD_POTENTIAL_TOTAL = "Potential Total";
    private static final String FIELD_SUM_GR_12 = "Sum GR 12";
    private static final String FIELD_SUM_TOTAL = "Sum Total";

    /*
     * Other constants
     */
    private static final int FIELD_COUNT = 29;
    private static final String[] GRADE_LEVELS = {"8", "08", "9", "09", "10", "11", "12", "SU"};
    private static final String GRADE_LEVEL_12 = "12";
    private static final String PARAM_PROGRAM_OID = "programStudiesOid";

    private List<String> m_columns;
    private Collection<String> m_schoolOids;
    private BcGraduationManager m_graduationManager;
    private UserDataContainer m_userData;
    private String m_programOid;

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
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
        DataGrid grid = new DataGrid();

        X2Criteria criteria = buildStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
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
                        satisfiedOtherRequirementOids);

                String totalCompleted = "";
                double totalEarned = 0;
                double totalProgress = 0;

                for (GraduationRequirement grq : grqList) {
                    String str = grq.getCode();
                    totalCompleted = String.valueOf(m_graduationManager.getTotalCreditsGained(null, grq.getOid(),
                            rawCreditsGained));
                    grqTotals.put(str, totalCompleted);
                    totalEarned += m_graduationManager.getTotalCreditsGained(null, grq.getOid(), rawCreditsGained);
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
                        if (courseOids.contains(transcript.getSchoolCourseOid())) {
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
                grid.set(FIELD_SD, student.getSchool().getSchoolId());
                grid.set(FIELD_SCHOOL, student.getSchool().getSchoolId());
                grid.set(FIELD_PUPIL, student.getLocalId());
                grid.set(FIELD_PEN, student.getStateId());
                grid.set(FIELD_STUDENT_USUAL_NAME, student.getNameView());
                grid.set(FIELD_GRADE, student.getGradeLevel());
                grid.set(FIELD_DOB, formatter.format(student.getPerson().getDob()));
                grid.set(FIELD_PROJ_GR_YR, String.valueOf(student.getYog()));
                grid.set(FIELD_DIPL_MET, "");
                grid.set(FIELD_LA0, grqTotals.get(FIELD_LA0));
                grid.set(FIELD_LA1, grqTotals.get(FIELD_LA1));
                grid.set(FIELD_LA2, grqTotals.get(FIELD_LA2));
                grid.set(FIELD_SS0, grqTotals.get(FIELD_SS0));
                grid.set(FIELD_SS0, grqTotals.get(FIELD_SS1));
                grid.set(FIELD_SC0, grqTotals.get(FIELD_SC0));
                grid.set(FIELD_SC1, grqTotals.get(FIELD_SC1));
                grid.set(FIELD_MA0, grqTotals.get(FIELD_MA0));
                grid.set(FIELD_MA1, grqTotals.get(FIELD_MA1));
                grid.set(FIELD_PE0, grqTotals.get(FIELD_PE0));
                grid.set(FIELD_PL0, grqTotals.get(FIELD_PL0));
                grid.set(FIELD_FAS, grqTotals.get(FIELD_FAS));
                grid.set(FIELD_PA, grqTotals.get(FIELD_PA));
                grid.set(FIELD_OTR, grqTotals.get("NDC"));
                grid.set(FIELD_EARNED_GR_12, String.valueOf(totalGr12Earned));
                grid.set(FIELD_EARNED_TOTAL, String.valueOf(totalEarned));
                grid.set(FIELD_POTENTIAL_GR_12, String.valueOf(totalGr12Progress));
                grid.set(FIELD_POTENTIAL_TOTAL, String.valueOf(totalProgress));
                grid.set(FIELD_SUM_GR_12, String.valueOf(sumGr12));
                grid.set(FIELD_SUM_TOTAL, String.valueOf(totalEarned + totalProgress));
            }
        } finally {
            students.close();
        }
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        SchoolTranscriptData reportData = new SchoolTranscriptData();
        m_graduationManager = reportData.new BcGraduationManager(getBroker());

        m_programOid = (String) getParameter(PARAM_PROGRAM_OID);

        setLineSeparator(FORMAT_EOL_WINDOWS);
        setUseValueWrappers(false);

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SD);
        m_columns.add(FIELD_SCHOOL);
        m_columns.add(FIELD_PUPIL);
        m_columns.add(FIELD_PEN);
        m_columns.add(FIELD_STUDENT_USUAL_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_DOB);
        m_columns.add(FIELD_PROJ_GR_YR);
        m_columns.add(FIELD_DIPL_MET);
        m_columns.add(FIELD_LA0);
        m_columns.add(FIELD_LA1);
        m_columns.add(FIELD_LA2);
        m_columns.add(FIELD_SS0);
        m_columns.add(FIELD_SS1);
        m_columns.add(FIELD_SC0);
        m_columns.add(FIELD_SC1);
        m_columns.add(FIELD_MA0);
        m_columns.add(FIELD_MA1);
        m_columns.add(FIELD_PE0);
        m_columns.add(FIELD_PL0);
        m_columns.add(FIELD_FAS);
        m_columns.add(FIELD_PA);
        m_columns.add(FIELD_OTR);
        m_columns.add(FIELD_EARNED_GR_12);
        m_columns.add(FIELD_EARNED_TOTAL);
        m_columns.add(FIELD_POTENTIAL_GR_12);
        m_columns.add(FIELD_POTENTIAL_TOTAL);
        m_columns.add(FIELD_SUM_GR_12);
        m_columns.add(FIELD_SUM_TOTAL);

        m_schoolOids = getSchoolOids();

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
     * Build student criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_SCHOOL_OID, m_schoolOids);
        criteria.addIn(SisStudent.COL_GRADE_LEVEL, Arrays.asList(GRADE_LEVELS));
        criteria.addEqualTo(SisStudent.REL_PROGRAM_STUDIES + ModelProperty.PATH_DELIMITER +
                GraduationStudentProgram.COL_PROGRAM_STUDIES_OID, m_programOid);

        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
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
        criteria.addIn(Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID, m_schoolOids);

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
