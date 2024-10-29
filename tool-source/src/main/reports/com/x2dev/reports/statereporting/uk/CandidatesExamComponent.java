/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The CandidateExamComponent.java is the java class to process the input definition
 * to get the needed data and print the report for it. The report will have the course,
 * awarding body, season, series, and list of students name and list of exam option
 * titles that were selected in the input definition.
 *
 * The file for the format for this file is "CandidatesExamComponent.jrxml" and
 * for the input definition is "CandidatesExamComponentInput.xml".
 *
 * So in the input definition, the user can select one course and this java class will
 * generate and print all the students in that course. The user can also select the season,
 * awarding body, and the list of option titles that will be printed on the report. The
 * exam series will be generated automatically based on the option titles that selected.
 * This report is used as the "checklist" to keep track which students take which exam option.
 *
 * @author jantonius
 *
 */
public class CandidatesExamComponent extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    private static final String GRID_COURSE_NAME = "courseName";
    private static final String GRID_AWARDING_BODY = "awardingBody";
    private static final String GRID_SEASON = "season";
    private static final String GRID_SERIES = "series";
    private static final String GRID_STUDENT_NAME = "studentName";
    private static final String GRID_OPTION_TITLE = "optionTitle";

    private static final String PARAM_COURSE_LIST = "courseList";
    private static final String PARAM_SEASON = "season";
    // private static final String PARAM_AWARDING_BODY = "awardingBodyList";
    private static final String PARAM_OPTION_LIST = "optionList";
    private static final String PARAM_ACTIVE_ONLY = "activeOnly";

    private static final String VAR_COURSE_VIEW = "mstCourseView";

    // Collection of Exam Component Title
    private Map<String, Collection<String>> m_optionTitleList; // map the seriesOID to the Exam
                                                               // Component Title (Option Title)

    // Map of the Exam Series OID with the instance of the Exam Series
    Map<String, ExamSeries> m_exSerMap;

    // Map of the Awarding Body
    Map<String, String> m_awdBodyMap;

    // Collection of mstOID (Master Schedule OID) List
    ArrayList<String> m_mstOIDList;

    // Some parameters that needed
    private String m_season = "";

    private Map<String, String> m_courseDescList; // map the mstOID to the course description
                                                  // (course name and teacher name)
    private Map<String, String> m_courseViewList; // map the mstOID to the course view (course
                                                  // number)
    private Map<String, Collection<SisStudent>> m_courseStudentMap; // map the mstOID to the list of
                                                                    // students inside it

    int m_maxOptionTitle = 20; // maximum number of Exam Option Title per page

    /**
     * Prepares the data source that will be used by the Jasper (iReport) design. This method is
     * called after <code>initialize(UserDataContainer)</code> and before
     * <code>releaseResources()</code>.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (m_mstOIDList != null && !m_mstOIDList.isEmpty()) {
            // If the student list is not empty, then do the query for each student record
            for (String mstOID : m_mstOIDList) {
                Collection<SisStudent> studentColl = m_courseStudentMap.get(mstOID);
                String courseName = m_courseDescList.get(mstOID);

                // when at least one exam option is selected
                if (m_optionTitleList != null && !m_optionTitleList.isEmpty()) {
                    // then separate each page based on the Course Section AND Exam Series
                    Set<String> examSeriesOIDList = m_optionTitleList.keySet();
                    for (String examSeriesOID : examSeriesOIDList) {
                        ExamSeries exSer = m_exSerMap.get(examSeriesOID);
                        Collection<String> optionTitleList = m_optionTitleList.get(examSeriesOID);

                        for (Student std : studentColl) {
                            // if the Exam Option Titles are more than the maximum number of Title
                            // that can be shown on one page,
                            // then we need to print it the next page
                            for (int counter = 0; (counter * m_maxOptionTitle) < optionTitleList.size(); counter++) {
                                grid.append();
                                grid.set(VAR_COURSE_VIEW, m_courseViewList.get(mstOID));
                                grid.set(GRID_STUDENT_NAME, std.getNameView());
                                grid.set(GRID_COURSE_NAME, courseName);
                                grid.set(GRID_AWARDING_BODY, m_awdBodyMap.get(exSer.getAwardingBody()));
                                grid.set(GRID_SEASON, m_season);
                                grid.set(GRID_SERIES, exSer.getSeriesId());

                                for (int i = 0; (i + (counter * m_maxOptionTitle)) < optionTitleList.size()
                                        && i < m_maxOptionTitle; i++) {
                                    int counter2 = i + 1;
                                    grid.set(GRID_OPTION_TITLE + counter2, ((ArrayList<String>) optionTitleList)
                                            .get(i + (counter * m_maxOptionTitle)));
                                }
                            }
                        }
                    }
                } else // otherwise, just print the student list name without exam series, awarding
                       // body, and exam option title
                {
                    for (Student std : studentColl) {
                        grid.append();
                        grid.set(VAR_COURSE_VIEW, m_courseViewList.get(mstOID));
                        grid.set(GRID_STUDENT_NAME, std.getNameView());
                        grid.set(GRID_COURSE_NAME, courseName);
                        grid.set(GRID_AWARDING_BODY, "-");
                        grid.set(GRID_SEASON, m_season);
                        grid.set(GRID_SERIES, "-");
                    }
                }
            }
            /*
             * Sort it out by "mstCourseView", "Exam Series", "Option Title 1" and then
             * "Student Name"
             */
            grid.sort(
                    Arrays.asList(
                            new String[] {VAR_COURSE_VIEW, GRID_SERIES, GRID_OPTION_TITLE + "1", GRID_STUDENT_NAME}),
                    false);
            grid.beforeTop();
        }

        return grid;
    }

    /**
     *
     * This method is provided as a way for subclasses to save session state information.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        // Get the Course we should filter and put all the students that enroll in those courses
        // into a set
        String courses = (String) getParameter(PARAM_COURSE_LIST); // comes in as <blank>, or comma
                                                                   // separated refOID
        if (!StringUtils.isEmpty(courses)) {
            m_mstOIDList = StringUtils.convertDelimitedStringToList(courses, ',');

            // Check whether we should select the active students only
            boolean activeOnly = ((Boolean) getParameter(PARAM_ACTIVE_ONLY)).booleanValue();

            // initialize the Map
            m_courseDescList = new HashMap<String, String>();
            m_courseViewList = new HashMap<String, String>();
            m_courseStudentMap = new HashMap<String, Collection<SisStudent>>();

            // for each mstOID, get the course Name & Section and the Teacher Name, also the list of
            // students in that
            // course and put them in the Map.
            for (String mstOID : m_mstOIDList) {
                MasterSchedule mstSchedule = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, mstOID);

                // get the course description and add it to the Map
                String courseDesc = mstSchedule.getCourseView() + " - " + mstSchedule.getStaffView();
                m_courseDescList.put(mstOID, courseDesc);

                m_courseViewList.put(mstOID, mstSchedule.getCourseView());

                // Collection for the student
                Collection<StudentSchedule> stdScheduleCol = mstSchedule.getStudentSections();
                Collection<String> stdOIDList = new HashSet<String>(); // hash set to prevent
                                                                       // duplication

                // get all the stdOID for the given mstOID
                for (StudentSchedule stdSchedule : stdScheduleCol) {
                    String tempStdOID = stdSchedule.getStudentOid();
                    stdOIDList.add(tempStdOID); // because stdOIDList is a HashSet, there won't be
                                                // any duplicate
                }

                /*
                 * Set up the STD (student) criteria.
                 */
                X2Criteria stdCriteria = new X2Criteria();

                if (activeOnly) // Check whether we should select the active students only
                {
                    stdCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                            SisStudent.COL_ENROLLMENT_STATUS));
                }

                // Get the school ID of the current school selected
                if (getSchool() != null) {
                    stdCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
                }

                // If the studentOID list is not empty, then add those students to the criteria.
                // Otherwise, give an "empty" student to prevent an error and give an empty result.
                if (!stdOIDList.isEmpty()) {
                    stdCriteria.addIn(X2BaseBean.COL_OID, stdOIDList);
                } else {
                    stdCriteria.addEqualTo(X2BaseBean.COL_OID, "");
                }

                /*
                 * Query the students and store them in a Collection
                 */
                BeanQuery studentQuery = new BeanQuery(SisStudent.class, stdCriteria);
                Collection<SisStudent> studentsColl = getBroker().getCollectionByQuery(studentQuery);

                // add the collection of the students to the Map
                m_courseStudentMap.put(mstOID, studentsColl);
            }
        }

        // Get the Season name
        String seasonID = (String) getParameter(PARAM_SEASON);
        if (!StringUtils.isEmpty(seasonID)) {
            ExamSeason curSeason = (ExamSeason) getBroker().getBeanByOid(ExamSeason.class, seasonID);
            m_season = curSeason.getName();
        }

        // Get all Awarding Body (for mapping later on)
        X2Criteria awdBodyCrit = new X2Criteria();
        awdBodyCrit.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbAwdingBody");
        BeanQuery awdBodyQuery = new BeanQuery(ReferenceCode.class, awdBodyCrit);
        Map<String, ReferenceCode> awdBodyRefMap = getBroker().getMapByQuery(awdBodyQuery, X2BaseBean.COL_OID, 20);
        m_awdBodyMap = new HashMap<String, String>();
        for (String refOID : awdBodyRefMap.keySet()) {
            ReferenceCode refCode = awdBodyRefMap.get(refOID);
            m_awdBodyMap.put(refCode.getCode(), refCode.getDescription()); // map the awarding body
                                                                           // number/code to the
                                                                           // title/description
        }

        // Get the Option Title List that we want to print
        String optionList = (String) getParameter(PARAM_OPTION_LIST); // comes in as <blank>, or
                                                                      // comma separated optOID
        if (!StringUtils.isEmpty(optionList)) {
            ArrayList<String> optOIDList = StringUtils.convertDelimitedStringToList(optionList, ',');

            // Get all the Exam Option based on the optOID that we get
            X2Criteria exOptCrit = new X2Criteria();
            exOptCrit.addIn(X2BaseBean.COL_OID, optOIDList);
            BeanQuery exOptBean = new BeanQuery(ExamOption.class, exOptCrit);
            Map<String, ExamOption> exOptMap = getBroker().getMapByQuery(exOptBean, X2BaseBean.COL_OID, 20);

            // Get all Exam series for mapping later on
            X2Criteria exSerCrit = new X2Criteria();
            BeanQuery exSerBean = new BeanQuery(ExamSeries.class, exSerCrit);
            m_exSerMap = getBroker().getMapByQuery(exSerBean, X2BaseBean.COL_OID, 0);

            Collection<String> examSeriesOIDCol = new ArrayList<String>();
            Collection<String> allExamOptionOIDCol = new ArrayList<String>();

            for (int i = 0; i < optOIDList.size(); i++) {
                allExamOptionOIDCol.add(optOIDList.get(i));

                // add the series to series list (prevent duplicate)
                String temSeriesOID = exOptMap.get(optOIDList.get(i)).getSeriesOid();
                if (!examSeriesOIDCol.contains(temSeriesOID)) {
                    examSeriesOIDCol.add(temSeriesOID);
                }
            }

            if (!examSeriesOIDCol.isEmpty()) {
                m_optionTitleList = new HashMap<String, Collection<String>>();

                // then separate the Option Title based on the exam series
                for (String exSerOID : examSeriesOIDCol) {
                    Collection<String> tempOptTitleCol = new ArrayList<String>();

                    for (String exOptOID : allExamOptionOIDCol) {
                        ExamOption tempExOpt = exOptMap.get(exOptOID);

                        // if the exam option belong to the given exam series
                        if (tempExOpt.getSeries().getOid().equals(exSerOID)) {
                            // add the Exam Option (just the title) to the temporary Option Title
                            // Collection
                            tempOptTitleCol.add(tempExOpt.getOptionEntryCode() + " - " + tempExOpt.getTitle());

                            // remove the Exam Option from the list
                            // allExamOptionOIDCol.remove(tempExOpt.getOid());
                        }
                    }

                    // put the collection of Exam Option (title) and Map it to the Exam Series
                    m_optionTitleList.put(exSerOID, tempOptTitleCol);
                }
            }
        }
    }
}
