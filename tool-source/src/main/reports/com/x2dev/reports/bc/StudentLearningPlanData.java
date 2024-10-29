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
package com.x2dev.reports.bc;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEdPlanGoal;
import com.x2dev.sis.model.beans.StudentEdPlanMeeting;
import com.x2dev.sis.model.beans.StudentEdPlanMeetingParticipant;
import com.x2dev.utils.CollectionUtils;
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * The Class StudentLearningPlanData.
 *
 * @author Follett Software Company
 */
public class StudentLearningPlanData extends BaseFormReportJavaSource {
    /**
     * Serial Version
     */
    private static final long serialVersionUID = 1L;

    /**
     * ID of the goals and participants sub-reports.
     */
    public static final String GOALS_SUBREPORT_ID = "BC-SLP-001-SUB1";
    public static final String PARTICIPANTS_SUBREPORT_ID = "BC-SLP-001-SUB2";

    /**
     * Parameter containing the data for the goals and participants sub-reports.
     */
    public static final String PARAM_GOALS_SUBREPORT_DATA = "goalsData";
    public static final String PARAM_PARTICIPANTS_SUBREPORT_DATA = "participantsData";

    /**
     * Parameter containing the format for the goals and participants sub-reports.
     */
    public static final String PARAM_GOALS_SUBREPORT_FORMAT = "goalsFormat";
    public static final String PARAM_PARTICIPANTS_SUBREPORT_FORMAT = "participantsFormat";

    /**
     * Parameter constants
     */
    public static final String PARAM_STUDENT = "student";
    public static final String PARAM_STUDENT_ED_PLAN = "studentEdPlan";
    public static final String PARAM_STUDENT_DOB = "studentDOB";
    public static final String PARAM_MEETING_DATE = "meetingDate";

    /**
     * Student learning plan member variable
     */
    private StudentEdPlan m_studentLearningPlan = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());
        if (m_studentLearningPlan != null) {
            addParameter(PARAM_STUDENT, m_studentLearningPlan.getStudent());
            addParameter(PARAM_STUDENT_ED_PLAN, m_studentLearningPlan);

            int age = m_studentLearningPlan.getStudent().getPerson().getAge();
            addParameter(PARAM_STUDENT_DOB, Integer.toString(age));


            // Build meeting participants list and add support for the goals sub-report
            Report goalsSubreport = ReportUtils.getReport(GOALS_SUBREPORT_ID, getBroker());
            JRDataSource goalsDataSource = loadGoals();
            addParameter(PARAM_GOALS_SUBREPORT_FORMAT, new ByteArrayInputStream(goalsSubreport.getCompiledFormat()));
            addParameter(PARAM_GOALS_SUBREPORT_DATA, goalsDataSource);

            // Build meeting participants list and add support for the participants sub-report
            Report participantsSubreport = ReportUtils.getReport(PARTICIPANTS_SUBREPORT_ID, getBroker());
            JRDataSource participantsDataSource = loadParticipantsList();
            addParameter(PARAM_PARTICIPANTS_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(participantsSubreport.getCompiledFormat()));
            addParameter(PARAM_PARTICIPANTS_SUBREPORT_DATA, participantsDataSource);
        }

        return new SimpleBeanDataSource(m_studentLearningPlan, dictionary, getLocale());
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormDefinition() != null) {
            super.initialize();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        /*
         * If we're outside the context of a form, set the form storage, owner, and dictionary.
         */
        if (getFormDefinition() == null) {
            m_studentLearningPlan = userData.getCurrentRecord(StudentEdPlan.class);

            if (m_studentLearningPlan != null) {
                setFormOwner(m_studentLearningPlan);
                setFormStorage(m_studentLearningPlan);

                ExtendedDataDictionary extendedDictionary = m_studentLearningPlan.getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());

                setDictionary(dictionary);
            }
        }
    }

    /**
     * Returns a bean collection data source of student learning plan goals for the sub-report for
     * the current student learning plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadGoals() {
        Collection<StudentEdPlanGoal> allGoals = new ArrayList<StudentEdPlanGoal>();
        if (m_studentLearningPlan != null && !CollectionUtils.isEmpty(m_studentLearningPlan.getStudentEdPlanGoals())) {
            allGoals = m_studentLearningPlan.getStudentEdPlanGoals();
        }

        return new BeanCollectionDataSource(allGoals, true, getDictionary(), getLocale());
    }

    /**
     * Returns a bean collection data source of latest meeting participants for the sub-report for
     * the current student learning plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadParticipantsList() {
        Collection<StudentEdPlanMeetingParticipant> participants = new ArrayList<StudentEdPlanMeetingParticipant>();

        // Build meeting participants list
        if (m_studentLearningPlan != null) {
            StudentEdPlanMeeting lastMeeting = m_studentLearningPlan.getLastMeeting(getBroker());
            if (lastMeeting != null) {
                addParameter(PARAM_MEETING_DATE, lastMeeting.getDate());
                participants = lastMeeting.getStudentEdPlanMeetingParticipants(getBroker());
            }
        }

        return new BeanCollectionDataSource(participants, true, getDictionary(), getLocale());
    }
}
