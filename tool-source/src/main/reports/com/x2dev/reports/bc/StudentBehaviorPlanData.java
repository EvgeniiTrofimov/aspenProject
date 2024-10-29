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
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEdPlanEvaluation;
import com.x2dev.sis.model.beans.StudentEdPlanMeeting;
import com.x2dev.sis.model.beans.StudentEdPlanMeetingParticipant;
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
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class StudentBehaviorPlanData.
 *
 * @author Follett Software Company
 */
public class StudentBehaviorPlanData extends BaseFormReportJavaSource {
    /**
     * Serial Version
     */
    private static final long serialVersionUID = 1L;

    /**
     * ID of the planning foundations, antecedents, responses, and participants sub-reports.
     */
    public static final String FOUNDATIONS_SUBREPORT_ID = "BC-SBP-001-SUB1";
    public static final String ANTECEDENTS_SUBREPORT_ID = "BC-SBP-001-SUB2";
    public static final String RESPONSES_SUBREPORT_ID = "BC-SBP-001-SUB3";
    public static final String PARTICIPANTS_SUBREPORT_ID = "BC-SBP-001-SUB4";

    /**
     * Parameter containing the data for the foundations, antecedents, responses, and participants
     * sub-reports.
     */
    public static final String PARAM_FOUNDATIONS_SUBREPORT_DATA = "planningFoundationsData";
    public static final String PARAM_ANTECEDENTS_SUBREPORT_DATA = "antecedentsData";
    public static final String PARAM_RESPONSES_SUBREPORT_DATA = "behaviorsResponsesData";
    public static final String PARAM_PARTICIPANTS_SUBREPORT_DATA = "participantsData";

    /**
     * Parameter containing the format for the foundations, antecedents, responses, and participants
     * sub-reports.
     */
    public static final String PARAM_FOUNDATIONS_SUBREPORT_FORMAT = "planningFoundationsFormat";
    public static final String PARAM_ANTECEDENTS_SUBREPORT_FORMAT = "antecedentsFormat";
    public static final String PARAM_RESPONSES_SUBREPORT_FORMAT = "behaviorsResponsesFormat";
    public static final String PARAM_PARTICIPANTS_SUBREPORT_FORMAT = "participantsFormat";

    /**
     * Parameter constants
     */
    public static final String PARAM_STUDENT = "student";
    public static final String PARAM_STUDENT_ED_PLAN = "studentEdPlan";
    public static final String PARAM_STUDENT_DOB = "studentDOB";
    public static final String PARAM_CASE_MANAGER = "caseManager";
    public static final String PARAM_MEETING_DATE = "meetingDate";

    /**
     * Alias constants
     */
    public static final String ALIAS_PLANNING_FOUNDATIONS_STRENGTHS = "sbp-foundation-strengths";
    public static final String ALIAS_PLANNING_FOUNDATIONS_CONCERNS = "sbp-foundation-concerns";
    public static final String ALIAS_BEHAVIOR = "sbp-behavior";
    public static final String ALIAS_RESPONSE = "sbp-response";

    /**
     * Student learning plan member variable
     */
    private StudentEdPlan m_studentBehaviorPlan = null;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

        if (m_studentBehaviorPlan != null) {
            addParameter(PARAM_STUDENT, m_studentBehaviorPlan.getStudent());
            addParameter(PARAM_STUDENT_ED_PLAN, m_studentBehaviorPlan);
            addParameter(PARAM_CASE_MANAGER,
                    m_studentBehaviorPlan.getStaff2() != null ? m_studentBehaviorPlan.getStaff2().getNameView() : "");

            // Add parameter for Age
            int age = m_studentBehaviorPlan.getStudent().getPerson().getAge();
            addParameter(PARAM_STUDENT_DOB, Integer.toString(age));

            // Add support for the planning foundations sub-report
            Report foundationsSubreport = ReportUtils.getReport(FOUNDATIONS_SUBREPORT_ID, getBroker());
            JRDataSource foundationsDataSource = loadPlanningFoundations();
            addParameter(PARAM_FOUNDATIONS_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(foundationsSubreport.getCompiledFormat()));
            addParameter(PARAM_FOUNDATIONS_SUBREPORT_DATA, foundationsDataSource);

            // Add support for the antecedents / setting events and strategies sub-report
            Report antecedentsSubreport = ReportUtils.getReport(ANTECEDENTS_SUBREPORT_ID, getBroker());
            JRDataSource antecedentsDataSource = loadAntecedents();
            addParameter(PARAM_ANTECEDENTS_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(antecedentsSubreport.getCompiledFormat()));
            addParameter(PARAM_ANTECEDENTS_SUBREPORT_DATA, antecedentsDataSource);

            // Add support for the behaviors and responses sub-report
            Report responsesSubreport = ReportUtils.getReport(RESPONSES_SUBREPORT_ID, getBroker());
            JRDataSource responsesDataSource = loadResponses();
            addParameter(PARAM_RESPONSES_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(responsesSubreport.getCompiledFormat()));
            addParameter(PARAM_RESPONSES_SUBREPORT_DATA, responsesDataSource);

            // Add support for the participants sub-report
            Report participantsSubreport = ReportUtils.getReport(PARTICIPANTS_SUBREPORT_ID, getBroker());
            JRDataSource participantsDataSource = loadParticipantsList();
            addParameter(PARAM_PARTICIPANTS_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(participantsSubreport.getCompiledFormat()));
            addParameter(PARAM_PARTICIPANTS_SUBREPORT_DATA, participantsDataSource);
        }

        return new SimpleBeanDataSource(m_studentBehaviorPlan, dictionary, getLocale());
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
            m_studentBehaviorPlan = userData.getCurrentRecord(StudentEdPlan.class);

            if (m_studentBehaviorPlan != null) {
                setFormOwner(m_studentBehaviorPlan);
                setFormStorage(m_studentBehaviorPlan);

                ExtendedDataDictionary extendedDictionary = m_studentBehaviorPlan.getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());

                setDictionary(dictionary);
            }
        }
    }

    /**
     * Returns a bean collection data source of the antecedents/setting events and strategies for
     * the sub-report for
     * the current student behavior plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadAntecedents() {
        Collection<IepAccommodation> antecedents = new ArrayList<IepAccommodation>();

        // Build the 'behaviors and responses' list
        if (m_studentBehaviorPlan != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepAccommodation.COL_STUDENT_ED_PLAN_OID, m_studentBehaviorPlan.getOid());

            QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);
            antecedents = getBroker().getCollectionByQuery(query);
        }

        return new BeanCollectionDataSource(antecedents, true, getDictionary(), getLocale());
    }

    /**
     * Returns a bean collection data source of latest meeting participants for the sub-report for
     * the current student behavior plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadParticipantsList() {
        Collection<StudentEdPlanMeetingParticipant> participants = new ArrayList<StudentEdPlanMeetingParticipant>();

        // Build meeting participants list
        if (m_studentBehaviorPlan != null) {
            StudentEdPlanMeeting lastMeeting = m_studentBehaviorPlan.getLastMeeting(getBroker());
            if (lastMeeting != null) {
                addParameter(PARAM_MEETING_DATE, lastMeeting.getDate());
                participants = lastMeeting.getStudentEdPlanMeetingParticipants(getBroker());
            }
        }

        return new BeanCollectionDataSource(participants, true, getDictionary(), getLocale());
    }

    /**
     * Returns a bean collection data source of the behaviors and responses for the sub-report for
     * the current student behavior plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadPlanningFoundations() {
        Collection<StudentEdPlanEvaluation> responses = new ArrayList<StudentEdPlanEvaluation>();

        // Build the 'behaviors and responses' list
        if (m_studentBehaviorPlan != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEdPlanEvaluation.COL_STUDENT_ED_PLAN_OID, m_studentBehaviorPlan.getOid());
            criteria.addNotNull(
                    getDictionary().findDataDictionaryFieldByAlias(ALIAS_PLANNING_FOUNDATIONS_STRENGTHS).getJavaName());
            criteria.addNotNull(
                    getDictionary().findDataDictionaryFieldByAlias(ALIAS_PLANNING_FOUNDATIONS_CONCERNS).getJavaName());

            QueryByCriteria query = new QueryByCriteria(StudentEdPlanEvaluation.class, criteria);
            responses = getBroker().getCollectionByQuery(query);
        }

        return new BeanCollectionDataSource(responses, true, getDictionary(), getLocale());
    }

    /**
     * Returns a bean collection data source of the behaviors and responses for the sub-report for
     * the current student behavior plan.
     *
     * @return BeanCollectionDataSource
     */
    private BeanCollectionDataSource loadResponses() {
        Collection<StudentEdPlanEvaluation> responses = new ArrayList<StudentEdPlanEvaluation>();

        // Build the 'behaviors and responses' list
        if (m_studentBehaviorPlan != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEdPlanEvaluation.COL_STUDENT_ED_PLAN_OID, m_studentBehaviorPlan.getOid());
            criteria.addNotNull(getDictionary().findDataDictionaryFieldByAlias(ALIAS_BEHAVIOR).getJavaName());
            criteria.addNotNull(getDictionary().findDataDictionaryFieldByAlias(ALIAS_RESPONSE).getJavaName());

            QueryByCriteria query = new QueryByCriteria(StudentEdPlanEvaluation.class, criteria);
            responses = getBroker().getCollectionByQuery(query);
        }

        return new BeanCollectionDataSource(responses, true, getDictionary(), getLocale());
    }
}
