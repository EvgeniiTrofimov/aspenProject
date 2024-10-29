/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEdPlanGoal;
import com.x2dev.sis.model.beans.StudentEdPlanMeeting;
import com.x2dev.sis.model.beans.StudentEdPlanMeetingParticipant;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the RI Special Education Behavioral Intervention Plan form.
 *
 * This form is owned by Student and stored in GenericFormData.
 *
 * @author X2 Development Corporation
 */
public class BehavioralInterventionPlanData extends ReportJavaSourceNet {

    private static final String ALIAS_OBJECTIVE = "ribip-student-objectives";
    private static final String ALIAS_TEAM_NAME = "ribip-team-member-name";
    private static final String ALIAS_TEAM_ROLE = "ribip-team-member-role";

    private static final String DATASOURCE_ATTENDEE_NAME = "meetingAttendees";
    private static final String DATASOURCE_ATTENDEE_ROLE = "meetingAttendeesRoles";
    private static final String DATASOURCE_ATTENDEE_SIGNATURE = "meetingAttendeesSig";
    private static final String DATASOURCE_OBJECTIVES = "objectives";

    private static final String PARAM_ATTENDANCE = "attendance";
    private static final String PARAM_CONDUCT = "conduct";

    private StudentEdPlan m_BIPPlan = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());

        SisStudent student = m_BIPPlan.getStudent();
        if (student != null) {
            loadAttendance(student);
            loadConduct(student);
        }
        loadObjectives(m_BIPPlan, dictionary);
        loadMeetingAttendees(m_BIPPlan);

        return new SimpleBeanDataSource(m_BIPPlan, dictionary, getLocale());
    }

    /**
     * Gather Student attendance counts.
     *
     * @param student
     */
    private void loadAttendance(SisStudent student) {
        int absent = 0;
        int tardy = 0;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE,
                student.getOrganization1().getCurrentContext().getStartDate());
        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        Collection<StudentAttendance> attendances = getBroker().getCollectionByQuery(query);
        for (StudentAttendance attendance : attendances) {
            if (!attendance.getExcusedIndicator()) {
                if (attendance.getAbsentIndicator()) {
                    absent++;
                } else if (attendance.getTardyIndicator()) {
                    tardy++;
                }
            }
        }
        StringBuilder builder = new StringBuilder();
        if (absent > 0) {
            builder.append(Integer.toString(absent)).append(" Unexcused Absences");
        }
        if (tardy > 0) {
            if (absent > 0) {
                builder.append("\n");
            }
            builder.append(Integer.toString(tardy)).append(" Tardies");
        }
        addParameter(PARAM_ATTENDANCE, builder.toString());
    }

    /**
     * Gather Student conduct counts.
     *
     * @param student
     */
    private void loadConduct(SisStudent student) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ConductIncident.COL_STUDENT_OID, student.getOid());
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                student.getOrganization1().getCurrentContext().getStartDate());
        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);
        int conduct = getBroker().getCount(query);
        StringBuilder builder = new StringBuilder();
        if (conduct > 0) {
            builder.append(Integer.toString(conduct)).append(" Conduct referrals");
        }
        addParameter(PARAM_CONDUCT, builder.toString());
    }

    /**
     * Gather meeting attendees from the Generic Form child table with type "team".
     *
     * @param plan
     */
    private void loadMeetingAttendees(StudentEdPlan plan) {
        Map<String, String> data = new TreeMap<>();
        StudentEdPlanMeeting meeting = plan.getLastMeeting(getBroker());
        if (meeting != null) {
            Collection<StudentEdPlanMeetingParticipant> children =
                    meeting.getStudentEdPlanMeetingParticipants(getBroker());
            for (StudentEdPlanMeetingParticipant child : children) {
                String name = null;
                if (child.getPerson() != null) {
                    name = child.getPerson().getFirstName() + " " + child.getPerson().getLastName();
                }
                String role = child.getRoleCode();
                if (name != null) {
                    data.put(name, role);
                }
            }
            if (data.size() == 0) {
                data.put(" ", " ");
            }
        }
        addParameter(DATASOURCE_ATTENDEE_NAME, new JRBeanCollectionDataSource(data.keySet()));
        addParameter(DATASOURCE_ATTENDEE_ROLE, new JRBeanCollectionDataSource(data.values()));
        addParameter(DATASOURCE_ATTENDEE_SIGNATURE, new JRBeanCollectionDataSource(data.keySet()));
    }

    /**
     * Gather objectives from the Ed Plan Goals child table.
     *
     * @param sep
     * @param dictionary
     */
    private void loadObjectives(StudentEdPlan sep, DataDictionary dictionary) {
        List<String> data = new ArrayList<String>();
        DataDictionaryField objectiveField = dictionary.findDataDictionaryFieldByAlias(ALIAS_OBJECTIVE);
        Collection<StudentEdPlanGoal> children = sep.getStudentEdPlanGoals(getBroker());
        for (StudentEdPlanGoal goal : children) {
            String objective = (String) goal.getFieldValueByBeanPath(objectiveField.getJavaName());
            if (!StringUtils.isEmpty(objective)) {
                data.add(objective);
            }
        }
        if (data.size() == 0) {
            data.add("");
        }
        addParameter(DATASOURCE_OBJECTIVES, new JRBeanCollectionDataSource(data));
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
        super.saveState(userData);
        m_BIPPlan = userData.getCurrentRecord(StudentEdPlan.class);
    }
}
