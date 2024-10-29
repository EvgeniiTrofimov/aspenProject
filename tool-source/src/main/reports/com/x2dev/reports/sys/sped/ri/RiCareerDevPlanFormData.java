/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports5.engine.data.JRMapCollectionDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class RiCareerDevPlanFormData extends BaseFormReportJavaSource {

    private static final long serialVersionUID = 1L;

    private static final String PARAM_AGE = "age";
    private static final String PARAM_TEAM_LIST = "teamList";
    private static final String PARAM_MEMBER_NAME = "name";
    private static final String PARAM_MEMBER_ROLE = "role";
    private static final String PARAM_MEMBER_INVITE = "invited";
    private static final String PARAM_MEMBER_ATTEND = "attended";

    private static final String EMBEDDED_FILTER_FIELD = "crrpln-ch-embedded-type";

    private static final String CHILD_TYPE_TRANS_ASSESS = "Transition Assessments";
    private static final String CHILD_PARAM_TRANS_ASSESS = "transitionAssessments";
    private static final String[] CHILD_FIELDS_TRANS_ASSESS = new String[] {"crrpln-ch-method",
            "crrpln-ch-date1"};

    private static final String CHILD_TYPE_ASSESS_RESULTS = "Assessment Results";
    private static final String CHILD_PARAM_ASSESS_RESULTS = "assessmentResults";
    private static final String[] CHILD_FIELDS_ASSESS_RESULTS = new String[] {"crrpln-ch-assessment-results",
            "crrpln-ch-barriers-needs"};

    private static final String CHILD_TYPE_ACTIVITIES = "Activities";
    private static final String[] CHILD_FIELDS_ACTIVITIES = new String[] {"crrpln-ch-activity-task",
            "crrpln-ch-person-responsible",
            "crrpln-ch-completed",
            "crrpln-ch-progress"};

    private static final String CHILD_TYPE_WORK_EXP = "Work Experiences";
    private static final String CHILD_PARAM_WORK_EXP = "workExperiences";
    private static final String[] CHILD_FIELDS_WORK_EXP = new String[] {"crrpln-ch-experience-type",
            "crrpln-ch-location",
            "crrpln-ch-based-pcp",
            "crrpln-ch-anticipated-dates"};

    private static final String CHILD_TYPE_COMMUN_ACT = "Community Activities";
    private static final String CHILD_PARAM_COMMUN_ACT = "communityActivities";
    private static final String[] CHILD_FIELDS_COMMUN_ACT = new String[] {"crrpln-ch-location",
            "crrpln-ch-person-responsible"};

    private static final String CHILD_TYPE_CAREER_CLUSTER_1 = "CareerCluster1";
    private static final String CHILD_TYPE_CAREER_CLUSTER_2 = "CareerCluster2";
    private static final String CHILD_TYPE_CAREER_CLUSTER_3 = "CareerCluster3";
    private static final String[] CHILD_FIELDS_CAREER_CLUSTER = new String[] {"crrpln-ch-location",
            "crrpln-ch-method",
            "crrpln-ch-barriers-needs",
            "crrpln-ch-assessment-results"};

    private static final String CHILD_TYPE_INT_WORK = "Integrated Work";
    private static final String CHILD_PARAM_INT_WORK = "integratedWork";
    private static final String[] CHILD_FIELDS_INT_WORK = new String[] {"crrpln-ch-location",
            "crrpln-ch-activity-task",
            "crrpln-ch-anticipated-dates",
            "crrpln-ch-total-days"};

    private static final String CHILD_TYPE_TRANSPORT = "Transportation";
    private static final String[] CHILD_FIELDS_TRANSPORT = new String[] {"crrpln-ch-location",
            "crrpln-ch-person-responsible",
            "crrpln-ch-anticipated-dates"};

    private static final String CHILD_TYPE_GOALS = "Employment Goal";
    private static final String CHILD_PARAM_GOALS = "goals";
    private static final String[] CHILD_FIELDS_GOALS = new String[] {"crrpln-ch-activity-task",
            "crrpln-ch-experience-type",
            "crrpln-ch-person-responsible",
            "crrpln-ch-barriers-needs",
            "crrpln-ch-paid"};

    private static final String CHILD_TYPE_PLACEMENT = "Job Placement";
    private static final String CHILD_PARAM_PLACEMENT = "placement";
    private static final String[] CHILD_FIELDS_PLACEMENT = new String[] {"crrpln-ch-activity-task",
            "crrpln-ch-person-responsible",
            "crrpln-ch-anticipated-dates",
            "crrpln-ch-barriers-needs"};

    private static final String CHILD_TYPE_DEVELOPMENT = "Job Development";
    private static final String CHILD_PARAM_DEVELOPMENT = "development";
    private static final String[] CHILD_FIELDS_DEVELOPMENT = new String[] {"crrpln-ch-location",
            "crrpln-ch-activity-task",
            "crrpln-ch-barriers-needs",
            "crrpln-ch-date1"};

    private boolean m_isBlank = false;
    private String m_fieldType;
    private DateAsStringConverter m_format;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        if (getFormStorage() == null || StringUtils.isEmpty(getFormStorage().getOid())) {
            m_isBlank = true;
        }
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(EMBEDDED_FILTER_FIELD);
        m_fieldType = field.getJavaName();

        IepData iep = (IepData) getFormOwner();
        prepareAge(iep);
        prepareTeam(iep);

        m_format = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(),
                true);
        GenericFormData data = (GenericFormData) getFormStorage();
        Collection<GenericFormChildData> childData = data.getGenericFormDataChildren(getBroker());
        prepareChildSet(childData, CHILD_TYPE_ACTIVITIES, CHILD_FIELDS_ACTIVITIES, CHILD_TYPE_ACTIVITIES);
        prepareChildSet(childData, CHILD_TYPE_ASSESS_RESULTS, CHILD_FIELDS_ASSESS_RESULTS, CHILD_PARAM_ASSESS_RESULTS);
        prepareChildSet(childData, CHILD_TYPE_CAREER_CLUSTER_1, CHILD_FIELDS_CAREER_CLUSTER,
                CHILD_TYPE_CAREER_CLUSTER_1);
        prepareChildSet(childData, CHILD_TYPE_CAREER_CLUSTER_2, CHILD_FIELDS_CAREER_CLUSTER,
                CHILD_TYPE_CAREER_CLUSTER_2);
        prepareChildSet(childData, CHILD_TYPE_CAREER_CLUSTER_3, CHILD_FIELDS_CAREER_CLUSTER,
                CHILD_TYPE_CAREER_CLUSTER_3);
        prepareChildSet(childData, CHILD_TYPE_COMMUN_ACT, CHILD_FIELDS_COMMUN_ACT, CHILD_PARAM_COMMUN_ACT);
        prepareChildSet(childData, CHILD_TYPE_DEVELOPMENT, CHILD_FIELDS_DEVELOPMENT, CHILD_PARAM_DEVELOPMENT);
        prepareChildSet(childData, CHILD_TYPE_GOALS, CHILD_FIELDS_GOALS, CHILD_PARAM_GOALS);
        prepareChildSet(childData, CHILD_TYPE_INT_WORK, CHILD_FIELDS_INT_WORK, CHILD_PARAM_INT_WORK);
        prepareChildSet(childData, CHILD_TYPE_PLACEMENT, CHILD_FIELDS_PLACEMENT, CHILD_PARAM_PLACEMENT);
        prepareChildSet(childData, CHILD_TYPE_TRANSPORT, CHILD_FIELDS_TRANSPORT, CHILD_TYPE_TRANSPORT);
        prepareChildSet(childData, CHILD_TYPE_TRANS_ASSESS, CHILD_FIELDS_TRANS_ASSESS, CHILD_PARAM_TRANS_ASSESS);
        prepareChildSet(childData, CHILD_TYPE_WORK_EXP, CHILD_FIELDS_WORK_EXP, CHILD_PARAM_WORK_EXP);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Return the age of the student as of today.
     *
     * @param iep
     *
     * @return Integer
     */
    private void prepareAge(IepData iep) {
        Integer age = (Integer) getParameter(PARAM_AGE);
        if ((age == null || age.intValue() == 0) && iep != null) {
            Student student = iep.getStudent();
            if (student != null) {
                Person person = student.getPerson();
                age = person.getAgeAsOfDate(getPlainDate());
                addParameter(PARAM_AGE, Integer.valueOf(age));
            }
        }
    }

    /**
     * Lookup the IEP team members for the CDP Team.
     *
     * @param iep
     */
    private void prepareTeam(IepData iep) {
        Collection<Map<String, ?>> teamMembers = new ArrayList<Map<String, ?>>();
        if (!m_isBlank) {
            Collection<IepTeamMember> team = iep.getTeamMembers(getBroker());
            for (IepTeamMember member : team) {
                String name = member.getNameView();
                String role = member.getMemberRoleCode();
                Boolean invited = Boolean.FALSE;
                Boolean attended = Boolean.FALSE;
                Collection<IepMeetingAttendance> attendance = member.getMeetingAttendance(getBroker());
                for (IepMeetingAttendance att : attendance) {
                    invited = Boolean.TRUE;
                    if (att.getPresentIndicator()) {
                        attended = Boolean.TRUE;
                    }
                }
                Map<String, Object> teamMember = new HashMap<String, Object>();
                teamMember.put(PARAM_MEMBER_NAME, name);
                teamMember.put(PARAM_MEMBER_ROLE, role);
                teamMember.put(PARAM_MEMBER_INVITE, invited);
                teamMember.put(PARAM_MEMBER_ATTEND, attended);
                teamMembers.add(teamMember);
            }
        }
        if (teamMembers.size() == 0) {
            teamMembers.add(new HashMap<String, Object>());
        }
        addParameter(PARAM_TEAM_LIST, new JRMapCollectionDataSource(teamMembers));
    }

    /**
     * Prepare a collection of Maps of child data from specified child types.
     * Find child records of type, retrieve fields from it and add them to Maps.
     * Set the list of maps as a parameter for lists in the report.
     *
     * @param childData
     * @param childType
     * @param fields
     * @param parameterName
     */
    private void prepareChildSet(Collection<GenericFormChildData> childData,
                                 String childType,
                                 String[] fields,
                                 String parameterName) {
        Collection<Map<String, ?>> childCollection = new ArrayList<Map<String, ?>>();
        for (GenericFormChildData child : childData) {
            String type = (String) child.getFieldValueByBeanPath(m_fieldType);
            if (childType.equals(type)) {
                Map<String, Object> row = new HashMap<String, Object>();
                for (String field : fields) {
                    Object value = child.getFieldValueByAlias(field, getDictionary());
                    // For the Date aliased field, convert to user display format.
                    if ("crrpln-ch-date1".equals(field) && value != null) {
                        value = m_format.javaToString(value);
                    }
                    row.put(field, value);
                }
                childCollection.add(row);
            }
        }
        if (childCollection.size() == 0) {
            childCollection.add(new HashMap<String, Object>());
        }
        addParameter(parameterName, new JRMapCollectionDataSource(childCollection));
    }
}
