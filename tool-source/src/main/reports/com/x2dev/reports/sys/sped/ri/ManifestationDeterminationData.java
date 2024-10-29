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
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the RI Special Education Manifestation Review form.
 *
 * This form is owned by Student and stored in GenericFormData.
 *
 * @author X2 Development Corporation
 */
public class ManifestationDeterminationData extends BaseFormReportJavaSource {
    private static final String ALIAS_INCIDENT_OID = "mft-incident-oid";
    private static final String ALIAS_SERVICE_DESCRIPTION = "service-description";
    private static final String ALIAS_TEAM_ROLE = "mft-team-role";
    private static final String DATASOURCE_ATTENDEE_NAME = "meetingAttendees";
    private static final String DATASOURCE_ATTENDEE_ROLE = "meetingAttendeesRoles";

    private static final String PARAM_INCIDENT_DATE = "incidentDate";
    private static final String PARAM_INCIDENT_TIME = "incidentTime";
    private static final String PARAM_INCIDENT_LOCATION = "incidentLocation";
    private static final String PARAM_INCIDENT_BEHAVIOR = "incidentBehavior";
    private static final String PARAM_INCIDENT_DESCRIPTION = "incidentDescription";
    private static final String PARAM_DISABILITY = "disability";
    private static final String PARAM_GOALS_SERVICES = "goalsServices";
    private static final String PARAM_DISCIPLINE = "disciplineHistory";
    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_MEETING_ATTENDEES = "meetingAttendees";
    private static final String PARAM_MEETING_ROLES = "meetingAttendeesRoles";
    private static final String PARAM_MEETING_SIGNATURE = "meetingAttendeesSig";
    private static final String PARAM_MEETING_AGREE = "meetingAttendeesAgree";

    private static final String INPUT_PARAM_COPY_TEAM = "copyTeam";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        SimpleFormDataSource dataSource = null;
        SisStudent student = (SisStudent) getFormOwner();
        GenericFormData gfd = (GenericFormData) getFormStorage();
        AppGlobals.getLog().log(Level.INFO, " manifest std:" + student);
        if (student != null && gfd != null) {
            IepData iep = student.getActiveIep();
            AppGlobals.getLog().log(Level.INFO, " manifest iep:" + iep);
            loadIncident(gfd);
            loadIepInfo(iep);
            loadConduct(student);
            loadMeetingAttendees(gfd, iep);
        }
        Date formDate = null;
        if (getFormInstance() != null) {
            long formCreateTime = getFormInstance().getCreatedTime();
            formDate = new Date(formCreateTime);
        } else {
            formDate = new Date();
        }
        addParameter(PARAM_MEETING_DATE, formDate);

        dataSource = new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Gather Student conduct counts.
     *
     * @param student
     */
    private void loadConduct(SisStudent student) {
        List<String> suspensionCodes = new ArrayList<String>();
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        ReferenceTable refTable = field.getReferenceTable();
        for (ReferenceCode refCode : refTable.getReferenceCodes(getBroker())) {
            if ("B".equals(refCode.getStateCode())) {
                suspensionCodes.add(refCode.getCode());
            }
        }
        StringBuilder builder = new StringBuilder();
        int count = 0;
        int suspension = 0;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ConductIncident.COL_STUDENT_OID, student.getOid());
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                student.getOrganization1().getCurrentContext().getStartDate());
        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);
        QueryIterator<ConductIncident> iterator = getBroker().getIteratorByQuery(query);
        while (iterator.hasNext()) {
            ConductIncident incident = iterator.next();
            for (ConductAction action : incident.getConductActions(getBroker())) {
                if (suspensionCodes.contains(action.getActionCode())) {
                    suspension += action.getActionPenaltyTime().intValue();
                }
            }

            count++;
        }
        if (count > 0) {
            builder.append(Integer.toString(count)).append(" conduct referrals");
            if (suspension > 0) {
                builder.append("\n").append(Integer.toString(suspension)).append(" days Out-of-School suspension");
            }
        }
        addParameter(PARAM_DISCIPLINE, builder.toString());
    }

    /**
     * Load conduct incident information from the specified incident.
     *
     * @param gfd
     */
    private void loadIncident(GenericFormData gfd) {
        DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy", getLocale());
        DateFormat timeFormat = new SimpleDateFormat("hh:mm a", getLocale());
        String incidentOid = (String) gfd.getFieldValueByAlias(ALIAS_INCIDENT_OID, getDictionary());
        if (!StringUtils.isEmpty(incidentOid)) {
            ConductIncident incident = getBroker().getBeanByOid(ConductIncident.class, incidentOid);
            if (incident != null) {
                addParameter(PARAM_INCIDENT_DATE, dateFormat.format(incident.getIncidentDate()));
                addParameter(PARAM_INCIDENT_TIME, timeFormat.format(incident.getIncidentTime()));
                addParameter(PARAM_INCIDENT_LOCATION, incident.getIncidentLocation());
                addParameter(PARAM_INCIDENT_DESCRIPTION, incident.getDescription());
                DataDictionaryField field = getDictionary().findDataDictionaryField(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);
                ReferenceCode refCode = ReferenceManager.getCode(field.getReferenceTableOid(),
                        incident.getIncidentCode(), null, getBroker());
                String behavior = incident.getIncidentCode() + " " + refCode.getStateCode();
                addParameter(PARAM_INCIDENT_BEHAVIOR, behavior);
            }
        }
    }

    /**
     * Gather goal, service and disability information from the current active IEP.
     *
     * @param iep
     */
    private void loadIepInfo(IepData iep) {
        if (iep != null) {
            DataDictionary iepDictionary = DataDictionary.getDistrictDictionary(iep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            DataDictionaryField serviceDescriptionField =
                    iepDictionary.findDataDictionaryFieldByAlias(ALIAS_SERVICE_DESCRIPTION);
            // Get goals and services
            List<String> goalsServices = new ArrayList<String>();
            Collection<IepGoal> goals = iep.getIepGoals(getBroker());
            for (IepGoal goal : goals) {
                if (!StringUtils.isEmpty(goal.getGoal())) {
                    goalsServices.add(goal.getGoal());
                }
            }
            if (serviceDescriptionField != null) {
                Collection<IepService> services = iep.getIepServices(getBroker());
                for (IepService service : services) {
                    String description =
                            (String) service.getFieldValueByBeanPath(serviceDescriptionField.getJavaName());
                    if (!StringUtils.isEmpty(description)) {
                        goalsServices.add(description);
                    }
                }
            }
            addParameter(PARAM_GOALS_SERVICES, StringUtils.convertCollectionToDelimitedString(goalsServices, "\n"));

            // Get disabilities
            List<String> disabilitiesList = new ArrayList<String>();
            Collection<IepDisability> disabilities = iep.getIepDisability(getBroker());
            for (IepDisability disability : disabilities) {
                disabilitiesList.add(disability.getDisabilityCode());
            }
            addParameter(PARAM_DISABILITY, StringUtils.convertCollectionToDelimitedString(disabilitiesList, "\n"));
        }
    }

    /**
     * Gather meeting date and attendees from the latest meeting on the active IEP.
     * Team is held in Generic Form Child table.
     *
     * If the "copy team" option is set, go through IEP team members and copy them
     * into the generic form child table.
     *
     * @param iep
     */
    private void loadMeetingAttendees(GenericFormData gfd, IepData iep) {
        Boolean copyTeam = (Boolean) getParameter(INPUT_PARAM_COPY_TEAM);
        DataDictionary gfdDictionary = DataDictionary.getDistrictDictionary(gfd.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        Collection<GenericFormChildData> gfcs = null;

        // If copy is set, copy members from the IEP meeting.
        if (iep != null && !StringUtils.isEmpty(gfd.getOid()) &&
                copyTeam != null && copyTeam.booleanValue()) {

            gfcs = gfd.getGenericFormDataChildren(getBroker());
            Collection<String> gfcPersonOids =
                    CollectionUtils.getPropertyCollection(gfcs, GenericFormChildData.COL_PERSON_OID);

            IepMeeting meeting = null;
            Collection<IepMeeting> meetings = iep.getIepMeeting(getBroker());
            for (IepMeeting img : meetings) {
                if (meeting == null || meeting.getDate().before(img.getDate())) {
                    meeting = img;
                }
            }
            if (meeting != null) {
                Collection<IepMeetingAttendance> attendance = meeting.getMeetingAttendance(getBroker());
                for (IepMeetingAttendance ima : attendance) {
                    IepTeamMember itm = ima.getTeamMember();
                    if (itm != null && itm.getPersonOid() != null && !gfcPersonOids.contains(itm.getPersonOid())) {
                        GenericFormChildData child =
                                X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());
                        child.setGenericFormDataOid(gfd.getOid());
                        child.setPersonOid(itm.getPersonOid());
                        child.setFieldValueByAlias(ALIAS_TEAM_ROLE, itm.getMemberRoleCode(), gfdDictionary);
                        getBroker().saveBean(child);
                        gfcPersonOids.add(itm.getPersonOid());
                    }
                }
            }
        }

        // Load team members from the generic form child table.
        gfcs = gfd.getGenericFormDataChildren(getBroker());
        Map<String, String> data = new TreeMap<>();

        for (GenericFormChildData gfc : gfcs) {
            Person psn = gfc.getPerson();
            if (psn != null && psn.getNameView() != null) {
                data.put(psn.getNameView(), (String) gfc.getFieldValueByAlias(ALIAS_TEAM_ROLE, gfdDictionary));
            }
        }

        addParameter(PARAM_MEETING_ATTENDEES, new JRBeanCollectionDataSource(data.keySet()));
        addParameter(PARAM_MEETING_ROLES, new JRBeanCollectionDataSource(data.values()));
        addParameter(PARAM_MEETING_SIGNATURE, new JRBeanCollectionDataSource(data.keySet()));
        addParameter(PARAM_MEETING_AGREE, new JRBeanCollectionDataSource(data.keySet()));

    }
}
