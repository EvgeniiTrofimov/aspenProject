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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;

/**
 * RI Special Education data cleanup procedure. This procedure attempts to fix common problems
 * with special education census reporting fields.
 * <p>
 * The IEP Service Program Continuum and Service Cost Center fields are calculated based on the
 * IEP's placement and early childhood placement fields, as well as the presence (or lack) of
 * extended school year (ESY) services.
 * <p>
 * Meeting data issues are also addressed. If the IEP does not have a meeting date, or the meeting
 * date does not match the date on one of the child IEP_MEETING records, the date of the most recent
 * IEP meeting is set on the IEP. In addition, if at least one of the related attendance records
 * does not have the "present" flag set, the flag is set for all records not marked as excusals.
 *
 * @author mmastrangelo
 */
public class RISpecialEdDataCleanup extends ProcedureJavaSource {
    private static final String UPDATE_COST_CENTER_PARAM = "updateCostCenter";
    private static final String UPDATE_PROGRAM_CONTINUUM_PARAM = "updateProgramContinuum";
    private static final String UPDATE_MEETING_DATE_PARAM = "updateMeetingDate";
    private static final String UPDATE_MEETING_ATTENDANCE_PARAM = "updateMeetingAttendance";

    private static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";
    private static final String PROGRAM_CONTINUUM_ALIAS = "iep-service-program-continuum";
    private static final String COST_CENTER_ALIAS = "iep-service-cost-center";

    private static final String PLACEMENT_ALIAS = "placement";
    private static final String PLACEMENT_EC_ALIAS = "placement-ec";

    private DataDictionary m_dictionary;
    private Map<String, String> m_placementContinuumMapping;
    private Map<String, String> m_placementEcContinuumMapping;
    private Map<String, String> m_placementCostCenterMapping;
    private Map<String, String> m_placementEcCostCenterMapping;
    private Map<String, String> m_placementCostCenterEsyMapping;
    private Map<String, String> m_placementEcCostCenterEsyMapping;

    private boolean m_updateCostCenter;
    private boolean m_updateProgramContinuum;
    private boolean m_updateMeetingDate;
    private boolean m_updateMeetingAttendance;

    private Set<String> m_iepsWithEsyServices;
    private Map<String, TreeSet<PlainDate>> m_meetingDates;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_dictionary = DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), getBroker()),
                getBroker().getPersistenceKey());

        m_updateCostCenter = Boolean.TRUE.equals(getParameter(UPDATE_COST_CENTER_PARAM));
        m_updateProgramContinuum = Boolean.TRUE.equals(getParameter(UPDATE_PROGRAM_CONTINUUM_PARAM));
        m_updateMeetingDate = Boolean.TRUE.equals(getParameter(UPDATE_MEETING_DATE_PARAM));
        m_updateMeetingAttendance = Boolean.TRUE.equals(getParameter(UPDATE_MEETING_ATTENDANCE_PARAM));

        loadPlacementContinuumMapping();
        loadPlacementEcContinuumMapping();

        loadPlacementCostCenterMapping();
        loadPlacementEcCostCenterMapping();
        loadPlacementCostCenterEsyMapping();
        loadPlacementEcCostCenterEsyMapping();

        loadIepsWithEsyServices();
        loadMeetingDates();

        updateIeps();
        updateMeetingAttendance();
    }

    /**
     * Load ieps with esy services.
     */
    private void loadIepsWithEsyServices() {
        m_iepsWithEsyServices = new HashSet<String>(1000);

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(IepService.COL_SERVICE_MODE,
                Arrays.asList(new String[] {"Special Education", "Related Services"}));
        criteria.addEqualTo(IepService.COL_EXTENDED_YEAR_INDICATOR, Boolean.TRUE);
        criteria.addIn(IepService.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, getIepCriteria()));

        SubQuery query = new SubQuery(IepService.class, IepService.COL_IEP_DATA_OID, criteria);

        m_iepsWithEsyServices.addAll(getBroker().getSubQueryCollectionByQuery(query));
    }

    /**
     * Load meeting dates.
     */
    private void loadMeetingDates() {
        m_meetingDates = new HashMap<String, TreeSet<PlainDate>>();

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(IepMeeting.COL_IEP_DATA_OID,
                new SubQuery(IepData.class, X2BaseBean.COL_OID, getIepCriteria()));

        BeanQuery query = new BeanQuery(IepMeeting.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepMeeting meeting = (IepMeeting) iterator.next();

                TreeSet<PlainDate> datesForIep = m_meetingDates.get(meeting.getIepDataOid());
                if (datesForIep == null) {
                    datesForIep = new TreeSet<PlainDate>();
                    m_meetingDates.put(meeting.getIepDataOid(), datesForIep);
                }

                datesForIep.add(meeting.getDate());
            }
        } finally {
            iterator.close();
        }

    }

    /**
     * Load placement continuum mapping.
     */
    private void loadPlacementContinuumMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("General ed. class w/ special ed. consultation", "1");
        mapping.put("Home or hospitalized instruction", "4");
        mapping.put("Special class in a school district building", "2");
        mapping.put("Special education day school program", "3");
        mapping.put("Special education residential school", "6");

        m_placementContinuumMapping = mapping;
    }

    /**
     * Load placement cost center mapping.
     */
    private void loadPlacementCostCenterMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("General ed. class w/ special ed. consultation", "32202");
        mapping.put("Home or hospitalized instruction", "32207");
        mapping.put("Special class in a school district building", "32203");
        mapping.put("Special education day school program", "32208");
        mapping.put("Special education residential school", "32212");

        m_placementCostCenterMapping = mapping;
    }

    /**
     * Load placement cost center esy mapping.
     */
    private void loadPlacementCostCenterEsyMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("General ed. class w/ special ed. consultation", "32202");
        mapping.put("Home or hospitalized instruction", "32207");
        mapping.put("Special class in a school district building", "32204");
        mapping.put("Special education day school program", "32209");
        mapping.put("Special education residential school", "32212");

        m_placementCostCenterEsyMapping = mapping;
    }

    /**
     * Load placement ec continuum mapping.
     */
    private void loadPlacementEcContinuumMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("Full time EC special ed.", "5");
        mapping.put("General EC w/ on site consultation", "5");
        mapping.put("Home w/ supplementary EC special ed. setting", "");
        mapping.put("Home-based special ed. and related services", "");
        mapping.put("Integrated preschool", "5");
        mapping.put("Residential special ed. school", "6");
        mapping.put("Special ed. day school", "3");
        mapping.put("Temporary placement (30 days or less)", "");

        m_placementEcContinuumMapping = mapping;
    }

    /**
     * Load placement ec cost center mapping.
     */
    private void loadPlacementEcCostCenterMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("Full time EC special ed.", "32214");
        mapping.put("General EC w/ on site consultation", "32213");
        mapping.put("Home w/ supplementary EC special ed. setting", "");
        mapping.put("Home-based special ed. and related services", "");
        mapping.put("Integrated preschool", "32213");
        mapping.put("Residential special ed. school", "32212");
        mapping.put("Special ed. day school", "32208");
        mapping.put("Temporary placement (30 days or less)", "");

        m_placementEcCostCenterMapping = mapping;
    }

    /**
     * Load placement ec cost center esy mapping.
     */
    private void loadPlacementEcCostCenterEsyMapping() {
        HashMap<String, String> mapping = new HashMap<String, String>();

        mapping.put("Full time EC special ed.", "32214");
        mapping.put("General EC w/ on site consultation", "32214");
        mapping.put("Home w/ supplementary EC special ed. setting", "");
        mapping.put("Home-based special ed. and related services", "");
        mapping.put("Integrated preschool", "32214");
        mapping.put("Residential special ed. school", "32212");
        mapping.put("Special ed. day school", "32209");
        mapping.put("Temporary placement (30 days or less)", "");

        m_placementEcCostCenterEsyMapping = mapping;
    }

    /**
     * Gets the continuum mapping.
     *
     * @param ec boolean
     * @return Map
     */
    private Map<String, String> getContinuumMapping(boolean ec) {
        return ec ? m_placementEcContinuumMapping : m_placementContinuumMapping;
    }

    /**
     * Gets the cost center mapping.
     *
     * @param ec boolean
     * @param esy boolean
     * @return Map
     */
    private Map<String, String> getCostCenterMapping(boolean ec, boolean esy) {
        Map<String, String> mapping;

        if (ec) {
            mapping = esy ? m_placementEcCostCenterEsyMapping : m_placementEcCostCenterMapping;
        } else {
            mapping = esy ? m_placementCostCenterEsyMapping : m_placementCostCenterMapping;
        }

        return mapping;
    }

    /**
     * Update ieps.
     */
    private void updateIeps() {
        int continuumUpdates = 0;
        int costCenterUpdates = 0;
        int meetingDateUpdates = 0;

        BeanQuery beanQuery = new BeanQuery(IepData.class, getIepCriteria());

        QueryIterator iterator = getBroker().getIteratorByQuery(beanQuery);
        try {
            while (iterator.hasNext()) {
                IepData iep = (IepData) iterator.next();

                String placement = (String) iep.getFieldValueByAlias(PLACEMENT_ALIAS, m_dictionary);
                String placementEc = (String) iep.getFieldValueByAlias(PLACEMENT_EC_ALIAS, m_dictionary);

                String placementCode = placement;
                boolean ec = false;
                if (!StringUtils.isEmpty(placementEc)) {
                    ec = true;
                    placementCode = placementEc;
                }

                boolean esy = m_iepsWithEsyServices.contains(iep.getOid());

                // ----------- Program Continuum update
                String programContinuum = (String) iep.getFieldValueByAlias(PROGRAM_CONTINUUM_ALIAS, m_dictionary);
                if (m_updateProgramContinuum && StringUtils.isEmpty(programContinuum)) {
                    String continuumStateCode = getContinuumMapping(ec).get(placementCode);
                    if (!StringUtils.isEmpty(continuumStateCode)) {
                        programContinuum = translateStateCodeToUserCode(PROGRAM_CONTINUUM_ALIAS, continuumStateCode);
                        if (!StringUtils.isEmpty(programContinuum)) {
                            iep.setFieldValueByAlias(PROGRAM_CONTINUUM_ALIAS, programContinuum, m_dictionary);
                            continuumUpdates++;
                        }
                    }
                }

                // ----------- Cost Center update
                String costCenter = (String) iep.getFieldValueByAlias(COST_CENTER_ALIAS, m_dictionary);
                if (m_updateCostCenter && StringUtils.isEmpty(costCenter)) {
                    String costCenterStateCode = getCostCenterMapping(ec, esy).get(placementCode);
                    if (!StringUtils.isEmpty(costCenterStateCode)) {
                        costCenter = translateStateCodeToUserCode(COST_CENTER_ALIAS, costCenterStateCode);
                        if (!StringUtils.isEmpty(costCenter)) {
                            iep.setFieldValueByAlias(COST_CENTER_ALIAS, costCenter, m_dictionary);
                            costCenterUpdates++;
                        }
                    }
                }

                // ----------- Meeting date update
                if (m_updateMeetingDate) {
                    TreeSet<PlainDate> datesForIep = m_meetingDates.get(iep.getOid());
                    if (datesForIep != null) {
                        if (iep.getMeetingDate() == null || !datesForIep.contains(iep.getMeetingDate())) {
                            PlainDate dateToSet = datesForIep.last();
                            iep.setMeetingDate(dateToSet);
                            meetingDateUpdates++;
                        }
                    }
                }

                if (iep.isDirty()) {
                    getBroker().saveBeanForced(iep);
                }
            }
        } finally {
            iterator.close();
        }

        logMessage("Updated " + continuumUpdates + " program continuum codes");
        logMessage("Updated " + costCenterUpdates + " cost center codes");
        logMessage("Updated " + meetingDateUpdates + " meeting dates");
    }

    /**
     * Update meeting attendance.
     */
    private void updateMeetingAttendance() {
        int attendanceRecordUpdates = 0;

        if (m_updateMeetingAttendance) {
            X2Criteria attendanceCriteria = new X2Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_PRESENT_INDICATOR, Boolean.TRUE);

            SubQuery attendanceSubQuery = new SubQuery(IepMeetingAttendance.class,
                    IepMeetingAttendance.COL_IEP_MEETING_OID, attendanceCriteria);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(IepMeeting.REL_IEP_DATA + "." + translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS),
                    BooleanAsStringConverter.TRUE);
            criteria.addEqualToField(IepMeeting.REL_IEP_DATA + "." + IepData.COL_MEETING_DATE, IepMeeting.COL_DATE);
            criteria.addNotIn(X2BaseBean.COL_OID, attendanceSubQuery);

            BeanQuery query = new BeanQuery(IepMeeting.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    IepMeeting meeting = (IepMeeting) iterator.next();

                    for (IepMeetingAttendance attendance : meeting.getMeetingAttendance(getBroker())) {
                        if (!attendance.getExcusedIndicator()) {
                            attendance.setPresentIndicator(true);
                            getBroker().saveBeanForced(attendance);
                            attendanceRecordUpdates++;
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }

        logMessage("Marked " + attendanceRecordUpdates + " meeting attendance records present");
    }

    /**
     * Gets the iep criteria.
     *
     * @return Criteria
     */
    private Criteria getIepCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS), "1");

        return criteria;
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        return field.getJavaName();
    }

    /**
     * Translate state code to user code.
     *
     * @param alias String
     * @param stateCode String
     * @return String
     */
    private String translateStateCodeToUserCode(String alias, String stateCode) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        return m_dictionary.findUserStateReferenceCode(field.getReferenceTableOid(), stateCode);
    }
}
