/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductAction;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductIncident;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductOffense;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchoolIncident;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OnsisSchoolIncident extends OnsisStateReportData {
    public static final String FIELD_MAP_KEY_DELIMITER = "-";
    public static final String FIELD_YOUR_REFERENCE_NUMBER = "YourReferenceNumber";

    public static class OnsisSchoolIncidentEntity extends OnsisStateReportEntity {
        private List<List<ToolConductIncident>> m_studentIncidents;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public OnsisSchoolIncidentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            OnSchoolIncident incident = (OnSchoolIncident) bean;

            Map<String, List<ToolConductIncident>> studentIncidentsMap = incident.getConductIncidents(getBroker())
                    .stream()
                    .filter(this::isReportableStdInc)
                    .collect(Collectors.groupingBy(
                            (cnd) -> getCndMapKey(incident, cnd),
                            LinkedHashMap::new,
                            Collectors.toList()));
            m_studentIncidents = studentIncidentsMap.values().stream().collect(Collectors.toList());

            setRowCount(m_studentIncidents.size());
        }

        /**
         * Gets the cnd map key.
         *
         * @param incident
         * @param cnd
         * @return String
         */
        public String getCndMapKey(OnSchoolIncident incident, ToolConductIncident cnd) {
            return Stream.of(
                    getIncidentDate(incident, cnd).toString(),
                    getIncidentTime(incident, cnd).toString(),
                    getIncidentLocation(incident, cnd))
                    .collect(Collectors.joining(FIELD_MAP_KEY_DELIMITER));
        }

        /**
         * Gets the list of conduct incidents
         *
         * @return
         */
        public List<ToolConductIncident> getConductIncidents() {
            return m_studentIncidents.get(getCurrentRow());
        }

        /**
         * Gets the incident date.
         *
         * @return String
         */
        public PlainDate getIncidentDate() {
            return getIncidentDate((OnSchoolIncident) getBean(), getConductIncidents().get(0));
        }

        /**
         * Gets the incident id.
         *
         * @return String
         */
        public String getIncidentId() {
            /*
             * Get from CSV only.
             * Don't create if not in CSV
             * because Onsis wants to generate this key on an Add.
             */
            OnsisCsvDataRecord csvRecord = getCsvRecord();
            return (csvRecord != null)
                    ? csvRecord.getSingleFieldValue(OnsisExtractHelper.CsvField.INCIDENT_ID)
                    : null;
        }

        /**
         * Gets the incident time.
         *
         * @return String
         */
        public PlainTime getIncidentTime() {
            return getIncidentTime((OnSchoolIncident) getBean(), getConductIncidents().get(0));
        }

        /**
         * Gets the incident location.
         *
         * @return String
         */
        public String getIncidentLocation() {
            return getIncidentLocation((OnSchoolIncident) getBean(), getConductIncidents().get(0));
        }

        /**
         * Fallback model:
         * Get Date/Time from CND if CND Date exists.
         * Else get Date from UDE.
         *
         * @return
         */
        private PlainDate getIncidentDate(OnSchoolIncident incident, ToolConductIncident cnd) {
            PlainDate cndIncidentDate = cnd.getIncidentDate();
            if (cndIncidentDate != null) {
                return cndIncidentDate;
            }
            OnSchoolIncident udeIncident = (OnSchoolIncident) getBean();
            return udeIncident.getIncidentDate();
        }

        /**
         * Fallback model:
         * If CND Date equals UDE date,
         * use CND Time if it exists, else UDE time.
         *
         * Else (CND Date != UDE Date),
         * use CND Time even if null.
         *
         * @return
         */
        private PlainTime getIncidentTime(OnSchoolIncident incident, ToolConductIncident cnd) {
            /*
             * Gather stuff
             */
            PlainDate cndIncidentDate = cnd.getIncidentDate();
            PlainTime cndIncidentTime = cnd.getIncidentTime();

            OnSchoolIncident udeIncident = (OnSchoolIncident) getBean();
            PlainDate udeIncidentDate = udeIncident.getIncidentDate();
            /*
             * If CND Date equals UDE date,
             * use CND Time if it exists, else UDE time.
             */
            if ((cndIncidentDate != null && cndIncidentDate.equals(udeIncidentDate))
                    || (cndIncidentDate == null && udeIncidentDate == null)) {
                if (cndIncidentTime != null) {
                    return cndIncidentTime;
                }
                return udeIncident.getIncidentTime();
            }

            /*
             * Else (CND Date != UDE Date),
             * use CND Time even if null.
             */
            return cndIncidentTime;
        }

        /**
         * Get location from CND if it exists,
         * else get location from UDE
         *
         * @return
         */
        private String getIncidentLocation(OnSchoolIncident incident, ToolConductIncident cnd) {
            String locationStateCode = cnd.getIncidentLocationState();
            if (locationStateCode == null) {
                OnSchoolIncident udeIncident = (OnSchoolIncident) getBean();
                locationStateCode = udeIncident.getIncidentLocationState();
            }
            return locationStateCode;
        }

        /**
         *
         * @return
         */
        private OnsisCsvDataRecord getCsvRecord() {
            OnsisExtractRecords matcher = getGlobalData().getExtractHelper()
                    .getMatcherByExtractType(OnsisStateReportData.EXTRACT_TYPE_SCHOOL_INCIDENT);

            String myReferenceNumber = deepGetFieldValueByFieldName(FIELD_YOUR_REFERENCE_NUMBER);
            if (StringUtils.isBlank(myReferenceNumber)) {
                return null;
            }

            OnsisCsvDataRecord csvRecord = matcher == null ? null
                    : matcher.findRecord(
                            Arrays.asList(
                                    OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                                    OnsisExtractHelper.CsvField.YOUR_REFERENCE_NUMBER.toString()),
                            Arrays.asList(
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString()),
                                    myReferenceNumber));
            return csvRecord;
        }

        /**
         * Checks if conduct incident is reportable.
         *
         * @param cnd ConductIncident
         * @return true, if is reportable
         */
        private boolean isReportableStdInc(ToolConductIncident cnd) {
            OnStudent student = (OnStudent) cnd.getStudent(getBroker());
            if (student != null) {
                Boolean exclude = student.getExcludeFromReporting();
                if (exclude != null && exclude.booleanValue()) {
                    return false;
                }
            }
            Collection<ToolConductAction> conductActions = cnd.getConductActions(getBroker());
            List<ToolConductAction> reportableActions = conductActions.stream()
                    .filter(act -> !StringUtils.isBlank(act.getActionCodeState()))
                    .collect(Collectors.toList());
            if (reportableActions.isEmpty()) {
                return false;
            }

            String primaryInfraction = cnd.getIncidentCode();
            if (!StringUtils.isBlank(primaryInfraction)) {
                return true;
            }

            Collection<ToolConductOffense> conductOffenses = cnd.getConductOffenses(getBroker());
            List<ToolConductOffense> reportableOffenses = conductOffenses.stream()
                    .filter(cno -> !StringUtils.isBlank(cno.getIncidentCode()))
                    .collect(Collectors.toList());

            return reportableOffenses.size() > 0;
        }

    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        List<OnSchoolIncident> incidents = new ArrayList<>(ToolBean.getCachedToolBeans(OnSchoolIncident.class)).stream()
                .filter(cnd -> cnd.getIncidentDate() != null
                        && getGlobalData().getDateRange().contains(cnd.getIncidentDate()))
                .collect(Collectors.toList());
        setBeans(incidents);
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();
        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());
        return calcs;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSchoolIncidentEntity.class);
    }
}
