/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolAddress;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolRace;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendarDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export MD Direct Certification System export.
 *
 * @author X2 Development Corporation
 */
public class MDDirectCertSysData extends StateReportData {

    /**
     * The Class MDDirectCertSysEntity.
     *
     * @author X2 Development Corporation
     */
    public static class MDDirectCertSysEntity extends ToolsSharedContainer.StateReportEntity {

        ToolEnrollment m_enrStd;
        PlainDate m_enrStartDate;

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return entity name
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            MDStudent student = (MDStudent) getBean();
            String name = STRING_EMPTY;
            name +=
                    student.getNameView() + " [Local ID: " + student.getLocalId() + ", State ID: "
                            + student.getStateId() + "] ";
            return name;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            MDStudent std = (MDStudent) bean;
            MDDirectCertSysData mdData = (MDDirectCertSysData) data;
            ToolSchoolCalendar cal = null;
            if (!StringUtils.isEmpty(std.getCalendarCode())) {
                cal = std.getSchool(data.getBroker()).getCalendarByCode(data.getBroker(),
                        data.getCurrentContext().getOid(),
                        std.getCalendarCode());
            }
            if (cal == null) {
                cal = std.getSchool(data.getBroker()).getMostCommonCalendar(data.getBroker(),
                        data.getCurrentContext().getOid());
            }
            if (cal != null) {
                PlainDate firstInSessionDate = cal.getDatesInSession(data.getBroker()).first();
                PlainDate lastInSessionDate = cal.getDatesInSession(data.getBroker()).last();
                m_enrStd =
                        mdData.getEnrollmentForDate(std.getEnrollments(data.getBroker()), lastInSessionDate, "ESY",
                                data.getBroker());
                if (m_enrStd != null) {
                    MDSchool skl = (MDSchool) m_enrStd.getSchool(data.getBroker());
                    if (StringUtils.isEmpty(skl.getStateID())) {
                        setRowCount(0);
                    } else {
                        setRowCount(1);
                        PlainDate enrDate = m_enrStd.getEnrollmentDate();
                        if (enrDate != null && DateUtils.isBetween(enrDate, firstInSessionDate,
                                lastInSessionDate)) {
                            m_enrStartDate = enrDate;
                        } else {
                            m_enrStartDate = firstInSessionDate;
                        }
                    }
                }
            }
        }
    }

    /**
     * The Class MDSchool.
     */
    public static class MDSchool extends ToolSchool {

        private static final String ALIAS_SKL_STATE_ID = "StateId";

        /**
         * Fields
         */
        public static final ToolBeanColumn FIELD_SKL_STATE_ID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_STATE_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_SKL_STATE_ID);

        /**
         * Instantiates a new MD school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public MDSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the state ID.
         *
         * @return String
         */
        public String getStateID() {
            return getValueString(FIELD_SKL_STATE_ID);
        }
    }
    /**
     * The Class Student.
     */
    public static class MDStudent extends ToolStudent {

        // Fields
        public static final ToolBeanColumn FIELD_GENDER =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().genderCode());
        public static final ToolBeanColumn FIELD_HISP_IND =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().hispanicLatinoIndicator());
        public static final ToolBeanColumn FIELD_PSN_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().personId());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_GENDER,
                        FIELD_HISP_IND,
                        FIELD_PSN_ID);

        /**
         * Instantiates a new Student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public MDStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Checks if is hispanic latino.
         *
         * @return true, if is hispanic latino
         */
        public boolean isHispanicLatino() {
            return getValueLogical(FIELD_HISP_IND);
        }
    }

    /**
     * The Class RetrieveAddress.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAddress implements FieldRetriever {
        private static final String CALC_ID = "MD-STD-ADR";
        private static final String CALC_PARAM_ADR_01 = "ADR_01";
        private static final String CALC_PARAM_ADR_02 = "ADR_02";
        private static final String CALC_PARAM_CITY = "CITY";
        private static final String CALC_PARAM_STATE = "STATE";
        private static final String CALC_PARAM_ZIP = "ZIP";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            MDStudent std = (MDStudent) entity.getBean();
            String returnObject = null;
            ToolAddress adr = std.getPhysicalAddress(getBroker());
            if (adr != null) {
                if (CALC_PARAM_ADR_01.equals(param)) {
                    returnObject = adr.getLine1();
                } else if (CALC_PARAM_ADR_02.equals(param)) {
                    returnObject = adr.getLine2();
                } else if (CALC_PARAM_CITY.equals(param)) {
                    returnObject = adr.getCity();
                } else if (CALC_PARAM_STATE.equals(param)) {
                    returnObject = adr.getState();
                } else if (CALC_PARAM_ZIP.equals(param)) {
                    returnObject = adr.getPostalCode();
                }
            }
            return returnObject;
        }
    }

    /**
     * The Class RetrieveEnrollment.
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private static final String CALC_ID = "MD-STD-ENR";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String returnObject = "";
            PlainDate dateToFormat = ((MDDirectCertSysEntity) entity).m_enrStartDate;
            if (dateToFormat != null) {
                returnObject = m_dateFormat.format(dateToFormat);
            }
            return returnObject;
        }
    }

    /**
     * Retrieve Race code:
     *
     * When psnHispLatInd = True, return "H".
     * When psnHispLatInd = False and student has multiple race code, return "O"
     * When When psnHispLatInd = False and student race code maps to state code 1, return "N".
     * When psnHispLatInd = False and student race code maps to state code 2, return "A".
     * When psnHispLatInd = False and student race code maps to state code 3, return "B".
     * When psnHispLatInd = False and student race code maps to state code 4, return P.
     * When psnHispLatInd = False and student race code maps to state code 5, return "C".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        private static final String CALC_ID = "MD-DCS-RACE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String raceCode = "";
            MDStudent std = (MDStudent) entity.getBean();
            Collection<ToolRace> races = std.getPersonRaces(data.getBroker());
            // Find the reference code that we are looking for.
            if (std.isHispanicLatino()) {
                raceCode = "H";
            } else if (races != null && !races.isEmpty()) {
                if (races.size() > 1) {
                    raceCode = "O";
                } else {
                    ToolRace race = races.stream().findFirst().orElse(null);
                    if (race != null) {
                        String code = race.getRaceCode();
                        if (!StringUtils.isEmpty(code) && m_raceCodes.containsKey(code)) {
                            String stateRaceCode = m_raceCodes.get(code).getStateCode();
                            if (!StringUtils.isEmpty(stateRaceCode)) {
                                switch (stateRaceCode) {
                                    case "1":
                                        raceCode = "N";
                                        break;
                                    case "2":
                                        raceCode = "A";
                                        break;
                                    case "3":
                                        raceCode = "B";
                                        break;
                                    case "4":
                                        raceCode = "P";
                                        break;
                                    case "5":
                                        raceCode = "C";
                                        break;
                                    default:
                                        break;
                                }
                            }
                        }
                    }
                }
            }
            return raceCode;
        }
    }

    /**
     * The Class RetrieveSchool.
     */
    protected class RetrieveSchool implements FieldRetriever {
        private static final String CALC_ID = "MD-SKL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String returnObject = "";
            MDDirectCertSysEntity mdEntity = (MDDirectCertSysEntity) entity;
            if (mdEntity.m_enrStd != null) {
                MDSchool skl = (MDSchool) mdEntity.m_enrStd.getSchool(data.getBroker());
                if (skl != null) {
                    returnObject = skl.getStateID();
                }
            }
            return returnObject;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "MD-STD-CLEAN";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) entity.getBean().getFieldValueByColumnName(field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(STRING_EMPTY);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = STRING_EMPTY;
            }
            return cleanValue;
        }
    }

    /*
     * General Constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String STRING_EMPTY = "";

    /*
     * Member variables
     */
    protected String m_activeCode;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyyMMdd");
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // Initialize fields+
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            ToolBean.setDictionaryExtractor(getDictionaryExtractor());
            ToolBean.registerClass(ToolAddress.class);
            ToolBean.registerClass(MDStudent.class);
            ToolBean.registerClass(MDSchool.class);
            ToolBean.registerClass(ToolRace.class);
            ToolBean.registerClass(ToolSchoolCalendar.class);
            ToolBean.registerClass(ToolSchoolCalendarDate.class);
            ToolBean.registerClass(ToolStudentSchedule.class);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setCurrentContext(m_currentContext)
                    .setEndDate(m_currentContext.getEndDate())
                    .setIncludeSecondarySpans(false);
            if (isSchoolContext()) {
                spanCriteria.setSchoolOids(Arrays.asList(getSchool().getOid()));
            }
            X2Criteria inputCriteria = new X2Criteria();
            applyInputCriteria(inputCriteria, false, null);
            if (!inputCriteria.isEmpty()) {
                spanCriteria.setStudentLimitingCriteria(inputCriteria);
            }

            // Preloads

            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
            candidateCriteria.addEqualTo(SisBeanPaths.STUDENT.enrollmentStatus().getPath(), m_activeCode);
            // Check user selection criteria.
            setEntityClass(MDDirectCertSysEntity.class);
            setFilterable(FilterableFactory.create(getBroker(), MDStudent.class, candidateCriteria, null));

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

            // Get race code reference codes for use in the race retriever.
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);
            ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_PERSON_RACES);

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveAddress.CALC_ID, new RetrieveAddress());
            calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
            calcs.put(RetrieveEnrollment.CALC_ID, new RetrieveEnrollment());
            calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the enrollment for date.
     *
     * @param enrollments List<ToolEnrollment>
     * @param date the date
     * @param types the types
     * @param broker the broker
     * @return the enrollment for date
     */
    private ToolEnrollment getEnrollmentForDate(List<ToolEnrollment> enrollments,
                                                PlainDate date,
                                                String types,
                                                X2Broker broker) {
        ToolEnrollment lastEnrollment = null;
        if (enrollments != null) {
            for (ToolEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentDate() != null && !enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                        break;
                    }
                }
            }
        }
        return lastEnrollment;
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        if (m_currentContext == null) {
            Organization organization = OrganizationManager.getRootOrganization(getBroker());
            m_currentContext = organization.getCurrentContext();
        }
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
    }
}
