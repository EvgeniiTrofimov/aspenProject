/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2020 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceLog;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.TimeUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MedicaidBillingData.
 */
public class MedicaidBillingData extends StateReportData {
    public static final String ALIAS_ALL_ISL_SVSLOGCREATOR = "all-isl-svsLogCreator";

    public static final String CALC_TIME = "CALC_TIME";
    public static final String PROVIDER_NAME = "PROVIDER_NAME";
    public static final String PROVIDER_NUMBER = "PROVIDER_NUMBER";
    public static final String SERVICE_MODE = "SERVICE_MODE";

    public static final String SERVICES_SECTION_C_MODE = "SpecialEd - Other";

    /**
     * An optional parameter specifying a specific case manager to filter the report on
     */
    public static final String CASE_MANAGER_PARAM = "caseManager";

    /**
     * Name for the "end date" input parameter. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * A parameter specifying if to exclude consultation services from the report
     */
    public static final String EXCLUDE_CONSULT_PARAM = "excludeConsult";

    /**
     * A parameter specifying exclude or not student absent record from the report
     */
    public static final String STUDENT_ABSENT_PARAM = "excludeStudentAbsent";

    /**
     * An optional parameter specifying a specific placement program to filter the report on
     */
    public static final String PLACEMENT_PARAM = "placement";

    /**
     * A parameter passed to iReport specifying whether or not an individual provider was selected
     */
    public static final String PROVIDER_SPECIFIED = "providerSpecified";

    /**
     * School OID parameter specifying which school the services should be listed for
     */
    public static final String SCHOOL_OID_PARAM = "schoolOid";

    /**
     * Start service provider parameter specifying the name of the service provider
     */
    public static final String SERVICE_PROVIDER_PARAM = "serviceProvider";

    /**
     * Name for the "start date" input parameter. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Name for the "Return Service Log Creator" input parameter.
     */
    protected static final String PARAM_SERVICE_LOG_CREATOR = "serviceLogCreator";


    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;
    private boolean m_serviceLogCreator = false;

    // Database Field constant
    private static String CONSULTATION_FIELD = "Consultation";

    /**
     * The Class MedicaidRosterEntity.
     */
    public static class MedicaidBillingEntity extends StateReportEntity {
        MedicaidBillingData m_data;

        /**
         * Instantiates a new MA entity.
         */
        public MedicaidBillingEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            IepServiceLog serviceLog = (IepServiceLog) getBean();
            String name = serviceLog.getIepService().getStudent().getNameView() +
                    " [LASID: " + serviceLog.getIepService().getStudent().getLocalId() +
                    ", SASID: " + serviceLog.getIepService().getStudent().getStateId() +
                    "] ";
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (MedicaidBillingData) data;
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Class to retrieve value of the field.
     */
    protected class RetrieveElement implements FieldRetriever {

        protected static final String RETRIEVER_ID = "MA-EL-RETR";

        /**
         * Instantiates a new MA retriever.
         */
        public RetrieveElement() {
            super();
        }

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            IepServiceLog iepServiceLog = (IepServiceLog) entity.getBean();
            Object returnValue = "";

            switch ((String) field.getParameter()) {
                case CALC_TIME:
                    if (iepServiceLog.getStartTime() != null) {
                        returnValue = TimeUtils.add(iepServiceLog.getStartTime(), Calendar.MINUTE,
                                iepServiceLog.getDuration());
                    }
                    break;
                case PROVIDER_NAME:
                    if (m_serviceLogCreator) {
                        returnValue = iepServiceLog.getFieldValueByAlias(ALIAS_ALL_ISL_SVSLOGCREATOR);
                    } else {
                        if (iepServiceLog.getIepService().getStaff() != null) {
                            returnValue = iepServiceLog.getIepService().getStaff().getNameView();
                        }
                    }
                    break;
                case PROVIDER_NUMBER:
                    if (m_serviceLogCreator) {
                        returnValue = iepServiceLog.getIepService().getStudent().getMedicaidId();
                    } else {
                        if (iepServiceLog.getIepService().getStaff() != null) {
                            returnValue = iepServiceLog.getIepService().getStaff().getMedicaidId();
                        }
                    }
                    break;
                case SERVICE_MODE:
                    if (iepServiceLog.getIepService() != null) {
                        if (SERVICES_SECTION_C_MODE.equals(iepServiceLog.getIepService().getServiceMode())) {
                            returnValue = "Y";
                        } else {
                            returnValue = "N";
                        }
                    }
                    break;
            }
            return returnValue;
        }
    }

    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    private QueryByCriteria m_criteria;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_serviceLogCreator = ((Boolean) getParameter(PARAM_SERVICE_LOG_CREATOR)).booleanValue();

        if ((m_startDate == null) && (m_endDate == null)) {
            m_startDate = DateUtils.getFirstOfMonth(DateUtils.add(new PlainDate(), Calendar.MONTH, -1));
            m_endDate = DateUtils.getLastOfMonth(DateUtils.add(new PlainDate(), Calendar.MONTH, -1));
        }

        Criteria criteria = new Criteria();

        if (m_startDate != null) {
            criteria.addGreaterOrEqualThan(IepServiceLog.COL_DATE, m_startDate);
        }

        if (m_endDate != null) {
            criteria.addLessOrEqualThan(IepServiceLog.COL_DATE, m_endDate);
        }

        String schoolOid = (String) getParameter(SCHOOL_OID_PARAM);

        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_STUDENT + PATH_DELIMITER
                    + SisStudent.COL_SCHOOL_OID, schoolOid);
        }

        if (!StringUtils.isEmpty((String) getParameter(CASE_MANAGER_PARAM))) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_IEP_DATA
                    + PATH_DELIMITER + IepData.COL_STAFF_OID, getParameter(CASE_MANAGER_PARAM));
        }

        if (!StringUtils.isEmpty((String) getParameter(PLACEMENT_PARAM))) {
            criteria.addEqualTo(
                    IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_IEP_DATA + PATH_DELIMITER
                            + IepData.REL_PLACEMENTS + PATH_DELIMITER + IepPlacement.COL_IEP_PLACEMENT_PROGRAM_OID,
                    getParameter(PLACEMENT_PARAM));
        }

        if (!StringUtils.isEmpty((String) getParameter(SERVICE_PROVIDER_PARAM))) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.COL_STAFF_OID,
                    getParameter(SERVICE_PROVIDER_PARAM));
        }

        if (Boolean.valueOf((String) getParameter(EXCLUDE_CONSULT_PARAM)).booleanValue()) {
            criteria.addNotEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.COL_SERVICE_MODE,
                    CONSULTATION_FIELD);
        }

        if (((Boolean) getParameter(STUDENT_ABSENT_PARAM)).booleanValue()) {
            criteria.addNotEqualTo(IepServiceLog.COL_ABSENT_INDICATOR, BooleanAsStringConverter.TRUE);
        }

        setEntityClass(MedicaidBillingEntity.class);

        m_criteria = new QueryByCriteria(IepServiceLog.class, criteria);

        // Set the query to be used for student selection.
        setQuery(m_criteria);

        // Build a map of calculations/retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveElement.RETRIEVER_ID, new RetrieveElement());
        super.addCalcs(calcs);
    }
}
