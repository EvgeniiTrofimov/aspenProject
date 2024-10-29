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

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * FL Student Demographic Information report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2016-17-student-info-system
 * /student-demographic-info.stml
 *
 * @author Follett Software Company
 */
public class FLStudentTransportationData extends FLStateReportData {

    /**
     * The Class FLStdTransportationEntity.
     */
    public static class FLStdTransportationEntity extends FLStateReportEntity {
        private FLStudentTransportationData m_data;
        private SisStudent m_record;

        /**
         * Instantiates a new FL std transportation entity.
         */
        public FLStdTransportationEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return Sis student
         */
        public SisStudent getCurrentRecord() {
            return m_record;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLStudentTransportationData) data;
            m_record = (SisStudent) getBean();
            setRowCount(m_data.getStudentHelper().isStudentEligible(m_record) ? 1 : 0);
        }
    }

    /**
     * Retrieve
     * Hazardous Walking Code. Data Element Number: 127275
     *
     * @author Follett Software Company
     */
    protected class RetrieveHazardWalk implements FieldRetriever {

        public static final String CALC_ID = "HAZARD_WALK";

        public static final String CODE_DEFAULT = "000000";
        public static final String CODE_HAZARD = "111111";
        public static final String EXPORT_FIELD_MEMB_CATEGORY = "Tran Memb Category";
        public static final String HAZARD_MEMB_CATEGORY = "G";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = CODE_DEFAULT;
            String mebCategoryValue = entity.getFieldValue(EXPORT_FIELD_MEMB_CATEGORY);
            if (!StringUtils.isEmpty(mebCategoryValue) && HAZARD_MEMB_CATEGORY.equals(mebCategoryValue)) {
                value = CODE_HAZARD;
            }
            return value;
        }
    }

    /**
     * Retrieve
     * 1. Bus Number. Data Element Number: 105225
     * 2. Bus Route. Data Element Number: 105625
     * 3. Vehicle Category. Data Element Number: 184125
     * 4. Membership Category Data Element Number: 181113
     *
     * @author Follett Software Company
     */
    protected class RetrieveTransportationInfo implements FieldRetriever {

        public static final String CALC_ID = "TRAN_INFO";

        public static final String PARAM_BUS_NUM = "BUS-NUM";
        public static final String PARAM_BUS_ROUTE = "BUS-ROUTE-NUM";
        public static final String PARAM_MEMB_CATEGORY = "MEMB-CATEGORY";
        public static final String PARAM_VEHICLE_CATEGORY = "VEHICLE-CATEGORY";

        private static final String ALIAS_STR_BUS_NUM = "all-str-BusNumber";
        private static final String ALIAS_STR_BUS_ROUTE = "all-str-BusRoute";
        private static final String ALIAS_STR_MEMB_CATEGORY = "all-str-MembershipCategory";
        private static final String ALIAS_STR_VEHICLE_CATEGORY = "all-str-VehicleCategory";

        DataDictionaryField m_fieldBusNumber;
        DataDictionaryField m_fieldBusRoute;
        DataDictionaryField m_fieldMembershipCategory;
        DataDictionaryField m_fieldVehicleCategory;

        /**
         * Instantiates a new retrieve transportation info.
         */
        public RetrieveTransportationInfo() {
            m_fieldMembershipCategory = translateAliasToDictionaryField(ALIAS_STR_MEMB_CATEGORY, true);
            m_fieldVehicleCategory = translateAliasToDictionaryField(ALIAS_STR_VEHICLE_CATEGORY, true);
            m_fieldBusNumber = translateAliasToDictionaryField(ALIAS_STR_BUS_NUM, true);
            m_fieldBusRoute = translateAliasToDictionaryField(ALIAS_STR_BUS_ROUTE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentTransportation transport = null;
            SisStudent student = ((FLStdTransportationEntity) entity).getCurrentRecord();
            String param = (String) field.getParameter();
            if (m_strMap.containsKey(student.getOid()) && !m_strMap.get(student.getOid()).isEmpty()) {
                transport = m_strMap.get(student.getOid()).get(0);
            }
            if (transport != null) {
                if (PARAM_BUS_NUM.equals(param)) {
                    value = FLStudentTransportationData.this.getFieldValue(transport, m_fieldBusNumber);
                } else if (PARAM_BUS_ROUTE.equals(param)) {
                    value = FLStudentTransportationData.this.getFieldValue(transport, m_fieldBusRoute);
                } else if (PARAM_MEMB_CATEGORY.equals(param)) {
                    value = FLStudentTransportationData.this.getFieldValue(transport, m_fieldMembershipCategory);
                } else if (PARAM_VEHICLE_CATEGORY.equals(param)) {
                    value = FLStudentTransportationData.this.getFieldValue(transport, m_fieldVehicleCategory);
                }
            }
            return value;
        }
    }

    protected Map<String, List<StudentTransportation>> m_strMap;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        QueryByCriteria transporationQuery = new QueryByCriteria(StudentTransportation.class, getSTRCriteria());
        m_strMap =
                getBroker().getGroupedCollectionByQuery(transporationQuery, StudentTransportation.COL_STUDENT_OID, 100);
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID, m_strMap.keySet());
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLStdTransportationEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Function for building custom Student Transportation criteria.
     *
     * @return criteria for query for list of active student transportations
     *         limited by report date range
     * @throws X2BaseException exception
     */
    private X2Criteria getSTRCriteria() throws X2BaseException {
        X2Criteria strCriteria = null;

        if (getStudentHelper() != null) {
            strCriteria = new X2Criteria();
            strCriteria.addNotNull(StudentTransportation.COL_START_DATE);
            strCriteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, getSurveyPeriod().getEndDate());
            SubQuery stdSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentHelper().getStudentCriteria());
            strCriteria.addIn(StudentTransportation.COL_STUDENT_OID, stdSubQuery);
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addIsNull(StudentTransportation.COL_END_DATE);
            X2Criteria orEndDateCriteria = new X2Criteria();
            orEndDateCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, getSurveyPeriod()
                    .getStartDate());
            endDateCriteria.addOrCriteria(orEndDateCriteria);
            strCriteria.addAndCriteria(endDateCriteria);
        }

        return strCriteria;
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveTransportationInfo.CALC_ID, new RetrieveTransportationInfo());
        calcs.put(RetrieveHazardWalk.CALC_ID, new RetrieveHazardWalk());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
