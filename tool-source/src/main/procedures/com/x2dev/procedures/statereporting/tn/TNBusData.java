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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for TN 015 Bus Export.
 * Entity represents whole organization, and each entity row it is a bus.
 * Presently information about bus stored in reference code appropriate for bus.
 */
public class TNBusData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class TNBusEntity.
     */
    public static class TNBusEntity extends TNStateReportEntity {
        private static final String DATE_FORMAT_SERVICE = "yyyy-MM-dd";

        private SimpleDateFormat m_format = new SimpleDateFormat(DATE_FORMAT_SERVICE);
        private ArrayList<UserDefinedTableD> m_buses = new ArrayList<>();
        private TNBusData m_bData;
        Map<String, Collection<ReferenceCode>> m_busCodes;

        /**
         * Gets the current bus bean.
         *
         * @return UserDefinedTableD bean
         */
        public UserDefinedTableD getCurrentBus() {
            return m_buses.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            UserDefinedTableD bus = getCurrentBus();
            return getBusNumber(bus);
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
            m_bData = (TNBusData) data;

            ExtendedDataDictionary extDictionary =
                    (ExtendedDataDictionary) data.getDataDictionary().getExtendedDictionary();
            String refTableOid = null;
            DataDictionaryField dictionaryField =
                    data.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_AM_BUS);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                refTableOid = dictionaryField.getReferenceTableOid();
            }

            X2Criteria uddCriteria = new X2Criteria();
            uddCriteria.addEqualTo(UserDefinedTableD.COL_EXTENDED_DATA_DICTIONARY_OID, extDictionary.getOid());
            QueryByCriteria uddQuery = new QueryByCriteria(UserDefinedTableD.class, uddCriteria);
            for (Object item : data.getBroker().getCollectionByQuery(uddQuery)) {
                UserDefinedTableD bus = (UserDefinedTableD) item;
                String busNumber = getBusNumber(bus);
                String description = (String) bus.getFieldValueByAlias(ALIAS_BUS_DESCRIPTION, data.getDataDictionary());
                boolean disabled = true;
                if (!StringUtils.isEmpty(busNumber) && datesOverlapWithYear(bus) &&
                        ((m_bData.m_allBuses == null || m_bData.m_busNumbersCodesOids == null) ||
                                (m_bData.m_allBuses.booleanValue()
                                        || m_bData.m_busNumbersCodesOids.contains(bus.getOid())))) {
                    m_buses.add(bus);
                    disabled = false;
                }
                if (!StringUtils.isEmpty(refTableOid)) {
                    updateRefTable(data, refTableOid, busNumber, description, disabled);
                }
            }
            setRowCount(m_buses.size());
            TNBusData tnData = (TNBusData) data;
            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Update ref table.
         *
         * @param data StateReportData
         * @param refTableOid String
         * @param busNumber String
         * @param description String
         * @param disabled boolean
         */
        private void updateRefTable(StateReportData data,
                                    String refTableOid,
                                    String busNumber,
                                    String description,
                                    boolean disabled) {
            Collection<ReferenceCode> codes = getReferenceCodes(data, refTableOid, busNumber);
            if (codes == null || codes.isEmpty()) {
                ReferenceCode refCode =
                        X2BaseBean.newInstance(ReferenceCode.class, data.getBroker().getPersistenceKey());
                refCode.setReferenceTableOid(refTableOid);
                refCode.setCode(busNumber);
                refCode.setStateCode(busNumber);
                refCode.setDescription(description);
                refCode.setDisabledIndicator(disabled);
                refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
                refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
                data.getBroker().saveBeanForced(refCode);
            } else if (codes.size() == 1) {
                ReferenceCode refCode = codes.iterator().next();
                refCode.setCode(busNumber);
                refCode.setStateCode(busNumber);
                refCode.setDescription(description);
                refCode.setDisabledIndicator(disabled);
                if (refCode.isDirty()) {
                    data.getBroker().saveBeanForced(refCode);
                }
            }
        }

        /**
         * Gets the reference codes.
         *
         * @param data StateReportData
         * @param refTableOid String
         * @param busNumber String
         * @return Collection
         */
        private Collection<ReferenceCode> getReferenceCodes(StateReportData data,
                                                            String refTableOid,
                                                            String busNumber) {
            if (m_busCodes == null) {
                m_busCodes = Collections.EMPTY_MAP;
                if (!StringUtils.isEmpty(refTableOid)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
                    BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
                    m_busCodes = data.getBroker().getGroupedCollectionByQuery(query, ReferenceCode.COL_STATE_CODE, 256);
                }
            }
            Collection<ReferenceCode> codes = m_busCodes.get(busNumber);
            return codes;
        }

        /**
         * Returns true if dates of bus service overlap with dates of school year, otherwise false.
         *
         * @param bus UserDefinedTableD
         * @return boolean
         */
        private boolean datesOverlapWithYear(UserDefinedTableD bus) {
            boolean datesOverlapWithYear = false;

            String startDateDB = (String) bus.getFieldValueByBeanPath(m_bData.m_fieldBusServiceBeginDate);
            String endDateDB = (String) bus.getFieldValueByBeanPath(m_bData.m_fieldBusServiceEndDate);
            Date serviceStartDate = null;
            Date serviceEndDate = null;
            try {
                if (!StringUtils.isEmpty(startDateDB)) {
                    serviceStartDate = m_format.parse(startDateDB);
                }
                if (!StringUtils.isEmpty(endDateDB)) {
                    serviceEndDate = m_format.parse(endDateDB);
                }
            } catch (ParseException e) {
                e.printStackTrace();
            }

            PlainDate contextStartDate = m_bData.getCurrentContext().getStartDate();
            PlainDate contextEndDate = m_bData.getCurrentContext().getEndDate();

            if (rangesCorrectAndOverlap(serviceStartDate, serviceEndDate, contextStartDate, contextEndDate)) {
                datesOverlapWithYear = true;
            }

            return datesOverlapWithYear;
        }

        /**
         * Gets the bus number.
         *
         * @param bus UserDefinedTableD
         * @return String
         */
        private String getBusNumber(UserDefinedTableD bus) {
            return (String) bus.getFieldValueByBeanPath(m_bData.m_fieldBusNum);
        }

        /**
         * Return true if left date pair overlaps with right date pair.
         * Null is considering as unlimited past/future.
         *
         * @param leftStartDate Date
         * @param leftEndDate Date
         * @param rightStartDate Date
         * @param rightEndDate Date
         * @return boolean
         */
        private boolean rangesCorrectAndOverlap(Date leftStartDate,
                                                Date leftEndDate,
                                                Date rightStartDate,
                                                Date rightEndDate) {
            boolean rangesOverlap = false;

            boolean correctDates =
                    ((leftStartDate == null || leftEndDate == null) || !leftStartDate.after(leftEndDate)) &&
                            ((rightStartDate == null || rightEndDate == null) || !rightStartDate.after(rightEndDate));

            // Both ranges have unlimited past
            if (correctDates &&
                    ((leftStartDate == null && rightStartDate == null) ||
                    // both ranges have unlimited future
                            (leftEndDate == null && rightEndDate == null) ||
                            // left range has unlimited past and future
                            (leftStartDate == null && leftEndDate == null) ||
                            // right range has unlimited past and future
                            (rightStartDate == null && rightEndDate == null) ||

                            // to this point if range has unlimited past (null), it has limited
                            // future and second range has limited past (not nulls),
                            // and if range has unlimited future (null), it has limited past and
                            // second range has limited future (not nulls), so
                            // we can check if ranges are overlap
                            (leftEndDate == null && !leftStartDate.after(rightEndDate)) ||
                            (rightEndDate == null && !rightStartDate.after(leftEndDate)) ||
                            (leftStartDate == null && !leftEndDate.before(rightStartDate)) ||
                            (rightStartDate == null && !rightEndDate.before(leftStartDate)) ||

                            (leftStartDate != null && leftEndDate != null &&
                                    rightStartDate != null && rightEndDate != null &&
                                    !leftStartDate.after(rightEndDate) && !leftEndDate.before(rightStartDate)))) {
                rangesOverlap = true;
            }

            return rangesOverlap;
        }
    }

    /**
     * Field Retriever for getting values from reference code of the corresponding bus.
     */
    public class RetrieveBusInfo implements FieldRetriever {
        private final String PARAM_ADVERTISING = "ADVERTISING";
        private final String PARAM_AIR_CONDITIONING = "AIR_CONDITIONING";
        private final String PARAM_BUS_NUMBER = "BUS_NUMBER";
        private final String PARAM_BUS_TYPE = "BUS_TYPE";
        private final String PARAM_COMMUNICATION = "COMMUNICATION";
        private final String PARAM_DAILY_MILES = "DAILY_MILES";
        private final String PARAM_ESCS = "ESCS";
        private final String PARAM_FATAL_OFF_BOARD = "FATAL_OFF_BOARD";
        private final String PARAM_FATAL_ON_BOARD = "FATAL_ON_BOARD";
        private final String PARAM_FUEL_TYPE = "FUEL_TYPE";
        private final String PARAM_GPS = "GPS";
        private final String PARAM_HOSPITALIZED = "HOSP_OVERNIGHT";
        private final String PARAM_INSPECTED = "INSPECTED";
        private final String PARAM_INVOLVED_ACCIDENT = "INVOLVED_ACCIDENT";
        private final String PARAM_PD_ACCIDENTS = "PD_ACCIDENTS";
        private final String PARAM_PI_ACCIDENTS = "PI_ACCIDENTS";
        private final String PARAM_PRIVATELY_OWNED = "PRIVATELY_OWNED";
        private final String PARAM_RESTRAINS = "RESTRAINS";
        private final String PARAM_SAFETY_COMPLAINTS = "SAFETY COMPLAINTS";
        private final String PARAM_SCHOOLYEAR = "SCHOOLYEAR";
        private final String PARAM_SPECIALLY_EQUIPPED = "SPECIALLY_EQUIPPED";
        private final String PARAM_SURVEILLANCE = "SURVEILLANCE";
        private final String PARAM_TOTAL_ACCIDENTS = "TOTAL_ACCIDENTS";
        private final String PARAM_TREATED = "TREATED_RELEASED";
        private final String PARAM_YEAR_BUS_BEGAN = "YEAR_BUS_BEGAN";
        private final String PARAM_WHEEL_CHAIRS_LIFTS = "WHEEL_CHAIRS_LIFTS";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            TNBusEntity tnEntity = (TNBusEntity) entity;
            UserDefinedTableD bus = tnEntity.getCurrentBus();
            Object value = null;
            if (PARAM_AIR_CONDITIONING.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldAirConditioning);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_BUS_NUMBER.equals(param)) {
                value = bus.getFieldValueByBeanPath(m_fieldBusNum);
            } else if (PARAM_BUS_TYPE.equals(param)) {
                value = bus.getFieldValueByBeanPath(m_fieldBusType);
            } else if (PARAM_ADVERTISING.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldAdvertising);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_COMMUNICATION.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldCommunication);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_DAILY_MILES.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldDailyMiles);
                try {
                    value = Integer.valueOf(Double.valueOf(rowValue).intValue());
                } catch (Exception e) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Field value cannot be identified", "Value: " +
                                    rowValue);
                    entity.addRetrievalError(field.getFieldId(), error);
                    value = "0";
                }

            } else if (PARAM_FUEL_TYPE.equals(param)) {
                value = bus.getFieldValueByBeanPath(m_fieldFuel);
            } else if (PARAM_GPS.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldGps);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_INSPECTED.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldInspected);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_TOTAL_ACCIDENTS.equals(param)) {
                value = integerValue(data, bus, m_fieldAccident);
            } else if (PARAM_INVOLVED_ACCIDENT.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldAccident);
                if (!StringUtils.isEmpty(rowValue)) {
                    try {
                        value = Integer.valueOf(Double.valueOf(rowValue).intValue());
                    } catch (NumberFormatException nfe) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Field value cannot be identified", "Value: " +
                                        rowValue);
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
            } else if (PARAM_RESTRAINS.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldRestraints);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_PRIVATELY_OWNED.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldPrivate);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_SCHOOLYEAR.equals(param)) {
                value = m_schoolYear;
            } else if (PARAM_SPECIALLY_EQUIPPED.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldSpeciallyEquipped);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_SURVEILLANCE.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldSurveillance);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_WHEEL_CHAIRS_LIFTS.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldChairLifts);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_ESCS.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldESCS);
                value = BooleanAsStringConverter.TRUE.equals(rowValue) ? "Y" : "N";
            } else if (PARAM_YEAR_BUS_BEGAN.equals(param)) {
                String rowValue = (String) bus.getFieldValueByBeanPath(m_fieldYearBegan);
                if (!StringUtils.isEmpty(rowValue)) {
                    try {
                        value = Integer.valueOf(Double.valueOf(rowValue).intValue());
                        value = value.toString();
                    } catch (NumberFormatException nfe) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Field value cannot be identified", "Value: " +
                                        rowValue);
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
            } else if (PARAM_PI_ACCIDENTS.equals(param)) {
                value = integerValue(data, bus, m_fieldPiAccidents);
            } else if (PARAM_TREATED.equals(param)) {
                value = integerValue(data, bus, m_fieldTreated);
            } else if (PARAM_HOSPITALIZED.equals(param)) {
                value = integerValue(data, bus, m_fieldHospitalized);
            } else if (PARAM_FATAL_ON_BOARD.equals(param)) {
                value = integerValue(data, bus, m_fieldFatilitiesOnBoard);
            } else if (PARAM_FATAL_OFF_BOARD.equals(param)) {
                value = integerValue(data, bus, m_fieldFatilitiesOffBoard);
            } else if (PARAM_PD_ACCIDENTS.equals(param)) {
                value = integerValue(data, bus, m_fieldPdAccidents);
            } else if (PARAM_SAFETY_COMPLAINTS.equals(param)) {
                value = integerValue(data, bus, m_fieldSafetyComplaints);
            }

            return value;
        }

        /**
         * Integer value.
         *
         * @param data StateReportData
         * @param bean UserDefinedTableD
         * @param beanPath String
         * @return Integer
         * @throws X2BaseException exception
         */
        Integer integerValue(StateReportData data, UserDefinedTableD bean, String beanPath) throws X2BaseException {
            Integer value = Integer.valueOf(0);
            Object rowValue = data.getPropertyAsJavaType(bean, beanPath);
            if (rowValue instanceof Number) {
                value = Integer.valueOf(((Number) rowValue).intValue());
            }
            return value;
        }
    }

    /**
     * Validate Fuel Type.
     */
    public class ValidateFuelType implements FieldValidator {
        private ArrayList<String> m_validFieldTypes = new ArrayList<String>();
        {
            m_validFieldTypes.add("GAS");
            m_validFieldTypes.add("DIESEL");
            m_validFieldTypes.add("CNG");
            m_validFieldTypes.add("ELEC");
            m_validFieldTypes.add("LNG");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            if (!m_validFieldTypes.contains(value)) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, "Fuel Type not valid", "Value = " +
                                value +
                                ". Valid fuel types are " +
                                m_validFieldTypes.toString());
                errors.add(error);
            }
            return errors;
        }

    }

    // Bean paths
    protected String m_fieldAccident;
    protected String m_fieldAdvertising;
    protected String m_fieldAirConditioning;
    protected String m_fieldAMBuses;
    protected String m_fieldBusNum;
    protected String m_fieldBusServiceBeginDate;
    protected String m_fieldBusServiceEndDate;
    protected String m_fieldBusType;
    protected String m_fieldChairLifts;
    protected String m_fieldCommunication;
    protected String m_fieldDailyMiles;
    protected String m_fieldESCS;
    protected String m_fieldFatilitiesOffBoard;
    protected String m_fieldFatilitiesOnBoard;
    protected String m_fieldFuel;
    protected String m_fieldGps;
    protected String m_fieldHospitalized;
    protected String m_fieldInspected;
    protected String m_fieldPdAccidents;
    protected String m_fieldPiAccidents;
    protected String m_fieldPrivate;
    protected String m_fieldRestraints;
    protected String m_fieldSafetyComplaints;
    protected String m_fieldSpeciallyEquipped;
    protected String m_fieldSurveillance;
    protected String m_fieldTreated;
    protected String m_fieldYearBegan;
    protected String m_schoolYear;

    private final String PARAM_ALL_BUSES = "allBuses";
    private final String PARAM_BUSES = "buses";

    // Static constants

    // Aliases
    protected static final String ALIAS_ACCIDENT = "DOE ACCIDENT";
    protected static final String ALIAS_ADVERTISING = "DOE ADVERTISING";
    protected static final String ALIAS_AIR_CONDITIONING = "DOE A/C";
    protected static final String ALIAS_AM_BUS = "DOE AM BUS";
    protected static final String ALIAS_BUS_DESCRIPTION = "DOE BUS DESCRIPTION";
    protected static final String ALIAS_BUS_DISABLED = "DOE BUS DISABLED";
    protected static final String ALIAS_BUS_NUM = "DOE BUS NUM";
    protected static final String ALIAS_BUS_TYPE = "DOE BUS TYPE";
    protected static final String ALIAS_BUS_SERVICE_BEGIN_DATE = "all-rcd-BusServiceBeginDate";
    protected static final String ALIAS_BUS_SERVICE_END_DATE = "all-rcd-BusServiceEndDate";
    protected static final String ALIAS_CHAIR_LIFTS = "DOE CHAIR LIFTS";
    protected static final String ALIAS_COMMUNICATION = "DOE COMMUNICATION";
    protected static final String ALIAS_DAILY_MILES = "DOE DAILY MILES";
    protected static final String ALIAS_DAILY_MILES_AM = "DOE DAILY ONE WAY AM MILES";
    protected static final String ALIAS_ESCS = "DOE ESCS";
    protected static final String ALIAS_FATAL_OFF_BOARD = "DOE FATALITIES OFF BOARD";
    protected static final String ALIAS_FATAL_ON_BOARD = "DOE FATALITIES ON BOARD";
    protected static final String ALIAS_FUEL = "DOE FUEL";
    protected static final String ALIAS_GPS = "DOE GPS";
    protected static final String ALIAS_HOSPITALIZED = "DOE HOSPITALIZED OVERNIGHT";
    protected static final String ALIAS_INSPECTED = "DOE INSPECTED";
    protected static final String ALIAS_PD_ACCIDENTS = "DOE PD ACCIDENTS";
    protected static final String ALIAS_PI_ACCIDENTS = "DOE PI ACCIDENTS";
    protected static final String ALIAS_PRIVATE = "DOE PRIVATE";
    protected static final String ALIAS_RESTRAINTS = "DOE RESTRAINTS";
    protected static final String ALIAS_SAFETY_COMPLAINTS = "DOE SAFETY COMPLAINTS";
    protected static final String ALIAS_SPECIALLY_EQUIPPED = "DOE SPECIALLY EQUIPPED";
    protected static final String ALIAS_SURVEILLANCE = "DOE SURVEILLANCE";
    protected static final String ALIAS_TOTAL_ACCIDENT = "DOE TOTAL ACCIDENTS";
    protected static final String ALIAS_TOTAL_TREATED = "DOE TREATED AND RELEASED";
    protected static final String ALIAS_YEAR_BEGAN = "DOE YEAR BEGAN";

    protected static final String PARAM_CURR_CONTEXT = "currentYearContext";

    protected Boolean m_allBuses;
    protected String m_busNumbersCodesOids;

    /**
     * A local copy of the data dictionary using the correct extended data dictionary.
     */
    private DataDictionary m_dictionaryBus;


    /**
     * Gets the data dictionary.
     *
     * @return Data dictionary
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getDataDictionary()
     */
    @Override
    public DataDictionary getDataDictionary() {
        if (m_dictionaryBus == null) {
            ExportFormatDefinition definition = getBroker().getBeanByOid(ExportFormatDefinition.class, getEfdOid());
            ExtendedDictionaryAttributes extendedDictionary = null;
            if (definition != null) {
                extendedDictionary = definition.getExtendedDataDictionary();
            }

            m_dictionaryBus = DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        }

        return m_dictionaryBus;
    }

    /**
     * Sets the version as soon as the current context is determined.
     *
     * @param currentContext void
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#setCurrentContext(com.follett.fsc.core.k12.beans.DistrictSchoolYearContext)
     */
    @Override
    public void setCurrentContext(DistrictSchoolYearContext currentContext) {
        if (currentContext.getSchoolYear() > 2019) {
            setExportVersion(4);
        } else if (currentContext.getSchoolYear() > 2017) {
            setExportVersion(3);
        } else if (currentContext.getSchoolYear() > 2015) {
            setExportVersion(2);
        }
        super.setCurrentContext(currentContext);
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        m_allBuses = (Boolean) getParameter(PARAM_ALL_BUSES);
        m_busNumbersCodesOids = (String) getParameter(PARAM_BUSES);
        if (getSetupErrors().size() != 0) {
            return;
        }

        // build the query for students to report.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
        QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

        setQuery(query);
        setEntityClass(TNBusEntity.class);
        // Add any necessary FieldRetrievers and FieldValidators
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("BUS_CALC", new RetrieveBusInfo());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldValidator>();
        validators.put("FUEL_TYPE", new ValidateFuelType());
        super.addValidators(validators);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldAirConditioning = translateAliasToJavaName(ALIAS_AIR_CONDITIONING, true);
        m_fieldAMBuses = translateAliasToJavaName(ALIAS_AM_BUS, true);
        m_fieldSurveillance = translateAliasToJavaName(ALIAS_SURVEILLANCE, true);
        m_fieldBusNum = translateAliasToJavaName(ALIAS_BUS_NUM, true);
        m_fieldSafetyComplaints = translateAliasToJavaName(ALIAS_SAFETY_COMPLAINTS, true);
        m_fieldESCS = translateAliasToJavaName(ALIAS_ESCS, true);
        m_fieldBusType = translateAliasToJavaName(ALIAS_BUS_TYPE, true);
        m_fieldAdvertising = translateAliasToJavaName(ALIAS_ADVERTISING, true);

        m_fieldFuel = translateAliasToJavaName(ALIAS_FUEL, true);
        m_fieldGps = translateAliasToJavaName(ALIAS_GPS, true);
        m_fieldInspected = translateAliasToJavaName(ALIAS_INSPECTED, true);

        m_fieldRestraints = translateAliasToJavaName(ALIAS_RESTRAINTS, true);
        m_fieldPrivate = translateAliasToJavaName(ALIAS_PRIVATE, true);
        m_fieldSpeciallyEquipped = translateAliasToJavaName(ALIAS_SPECIALLY_EQUIPPED, true);
        m_fieldCommunication = translateAliasToJavaName(ALIAS_COMMUNICATION, true);
        m_fieldChairLifts = translateAliasToJavaName(ALIAS_CHAIR_LIFTS, true);
        m_fieldYearBegan = translateAliasToJavaName(ALIAS_YEAR_BEGAN, true);
        m_fieldBusServiceBeginDate = translateAliasToJavaName(ALIAS_BUS_SERVICE_BEGIN_DATE, true);
        m_fieldBusServiceEndDate = translateAliasToJavaName(ALIAS_BUS_SERVICE_END_DATE, true);

        m_schoolYear = Integer.toString(getCurrentContext().getSchoolYear() - 1);

        if (getCurrentContext().getSchoolYear() > 2015) {
            m_fieldDailyMiles = translateAliasToJavaName(ALIAS_DAILY_MILES_AM, true);
            m_fieldAccident = translateAliasToJavaName(ALIAS_TOTAL_ACCIDENT, true);
            m_fieldPiAccidents = translateAliasToJavaName(ALIAS_PI_ACCIDENTS, true);
            m_fieldTreated = translateAliasToJavaName(ALIAS_TOTAL_TREATED, true);
            m_fieldHospitalized = translateAliasToJavaName(ALIAS_HOSPITALIZED, true);
            m_fieldFatilitiesOnBoard = translateAliasToJavaName(ALIAS_FATAL_ON_BOARD, true);
            m_fieldFatilitiesOffBoard = translateAliasToJavaName(ALIAS_FATAL_OFF_BOARD, true);
            m_fieldPdAccidents = translateAliasToJavaName(ALIAS_PD_ACCIDENTS, true);

        } else {
            m_fieldAccident = translateAliasToJavaName(ALIAS_ACCIDENT, true);
            m_fieldDailyMiles = translateAliasToJavaName(ALIAS_DAILY_MILES, true);
        }
    }
}
