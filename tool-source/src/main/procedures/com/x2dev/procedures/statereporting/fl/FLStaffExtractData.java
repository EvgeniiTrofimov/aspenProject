/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.StaffInfo;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * FL Staff Demographic Information report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2015-16-student-info-system/student-demographic-info.stml
 *
 * @author Follett Software Company
 */
public class FLStaffExtractData extends FLStateReportData {

    /**
     * The Class FLStaffDemographicEntity.
     */
    public static class FLStaffDemographicEntity extends FLStateReportEntity {
        private SisStaff m_record;

        /**
         * Instantiates a new FL staff demographic entity.
         */
        public FLStaffDemographicEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return Sis staff
         */
        public SisStaff getCurrentRecord() {
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
            SisStaff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    "] ";
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

            m_record = (SisStaff) getBean();
            setRowCount(1);
        }
    }

    /**
     * Retriever for certificate number.
     */
    protected class RetrieveCertificateNumber implements FieldRetriever {
        public static final String CALC_ID = "CERTIIFICATE_NUMBER";

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
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            StaffInfo info = getStaffHelper().getStaffInfo(staff);
            return info.getCertificationNumber();
        }
    }

    /**
     * Retriever for absent/present days.
     */
    protected class RetrieveDays implements FieldRetriever {
        public static final String CALC_ID = "DAYS";

        private static final String PARAM_ABSENT_OTHER = "  ";
        private static final String PARAM_ABSENT_PERSONAL_LEAVE = "ABSENT_PERSONAL_LEAVE";
        private static final String PARAM_ABSENT_SICK_LEAVE = "ABSENT_SICK_LEAVE";
        private static final String PARAM_ABSENT_TEMPORARY_DUTY_ELSEWHERE = "ABSENT_TEMPORARY_DUTY_ELSEWHERE";
        private static final String PARAM_PRESENT = "PRESENT";

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
            Object value = null;

            // TODO: we need more information to extract the values below
            // SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            // StaffInfo info = m_staffHelper.getStaffInfo(staff);
            if (!FLStateReportData.SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode()) &&
                    !FLStateReportData.SURVEY_PERIOD_3.equals(getSurveyPeriod().getCode())) {
                if (PARAM_ABSENT_OTHER.equals(field.getParameter())) {
                    value = "0";
                } else if (PARAM_ABSENT_PERSONAL_LEAVE.equals(field.getParameter())) {
                    value = "0";
                }
                if (PARAM_ABSENT_SICK_LEAVE.equals(field.getParameter())) {
                    value = "0";
                }
                if (PARAM_ABSENT_TEMPORARY_DUTY_ELSEWHERE.equals(field.getParameter())) {
                    value = "0";
                }
                if (PARAM_PRESENT.equals(field.getParameter())) {
                    value = "180";
                }
            }
            return value;
        }
    }

    /**
     * Retriever for current position employment day.
     */
    protected class RetrieveEmploymentDate implements FieldRetriever {
        public static final String CALC_ID = "EMPLOYMENT_DAY";

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
            Object value = null;

            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            StaffPosition primary = getStaffHelper().getPrimaryStaffPosition(staff);
            if (primary != null) {
                String jobCode = primary.getJobCode();
                if (!StringUtils.isEmpty(jobCode)) {
                    List<StaffPosition> positions =
                            new ArrayList<StaffPosition>(getStaffHelper().getStaffPositions(staff));
                    int index = positions.indexOf(primary);
                    StaffPosition continous = primary;
                    for (int i = index - 1; i >= 0; i--) {
                        StaffPosition current = positions.get(i);
                        if (!jobCode.equals(current.getJobCode()) ||
                                !datesAdjacent(current.getEndDate(), continous.getStartDate())) {
                            break;
                        }
                        continous = current;
                    }
                    value = continous.getStartDate();
                }
            }
            return value;
        }

        /**
         * Dates adjacent.
         *
         * @param priorDate PlainDate
         * @param nextDate PlainDate
         * @return true, if successful
         */
        private boolean datesAdjacent(PlainDate priorDate, PlainDate nextDate) {
            if (priorDate.before(nextDate)) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(priorDate);
                cal.add(Calendar.DATE, 1);
                priorDate = new PlainDate(cal.getTime());
            }
            return !priorDate.before(nextDate);
        }
    }

    /**
     * Retriever for Personnel Evaluation fields
     *
     * @author Follett Software Company
     */
    protected class RetrievePersonnelEvaluation implements FieldRetriever {
        public static final String CALC_ID = "CALC_PE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            if (!FLStateReportData.SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode())
                    && !FLStateReportData.SURVEY_PERIOD_3.equals(getSurveyPeriod().getCode())) {
                value = staff.getFieldValueByAlias((String) field.getParameter());
            }
            return value;
        }
    }

    /**
     * Retriever for Primary Job Code
     * http://www.fldoe.org/core/fileparse.php/12025/urlt/1516-209310.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryJobCode implements FieldRetriever {
        public static final String CALC_ID = "JOB_CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            StaffPosition primary = getStaffHelper().getPrimaryStaffPosition(staff);
            if (primary != null) {
                value = primary.getJobCode();
            }
            return value;
        }
    }

    /**
     * Retriever for race.
     */
    protected class RetrieveRace implements FieldRetriever {
        public static final String CALC_ID = "RACE";

        private static final int INITIAL_MAP_SIZE = 200000;

        private static final String VALUE_NO = "N";
        private static final String VALUE_YES = "Y";

        private Map<String, Collection<Race>> m_raceMap;

        /**
         * Instantiates a new retrieve race.
         *
         * @throws X2BaseException exception
         */
        public RetrieveRace() throws X2BaseException {
            SubQuery staffSubQuery =
                    new SubQuery(SisStaff.class, SisStaff.COL_PERSON_OID, getStaffHelper().getStaffCriteria());
            X2Criteria raceCriteria = new X2Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, staffSubQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, INITIAL_MAP_SIZE);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            String value = VALUE_NO;
            Collection<Race> races = m_raceMap.get(staff.getPersonOid());
            String raceCode = (String) field.getParameter();
            if (races != null) {
                for (Race race : races) {
                    if (raceCode.equalsIgnoreCase(race.getRaceCode())) {
                        value = VALUE_YES;
                        break;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for Separation values
     *
     * @author Follett Software Company
     */
    protected class RetrieveSeparation implements FieldRetriever {
        public static final String CALC_ID = "CALC_SEP";

        private static final String ALIAS_STF_SEPARATION_DATE = "all-stf-SeparationDate";

        private SimpleDateFormat m_toAliasDateFormat = new SimpleDateFormat(DateAsStringConverter.STRING_DATE_FORMAT);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            if (!FLStateReportData.SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode())
                    && !FLStateReportData.SURVEY_PERIOD_3.equals(getSurveyPeriod().getCode())) {
                value = staff.getFieldValueByAlias((String) field.getParameter());
                if (ALIAS_STF_SEPARATION_DATE.equals(field.getParameter()) && !StringUtils.isEmpty((String) value)) {
                    try {
                        value = new PlainDate(m_toAliasDateFormat.parse((String) value));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for staff legal name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveStaffName implements FieldRetriever {
        public static final String CALC_ID = "STF_NAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStaff staff = ((FLStaffDemographicEntity) entity).getCurrentRecord();
            StaffInfo info = getStaffHelper().getStaffInfo(staff);
            return info.formatStaffLegalName();
        }
    }

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

        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_BEGIN_DATE, getSurveyPeriod().getStartDate());
        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());
        setQuery(getStaffHelper().getStaffQuery(false));
        setEntityClass(FLStaffDemographicEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
        calcs.put(RetrieveStaffName.CALC_ID, new RetrieveStaffName());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveStaffSsn.CALC_ID, new RetrieveStaffSsn());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveStaffPrimarySchoolNumber.CALC_ID, new RetrieveStaffPrimarySchoolNumber());
        calcs.put(RetrieveCertificateNumber.CALC_ID, new RetrieveCertificateNumber());
        calcs.put(RetrieveDays.CALC_ID, new RetrieveDays());
        calcs.put(RetrievePrimaryJobCode.CALC_ID, new RetrievePrimaryJobCode());
        calcs.put(RetrieveEmploymentDate.CALC_ID, new RetrieveEmploymentDate());
        calcs.put(RetrievePersonnelEvaluation.CALC_ID, new RetrievePersonnelEvaluation());
        calcs.put(RetrieveSeparation.CALC_ID, new RetrieveSeparation());
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
