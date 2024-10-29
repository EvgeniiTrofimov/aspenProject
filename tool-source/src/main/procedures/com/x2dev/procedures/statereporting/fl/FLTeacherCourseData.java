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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.ScheduleTeacherInfo;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLTeacherCourseData.
 */
public class FLTeacherCourseData extends FLStateReportData {

    /**
     * The Class FLTeacherCourseEntity.
     */
    public static class FLTeacherCourseEntity extends FLStateReportEntity {
        private FLTeacherCourseData m_data;
        private List<ScheduleTeacherInfo> m_schedules;

        /**
         * Instantiates a new FL teacher course entity.
         */
        public FLTeacherCourseEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return the entity name
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
         * Gets the schedule info.
         *
         * @return the schedule info
         */
        public ScheduleTeacherInfo getScheduleInfo() {
            return m_schedules.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLTeacherCourseData) data;
            List<ScheduleTeacher> schedules = m_data.getStaffHelper().getTeacherSchedules(bean.getOid());
            if (schedules != null && !schedules.isEmpty()) {
                m_schedules = new ArrayList(schedules.size());
                for (ScheduleTeacher schedule : schedules) {
                    m_schedules.add(m_data.getStaffHelper().new ScheduleTeacherInfo(schedule));
                }
                setRowCount(m_schedules.size());
            } else {
                setRowCount(0);
            }
        }
    }


    /**
     * The Class RetrieveStaffInfo.
     */
    protected class RetrieveStaffInfo implements FieldRetriever {
        public static final String CALC_ID = "STAFF_INFO";
        private static final String PARAM_CERTIFICATION_NUMBER = "CERTIFICATION NUMBER";
        private static final String PARAM_SSN = "SSN";

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
            ScheduleTeacherInfo info = ((FLTeacherCourseEntity) entity).getScheduleInfo();
            Object value = null;
            switch (field.getParameter().toString()) {
                case PARAM_CERTIFICATION_NUMBER:
                    value = info.getStaffInfo().getCertificationNumber();
                    break;
                case PARAM_SSN:
                    value = info.getStaffInfo().getSSN();
                    break;
                default:
                    break;
            }
            return value;
        }

    }

    /**
     * The Class RetrieveStudentScheduleInfo.
     */
    protected class RetrieveStudentScheduleInfo implements FieldRetriever {
        public static final String CALC_ID = "SCHED_INFO";
        private static final String PARAM_BLENDED_LEARNING_COURSE = "BLENDED LEARNING COURSE";
        private static final String PARAM_CERTIFICATION_STATUS = "CERTIFICATION STATUS";
        private static final String PARAM_COURSE = "COURSE";
        private static final String PARAM_FACILITY_TYPE = "FACILITY TYPE";
        private static final String PARAM_FISH_NUMBER = "FISH NUMBER";
        private static final String PARAM_FUND_SOURCE = "FUND SOURCE";
        private static final String PARAM_HIGHLY_QUALIFIED_STATUS = "HIGHLY QUALIFIED STATUS";
        private static final String PARAM_PERIOD_NUMBER = "PERIOD NUMBER";
        private static final String PARAM_PRIMARY_INSTRUCTOR = "PRIMARY INSTRUCTOR";
        private static final String PARAM_SCHEDULING_METHOD = "SCHEDULING METHOD";
        private static final String PARAM_SECTION = "SECTION";
        private static final String PARAM_STUDENT_SCHOOL = "STUDENT SCHOOL";
        private static final String PARAM_TEAM_TEACHER_TRAINING = "TEAM TEACHER TRAINING";
        private static final String PARAM_TERM = "TERM";
        private static final String PARAM_TERM_DAYS = "TERM DAYS";
        private static final String PARAM_TERM_SURVEY_INDICATOR = "TERM SURVEY INDICATOR";
        private static final String VALUE_NOT_APPLICABLE = "Z";

        private final List<String> SCHOOL_YEAR_VALID_CODES = Arrays.asList(
                FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3);

        private DataDictionaryField m_fieldScheduleTermCode;

        /**
         * Instantiates a new retrieve student schedule info.
         */
        public RetrieveStudentScheduleInfo() {
            m_fieldScheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
        }

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
            ScheduleTeacherInfo info = ((FLTeacherCourseEntity) entity).getScheduleInfo();
            FLTeacherCourseData flData = (FLTeacherCourseData) data;
            Object value = null;
            switch (field.getParameter().toString()) {

                case PARAM_BLENDED_LEARNING_COURSE:
                    value = info.getMasterScheduleInfo().getBlendedLearningIndicator();
                    break;
                case PARAM_CERTIFICATION_STATUS:
                    value = info.getCertificationStatus();
                    break;
                case PARAM_COURSE:
                    value = info.getMasterScheduleInfo().getCourseNumber();
                    break;
                case PARAM_FACILITY_TYPE:
                    value = info.getMasterScheduleInfo().getFacilityType();
                    break;
                case PARAM_FISH_NUMBER:
                    // if (SCHOOL_YEAR_VALID_CODES.contains(flData.getSurveyPeriod().getCode())) {
                    value = info.getMasterScheduleInfo().getClassroomIdentificationNo();
                    // }
                    break;
                case PARAM_FUND_SOURCE:
                    if (SCHOOL_YEAR_VALID_CODES.contains(flData.getSurveyPeriod().getCode())) {
                        value = info.getNCLBTitleIIIFundedIndicator();
                        if (!((Boolean) value).booleanValue()) {
                            value = info.getMasterScheduleInfo().getNCLBTitleIIIFundedIndicator();
                        }
                    }
                    break;
                case PARAM_HIGHLY_QUALIFIED_STATUS:
                    value = info.getHighlyQualifiedStatus();
                    break;
                case PARAM_PERIOD_NUMBER:
                    value = info.getMasterScheduleInfo().getPeriodNumber();
                    break;
                case PARAM_PRIMARY_INSTRUCTOR:
                    value = info.getPrimaryTeacherIndicator();
                    break;
                case PARAM_SCHEDULING_METHOD:
                    if (SCHOOL_YEAR_VALID_CODES.contains(flData.getSurveyPeriod().getCode())) {
                        value = info.getSchedulingMethod();
                    } else {
                        value = VALUE_NOT_APPLICABLE;
                    }
                    break;
                case PARAM_SECTION:
                    value = info.getMasterScheduleInfo().getSectionNumber();
                    break;
                case PARAM_STUDENT_SCHOOL:
                    value = info.getMasterScheduleInfo().getSchoolNumber();
                    break;
                case PARAM_TEAM_TEACHER_TRAINING:
                    if (SCHOOL_YEAR_VALID_CODES.contains(flData.getSurveyPeriod().getCode())) {
                        value = info.getTeamTeacherTraining();
                    } else {
                        value = VALUE_NOT_APPLICABLE;
                    }
                    break;
                case PARAM_TERM:
                    value = flData.getFieldValue(info.getMasterScheduleInfo().getSection().getScheduleTerm(),
                            m_fieldScheduleTermCode);
                    break;
                case PARAM_TERM_DAYS:
                    value = info.getMasterScheduleInfo().getDaysInTerm();
                    break;
                case PARAM_TERM_SURVEY_INDICATOR:
                    value = Boolean.FALSE;
                    String termOid = info.getMasterScheduleInfo().getSection().getScheduleTermOid();
                    Collection<ScheduleTermDate> dates = flData.getScheduleHelper().getScheduleTermDates(termOid);
                    for (ScheduleTermDate tmd : dates) {
                        if (!tmd.getStartDate().after(flData.getSurveyPeriod().getEndDate()) &&
                                !tmd.getEndDate().before(flData.getSurveyPeriod().getStartDate())) {
                            value = Boolean.TRUE;
                            break;
                        }
                    }
                    break;
                default:
                    break;
            }
            return value;
        }

    }

    private FLScheduleHelper m_scheduleHelper;
    private FLStaffHelper m_staffHelper;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        translateAliasToDictionaryField(FLStaffHelper.ALIAS_CERTIFICATION_STATUS_MTC, true);
        translateAliasToDictionaryField(FLStaffHelper.ALIAS_HIGHLY_QUALIFIED_STATUS_MTC, true);
        translateAliasToDictionaryField(FLStaffHelper.ALIAS_SCHEDULING_METHO_MTC, true);
        translateAliasToDictionaryField(FLStaffHelper.ALIAS_TEAM_TEACHER_TRAINING_MTC, true);
        translateAliasToDictionaryField(FLStaffHelper.ALIAS_TITLE_III_FUNDED_MTC, true);

        if (getSetupErrors().size() != 0) {
            return;
        }

        m_staffHelper = new FLStaffHelper(this);
        m_staffHelper.setStaffSelectionMode(FLStaffHelper.MODE_SCHEDULED);
        m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
        m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());

        m_scheduleHelper = new FLScheduleHelper(this, getSurveyPeriod().getStartDate(), getSurveyPeriod().getEndDate());

        setQuery(m_staffHelper.getStaffQuery(false));
        setEntityClass(FLTeacherCourseEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the schedule helper.
     *
     * @return the schedule helper
     */
    protected FLScheduleHelper getScheduleHelper() {
        return m_scheduleHelper;
    }

    /**
     * Gets the staff helper.
     *
     * @return the staff helper
     */
    @Override
    public FLStaffHelper getStaffHelper() {
        return m_staffHelper;
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveStudentScheduleInfo.CALC_ID, new RetrieveStudentScheduleInfo());
        calcs.put(RetrieveStaffInfo.CALC_ID, new RetrieveStaffInfo());
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
