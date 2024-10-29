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

import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_COURSE_NUMBER;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_DISTANCE_LEARNING_DELIVERY_INDICATOR;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_FACILITY_TYPE;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_HOMELESS_ADULT_PROGRAM_INDICATOR;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_SCHOOL_NUMBER_CIS;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_SECTION_NUMBER;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_SOCIAL_SECURITY_NUMBER;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_WDIS_CLASS_LENGTH;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.ScheduleTeacherInfo;
import com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;


/**
 * The Class FLWdisTeacherCourseData.
 */
public class FLWdisTeacherCourseData extends FLStateReportData {


    /**
     * The Class FLTcrsEntity.
     */
    public static class FLTcrsEntity extends FLStateReportEntity implements WdisEntity {
        private FLWdisTeacherCourseData m_data;
        private List<ScheduleTeacherInfo> m_schedules;


        /**
         * Instantiates a new FL tcrs entity.
         */
        public FLTcrsEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
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
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
         */
        @Override
        public StudentProgramParticipation getProgram() {
            return null;
        }


        /**
         * Gets the schedule info.
         *
         * @return Schedule teacher info
         */
        public ScheduleTeacherInfo getScheduleInfo() {
            return m_schedules.get(getCurrentRow());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLWdisTeacherCourseData) data;
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


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ScheduleTeacherInfo info = ((FLTcrsEntity) entity).getScheduleInfo();
            Object value = null;
            switch (field.getParameter().toString()) {
                case FIELD_SOCIAL_SECURITY_NUMBER:
                    value = info.getStaffInfo().getSSN();
                    break;
                default:
                    break;
            }
            return value;
        }

    }


    /**
     * The Class RetrieveTeacherScheduleInfo.
     */
    protected class RetrieveTeacherScheduleInfo implements FieldRetriever {
        public static final String CALC_ID = "SCHED_INFO";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ScheduleTeacherInfo info = ((FLTcrsEntity) entity).getScheduleInfo();
            Object value = null;
            switch (field.getParameter().toString()) {
                case FIELD_COURSE_NUMBER:
                    value = info.getMasterScheduleInfo().getCourseNumber();
                    break;
                case FIELD_FACILITY_TYPE:
                    value = info.getMasterScheduleInfo().getFacilityType();
                    break;
                case FIELD_SECTION_NUMBER:
                    value = info.getMasterScheduleInfo().getSectionNumber();
                    break;
                case FIELD_SCHOOL_NUMBER_CIS:
                    value = info.getMasterScheduleInfo().getSchoolNumber();
                    break;
                case FIELD_DISTANCE_LEARNING_DELIVERY_INDICATOR:
                    value = info.getMasterScheduleInfo().getDLDIndicator();
                    break;
                case FIELD_HOMELESS_ADULT_PROGRAM_INDICATOR:
                    value = info.getMasterScheduleInfo().getHomelessAdultProgramIndicator();
                    break;
                case FIELD_WDIS_CLASS_LENGTH:
                    value = info.getMasterScheduleInfo().getSectionHoursInSurveyPeriod();
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
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getStaffHelper()
     */
    @Override
    public FLStaffHelper getStaffHelper() {
        return m_staffHelper;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        m_staffHelper = new FLStaffHelper(this, true);
        m_staffHelper.setStaffSelectionMode(FLStaffHelper.MODE_SCHEDULED);
        m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
        m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());

        m_scheduleHelper = new FLScheduleHelper(this, getSurveyPeriod().getStartDate(), getSurveyPeriod().getEndDate());

        setQuery(m_staffHelper.getStaffQuery(false));
        setEntityClass(FLTcrsEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }


    /**
     * Gets the schedule helper.
     *
     * @return FL schedule helper
     */
    protected FLScheduleHelper getScheduleHelper() {
        return m_scheduleHelper;
    }


    /**
     * Register field retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveTeacherScheduleInfo.CALC_ID, new RetrieveTeacherScheduleInfo());
        calcs.put(RetrieveStaffInfo.CALC_ID, new RetrieveStaffInfo());
        calcs.put(RetrieveWdisYear.CALC_ID, new RetrieveWdisYear());
        super.addCalcs(calcs);
    }


    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }

}
