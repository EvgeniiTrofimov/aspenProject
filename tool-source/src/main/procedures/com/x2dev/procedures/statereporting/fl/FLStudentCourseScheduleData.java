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
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * The Class FLStudentCourseScheduleData.
 */
public class FLStudentCourseScheduleData extends FLStateReportData {

    /**
     * The Class FLStudentCourseScheduleEntity.
     */
    public static class FLStudentCourseScheduleEntity extends FLStateReportEntity {
        private SisStudent m_bean;
        private FLStudentCourseScheduleData m_data;
        private List<StudentScheduleInfo> m_records;

        /**
         * Instantiates a new FL student course schedule entity.
         */
        public FLStudentCourseScheduleEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentScheduleInfo info = getScheduleInfo();
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", " + info.getSection().getCourseView() +
                    "] ";
            return name;
        }

        /**
         * Gets the schedule info.
         *
         * @return Student schedule info
         */
        public StudentScheduleInfo getScheduleInfo() {
            return m_records.get(getCurrentRow());
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

            m_data = (FLStudentCourseScheduleData) data;
            m_bean = (SisStudent) getBean();
            if (m_data.getStudentHelper().isStudentEligible(m_bean)) {
                m_records = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_bean);
                // remove duplicate sections
                Set<String> mstOids = new HashSet();
                Iterator<StudentScheduleInfo> iterator = m_records.iterator();
                while (iterator.hasNext()) {
                    StudentScheduleInfo item = iterator.next();
                    String mstOid = item.getSection().getOid();
                    if (mstOids.contains(mstOid)) {
                        iterator.remove();
                    } else {
                        mstOids.add(mstOid);
                    }
                }
                setRowCount(m_records.size());
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * The Class RetrieveStudentScheduleInfo.
     */
    protected class RetrieveStudentScheduleInfo implements FieldRetriever {
        public static final String CALC_ID = "SCHED_INFO";

        private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";
        private static final String ALIAS_ONLINE_PROVIDER = "all-mst-OnlineProvider";
        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private static final String PARAM_COURSE = "COURSE";
        private static final String PARAM_COURSE_GRADE = "COURSE GRADE";
        private static final String PARAM_CTE_PROGRAM = "CTE PROGRAM";
        private static final String PARAM_DAYS_PER_WEEK = "DAYS PER WEEK";
        private static final String PARAM_DISTRICT = "SECTION DISTRICT";
        private static final String PARAM_DUAL_ENROLLMENT = "DUAL ENROLLMENT";
        private static final String PARAM_ELL_INSTRUCTIONAL_MODEL = "ELL MODEL";
        private static final String PARAM_FEFP_PROGRAM_NUMBER = "FEFP PROGRAM NUMBER";
        private static final String PARAM_FTE = "FTE";
        private static final String PARAM_GRADE_LEVEL = "GRADE LEVEL";
        private static final String PARAM_ONLINE_PROVIDER = "ONLINE PROVIDER";
        private static final String PARAM_PERIOD_NUMBER = "PERIOD NUMBER";
        private static final String PARAM_READING_INTERVENTION = "READING INTERVENTION";
        private static final String PARAM_SCHEDULED_CERTAIN = "SCHEDULED CERTAIN";
        private static final String PARAM_SCHEDULED_FRIDAY = "SCHEDULED FRIDAY";
        private static final String PARAM_SCHEDULED_MONDAY = "SCHEDULED MONDAY";
        private static final String PARAM_SCHEDULED_SATURDAY = "SCHEDULED SATURDAY";
        private static final String PARAM_SCHEDULED_THURSDAY = "SCHEDULED THURSDAY";
        private static final String PARAM_SCHEDULED_TUESDAY = "SCHEDULED TUESDAY";
        private static final String PARAM_SCHEDULED_WEDNESDAY = "SCHEDULED WEDNESDAY";
        private static final String PARAM_SCHOOL = "SECTION SCHOOL";
        private static final String PARAM_SCHOOL_STUDENT = "STUDENT SCHOOL";
        private static final String PARAM_SECTION = "SECTION";
        private static final String PARAM_TERM = "TERM";
        private static final String PARAM_VIRTUAL_INSTRUCTION_PROVIDER = "VIRTUAL INSTRUCTION";
        private static final String PARAM_VIRTUAL_LOCATION = "VIRTUAL LOCATION";
        private static final String PARAM_WEEKLY_MINUTES = "WEEKLY MINUTES";

        private final List<String> READING_INTERVENTION_GRADES = Arrays.asList("KG", "01", "02", "03", "04", "05");

        private DataDictionaryField m_fieldDistrictNumber;
        private DataDictionaryField m_fieldOnlineProvider;
        private DataDictionaryField m_fieldScheduleTermCode;
        private DataDictionaryField m_fieldSchoolNumber;

        /**
         * Instantiates a new retrieve student schedule info.
         *
         * @param studentHelper FLStudentHelper
         * @throws X2BaseException exception
         */
        public RetrieveStudentScheduleInfo(FLStudentHelper studentHelper) throws X2BaseException {
            // Initialize FTE Calculator to return any potential setup errors
            studentHelper.getFteCalculator();

            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldOnlineProvider = translateAliasToDictionaryField(ALIAS_ONLINE_PROVIDER, true);
            m_fieldScheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((FLStudentCourseScheduleEntity) entity).getScheduleInfo();
            FLStudentCourseScheduleData flData = (FLStudentCourseScheduleData) data;
            Object value = null;
            switch (field.getParameter().toString()) {
                case PARAM_COURSE:
                    value = info.getMasterScheduleInfo().getCourseNumber();
                    break;
                case PARAM_COURSE_GRADE:
                    value = info.getTranscriptGradeLetter();
                    break;
                case PARAM_CTE_PROGRAM:
                    value = info.getMasterScheduleInfo().getCteProgramNumber();
                    break;
                case PARAM_DAYS_PER_WEEK:
                    value = info.getMasterScheduleInfo().getDaysPerWeek();
                    break;
                case PARAM_DISTRICT:
                    value = flData.getFieldValue(info.getSection().getSchedule().getSchool().getOrganization1(),
                            m_fieldDistrictNumber);
                    break;
                case PARAM_DUAL_ENROLLMENT:
                    value = info.getDualEnrollmentIndicator();
                    break;
                case PARAM_ELL_INSTRUCTIONAL_MODEL:
                    value = info.getEllInstructionalModel();
                    break;
                case PARAM_FEFP_PROGRAM_NUMBER:
                    value = info.getStudentInfo().getFefpProgram();
                    break;
                case PARAM_FTE:
                    value = info.getFte();
                    break;
                case PARAM_GRADE_LEVEL:
                    value = info.getStudentInfo().getGradeLevel(flData.getSurveyPeriod().getDateCertain());
                    break;
                case PARAM_ONLINE_PROVIDER:
                    value = flData.getFieldValue(info.getSection(), m_fieldOnlineProvider);
                    break;
                case PARAM_PERIOD_NUMBER:
                    value = info.getMasterScheduleInfo().getPeriodNumber();
                    break;
                case PARAM_READING_INTERVENTION:
                    String gradeLevel = info.getStudentInfo().getGradeLevel(flData.getSurveyPeriod().getDateCertain());
                    if (READING_INTERVENTION_GRADES.contains(gradeLevel)
                            && !flData.getSurveyPeriod().getCode().equals(SURVEY_PERIOD_1)
                            && !flData.getSurveyPeriod().getCode().equals(SURVEY_PERIOD_4)) {
                        value = info.getReadingInterventionIndicator();
                    }
                    break;
                case PARAM_SCHEDULED_CERTAIN:
                    if (!((SURVEY_PERIOD_2.equals(getSurveyPeriodCode()) || SURVEY_PERIOD_3.equals(getSurveyPeriodCode()))
                            && info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.FRIDAY).booleanValue())) {
                        value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.MONDAY);
                    }
                    break;
                case PARAM_SCHEDULED_MONDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.MONDAY);
                    break;
                case PARAM_SCHEDULED_TUESDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.TUESDAY);
                    break;
                case PARAM_SCHEDULED_WEDNESDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.WEDNESDAY);
                    break;
                case PARAM_SCHEDULED_THURSDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.THURSDAY);
                    break;
                case PARAM_SCHEDULED_FRIDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.FRIDAY);
                    break;
                case PARAM_SCHEDULED_SATURDAY:
                    value = info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.SATURDAY);
                    break;
                case PARAM_SCHOOL:
                    value = flData.getFieldValue(info.getSchool(), m_fieldSchoolNumber);
                    break;
                case PARAM_SCHOOL_STUDENT:
                    value = flData.getFieldValue(info.getStudentInfo().getSchool(getSurveyPeriod().getDateCertain()),
                            m_fieldSchoolNumber);
                    break;
                case PARAM_SECTION:
                    value = info.getMasterScheduleInfo().getSectionNumber();
                    break;
                case PARAM_TERM:
                    value = flData.getFieldValue(info.getSection().getScheduleTerm(), m_fieldScheduleTermCode);
                    break;
                case PARAM_VIRTUAL_INSTRUCTION_PROVIDER:
                    value = info.getMasterScheduleInfo().getVirtualInstructionProvider();
                    break;
                case PARAM_VIRTUAL_LOCATION:
                    value = info.getMasterScheduleInfo().getVirtualCourseLocation();
                    break;
                case PARAM_WEEKLY_MINUTES:
                    value = info.getMasterScheduleInfo().getMinutesPerWeek();
                    break;
                default:
                    break;
            }
            return value;
        }

    }

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentScheduleHelper m_studentScheduleHelper;

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

        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLStudentCourseScheduleEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        // TODO: Add initialization which produces setup errors here
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveStudentScheduleInfo.CALC_ID, new RetrieveStudentScheduleInfo(getStudentHelper()));
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addCalcs(calcs);
        // TODO: register retrievers
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        super.addValidators(validators);
        // TODO: register validators
    }

}
