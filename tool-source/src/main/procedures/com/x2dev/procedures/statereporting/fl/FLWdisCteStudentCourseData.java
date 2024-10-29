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

import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.*;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;


/**
 * The Class FLWdisCteStudentCourseData.
 */
public class FLWdisCteStudentCourseData extends FLStateReportData {


    /**
     * The Class FLCteStudentCourseEntity.
     */
    public static class FLCteStudentCourseEntity extends FLStateReportEntity implements WdisEntity {
        private static final String ALIAS_COURSE_ID = "all-crs-StateId";

        private SisStudent m_bean;
        private FLWdisCteStudentCourseData m_data;
        private List<StudentScheduleInfo> m_records;


        /**
         * Instantiates a new FL cte student course entity.
         */
        public FLCteStudentCourseEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
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
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
         */
        @Override
        public StudentProgramParticipation getProgram() {
            StudentProgramParticipation pgm = null;

            List<StudentProgramParticipation> pgms =
                    m_data.m_studentProgramDataset.getPrograms(m_bean.getOid());
            if (pgms != null && pgms.size() > 0) {
                pgm = pgms.get(pgms.size() - 1);
            }

            return pgm;
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLWdisCteStudentCourseData) data;
            m_bean = (SisStudent) getBean();
            if (m_data.getStudentHelper().isStudentEligible(m_bean) && getProgram() != null) {
                m_records = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_bean);
                Collections.sort(m_records, new Comparator<StudentScheduleInfo>() {
                    @Override
                    public int compare(StudentScheduleInfo o1, StudentScheduleInfo o2) {
                        String courseId1 = (String) o1.getCourse().getFieldValueByAlias(ALIAS_COURSE_ID);
                        String courseId2 = (String) o2.getCourse().getFieldValueByAlias(ALIAS_COURSE_ID);
                        return courseId1.compareTo(courseId2);
                    }

                });
                setRowCount(m_records.size());
            } else {
                setRowCount(0);
            }
        }
    }


    /**
     * The Class RetrieveProgram.
     */
    class RetrieveProgram implements FieldRetriever {
        public static final String CALC_ID = "PROGRAM_INFO";

        private DataDictionaryField m_fieldPgmFullPgmCompleter;
        private final List<String> m_fullPgmCompletedCodes = Arrays.asList("A", "P", "V");


        /**
         * Instantiates a new retrieve program.
         */
        public RetrieveProgram() {
            m_fieldPgmFullPgmCompleter = translateAliasToDictionaryField(m_studentProgramDataset.getDataDictionary(),
                    ALIAS_PGM_CTE_FULL_PGM_COMPLETER, true);
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLCteStudentCourseEntity flEntity = (FLCteStudentCourseEntity) entity;
            FLWdisCteStudentCourseData flData = (FLWdisCteStudentCourseData) data;
            Object value = null;
            String param = (String) field.getParameter();
            switch (param) {
                case FIELD_FULL_PROGRAM_COMPLETER:
                    value = flData.getFieldValue(flEntity.getProgram(), m_fieldPgmFullPgmCompleter);
                    break;
                case FIELD_CTE_DATE_OF_PROGRAM_COMPLETION:
                    String fullProgramCompleter =
                            (String) flData.getFieldValue(flEntity.getProgram(), m_fieldPgmFullPgmCompleter);
                    if (m_fullPgmCompletedCodes.contains(fullProgramCompleter)) {
                        value = flEntity.getProgram().getEndDate();
                    }
                    break;

                default:
                    break;
            }
            return value;
        }
    }


    /**
     * The Class RetrieveScheduleInfo.
     */
    class RetrieveScheduleInfo implements FieldRetriever {
        public static final String CALC_ID = "SCHED_INFO";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLCteStudentCourseEntity flEntity = (FLCteStudentCourseEntity) entity;
            StudentScheduleInfo info = flEntity.getScheduleInfo();
            Object value = null;
            String param = (String) field.getParameter();
            switch (param) {
                case FIELD_ADULT_FEE_STATUS_FIRST:
                    value = info.getAdultFeeStatusFirst();
                    break;
                case FIELD_ADULT_FEE_STATUS_SECOND:
                    value = info.getAdultFeeStatusSecond();
                    break;
                case FIELD_COST_REPORTING_CODE:
                    value = info.getCostReportingCode();
                    break;
                case FIELD_COURSE_NUMBER:
                    value = info.getMasterScheduleInfo().getCourseNumber();
                    break;
                case FIELD_CTE_AGE_PROGRAM_CODE:
                    value = getProgramCode(flEntity.getProgram());
                    break;
                case FIELD_DATE_OF_ENTRY_PROGRAM_COURSE_SECTION:
                    value = info.getWdisEntryDate();
                    break;
                case FIELD_DATE_OF_EXIT_PROGRAM_COURSE_SECTION:
                    value = info.getWdisExitDate();
                    break;
                case FIELD_FINANCIAL_ASSISTANCE_RECEIVED:
                    value = info.getFinAssistanceReceived();
                    break;
                case FIELD_SECTION_NUMBER:
                    value = info.getMasterScheduleInfo().getSectionNumber();
                    break;
                case FIELD_WDIS_STUDENT_INSTRUCTIONAL_HOURS:
                    value = info.getWdisStudentInstrHrs();
                    break;
                case FIELD_GRADE_LEVEL:
                    value = info.getStudentInfo().getGradeLevel(getSurveyPeriod().getDateCertain());
                    break;
                case FIELD_POSTSECONDARY_SCHOOL_OF_ENROLLMENT:
                    value = info.getPostSecondarySchoolOfEnrollment();
                    break;
                case FIELD_POSTSECONDARY_COURSE_STATUS:
                    value = info.getWdisCourseStatus();
                    break;
                case FIELD_DUAL_ENROLLMENT_INDICATOR:
                    value = info.getDualEnrollmentIndicator();
                    break;
                case FIELD_CTE_DUAL_ENR_CRS_LOCATION:
                    value = info.getDualEnrollmentCrsLocation();
                    break;
                case FIELD_CTE_BASIC_SKILLS_EXAMINATION:
                    value = info.getBasicSkillsExam();
                    break;
                case FIELD_FULL_TIME_STUDENT_INDICATOR:
                    value = info.getFullTimeIndicator();
                    break;
                case FIELD_CTE_AGE_COMPLETION_POINT_CODE:
                    value = info.getWdisComplPoint(getProgramCode(flEntity.getProgram()));
                    break;
                case FIELD_TOTAL_CLOCK_HOURS_EARNED_TOWARDS_AWARD:
                    value = info.getWdisTotalHoursEarnedToAward();
                    break;
                default:
                    break;
            }
            return value;
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_G,
                    FLStateReportData.SURVEY_PERIOD_W, FLStateReportData.SURVEY_PERIOD_X,
                    FLStateReportData.SURVEY_PERIOD_S);

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentProgramDataset m_studentProgramDataset;
    protected StudentScheduleHelper m_studentScheduleHelper;

    private final static String DDX_ID_CTE = "FL-PGM-CTE";

    private final static String ALIAS_PGM_CTE_FULL_PGM_COMPLETER = "pgm-cte-full-program-compl";
    private final static String ALIAS_PGM_CTE_PROGRAM_ID = "pgm-cte-program-id";


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CUSTOM_SURVEY_PERIOD_VALID_CODES;
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
        getStudentHelper().setWdisMode(WDIS_MODE_POST_SECONDARY_CTE);
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLCteStudentCourseEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        m_studentProgramDataset = getStudentHelper().getStudentProgramDataset(DDX_ID_CTE, getSurveyPeriod());

        registerFieldRetrievers();
        registerFieldValidators();
    }


    /**
     * Gets the program code.
     *
     * @param pgm StudentProgramParticipation
     * @return String
     */
    private String getProgramCode(StudentProgramParticipation pgm) {
        return (String) pgm.getFieldValueByAlias(ALIAS_PGM_CTE_PROGRAM_ID,
                m_studentProgramDataset.getDataDictionary());
    }


    /**
     * Register field retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveScheduleInfo.CALC_ID, new RetrieveScheduleInfo());
        calcs.put(RetrieveProgram.CALC_ID, new RetrieveProgram());
        calcs.put(RetrieveWdisYear.CALC_ID, new RetrieveWdisYear());

        super.addCalcs(calcs);
    }


    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        super.addValidators(validators);
    }


}
