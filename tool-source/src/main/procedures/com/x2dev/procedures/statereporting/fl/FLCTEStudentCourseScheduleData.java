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
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLCTEStudentCourseScheduleData.
 */
public class FLCTEStudentCourseScheduleData extends FLStateReportData {

    /**
     * The Class FLStudentCourseScheduleEntity.
     */
    public static class FLStudentCourseScheduleEntity extends FLStateReportEntity {
        private SisStudent m_bean;
        private FLCTEStudentCourseScheduleData m_data;
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

            m_data = (FLCTEStudentCourseScheduleData) data;
            m_bean = (SisStudent) getBean();
            if (m_data.getStudentHelper().isStudentEligible(m_bean)) {
                m_records = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_bean);
                setRowCount(m_records.size());
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * Field retriever for SCHED_INFO Fields.
     */
    protected class RetrieveStudentScheduleInfo implements FieldRetriever {
        public static final String CALC_ID = "SCHED_INFO";

        private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";
        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private static final String PARAM_COURSE = "COURSE";
        private static final String PARAM_CTE_COMPLETION_POINT = "CTE COMPLETION POINT";
        private static final String PARAM_CTE_EXCEPT = "CTE EXCEPT";
        private static final String PARAM_CTE_PROGRAM = "CTE PROGRAM";
        private static final String PARAM_DISTRICT = "SECTION DISTRICT";
        private static final String PARAM_FEFP_PROGRAM_NUMBER = "FEFP PROGRAM NUMBER";
        private static final String PARAM_GRADE_LEVEL = "GRADE LEVEL";
        private static final String PARAM_PERIOD_NUMBER = "PERIOD NUMBER";
        private static final String PARAM_SCHOOL = "SECTION SCHOOL";
        private static final String PARAM_SCHOOL_STUDENT = "STUDENT SCHOOL";
        private static final String PARAM_SECTION = "SECTION";
        private static final String PARAM_TERM = "TERM";

        private DataDictionaryField m_fieldDistrictNumber;
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

            m_fieldScheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
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
            StudentScheduleInfo info = ((FLStudentCourseScheduleEntity) entity).getScheduleInfo();
            FLCTEStudentCourseScheduleData flData = (FLCTEStudentCourseScheduleData) data;
            Object value = null;
            switch (field.getParameter().toString()) {
                case PARAM_COURSE:
                    value = info.getMasterScheduleInfo().getCourseNumber();
                    break;
                case PARAM_CTE_COMPLETION_POINT:
                    value = info.getMasterScheduleInfo().getCteCompletionPointCode();
                    break;
                case PARAM_CTE_EXCEPT:
                    value = info.getCteCourseSetting();
                    break;
                case PARAM_CTE_PROGRAM:
                    value = info.getMasterScheduleInfo().getCteProgramNumber();
                    break;
                case PARAM_DISTRICT:
                    value = flData.getFieldValue(info.getSection().getSchedule().getSchool().getOrganization1(),
                            m_fieldDistrictNumber);
                    break;
                case PARAM_FEFP_PROGRAM_NUMBER:
                    value = info.getStudentInfo().getFefpProgram();
                    break;
                case PARAM_GRADE_LEVEL:
                    value = info.getStudentInfo().getGradeLevel(flData.getSurveyPeriod().getDateCertain());
                    break;
                case PARAM_PERIOD_NUMBER:
                    value = info.getMasterScheduleInfo().getPeriodNumber();
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
                default:
                    break;
            }
            return value;
        }
    }

    /**
     * Field retriever for CTE Fields.
     */
    protected class RetrieveCteFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_CTE";
        public static final String DDX_ID = "FL-PGM-CTE";

        private static final String ALIAS_COMPLETION_POINT = "pgm-completion-point";
        private static final String ALIAS_INTERNSHIP_PARTICIPANT = "pgm-internship-participant";
        private static final String ALIAS_MOCP = "pgm-mocp";

        private static final String PARAM_COMPLETION_POINT = "COMPLETION POINT";
        private static final String PARAM_INTERNSHIP_PARTICIPANT = "INTERNSHIP PARTICIPANT";
        private static final String PARAM_MOCP = "MOCP";

        private DataDictionaryField m_fieldCompletionPoint;
        private DataDictionaryField m_fieldInternshipParticipant;
        private DataDictionaryField m_fieldMOCP;

        /**
         * Instantiates a new retrieve cte fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveCteFields() throws X2BaseException {
            StudentProgramDataset pgmCteDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary pgmCteDataDictionary = pgmCteDataset.getDataDictionary();

            m_fieldCompletionPoint = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_COMPLETION_POINT, true);
            m_fieldInternshipParticipant = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_INTERNSHIP_PARTICIPANT, true);
            m_fieldMOCP = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_MOCP, true);
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

            FLCTEStudentCourseScheduleData flData = (FLCTEStudentCourseScheduleData) data;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(entity.getBean().getOid(), DDX_ID, flData.getSurveyPeriod());

            String parameter = (String) field.getParameter();
            Object value = null;

            if (pgm != null) {
                switch (parameter) {
                    case PARAM_COMPLETION_POINT:
                        value = flData.getFieldValue(pgm, m_fieldCompletionPoint);
                        break;
                    case PARAM_INTERNSHIP_PARTICIPANT:
                        value = flData.getFieldValue(pgm, m_fieldInternshipParticipant);
                        break;
                    case PARAM_MOCP:
                        value = flData.getFieldValue(pgm, m_fieldMOCP);
                        break;
                }
            }

            return value;
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);
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

        DataDictionaryField field = translateAliasToDictionaryField(FLScheduleHelper.ALIAS_CTE_PROGRAM_CODE_CRS, true);
        if (getSetupErrors().size() != 0) {
            return;
        }

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                getStudentHelper().getStudentProgramDataset(
                        RetrieveCteFields.DDX_ID,
                        getSurveyPeriod()).getStudentSubQuery());
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLStudentCourseScheduleEntity.class);

        FLStudentHelper helper = getStudentHelper();
        helper.getStudentScheduleCriteria()
                .addNotEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        field.getJavaName(), getBroker().getPersistenceKey());
        helper.getStudentScheduleChangeCriteria()
                .addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        field.getJavaName(), getBroker().getPersistenceKey());

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CUSTOM_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveCteFields.CALC_ID, new RetrieveCteFields());
        calcs.put(RetrieveStudentScheduleInfo.CALC_ID, new RetrieveStudentScheduleInfo(getStudentHelper()));
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        super.addValidators(validators);
    }

}
