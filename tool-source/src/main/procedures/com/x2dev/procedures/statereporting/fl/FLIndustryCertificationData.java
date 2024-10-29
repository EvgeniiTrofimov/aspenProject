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
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.fl.FLCTEStudentCourseScheduleData.RetrieveCteFields;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper.MasterScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * The Class FLIndustryCertificationData.
 */
public class FLIndustryCertificationData extends FLStateReportData {

    /**
     * The Class FLIndustryCertificationEntity.
     */
    public static class FLIndustryCertificationEntity extends FLStateReportEntity {

        private FLIndustryCertificationData m_data;
        private StudentProgramParticipation m_pgm;
        private SisStudent m_record;
        private List<StudentScheduleInfo> m_scheduleInfos;

        /**
         * Instantiates a new FL industry certification entity.
         */
        public FLIndustryCertificationEntity() {
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
            SisStudent student = m_record;
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
            return name;
        }

        /**
         * Gets the schedule info.
         *
         * @return Student schedule info
         */
        public StudentScheduleInfo getScheduleInfo() {
            return m_scheduleInfos.get(getCurrentRow());
        }

        /**
         * Gets the student program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getStudentProgram() {
            return m_pgm;
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

            m_data = (FLIndustryCertificationData) data;
            m_record = (SisStudent) bean;
            m_pgm = m_data.getStudentHelper().getStudentProgram(m_record.getOid(), DDX_ID, m_data.getSurveyPeriod());

            if (m_data.getStudentHelper().isStudentEligible(m_record)) {
                m_scheduleInfos = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_record);
                setRowCount(m_scheduleInfos.size());
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * Field retriever for CAPE fields.
     */
    protected class RetrieveProfessionalAcademyFields implements FieldRetriever {

        public static final String CALC_ID = "PGM_CAPE";

        private static final String ALIAS_CAPE_IDENTIFIER = "pgm-cape-identifier";
        private static final String ALIAS_CAPE_INDUSTRY_CERTIFICATION = "pgm-cape-industry-cert";
        private static final String ALIAS_CAPE_INDUSTRY_CERTIFICATION_ID = "pgm-cape-industry-cert-id";

        private static final String ALIAS_CRS_CTE_PROGRAM = "all-crs-CTEProgramCode";

        private static final String PARAM_ID = "ID";
        private static final String PARAM_INDUSTRY_CERTIFICATION = "INDUSTRY_CERTIFICATION";
        private static final String PARAM_INDUSTRY_CERTIFICATION_ID = "INDUSTRY_CERTIFICATION_ID";
        private static final String PARAM_PROGRAM_CODE = "PROGRAM_CODE";

        private DataDictionaryField m_fieldCapeIdentifier;
        private DataDictionaryField m_fieldCapeIndustryCert;
        private DataDictionaryField m_fieldCapeIndustryCertId;

        /**
         * Instantiates a new retrieve professional academy fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveProfessionalAcademyFields() throws X2BaseException {

            StudentProgramDataset pgmCapeDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmCapeDataset.getDataDictionary();

            m_fieldCapeIdentifier = translateAliasToDictionaryField(dataDict, ALIAS_CAPE_IDENTIFIER, true);
            m_fieldCapeIndustryCertId =
                    translateAliasToDictionaryField(dataDict, ALIAS_CAPE_INDUSTRY_CERTIFICATION_ID, true);
            m_fieldCapeIndustryCert =
                    translateAliasToDictionaryField(dataDict, ALIAS_CAPE_INDUSTRY_CERTIFICATION, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLIndustryCertificationEntity flEntity = (FLIndustryCertificationEntity) entity;
            FLIndustryCertificationData flData = (FLIndustryCertificationData) data;

            String parameter = (String) field.getParameter();
            StudentProgramParticipation currentPgm = flEntity.getStudentProgram();
            Object value = null;

            switch (parameter) {
                case PARAM_PROGRAM_CODE:
                    value = flEntity.getScheduleInfo().getCourse().getFieldValueByAlias(ALIAS_CRS_CTE_PROGRAM);
                    break;
                case PARAM_ID:
                    value = flData.getFieldValue(currentPgm, m_fieldCapeIdentifier);
                    break;
                case PARAM_INDUSTRY_CERTIFICATION:
                    value = flData.getFieldValue(currentPgm, m_fieldCapeIndustryCert);
                    break;
                case PARAM_INDUSTRY_CERTIFICATION_ID:
                    value = flData.getFieldValue(currentPgm, m_fieldCapeIndustryCertId);
                    break;
            }

            return value;
        }
    }

    /**
     * Field retriever for Schedule fields.
     */
    protected class RetrieveStudentScheduleInfo implements FieldRetriever {

        public static final String CALC_ID = "SCHED_INFO";

        private static final String PARAM_COURSE = "COURSE";
        private static final String PARAM_DUAL_ENROLLMENT = "DUAL ENROLLMENT";
        private static final String PARAM_SCHOOL_YEAR = "SCHOOL_YEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLIndustryCertificationEntity flEntity = (FLIndustryCertificationEntity) entity;
            FLIndustryCertificationData flData = (FLIndustryCertificationData) data;
            StudentScheduleInfo info = flEntity.getScheduleInfo();

            Calendar calendar = Calendar.getInstance();

            Object value = null;
            switch (field.getParameter().toString()) {
                case PARAM_COURSE:
                    MasterSchedule masterSchedule = info.getSection();
                    MasterScheduleInfo masterScheduleInfo =
                            flData.m_scheduleHelper.getMasterScheduleInfo(masterSchedule.getOid());
                    value = (masterScheduleInfo != null) ? masterScheduleInfo.getCourseNumber() : "";
                    break;
                case PARAM_DUAL_ENROLLMENT:
                    value = info.getDualEnrollmentType();
                    break;
                case PARAM_SCHOOL_YEAR:
                    calendar.setTime(info.getCourse().getDistrictContext().getStartDate());
                    int start = calendar.get(Calendar.YEAR);
                    calendar.setTime(info.getCourse().getDistrictContext().getEndDate());
                    int end = calendar.get(Calendar.YEAR);
                    value = String.format("%02d%02d", Integer.valueOf(start % 100), Integer.valueOf(end % 100));
                    break;
                default:
                    break;
            }
            return value;
        }

    }

    private static final String ALIAS_CRS_CTE_PROGRAM_CODE = "all-crs-CTEProgramCode";

    /**
     * Instance variables.
     */
    public static final String DDX_ID = "FL-PGM-CAPE";

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    protected static final List<String> CAPEIC_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);

    public StudentScheduleHelper m_studentScheduleHelper;

    protected FLScheduleHelper m_scheduleHelper;

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {

        super.initialize();
        initializeFields();

        Collection<StateReportValidationError> errors = getSetupErrors();
        if (errors.size() != 0) {
            return;
        }

        DataDictionaryField fieldCteProgramCode =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_CTE_PROGRAM_CODE);

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID,
                        getStudentProgramsCriteria()));

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                getStudentHelper().getStudentProgramDataset(
                        RetrieveCteFields.DDX_ID,
                        getSurveyPeriod()).getStudentSubQuery());

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLIndustryCertificationEntity.class);

        getStudentHelper().getStudentScheduleCriteria()
                .addNotEmpty(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        fieldCteProgramCode.getJavaName(), getBroker().getPersistenceKey());
        getStudentHelper().getStudentScheduleChangeCriteria()
                .addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        fieldCteProgramCode.getJavaName(), getBroker().getPersistenceKey());

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CAPEIC_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Gets the student programs criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentProgramsCriteria() {

        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(StudentProgramParticipation.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, DDX_ID);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getSurveyPeriod().getEndDate());
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getSurveyPeriod().getStartDate());
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        return criteria;
    }

    /**
     * Lookup field aliases and paths.
     *
     * @throws X2BaseException exception
     */
    private void initializeFields() throws X2BaseException {
        // TODO: Add initialization which produces setup errors here
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveProfessionalAcademyFields.CALC_ID, new RetrieveProfessionalAcademyFields());
        calcs.put(RetrieveStudentScheduleInfo.CALC_ID, new RetrieveStudentScheduleInfo());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        super.addValidators(validators);
    }
}
