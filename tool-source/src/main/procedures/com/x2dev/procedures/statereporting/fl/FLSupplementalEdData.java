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
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * FL Title I Supplemental Educational Services report.
 */
public class FLSupplementalEdData extends FLStateReportData {

    /**
     * The Class FLSupplementalEdEntity.
     */
    public static class FLSupplementalEdEntity extends FLStateReportEntity {
        private FLSupplementalEdData m_data;
        private StudentProgramParticipation m_pgm;
        private SisStudent m_record;

        /**
         * Instantiates a new FL supplemental ed entity.
         */
        public FLSupplementalEdEntity() {
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
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
            return name;
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

            m_data = (FLSupplementalEdData) data;
            m_record = (SisStudent) getBean();
            m_pgm = m_data.getStudentHelper().getStudentProgram(m_record.getOid(), DDX_ID, m_data.getSurveyPeriod());
            setRowCount(m_data.getStudentHelper().isStudentEligible(m_record) ? 1 : 0);
        }
    }

    /**
     * Field retriever for Supplemental Educational Services fields.
     */
    protected class RetrieveSupplementalEducationalServicesFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_SES";

        private static final String ALIAS_MATH = "pgm-ses-math";
        private static final String ALIAS_READ_LANG_ARTS = "pgm-ses-read-lang-art";
        private static final String ALIAS_SCIENCE = "pgm-ses-science";
        private static final String ALIAS_SES_SCHOOL = "pgm-ses-school";
        private static final String ALIAS_SRV_DATE = "pgm-ses-services-date";
        private static final String ALIAS_SRV_PROVIDER = "pgm-ses-service-provider";
        private static final String ALIAS_TERM = "pgm-ses-term";

        private static final String PARAM_MATH = "HOC_MATH";
        private static final String PARAM_READ_LANG_ARTS = "HOC_READ_LANG_ARTS";
        private static final String PARAM_SCIENCE = "HOC_SCIENCE";
        private static final String PARAM_SES_SCHOOL = "SES_SCHOOL";
        private static final String PARAM_SRV_DATE = "SRV_DATE";
        private static final String PARAM_SRV_PROVIDER = "SRV_PROVIDER";
        private static final String PARAM_TERM = "TERM";

        private DataDictionaryField m_fieldMath;
        private DataDictionaryField m_fieldReadLangArts;
        private DataDictionaryField m_fieldScience;
        private DataDictionaryField m_fieldServiceDate;
        private DataDictionaryField m_fieldServiceProvider;
        private DataDictionaryField m_fieldSesSchool;
        private DataDictionaryField m_fieldSesTerm;

        /**
         * Instantiates a new retrieve supplemental educational services fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveSupplementalEducationalServicesFields() throws X2BaseException {
            StudentProgramDataset pgmSESDataset =
                    getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            DataDictionary dataDict = pgmSESDataset.getDataDictionary();
            m_fieldServiceProvider =
                    translateAliasToDictionaryField(dataDict, ALIAS_SRV_PROVIDER, true);
            m_fieldServiceDate =
                    translateAliasToDictionaryField(dataDict, ALIAS_SRV_DATE, true);
            m_fieldSesSchool =
                    translateAliasToDictionaryField(dataDict, ALIAS_SES_SCHOOL, true);
            m_fieldScience =
                    translateAliasToDictionaryField(dataDict, ALIAS_SCIENCE, true);
            m_fieldMath = translateAliasToDictionaryField(dataDict, ALIAS_MATH, true);
            m_fieldReadLangArts =
                    translateAliasToDictionaryField(dataDict, ALIAS_READ_LANG_ARTS, true);
            m_fieldSesTerm = translateAliasToDictionaryField(dataDict, ALIAS_TERM, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLSupplementalEdEntity flEntity = (FLSupplementalEdEntity) entity;
            FLSupplementalEdData flData = (FLSupplementalEdData) data;

            String parameter = (String) field.getParameter();
            StudentProgramParticipation pgm = flEntity.getStudentProgram();
            Object value = null;

            if (pgm != null) {
                switch (parameter) {
                    case PARAM_SRV_PROVIDER:
                        value = flData.getFieldValue(pgm, m_fieldServiceProvider);
                        break;
                    case PARAM_SRV_DATE:
                        value = flData.getFieldValue(pgm, m_fieldServiceDate);
                        break;
                    case PARAM_SES_SCHOOL:
                        value = flData.getFieldValue(pgm, m_fieldSesSchool);
                        break;
                    case PARAM_SCIENCE:
                        value = flData.getFieldValue(pgm, m_fieldScience);
                        break;
                    case PARAM_MATH:
                        value = flData.getFieldValue(pgm, m_fieldMath);
                        break;
                    case PARAM_READ_LANG_ARTS:
                        value = flData.getFieldValue(pgm, m_fieldReadLangArts);
                        break;
                    case PARAM_TERM:
                        try {
                            value = flData.getFieldValue(pgm, m_fieldSesTerm);
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                        break;
                }

            }

            return value;
        }
    }

    protected static final List<String> SES_SURVEY_PERIOD_VALID_CODES = Arrays.asList(FLStateReportData.SURVEY_PERIOD_3,
            FLStateReportData.SURVEY_PERIOD_5, FLStateReportData.SURVEY_PERIOD_9);
    /**
     * Instance variables.
     */

    protected FLScheduleHelper m_scheduleHelper;
    private static final String DDX_ID = "FL-PGM-SES";

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
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

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID,
                        getStudentProgramsCriteria()));

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                getStudentHelper().getStudentProgramDataset(DDX_ID,
                        getSurveyPeriod()).getStudentSubQuery());

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLSupplementalEdEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return SES_SURVEY_PERIOD_VALID_CODES;
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
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */

    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveSupplementalEducationalServicesFields.CALC_ID,
                new RetrieveSupplementalEducationalServicesFields());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */

    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
