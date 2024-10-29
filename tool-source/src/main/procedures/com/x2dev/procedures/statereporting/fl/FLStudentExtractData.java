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

import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * FL Student Demographic Information report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2016-17-student-info-system
 * /student-demographic-info.stml
 *
 * @author Follett Software Company
 */
public class FLStudentExtractData extends FLStateReportData {

    /**
     * The Class FLStudentDemographicEntity.
     */
    public static class FLStudentDemographicEntity extends FLStateReportEntity {
        private FLStudentExtractData m_data;
        private SisStudent m_record;

        /**
         * Instantiates a new FL student demographic entity.
         */
        public FLStudentDemographicEntity() {
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
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() + "] ";
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

            m_data = (FLStudentExtractData) data;
            m_record = (SisStudent) getBean();
            setRowCount(m_data.getStudentHelper().isStudentEligible(m_record) ? 1 : 0);
        }
    }

    /**
     * Field retriever for Agency code.
     */
    protected class RetrieveAgencyCode implements FieldRetriever {
        public static final String CALC_ID = "AGENCY_CODE";

        private static final String ALIAS_AGENCY_CODE = "pgm-agency-code";
        private static final String DDX_ID = "FL-PGM-AGENCY";
        private static final String DEFAULT_CODE = "Z";

        private DataDictionaryField m_fieldAgencyCode;

        /**
         * Instantiates a new retrieve agency code.
         *
         * @throws X2BaseException exception
         */
        public RetrieveAgencyCode() throws X2BaseException {
            StudentProgramDataset pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldAgencyCode =
                    translateAliasToDictionaryField(pgmDataset.getDataDictionary(), ALIAS_AGENCY_CODE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = DEFAULT_CODE;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(entity.getBean().getOid(), DDX_ID, getSurveyPeriod());
            if (pgm != null) {
                value = (String) FLStudentExtractData.this.getFieldValue(pgm, m_fieldAgencyCode);
            }
            return value;
        }
    }

    /**
     * Field retriever for ELL code.
     */
    protected class RetrieveEllCode implements FieldRetriever {
        public static final String CALC_ID = "PGM_ELL";

        private static final String ALIAS_ELL_CODE = "pgm-ell-code";
        private static final String ALIAS_SURVEY_DATE = "pgm-survey-date";
        private static final String DDX_ID = "FL-PGM-ELL";
        private static final String DEFAULT_CODE = "ZZ";

        private static final String PARAM_CODE = "CODE";

        private DataDictionaryField m_fieldEllCode;
        private DataDictionaryField m_fieldSurveyDate;
        private StudentProgramDataset m_pgmDataset;

        /**
         * Instantiates a new retrieve ell code.
         *
         * @throws X2BaseException exception
         */
        public RetrieveEllCode() throws X2BaseException {
            m_pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldEllCode = translateAliasToDictionaryField(m_pgmDataset.getDataDictionary(), ALIAS_ELL_CODE, true);
            m_fieldSurveyDate =
                    translateAliasToDictionaryField(m_pgmDataset.getDataDictionary(), ALIAS_SURVEY_DATE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isCode = PARAM_CODE.equals(field.getParameter().toString());
            Object value = isCode ? DEFAULT_CODE : null;

            List<StudentProgramParticipation> list = m_pgmDataset.getPrograms(entity.getBean().getOid());

            if (list != null && !list.isEmpty()) {
                StudentProgramParticipation pgm = list.get(list.size() - 1);
                if (pgm != null) {
                    value = FLStudentExtractData.this.getFieldValue(pgm, isCode ? m_fieldEllCode : m_fieldSurveyDate);
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for student grade level code.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        public static final String CALC_ID = "GRADE_LEVEL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLStudentDemographicEntity) entity).getCurrentRecord();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.getGradeLevel(((FLStateReportData) data).getSurveyPeriod().getStartDate());
        }
    }

    /**
     * Field retriever for Institution Number
     * Data Element Number: 140575.
     */
    protected class RetrieveInstitutionNumber implements FieldRetriever {
        public static final String CALC_ID = "INSTITUTION_NUMBER";
        private static final String ALIAS_INSTITUTION = "pgm-institution";
        private static final String DDX_ID = "FL-PGM-NEGLECT";
        private static final String DEFAULT_VALUE = "0000";

        private DataDictionaryField m_fieldInstitution;

        /**
         * Instantiates a new retrieve institution number.
         *
         * @throws X2BaseException exception
         */
        public RetrieveInstitutionNumber() throws X2BaseException {
            StudentProgramDataset pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldInstitution =
                    translateAliasToDictionaryField(pgmDataset.getDataDictionary(), ALIAS_INSTITUTION, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = DEFAULT_VALUE;
            SurveyPeriod period = getSurveyPeriod();
            if (period.getCode().equals(FLStateReportData.SURVEY_PERIOD_5) ||
                    period.getCode().equals(FLStateReportData.SURVEY_PERIOD_9)) {
                int idx = Integer.valueOf((String) field.getParameter()).intValue();

                List<StudentProgramParticipation> pgms =
                        getStudentHelper().getStudentPrograms(entity.getBean().getOid(), DDX_ID, getSurveyPeriod());
                if (pgms != null && pgms.size() > idx) {
                    value = (String) FLStudentExtractData.this.getFieldValue(pgms.get(pgms.size() - idx - 1),
                            m_fieldInstitution);
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Lunch Status.
     */
    protected class RetrieveLunchStatus implements FieldRetriever {
        public static final String CALC_ID = "PGM_LUNCH_STATUS";

        private static final String ALIAS_LUNCH_STATUS = "pgm-lunch-status";
        private static final String DDX_ID = "FL-PGM-LUNCH";
        private static final String DEFAULT_VALUE = "0";
        private DataDictionaryField m_fieldLunchStatus;

        /**
         * Instantiates a new retrieve lunch status.
         *
         * @throws X2BaseException exception
         */
        public RetrieveLunchStatus() throws X2BaseException {
            StudentProgramDataset pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldLunchStatus =
                    translateAliasToDictionaryField(pgmDataset.getDataDictionary(), ALIAS_LUNCH_STATUS, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = DEFAULT_VALUE;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(entity.getBean().getOid(), DDX_ID, getSurveyPeriod());
            if (pgm != null) {
                value = (String) FLStudentExtractData.this.getFieldValue(pgm, m_fieldLunchStatus);
            }
            return value;
        }
    }

    /**
     * Field retriever for migrant extended fields.
     */
    protected class RetrieveMigrant implements FieldRetriever {
        public static final String CALC_ID = "PGM_MIGRANT";

        private static final String ALIAS_QAD = "pgm-qualarrival-date";
        private static final String ALIAS_STATUS = "pgm-migrant-status";
        private static final String DDX_ID = "FL-PGM-MIGRANT";
        private static final String DEFAULT_VALUE = "Z";
        private static final String DEFAULT_ZERO = "00000000";

        private static final String PARAM_QAD = "QAD";

        private DataDictionaryField m_fieldQad;
        private DataDictionaryField m_fieldStatus;

        /**
         * Instantiates a new retrieve migrant.
         *
         * @throws X2BaseException exception
         */
        public RetrieveMigrant() throws X2BaseException {
            StudentProgramDataset pgmDataset = getStudentHelper().getStudentProgramDataset(DDX_ID, getSurveyPeriod());
            m_fieldQad = translateAliasToDictionaryField(pgmDataset.getDataDictionary(), ALIAS_QAD, true);
            m_fieldStatus = translateAliasToDictionaryField(pgmDataset.getDataDictionary(), ALIAS_STATUS, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isQad = PARAM_QAD.equals(field.getParameter().toString());
            Object value = isQad ? DEFAULT_ZERO : DEFAULT_VALUE;

            StudentProgramParticipation pgm =
                    getStudentHelper().getStudentProgram(entity.getBean().getOid(), DDX_ID, getSurveyPeriod());
            if (pgm != null) {
                value = FLStudentExtractData.this.getFieldValue(pgm, isQad ? m_fieldQad : m_fieldStatus);
            }
            return value;
        }
    }

    /**
     * Field retriever for zoned district and school
     * Data Element Number: 115629, 173174.
     *
     * @author Follett Software Company
     */
    protected class RetrieveNumberZoned implements FieldRetriever {
        public static final String CALC_ID = "NUMBER_ZONED";

        private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";
        private static final String ALIAS_DISTRICT_NUMBER_Z = "all-std-ZonedDistrict";
        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
        private static final String ALIAS_SCHOOL_NUMBER_Z = "all-std-ZonedSchool";

        private static final String PARAM_DISTRICT = "DISTRICT";
        private static final String PARAM_SCHOOL = "SCHOOL";

        private DataDictionaryField m_fieldDistrictNumber;
        private DataDictionaryField m_fieldDistrictNumberZ;
        private DataDictionaryField m_fieldSchoolNumber;
        private DataDictionaryField m_fieldSchoolNumberZ;

        /**
         * Instantiates a new retrieve number zoned.
         */
        public RetrieveNumberZoned() {
            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
            m_fieldDistrictNumberZ = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER_Z, true);
            m_fieldSchoolNumberZ = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER_Z, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String periodCode = ((FLStateReportData) data).getSurveyPeriodCode();
            String value = null;
            //
            // http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-115629.pdf
            // http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-173174.pdf
            //
            // Note: This element should be zero filled for survey periods 1, 4, 5, 7 and 9.
            //
            if (!SURVEY_PERIOD_1.equals(periodCode) && !SURVEY_PERIOD_4.equals(periodCode) &&
                    !SURVEY_PERIOD_5.equals(periodCode) && !SURVEY_PERIOD_7.equals(periodCode) &&
                    !SURVEY_PERIOD_9.equals(periodCode)) {
                FLStudentExtractData flData = (FLStudentExtractData) data;
                SisStudent student = ((FLStudentDemographicEntity) entity).getCurrentRecord();
                if (PARAM_DISTRICT.equals(field.getParameter().toString())) {
                    value = (String) flData.getFieldValue(student, m_fieldDistrictNumberZ);
                    if (StringUtils.isEmpty(value)) {
                        value = (String) flData.getFieldValue(student.getOrganization1(), m_fieldDistrictNumber);
                    }
                } else if (PARAM_SCHOOL.equals(field.getParameter().toString())) {
                    value = (String) flData.getFieldValue(student, m_fieldSchoolNumberZ);
                    if (StringUtils.isEmpty(value)) {
                        StudentInfo info = getStudentHelper().getStudentInfo(student);
                        SisSchool school = info.getSchool(getSurveyPeriod().getSnapshotDate());
                        if (school != null) {
                            value = (String) FLStudentExtractData.this.getFieldValue(school, m_fieldSchoolNumber);
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for race.
     */
    protected class RetrieveRace implements FieldRetriever {
        public static final String CALC_ID = "RACE";

        private static final String VALUE_NO = "N";
        private static final String VALUE_YES = "Y";

        private Map<String, String> m_raceStateCodes = new HashMap();

        /**
         * Instantiates a new retrieve race.
         */
        public RetrieveRace() {
            super();

            DataDictionaryField field =
                    getDataDictionary().findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> codes = getReferenceCodes(field.getReferenceTableOid());
                for (ReferenceCode item : codes.values()) {
                    m_raceStateCodes.put(item.getCode(), item.getStateCode());
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLStudentDemographicEntity) entity).getCurrentRecord();
            String value = VALUE_NO;
            Collection<Race> races = getStudentHelper().getStudentRaceMap().get(student.getPersonOid());
            String raceCode = (String) field.getParameter();
            if (races != null) {
                for (Race race : races) {
                    if (raceCode.equalsIgnoreCase(m_raceStateCodes.get(race.getRaceCode()))) {
                        value = VALUE_YES;
                        break;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for student resident status.
     *
     * @author Follett Software Company
     */
    protected class RetrieveResidentStatus implements FieldRetriever {
        public static final String CALC_ID = "RESIDENT_STATUS";

        private static final String ALIAS_RESIDENT_STATUS = "all-enr-ResidentStatus";
        private static final String DEFAULT_STATUS = "3";

        private DataDictionaryField m_fieldResidentStatus;

        /**
         * Instantiates a new retrieve resident status.
         */
        public RetrieveResidentStatus() {
            m_fieldResidentStatus = translateAliasToDictionaryField(ALIAS_RESIDENT_STATUS, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLStudentDemographicEntity) entity).getCurrentRecord();
            FLStudentExtractData flData = (FLStudentExtractData) data;
            StudentEnrollment enrollment = flData.getStudentHelper().getEnrollmentForDate(student.getOid(),
                    flData.getSurveyPeriod().getSnapshotDate(), StudentEnrollment.ENTRY);
            String value = null;
            if (enrollment != null) {
                value = (String) flData.getFieldValue(enrollment, m_fieldResidentStatus);
            }
            return StringUtils.isEmpty(value) ? DEFAULT_STATUS : value;
        }
    }

    /**
     * Retriever for student legal name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveStudentName implements FieldRetriever {
        public static final String CALC_ID = "STD_NAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLStudentDemographicEntity) entity).getCurrentRecord();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.formatStudentLegalName();
        }
    }

    /**
     * Gets the export student criteria.
     *
     * @param data FLStateReportData
     * @return FL student helper
     */
    public static FLStudentHelper getExportStudentHelper(FLStateReportData data) {
        FLStudentHelper studentHelper = null;
        try {
            studentHelper = new FLStudentHelper(data);
            studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            studentHelper
                    .setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, data.getCurrentContext().getStartDate());
            studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, data.getCurrentContext().getEndDate());
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
        return studentHelper;
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

        FLStudentHelper studentHelper = getExportStudentHelper(this);
        setQuery(studentHelper.getStudentQuery(false));
        setEntityClass(FLStudentDemographicEntity.class);

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
        calcs.put(RetrieveAgencyCode.CALC_ID, new RetrieveAgencyCode());
        calcs.put(RetrieveEllCode.CALC_ID, new RetrieveEllCode());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
        calcs.put(RetrieveInstitutionNumber.CALC_ID, new RetrieveInstitutionNumber());
        calcs.put(RetrieveLunchStatus.CALC_ID, new RetrieveLunchStatus());
        calcs.put(RetrieveNumberZoned.CALC_ID, new RetrieveNumberZoned());
        calcs.put(RetrieveMigrant.CALC_ID, new RetrieveMigrant());
        calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
        calcs.put(RetrieveResidentStatus.CALC_ID, new RetrieveResidentStatus());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveStudentName.CALC_ID, new RetrieveStudentName());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
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
