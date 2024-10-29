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

import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_ADDITIONAL_HOURS_CREDITED;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_CTE_AGE_PROGRAM_CODE;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_INDUSTRY_CERTIFICATION_OUTCOME;
import static com.x2dev.procedures.statereporting.fl.FLWdisExportConfiguration.FIELD_POSTSECONDARY_SCHOOL_OF_ENROLLMENT;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
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
import com.x2dev.procedures.statereporting.fl.FLWdisSupplementalInfoData.FLSupEntity.SupRecord;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * The Class FLWdisSupplementalInfoData.
 */
public class FLWdisSupplementalInfoData extends FLStateReportData {


    /**
     * The Class FLSupEntity.
     */
    public static class FLSupEntity extends FLStateReportEntity implements WdisEntity {


        /**
         * The Class SupRecord.
         */
        public static class SupRecord {
            private String m_additionalHrs;
            private String m_indCertId;
            private String m_indCertOut;
            private StudentScheduleInfo m_schedule;


            /**
             * Instantiates a new sup record.
             *
             * @param info StudentScheduleInfo
             * @param addHrs String
             * @param indCertId String
             * @param indCertOut String
             */
            public SupRecord(StudentScheduleInfo info, String addHrs, String indCertId, String indCertOut) {
                m_schedule = info;
                m_additionalHrs = addHrs;
                m_indCertOut = indCertOut;
                m_indCertId = indCertId;
            }


            /**
             * Gets the additional hrs.
             *
             * @return String
             */
            private String getAdditionalHrs() {
                return m_additionalHrs;
            }


            /**
             * Gets the course view.
             *
             * @return String
             */
            private String getCourseView() {
                return m_schedule.getSection().getCourseView();
            }


            /**
             * Gets the ind cert id.
             *
             * @return String
             */
            private String getIndCertId() {
                return m_indCertId;
            }


            /**
             * Gets the ind cert out.
             *
             * @return String
             */
            private String getIndCertOut() {
                return m_indCertOut;
            }


            /**
             * Gets the post sec school of enr.
             *
             * @return String
             * @throws X2BaseException exception
             */
            private String getPostSecSchoolOfEnr() throws X2BaseException {
                return m_schedule.getPostSecondarySchoolOfEnrollment();
            }
        }

        private FLWdisSupplementalInfoData m_data;
        private SisStudent m_student;

        private List<SupRecord> m_records;


        /**
         * Instantiates a new FL sup entity.
         */
        public FLSupEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SupRecord record = getRecord();
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", " + record.getCourseView() +
                    "] ";
            return name;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
         */
        @Override
        public StudentProgramParticipation getProgram() {
            StudentProgramParticipation pgm = null;

            pgm = getProgram(m_data.m_pgmDatasetAge);

            if (pgm == null) {
                pgm = getProgram(m_data.m_pgmDatasetCte);
            }

            return pgm;
        }


        /**
         * Gets the record.
         *
         * @return Sup record
         */
        public SupRecord getRecord() {
            return m_records.get(getCurrentRow());
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLWdisSupplementalInfoData) data;
            m_student = (SisStudent) getBean();

            m_records = new ArrayList<>();
            if (getProgram() != null) {
                List<StudentScheduleInfo> infos = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_student);
                List<StudentProgramParticipation> programs = m_data.m_pgmDatasetCape.getPrograms(bean.getOid());
                for (StudentScheduleInfo info : infos) {
                    if (programs != null) {
                        for (StudentProgramParticipation program : programs) {
                            SupRecord record = new SupRecord(info, null,
                                    (String) m_data.getFieldValue(program, m_data.m_fieldCapeIndustryCertId),
                                    (String) m_data.getFieldValue(program, m_data.m_fieldCapeIndustryCert));
                            m_records.add(record);
                        }
                    }
                    String addHrs = (String) m_data.getFieldValue(m_student, m_data.m_fieldStdAddHrsCredited);
                    if (!StringUtils.isEmpty(addHrs)) {
                        addHrs = StringUtils.padLeft(addHrs, 6, '0');
                        SupRecord record = new SupRecord(info, addHrs, null, null);
                        m_records.add(record);
                    }
                }
            }

            setRowCount(m_records.size());
        }


        /**
         * Gets the program.
         *
         * @param dataset StudentProgramDataset
         * @return Student program participation
         */
        private StudentProgramParticipation getProgram(StudentProgramDataset dataset) {
            StudentProgramParticipation pgm = null;

            List<StudentProgramParticipation> pgms = dataset.getPrograms(m_student.getOid());
            if (pgms != null && pgms.size() > 0) {
                pgm = pgms.get(pgms.size() - 1);
            }
            return pgm;
        }
    }


    /**
     * The Class RetrieveSupInfo.
     */
    class RetrieveSupInfo implements FieldRetriever {
        public static final String CALC_ID = "SUP_INFO";

        private Map<String, DataDictionaryField> m_pgmCodesDdxFields = new HashMap<>();


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLSupEntity flEntity = (FLSupEntity) entity;
            FLStateReportData flData = (FLStateReportData) data;
            SupRecord record = flEntity.getRecord();
            Object value = null;
            String param = (String) field.getParameter();
            switch (param) {
                case FIELD_POSTSECONDARY_SCHOOL_OF_ENROLLMENT:
                    value = record.getPostSecSchoolOfEnr();
                    break;
                case FIELD_ADDITIONAL_HOURS_CREDITED:
                    value = record.getAdditionalHrs();
                    break;
                case FIELD_CTE_AGE_PROGRAM_CODE:
                    String alias = ALIAS_WDIS_PROGRAM_CODE.replace(PATTERN_TO_REPLACE,
                            flEntity.getProgram().getProgramCode().toLowerCase());
                    String ddxOid = flEntity.getProgram().getExtendedDataDictionaryOid();
                    DataDictionaryField fieldPgmCode = m_pgmCodesDdxFields.get(ddxOid);
                    if (fieldPgmCode == null) {
                        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                                flEntity.getProgram().getExtendedDataDictionary(),
                                data.getBroker().getPersistenceKey());
                        fieldPgmCode = flData.translateAliasToDictionaryField(dictionary, alias, true);
                        m_pgmCodesDdxFields.put(ddxOid, fieldPgmCode);
                    }
                    value = flData.getFieldValue(flEntity.getProgram(), fieldPgmCode);
                    break;
                case FIELD_INDUSTRY_CERTIFICATION_IDENTIFIER:
                    value = record.getIndCertId();
                    break;
                case FIELD_INDUSTRY_CERTIFICATION_OUTCOME:
                    value = record.getIndCertOut();
                    break;
                default:
                    break;
            }
            return value;
        }

    }

    protected static final String DDX_ID_AGE = "FL-PGM-AGE";
    protected static final String DDX_ID_CAPE = "FL-PGM-CAPE";
    protected static final String DDX_ID_CTE = "FL-PGM-CTE";

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_W,
                    FLStateReportData.SURVEY_PERIOD_S);

    private static final String ALIAS_CAPE_INDUSTRY_CERTIFICATION = "pgm-cape-industry-cert";
    private static final String ALIAS_CAPE_INDUSTRY_CERTIFICATION_ID = "pgm-cape-industry-cert-id";
    private static final String ALIAS_WDIS_PROGRAM_CODE = "pgm-%TOREPLACE%-program-id";
    private static final String ALIAS_STD_ADDIT_HRS_CREDITED = "all-std-WdisAdditionalHrsCredited";

    private static final String PATTERN_TO_REPLACE = "%TOREPLACE%";

    private DataDictionaryField m_fieldCapeIndustryCert;
    private DataDictionaryField m_fieldCapeIndustryCertId;
    private DataDictionaryField m_fieldStdAddHrsCredited;

    protected StudentProgramDataset m_pgmDatasetAge;
    protected StudentProgramDataset m_pgmDatasetCte;

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentScheduleHelper m_studentScheduleHelper;

    private StudentProgramDataset m_pgmDatasetCape;


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
        getStudentHelper().setWdisMode(WDIS_MODE_BOTH);
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLSupEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        m_pgmDatasetCape = getStudentHelper().getStudentProgramDataset(DDX_ID_CAPE, getSurveyPeriod());

        m_fieldCapeIndustryCertId =
                translateAliasToDictionaryField(m_pgmDatasetCape.getDataDictionary(),
                        ALIAS_CAPE_INDUSTRY_CERTIFICATION_ID,
                        true);
        m_fieldCapeIndustryCert =
                translateAliasToDictionaryField(m_pgmDatasetCape.getDataDictionary(), ALIAS_CAPE_INDUSTRY_CERTIFICATION,
                        true);

        m_fieldStdAddHrsCredited = translateAliasToDictionaryField(ALIAS_STD_ADDIT_HRS_CREDITED, true);

        m_pgmDatasetAge = getStudentHelper().getStudentProgramDataset(DDX_ID_AGE, getSurveyPeriod());
        m_pgmDatasetCte = getStudentHelper().getStudentProgramDataset(DDX_ID_CTE, getSurveyPeriod());

        registerFieldRetrievers();
        registerFieldValidators();
    }


    /**
     * Register field retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveSupInfo.CALC_ID, new RetrieveSupInfo());
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
