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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class FLCompensatoryProjectEvaluationData.
 */
public class FLCompensatoryProjectEvaluationData extends FLStateReportData {

    /**
     * The Class FLCompensatoryProjectEvaluationEntity.
     */
    public static class FLCompensatoryProjectEvaluationEntity extends FLStateReportEntity {
        private List<MigrantProgramInfo> m_programInfos;

        /**
         * Instantiates a new FL compensatory project evaluation entity.
         */
        public FLCompensatoryProjectEvaluationEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    "] ";
            return name;
        }

        /**
         * Gets the program info.
         *
         * @return the program info
         */
        public MigrantProgramInfo getProgramInfo() {
            return m_programInfos.get(getCurrentRow());
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

            m_programInfos = null;

            List<StudentProgramParticipation> list =
                    ((FLCompensatoryProjectEvaluationData) data).m_pgmDataset.getPrograms(bean.getOid());
            if (list != null && !list.isEmpty()) {
                m_programInfos = new ArrayList(list.size());
                for (StudentProgramParticipation pgm : list) {
                    m_programInfos.add(((FLCompensatoryProjectEvaluationData) data).new MigrantProgramInfo(pgm));
                }
            }

            if (m_programInfos != null) {
                setRowCount(m_programInfos.size());
            } else {
                setRowCount(0);
            }
        }
    }

    /**
     * The Class MigrantProgramInfo.
     */
    protected class MigrantProgramInfo {
        private StudentProgramParticipation m_program;

        /**
         * Instantiates a new migrant program info.
         *
         * @param program the program
         */
        public MigrantProgramInfo(StudentProgramParticipation program) {
            m_program = program;
        }


        /**
         * Gets the program.
         *
         * @return the program
         */
        public StudentProgramParticipation getProgram() {
            return m_program;
        }
    }

    /**
     * Field retriever for MIGRANT Fields.
     */
    protected class RetrieveMigrantFields implements FieldRetriever {
        public static final String CALC_ID = "PGM_MIGRANT";
        public static final String DDX_ID = "FL-PGM-MIGRANT";

        private static final String ALIAS_CONTINUATION = "pgm-continuation";
        private static final String ALIAS_MODEL = "pgm-model";
        private static final String ALIAS_PRIORITY = "pgm-priority";
        private static final String ALIAS_PROJECT_TYPE = "pgm-project-type";
        private static final String ALIAS_REFERRED_SERVICES = "pgm-referred-services";
        private static final String ALIAS_SERVICE_PROVIDER = "pgm-service-provider";
        private static final String ALIAS_STD_SRV_CODE = "pgm-std-services-code";
        private static final String ALIAS_SUBJECT_AREA = "pgm-subject-area";
        private static final String ALIAS_SUPPORT_SERVICES = "pgm-support-services";
        private static final String ALIAS_TERM = "pgm-term-code";

        private static final String PARAM_CONTINUATION = "CONTINUATION";
        private static final String PARAM_MODEL = "MODEL";
        private static final String PARAM_PRIORITY = "PRIORITY";
        private static final String PARAM_PROJECT_TYPE = "PROJECT TYPE";
        private static final String PARAM_REFERRED_SERVICES = "REFERRED SERVICES";
        private static final String PARAM_SERVICE_PROVIDER = "SERVICE PROVIDER";
        private static final String PARAM_STD_SRV_CODE = "STD SRV CODE ";
        private static final String PARAM_SUBJECT_AREA = "SUBJECT AREA";
        private static final String PARAM_SUPPORT_SERVICES = "SUPPORT SERVICES";
        private static final String PARAM_TERM = "TERM";

        private DataDictionaryField m_fieldContinuation;
        private DataDictionaryField m_fieldModel;
        private DataDictionaryField m_fieldPriority;
        private DataDictionaryField m_fieldProjectType;
        private DataDictionaryField m_fieldReferredServices;
        private DataDictionaryField m_fieldServiceProvider;
        private DataDictionaryField m_fieldStdServicesCode;
        private DataDictionaryField m_fieldSubjectArea;
        private DataDictionaryField m_fieldSupportServices;
        private DataDictionaryField m_fieldTerm;

        /**
         * Instantiates a new retrieve migrant fields.
         *
         * @throws X2BaseException the x 2 base exception
         */
        public RetrieveMigrantFields() throws X2BaseException {
            StudentProgramDataset pgmCteDataset = getStudentHelper().getStudentProgramDataset(DDX_ID,
                    getSurveyPeriod());
            DataDictionary pgmCteDataDictionary = pgmCteDataset.getDataDictionary();

            m_fieldSupportServices = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_SUPPORT_SERVICES, true);
            m_fieldReferredServices = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_REFERRED_SERVICES, true);
            m_fieldPriority = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_PRIORITY, true);
            m_fieldStdServicesCode = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_STD_SRV_CODE, true);
            m_fieldContinuation = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_CONTINUATION, true);
            m_fieldProjectType = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_PROJECT_TYPE, true);
            m_fieldSubjectArea = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_SUBJECT_AREA, true);
            m_fieldModel = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_MODEL, true);
            m_fieldTerm = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_TERM, true);
            m_fieldServiceProvider = translateAliasToDictionaryField(pgmCteDataDictionary,
                    ALIAS_SERVICE_PROVIDER, true);
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

            FLCompensatoryProjectEvaluationData flData = (FLCompensatoryProjectEvaluationData) data;
            FLCompensatoryProjectEvaluationEntity flEntity = (FLCompensatoryProjectEvaluationEntity) entity;
            String parameter = (String) field.getParameter();
            MigrantProgramInfo info = flEntity.getProgramInfo();
            StudentProgramParticipation pgm = info.getProgram();

            Object value = null;

            DataDictionaryField multiValueField = null;

            if (parameter.contains(PARAM_STD_SRV_CODE)) {
                multiValueField = m_fieldStdServicesCode;
            } else if (parameter.contains(PARAM_SUBJECT_AREA)) {
                multiValueField = m_fieldSubjectArea;
            }

            if (multiValueField != null) {
                value = "Z";
                String values = (String) flData.getFieldValue(pgm, multiValueField);
                if (!StringUtils.isEmpty(values)) {
                    List<String> codes = Arrays.asList((values).split(","));
                    String codeMatch = parameter.substring(parameter.length() - 1).toUpperCase();
                    if (codes.contains(codeMatch)) {
                        value = codeMatch;
                    }
                }
            } else {
                switch (parameter) {
                    case PARAM_PROJECT_TYPE:
                        value = flData.getFieldValue(pgm, m_fieldProjectType);
                        break;
                    case PARAM_MODEL:
                        value = flData.getFieldValue(pgm, m_fieldModel);
                        break;
                    case PARAM_SUPPORT_SERVICES:
                        String values = (String) flData.getFieldValue(pgm, m_fieldSupportServices);
                        StringBuilder outputService = new StringBuilder();
                        if (!StringUtils.isEmpty(values)) {
                            List<String> codes = Arrays.asList((values).split(","));
                            for (String code : codes) {
                                outputService.append(code);
                            }
                        }
                        while (outputService.length() < 4) {
                            outputService.append("Z");
                        }
                        value = outputService.toString();
                        break;
                    case PARAM_REFERRED_SERVICES:
                        value = flData.getFieldValue(pgm, m_fieldReferredServices);
                        break;
                    case PARAM_PRIORITY:
                        value = flData.getFieldValue(pgm, m_fieldPriority);
                        break;
                    case PARAM_CONTINUATION:
                        value = flData.getFieldValue(pgm, m_fieldContinuation);
                        break;
                    case PARAM_TERM:
                        value = flData.getFieldValue(pgm, m_fieldTerm);
                        break;
                    case PARAM_SERVICE_PROVIDER:
                        value = flData.getFieldValue(pgm, m_fieldServiceProvider);
                        break;
                }
            }

            return value;
        }
    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);

    protected StudentProgramDataset m_pgmDataset;

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

        m_pgmDataset = getStudentHelper().getStudentProgramDataset(
                RetrieveMigrantFields.DDX_ID,
                getSurveyPeriod());

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                m_pgmDataset.getStudentSubQuery());
        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLCompensatoryProjectEvaluationEntity.class);

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
     * @throws X2BaseException the x 2 base exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveMigrantFields.CALC_ID, new RetrieveMigrantFields());
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
