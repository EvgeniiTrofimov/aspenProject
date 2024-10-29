/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;

/**
 * State report data module for the RI Special Education Evaluations census. The base class for
 * this state report is IEP_OTHER_SERVICE (this table is used as storage for the evaluation log).
 * Evaluations performed between the start of the school year and the report date are included in
 * this report. Only students included in the Student Census are included, based on the "In Last
 * State Report" boolean alias on the IEP record. Therefore, it is required that the Student Census
 * be run prior to running this report.
 * <p>
 * The core functionality of this extract is straightforward; information is taken directly from
 * the IEP_OTHER_SERVICE table. An optional feature is provided to create evaluation log entries
 * in the IEP_OTHER_SERVICE table based on the evaluations selected in the Evaluation Consent form,
 * if available. This is useful for districts that have not utilized the evaluation log.
 * <p>
 * If selected based on the "updateEvalLogs" input parameter, Evaluation Consent forms completed
 * during the current school year will be gathered. Each relevant selection in the forms will be
 * converted to an evaluation log entry. The date of the log entry will be based on the date of
 * the "Complete Assessments" workflow phase completion date.
 *
 * @author mmastrangelo
 */
public class RISpecialEducationEvaluations extends StateReportData {
    /**
     * Entity class for student attendance export.
     */
    public static class RISpecialEducationEvaluationsEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        private static final String ALIAS_EVAL_LOG_AREA = "eval-log-area";
        private static final String ALIAS_EVAL_LOG_DATE = "eval-log-date";

        /**
         * Instantiates a new RI special education evaluations entity.
         */
        public RISpecialEducationEvaluationsEntity() {

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            IepOtherService service = (IepOtherService) getBean();
            StringBuilder name = new StringBuilder();
            name.append(service.getIepData().getStudent().getNameView());
            name.append("-[SASID: ");
            name.append(service.getIepData().getStudent().getStateId());
            name.append("]-[AREA: ");
            name.append(StringUtils.unNullify(
                    service.getFieldValueByAlias(ALIAS_EVAL_LOG_AREA, getData().getDataDictionary()).toString()));
            name.append("]-[DATE: ");
            name.append(StringUtils.unNullify(
                    service.getFieldValueByAlias(ALIAS_EVAL_LOG_DATE, getData().getDataDictionary()).toString()));
            name.append("]");
            return name.toString();
        }
    }

    // Input Parameters ---------------------------------------------------------------------------
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String SENIOR_GRAD_DATE = "seniorGraduationDate";
    public static final String UPDATE_EVAL_LOGS = "updateEvalLogs";

    // Field Aliases ------------------------------------------------------------------------------
    private static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";
    private static final String EVAL_LOG_AREA_ALIAS = "eval-log-area";

    private static final String EVAL_FORM_ADAPTED_PE = "EVAL-ADAPT";
    private static final String EVAL_FORM_ASSISTIVE_TECH = "EVAL-AST-TECH";
    private static final String EVAL_FORM_CLINICAL_PSYCH = "EVAL-CLINICAL";
    private static final String EVAL_FORM_EDUCATIONAL = "EVAL-EDUCATIONAL";
    private static final String EVAL_FORM_FUNCTIONAL_BEHAVIORAL = "EVAL-BEHAVIORAL";
    private static final String EVAL_FORM_MEDICAL_GENERAL = "EVAL-MED-GEN";
    private static final String EVAL_FORM_NEUROPSYCHOLOGICAL = "EVAL-NEUROPHYS";
    private static final String EVAL_FORM_OBSERVATION = "EVAL-OBSERVATION";
    private static final String EVAL_FORM_OT = "EVAL-OCCUPATIONAL";
    private static final String EVAL_FORM_PT = "EVAL-PHYSICAL";
    private static final String EVAL_FORM_PSYCHIATRIC = "EVAL-PSYCHIATRIC";
    private static final String EVAL_FORM_PSYCHOLOGICAL = "EVAL-PSYCHOLOGICAL";
    private static final String EVAL_FORM_SOCIAL_HISTORY = "EVAL-SOCIAL";
    private static final String EVAL_FORM_SPEECH = "EVAL-SPEECH";
    private static final String EVAL_FORM_VOCATIONAL = "EVAL-VOCATIONAL";
    private static final String EVAL_FORM_OUTSIDE_EVAL = "EVAL-OUTSIDE-DESC";
    private static final String EVAL_FORM_OTHER_EVAL = "EVAL-OTHER-DESC";

    private static final String CONSENT_FORM_DEFINITION_ID = "SPED-RI-PERM-EVAL";
    private static final String COMPLETE_ASSESSMENTS_PHASE_NAME = "Complete Assessments";

    private static final String ENR_TYPE_GRADUATE = "Graduate";
    private static final String OTHER_SERVICE_EVALUATION_TYPE = "Evaluation";

    protected DistrictSchoolYearContext m_currentSchoolYear;
    protected PlainDate m_reportDate;
    protected PlainDate m_senior_grade_date = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_senior_grade_date = (PlainDate) getParameter(SENIOR_GRAD_DATE);
        loadSchoolYear();

        X2Criteria reportCriteria = getReportCriteria();
        setQuery(new BeanQuery(IepOtherService.class, reportCriteria));

        if (Boolean.TRUE.equals(getParameter(UPDATE_EVAL_LOGS))) {
            updateEvaluationLogFromConsentForm(reportCriteria);
        }
        setEntityClass(RISpecialEducationEvaluationsEntity.class);
    }

    /**
     * Load school year.
     */
    protected void loadSchoolYear() {
        Criteria schoolYearCriteria = new Criteria();
        schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);

        BeanQuery schoolYearQuery = new BeanQuery(DistrictSchoolYearContext.class, schoolYearCriteria);

        m_currentSchoolYear = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
    }

    /**
     * Gets the report criteria.
     *
     * @return X 2 criteria
     */
    protected X2Criteria getReportCriteria() {
        Criteria subqueryCriteria = new Criteria();
        subqueryCriteria.addEqualTo(translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);

        SubQuery subQuery = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, subqueryCriteria);

        X2Criteria reportCriteria = new X2Criteria();
        reportCriteria.addIn(IepOtherService.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID, subQuery);
        // reportCriteria.addGreaterOrEqualThan(IepOtherService.COL_START_DATE,
        // m_currentSchoolYear.getStartDate());
        reportCriteria.addLessOrEqualThan(IepOtherService.COL_START_DATE, m_reportDate);
        if (m_senior_grade_date != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, ENR_TYPE_GRADUATE);
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_DATE, m_senior_grade_date);
            reportCriteria.addIn(IepOtherService.COL_STUDENT_OID,
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria));
        }
        return reportCriteria;
    }

    /**
     * Gets the consent form state code mapping.
     *
     * @return Map
     */
    private Map<String, String> getConsentFormStateCodeMapping() {
        HashMap<String, String> stateCodes = new HashMap<String, String>();

        stateCodes.put(EVAL_FORM_ADAPTED_PE, "G");
        stateCodes.put(EVAL_FORM_ASSISTIVE_TECH, "T");
        stateCodes.put(EVAL_FORM_CLINICAL_PSYCH, "H");
        stateCodes.put(EVAL_FORM_EDUCATIONAL, "D");
        stateCodes.put(EVAL_FORM_FUNCTIONAL_BEHAVIORAL, "L");
        stateCodes.put(EVAL_FORM_MEDICAL_GENERAL, "A");
        stateCodes.put(EVAL_FORM_NEUROPSYCHOLOGICAL, "W");
        stateCodes.put(EVAL_FORM_OBSERVATION, "Z");
        stateCodes.put(EVAL_FORM_OT, "O");
        stateCodes.put(EVAL_FORM_PT, "P");
        stateCodes.put(EVAL_FORM_PSYCHIATRIC, "X");
        stateCodes.put(EVAL_FORM_PSYCHOLOGICAL, "B");
        stateCodes.put(EVAL_FORM_SOCIAL_HISTORY, "E");
        stateCodes.put(EVAL_FORM_SPEECH, "C");
        stateCodes.put(EVAL_FORM_VOCATIONAL, "V");

        return stateCodes;
    }

    /**
     * Update evaluation log from consent form.
     *
     * @param reportCriteria Criteria
     */
    private void updateEvaluationLogFromConsentForm(Criteria reportCriteria) {
        FormDefinition formDefinition = FormDefinition.getById(CONSENT_FORM_DEFINITION_ID, getBroker());

        if (formDefinition != null) {
            Map<String, String> consentFormStateCodeMapping = getConsentFormStateCodeMapping();

            /*
             * Load a map of form instances; the instance contains the date the form was created
             */
            Criteria instanceCriteria = new Criteria();
            instanceCriteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, formDefinition.getOid());
            instanceCriteria.addGreaterOrEqualThan(FormInstance.COL_CREATED_TIME,
                    Long.valueOf(m_currentSchoolYear.getStartDate().getTime()));
            instanceCriteria.addLessOrEqualThan(FormInstance.COL_CREATED_TIME,
                    Long.valueOf(m_currentSchoolYear.getEndDate().getTime()));

            BeanQuery instanceQuery = new BeanQuery(FormInstance.class, instanceCriteria);

            Map<String, FormInstance> instances =
                    getBroker().getMapByQuery(instanceQuery, FormInstance.COL_STORAGE_OBJECT_OID, 1024);

            /*
             * Load a map of progress entries for the "Complete Assessments" phase. We will try to
             * use the date the phase was completed as the date the assessment was performed
             */
            DataDictionaryTable iepTable = getDataDictionary().findDataDictionaryTableByClass(IepData.class.getName());

            Criteria workflowProgressCriteria = new Criteria();
            workflowProgressCriteria.addEqualTo(WorkflowProgress.REL_WORKFLOW_PHASE + "." +
                    WorkflowPhase.COL_NAME, COMPLETE_ASSESSMENTS_PHASE_NAME);
            workflowProgressCriteria.addEqualTo(WorkflowProgress.REL_WORKFLOW + "." +
                    Workflow.REL_WORKFLOW_DEFINITION + "." +
                    WorkflowDefinition.COL_OWNER_DATA_TABLE_CONFIG_OID, iepTable.getDataTableConfig().getOid());

            BeanQuery workflowProgressQuery = new BeanQuery(WorkflowProgress.class, workflowProgressCriteria);

            Map<String, WorkflowProgress> progressEntries =
                    getBroker().getMapByQuery(workflowProgressQuery,
                            WorkflowProgress.REL_WORKFLOW + "." + Workflow.COL_OWNER_OID, 1024);

            /*
             * Load a lookup set of IEP OIDs that have evaluation log entries. We will skip these
             * IEPs.
             */
            HashSet<String> iepsWithEvalLogs = new HashSet<String>(1024);
            iepsWithEvalLogs.addAll(getBroker().getSubQueryCollectionByQuery(
                    new SubQuery(IepOtherService.class, IepOtherService.COL_IEP_DATA_OID, reportCriteria)));

            /*
             * Get a data dictionary object to use for retrieving values from the form
             */
            DataDictionary formDictionary = DataDictionary.getDistrictDictionary(
                    formDefinition.getExtendedDataDictionary(), formDefinition.getPersistenceKey());

            /*
             * Now iterate over the GenericFormData beans that back the form; the actual evaluation
             * selections are on this bean
             */
            Criteria storageCriteria = new Criteria();
            storageCriteria.addIn(X2BaseBean.COL_OID,
                    new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, instanceCriteria));

            BeanQuery storageQuery = new BeanQuery(GenericFormData.class, storageCriteria);
            QueryIterator storageIterator = getBroker().getIteratorByQuery(storageQuery);
            try {
                while (storageIterator.hasNext()) {
                    GenericFormData formData = (GenericFormData) storageIterator.next();
                    FormInstance instance = instances.get(formData.getOid());

                    /*
                     * Only update IEPs that have no evaluation log entries
                     */
                    if (!iepsWithEvalLogs.contains(instance.getOwnerObjectOid())) {
                        PlainDate evalDate = new PlainDate(instance.getCreatedTime());

                        WorkflowProgress progress = progressEntries.get(instance.getOwnerObjectOid());
                        if (progress != null) {
                            evalDate = progress.getDate();
                        }

                        for (String alias : consentFormStateCodeMapping.keySet()) {
                            boolean hasEval =
                                    "1".equalsIgnoreCase((String) formData.getFieldValueByAlias(alias, formDictionary));
                            if (hasEval) {
                                String stateCode = consentFormStateCodeMapping.get(alias);
                                String evalCode = lookupUserValue(IepOtherService.class,
                                        translateAliasToJavaName(EVAL_LOG_AREA_ALIAS, true), stateCode);

                                createEvalLogEntry(evalDate, instance.getOwnerObjectOid(), evalCode);
                            }
                        }

                        String outsideEval =
                                (String) formData.getFieldValueByAlias(EVAL_FORM_OUTSIDE_EVAL, formDictionary);
                        if (!StringUtils.isEmpty(outsideEval)) {
                            String evalCode = lookupUserValue(IepOtherService.class,
                                    translateAliasToJavaName(EVAL_LOG_AREA_ALIAS, true), "Q");
                            createEvalLogEntry(evalDate, instance.getOwnerObjectOid(), evalCode);
                        }

                        String otherDescription =
                                (String) formData.getFieldValueByAlias(EVAL_FORM_OTHER_EVAL, formDictionary);
                        if (!StringUtils.isEmpty(otherDescription)) {
                            String evalCode = lookupUserValue(IepOtherService.class,
                                    translateAliasToJavaName(EVAL_LOG_AREA_ALIAS, true), "U");
                            createEvalLogEntry(evalDate, instance.getOwnerObjectOid(), evalCode);
                        }
                    }
                }
            } finally {
                storageIterator.close();
            }
        }
    }

    /**
     * Creates the eval log entry.
     *
     * @param date PlainDate
     * @param iepOid String
     * @param evalCode String
     */
    private void createEvalLogEntry(PlainDate date, String iepOid, String evalCode) {
        IepOtherService evalLogEntry = new IepOtherService(getBroker().getPersistenceKey());
        evalLogEntry.setIepDataOid(iepOid);
        evalLogEntry.setExtendedDataDictionaryOid(getDataDictionary().getExtendedDictionaryOid());
        evalLogEntry.setServiceType(OTHER_SERVICE_EVALUATION_TYPE);
        evalLogEntry.setStartDate(date);
        evalLogEntry.setFieldValueByAlias(EVAL_LOG_AREA_ALIAS, evalCode, getDataDictionary());

        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, iepOid);

        SubQuery subQuery = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, criteria);

        evalLogEntry.setStudentOid((String) getBroker().getSubQueryValueByQuery(subQuery));

        getBroker().saveBeanForced(evalLogEntry);
    }
}
