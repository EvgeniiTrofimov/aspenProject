/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;



/**
 * The Class EdResponseFormData.
 *
 * @author X2 Development Corporation
 */
public class EdResponseFormData extends BaseFormReportJavaSource {
    private static final String ALIAS_RESPONSE = "response";

    private static final List<String> CORRECT_OUTCOME_ID =
            new ArrayList<String>(Arrays.asList("observe", "observe4", "observe3",
                    "FormativeEval", "SummativeEval2", "AssessmentMtg"));

    private static final String EMPTY = "";

    private static final String FORM_ID_SELF_ASSESS = "SELF-ASSESS";

    private static final String OUTCOME_ASSESSMENT_MTG = "AssessmentMtg";
    private static final String OUTCOME_FORMATIVE_EVAL = "FormativeEval";
    private static final String OUTCOME_SUMMATIVE_EVAL2 = "SummativeEval2";

    private static final String PARAM_LAST_PLAN_IMPL_CORRECT_OUTCOME = "correctOutcome";
    private static final String PARAM_RESPONCE = "responce";

    private static final String PREFIX_FA = "fa-";
    private static final String PREFIX_FORM = "form-";
    private static final String PREFIX_SE = "se-";

    private static final String REPORT_PARAMETER_CURRENT_WORKFLOW = "currentWorkflow";
    private static final String REPORT_PARAMETER_PRIMARY_EVALUATOR = "primaryEvaluator";
    private static final String REPORT_PARAMETER_SUPERVISOR_EVALUATOR = "supervisorEvaluator";

    private static final String STORAGE_ALIAS_PRIMARY_EVALUATOR_OID = "stf-oid-primary";
    private static final String STORAGE_ALIAS_SUPERVIS_EVALUATOR = "supervise-2-role";

    private static final String W_PHASE_PLAN_IMPLEMENT = "PLAN-IMPLEMENT";


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        /*
         * Get fields "Primary evaluator" and "Supervisor evaluator" from the SELF-ASSESS form
         * associated with the
         * workflow.
         */



        Collection<WorkflowProgressForm> wpfs = getFormInstance().getWorkflowProgressForms();

        // Prepare used references.
        String primaryEvaluator = null;
        String supervisorEvaluator = null;
        WorkflowProgress currentWorkflowProgress = null;
        WorkflowProgress workflowForDetermineFieldValue = null;
        String selectOutcomeInPlanImpl = null;
        String response = null;
        WorkflowProgress lastPlanImpWProgress = null;
        String alias = null;

        String currentWOid = (String) getParameter(REPORT_PARAMETER_CURRENT_WORKFLOW);
        for (WorkflowProgressForm wpf : wpfs) {
            currentWorkflowProgress = wpf.getWorkflowProgress();
        }
        if (currentWorkflowProgress != null) {

            // Prepare used references.
            String primEvalOid = null;

            // Prepare criteria and create query.
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                    FormDefinition.COL_ID, FORM_ID_SELF_ASSESS);
            criteria.addEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + ModelProperty.PATH_DELIMITER +
                    WorkflowProgressForm.REL_WORKFLOW_PROGRESS + ModelProperty.PATH_DELIMITER +
                    WorkflowProgress.COL_WORKFLOW_OID,
                    currentWOid);
            QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);


            // Get needed formInstances (it should be only one SELF-ASSESS form).
            Collection<FormInstance> formInstances = getBroker().getCollectionByQuery(query);
            for (FormInstance fi : formInstances) {
                // Get data dictionary for current Storage table.
                DataDictionary dd =
                        DataDictionary.getDistrictDictionary(fi.getFormDefinition().getExtendedDataDictionary(),
                                getBroker().getPersistenceKey());
                // Get Primary evaluator's OID (a Staff OID) to use in further.
                primEvalOid = (String) fi.getStorageObject().getFieldValueByAlias(STORAGE_ALIAS_PRIMARY_EVALUATOR_OID,
                        dd);

                // Get Supervisor evaluator.
                supervisorEvaluator = (String) fi.getStorageObject().getFieldValueByAlias(
                        STORAGE_ALIAS_SUPERVIS_EVALUATOR, dd);
            }

            // Get Primary evaluator.
            if (primEvalOid != null) {
                Staff pe = (Staff) getBroker().getBeanByOid(Staff.class, primEvalOid);
                if (pe != null) {
                    primaryEvaluator = pe.getNameView();
                }
            }

            ///////////////////////////////////////////////////
            lastPlanImpWProgress = getPreviosWProgressByWPhaseId(currentWorkflowProgress, W_PHASE_PLAN_IMPLEMENT);
            String outcome =
                    (lastPlanImpWProgress == null ? EMPTY : lastPlanImpWProgress.getWorkflowPhaseOutcome().getId());
            if (CORRECT_OUTCOME_ID.contains(outcome)) {
                selectOutcomeInPlanImpl = outcome;
            }
            if (outcome.equals(OUTCOME_FORMATIVE_EVAL)) {
                alias = PREFIX_FORM + ALIAS_RESPONSE;
                workflowForDetermineFieldValue = currentWorkflowProgress;
            } else if (outcome.equals(OUTCOME_SUMMATIVE_EVAL2)) {
                alias = PREFIX_SE + ALIAS_RESPONSE;
                workflowForDetermineFieldValue = currentWorkflowProgress;
            } else if (outcome.equals(OUTCOME_ASSESSMENT_MTG)) {
                alias = PREFIX_FA + ALIAS_RESPONSE;
                Collection<WorkflowProgress> wProgressCollection =
                        lastPlanImpWProgress.getWorkflowPhaseOutcome().getNextWorkflowPhase().getWorkflowProgress();
                WorkflowProgress lastWProgess =
                        getPreviousWProgressFromCollection(currentWorkflowProgress, wProgressCollection);
                workflowForDetermineFieldValue = lastWProgess;
            } else {
                alias = ALIAS_RESPONSE;
                Collection<WorkflowProgress> wProgressCollection =
                        lastPlanImpWProgress.getWorkflowPhaseOutcome().getNextWorkflowPhase().getWorkflowProgress();
                WorkflowProgress lastWProgess =
                        getPreviousWProgressFromCollection(currentWorkflowProgress, wProgressCollection);
                workflowForDetermineFieldValue = lastWProgess;
            }
            response = (String) getFValueByAliasFromWProgress(workflowForDetermineFieldValue, alias);
        }

        addParameter(REPORT_PARAMETER_PRIMARY_EVALUATOR, primaryEvaluator);
        addParameter(REPORT_PARAMETER_SUPERVISOR_EVALUATOR, supervisorEvaluator);
        addParameter(PARAM_LAST_PLAN_IMPL_CORRECT_OUTCOME,
                selectOutcomeInPlanImpl == null ? EMPTY : selectOutcomeInPlanImpl);
        addParameter(PARAM_RESPONCE, response == null ? EMPTY : response);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }



    /**
     * <p>
     * Get field value by alias from workflowProgress
     * </p>
     * <p>
     * Warning: storage object in this workflowProgress must be GenericFormData
     * </p>
     * .
     *
     * @param wProgress WorkflowProgress
     * @param alias String
     * @return Object
     * @
     */
    private Object getFValueByAliasFromWProgress(WorkflowProgress wProgress, String alias) {
        GenericFormData genericFormData = (GenericFormData) wProgress.getFormInstance().getStorageObject();
        FormDefinition formDefinition = wProgress.getFormInstance().getFormDefinition();
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(formDefinition.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        return genericFormData.getFieldValueByAlias(alias, dataDictionary);
    }

    /**
     * Finds list of WorkflowProgress that match wPhaseId
     * and returns previous WorkflowProgress relative to currentWorProgress from list found.
     *
     * @param currentWorProgress WorkflowProgress
     * @param wPhaseId String
     * @return previous WorkflowProgress relative to currentWorProgress from list found
     */
    private WorkflowProgress getPreviosWProgressByWPhaseId(WorkflowProgress currentWorProgress, String wPhaseId) {
        String workflowOid = currentWorProgress.getWorkflowOid();
        Criteria wProgressCriteria = new Criteria();
        wProgressCriteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_OID, workflowOid);
        wProgressCriteria.addEqualTo(
                WorkflowProgress.REL_WORKFLOW_PHASE + ModelProperty.PATH_DELIMITER + WorkflowPhase.COL_ID, wPhaseId);
        QueryByCriteria wProgressQuery = new QueryByCriteria(WorkflowProgress.class, wProgressCriteria);
        Collection<WorkflowProgress> wProgressCollection = getBroker().getCollectionByQuery(wProgressQuery);
        return getPreviousWProgressFromCollection(currentWorProgress, wProgressCollection);
    }

    /**
     * get previous WorkflowProgress relatively to current WorkflowProgress from collection.
     *
     * @param currentWorProgress WorkflowProgress
     * @param wProgressCollection Collection<WorkflowProgress>
     * @return Workflow progress
     */
    private WorkflowProgress getPreviousWProgressFromCollection(WorkflowProgress currentWorProgress,
                                                                Collection<WorkflowProgress> wProgressCollection) {
        WorkflowProgress lastProgress = null;

        Iterator<WorkflowProgress> progressIterator = wProgressCollection.iterator();

        while (progressIterator.hasNext()) {
            WorkflowProgress wProgress = progressIterator.next();
            if (wProgress.getTimestamp() < currentWorProgress.getTimestamp()) {
                lastProgress = (lastProgress == null ? wProgress
                        : wProgress.getTimestamp() > lastProgress.getTimestamp() ? wProgress : lastProgress);
            }
        }
        return lastProgress;
    }
}
