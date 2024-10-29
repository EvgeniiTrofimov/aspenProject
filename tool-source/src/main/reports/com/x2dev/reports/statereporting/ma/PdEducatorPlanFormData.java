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
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PdEducatorPlanFormData.
 *
 * @author X2 Development Corporation
 */
public class PdEducatorPlanFormData extends BaseFormReportJavaSource {
    // Subreports datasources names.
    private static final String DATA_SOURCE_NAME_PROF_ACTIV = "profActivDataSource";
    private static final String DATA_SOURCE_NAME_STUDENT_ACTIV = "studActivDataSource";
    private static final String DATA_SOURCE_NAME_PROF_GOALS = "profGoalsDataSource";
    private static final String DATA_SOURCE_NAME_STUDENT_GOALS = "studGoalsDataSource";

    // Form ID.
    private static final String FORM_ID_SELF_ASSESS = "SELF-ASSESS";

    // Goal types.
    private static final String GOAL_ACTIVITY_PROFESSIONAL = "Activity-Professional";
    private static final String GOAL_ACTIVITY_STUDENT = "Activity-Student";
    private static final String GOAL_PROFESSIONAL = "Goal-Professional";
    private static final String GOAL_STUDENT = "Goal-Student";

    // Report ID input parameter.
    private static final String INPUT_PARAMETER_REPORT_ID = "reportId";

    // Report parameters.
    private static final String REPORT_PARAMETER_PRIMARY_EVALUATOR = "primaryEvaluator";
    private static final String REPORT_PARAMETER_SUPERVISOR_EVALUATOR = "supervisorEvaluator";

    // Storage table aliases.
    private static final String STORAGE_ALIAS_ACTIVITY_ACTION = "activity-action-big";
    private static final String STORAGE_ALIAS_ACTIVITY_RESOURCE = "activity-resource-big";
    private static final String STORAGE_ALIAS_ACTIVITY_TIMELINE = "activity-timeline";
    private static final String STORAGE_ALIAS_GOAL_ID = "goal-id";
    private static final String STORAGE_ALIAS_GOAL_NARRATIVE = "goal-narrative";
    private static final String STORAGE_ALIAS_GOAL_SCOPE = "goal-scope";
    private static final String STORAGE_ALIAS_GOAL_TYPE = "goal-type";
    private static final String STORAGE_ALIAS_PRIMARY_EVALUATOR_OID = "stf-oid-primary";
    private static final String STORAGE_ALIAS_SUPERVIS_EVALUATOR = "supervise-2-role";

    // Report fields.
    private static final String SUBREPORT_FIELD_ACTIVITY_ACTION = "activity-action";
    private static final String SUBREPORT_FIELD_ACTIVITY_RESOURCE = "activity-resource";
    private static final String SUBREPORT_FIELD_ACTIVITY_TIMELINE = "activity-timeline";
    private static final String SUBREPORT_FIELD_GOAL_ID = "goal-id";
    private static final String SUBREPORT_FIELD_GOAL_SCOPE = "goal-scope";
    private static final String SUBREPORT_FIELD_GOAL_NARRATIVE = "goal-narrative";

    // Subreports formats names.
    private static final String SUBREPORT_FORMAT_PREFIX = "subreportFormat";

    // Subreports IDs.
    private static final List<String> m_subreportIds = Arrays.asList("TOBS-EDU-PLAN_ST_AC",
            "TOBS-EDU-PLAN_PRO_AC",
            "TOBS-GOAL-SET_ST_G",
            "TOBS-GOAL-SET_PRO_G");

    // Subreports datasources variables.
    private ReportDataGrid m_gridProfActiv;
    private ReportDataGrid m_gridStudActiv;
    private ReportDataGrid m_gridStudGoals;
    private ReportDataGrid m_gridProfGoals;

    private String m_reportId;

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
        String workflowOid = null;
        WorkflowProgress wp = null;

        for (WorkflowProgressForm wpf : wpfs) {
            wp = wpf.getWorkflowProgress();
            workflowOid = wp.getWorkflowOid();
        }

        if (workflowOid != null) {
            // Prepare used references.
            String primEvalOid = null;

            // Prepare criteria and create query.
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                    FormDefinition.COL_ID, FORM_ID_SELF_ASSESS);
            criteria.addEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + ModelProperty.PATH_DELIMITER +
                    WorkflowProgressForm.REL_WORKFLOW_PROGRESS + ModelProperty.PATH_DELIMITER +
                    WorkflowProgress.COL_WORKFLOW_OID,
                    workflowOid);
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
        }

        // Add "Primary evaluator" and "Supervisor evaluator".
        addParameter(REPORT_PARAMETER_PRIMARY_EVALUATOR, primaryEvaluator);
        addParameter(REPORT_PARAMETER_SUPERVISOR_EVALUATOR, supervisorEvaluator);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_reportId = (String) getParameter(INPUT_PARAMETER_REPORT_ID);
        setFormatId(m_reportId);

        prepareSubreportDataSources();

        // Add datasources for subreports.
        addParameter(DATA_SOURCE_NAME_STUDENT_ACTIV, m_gridStudActiv);
        addParameter(DATA_SOURCE_NAME_PROF_ACTIV, m_gridProfActiv);
        addParameter(DATA_SOURCE_NAME_STUDENT_GOALS, m_gridStudGoals);
        addParameter(DATA_SOURCE_NAME_PROF_GOALS, m_gridProfGoals);

        addReportFormats(m_subreportIds);
    }

    /**
     * Add activity to the grid.
     *
     * @param activity GenericFormChildData
     * @param grid ReportDataGrid
     */
    private void addActivityToGrid(GenericFormChildData activity, ReportDataGrid grid) {
        grid.append();
        grid.set(SUBREPORT_FIELD_ACTIVITY_ACTION, activity.getFieldValueByAlias(
                STORAGE_ALIAS_ACTIVITY_ACTION,
                getDictionary()));
        grid.set(SUBREPORT_FIELD_ACTIVITY_RESOURCE, activity.getFieldValueByAlias(
                STORAGE_ALIAS_ACTIVITY_RESOURCE,
                getDictionary()));
        grid.set(SUBREPORT_FIELD_ACTIVITY_TIMELINE, activity.getFieldValueByAlias(
                STORAGE_ALIAS_ACTIVITY_TIMELINE,
                getDictionary()));
        grid.set(SUBREPORT_FIELD_GOAL_ID, activity.getFieldValueByAlias(
                STORAGE_ALIAS_GOAL_ID,
                getDictionary()));
        grid.set(SUBREPORT_FIELD_GOAL_SCOPE, activity.getFieldValueByAlias(
                STORAGE_ALIAS_GOAL_SCOPE,
                getDictionary()));
        // It is needed because there are problems with styled text ("id" and "scope")
        // if some non-ASCII symbols were passed.
        String narrative = null;
        narrative = (String) activity.getFieldValueByAlias(STORAGE_ALIAS_GOAL_NARRATIVE, getDictionary());
        if (narrative != null) {
            narrative = narrative.replaceAll("[^\\x0A\\x0D\\x20-\\x7E]", "");
        }
        grid.set(SUBREPORT_FIELD_GOAL_NARRATIVE, narrative);
    }

    /**
     * Add reports formats to the SimpleBeanDataSource as parameters.
     *
     * @param subReportsIds List<String>
     */
    private void addReportFormats(List<String> subReportsIds) {
        int index = 0;
        for (String subReportId : subReportsIds) {
            index++;
            Report subReport = ReportUtils.getReport(subReportId, getBroker());
            addParameter(SUBREPORT_FORMAT_PREFIX + index, new ByteArrayInputStream(subReport.getCompiledFormat()));
        }
    }

    /**
     * Prepare datasources for subreports "Student Learning Goal(s): Planned Activities" and
     * "Professional Practice Goal(s): Planned Activities".
     */
    private void prepareSubreportDataSources() {
        m_gridStudActiv = new ReportDataGrid();
        m_gridProfActiv = new ReportDataGrid();
        m_gridStudGoals = new ReportDataGrid();
        m_gridProfGoals = new ReportDataGrid();

        Collection<GenericFormChildData> childs = ((GenericFormData) getFormStorage()).getGenericFormDataChildren();

        for (GenericFormChildData child : childs) {
            String activity = (String) child.getFieldValueByAlias(STORAGE_ALIAS_GOAL_TYPE, getDictionary());

            if (GOAL_ACTIVITY_STUDENT.equals(activity)) {
                addActivityToGrid(child, m_gridStudActiv);
            } else if (GOAL_ACTIVITY_PROFESSIONAL.equals(activity)) {
                addActivityToGrid(child, m_gridProfActiv);
            } else if (GOAL_STUDENT.equals(activity)) {
                addActivityToGrid(child, m_gridStudGoals);
            } else if (GOAL_PROFESSIONAL.equals(activity)) {
                addActivityToGrid(child, m_gridProfGoals);
            }
        }

        // These conditions needed to print correctly empty subreports.
        if (m_gridProfActiv.rowCount() == 0) {
            m_gridProfActiv.append();
        }
        if (m_gridStudActiv.rowCount() == 0) {
            m_gridStudActiv.append();
        }
        if (m_gridStudGoals.rowCount() == 0) {
            m_gridStudGoals.append();
        }
        if (m_gridProfGoals.rowCount() == 0) {
            m_gridProfGoals.append();
        }

        m_gridStudActiv.beforeTop();
        m_gridProfActiv.beforeTop();
        m_gridStudGoals.beforeTop();
        m_gridProfGoals.beforeTop();
    }
}
