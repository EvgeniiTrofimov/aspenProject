/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcomeForm;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Bean report class
 * class used for provide public method for initialize form-based report and Grid<br>
 * It is needed for SYS-SPED-IL-ALL-54 and other reports which show data from another reports which
 * extend this class<br>
 * common interface allows initialize "form-based report" class and fill it Grid<br>
 * also provide next feature:
 * 1) multiple mode. If report has few instance or few page with the same view but difference data
 * and data get from object, each page has own object:
 * filterMultipleFormInstance and processMultiple.
 * 2)print blank or no.
 * printBlankIfInstanceDoesntExist
 *
 */
public class BeanReport extends BaseFormReportJavaSource {
    private static final String JR_PARAM_INIT_REPORTS = "initReport";

    protected Boolean m_initInstanse = Boolean.valueOf(false);
    protected X2Broker m_broker;
    protected Map<String, Object> m_params;
    protected FormInstance m_formInstance = null;
    protected Locale m_locLocale = null;

    /**
     * Default constructor.
     */
    public BeanReport() {
        super();
    }


    /**
     * if multipleNumber is not empty and it is numeric- it mean that it is multiple mode<br>
     * if multipleNumber is empty or is not numeric - it is not multiple mode<br>
     * multiple mode<br>
     * method try filtered input formInstances for current mode<br>
     * default behavior - multipleNumber it is "int" which indicate position from input
     * formInstances<br>
     * int start from 1, not from 0<br>
     * logic get appropriate FormInstance from formInstances by list position (multipleNumber
     * -1)<br>
     * you can override default behavior from child class<br>
     * not multiple mode<br>
     * default behavior - return all input formInstances.
     *
     * @param formInstances List<FormInstance>
     * @param multipleNumber String
     * @param workFlow Workflow
     * @param broker X2Broker
     * @return List
     */
    public List<FormInstance> filterMultipleFormInstance(List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workFlow,
                                                         X2Broker broker) {
        List<FormInstance> returnFormInstances = null;
        if (StringUtils.isEmpty(multipleNumber)) {
            returnFormInstances = formInstances;
        } else if (StringUtils.isNumeric(multipleNumber)) {
            int number = Integer.parseInt(multipleNumber);
            if (formInstances.size() >= number) {
                returnFormInstances = new ArrayList<FormInstance>();
                returnFormInstances.add(formInstances.get(number - 1));
            }
        }
        return returnFormInstances == null ? new ArrayList<FormInstance>() : returnFormInstances;

    }


    /**
     * Initializes form-based report.
     *
     * @param formInstance can be null
     * @param formStorage X2BaseBean
     * @param formOwner owner can exist even formInstance is null. There is why it has own param
     * @param dictionary can exist even formInstance is null. There is why it has own param
     * @param params Map<String,Object>
     * @param broker X2Broker
     */
    public void initBeanReport(FormInstance formInstance,
                               X2BaseBean formStorage,
                               X2BaseBean formOwner,
                               DataDictionary dictionary,
                               Map<String, Object> params,
                               X2Broker broker) {

        m_initInstanse = Boolean.TRUE;

        setFormStorage(formStorage);
        setFormOwner(formOwner);
        setDictionary(dictionary);
        setBroker(broker);
        m_params = params;
        addParameter(JR_PARAM_INIT_REPORTS, m_initInstanse);
        addParameter(PARAM_FORM_OWNER, formOwner);
        addParameter(PARAM_FORM_STORAGE, formStorage);
        addParameter(PARAM_DICTIONARY, dictionary);
        addParameter(PARAM_BLANK, Boolean.valueOf(isBlank()));
        m_formInstance = formInstance;
    }

    /**
     * indicate is this object filled by initBeanReport method.
     *
     * @return Boolean
     */
    public Boolean isInitInstance() {
        return m_initInstanse;
    }

    /**
     * Gets the form instance.
     *
     * @return Form instance
     * @see BaseFormReportJavaSource#getFormInstance()
     */
    @Override
    public FormInstance getFormInstance() {
        FormInstance formInstance = null;
        if (m_initInstanse.booleanValue()) {
            formInstance = m_formInstance;
        } else {
            formInstance = super.getFormInstance();
        }
        return formInstance;
    }


    /**
     * !!! it is call before initBeanReport<br>
     * do not use class members<br>
     * !!! it is new Instance created just for filter formInstances<br>
     * do not put data like class members<br>
     * it is not static for privide ability be override<br>
     * Common case for filterMultipleFormInstance<br>
     * You should override filterMultipleFormInstance and use this method if "List<FormInstance>
     * formInstances" param in<br>
     * filterMultipleFormInstance method sort by random.<br>
     * this method sort formInstances Unlike default filterMultipleFormInstance method<br>
     * It order by phase sequence.<br>
     * it is work only for multiple mode (see @param multipleNumber)<br>
     * We can not use outcomeMultipleBehaviour straight in BeanReport because this method contain
     * specific param "formDefIds"<br>
     * which help match phase and formInstance with listed form definition ids<br>
     * Logic:<br>
     * try find for current workflow all outcome forms with form definition Id listed in formDefIds
     * and sort it by phase sequence (Ascending)<br>
     * multipleNumber it is ordinal number for selected "outcome form/phase".<br>
     * "outcome form/phase". mean that phase should have only one outcome form with target
     * formDefIds:<br>
     * If phase will have few outcomes and more than one outcome will have form definition with id
     * listed on formDefIds <br>
     * - in this case logic will not working and should be rework. At least this case is not met and
     * not testing<br>
     * if phase will have few outcomes but only one outcome will have definition with id listed on
     * formDefIds - it should work
     * logic try find FormInstance for founded outcome form
     *
     * @param formInstances List<FormInstance>
     * @param multipleNumber require be not null and numeric otherwise can throw Exception<br>
     * @param formDefIds List<String>
     * @param broker X2Broker
     * @param workflow Workflow
     * @return List
     */
    public List<FormInstance> outcomeMultipleBehaviour(List<FormInstance> formInstances,
                                                       String multipleNumber,
                                                       List<String> formDefIds,
                                                       X2Broker broker,
                                                       Workflow workflow) {
        List<FormInstance> returnFormInstances = new ArrayList<FormInstance>();

        if (workflow != null) {
            int multNumber = Integer.parseInt(multipleNumber);
            List<String> instanceOids = new ArrayList<String>();
            for (FormInstance formInstance : formInstances) {
                String oid = formInstance.getOid();
                if (oid != null) {
                    instanceOids.add(oid);
                }
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(
                    WorkflowPhaseOutcomeForm.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                    formDefIds);
            criteria.addEqualTo(WorkflowPhaseOutcomeForm.REL_WORKFLOW_PHASE_OUTCOME + ModelProperty.PATH_DELIMITER
                    + WorkflowPhaseOutcome.REL_WORKFLOW_PHASE + ModelProperty.PATH_DELIMITER +
                    WorkflowPhase.COL_WORKFLOW_DEFINITION_OID, workflow.getWorkflowDefinitionOid());
            QueryByCriteria byCriteria = new QueryByCriteria(WorkflowPhaseOutcomeForm.class, criteria);
            byCriteria.addOrderBy(WorkflowPhaseOutcomeForm.REL_WORKFLOW_PHASE_OUTCOME + ModelProperty.PATH_DELIMITER
                    + WorkflowPhaseOutcome.REL_WORKFLOW_PHASE + ModelProperty.PATH_DELIMITER +
                    WorkflowPhase.COL_SEQUENCE_NUMBER, true);


            List<WorkflowPhaseOutcomeForm> outcomeForms =
                    new ArrayList<WorkflowPhaseOutcomeForm>(broker.getCollectionByQuery(byCriteria));

            if (outcomeForms.size() >= multNumber) {
                WorkflowPhaseOutcomeForm outcomeForm = outcomeForms.get(multNumber - 1);
                criteria = new X2Criteria();
                criteria.addIn(WorkflowProgressForm.COL_FORM_INSTANCE_OID, instanceOids);
                criteria.addEqualTo(WorkflowProgressForm.REL_WORKFLOW_PROGRESS + ModelProperty.PATH_DELIMITER
                        + WorkflowProgress.COL_WORKFLOW_OID, workflow.getOid());
                criteria.addEqualTo(
                        WorkflowProgressForm.REL_WORKFLOW_PROGRESS + ModelProperty.PATH_DELIMITER
                                + WorkflowProgress.COL_WORKFLOW_PHASE_OUTCOME_OID,
                        outcomeForm.getWorkflowPhaseOutcomeOid());
                byCriteria = new QueryByCriteria(WorkflowProgressForm.class, criteria);
                List<WorkflowProgressForm> progressForms =
                        new ArrayList<WorkflowProgressForm>(broker.getCollectionByQuery(byCriteria));
                if (progressForms.size() == 1) {
                    returnFormInstances.add(progressForms.get(0).getFormInstance());

                }
            }

        }

        return returnFormInstances;
    }

    /**
     * It method called before initialize BeanReport. Do not use class members there are is not
     * initialized.<br>
     * Also it is will new Instance. Do not save any data like class members. Because instance need
     * only for determine<br>
     * - should or should't print blank. If blank will print - logic will create new BeanReport
     * instance for blank.<br>
     * Instance for blank will another than instance where call printBlankIfInstanceDoesntExist
     * method.<br>
     * this method should be static, but if it is will static we can not override default behavior
     *
     * @param multipleNumber String
     * @param workFlow Workflow
     * @param broker X2Broker
     * @return true, if successful
     */
    public boolean printBlankIfInstanceDoesntExist(String multipleNumber, Workflow workFlow, X2Broker broker) {
        return false;
    }


    /**
     * Method called after initBeanReprot<br>
     * You can use class member and save data like class member<br>
     * It need in case if multiple instance depend on another cases than FormInstance.
     * For example 3454 report has one FormInstance but it has few meeting and multiple instance
     * depend on count of meetings
     *
     * @param multipleNumber String
     * @param workFlow Workflow
     */
    public void processMultiple(String multipleNumber, Workflow workFlow) {
        // nothing to do
    }


    /**
     * Adds the parameter.
     *
     * @param key String
     * @param value Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#addParameter(java.lang.String,
     *      java.lang.Object)
     */
    @Override

    protected void addParameter(String key, Object value) {
        if (m_initInstanse.booleanValue()) {
            m_params.put(key, value);
        } else {
            super.addParameter(key, value);
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = super.getBroker();
        }

        return m_broker;
    }

    /**
     * Gets the locale.
     *
     * @return Locale
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getLocale()
     */
    @Override
    protected Locale getLocale() {
        Locale locale = null;
        if (m_initInstanse.booleanValue()) {
            if (m_locLocale == null) {
                m_locLocale = Locale.getDefault();
            }
            locale = m_locLocale;
        } else {
            locale = super.getLocale();
        }
        return locale;
    }

    /**
     * Gets the parameter.
     *
     * @param key String
     * @return Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getParameter(java.lang.String)
     */
    @Override
    protected Object getParameter(String key) {
        Object returnObject = null;
        if (m_initInstanse.booleanValue()) {
            returnObject = m_params.get(key);
        } else {
            returnObject = super.getParameter(key);
        }
        return returnObject;
    }

    /**
     * Gets the parameters.
     *
     * @return Map
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getParameters()
     */
    @Override
    protected Map getParameters() {
        Map returnMap;
        if (m_initInstanse.booleanValue()) {
            returnMap = m_params;
        } else {
            returnMap = super.getParameters();
        }
        return returnMap;
    }

    /**
     * Sets the broker.
     *
     * @param broker void
     */
    protected void setBroker(X2Broker broker) {
        m_broker = broker;
    }
}
