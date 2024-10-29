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
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcomeForm;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.WorkflowProgressForm;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>Simple   FormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class MeetingNotes extends BeanReport {

    private static final String TBD_IEP_MEETING = "tbdIEPMeeting";

    /**
     * Filter multiple form instance.
     *
     * @param formInstances List<FormInstance>
     * @param multipleNumber String
     * @param workflow Workflow
     * @param broker X2Broker
     * @return List
     * @see BeanReport#filterMultipleFormInstance(List, String, Workflow, X2Broker)
     */
    @Override
    public List<FormInstance> filterMultipleFormInstance(List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workflow,
                                                         X2Broker broker) {
        List<FormInstance> returnFormInstances = null;

        List<String> formDefIdList = getFormDefinitionIDList(TBD_IEP_MEETING, workflow, broker);
        List<FormInstance> loadedFormInstances = getFormInstances(formDefIdList, workflow, broker);

        if (!StringUtils.isEmpty(multipleNumber) && StringUtils.isNumeric(multipleNumber)) {
            returnFormInstances =
                    outcomeMultipleBehaviour(loadedFormInstances, multipleNumber, formDefIdList, broker, workflow);
        } else {
            returnFormInstances = loadedFormInstances;
        }

        return returnFormInstances;
    }

    /**
     * Short description:<br>
     * try find form definition ids with storage table configuration oid like tbdTableConfOid for
     * current workflow.<br>
     * Current implementation:<br>
     * try find WorkflowPhaseOutcomeForm in current workflow definition which has form definition
     * with storage table configuration oid like tbdTableConfOid<br>
     * than get list unique form definition ids from selected WorkflowPhaseOutcomeForm
     *
     * @param tbdTableConfOid String
     * @param workflow Workflow
     * @param broker X2Broker
     * @return List
     */
    private List<String> getFormDefinitionIDList(String tbdTableConfOid, Workflow workflow, X2Broker broker) {
        List<String> fdIdList = null;
        String workflowId = workflow.getWorkflowDefinition().getId();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowPhaseOutcomeForm.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                FormDefinition.COL_STORAGE_DATA_TABLE_CONFIG_OID, tbdTableConfOid);
        criteria.addEqualTo(WorkflowPhaseOutcomeForm.REL_WORKFLOW_PHASE_OUTCOME + ModelProperty.PATH_DELIMITER +
                WorkflowPhaseOutcome.REL_WORKFLOW_PHASE + ModelProperty.PATH_DELIMITER +
                WorkflowPhase.REL_WORKFLOW_DEFINITION + ModelProperty.PATH_DELIMITER +
                WorkflowDefinition.COL_ID, workflowId);
        Map<String, Collection<WorkflowPhaseOutcomeForm>> formDefIdsMap =
                broker.getGroupedCollectionByQuery(new QueryByCriteria(WorkflowPhaseOutcomeForm.class, criteria),
                        WorkflowPhaseOutcomeForm.REL_FORM_DEFINITION +
                                ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        10);
        fdIdList = new ArrayList<String>(formDefIdsMap.keySet());

        return fdIdList;
    }

    /**
     * try find FormInstances for current workflow where form Definition ID in formDefIdList.
     *
     * @param formDefIdList List<String>
     * @param workflow Workflow
     * @param broker X2Broker
     * @return List
     */
    private List<FormInstance> getFormInstances(List<String> formDefIdList, Workflow workflow, X2Broker broker) {
        List<FormInstance> formInstances = new ArrayList<FormInstance>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, workflow.getOwnerOid());
        criteria.addIn(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER +
                FormDefinition.COL_ID, formDefIdList);
        criteria.addEqualTo(FormInstance.REL_WORKFLOW_PROGRESS_FORMS + ModelProperty.PATH_DELIMITER +
                WorkflowProgressForm.REL_WORKFLOW_PROGRESS + ModelProperty.PATH_DELIMITER +
                WorkflowProgress.COL_WORKFLOW_OID, workflow.getOid());
        formInstances.addAll(broker.getCollectionByQuery(new QueryByCriteria(FormInstance.class, criteria)));

        return formInstances;
    }
}
