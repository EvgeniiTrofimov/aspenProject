/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.business.sped.SpedUtils;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Procedure to update Consent date after Consent receive outcome if date was not set.
 */
public class MAUpdateConsentDateProcedure extends ProcedureJavaSource {

    /**
     * Constants.
     */
    private static final String ALIAS_CONSENT_RECEIVED = "asm-consent-date";
    private static final String WPH_REC_NAME = "Receive assessment decision";
    private static final String WPO_METHOD_ID = "assessmentConsentReceived";

    /**
     * Class members.
     */
    private Collection<WorkflowProgress> m_progresses = new ArrayList<WorkflowProgress>();

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        for (WorkflowProgress progress : m_progresses) {
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            if (iep != null && iep.getFieldValueByAlias(ALIAS_CONSENT_RECEIVED) == null) {
                iep.setFieldValueByAlias(ALIAS_CONSENT_RECEIVED, progress.getDate().toString(), dictionary);
                getBroker().saveBeanForced(iep);
                logMessage("Consent received date was updated for Student with State ID = \""
                        + iep.getStudent().getStateId() + "\". Date by Alias = \"" + ALIAS_CONSENT_RECEIVED
                        + "\" set to \""
                        + progress.getDate() + "\".");
            }
        }
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowProgress.REL_WORKFLOW_PHASE + ModelProperty.PATH_DELIMITER + WorkflowPhase.COL_NAME,
                WPH_REC_NAME);
        criteria.addEqualTo(WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + ModelProperty.PATH_DELIMITER
                + WorkflowPhaseOutcome.COL_METHOD_ID, WPO_METHOD_ID);
        criteria.addNotNull(WorkflowProgress.COL_DATE);
        m_progresses = getBroker().getCollectionByQuery(new QueryByCriteria(WorkflowProgress.class, criteria));
    }
}
