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
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.WorkflowComplianceRule;
import com.follett.fsc.core.k12.beans.WorkflowComplianceRule.DayType;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This import only add/update where the key for update is {relWfcWphOid,relWfcWpoOid}.
 *
 * @author X2 Development Corporation
 */
public class MAWorkflowRulesImport extends TextImportJavaSource {

    private static final long serialVersionUID = 1L;

    /**
     * Parameters
     */
    public static final String PARAM_DAY_TYPE = "dayType";
    public static final String PARAM_DAYS_TO_COMPLETE = "daysToComplete";
    public static final String PARAM_DISABLED = "disabled";
    public static final String PARAM_PHASE_NAME = "wphName";
    public static final String PARAM_RULE_DESC = "wfcDescription";
    public static final String PARAM_RULE_NAME = "wfcName";
    public static final String PARAM_TRIGGER_NAME = "triggerName";
    public static final String PARAM_TRIGGER_OUTCOME = "wpoOutcome";
    public static final String PARAM_WARN_TRESHOLD = "warnTreshold";
    public static final String PARAM_WORKFLOW_ID = "wfdOid";

    private Converter m_converter;
    private final DayType[] m_dayTypes = WorkflowComplianceRule.DayType.values();
    private int m_fieldCount = 0;
    private LinkedList<String> m_importMessage = new LinkedList<>();
    private int m_indDayType;
    private int m_indDaysToComplete;
    private int m_indDisabled;
    private int m_indPhaseName;
    private int m_indRuleDesc;
    private int m_indRuleName;
    private int m_indTriggerPhaseName;
    private int m_indTriggerOutcome;
    private int m_indWarnTreshold;
    private int m_indWfdOid;
    private Map<String, WorkflowDefinition> m_wfdMap;


    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
    }

    /**
     * Return StringBuilder which contain the text that would show to user when the import procedure
     * finished.
     *
     * @return StringBuilder
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder bufferMessage = super.getImportStatistics();
        if (!m_importMessage.isEmpty()) {
            bufferMessage.append('\n');
            bufferMessage.append("Detailed Import Statistic for Insert/Update/Match records.");
            bufferMessage.append('\n');
            for (String message : m_importMessage) {
                bufferMessage.append('\n');
                bufferMessage.append(message);
            }
        }
        return bufferMessage;
    }

    /**
     * Import record.
     *
     * @param record List
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        if (lineNumber == 1) {
            return; // first line of the input file is headers.
        }
        WorkflowDefinition wfd = null;
        WorkflowPhase wph = null;
        WorkflowPhaseOutcome wpo = null;
        WorkflowPhase wphTrigger = null;
        WorkflowComplianceRule wfc = null;
        String wfdOid = record.get(m_indWfdOid).trim();
        if (!StringUtils.isEmpty(wfdOid)) {
            wfd = m_wfdMap.get(wfdOid);
            if (wfd == null) {
                wfdOid = BeanManager.getFullOid(wfdOid, getBroker().getPersistenceKey());
                wfd = m_wfdMap.get(wfdOid);
            }
            if (wfd != null) {
                String wphName = record.get(m_indPhaseName);
                X2Criteria wphCriteria = new X2Criteria();
                wphCriteria.addEqualTo(WorkflowPhase.COL_WORKFLOW_DEFINITION_OID, wfd.getOid());
                wphCriteria.addEqualToIgnoreCase(WorkflowPhase.COL_NAME, wphName);
                wph = (WorkflowPhase) getBroker()
                        .getBeanByQuery(new QueryByCriteria(WorkflowPhase.class, wphCriteria));
                if (wph != null) {
                    X2Criteria wphTriggerCriteria = new X2Criteria();
                    wphTriggerCriteria.addEqualTo(WorkflowPhase.COL_WORKFLOW_DEFINITION_OID, wfd.getOid());
                    wphTriggerCriteria.addEqualToIgnoreCase(WorkflowPhase.COL_NAME,
                            record.get(m_indTriggerPhaseName));
                    wphTrigger = (WorkflowPhase) getBroker()
                            .getBeanByQuery(new QueryByCriteria(WorkflowPhase.class, wphTriggerCriteria));
                    if (wphTrigger != null) {
                        X2Criteria wpoCriteria = new X2Criteria();
                        wpoCriteria.addEqualTo(WorkflowPhaseOutcome.COL_WORKFLOW_PHASE_OID, wphTrigger.getOid());
                        wpoCriteria.addEqualToIgnoreCase(WorkflowPhaseOutcome.COL_OUTCOME,
                                record.get(m_indTriggerOutcome));
                        wpo = (WorkflowPhaseOutcome) getBroker()
                                .getBeanByQuery(new QueryByCriteria(WorkflowPhaseOutcome.class, wpoCriteria));
                        if (wpo != null) {
                            X2Criteria wfcCriteria = new X2Criteria();
                            wfcCriteria.addEqualTo(WorkflowComplianceRule.COL_WORKFLOW_PHASE_OID, wph.getOid());
                            wfcCriteria.addEqualTo(WorkflowComplianceRule.COL_WORKFLOW_PHASE_OUTCOME_OID, wpo.getOid());
                            wfc = (WorkflowComplianceRule) getBroker()
                                    .getBeanByQuery(new QueryByCriteria(WorkflowComplianceRule.class, wfcCriteria));
                            if (wfc == null) {
                                wfc = X2BaseBean.newInstance(WorkflowComplianceRule.class,
                                        getBroker().getPersistenceKey());
                                wfc.setWorkflowPhaseOid(wph.getOid());
                                wfc.setWorkflowPhaseOutcomeOid(wpo.getOid());
                                wfc.setName(record.get(m_indRuleName));
                                wfc.setDescription(record.get(m_indRuleDesc));
                                wfc.setDaysToComplete((Integer.valueOf(record.get(m_indDaysToComplete)).intValue()));
                                wfc.setWarningThreshold((Integer.valueOf(record.get(m_indWarnTreshold)).intValue()));
                                String recordValue = record.get(m_indDayType);
                                Integer enumValue = null;
                                for (DayType type : m_dayTypes) {
                                    if (recordValue.toLowerCase().startsWith(type.toString().toLowerCase())) {
                                        enumValue = Integer.valueOf(type.ordinal());
                                        break;
                                    }
                                }
                                wfc.setDayType(enumValue != null ? enumValue.intValue() : 0);
                                wfc.setDisabledIndicator(stringToBoolean(record.get(m_indDisabled)));
                                getBroker().saveBeanForced(wfc);
                                m_importMessage.add("Acc.to lineNumber = " + lineNumber
                                        + ". ADDED. Compliance rule for Workflow Definition with Name = \""
                                        + wfd.getName()
                                        + "\", with Workflow Phase Name = \"" + wph.getName()
                                        + "\", with Workflow Phase Outcome = \"" + wpo.getOutcome()
                                        + "\", with Workflow Phase for Workflow Phase Outcome Name = \""
                                        + wphTrigger.getName() + "\".");
                                incrementInsertCount();

                            } else {
                                m_importMessage.add("Acc. to lineNumber = " + lineNumber
                                        + ". MATCH. Compliance rule for Workflow Definition with Name = \""
                                        + wfd.getName()
                                        + "\", with Workflow Phase Name = \"" + wph.getName()
                                        + "\", with Workflow Phase Outcome = \"" + wpo.getOutcome()
                                        + "\", with Workflow Phase for Workflow Phase Outcome Name = \""
                                        + wphTrigger.getName() + "\".");
                                incrementMatchCount();
                            }
                        } else {
                            logInvalidRecord(lineNumber,
                                    "Workflow Phase Outcome with Name = \"" + record.get(m_indTriggerOutcome)
                                            + "\", for Workflow Definition with Name = \"" + wfd.getName()
                                            + "\" not found. Workflow Phase for Workflow Phase Outcome can't be set. WF Definition name = \""
                                            + wfd.getName() + "\".");
                            incrementSkipCount();
                        }
                    } else {
                        logInvalidRecord(lineNumber,
                                "Workflow Trigger Phase with Name = \"" + record.get(m_indTriggerPhaseName)
                                        + "\", and Workflow Phase = \"" + wph.getName()
                                        + "\" not found. WF Definition name = \""
                                        + wfd.getName());
                        incrementSkipCount();
                    }
                } else {
                    logInvalidRecord(lineNumber, "Workflow Phase with Name = \"" + wphName
                            + ", and Workflow Definition Oid = \"" + wfd.getOid()
                            + "\" not found. WF Definition name = \""
                            + wfd.getName());
                    incrementSkipCount();
                }
            } else {
                logInvalidRecord(lineNumber, "Workflow Definition with Oid = \"" + wfdOid + "\" not found.");
                incrementSkipCount();
            }
        } else {
            logInvalidRecord(lineNumber, "No value for Workflow ID. Check please importedd file.");
            incrementSkipCount();
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setValueWrappingMode(VALUE_WRAPPING_MODE.REQUIRED);
        setValueDelimiter(',');
        setValueWrapper('"');
        m_indDayType = ((Integer) getParameter(PARAM_DAY_TYPE)).intValue();
        m_fieldCount++;
        m_indDaysToComplete = ((Integer) getParameter(PARAM_DAYS_TO_COMPLETE)).intValue();
        m_fieldCount++;
        m_indDisabled = ((Integer) getParameter(PARAM_DISABLED)).intValue();
        m_fieldCount++;
        m_indPhaseName = ((Integer) getParameter(PARAM_PHASE_NAME)).intValue();
        m_fieldCount++;
        m_indRuleDesc = ((Integer) getParameter(PARAM_RULE_DESC)).intValue();
        m_fieldCount++;
        m_indRuleName = ((Integer) getParameter(PARAM_RULE_NAME)).intValue();
        m_fieldCount++;
        m_indTriggerPhaseName = ((Integer) getParameter(PARAM_TRIGGER_NAME)).intValue();
        m_fieldCount++;
        m_indTriggerOutcome = ((Integer) getParameter(PARAM_TRIGGER_OUTCOME)).intValue();
        m_fieldCount++;
        m_indWarnTreshold = ((Integer) getParameter(PARAM_WARN_TRESHOLD)).intValue();
        m_fieldCount++;
        m_indWfdOid = ((Integer) getParameter(PARAM_WORKFLOW_ID)).intValue();
        m_fieldCount++;
        m_wfdMap = getBroker().getMapByQuery(new QueryByCriteria(WorkflowDefinition.class), X2BaseBean.COL_OID, 512);
    }

    /**
     * Converts the simple string representation of a boolean to the application-specific string
     * representation of the same boolean value.
     *
     * @param source String
     * @return String
     * @see com.x2dev.utils.converters.Converter#stringToJava(java.lang.String)
     */
    private boolean stringToBoolean(String source) {
        boolean convertedValue = false;
        if (source != null) {
            if (m_converter == null) {
                m_converter = ConverterFactory.getConverterForClass(Converter.BOOLEAN_CONVERTER);
            }
            convertedValue = (((Boolean) m_converter.stringToJava(source))).booleanValue();
        }
        return convertedValue;
    }
}
