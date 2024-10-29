/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ViewTemplate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for correcting current aliases to proposed for staff roster.
 *
 * @author X2 Development Corporation
 */
public class CorrectAliasesForStaffRoster extends ProcedureJavaSource {

    /**
     * Codes
     */
    protected static final String CODE_STATE_04 = "04";
    protected static final String CODE_STATE_05 = "05";
    protected static final String SR_09_CODE = "Working, non-district employee";
    protected static final String SR_34_CODE_1 = "Year 1 of Data Collection";
    protected static final String SR_34_CODE_2 = "Year 2 of Data Collection";

    /**
     * Current aliases
     */
    protected static final String SR_09_EPIMS_STATUS = "SR09";
    protected static final String SR_34_IMPACT_ON_STD = "SR34";

    /**
     * Old aliases
     */
    protected static final String SR_35_DAYS_PRESENT = "SR35";
    protected static final String SR_36_ADJUSTED_DAYS_PRESENT = "SR36";
    protected static final String SR_37_DAYS_EXPECTED = "SR37";
    protected static final String SR_38_ADJUSTED_DAYS_EXPECTED = "SR38";
    protected static final String SR_39_EDUCATOR_EVALUATION_PLAN = "SR39";

    /**
     * New aliases
     */
    protected static final String SR_35_EDUCATOR_EVALUATION_PLAN_NEW = "SR35 STF";
    protected static final String SR_36_DAYS_PRESENT_NEW = "SR36 STF";
    protected static final String SR_36_ADJUSTED_DAYS_PRESENT_NEW = "SR36 STF ADJUSTED";
    protected static final String SR_37_DAYS_EXPECTED_NEW = "SR37 STF";
    protected static final String SR_37_ADJUSTED_DAYS_EXPECTED_NEW = "SR37 STF ADJUSTED";

    /**
     * Class members
     */
    private X2Broker m_broker;
    private DataDictionary m_dictionary;

    /**
     * Returns a local instance of a district data dictionary.
     *
     * @return DataDictionary.
     */
    public DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        correctAliases();
        correctTemplates();
        correctReferenceCodes();

    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_broker = getBroker();

    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);

    }

    /**
     * 1. Look for the fields with aliases that are needed to be changed.</br>
     * 2. Change found aliases to the proposed.
     */
    private void correctAliases() {

        DataDictionaryField fldSr35 = getDataDictionary().findDataDictionaryFieldByAlias(SR_35_DAYS_PRESENT);
        DataDictionaryField fldSr36 = getDataDictionary().findDataDictionaryFieldByAlias(SR_36_ADJUSTED_DAYS_PRESENT);
        DataDictionaryField fldSr37 = getDataDictionary().findDataDictionaryFieldByAlias(SR_37_DAYS_EXPECTED);
        DataDictionaryField fldSr38 = getDataDictionary().findDataDictionaryFieldByAlias(SR_38_ADJUSTED_DAYS_EXPECTED);
        DataDictionaryField fldSr39 =
                getDataDictionary().findDataDictionaryFieldByAlias(SR_39_EDUCATOR_EVALUATION_PLAN);

        processDataField(fldSr35, SR_35_DAYS_PRESENT, SR_36_DAYS_PRESENT_NEW);
        processDataField(fldSr36, SR_36_ADJUSTED_DAYS_PRESENT, SR_36_ADJUSTED_DAYS_PRESENT_NEW);
        processDataField(fldSr37, SR_37_DAYS_EXPECTED, SR_37_DAYS_EXPECTED_NEW);
        processDataField(fldSr38, SR_38_ADJUSTED_DAYS_EXPECTED, SR_37_ADJUSTED_DAYS_EXPECTED_NEW);
        processDataField(fldSr39, SR_39_EDUCATOR_EVALUATION_PLAN, SR_35_EDUCATOR_EVALUATION_PLAN_NEW);

    }

    /**
     * Add reference codes for SR09 and SR34 to the client data.
     */
    private void correctReferenceCodes() {

        DataDictionaryField fldSr09 = getDataDictionary().findDataDictionaryFieldByAlias(SR_09_EPIMS_STATUS);
        DataDictionaryField fldSr34 = getDataDictionary().findDataDictionaryFieldByAlias(SR_34_IMPACT_ON_STD);

        processReferenceCode(fldSr09, SR_09_CODE, CODE_STATE_05, SR_09_EPIMS_STATUS);
        processReferenceCode(fldSr34, SR_34_CODE_1, CODE_STATE_04, SR_34_IMPACT_ON_STD);
        processReferenceCode(fldSr34, SR_34_CODE_2, CODE_STATE_05, SR_34_IMPACT_ON_STD);
    }

    /**
     * Correct all existing templates.
     */
    private void correctTemplates() {
        X2Criteria criteria = new X2Criteria();
        X2Criteria criteria1 = new X2Criteria();
        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        X2Criteria criteria4 = new X2Criteria();

        criteria.addContains(ViewTemplate.COL_VIEW_DEFINITION, SR_35_DAYS_PRESENT);

        criteria1.addContains(ViewTemplate.COL_VIEW_DEFINITION, SR_36_ADJUSTED_DAYS_PRESENT);
        criteria.addOrCriteria(criteria1);

        criteria2.addContains(ViewTemplate.COL_VIEW_DEFINITION, SR_37_DAYS_EXPECTED);
        criteria.addOrCriteria(criteria2);

        criteria3.addContains(ViewTemplate.COL_VIEW_DEFINITION, SR_38_ADJUSTED_DAYS_EXPECTED);
        criteria.addOrCriteria(criteria3);

        criteria4.addContains(ViewTemplate.COL_VIEW_DEFINITION, SR_39_EDUCATOR_EVALUATION_PLAN);
        criteria.addOrCriteria(criteria4);

        QueryByCriteria query = new QueryByCriteria(ViewTemplate.class, criteria);
        query.addOrderByAscending(ViewTemplate.COL_CONTEXT);

        QueryIterator templates = getBroker().getIteratorByQuery(query);

        boolean isAnyModified = false;
        try {
            while (templates.hasNext()) {
                ViewTemplate template = (ViewTemplate) templates.next();
                String viewDefinition = template.getViewDefinition();

                int pos35 = viewDefinition.indexOf(SR_35_DAYS_PRESENT);
                int pos36 = viewDefinition.indexOf(SR_36_ADJUSTED_DAYS_PRESENT);
                int pos37 = viewDefinition.indexOf(SR_37_DAYS_EXPECTED);
                int pos38 = viewDefinition.indexOf(SR_38_ADJUSTED_DAYS_EXPECTED);
                int pos39 = viewDefinition.indexOf(SR_39_EDUCATOR_EVALUATION_PLAN);

                if (pos35 > 0) {
                    viewDefinition = viewDefinition.replaceAll("alias=\"" + SR_35_DAYS_PRESENT + "\"",
                            "alias=\"" + SR_36_DAYS_PRESENT_NEW + "\"");
                }
                if (pos36 > 0) {
                    viewDefinition = viewDefinition.replaceAll("alias=\"" + SR_36_ADJUSTED_DAYS_PRESENT + "\"",
                            "alias=\"" + SR_36_ADJUSTED_DAYS_PRESENT_NEW + "\"");
                }
                if (pos37 > 0) {
                    viewDefinition = viewDefinition.replaceAll("alias=\"" + SR_37_DAYS_EXPECTED + "\"",
                            "alias=\"" + SR_37_DAYS_EXPECTED_NEW + "\"");
                }
                if (pos38 > 0) {
                    viewDefinition = viewDefinition.replaceAll("alias=\"" + SR_38_ADJUSTED_DAYS_EXPECTED + "\"",
                            "alias=\"" + SR_37_ADJUSTED_DAYS_EXPECTED_NEW + "\"");
                }
                if (pos39 > 0) {
                    viewDefinition = viewDefinition.replaceAll("alias=\"" + SR_39_EDUCATOR_EVALUATION_PLAN + "\"",
                            "alias=\"" + SR_35_EDUCATOR_EVALUATION_PLAN_NEW + "\"");
                }

                template.setViewDefinition(viewDefinition);
                if (template.isDirty()) {
                    logMessage("Updating template " + template.getContext() + " " + template.getName());

                    m_broker.saveBeanForced(template);
                    isAnyModified = true;
                }
            }

            if (!isAnyModified) {
                logMessage("There were not found templates needed to be modified.");
            }
        } finally {
            templates.close();
        }

    }

    /**
     * Set up proper alias and save bean.
     *
     * @param field DataDictionaryField
     * @param oldAlias String
     * @param newAlias String
     */
    private void processDataField(DataDictionaryField field, String oldAlias, String newAlias) {
        DataFieldConfig fddToProcess = null;


        if (field != null && (fddToProcess = field.getDataFieldConfig()) != null
                && fddToProcess.getDataFieldOid().startsWith("stf")) {
            String tempAlias = fddToProcess.getAlias();
            if (tempAlias.contains(oldAlias)) {
                fddToProcess.setAlias(tempAlias.replaceAll(oldAlias, newAlias));
            }


            if (fddToProcess.isDirty()) {
                logMessage("Updating bean path - "
                        + fddToProcess.getDataFieldOid() + " in table - "
                        + fddToProcess.getDataField().getDataTableOid()
                        + " by changing old alias: "
                        + oldAlias
                        + " to new alias: " + newAlias);

                m_broker.saveBeanForced(fddToProcess);
            }
        } else if (field == null) {
            logMessage("Field with alias " + oldAlias + " was not found.");
        } else if (!fddToProcess.getDataFieldOid().startsWith("stf")) {
            logMessage("Field with alias " + oldAlias + " is not in staff table.");
        }
    }

    /**
     * Check for the existence of the new code in the reference table matching on the state code
     * value.
     * If the value is not found, create the reference code.
     *
     * @param field DataDictionaryField
     * @param code String
     * @param stateCode String
     * @param alias String
     */
    private void processReferenceCode(DataDictionaryField field, String code, String stateCode, String alias) {
        DataFieldConfig fddToGetTable = null;

        if (field != null && (fddToGetTable = field.getDataFieldConfig()) != null) {
            ReferenceTable refTable = fddToGetTable.getReferenceTable();

            if (refTable != null) {
                if (refTable.getCodeMap() != null && !refTable.getCodeMap().keySet().contains(code)) {
                    ReferenceCode newRefCode =
                            X2BaseBean.newInstance(ReferenceCode.class, m_broker.getPersistenceKey());
                    newRefCode.setReferenceTableOid(refTable.getOid());
                    newRefCode.setSequenceNumber(0);
                    newRefCode.setCode(code);
                    newRefCode.setDescription(code);
                    newRefCode.setStateCode(stateCode);
                    newRefCode.setNoHideIndicator(false);
                    newRefCode.setDisabledIndicator(false);

                    m_broker.saveBeanForced(newRefCode);

                    logMessage("Reference Code created: Code = " + code + ", StateCode = " + stateCode
                            + ", ReferenceTableOid = " + refTable.getUserName() + ", Alias for the field = " + alias);

                } else {
                    logMessage("Code: " + code + " was not created in reference table = "
                            + refTable.getUserName()
                            + " referenced to the field with alias = "
                            + alias
                            + " because it is already exist.");
                }
            } else {
                logMessage("There was not found reference table for the field with alias = " + alias);
            }
        } else {
            logMessage("There was not found field with alias = " + alias);
        }

    }

}
