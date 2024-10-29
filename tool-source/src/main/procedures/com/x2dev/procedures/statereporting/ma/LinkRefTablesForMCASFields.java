/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure that looks at the student schedule and auto creates a MCAS Assessment
 * for those courses that are selected in the procedure input,
 * for the cycle indicated and align to the test code selected in the procedure input.
 *
 * @author Follett Software Company
 *
 */
public class LinkRefTablesForMCASFields extends ProcedureJavaSource {
    private static final String ALIAS_CRS_MCAS_TEST_CODE = "ma-crs-MCASTestCode";
    private static final String ALIAS_MCAS_TESTCODE = "MCASTestCode";
    private static final String ALIAS_MCAS_TESTFORMAT = "MCASTestFormat";

    private static final String ASM_DEF_ID = "MA MCAS";
    private static final String MCAS_REF_NAME_TEST_CODE = "MA MCAS Test Code";
    private static final String MCAS_REF_NAME_TEST_FORMAT = "MA MCAS Test Format";

    /**
     * Class members
     */
    private List<String> m_detailMessages = new LinkedList();
    private Map<String, ReferenceTable> m_refTablesMap;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        linkRefTablesForASD();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        populateRefTablesForMcas();
        linkRefTablesForASD();
        linkRefTablesForCRS();

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);

        for (String detail : m_detailMessages) {
            logMessage(detail);
        }
    }

    /**
     * Find proper ASD by asdId.
     */
    private void linkRefTablesForASD() {
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASM_DEF_ID);

        AssessmentDefinition asd = (AssessmentDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));

        if (asd == null) {
            logMessage("Required assessment definition with ID = " + getParameter(ASM_DEF_ID) + " could not be found");
        } else {
            DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                logMessage("Extended Dictinary for MCAS could not be loaded");
            } else {
                Collection<AssessmentColumnDefinition> acds = asd.getAssessmentColumnDefinitions();

                for (AssessmentColumnDefinition acd : acds) {
                    String acdAlias = acd.getAlias();

                    if (!StringUtils.isEmpty(acdAlias)) {

                        switch (acdAlias) {
                            case ALIAS_MCAS_TESTCODE:
                                if (m_refTablesMap.get(MCAS_REF_NAME_TEST_CODE) != null) {
                                    acd.setReferenceTableOid((m_refTablesMap.get(MCAS_REF_NAME_TEST_CODE).getOid()));
                                    getBroker().saveBeanForced(acd);
                                    logDetail(MCAS_REF_NAME_TEST_CODE +
                                            " was referenced to the Assesment Definition field with alias " +
                                            ALIAS_MCAS_TESTCODE);
                                } else {
                                    logDetail("Reference table with name = " + MCAS_REF_NAME_TEST_CODE
                                            + "was not found!!!");
                                }
                                break;

                            case ALIAS_MCAS_TESTFORMAT:
                                if (m_refTablesMap.get(MCAS_REF_NAME_TEST_FORMAT) != null) {
                                    acd.setReferenceTableOid((m_refTablesMap.get(MCAS_REF_NAME_TEST_FORMAT).getOid()));
                                    getBroker().saveBeanForced(acd);
                                    logDetail(MCAS_REF_NAME_TEST_FORMAT +
                                            " was referenced to the Assesment Definition field with alias " +
                                            ALIAS_MCAS_TESTFORMAT);
                                } else {
                                    logDetail("Reference table with name = " + MCAS_REF_NAME_TEST_FORMAT +
                                            "was not found!!!");
                                }
                                break;

                            default:
                                break;

                        }
                    }
                }
            }
        }
    }

    /**
     * Find proper ASD by asdId.
     */
    private void linkRefTablesForCRS() {
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(ALIAS_CRS_MCAS_TEST_CODE);

        if (field != null) {
            DataFieldConfig ddcField = field.getDataFieldConfig();
            ddcField.setReferenceTableOid(m_refTablesMap.get(MCAS_REF_NAME_TEST_CODE).getOid());
            getBroker().saveBeanForced(ddcField);
            logDetail(MCAS_REF_NAME_TEST_CODE + " was referenced to the CRS field with alias "
                    + ALIAS_CRS_MCAS_TEST_CODE);

        } else {
            logDetail("Field with alias " + ALIAS_CRS_MCAS_TEST_CODE + " wasn't found!!!");
        }

    }

    /**
     * Add detail message that will be appended after other messages.
     *
     * @param key String
     */
    private void logDetail(String key) {
        m_detailMessages.add(key);
    }

    /**
     * Method to populate Reference Tables map keyed on user name.
     */
    private void populateRefTablesForMcas() {
        X2Criteria refTableCriteria = new X2Criteria();
        refTableCriteria.addIn(ReferenceTable.COL_USER_NAME,
                Arrays.asList(new String[] {MCAS_REF_NAME_TEST_CODE, MCAS_REF_NAME_TEST_FORMAT}));

        QueryByCriteria refTableQuery = new QueryByCriteria(ReferenceTable.class, refTableCriteria);

        m_refTablesMap = getBroker().getMapByQuery(refTableQuery, ReferenceTable.COL_USER_NAME, 1024);

    }
}
