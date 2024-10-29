/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.LinkedList;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for setting up the required user defined field and reference table
 * used in the Civil Rights Data Collection report.
 *
 * @author Follett Software Company
 */
public class CivilRightsDataCollectionPart2Setup extends ProcedureJavaSource {
    /*
     * Transcript field constants
     */
    private static final String FIELD_LONG_NAME = "AP test result";
    private static final String FIELD_SHORT_NAME = "AP test result";
    private static final String FIELD_ALIAS = "AP test result";
    private static final String FIELD_TYPE = "Character";
    private static final int FIELD_LENGTH = 10;
    private static final int FIELD_DECIMAL = 0;
    private static final String FIELD_REFERENCE_TABLE = "AP test result codes";

    /*
     * Reference table constants
     */
    private static final String DOE_REFERENCE_CATEGORY = "DOE";
    private static final String REFERENCE_TABLE_TABLE_OID = "tblRefCode";
    private static final String DETAIL_CONTROL = "Dropdown";
    private static final String[] REF_CODE_CODE = new String[] {"Pass", "Fail", "Not taken"};

    /*
     * Instance variables.
     */
    private String m_currentTableOid;
    private SisOrganization m_district;
    private Collection<ReferenceCode> m_existingCodes;

    /**
     * Public method to use when calling the import without logging into the system. This was
     * designed for the Demo Database Manager to be able to run this import to setup non-MA
     * state-reporting information.
     *
     * @throws Exception exception
     */
    @Override
    public void execute() throws Exception {
        defineApTestResultsField();
        disableExistingCodes();

        for (int i = 0; i < REF_CODE_CODE.length; i++) {
            insertCode(REF_CODE_CODE[i], REF_CODE_CODE[i]);
        }
        updateCodes();

        releaseResources();
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

        m_existingCodes = new LinkedList<ReferenceCode>();

        m_district = (SisOrganization) getOrganization();
        if (m_district == null) {
            m_district = (SisOrganization) OrganizationManager.getRootOrganization(getBroker());
        }
    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Instantiates a general dictionary field in Transcript for AP test results.
     * Finds a user-defined A field and sets it up with the proper information.
     */
    private void defineApTestResultsField() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(FIELD_ALIAS);

        // Field does not exist. Create it.
        if (dictionaryField == null) {
            SubQuery trnDefQuery = new SubQuery(TranscriptColumnDefinition.class,
                    TranscriptColumnDefinition.COL_DATA_FIELD_CONFIG_OID, null);

            Criteria fddCriteria = new Criteria();
            /*
             * fddCriteria.addLike(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER +
             * DataField.REL_DATA_TABLE + PATH_DELIMITER +
             * DataTable.COL_OBJECT_PREFIX,
             * "TRN");
             */
            fddCriteria.addLike(X2BaseBean.COL_OID, "fddX2TRNA%"); // Transcript FieldA only.
            fddCriteria.addNotIn(X2BaseBean.COL_OID, trnDefQuery); // Not in a transcript
                                                                   // definition.
            fddCriteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);
            SubQuery fddQuery = new SubQuery(DataFieldConfig.class, "MAX(FDD_OID)", fddCriteria);

            Criteria criteria = new Criteria();
            // criteria.addSql(AVAILABLE_FIELDA_SQL);
            criteria.addIn(X2BaseBean.COL_OID, fddQuery);

            QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);
            DataFieldConfig field = (DataFieldConfig) getBroker().getBeanByQuery(query);

            if (field != null) {
                logMessage("Assigned '" + FIELD_LONG_NAME + "' to " + field.getDataFieldOid());

                field.setAlias(FIELD_ALIAS);
                field.setUserLongName(FIELD_LONG_NAME);
                field.setUserShortName(FIELD_SHORT_NAME);
                field.setUserType(FIELD_TYPE);
                field.setEnabledIndicator(true);
                field.setListEditIndicator(true);
                field.setUpdateIndicator(true);
                field.setDetailControl(DETAIL_CONTROL);
                field.setUserLength(FIELD_LENGTH);
                field.setUserDecimal(FIELD_DECIMAL);

                /*
                 * Set reference table if one is defined in the input file
                 */
                String referenceTableOid = findReferenceTableOid(FIELD_REFERENCE_TABLE, FIELD_LENGTH);
                field.setReferenceTableOid(referenceTableOid);

                /*
                 * Save field
                 */
                if (field.isDirty()) {
                    getBroker().saveBeanForced(field);
                }

                m_currentTableOid = field.getReferenceTableOid();
                loadExistingReferenceCodes();
            } else {
                logMessage("ERROR: No available field found for '" + FIELD_LONG_NAME + "'.");
            }
        } else {
            logMessage("Field '" + FIELD_LONG_NAME + "' already exists on "
                    + dictionaryField.getDataFieldConfig().getDataFieldOid());
            m_currentTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();
            loadExistingReferenceCodes();
        }
    }

    /**
     * Disables the existing codes in the current reference table. These are the codes that were not
     * touched by the codes in the input file.
     */
    private void disableExistingCodes() {
        for (ReferenceCode existingCode : m_existingCodes) {
            existingCode.setDisabledIndicator(true);
        }
    }

    /**
     * Finds the OID of the reference table that matches the passed name. If none exists, creates
     * a new table.
     *
     * @param tableName String
     * @param codeLength int
     * @return String
     */
    private String findReferenceTableOid(String tableName, int codeLength) {
        ReferenceTable table = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceTable.COL_USER_NAME, tableName);

        QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);
        table = (ReferenceTable) getBroker().getBeanByQuery(query);

        if (table == null) {
            table = X2BaseBean.newInstance(ReferenceTable.class, getBroker().getPersistenceKey());
            table.setCategory(DOE_REFERENCE_CATEGORY);
            table.setCodeLength(codeLength);
            table.setDataTableOid(REFERENCE_TABLE_TABLE_OID);
            table.setOwnerOid(m_district.getOid());
            table.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            table.setUserName(tableName);

            getBroker().saveBeanForced(table);
        }

        return table.getOid();
    }

    /**
     * Checks to see if the current reference code already exists in the table. If not, creates a
     * new code. If so, updates the description.
     *
     * @param recordCode String
     * @param recordDescription String
     */
    private void insertCode(String recordCode, String recordDescription) {
        ReferenceCode code = null;

        // Find if the code exists.
        for (ReferenceCode existingCode : m_existingCodes) {
            if (existingCode.getCode().equals(recordCode)) {
                code = existingCode;
                break;
            }
        }

        if (code == null) {
            // If no code exists, create a new one.
            code = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            code.setReferenceTableOid(m_currentTableOid);
            code.setCode(recordCode);
            code.setDescription(recordDescription);

            m_existingCodes.add(code);
        } else {
            // Update information and enable existing codes.
            code.setDescription(recordDescription);
            code.setDisabledIndicator(false);
        }
    }

    /**
     * Loads the existing codes for the current reference table.
     */
    private void loadExistingReferenceCodes() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, m_currentTableOid);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        m_existingCodes = getBroker().getCollectionByQuery(query);
    }

    /**
     * Update any code in the existing codes list that is dirty.
     */
    private void updateCodes() {
        for (ReferenceCode existingCode : m_existingCodes) {
            if (existingCode.isDirty()) {
                getBroker().saveBeanForced(existingCode);
            }
        }
    }
}
