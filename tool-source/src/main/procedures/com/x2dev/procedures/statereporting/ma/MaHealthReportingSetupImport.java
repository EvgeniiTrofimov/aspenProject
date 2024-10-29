/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for importing the reference tables used in state reporting. This import expects an input
 * MS Excel (xls) file with 10 columns.
 * <ul>
 * <li>Code for the row type. "F" represents a definition for a field to setup in the Data
 * Dictionary. "C" represents a row for a code to insert. The code is placed in the reference table
 * that was defined for the last "F" row. "D" represents a data dictionary cache refresh. Any value
 * other than "F", "D", or "C" will result in the row being skipped.
 * <li>F: Alias name C: Code
 * <li>F: Database Table name C: Description
 * <li>F: Database field name C: State code
 * <li>F: Long name C: Location
 * <li>F: Short name C: Unused
 * <li>F: User type C: Unused
 * <li>F: User length C: Unused
 * <li>F: User decimals C: Unused
 * <li>F: Reference table name C: Unused
 * </ul>
 * <style type="text/css">
 * table.xls { border: 1px solid black; }
 * table.xls td { border: 1px solid black; }
 * table.xls tr.header { background: #4F81BE; color:white; }
 * table.xls tr.field { background: #DBE5F1; }
 * table.xls tr.code { background: #FFFFFF; }
 * </style>
 * Example:
 * <table class="xls" cellspacing="0" cellpadding="2" border="0">
 * <tr class="header">
 * <td>TYPE (F/C/D)</td>
 * <td>F: Alias</td>
 * <td>F: Database Table</td>
 * <td>F: Database Field (or "UDF")</td>
 * <td>F: Long Name</td>
 * <td>F: Short Name</td>
 * <td>F: User Type</td>
 * <td>F: User Length</td>
 * <td>F: User Decimals</td>
 * <td>F: Reference Table Name</td>
 * </tr>
 * <tr class="header">
 * <td>(Field/Code/Dictionary Refresh)</td>
 * <td>C: Code</td>
 * <td>C: Description</td>
 * <td>C: State Code</td>
 * <td>C: Location</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * </tr>
 * <tr class="field">
 * <td>F</td>
 * <td>report-location</td>
 * <td>REF_CODE</td>
 * <td>UDF</td>
 * <td>Report Location</td>
 * <td>ReportLocation</td>
 * <td>Character</td>
 * <td>50</td>
 * <td>0</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="data dictionary refresh">
 * <td>D</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="field">
 * <td>F</td>
 * <td>&nbsp;</td>
 * <td>HEALTH_LOG</td>
 * <td>HLG_PRIMARY_ACTION_CODE</td>
 * <td>Primary action code</td>
 * <td>ActionCode</td>
 * <td>Character</td>
 * <td>50</td>
 * <td>0</td>
 * <td>Health Visit Action Codes</td>
 * </tr>
 * <tr class="code">
 * <td>C</td>
 * <td>Back T Class</td>
 * <td>Back To Class</td>
 * <td>&nbsp;</td>
 * <td>6.b</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class MaHealthReportingSetupImport extends XlsImportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /*
     * Row type index in the input file.
     */
    private static final int ROW_TYPE_INDEX = 0; // F: Field Def, C: Code Def, D: DD Refresh Def

    /*
     * Field column indices in the input file.
     */
    private static final int FIELD_COLUMN_ALIAS = 1;
    private static final int FIELD_COLUMN_DB_TABLE_NAME = 2;
    private static final int FIELD_COLUMN_DB_FIELD_NAME = 3;
    private static final int FIELD_COLUMN_LONG_NAME = 4;
    private static final int FIELD_COLUMN_SHORT_NAME = 5;
    private static final int FIELD_COLUMN_DATA_TYPE = 6;
    private static final int FIELD_COLUMN_LENGTH = 7;
    private static final int FIELD_COLUMN_DECIMAL = 8;
    private static final int FIELD_COLUMN_REF_TABLE = 9;

    /*
     * Code column indices in the input file.
     */
    private static final int CODE_COLUMN_CODE = 1;
    private static final int CODE_COLUMN_DESCRIPTION = 2;
    private static final int CODE_COLUMN_STATE_CODE = 3;
    private static final int CODE_COLUMN_LOCATION = 4;

    /*
     * Row type constants
     */
    private static final String ROW_TYPE_CODE = "C";
    private static final String ROW_TYPE_DICTIONARY_REFRESH = "D";
    private static final String ROW_TYPE_FIELD = "F";

    /*
     * Reference table constants
     */
    private static final String HEALTH_REFERENCE_CATEGORY = "HEALTH";
    private static final String REPORT_LOCATION_ALIAS = "report-location";
    private static final String REFERENCE_TABLE_TABLE_OID = "tblRefCode";

    /*
     * Message keys
     */
    private static final String MESSAGE_ALIAS_EXISTS_KEY = "message.import.aliasExists";
    private static final String MESSAGE_CANNOT_INSTANT_FIELD_KEY = "message.import.cannotInstantiateField";
    private static final String MESSAGE_INVALID_LENGTH_KEY = "message.import.invalidLengthEntered";
    private static final String MESSAGE_NO_REFERENCE_KEY = "message.import.refTable.notDefined";
    private static final String MESSAGE_NO_TABLE_FOUND_KEY = "message.import.tableNotFound";

    private boolean m_codesEntered;
    private String m_currentTableOid;
    private DataDictionary m_dictionary;
    private SisOrganization m_district;
    private Collection<ReferenceCode> m_existingCodes;

    /**
     * Public method to use when calling the import without logging into the system. This was
     * designed for the Demo Database Manager to be able to run this import to setup non-MA
     * state-reporting information.
     *
     * @param file File
     * @throws Exception exception
     */
    public void importFile(File file) throws Exception {
        initialize();
        super.importData(file);
        releaseResources();
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 10;
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        String rowType = record.get(ROW_TYPE_INDEX);

        if (ROW_TYPE_FIELD.equals(rowType)) {
            if (m_codesEntered) {
                disableExistingCodes();
            }

            m_codesEntered = false;
            defineField(record, lineNumber);
        } else if (ROW_TYPE_CODE.equals(rowType)) {
            if (m_currentTableOid == null) {
                incrementSkipCount();
                logInvalidRecord(lineNumber, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(MESSAGE_NO_REFERENCE_KEY));
            } else {
                m_codesEntered = true;
                insertCode(record);
            }
        } else if (ROW_TYPE_DICTIONARY_REFRESH.equals(rowType)) {
            releaseResources();
        }
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

        m_dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        m_existingCodes = new LinkedList<ReferenceCode>();

        if (getOrganization() != null) {
            m_district = (SisOrganization) getOrganization();
        } else {
            QueryByCriteria query = new QueryByCriteria(SisOrganization.class);
            m_district = (SisOrganization) getBroker().getBeanByQuery(query);
        }
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);
    }

    /**
     * Instantiates field associated with the alias on the passed line. If no such field exists,
     * finds a disabled user-defined field and sets it up with the proper information.
     *
     * @param record List<String>
     * @param lineNumber int
     */
    private void defineField(List<String> record, int lineNumber) {
        DataFieldConfig field = null;

        boolean isUserDefined = false;

        String alias = record.get(FIELD_COLUMN_ALIAS);
        String databaseTable = record.get(FIELD_COLUMN_DB_TABLE_NAME);
        String lengthString = record.get(FIELD_COLUMN_LENGTH);

        int length = 10;

        if (StringUtils.isInteger(lengthString)) {
            length = Integer.parseInt(lengthString);
        } else {
            logInvalidRecord(lineNumber, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                    .getMessage(MESSAGE_INVALID_LENGTH_KEY));
        }

        /*
         * If an alias is present, check the alias to field map
         */
        if (!StringUtils.isEmpty(alias)) {
            DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryFieldByAlias(alias);

            if (dictionaryField != null) {
                field = dictionaryField.getDataFieldConfig();
            }
        }

        /*
         * Next check to see if the name is a database field name
         */
        if (field == null) {
            String databaseFieldName = record.get(FIELD_COLUMN_DB_FIELD_NAME);
            DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryFieldByDatabaseName(databaseFieldName);

            if (dictionaryField != null) {
                field = dictionaryField.getDataFieldConfig();
            }
        }

        /*
         * If field is still null, use a user-defined field
         */
        DataDictionaryTable table = m_dictionary.findDataDictionaryTableByDatabaseName(databaseTable);
        if (field == null) {
            if (table == null) {
                logInvalidRecord(lineNumber, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(MESSAGE_NO_TABLE_FOUND_KEY, databaseTable));
            } else {
                field = DataDictionary.getAvailableField(table.getSystemOid(), length, getBroker());
                isUserDefined = true;
            }
        }

        if (field != null && field.getDataField().getDataTableOid().equals(table.getSystemOid())) {
            field.setAlias(alias);
            field.setUserLongName(record.get(FIELD_COLUMN_LONG_NAME));
            field.setUserShortName(record.get(FIELD_COLUMN_SHORT_NAME));
            field.setEnabledIndicator(true);

            /*
             * Only set these properties on a user-defined field
             */
            if (isUserDefined) {
                field.setUserLength(length);
                field.setUserType(record.get(FIELD_COLUMN_DATA_TYPE));

                int decimal = 0;
                String decimalString = record.get(FIELD_COLUMN_DECIMAL);

                if (StringUtils.isInteger(decimalString)) {
                    decimal = Integer.parseInt(decimalString);
                }

                field.setUserDecimal(decimal);
            }

            /*
             * Set reference table if one is defined in the input file
             */
            String referenceTableName = record.get(FIELD_COLUMN_REF_TABLE);
            if (!StringUtils.isEmpty(referenceTableName) && field.getReferenceTable() == null) {
                String referenceTableOid = findReferenceTableOid(referenceTableName, length);
                field.setReferenceTableOid(referenceTableOid);
            }

            /*
             * Save field
             */
            if (field.isDirty()) {
                getBroker().saveBeanForced(field);
            }

            m_currentTableOid = field.getReferenceTableOid();
            loadExistingReferenceCodes();
        } else {
            if (field != null && !field.getDataField().getDataTableOid().equals(table.getSystemOid())) {
                logInvalidRecord(lineNumber, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(MESSAGE_ALIAS_EXISTS_KEY,
                                alias, field.getDataField().getDataTable().getDatabaseName()));
            } else {
                logInvalidRecord(lineNumber, LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(MESSAGE_CANNOT_INSTANT_FIELD_KEY, alias));
            }
        }
    }

    /**
     * Disables the existing codes in the current reference table. These are the codes that were not
     * touched by the codes in the input file.
     */
    private void disableExistingCodes() {
        for (ReferenceCode existingCode : m_existingCodes) {
            existingCode.setDisabledIndicator(true);

            if (existingCode.isDirty()) {
                getBroker().saveBeanForced(existingCode);
            }
        }

        // Clear the collection so we don't go through it a second time
        m_existingCodes.clear();
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
            table = X2BaseBean.newInstance(SisReferenceTable.class, getBroker().getPersistenceKey());
            table.setCategory(HEALTH_REFERENCE_CATEGORY);
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
     * new code. If so, updates the description and state code.
     *
     * @param record List<String>
     */
    private void insertCode(List<String> record) {
        String recordCode = record.get(CODE_COLUMN_CODE);
        String recordDescription = record.get(CODE_COLUMN_DESCRIPTION);
        String recordStateCode = record.get(CODE_COLUMN_STATE_CODE);
        String reportLocation = record.get(CODE_COLUMN_LOCATION);

        ReferenceCode code = null;

        for (ReferenceCode existingCode : m_existingCodes) {
            if (existingCode.getCode().equals(recordCode)) {
                code = existingCode;
                break;
            }
        }

        if (code == null) {
            // If no code exists, create new one
            code = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            code.setReferenceTableOid(m_currentTableOid);

            code.setCode(recordCode);
            code.setDescription(recordDescription);
            code.setStateCode(recordStateCode);

            // Set the report location
            code.setFieldValueByAlias(REPORT_LOCATION_ALIAS, reportLocation);

            getBroker().saveBeanForced(code);
            incrementInsertCount();
        } else {
            incrementMatchCount();

            // Update information on existing codes
            code.setDescription(recordDescription);
            code.setStateCode(recordStateCode);

            // Set the report location
            code.setFieldValueByAlias(REPORT_LOCATION_ALIAS, reportLocation);

            if (code.isDirty()) {
                getBroker().saveBeanForced(code);
                incrementUpdateCount();
            }

            /*
             * Remove code from list of existing codes
             */
            m_existingCodes.remove(code);
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
}
