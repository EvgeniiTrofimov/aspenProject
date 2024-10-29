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
package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataTable;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.utils.io.file.CSVReader;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.poi37.poifs.filesystem.POIFSFileSystem;
import org.apache.poi37.ss.usermodel.Cell;
import org.apache.poi37.ss.usermodel.Row;
import org.apache.poi37.ss.usermodel.Sheet;
import org.apache.poi37.ss.usermodel.Workbook;
import org.apache.poi37.ss.usermodel.WorkbookFactory;

/**
 * Tool for importing the reference tables used in state reporting. This import expects an input
 * MS Excel (xls) file with 11 columns.
 * <ul>
 * <li>Code for the row type.</li>
 * <li>"F" represents a definition for a field to setup in the Data Dictionary.</li>
 * <li>"C" represents a row for a code to insert.</li>
 * <li>"R" represents a reference table to insert.</li>
 * <li>The code is placed in the reference table that was defined for the last "F" or "R" row.
 * Any value other than "F" or "C" or "R" will result in the row being skipped.</li>
 * </ul>
 * <style type="text/css">
 * table.xls { border: 1px solid black; }
 * table.xls td { border: 1px solid black; }
 * table.xls tr.header { background: #4F81BE; color:white; }
 * table.xls tr.field { background: #D7E4BC; color: #000000; font-weight: bold; }
 * table.xls tr.code { background: #FDE9D9; color: #000000; font-weight: bold; }
 * table.xls tr.reference { background: #FCD5B4; color: #000000; font-style: italic; }
 * </style>
 * Example:
 * <table class="xls" cellspacing="0" cellpadding="2" border="0">
 * <tr class="header">
 * <td>TYPE (F/C)</td>
 * <td>F: Alias</td>
 * <td>F: Database Table</td>
 * <td>F: Database Field (or "UDF")</td>
 * <td>F: Long Name</td>
 * <td>F: Short Name</td>
 * <td>F: User Type</td>
 * <td>F: User Length</td>
 * <td>F: User Decimals</td>
 * <td>F: Reference Table Name</td>
 * <td>F: Reference Display Mode (Dropdown/Picklist)</td>
 * <td>F: Reference Valid Code Required (Y/N)</td>
 * <td>F: Reference Picklist Fields</td>
 * </tr>
 * <tr class="header">
 * <td>(Field/Code)</td>
 * <td>C: Code</td>
 * <td>C: Description</td>
 * <td>C: State Code</td>
 * <td>C: Local Code</td>
 * <td>C: Federal Code</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * <td>C: NOT USED</td>
 * </tr>
 * <tr class="header">
 * <td></td>
 * <td>R: Reference Table OID</td>
 * <td>R: Reference Table Name</td>
 * <td>R: Reference Table Code Length</td>
 * <td>R: Reference Table Code Match column</td>
 * <td>R: Reference Table Code Update column</td>
 * <td>R: Reference Table Code Update Force</td>
 * <td>R: NOT USED</td>
 * <td>R: NOT USED</td>
 * <td>R: NOT USED</td>
 * <td>R: NOT USED</td>
 * <td>R: NOT USED</td>
 * </tr>
 * <tr class="field">
 * <td>F</td>
 * <td>DOE 11</td>
 * <td>STUDENT</td>
 * <td>UDF</td>
 * <td>Reason for reporting</td>
 * <td>DOE 11</td>
 * <td>Character</td>
 * <td>25</td>
 * <td>0</td>
 * <td>Reason Codes</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="code">
 * <td>C</td>
 * <td>School Choice</td>
 * <td>School Choice</td>
 * <td>02</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="code">
 * <td>C</td>
 * <td>Transfer</td>
 * <td>Transfer</td>
 * <td>03</td>
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
 * <td>DOE 12</td>
 * <td>STUDENT</td>
 * <td>UDF</td>
 * <td>Sending School</td>
 * <td>DOE 12</td>
 * <td>Character</td>
 * <td>6</td>
 * <td>0</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="field">
 * <td>F</td>
 * <td>DOE 14</td>
 * <td>STUDENT</td>
 * <td>UDF</td>
 * <td>Receiving School</td>
 * <td>DOE 14</td>
 * <td>Character</td>
 * <td>6</td>
 * <td>0</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * <tr class="reference">
 * <td>R</td>
 * <td>rtbRaceCodes</td>
 * <td>Race Codes</td>
 * <td>25</td>
 * <td>stateCode</td>
 * <td>federalCode</td>
 * <td>True</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * <td>&nbsp;</td>
 * </tr>
 * </table>
 */
public class StateReportingSetupImport extends ToolJavaSource {

    /**
     * A container class for a field defined in the spreadsheet.
     */
    class ModelField {
        String m_alias;
        String m_databaseField;
        String m_databaseTable;
        int m_lineNumber;
        String m_longName;
        String m_referenceDisplayMode;
        String m_referenceTableName;
        boolean m_referenceValidCodeRequired;
        String m_shortName;
        String m_userType;
        String m_userLength;
        String m_userDecimal;
        String m_picklistFields;
        String m_fieldHelp;

        /**
         * A representation of a field.
         *
         * Example: STUDENT > (alias or database field name)
         *
         * @return String
         */
        @Override
        public String toString() {
            String name = m_alias;
            if (StringUtils.isEmpty(name)) {
                name = m_databaseField;
            }
            return m_databaseTable + " > " + name;
        }
    }

    /**
     * A container class for a reference code defined in the spreadsheet.
     */
    class ModelRefCode {
        String m_code;
        String m_dependencyCode;
        String m_description;
        String m_federalCode;
        String m_categoryName;
        String m_isDisabled;
        String m_isCategory;
        int m_lineNumber;
        String m_localCode;
        ReferenceCode m_referenceCode;
        String m_referenceTableOid;
        String m_stateCode;
        Map<String, String> m_aliases = new HashedMap();

        /**
         * A string representation of a reference code. Returns the code name.
         *
         * @return String
         */
        @Override
        public String toString() {
            return m_code;
        }

        /**
         * Gets the aligned column.
         *
         * @param column String
         * @return String
         */
        public String getAlignedColumn(String column) {
            String value = null;
            if (ReferenceCode.COL_CODE.equals(column)) {
                value = m_code;
            } else if (ReferenceCode.COL_FEDERAL_CODE.equals(column)) {
                value = m_federalCode;
            } else if (ReferenceCode.COL_STATE_CODE.equals(column)) {
                value = m_stateCode;
            } else if (ReferenceCode.COL_DESCRIPTION.equals(column)) {
                value = m_description;
            }

            return value;
        }
    }

    /**
     * A container class for a reference table defined in the spreadsheet.
     */
    class ModelRefTable {
        List<ModelRefCode> m_codes;
        Map<String, String> m_map;
        Map<String, Integer> m_codeMatchCount;
        boolean m_force;
        int m_length;
        int m_lineNumber;
        String m_match;
        String m_name;
        String m_oid;
        String m_update;

        /**
         * Returns a string representation of the reference table.
         *
         * @return String
         */
        @Override
        public String toString() {
            return "[" + m_oid + "] " + m_name + " Match: " + m_match + " Update: "
                    + (m_update != null ? m_update : "-");
        }
    }

    /*
     * Index of the fields in the input file. These are listed in sequence order.
     */
    private static final int ROW_TYPE_INDEX = 0;
    // Column 1 - F: Alias, C: Code, R: Reference Table OID
    private static final int COLUMN_1_INDEX = 1;
    // Column 2 - F: Database table name, C: Description, R:Reference Table Name
    private static final int COLUMN_2_INDEX = 2;
    // Column 3 - F: Database field name, C: State code, R:Reference Table Code Length
    private static final int COLUMN_3_INDEX = 3;
    // Column 4 - F: Long name, C: Local code, R: Match column
    private static final int COLUMN_4_INDEX = 4;
    // Column 5 - F: Short name, C: Federal code, R: Update column
    private static final int COLUMN_5_INDEX = 5;
    // Column 6 - F: Data type, C: Category Name, R: Force Update
    private static final int COLUMN_6_INDEX = 6;
    // Column 7 - F: Length, C: Is a category?, R: Unused
    private static final int COLUMN_7_INDEX = 7;
    // Column 8 - F: Decimal, C: Dependency Code, R: Unused
    private static final int COLUMN_8_INDEX = 8;
    // Column 9 - F: Reference table, C: edfi enumeration value,R: Unused
    private static final int COLUMN_9_INDEX = 9;
    // Column 10 - F: Reference Display Mode, C: Disabled Indicator, R:Unused
    private static final int COLUMN_10_INDEX = 10;
    // Column 11 - F: Reference Valid Code Required, C:Unused, R: Unused
    private static final int COLUMN_11_INDEX = 11;
    // Column 12 - F: Field Help, C:Unused, R: Unused
    private static final int COLUMN_12_INDEX = 12;

    private static final int FIELD_COUNT = 13;

    private static final String FIELD_A_PREFIX = "fieldA";
    private static final String FIELD_B_PREFIX = "fieldB";
    private static final String FIELD_C_PREFIX = "fieldC";
    private static final String FIELD_D_PREFIX = "fieldD";

    private static final String PARAM_ADD_FIELDS = "addFields";
    private static final String PARAM_ADD_REFERENCE_CODES = "addReferenceCodes";
    private static final String PARAM_CODES_NOT_FOUND = "keepCodesNotFound";
    private static final String PARAM_COMMIT = "commit";
    private static final String PARAM_MATCH_DISABLED_REF_CODES = "matchDisabled";
    private static final String PARAM_MATCH_ON = "matchOn";
    private static final String PARAM_UPDATE_DISABLED_REF_CODES = "updateDisabled";

    private static final String ROW_TYPE_CODE = "C";
    private static final String ROW_TYPE_FIELD = "F";
    private static final String ROW_TYPE_REF = "R";

    private static final String DOE_REFERENCE_CATEGORY = "DOE";
    private static final String REFERENCE_TABLE_TABLE_OID = "tblRefCode";

    private List<ModelField> m_fields = new LinkedList<ModelField>();
    private Map<String, ModelRefTable> m_tables = new HashMap<String, ModelRefTable>();

    private DataDictionary m_dictionary;
    private Organization m_district;
    private Map<String, List<DataFieldConfig>> m_availableDataFields;
    private String m_matchOn;
    private boolean m_commit;
    private boolean m_matchDisabledRefCodes;
    private boolean m_updateDisabledRefCodes;
    private Map<String, Pair<String, Integer>> m_fieldLengthMap = new HashedMap();
    // we need keep tracking status aliases which importing
    // because some ref_codes can use this alias for insert data and before using this it we need
    // reload datadictionary
    // reload datadictionary cost many resources and we need reload only in case when alias was
    // imported successful.
    private Map<String, Boolean> m_importAliasIndicator = new HashedMap();

    private boolean m_addFields;
    private boolean m_addReferenceCodes;
    private boolean m_keepCodesNotFound;
    private Collection<String> m_createdAlias;

    private int m_matchCount = 0;
    private int m_insertCount = 0;
    private int m_updateCount = 0;
    private int m_skipCount = 0;

    private List<String> m_messages = new LinkedList<String>();
    private String m_currentTableName;

    /**
     * Import the data by reading each line by line in the first worksheet in the excel file.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    protected void importData(File sourceFile) throws Exception {
        InputStream inputStream = null;

        try {
            inputStream = new FileInputStream(sourceFile.getPath());
        } catch (FileNotFoundException fnfe) {
            logRecord(0, "Cannot find " + sourceFile.getAbsolutePath());
        }

        if (inputStream != null) {
            System.out.println(sourceFile.getName());
            if (sourceFile.getName().contains(".xls")) {
                parseExcelDoc(inputStream);
            } else if (sourceFile.getName().contains(".csv")) {
                parseCSV(sourceFile);
            }

        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // Initialize map to store table/alias's created during procedure to detect duplicates
        m_createdAlias = new ArrayList<String>();

        /*
         * "How do match reference codes?"
         * DEFAULT: By state code
         */
        m_matchOn = (String) getParameter(PARAM_MATCH_ON);
        if (m_matchOn == null) {
            m_matchOn = ReferenceCode.COL_STATE_CODE;
        }

        /*
         * Should we match disabled reference codes
         * DEFAULT: no
         */
        m_matchDisabledRefCodes = getParameter(PARAM_MATCH_DISABLED_REF_CODES) == null ? false
                : ((Boolean) getParameter(PARAM_MATCH_DISABLED_REF_CODES)).booleanValue();

        /*
         * Should we update disabled reference codes
         * DEFAULT: no
         */
        m_updateDisabledRefCodes = getParameter(PARAM_UPDATE_DISABLED_REF_CODES) == null ? false
                : ((Boolean) getParameter(PARAM_UPDATE_DISABLED_REF_CODES)).booleanValue();

        /*
         * "Should we keep reference codes not defined in the spreadsheet?"
         * Default: Keep them active
         */
        Boolean codesNotFound = (Boolean) getParameter(PARAM_CODES_NOT_FOUND);
        m_keepCodesNotFound = codesNotFound != null ? codesNotFound.booleanValue() : true;

        /*
         * "Do we add reference codes?"
         * Default: Yes (true)
         */
        Boolean addReferenceCodes = (Boolean) getParameter(PARAM_ADD_REFERENCE_CODES);
        m_addReferenceCodes = addReferenceCodes != null ? addReferenceCodes.booleanValue() : true; // default:
                                                                                                   // add
                                                                                                   // reference
                                                                                                   // codes

        /*
         * "Do we add fields?"
         * Default: Yes (true)
         */
        Boolean addFields = (Boolean) getParameter(PARAM_ADD_FIELDS);
        m_addFields = addFields != null ? addFields.booleanValue() : true; // default: add fields

        /*
         * "Are we committing our changes?"
         * Default: Yes (true)
         */
        Boolean commit = (Boolean) getParameter(PARAM_COMMIT);
        m_commit = commit != null ? commit.booleanValue() : true; // default: commit & review
                                                                  // changes

        /*
         * Retrieve all available fields
         */
        X2Criteria prefixCriteria = new X2Criteria();
        X2Criteria fieldDCriteria = new X2Criteria();
        X2Criteria fieldCCriteria = new X2Criteria();
        X2Criteria fieldBCriteria = new X2Criteria();
        X2Criteria fieldACriteria = new X2Criteria();
        fieldDCriteria.addBeginsWith(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME,
                FIELD_D_PREFIX);
        fieldCCriteria.addBeginsWith(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME,
                FIELD_C_PREFIX);
        fieldBCriteria.addBeginsWith(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME,
                FIELD_B_PREFIX);
        fieldACriteria.addBeginsWith(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME,
                FIELD_A_PREFIX);
        prefixCriteria.addOrCriteria(fieldDCriteria);
        prefixCriteria.addOrCriteria(fieldCCriteria);
        prefixCriteria.addOrCriteria(fieldBCriteria);
        prefixCriteria.addOrCriteria(fieldACriteria);
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(prefixCriteria);
        criteria.addEqualTo(DataFieldConfig.COL_FINAL_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_SYSTEM_USE_ONLY_INDICATOR,
                Boolean.FALSE);
        BeanQuery dataFieldConfigQuery = new BeanQuery(DataFieldConfig.class, criteria);
        dataFieldConfigQuery
                .addOrderByAscending(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_DATABASE_LENGTH);
        dataFieldConfigQuery
                .addOrderByAscending(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME);
        m_availableDataFields = getBroker().getGroupedCollectionByQuery(dataFieldConfigQuery,
                DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER +
                        DataField.REL_DATA_TABLE + PATH_DELIMITER + DataTable.COL_DATABASE_NAME,
                256);

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        if (getOrganization() != null) {
            m_district = getOrganization();
        } else {
            QueryByCriteria query = new QueryByCriteria(Organization.class);
            m_district = (Organization) getBroker().getBeanByQuery(query);
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
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        getJob().getInput().setFormat(ToolInput.HTML_FORMAT);
        importData((File) getParameter(FILE_KEY));
        ThreadUtils.checkInterrupt();
        execute();
        exportResults();
    }

    /**
     * Build the code map and the match count maps.
     *
     * @param refTable ModelRefTable
     * @param code ModelRefCode
     */
    private void buildUpdateMap(ModelRefTable refTable, ModelRefCode code) {
        String key = code.getAlignedColumn(refTable.m_match);
        String value = code.getAlignedColumn(refTable.m_update);

        if (!StringUtils.isEmpty(key)) {
            Integer count = Integer.valueOf(1);
            if (refTable.m_codeMatchCount.containsKey(key)) {
                count = refTable.m_codeMatchCount.get(key);
                count = Integer.valueOf(count.intValue() + 1);
            }
            refTable.m_codeMatchCount.put(key, count);
            refTable.m_map.put(key, value);
        }
    }

    /**
     * Given a Field object, detect if there are any changes and log the results.
     *
     * If the mode is to commit, save (or add) the field in the data dictionary
     *
     * @param field ModelField
     */
    private void defineField(ModelField field) {
        DataFieldConfig ddFieldConfig = null;

        boolean isCore = false;
        boolean isNew = false;
        String fieldFindSource = null;


        /*
         * Let the procedure know that we have already added a field with a particular table and
         * alias
         * so we do not double add during the same procedure. This will skip duplicates in the
         * import file.
         */
        if (m_createdAlias.contains(field.m_alias)) {
            logRecord(field.m_lineNumber,
                    "Field with alias '" + field.m_alias + "' on table '" + field.m_databaseTable +
                            "' has already been added during this import and has been skipped (check your import file for duplicates)");
            m_skipCount++;
        } else {
            // Check if the field's alias already exists
            DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryFieldByAlias(field.m_alias);
            if (dictionaryField != null) {
                ddFieldConfig = dictionaryField.getDataFieldConfig();
                fieldFindSource = "Alias " + field.m_alias;

                if (!ddFieldConfig.getDataField().getJavaName().startsWith("field")) {
                    fieldFindSource = "Field " + field.m_alias;
                    isCore = true;
                    logMessage("Warning: Replacing alias on core field " + field.m_databaseField + " with alias "
                            + field.m_alias);
                }
            }

            // If the data field config was not found, then check if it's using a core field
            if (ddFieldConfig == null) {
                dictionaryField = m_dictionary.findDataDictionaryFieldByDatabaseName(field.m_databaseField);
                if (dictionaryField != null) {
                    ddFieldConfig = dictionaryField.getDataFieldConfig();
                    fieldFindSource = "Field " + field.m_alias;

                    if (!ddFieldConfig.getDataField().getJavaName().startsWith("field")) {
                        isCore = true;
                    }
                }
            }

            // If the data field config hasn't been found, find an available field in the table
            if (ddFieldConfig == null) {
                ddFieldConfig = findAvailableField(field);
                isNew = true;
            }

            if (ddFieldConfig != null) {
                String databaseTable = ddFieldConfig.getDataField().getDataTable().getDatabaseName();
                if (!databaseTable.equals(field.m_databaseTable)) {
                    // The field that was found is on the wrong table!
                    logRecord(field.m_lineNumber, fieldFindSource + " is on the wrong table. Found in " + databaseTable
                            + " and should be in " + field.m_databaseTable);
                    m_skipCount++;
                } else {
                    List<String> changes = new LinkedList<String>();
                    boolean isDirty = false;

                    if (isNew || isCore) {
                        isDirty = true;
                        if (m_commit) {
                            ddFieldConfig.setAlias(field.m_alias);
                            ddFieldConfig.setEnabledIndicator(true);
                            // indicate that for this alias was found appropriate field
                            m_importAliasIndicator.put(field.m_alias, Boolean.valueOf(true));
                        }
                    }

                    if (!StringUtils.isEqual(ddFieldConfig.getUserLongName(), field.m_longName)) {
                        changes.add(ddFieldConfig.getUserLongName() + " (Long name) => " + field.m_longName);
                        isDirty = true;
                        if (isNew && m_commit) // Label new fields only
                        {
                            ddFieldConfig.setUserLongName(field.m_longName);
                        }
                    }

                    if (!StringUtils.isEqual(ddFieldConfig.getUserShortName(), field.m_shortName)) {
                        changes.add(ddFieldConfig.getUserShortName() + " (Short name) => " + field.m_shortName);
                        isDirty = true;
                        if (isNew && m_commit) // Label new fields only
                        {
                            ddFieldConfig.setUserShortName(field.m_shortName);
                        }
                    }

                    if (!StringUtils.isEqual(ddFieldConfig.getUserType(), field.m_userType)) {
                        isDirty = true;
                        if (isCore) {
                            changes.add(ddFieldConfig.getUserType() + " (User type) => Unable to change (core)");
                        } else {
                            changes.add(ddFieldConfig.getUserType() + " (User type) => " + field.m_userType);
                        }
                        if (m_commit && !isCore) // Only UDFs
                        {
                            ddFieldConfig.setUserType(field.m_userType);
                        }
                    }

                    if (ddFieldConfig.getUserLength() != Integer.parseInt(field.m_userLength)) {
                        isDirty = true;
                        if (isCore) {
                            changes.add(ddFieldConfig.getUserLength() + " (User length) => Unable to change (core)");
                        } else {
                            changes.add(ddFieldConfig.getUserLength() + " (User length) => " + field.m_userLength);
                        }
                        if (m_commit && !isCore) // Only UDFs
                        {
                            ddFieldConfig.setUserLength(Integer.parseInt(field.m_userLength));
                        }
                    }

                    if (ddFieldConfig.getUserDecimal() != Integer.parseInt(field.m_userDecimal)) {
                        isDirty = true;
                        if (isCore) {
                            changes.add(ddFieldConfig.getUserDecimal() + " (User decimal) => Unable to change (core)");
                        } else {
                            changes.add(ddFieldConfig.getUserDecimal() + " (User decimal) => " + field.m_userDecimal);
                        }
                        if (m_commit && !isCore) // Only UDFs
                        {
                            ddFieldConfig.setUserDecimal(Integer.parseInt(field.m_userDecimal));
                        }
                    }

                    if (!StringUtils.isEqual(ddFieldConfig.getPicklistFields(), field.m_picklistFields)) {
                        isDirty = true;
                        changes.add(ddFieldConfig.getUserDecimal() + " (Picklist fields) => " + field.m_picklistFields);

                        if (m_commit) {
                            ddFieldConfig.setPicklistFields(field.m_picklistFields);
                        }
                    }

                    if (!StringUtils.isEmpty(field.m_referenceTableName)) {
                        ModelRefTable refTable = m_tables.get(field.m_referenceTableName);
                        if (ddFieldConfig.getReferenceTable() == null
                                || !ddFieldConfig.getReferenceTable().getUserName()
                                        .equalsIgnoreCase(field.m_referenceTableName)) {
                            // findReferenceTable(refTable) will create the table if not found
                            ReferenceTable referenceTable = findReferenceTable(refTable);
                            changes.add("Reference table => " + field.m_referenceTableName);
                            isDirty = true;
                            if (m_commit) {
                                ddFieldConfig.setReferenceTableOid(referenceTable.getOid());
                            }
                        } else {
                            // Use the table attached to the field
                            refTable.m_oid = ddFieldConfig.getReferenceTable().getOid();
                        }
                    }

                    if (!StringUtils.isEqual(ddFieldConfig.getDetailControl(), field.m_referenceDisplayMode)) {
                        changes.add(ddFieldConfig.getDetailControl() + " => " + field.m_referenceDisplayMode);
                        isDirty = true;
                        if (m_commit && !isCore) {
                            ddFieldConfig.setDetailControl(field.m_referenceDisplayMode);
                        }
                    }

                    if (ddFieldConfig.getFieldDescription() != null
                            && !ddFieldConfig.getFieldDescription().equals(field.m_fieldHelp)) {
                        changes.add(ddFieldConfig.getFieldDescription() + " => " + field.m_fieldHelp);
                        isDirty = true;
                        if (m_commit) {
                            ddFieldConfig.setFieldDescription(field.m_fieldHelp);
                        }
                    }

                    if (ddFieldConfig.getValidReferenceOnlyIndicator() != field.m_referenceValidCodeRequired) {
                        changes.add("Valid Reference Only (" + ddFieldConfig.getValidReferenceOnlyIndicator() + ") => "
                                + field.m_referenceValidCodeRequired);
                        isDirty = true;
                        if (m_commit) {
                            ddFieldConfig.setValidReferenceOnlyIndicator(field.m_referenceValidCodeRequired);
                        }
                    }

                    if (isDirty) {
                        if (!StringUtils.isEmpty(field.m_alias)) {
                            m_createdAlias.add(field.m_alias);
                        }
                        if (isNew) {
                            logMessage("Inserted " + field + " : "
                                    + StringUtils.convertCollectionToDelimitedString(changes, ", "));
                            m_insertCount++;
                        } else {
                            logMessage("Updated " + field + " : "
                                    + StringUtils.convertCollectionToDelimitedString(changes, ", "));
                            m_updateCount++;
                        }

                        if (m_commit) {
                            ddFieldConfig.setEnabledIndicator(true);
                            getBroker().saveBeanForced(ddFieldConfig);
                        }
                    }
                }
            } else {
                // still cannot find an available field, something is wrong!
                logRecord(field.m_lineNumber, "Cannot instantiate field '" + field.m_alias
                        + "'!  Unable to find an available field on " + field.m_databaseTable);
                m_skipCount++;
            }
        }
    }

    /**
     * Define references (table and codes).
     *
     * @param refTable ModelRefTable
     */
    private void defineReferences(ModelRefTable refTable) {
        ReferenceTable referenceTable = findReferenceTable(refTable);

        logMessage("Defining Reference Codes for table: " + refTable.toString());

        if (!m_keepCodesNotFound) {
            disableExistingCodes(referenceTable);
        }

        String matchPath = (StringUtils.isEmpty(refTable.m_match)) ? m_matchOn : refTable.m_match;
        Map<String, ReferenceCode> codeMap = getCodeMap(referenceTable, matchPath);
        for (ModelRefCode code : refTable.m_codes) {
            code.m_referenceTableOid = referenceTable.getOid();
            ReferenceCode refCode = matchReferenceCode(code, matchPath, codeMap);
            updateReferenceCode(refTable, code, refCode);
        }
    }

    /**
     * Disables the existing codes in the current reference table. These are the codes that were not
     * touched by the codes in the input file.
     *
     * @param referenceTable ReferenceTable
     */
    private void disableExistingCodes(ReferenceTable referenceTable) {
        for (ReferenceCode existingCode : referenceTable.getReferenceCodes()) {
            logRecord(0, "Disabled code " + existingCode.getCode());

            if (m_commit) {
                existingCode.setDisabledIndicator(true);
                getBroker().saveBeanForced(existingCode);
            }
        }
    }

    /**
     * 1. Validate if the spreadsheet is correct
     * 2. If validation is successful, check for availability
     * 3. Import the codes and/or reference codes
     */
    private void execute() {
        // ------------------- VALIDATION ----------------------
        boolean validationSuccess = true;

        // validate the fields
        for (ModelField field : m_fields) {
            if (!validateField(field)) {
                validationSuccess = false;
            }
        }

        // validate the codes
        for (ModelRefTable refTable : m_tables.values()) {
            for (ModelRefCode refCode : refTable.m_codes) {
                if (!validateCode(refTable, refCode)) {
                    validationSuccess = false;
                }
            }
        }

        // -------------------- CHECK AVAILABILITY --------------------------
        Map<String, List<DataFieldConfig>> availableFields = new HashMap<String, List<DataFieldConfig>>();
        for (ModelField field : m_fields) {
            if (!availableFields.containsKey(field.m_databaseTable)) {
                availableFields.put(field.m_databaseTable, new LinkedList<DataFieldConfig>());
            }

        }

        // import the rows
        if (validationSuccess && m_addFields) {
            for (ModelField field : m_fields) {
                defineField(field);
            }
        }

        // import the codes
        if (validationSuccess && m_addReferenceCodes) {
            for (ModelRefTable refTable : m_tables.values()) {
                defineReferences(refTable);
            }
        }
    }

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        String sourceFileName = ((File) getParameter(FILE_KEY)).getName();
        buffer.append((m_commit ? "Commit Mode." : "Review Mode.") + '\n');
        buffer.append('\n');
        buffer.append("  Results" + '\n');
        buffer.append("------------------------------------------------\n");
        buffer.append("   File name: " + sourceFileName + '\n');
        buffer.append("   Match count: " + m_matchCount + '\n');
        buffer.append("   Update count: " + m_updateCount + '\n');
        buffer.append("   Insert count: " + m_insertCount + '\n');
        buffer.append("   Skipped count: " + m_skipCount + '\n');
        buffer.append("------------------------------------------------\n");

        buffer.append(StringUtils.convertCollectionToDelimitedString(m_messages, "\n"));

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Find the best availability for the given Field.
     *
     * @param field - Field to find availability
     * @return DataFieldConfig - the data field config object best fit for the field
     */
    private DataFieldConfig findAvailableField(ModelField field) {
        DataFieldConfig availableField = null;
        List<DataFieldConfig> ddConfigs = m_availableDataFields.get(field.m_databaseTable);
        if (ddConfigs != null) {
            for (Iterator<DataFieldConfig> it = ddConfigs.iterator(); it.hasNext();) {
                DataFieldConfig ddConfig = it.next();

                if (!field.m_userType.equalsIgnoreCase("Text") &&
                        Integer.parseInt(field.m_userLength) <= ddConfig.getDataField().getDatabaseLength() &&
                        !ddConfig.getDataField().getDatabaseType().equalsIgnoreCase("X")) {
                    availableField = ddConfig;
                    ddConfigs.remove(ddConfig);
                    break;
                } else if (field.m_userType.equalsIgnoreCase("Text") &&
                        ddConfig.getDataField().getDatabaseType().equals("X")) {
                    availableField = ddConfig;
                    ddConfigs.remove(ddConfig);
                    break;
                }
            }
            // determine if it is multiple value field (with ref table, type text and length > 50),
            // try use C field if D is not available
            if (availableField == null && field.m_userType.equalsIgnoreCase("Text")
                    && Integer.parseInt(field.m_userLength) > 50 && field.m_referenceTableName != null) {
                for (Iterator<DataFieldConfig> it = ddConfigs.iterator(); it.hasNext();) {
                    DataFieldConfig ddConfig = it.next();

                    if (ddConfig.getDataField().getDatabaseLength() == 50) {
                        availableField = ddConfig;
                        break;
                    }
                }
            }
        }

        return availableField;
    }

    /**
     * Find a reference table on oid, name, or new one in this order.
     *
     * @param refTable - Reference table to add
     *
     * @return ReferenceTable - the reference that was found by oid or name, or a new one if it
     *         wasn't found
     */
    private ReferenceTable findReferenceTable(ModelRefTable refTable) {
        ReferenceTable table = null;

        if (!StringUtils.isEmpty(refTable.m_oid)) // find the table by oid
        {
            table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTable.m_oid);
        }

        if (table == null) // if oid search failed, try finding by name
        {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTable.m_name);
            QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);
            table = (ReferenceTable) getBroker().getBeanByQuery(query);
        }

        if (table == null) // if finding by name failed, create reference table
        {
            table = X2BaseBean.newInstance(ReferenceTable.class, getBroker().getPersistenceKey());
            table.setCategory(DOE_REFERENCE_CATEGORY);
            table.setCodeLength(refTable.m_length);
            table.setDataTableOid(REFERENCE_TABLE_TABLE_OID);
            table.setOwnerOid(m_district.getOid());
            table.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            table.setUserName(refTable.m_name);

            logMessage("Inserted reference table " + table.getUserName());

            if (m_commit) {
                getBroker().saveBeanForced(table);
            }
        }

        refTable.m_oid = table.getOid();
        return table;
    }

    /**
     * Given the reference table, get the existing codes for it
     * Will only search based on what was defined as the match path or if empty
     * in "matchOn" (federal, state or code name).
     *
     * @param refTable ReferenceTable
     * @param codeBeanPath Defined as the match path in the spreadsheet per table
     * @return Map<String, ReferenceCode> - a map of the codes by &lt;matchOn, ReferenceCode&gt;
     */
    private Map<String, ReferenceCode> getCodeMap(ReferenceTable refTable, String codeBeanPath) {
        Criteria criteria = ReferenceManager.getCodesCriteria(refTable.getOid(), getOwnableCriteria(),
                !m_matchDisabledRefCodes, true, false, getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        Map<String, ReferenceCode> map = new HashMap<String, ReferenceCode>();
        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                String key = (String) code.getFieldValueByBeanPath(codeBeanPath);
                if (!StringUtils.isEmpty(key)) {
                    map.put(key, code);
                }
            }
        } finally {
            iterator.close();
        }

        return map;
    }

    /**
     * Gets the data dictionary field.
     *
     * @param className String
     * @param path String
     * @return Data dictionary field
     */
    private DataDictionaryField getDataDictionaryField(String className, String path) {
        ModelProperty prop = new ModelProperty(className, path, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * Gets the separator char.
     *
     * @param inputStream InputStream
     * @return char
     * @throws Exception exception
     */
    private char getSeparatorChar(InputStream inputStream) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(inputStream));
        String line = null;
        for (int i = 0; i < 4; ++i) {
            line = br.readLine();
        }

        br.close();

        if (line == null) {
            throw new Exception("Can't get separator char from input steam");
        }

        return line.charAt(1);
    }

    /**
     * Log a message.
     *
     * @param message - the message to be logged
     */
    private void logMessage(String message) {
        m_messages.add(message);
    }

    /**
     * Log a message with a line number.
     *
     * @param lineNumber int
     * @param message String
     */
    private void logRecord(int lineNumber, String message) {
        m_messages.add("Line " + lineNumber + ": " + message);
    }

    /**
     * Attempt to match the code to a reference code in the database.
     *
     * @param code ModelRefCode
     * @param matchOn String
     * @param codeMap Map<String,ReferenceCode>
     * @return A matched <code>ReferenceCode</code>, else instantiate a new bean.
     */
    private ReferenceCode matchReferenceCode(ModelRefCode code, String matchOn, Map<String, ReferenceCode> codeMap) {
        ReferenceCode refCode = null;

        if (ReferenceCode.COL_CODE.equals(matchOn) &&
                codeMap.containsKey(code.m_code)) {
            refCode = codeMap.get(code.m_code);
            m_matchCount++;
        } else if (ReferenceCode.COL_FEDERAL_CODE.equals(matchOn) &&
                codeMap.containsKey(code.m_federalCode)) {
            refCode = codeMap.get(code.m_federalCode);
            m_matchCount++;
        } else if (ReferenceCode.COL_STATE_CODE.equals(matchOn) &&
                codeMap.containsKey(code.m_stateCode)) {
            refCode = codeMap.get(code.m_stateCode);
            m_matchCount++;
        }

        if (refCode == null) {
            refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());

            refCode.setReferenceTableOid(code.m_referenceTableOid);
            refCode.setOwnerOid(m_district.getOid());
            refCode.setOwnerType(m_district.getOrganizationDefinition().getOwnerType());
        }

        return refCode;
    }

    /**
     * Parses a csv for input.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    private void parseCSV(File sourceFile) throws Exception {
        InputStream inputStream = new FileInputStream(sourceFile.getPath());
        try {
            char separatorChar = getSeparatorChar(new FileInputStream(sourceFile.getPath()));
            String[] line;
            int lineNumber = 0;
            CSVReader reader = new CSVReader(new InputStreamReader(inputStream));
            reader.setSeparator(separatorChar);
            while ((line = reader.parseLine()) != null) {
                if (lineNumber >= 3) {
                    List<String> record = new ArrayList<String>(FIELD_COUNT);

                    for (String cell : line) {
                        if (!StringUtils.isEmpty(cell)) {
                            if (cell.charAt(0) == '"') {
                                cell = cell.substring(1);
                            } else if (cell.charAt(cell.length() - 1) == '"') {
                                cell = cell.substring(0, cell.length() - 1);
                            }
                            record.add(cell);
                        } else {
                            record.add("");
                        }
                    }
                    prepareRow(lineNumber, record);
                }
                lineNumber++;
            }
        } finally {
            inputStream.close();
        }
    }

    /**
     * Uses Apache POI to parse the excel alias spreadsheet.
     *
     * @param inputStream InputStream
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void parseExcelDoc(InputStream inputStream) throws IOException {
        try {
            POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
            Workbook workbook = WorkbookFactory.create(fileSystem);
            Sheet sheet = workbook.getSheetAt(0);

            int lineNumber = 3; // May have to change this to third line
            while (lineNumber <= sheet.getLastRowNum()) {

                List<String> record = new ArrayList<String>(FIELD_COUNT);
                Row row = sheet.getRow(lineNumber);
                for (short i = 0; i <= FIELD_COUNT; i++) {
                    Cell cell;
                    try {
                        cell = row.getCell(i);
                    } catch (Exception e) {
                        throw new IllegalStateException(
                                "Cell from line " + (lineNumber + 1) + " at position " + (i + 1) + " cannot be loaded");
                    }
                    if (cell != null) {
                        switch (cell.getCellType()) {
                            case Cell.CELL_TYPE_NUMERIC:
                                String value = new BigDecimal(cell.getNumericCellValue()).toPlainString();
                                record.add(value);
                                break;
                            case Cell.CELL_TYPE_STRING:
                                record.add(cell.getRichStringCellValue().getString());
                                break;
                            default:
                                record.add("");
                                break;
                        }
                    } else {
                        record.add("");
                    }
                }

                prepareRow(lineNumber, record);
                lineNumber++;
            }
        } finally {
            inputStream.close();
        }
    }

    /**
     * Stores a record into either a Row, Code, or Reference object.
     *
     * @param lineNumber - the line number from the spreadsheet the row occured
     * @param record - the columns in that record
     */
    private void prepareRow(int lineNumber, List<String> record) {
        String rowType = record.get(ROW_TYPE_INDEX);

        if (ROW_TYPE_FIELD.equals(rowType)) {// type: "F"
            ModelField field = new ModelField();
            field.m_lineNumber = lineNumber;
            field.m_alias = record.get(COLUMN_1_INDEX).trim();
            field.m_databaseTable = record.get(COLUMN_2_INDEX).trim();
            field.m_databaseField = record.get(COLUMN_3_INDEX).trim();
            field.m_longName = record.get(COLUMN_4_INDEX).trim();
            field.m_shortName = record.get(COLUMN_5_INDEX).trim();
            field.m_userType = record.get(COLUMN_6_INDEX).trim();
            field.m_userLength = record.get(COLUMN_7_INDEX).trim();
            field.m_userDecimal = record.get(COLUMN_8_INDEX).trim();
            field.m_referenceTableName = record.get(COLUMN_9_INDEX).trim();
            field.m_referenceDisplayMode = record.get(COLUMN_10_INDEX).trim();
            field.m_referenceValidCodeRequired = StringUtils.isEmpty(record.get(COLUMN_11_INDEX)) ? false
                    : Boolean.valueOf(record.get(COLUMN_11_INDEX).trim()).booleanValue();
            if (record.size() > COLUMN_12_INDEX) {
                field.m_fieldHelp = record.get(COLUMN_12_INDEX).trim();
            }
            // RDM - Commented out for now, it looks like the alias spreadsheet changed over time,
            // leaving this code commented, in case it needs to be added back in
            // field.m_picklistFields = record.get(COLUMN_10_INDEX).trim();

            // if (!StringUtils.isEmpty(field.m_referenceTableName))
            // {
            // field.m_referenceDisplayMode = record.size() > COLUMN_10_INDEX ?
            // record.get(COLUMN_10_INDEX).trim() : "Dropdown";
            // field.m_referenceValidCodeRequired = record.size() > COLUMN_11_INDEX ?
            // convertStringToBoolean(record.get(COLUMN_11_INDEX).trim()) : false;
            // }
            m_fields.add(field);

            // add reference table, set current reference table name
            if (!StringUtils.isEmpty(field.m_referenceTableName) && !m_tables.containsKey(field.m_referenceTableName)) {
                ModelRefTable refTable = new ModelRefTable();
                refTable.m_codes = new LinkedList<ModelRefCode>();
                refTable.m_length = Integer.parseInt(record.get(COLUMN_7_INDEX));
                refTable.m_name = field.m_referenceTableName;
                refTable.m_map = new HashMap<String, String>();
                refTable.m_codeMatchCount = new HashMap<String, Integer>();
                refTable.m_match = m_matchOn;
                m_tables.put(field.m_referenceTableName, refTable);
            }
            m_currentTableName = record.get(COLUMN_9_INDEX);
        } else if (ROW_TYPE_REF.equals(rowType)) {// type: "R"
            String refOid = record.get(COLUMN_1_INDEX).trim();
            String refTableName = record.get(COLUMN_2_INDEX).trim();
            String refTableCodeLength = record.get(COLUMN_3_INDEX).trim();
            String refTableMatchColumn = record.get(COLUMN_4_INDEX).trim();
            String refTableUpdateColumn = record.get(COLUMN_5_INDEX).trim();
            boolean refTableUpdateForced = Boolean.parseBoolean(record.get(COLUMN_6_INDEX).trim());
            if (!m_tables.containsKey(refTableName)) {
                ModelRefTable refTable = new ModelRefTable();
                refTable.m_lineNumber = lineNumber;
                refTable.m_codes = new LinkedList<ModelRefCode>();
                refTable.m_map = new HashMap<String, String>();
                refTable.m_codeMatchCount = new HashMap<String, Integer>();
                m_tables.put(refTableName, refTable);
            }
            m_tables.get(refTableName).m_oid = refOid;
            m_tables.get(refTableName).m_name = refTableName;
            m_tables.get(refTableName).m_length = Integer.parseInt(refTableCodeLength);
            if (!StringUtils.isEmpty(refTableMatchColumn)) {
                m_tables.get(refTableName).m_match = refTableMatchColumn;
            }
            if (!StringUtils.isEmpty(refTableUpdateColumn)) {
                m_tables.get(refTableName).m_update = refTableUpdateColumn;
            }
            m_tables.get(refTableName).m_force = refTableUpdateForced;
            m_currentTableName = record.get(COLUMN_2_INDEX);
        } else if (ROW_TYPE_CODE.equals(rowType)) { // type: "C"
            ModelRefTable refTable = m_tables.get(m_currentTableName);
            ModelRefCode code = new ModelRefCode();
            code.m_lineNumber = lineNumber;
            code.m_code = record.get(COLUMN_1_INDEX).trim();
            code.m_description = record.get(COLUMN_2_INDEX).trim();
            code.m_stateCode = record.get(COLUMN_3_INDEX).trim();
            code.m_localCode = record.get(COLUMN_4_INDEX).trim();
            code.m_federalCode = record.get(COLUMN_5_INDEX).trim();
            code.m_categoryName = record.get(COLUMN_6_INDEX).trim();
            code.m_isCategory = record.get(COLUMN_7_INDEX).trim();
            code.m_dependencyCode = record.get(COLUMN_8_INDEX).trim();
            String edfiAliasValue = record.get(COLUMN_9_INDEX).trim();
            if (!StringUtils.isEmpty(edfiAliasValue)) {
                code.m_aliases.put("all-rcd-EdFiEnumeration", edfiAliasValue);
            }
            code.m_isDisabled = record.get(COLUMN_10_INDEX).trim();
            refTable.m_codes.add(code);
            // Build a map to match the codes in Aspen
            buildUpdateMap(refTable, code);
        }
    }

    /**
     * Validate alias field length.
     *
     * @param alias String
     * @param value String
     * @return String
     */
    private String validateAliasFieldLength(String alias, String value) {
        String message = null;
        if (!StringUtils.isEmpty(value)) {

            DataDictionaryField ddField = m_dictionary.findDataDictionaryFieldByAlias(alias);
            if (ddField != null) {
                // check it
                message = validateFieldLength(ddField.getTable().getClassName(), ddField.getJavaName(), value, 0);
            } else if (m_importAliasIndicator.containsKey(alias)) {
                int userFieldLength = 0;
                String userType = "";
                for (ModelField field : m_fields) {
                    if (!StringUtils.isEmpty(field.m_alias) && field.m_alias.equals(alias)) {
                        String length = field.m_userLength;
                        userType = field.m_userType;
                        if (!StringUtils.isEmpty(length)) {
                            try {
                                userFieldLength = Integer.parseInt(length);
                            } catch (Exception e) {
                                // nothing to do
                            }
                        }
                        break;
                    }
                }
                if (userFieldLength == 0) {
                    if (userType.equals("Date") || userType.equals("Time")) {
                        userFieldLength = 10;
                    } else if (userType.equals("Logical")) {
                        userFieldLength = 1;
                    }
                }

                if (!(userFieldLength == 0 && userType.equals("Text")) && value.length() > userFieldLength) {
                    message = "ERROR: " + alias + " cannot exceed " + userFieldLength + " characters;";
                }

            }
        }
        return message;
    }

    /**
     * Validate a reference code.
     *
     * @param refTable - the reference table the code belongs in
     * @param refCode - the reference code to be validated
     * @return boolean - true if successfully validated, false otherwise
     */
    private boolean validateCode(ModelRefTable refTable, ModelRefCode refCode) {
        int lineNumber = refCode.m_lineNumber;
        boolean result = true;

        // validate length of code
        String code = refCode.m_code;
        if (code.length() > refTable.m_length || code.length() > 50) {
            logRecord(lineNumber, "ERROR: Code " + code + " exceeds its reference table (" + refTable.m_name
                    + ")'s character length " + refTable.m_length + "!");
            result = false;
        }

        // validate length of description
        String description = refCode.m_description;
        if (description.length() > 100) {
            logRecord(lineNumber, "ERROR: Code description cannot exceed 100 characters!");
            result = false;
        }

        // validate length of state code
        String stateCode = refCode.m_stateCode;
        if (stateCode.length() > 20) {
            logRecord(lineNumber, "ERROR: State code cannot exceed 20 characters!");
            result = false;
        }

        // validate length of local code
        String localCode = refCode.m_localCode;
        if (localCode.length() > 20) {
            logRecord(lineNumber, "ERROR: Local code cannot exceed 20 characters!");
            result = false;
        }

        // validate length of federal code
        String federalCode = refCode.m_federalCode;
        if (federalCode.length() > 20) {
            logRecord(lineNumber, "ERROR: Federal code cannot exceed 20 characters!");
            result = false;
        }

        // validate length of dependency code
        String dependencyCode = refCode.m_dependencyCode;
        if (dependencyCode.length() > 50) {
            logRecord(lineNumber, "ERROR: Dependency code cannot exceed 50 characters!");
            result = false;
        }

        // Validate match column
        String matchColumn = refTable.m_match;
        if (!StringUtils.isEmpty(matchColumn)) {
            if (!matchColumn.equals(ReferenceCode.COL_CODE) &&
                    !matchColumn.equals(ReferenceCode.COL_FEDERAL_CODE) &&
                    !matchColumn.equals(ReferenceCode.COL_STATE_CODE) &&
                    !matchColumn.equals(ReferenceCode.COL_DESCRIPTION)) {
                logRecord(lineNumber, "ERROR: Match column is invalid!");
                result = false;
            }
        }

        // Validate update column
        String updateColumn = refTable.m_match;
        if (!StringUtils.isEmpty(updateColumn)) {
            if (!updateColumn.equals(ReferenceCode.COL_CODE) &&
                    !updateColumn.equals(ReferenceCode.COL_FEDERAL_CODE) &&
                    !updateColumn.equals(ReferenceCode.COL_STATE_CODE) &&
                    !updateColumn.equals(ReferenceCode.COL_DESCRIPTION)) {
                logRecord(lineNumber, "ERROR: Update column is invalid!");
                result = false;
            }
        }

        // validate that category is logic
        String isCatefory = refCode.m_isCategory;
        if (!StringUtils.isEmpty(isCatefory)) {
            if (!(BooleanAsStringConverter.TRUE.equals(isCatefory) ||
                    BooleanAsStringConverter.FALSE.equals(isCatefory))) {
                logRecord(lineNumber, "ERROR: Invalid format for \'Is a category?\' field! Should be \'1\' or \'0\';");
                result = false;
            }
        }

        // validate that Active/Inactive is logic
        String isDisabled = refCode.m_isDisabled;
        if (!StringUtils.isEmpty(isDisabled)) {
            if (!(BooleanAsStringConverter.TRUE.equals(isDisabled) ||
                    BooleanAsStringConverter.FALSE.equals(isDisabled))) {
                logRecord(lineNumber, "ERROR: Invalid format for \'Disabled?\' field! Should be \'1\' or \'0\';");
                result = false;
            }
        }

        // validate that aliases exist in datadictionary or they are importing now
        for (Entry<String, String> entry : refCode.m_aliases.entrySet()) {
            String alias = entry.getKey();
            String value = entry.getValue();
            if (!StringUtils.isEmpty(value)
                    && !(m_importAliasIndicator.containsKey(alias) || m_dictionary.containsAlias(alias))) {
                logRecord(lineNumber, "ERROR: Cannot find \'" + alias + "\' alias;");
                result = false;
            }
        }

        // validate field length aliases
        for (Entry<String, String> entry : refCode.m_aliases.entrySet()) {
            String message = validateAliasFieldLength(entry.getKey(), entry.getValue());
            if (!StringUtils.isEmpty(message)) {
                logRecord(lineNumber, message);
                result = false;
            }
        }

        // validate field length for category name
        String message = validateFieldLength(ReferenceCode.class.getName(), ReferenceCode.COL_CATEGORY,
                refCode.m_categoryName, 0);
        if (!StringUtils.isEmpty(message)) {
            logRecord(lineNumber, message);
            result = false;
        }

        return result;
    }

    /**
     * Validate a field.
     *
     * @param field ModelField
     * @return boolean
     */
    private boolean validateField(ModelField field) {
        int lineNumber = field.m_lineNumber;
        boolean result = true;

        // 1. validate alias (length must be <= 30)
        // rules was changed and and alias can has length more than 30
        /*
         * if (field.m_alias.length() > 30)
         * {
         * logRecord(lineNumber, "Alias character length cannot be greater than 30!");
         * result = false;
         * }
         */

        // 2. validate table (does it exist?)
        DataDictionaryTable table = m_dictionary.findDataDictionaryTableByDatabaseName(field.m_databaseTable);
        if (table == null) {
            logRecord(lineNumber, "Cannot find table " + field.m_databaseTable + "!");
            result = false;
        }

        // 3. validate long name (length must be <= 50)
        if (field.m_longName.length() > 50) {
            logRecord(lineNumber, "Long name cannot be greater than 50!");
            result = false;
        }

        // 4. validate short name (length must be <= 50)
        if (field.m_shortName.length() > 50) {
            logRecord(lineNumber, "Short name cannot be greater than 50!");
            result = false;
        }

        // 5. validate type (must equal Character, Logical, Text, Date, Time, Timestamp, Integer or
        // Number)
        if (!field.m_userType.matches("Character|Logical|Text|Date|Time|Timestamp|Integer|Number")) {
            logRecord(lineNumber, "Invalid user type " + field.m_userType);
            result = false;
        }

        // 6. validate user length
        if (!StringUtils.isNumeric(field.m_userLength))// must be numeric
        {
            logRecord(lineNumber, "User length is not numeric!");
            result = false;
        } else if (field.m_userType.equals("Character")) // if type is Character, must be <= 50
        {
            int length = (int) Double.parseDouble(field.m_userLength);
            if (length > 50) {
                logRecord(lineNumber, "User length cannot be greater than 50 if type is Character!");
                result = false;
            }
        } else if (field.m_userType.equals("Logical")) // if type is logical, must be <= 10
        {
            int length = (int) Double.parseDouble(field.m_userLength);
            if (length > 10) {
                logRecord(lineNumber, "User length cannot be greater than 10 if type is Logical!");
                result = false;
            }
        }

        // 7. validate decimal (must be numeric)
        if (!StringUtils.isNumeric(field.m_userDecimal)) {
            logRecord(lineNumber, "Decimal is not numeric!");
            result = false;
        }

        // 8. validate reference table name (length must be <= 100)
        if (field.m_referenceTableName.length() > 100) {
            logRecord(lineNumber, "Reference table name cannot be greater than 100!");
            result = false;
        }

        // 9. validate display mode (1) cannot be empty if there's a reference table, otherwise (2)
        // must match Dropdown or Picklist
        if (!StringUtils.isEmpty(field.m_referenceTableName) &&
                (!StringUtils.isEmpty(field.m_referenceDisplayMode) &&
                        !field.m_referenceDisplayMode.matches("Dropdown|Picklist"))) {
            logRecord(lineNumber, "Reference display mode must be either 'Dropdown' or 'Picklist'!");
            result = false;
        }

        // 10. Validate picklist fields (length must be <= 60)
        if (field.m_picklistFields != null && field.m_picklistFields.length() > 60) {
            logRecord(lineNumber, "Picklist fields cannot be greater than 60 characters!");
            result = false;
        }

        // if validate success - indicate that alias is importing now
        if (result) {
            m_importAliasIndicator.put(field.m_alias, null);
        }

        return result;
    }

    /**
     * Validate field length.
     *
     * @param className String
     * @param path String
     * @param value String
     * @param customLimit int
     * @return String
     */
    private String validateFieldLength(String className, String path, String value, int customLimit) {
        String message = null;
        int fieldLength = customLimit;
        String fieldName = null;
        String key = className + path;
        Pair<String, Integer> nameLengthPair = m_fieldLengthMap.get(key);
        if (nameLengthPair == null) {
            DataDictionaryField dataDictionaryField = getDataDictionaryField(className, path);
            DataFieldConfig dataFieldConfig = dataDictionaryField.getDataFieldConfig();
            fieldName = dataFieldConfig.getAlias();
            if (StringUtils.isEmpty(fieldName)) {
                fieldName = dataFieldConfig.getUserShortName();
                fieldName = StringUtils.isEmpty(fieldName) ? dataDictionaryField.getDatabaseName() : fieldName;
            }
            int dbfieldLength = dataDictionaryField.getDataFieldConfig().getUserLength();
            dbfieldLength = dbfieldLength == 0 ? dataDictionaryField.getDatabaseLength() : dbfieldLength;
            nameLengthPair = Pair.of(fieldName, Integer.valueOf(dbfieldLength));
            m_fieldLengthMap.put(key, nameLengthPair);
        }
        fieldName = nameLengthPair.getLeft();
        fieldLength = fieldLength == 0 ? nameLengthPair.getRight().intValue() : fieldLength;

        if (!StringUtils.isEmpty(value) && value.length() > fieldLength) {
            message = "ERROR: " + fieldName + " cannot exceed " + fieldLength + " characters;";
        }
        return message;
    }

    /**
     * Update the <code>ReferenceCode</code> and log the changes.
     *
     * @param modelRefTable ModelRefTable
     * @param modelRefCode ModelRefCode
     * @param refCode ReferenceCode
     */
    private void updateReferenceCode(ModelRefTable modelRefTable, ModelRefCode modelRefCode, ReferenceCode refCode) {
        String matchPath = (StringUtils.isEmpty(modelRefTable.m_match)) ? m_matchOn : modelRefTable.m_match;
        String updatePath = modelRefTable.m_update;
        List<String> changes = new LinkedList<String>();
        boolean isDirty = false;
        boolean skipRecord = false;
        boolean isNew = StringUtils.isEmpty(refCode.getOid());
        if (isNew) {
            changes.add("(Code) => " + modelRefCode.m_code);
            changes.add("(Description) => " + modelRefCode.m_description);
            changes.add("(State code) => " + modelRefCode.m_stateCode);
            changes.add("(Local code) => " + modelRefCode.m_localCode);
            changes.add("(Federal code) => " + modelRefCode.m_federalCode);

            changes.add("(Category) => " + modelRefCode.m_categoryName);
            String isCategory = StringUtils.isEmpty(modelRefCode.m_isCategory) ? Boolean.FALSE.toString()
                    : BooleanAsStringConverter.TRUE.equals(modelRefCode.m_isCategory) ? Boolean.TRUE.toString()
                            : Boolean.FALSE.toString();
            changes.add("(Category indicator) => " + isCategory);
            changes.add("(Dependency code) => " + modelRefCode.m_dependencyCode);
            String isDisabled = StringUtils.isEmpty(modelRefCode.m_isDisabled) ? Boolean.FALSE.toString()
                    : BooleanAsStringConverter.TRUE.equals(modelRefCode.m_isDisabled) ? Boolean.TRUE.toString()
                            : Boolean.FALSE.toString();
            changes.add("(Disabled indicator) => " + isDisabled);

            for (Entry<String, String> entry : modelRefCode.m_aliases.entrySet()) {

                Boolean aliasInitilizeIndicator = m_importAliasIndicator.get(entry.getKey());
                String oldValue = null;
                // try find alias in current datadictionary
                if (m_dictionary.containsAlias(entry.getKey())) {
                    oldValue = (String) refCode.getFieldValueByAlias(entry.getKey());
                }
                // if hasn't it mean that alias importing now, check it using
                // aliasInitilizeIndicator
                else if (aliasInitilizeIndicator != null && aliasInitilizeIndicator.booleanValue()) {
                    // if alias was imported we need reload datadictionary
                    DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
                    m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    if (m_dictionary.containsAlias(entry.getKey())) {
                        oldValue = (String) refCode.getFieldValueByAlias(entry.getKey());
                    }
                    // if after reload alias is not found report about it and skip record
                    else {
                        logRecord(modelRefCode.m_lineNumber, "Cannot instantiate \'" + modelRefCode.m_code
                                + "\' reference code. Because \'" + entry.getKey() + "\' alias wasn't found;");
                        if (!skipRecord) {
                            m_skipCount++;
                        }
                        skipRecord = true;
                    }
                }
                // if alias dosn't exist in dictionary and it wasn't import - report about it and
                // skip record
                // this situation can appear in case when wasn't found physical field for alias
                else {
                    logRecord(modelRefCode.m_lineNumber, "Cannot instantiate \'" + modelRefCode.m_code
                            + "\' reference code. Because \'" + entry.getKey() + "\' alias wasn't import;");
                    if (!skipRecord) {
                        m_skipCount++;
                    }
                    skipRecord = true;

                }

                if (!skipRecord) {
                    changes.add(oldValue + " (\"" + entry.getKey() + "\" alias) => " + entry.getValue());
                }
            }

            isDirty = true;

            if (m_commit && !skipRecord) {
                refCode.setCode(modelRefCode.m_code);
                refCode.setDescription(modelRefCode.m_description);
                refCode.setStateCode(modelRefCode.m_stateCode);
                refCode.setLocalCode(modelRefCode.m_localCode);
                refCode.setFederalCode(modelRefCode.m_federalCode);
                refCode.setDependencyCode(modelRefCode.m_dependencyCode);
                boolean categoryInd = StringUtils.isEmpty(modelRefCode.m_isCategory) ? false
                        : modelRefCode.m_isCategory.equals(BooleanAsStringConverter.TRUE) ? true : false;
                refCode.setCategoryIndicator(categoryInd);
                if (!categoryInd) {
                    refCode.setCategory(modelRefCode.m_categoryName);
                }

                boolean disabledInd = StringUtils.isEmpty(modelRefCode.m_isDisabled) ? false
                        : modelRefCode.m_isDisabled.equals(BooleanAsStringConverter.TRUE) ? true : false;
                refCode.setDisabledIndicator(disabledInd);

                for (Entry<String, String> entry : modelRefCode.m_aliases.entrySet()) {
                    if (m_dictionary.containsAlias(entry.getKey())) {
                        refCode.setFieldValueByAlias(entry.getKey(), entry.getValue(), m_dictionary);
                    }
                }
            }
        } else {
            String key = (String) refCode.getFieldValueByBeanPath(matchPath);
            if (modelRefTable.m_map.containsKey(key)) {
                Integer count = modelRefTable.m_codeMatchCount.get(key);
                if (refCode.getDisabledIndicator() && !m_updateDisabledRefCodes) {
                    logMessage("Matched disabled code " + refCode.getCode() + " : [" + matchPath + "] " + key
                            + " -- No Update");
                } else {
                    logMessage("Matched " + refCode.getCode() + " : [" + matchPath + "] " + key);
                }
                if (count.intValue() == 1) {
                    if (!refCode.getDisabledIndicator() || m_updateDisabledRefCodes) {
                        if (updatePath == null) {
                            // update not specified - update all columns
                            if (!StringUtils.isEqual(refCode.getCode(), modelRefCode.m_code)) {
                                changes.add(refCode.getCode() + " (Code) => " + modelRefCode.m_code);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setCode(modelRefCode.m_code);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getDescription(), modelRefCode.m_description)) {
                                changes.add(
                                        refCode.getDescription() + " (Description) => " + modelRefCode.m_description);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setDescription(modelRefCode.m_description);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getStateCode(), modelRefCode.m_stateCode)) {
                                changes.add(refCode.getStateCode() + " (State code) => " + modelRefCode.m_stateCode);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setStateCode(modelRefCode.m_stateCode);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getLocalCode(), modelRefCode.m_localCode)) {
                                changes.add(refCode.getLocalCode() + " (Local code) => " + modelRefCode.m_localCode);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setLocalCode(modelRefCode.m_localCode);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getFederalCode(), modelRefCode.m_federalCode)) {
                                changes.add(
                                        refCode.getFederalCode() + " (Federal code) => " + modelRefCode.m_federalCode);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setFederalCode(modelRefCode.m_federalCode);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getDisabledIndicator() ? BooleanAsStringConverter.TRUE
                                    : BooleanAsStringConverter.FALSE,
                                    StringUtils.isEmpty(modelRefCode.m_isDisabled) ? BooleanAsStringConverter.FALSE
                                            : modelRefCode.m_isDisabled)) {
                                changes.add(
                                        refCode.getDisabledIndicator() ? BooleanAsStringConverter.TRUE
                                                : BooleanAsStringConverter.FALSE + " (Disabled) => "
                                                        + (BooleanAsStringConverter.TRUE
                                                                .equals(modelRefCode.m_isDisabled) ? "true" : "false"));
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setDisabledIndicator(
                                            BooleanAsStringConverter.TRUE.equals(modelRefCode.m_isDisabled) ? true
                                                    : false);
                                }
                            }

                            if (!StringUtils.isEqual(refCode.getCategoryIndicator() ? BooleanAsStringConverter.TRUE
                                    : BooleanAsStringConverter.FALSE, modelRefCode.m_isCategory)) {
                                changes.add(
                                        refCode.getCategoryIndicator() ? BooleanAsStringConverter.TRUE
                                                : BooleanAsStringConverter.FALSE + " (Is a Category) => "
                                                        + modelRefCode.m_isCategory);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setCategoryIndicator(
                                            BooleanAsStringConverter.TRUE.equals(modelRefCode.m_isCategory) ? true
                                                    : false);
                                }
                            }
                            if (!refCode.getCategoryIndicator()
                                    && !StringUtils.isEqual(refCode.getCategory(), modelRefCode.m_categoryName)) {
                                changes.add(
                                        refCode.getCategory() + " (Category Name) => " + modelRefCode.m_categoryName);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setCategory(modelRefCode.m_categoryName);
                                }
                            }
                        } else {
                            String value = modelRefTable.m_map.get(key);
                            String currentValue = (String) refCode.getFieldValueByBeanPath(updatePath);
                            // Update if empty or we are forcing the update
                            if (modelRefTable.m_force || StringUtils.isEmpty(currentValue)) {
                                changes.add(refCode.getCode() + " (Code) matched [" + matchPath + "]" + key);
                                changes.add(refCode.getCode() + " (Code) updating [" + updatePath + "]" + value);
                                isDirty = true;
                                if (m_commit) {
                                    refCode.setFieldValueByBeanPath(updatePath, value);
                                }
                            } else {
                                logMessage("Skipped update to " + modelRefCode + " already populated: [" + updatePath
                                        + "] "
                                        + currentValue);
                                m_skipCount++;
                            }
                        }
                    }
                } else {
                    logMessage("Skipped update to " + refCode.getCode() + " (Code) - more than one match defined ["
                            + count + "].");
                    m_skipCount++;
                }
            } else {
                logMessage("Skipped update to " + refCode.getCode() + " (Code) not matched [" + key + "].");
                m_skipCount++;
            }
        }

        if (isDirty && !skipRecord) {
            if (isNew) {
                logMessage("Inserted code " + modelRefCode + " : "
                        + StringUtils.convertCollectionToDelimitedString(changes, ", "));
                m_insertCount++;
            } else {
                logMessage("Updated code " + modelRefCode + " : "
                        + StringUtils.convertCollectionToDelimitedString(changes, ", "));
                m_updateCount++;
            }

            if (m_commit) {
                getBroker().saveBeanForced(refCode);
            }
        }
    }
}
