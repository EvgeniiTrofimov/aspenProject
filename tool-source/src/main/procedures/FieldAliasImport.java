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
import static com.follett.fsc.core.k12.beans.DataFieldConfig.REL_DATA_FIELD;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.io.File;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for defining district-level data dictionary fields based on a text input file. The expected
 * input file is a plain text file with comma-separated values wrapped in double-quotation marks.
 * The file must contain the following fields (in order):
 * <ol>
 * <li>Table user name
 * <li>Alias
 * <li>Long name
 * <li>Short name
 * <li>User data type
 * <li>Length
 * <li>Decimals
 * <li>Enabled (Y/N)
 * <li>Required (Y/N)
 * <li>Read only (Y/N)
 * <li>Mass update (Y/N)
 * <li>List edit (Y/N)
 * <li>Default value
 * <li>Reference table name
 * <li>Detail control
 * <li>Valid reference code required (Y/N)
 * </ol>
 *
 * @author X2 Development Corporation
 */
public class FieldAliasImport extends TextImportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "update existing field" parameter, which updates names and setup information for
     * existing field aliases. Note that updating does not change the underlying database field in
     * which the field is defined. Therefore, the length is only updated if it is acceptable for the
     * database field.
     */
    public static final String UPDATE_EXISTING_PARAM = "updateExisting";

    /*
     * Record indexes
     */
    private static final int INDEX_TABLE_NAME = 0;
    private static final int INDEX_ALIAS = 1;
    private static final int INDEX_LONG_NAME = 2;
    private static final int INDEX_SHORT_NAME = 3;
    private static final int INDEX_DATA_TYPE = 4;
    private static final int INDEX_LENGTH = 5;
    private static final int INDEX_DECIMAL = 6;
    private static final int INDEX_ENABLED = 7;
    private static final int INDEX_REQUIRED = 8;
    private static final int INDEX_READ_ONLY = 9;
    private static final int INDEX_UPDATE = 10;
    private static final int INDEX_LIST_EDIT = 11;
    private static final int INDEX_DEFAULT_VALUE = 12;
    private static final int INDEX_REFERENCE_TABLE_NAME = 13;
    private static final int INDEX_DETAIL_CONTROL = 14;
    private static final int INDEX_VALID_REFERENCE_ONLY = 15;



    private boolean m_update = false;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 16;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        super.importData(sourceFile);

        DataDictionaryCache.clearDictionaries(getOrganization().getPersistenceKey(), true);
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
        String tableName = record.get(INDEX_TABLE_NAME);
        DataTableConfig table = getTableConfig(tableName);
        if (table == null) {
            logInvalidRecord(lineNumber, "Error - Unknown table " + tableName);
            incrementSkipCount();
        } else {
            String alias = record.get(INDEX_ALIAS);
            int length = getInteger(record, INDEX_LENGTH);

            DataFieldConfig field = lookupField(table.getDataTableOid(), alias);
            if (field == null) {
                field = DataDictionary.getAvailableField(table.getDataTableOid(), length, getBroker());

                if (field == null) {
                    logInvalidRecord(lineNumber, "Error - No available fields for alias " + alias);
                    incrementSkipCount();
                } else {
                    field.setEnabledIndicator(true);
                    field.setAlias(alias);
                }
            } else if (!m_update) {
                field = null;
            }

            if (field != null) {
                /*
                 * Update the length first. Make sure the length complies with the database field
                 * length.
                 */
                if (length > field.getDataField().getDatabaseLength()) {
                    logInvalidRecord(lineNumber, "Warning - Unable to update field length for alias " + alias);
                } else {
                    field.setUserLength(length);
                }

                field.setUserLongName(record.get(INDEX_LONG_NAME));
                field.setUserShortName(record.get(INDEX_SHORT_NAME));
                field.setUserType(record.get(INDEX_DATA_TYPE));
                field.setUserDecimal(getInteger(record, INDEX_DECIMAL));
                field.setEnabledIndicator(getBoolean(record, INDEX_ENABLED));
                field.setRequiredIndicator(getBoolean(record, INDEX_REQUIRED));
                field.setReadOnlyIndicator(getBoolean(record, INDEX_READ_ONLY));
                field.setUpdateIndicator(getBoolean(record, INDEX_UPDATE));
                field.setListEditIndicator(getBoolean(record, INDEX_LIST_EDIT));
                field.setDefaultValue(record.get(INDEX_DEFAULT_VALUE));

                String referenceTableName = record.get(INDEX_REFERENCE_TABLE_NAME);
                if (!StringUtils.isEmpty(referenceTableName)) {
                    ReferenceTable referenceTable = getReferenceTable(alias);
                    if (referenceTable == null) {
                        logInvalidRecord(lineNumber, "Warning - Unable to set reference table for alias " + alias
                                + "; unknown reference table " + referenceTableName);
                    } else {
                        field.setReferenceTableOid(referenceTable.getOid());
                    }
                }

                field.setDetailControl(record.get(INDEX_DETAIL_CONTROL));
                field.setListEditIndicator(getBoolean(record, INDEX_VALID_REFERENCE_ONLY));

                getBroker().saveBeanForced(field);
                incrementUpdateCount();
            }
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

        m_update = ((Boolean) getParameter(UPDATE_EXISTING_PARAM)).booleanValue();
    }

    /**
     * Retrives a boolean from the passed record.
     *
     * @param record List<String>
     * @param index int
     * @return boolean
     */
    private boolean getBoolean(List<String> record, int index) {
        Converter booleanConverter = ConverterFactory.getConverterForClass(Converter.BOOLEAN_CONVERTER, getLocale());
        return ((Boolean) booleanConverter.stringToJava(record.get(index))).booleanValue();
    }

    /**
     * Returns the district table with the passed name.
     *
     * @param name String
     * @return DataTableConfig
     */
    private DataTableConfig getTableConfig(String name) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DataTableConfig.COL_USER_NAME, name);

        QueryByCriteria query = new QueryByCriteria(DataTableConfig.class, criteria);

        return (DataTableConfig) getBroker().getBeanByQuery(query);
    }

    /**
     * Retrives an integer from the passed record.
     *
     * @param record List<String>
     * @param index int
     * @return int
     */
    private int getInteger(List<String> record, int index) {
        Converter integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER, getLocale());

        Integer integer = (Integer) integerConverter.stringToJava(record.get(index));
        if (integer == null) {
            integer = Integer.valueOf(0);
        }

        return integer.intValue();
    }

    /**
     * Returns the reference table with the passed name.
     *
     * @param name String
     * @return ReferenceTable
     */
    private ReferenceTable getReferenceTable(String name) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceTable.COL_USER_NAME, name);

        QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);

        return (ReferenceTable) getBroker().getBeanByQuery(query);
    }

    /**
     * Returns the field in the passed table with the passed alias if one exists. Otherwise null is
     * returned.
     *
     * @param tableOid String
     * @param alias String
     * @return DataFieldConfig
     */
    private DataFieldConfig lookupField(String tableOid, String alias) {
        Criteria criteria = DataDictionary.getAliasCriteria(alias);
        criteria.addEqualTo(REL_DATA_FIELD + "." + DataField.COL_DATA_TABLE_OID, tableOid);

        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);

        return (DataFieldConfig) getBroker().getBeanByQuery(query);
    }
}
