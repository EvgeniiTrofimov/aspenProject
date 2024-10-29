/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DataTable;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Data Field Utilization report (both the "summary" and "detail"
 * versions). This report shows field usage statistics for a single data table.
 *
 * @author X2 Development Corporation
 */
public class DataFieldUtilizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constant value for records that do not have a value (i.e, the values are "empty" - either
     * NULL or the empty string depending upon the underlying database).
     */
    public static final String EMPTY = "<< EMPTY >>";

    /**
     * Report ID for the format used in "detail" mode.
     */
    private static final String DETAIL_REPORT_ID = "SYS-ADM-006";

    /**
     * Name for the "dictionary table" report parameter. The value is an DataDictionaryTable.
     */
    public static final String DICTIONARY_TABLE_PARAM = "dictionaryTable";

    /**
     * Name of the report grid column that contains the number of occurrences for the current value
     * and field. This column is only used in detail mode.
     */
    public static final String GRID_COUNT = "count";

    /**
     * Name of the report grid column that contains the number of distinct values used in the
     * current field. This column is only used in summary mode.
     */
    public static final String GRID_DISTINCT_COUNT = "distinctCount";

    /**
     * Name of the report grid column that contains the number of records that contain a non-empty
     * value for the current field. This column is only used in summary mode.
     */
    public static final String GRID_NON_EMPTY_COUNT = "nonEmptyCount";

    /**
     * Name of the report grid column that contains the dictionary field for the selected table.
     */
    public static final String GRID_FIELD = "dictionaryField";

    /**
     * Name of the report grid column that contains the value used by the current field.This column
     * is only used in detail mode.
     */
    public static final String GRID_VALUE = "value";

    /**
     * Name for the "enabled only" report parameter. The value is a Boolean.
     */
    public static final String ENABLED_ONLY_PARAM = "enabledOnly";

    /**
     * Constant value for records that do have a value but whose value cannot be further analyzed
     * (i.e, the values are for a BLOB field and some databases do not support grouping by a BLOB).
     */
    public static final String NOT_EMPTY = "<< NOT EMPTY >>";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "show details" report parameter. The value is a Boolean.
     */
    public static final String SHOW_DETAILS_PARAM = "showDetails";

    /**
     * Name of the "table record count" report parameter. This value is an Integer.
     */
    public static final String TABLE_COUNT_PARAM = "tableCount";

    /**
     * Name for the "unused fields only" report parameter. The value is a Boolean.
     */
    public static final String UNUSED_ONLY_PARAM = "unusedOnly";

    private DataTableConfig m_districtTable = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Get the dictionary table and add it as a report parameter for use on the format. Also,
         * add the total number of records in the table as a report parameter.
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryTable table = dictionary.findDataDictionaryTable(m_districtTable);

        addParameter(DICTIONARY_TABLE_PARAM, table);
        try {
            Class beanClass = Class.forName(table.getClassName());
            QueryByCriteria query = new QueryByCriteria(beanClass, null);
            addParameter(TABLE_COUNT_PARAM, Integer.valueOf(getBroker().getCount(query)));
        } catch (ClassNotFoundException cnfe) {
            // Do nothing, this exception will never happen
        }

        /*
         * Analyze the fields for the selected dictionary table in either "detail" or "summary" mode
         */
        boolean showDetails = ((Boolean) getParameter(SHOW_DETAILS_PARAM)).booleanValue();
        ReportDataGrid fieldAnalysis = null;
        try {
            fieldAnalysis = getFieldAnalysis(table, showDetails);
            fieldAnalysis.beforeTop();
        } catch (SQLException sqle) {
            fieldAnalysis = new ReportDataGrid();
        }

        if (showDetails) {
            setFormatId(DETAIL_REPORT_ID);
        }

        return fieldAnalysis;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_districtTable = userData.getCurrentRecord(DataTableConfig.class);

        /*
         * If there is no ancestor DataTableConfig (e.g., the user ran this report from the Tools
         * module for testing purposes or from the homepage for whatever reason) then we will just
         * use the Table Config table Why CONFIG?
         *
         * 1. It should only have a small number of records (usually just 1).
         * 2. There aren't many fields to analyze.
         * 3. The TableConfig table is somewhat of the de facto default table (e.g., it's the
         * default
         * table in Tools > Global Lists).
         */
        if (m_districtTable == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(DataTableConfig.REL_DATA_TABLE + PATH_DELIMITER + DataTable.COL_CLASS_NAME,
                    SisOrganization.class.getName());

            QueryByCriteria query = new QueryByCriteria(DataTableConfig.class, criteria);
            m_districtTable = (DataTableConfig) getBroker().getBeanByQuery(query);
        }
    }

    /**
     * Appends rows to the grid with the following columns:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Column Name</th>
     * <th>Value Type</th>
     * </tr>
     * <tr>
     * <td>GRID_FIELD</td>
     * <td>DataDictionaryField object</td>
     * </tr>
     * <tr>
     * <td>GRID_VALUE</td>
     * <td>String object</td>
     * </tr>
     * <tr>
     * <td>GRID_COUNT</td>
     * <td>Integer object</td>
     * </tr>
     * </table>
     * .
     *
     * @param grid ReportDataGrid
     * @param field DataDictionaryField
     * @param valueUsage Map
     */
    private void appendDetailRecords(ReportDataGrid grid, DataDictionaryField field, Map valueUsage) {
        Iterator values = valueUsage.keySet().iterator();
        while (values.hasNext()) {
            String value = (String) values.next();
            Integer count = (Integer) valueUsage.get(value);

            grid.append();
            grid.set(GRID_FIELD, field);
            grid.set(GRID_VALUE, value);
            grid.set(GRID_COUNT, count);
        }
    }

    /**
     * Appends rows to the grid with the following columns:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Column Name</th>
     * <th>Value Type</th>
     * </tr>
     * <tr>
     * <td>GRID_FIELD</td>
     * <td>DataDictionaryField object</td>
     * </tr>
     * <tr>
     * <td>GRID_DISTINCT_COUNT</td>
     * <td>Integer object</td>
     * </tr>
     * <tr>
     * <td>GRID_NON_EMPTY_COUNT</td>
     * <td>Integer object</td>
     * </tr>
     * </table>
     * .
     *
     * @param grid ReportDataGrid
     * @param field DataDictionaryField
     * @param valueUsage Map
     * @param unusedOnly if true then the non-empty count for each row will be 0 (only unused
     *        fields will be appended)
     */
    private void appendSummaryRecords(ReportDataGrid grid,
                                      DataDictionaryField field,
                                      Map valueUsage,
                                      boolean unusedOnly) {
        int distinctCount = 0;
        int nonEmptyCount = 0;
        Iterator values = valueUsage.keySet().iterator();
        while (values.hasNext()) {
            String value = (String) values.next();

            if (!value.equals(EMPTY)) {
                distinctCount++;

                Integer count = (Integer) valueUsage.get(value);
                nonEmptyCount += count.intValue();
            }
        }

        if (!unusedOnly || nonEmptyCount == 0) {
            grid.append();
            grid.set(GRID_FIELD, field);
            grid.set(GRID_DISTINCT_COUNT, Integer.valueOf(distinctCount));
            grid.set(GRID_NON_EMPTY_COUNT, Integer.valueOf(nonEmptyCount));
        }
    }

    /**
     * Returns a database-specific clause for determining whether or not a BLOB field is empty. This
     * method will
     *
     * @param fieldName String
     * @param testForEmpty if true then the return value will take the form
     *        <code>OR fieldName = ''</code>, if false then the return value will take
     *        the form <code>AND fieldName <> ''</code>
     * @return A String suitable for insertion into an existing WHERE clause. The string may be
     *         empty if the underlying database does not have an extra "empty" condition for BLOB
     *         fields. The string will begin and end with a space character if not empty.
     */
    private String getExtraEmptyBlobCondition(String fieldName, boolean testForEmpty) {
        String condition = null;

        if (DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey())
                .getPlatform() == DatabaseOptimizerFactory.MYSQL) {
            if (testForEmpty) {
                condition = " OR " + fieldName + " = '' ";
            } else {
                condition = " AND " + fieldName + " <> '' ";
            }
        } else {
            condition = "";
        }

        return condition;
    }

    /**
     * Returns a grid containing the results to display. The grid structure depends upon the
     * SHOW_DETAILS_PARAM (either "detail" or "summary" mode).
     *
     * @param table DataDictionaryTable
     * @param showDetails boolean
     * @return DataGrid
     * @throws SQLException exception
     */
    private ReportDataGrid getFieldAnalysis(DataDictionaryTable table, boolean showDetails)
            throws SQLException {
        /*
         * Get the fields for the table in the correct sort order
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_DATA_TABLE_OID,
                table.getSystemOid());
        criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_SYSTEM_USE_ONLY_INDICATOR,
                Boolean.FALSE);

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null, null, null);

        if (((Boolean) getParameter(ENABLED_ONLY_PARAM)).booleanValue()) {
            criteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.TRUE);
        }

        String[] columns = new String[] {DataFieldConfig.COL_DATA_FIELD_OID,
                DataFieldConfig.COL_USER_LONG_NAME,
                DataFieldConfig.COL_SEQUENCE_NUMBER};

        ReportQueryByCriteria query = new ReportQueryByCriteria(DataFieldConfig.class, columns, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Create the grid and load the results.
         */
        boolean unusedOnly = ((Boolean) getParameter(UNUSED_ONLY_PARAM)).booleanValue();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        ReportDataGrid grid = new ReportDataGrid(dictionary.getFieldsForContext(table).size(), 3);

        ReportQueryIterator rows = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (rows.hasNext()) {
                Object[] row = (Object[]) rows.next();
                String fieldOid = (String) row[0];

                DataDictionaryField field = dictionary.findDataDictionaryField(fieldOid);

                Map valueUsage = getValueUsageMap(table, field);

                if (showDetails) {
                    appendDetailRecords(grid, field, valueUsage);
                } else {
                    appendSummaryRecords(grid, field, valueUsage, unusedOnly);
                }
            }
        } finally {
            rows.close();
        }

        return grid;
    }

    /**
     * Returns the code usage for the given table/field combination.
     *
     * @param table DataDictionaryTable
     * @param field DataDictionaryField
     * @return A SortedMap of count values (Integer objects) keyed on values (Object objects)
     * @throws SQLException exception
     */
    private SortedMap getValueUsageMap(DataDictionaryTable table, DataDictionaryField field)
            throws SQLException {
        boolean isBlobField = field.getDatabaseType().equals(DataField.BINARY_DATABASE_TYPE) ||
                field.getDatabaseType().equals(DataField.CLOB_DATABASE_TYPE);

        /*
         * BLOB values aren't converted because BLOB values aren't fully analyzed. The query for
         * BLOB fields is customized to return a String value.
         */
        Converter converter = null;
        if (!isBlobField) {
            converter = ConverterFactory.getConverterForClass(field.getEffectiveJavaType(),
                    getLocale(), field.isString());
        }

        SortedMap valueUsage = new TreeMap();

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet resultSet = null;
        try {
            /*
             * Not all databases support "GROUP BY" for a BLOB field. In such a case we only have
             * two groups: NULL and not NULL.
             */
            String sql = null;
            if (isBlobField) {
                sql = "SELECT NULL, COUNT(*)" +
                        "  FROM " + table.getDatabaseName() +
                        " WHERE " + field.getDatabaseName() + " IS NULL " +
                        getExtraEmptyBlobCondition(field.getDatabaseName(), true) +
                        "HAVING COUNT(*) > 0 " +
                        " UNION " +
                        "SELECT '" + NOT_EMPTY + "', COUNT(*)" +
                        "  FROM " + table.getDatabaseName() +
                        " WHERE " + field.getDatabaseName() + " IS NOT NULL " +
                        getExtraEmptyBlobCondition(field.getDatabaseName(), false) +
                        "HAVING COUNT(*) > 0";
            } else {
                sql = "SELECT " + field.getDatabaseName() + ", COUNT(*)" +
                        "  FROM " + table.getDatabaseName() +
                        " GROUP BY " + field.getDatabaseName();
            }

            statement = connection.createStatement();
            resultSet = statement.executeQuery(sql);

            while (resultSet.next()) {
                Object value = resultSet.getObject(1);
                Integer count = Integer.valueOf(resultSet.getObject(2).toString());

                /*
                 * Convert all values to strings for two reasons:
                 *
                 * 1. It's necessary in "detail" mode so the format displays the values properly.
                 * 2. We avoid a ClassCastException or NullPointerException when TreeMap.compareTo()
                 * is used to sort the map.
                 */
                String valueAsString = null;
                if (converter == null) {
                    if (value != null) {
                        valueAsString = value.toString();
                    }
                } else {
                    valueAsString = converter.javaToString(value);
                }

                /*
                 * Combine NULL and empty string values into one count. Both get stored in the map
                 * with the EMPTY value.
                 */
                if (StringUtils.isEmpty(valueAsString)) {
                    valueAsString = EMPTY;
                }

                /*
                 * Check if the value has an existing count, if so increment it. This check handles
                 * user-defined types that map different database values to the same valueAsString.
                 * For example, the user-defined boolean type maps both NULL and '0' values to
                 * "false".
                 */
                Integer existingCount = (Integer) valueUsage.get(valueAsString);
                if (existingCount != null) {
                    count = Integer.valueOf(existingCount.intValue() + count.intValue());
                }

                valueUsage.put(valueAsString, count);
            }
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException e) {
                    // ignore
                }
            }

            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException e) {
                    // ignore
                }
            }

            getBroker().returnConnection();
        }

        return valueUsage;
    }
}
