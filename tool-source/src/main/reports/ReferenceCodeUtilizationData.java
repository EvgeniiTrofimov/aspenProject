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
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.utils.StringUtils;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Reference Code Utilization report. This report shows code usage
 * statistics for a single reference table, grouped by the dictionary fields using the reference
 * table.
 *
 * @author X2 Development Corporation
 */
public class ReferenceCodeUtilizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constant value for records that do not have a reference code value (i.e, the values are
     * "empty" - either NULL or the empty string depending upon the underlying database).
     */
    public static final String EMPTY = "<< EMPTY >>";

    /**
     * Name of the report grid column that contains the actual code value used in the current
     * table/field. This value is useful if the value is not an existing reference code (i.e, the
     * value for GRID_REFERENCE_CODE is null).
     */
    public static final String GRID_CODE = "code";

    /**
     * Name of the report grid column that contains the number of times the code is used in the
     * current table/field.
     */
    public static final String GRID_COUNT = "count";

    /**
     * Name of the report grid column that contains the dictionary field linked to a reference
     * table.
     */
    public static final String GRID_DICTIONARY_FIELD = "dictionaryField";

    /**
     * Name of the report grid column that contains the dictionary table linked to a reference
     * table (through a dictionary field).
     */
    public static final String GRID_DICTIONARY_TABLE = "dictionaryTable";

    /**
     * Name of the report grid column that contains the reference code.
     */
    public static final String GRID_REFERENCE_CODE = "referenceCode";

    /**
     * Name for the "reference table OID" report parameter. The value is an String.
     */
    public static final String REFERENCE_TABLE_OID_PARAM = "referenceTableOid";

    /**
     * Name for the "reference table" report parameter. The value is an ReferenceTable.
     */
    public static final String REFERENCE_TABLE_PARAM = "referenceTable";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Get the reference table and add it as a report parameter for use on the format
         */
        String oid = (String) getParameter(REFERENCE_TABLE_OID_PARAM);
        ReferenceTable table =
                (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, oid);

        addParameter(REFERENCE_TABLE_PARAM, table);

        /*
         * Analyze the codes for the selected reference table
         */
        ReportDataGrid codeAnalysis = null;
        try {
            codeAnalysis = getCodeAnalysis(table);
            codeAnalysis.beforeTop();
        } catch (SQLException sqle) {
            codeAnalysis = new ReportDataGrid();
        }

        return codeAnalysis;
    }

    /**
     * Adds a record to the grid with the given values.
     *
     * @param grid ReportDataGrid
     * @param referenceCode ReferenceCode
     * @param code String
     * @param dictionaryField DataDictionaryField
     * @param dictionaryTable DataDictionaryTable
     * @param count Integer
     */
    private void appendRecord(ReportDataGrid grid,
                              ReferenceCode referenceCode,
                              String code,
                              DataDictionaryField dictionaryField,
                              DataDictionaryTable dictionaryTable,
                              Integer count) {
        grid.append();
        grid.set(GRID_CODE, code);
        grid.set(GRID_COUNT, count);
        grid.set(GRID_DICTIONARY_FIELD, dictionaryField);
        grid.set(GRID_DICTIONARY_TABLE, dictionaryTable);
        grid.set(GRID_REFERENCE_CODE, referenceCode);
    }

    /**
     * Returns a grid containing the results to display. The grid has the following columns:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Column Name</th>
     * <th>Value Type</th>
     * </tr>
     * <tr>
     * <td>GRID_DICTIONARY_FIELD</td>
     * <td>DataDictionaryField object</td>
     * </tr>
     * <tr>
     * <td>GRID_DICTIONARY_TABLE</td>
     * <td>DataDictionaryTable object</td>
     * </tr>
     * <tr>
     * <td>GRID_REFERENCE_CODE</td>
     * <td>ReferenceCode bean</td>
     * </tr>
     * <tr>
     * <td>GRID_CODE</td>
     * <td>String object</td>
     * </tr>
     * <tr>
     * <td>GRID_COUNT</td>
     * <td>Integer object</td>
     * </tr>
     * </table>
     * The grid is ordered first by DataDictionaryField (the long name) and then by the
     * ReferenceCode code.
     *
     * @param table ReferenceTable
     * @return ReportDataGrid
     * @throws SQLException exception
     */
    private ReportDataGrid getCodeAnalysis(ReferenceTable table) throws SQLException {
        /*
         * Get the codes for the reference table and which dictionary fields link to it
         */
        Criteria codeCriteria = ReferenceManager.getCodesCriteria(table.getOid(),
                getOwnableCriteria(),
                true,
                true,
                false,
                getBroker().getPersistenceKey());

        QueryByCriteria codeQuery = new QueryByCriteria(ReferenceCode.class, codeCriteria);
        codeQuery.addOrderByAscending(ReferenceCode.COL_CODE);

        Collection codes = getBroker().getCollectionByQuery(codeQuery);

        Criteria fieldCriteria = new Criteria();
        fieldCriteria.addEqualTo(DataFieldConfig.COL_REFERENCE_TABLE_OID, table.getOid());

        QueryByCriteria fieldQuery = new QueryByCriteria(DataFieldConfig.class, fieldCriteria);
        fieldQuery.addOrderByAscending(DataFieldConfig.COL_USER_LONG_NAME);

        Collection fields = getBroker().getCollectionByQuery(fieldQuery);

        /*
         * Create the grid and load the results.
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        ReportDataGrid grid = new ReportDataGrid(fields.size() * codes.size(), 4);

        Iterator fieldIterator = fields.iterator();
        while (fieldIterator.hasNext()) {
            DataFieldConfig field = (DataFieldConfig) fieldIterator.next();

            DataDictionaryField dictionaryField = dictionary.findDataDictionaryField(field);
            DataDictionaryTable dictionaryTable =
                    dictionary.findDataDictionaryTableByClass(dictionaryField.getDataTable().getClassName());

            Map codeUsage = getCodeUsageMap(dictionaryTable, dictionaryField);

            /*
             * Account for any codes that are part of the reference table
             */
            Iterator codeIterator = (codes).iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();

                Integer count = (Integer) codeUsage.remove(code.getCode());
                if (count == null) {
                    count = Integer.valueOf(0);
                }

                appendRecord(grid, code, code.getCode(), dictionaryField, dictionaryTable, count);
            }

            /*
             * Account for any codes that are NOT part of the reference table
             */
            Iterator nonCodes = codeUsage.keySet().iterator();
            while (nonCodes.hasNext()) {
                String code = (String) nonCodes.next();
                Integer count = (Integer) codeUsage.get(code);

                appendRecord(grid, null, code, dictionaryField, dictionaryTable, count);
            }
        }

        return grid;
    }

    /**
     * Returns the code usage for the given table/field combination.
     *
     * @param table DataDictionaryTable
     * @param field DataDictionaryField
     * @return A SortedMap of count values (Integer objects) key on codes (String objects)
     * @throws SQLException exception
     */
    private SortedMap getCodeUsageMap(DataDictionaryTable table, DataDictionaryField field)
            throws SQLException {
        SortedMap codeUsage = new TreeMap();

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet resultSet = null;
        try {
            String sql = "SELECT " + field.getDatabaseName() + ", COUNT(*)" +
                    "  FROM " + table.getDatabaseName() +
                    " GROUP BY " + field.getDatabaseName();

            statement = connection.createStatement();
            resultSet = statement.executeQuery(sql);

            while (resultSet.next()) {
                String code = (String) resultSet.getObject(1);
                Integer count = Integer.valueOf(resultSet.getObject(2).toString());

                /*
                 * Combine NULL and empty string values into one count. Both get stored in the map
                 * with the EMPTY code.
                 */
                if (StringUtils.isEmpty(code)) {
                    Integer existingCount = (Integer) codeUsage.get(EMPTY);
                    if (existingCount != null) {
                        count = Integer.valueOf(existingCount.intValue() + count.intValue());
                    }

                    code = EMPTY;
                }

                codeUsage.put(code, count);
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

        return codeUsage;
    }
}
