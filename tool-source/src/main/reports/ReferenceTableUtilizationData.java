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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PreferenceSet;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Reference Table Utilization report.
 *
 * @author X2 Development Corporation
 */
public class ReferenceTableUtilizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name of the report grid column that contains the number of disabled codes for a reference
     * table.
     */
    public static final String GRID_CODES_DISABLED = "disabledCodes";

    /**
     * Name of the report grid column that contains the total number of codes for a reference table.
     */
    public static final String GRID_CODES_TOTAL = "allCodes";

    /**
     * Name of the report grid column that contains the name of the dictionary field linked to a
     * reference table.
     */
    public static final String GRID_DICTIONARY_FIELD = "fieldName";

    /**
     * Name of the report grid column that contains the name of the dictionary table linked to a
     * reference table (through a dictionary field).
     */
    public static final String GRID_DICTIONARY_TABLE = "tableName";

    /**
     * Name of the report grid column that contains the indicator for whether or not to display the
     * core reference table values. This is used for records that represent a second, third, or more
     * data dictionary field for a single reference table.
     */
    public static final String GRID_PRINT = "print";

    /**
     * Name of the report grid column that contains the reference table.
     */
    public static final String GRID_REFERENCE_TABLE = "referenceTable";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private Map m_preferenceValues;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null, null, null);

        QueryByCriteria query = createQueryByCriteria(ReferenceTable.class, criteria);

        /*
         * Build the sort based on user input
         */
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Execute the query and return the results
         */
        ReportDataGrid results = getResults(query);
        results.beforeTop();

        return results;
    }

    /**
     * Appends a record to the grid and sets the columns to the given values.
     *
     * @param grid ReportDataGrid
     * @param table ReferenceTable
     * @param totalCodes int
     * @param disabledCodes int
     * @param fieldName String
     * @param tableName String
     * @param print boolean
     */
    private void appendRecord(ReportDataGrid grid,
                              ReferenceTable table,
                              int totalCodes,
                              int disabledCodes,
                              String fieldName,
                              String tableName,
                              boolean print) {
        grid.append();
        grid.set(GRID_CODES_DISABLED, Integer.valueOf(disabledCodes));
        grid.set(GRID_CODES_TOTAL, Integer.valueOf(totalCodes));
        grid.set(GRID_DICTIONARY_FIELD, fieldName);
        grid.set(GRID_DICTIONARY_TABLE, tableName);
        grid.set(GRID_PRINT, Boolean.valueOf(print));
        grid.set(GRID_REFERENCE_TABLE, table);
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
     * <td>GRID_REFERENCE_TABLE</td>
     * <td>ReferenceTable bean</td>
     * </tr>
     * <tr>
     * <td>GRID_CODES_TOTAL</td>
     * <td>Integer object</td>
     * </tr>
     * <tr>
     * <td>GRID_CODES_DISABLED</td>
     * <td>Integer object</td>
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
     * <td>GRID_PRINT</td>
     * <td>Boolean object</td>
     * </tr>
     * </table>
     *
     * @param tableQuery QueryByCriteria
     * @return DataGrid
     */
    private ReportDataGrid getResults(QueryByCriteria tableQuery) {
        int tableCount = getBroker().getCount(tableQuery);

        /*
         * Get all the codes and fields in one big grouped query rather than one little query per
         * table.
         */
        SubQuery subQuery = new SubQuery(ReferenceTable.class, X2BaseBean.COL_OID, tableQuery.getCriteria());

        /*
         * Loads reference codes for selected reference tables.
         */
        Criteria codeCriteria = new Criteria();
        codeCriteria.addIn(ReferenceCode.COL_REFERENCE_TABLE_OID, subQuery);

        QueryByCriteria codeQuery = new QueryByCriteria(ReferenceCode.class, codeCriteria);

        Map codesByTable = getBroker().getGroupedCollectionByQuery(codeQuery,
                ReferenceCode.COL_REFERENCE_TABLE_OID, tableCount);

        /*
         * Loads data fields that use selected reference tables.
         */
        Criteria fieldCriteria = new Criteria();
        fieldCriteria.addIn(DataFieldConfig.COL_REFERENCE_TABLE_OID, subQuery);

        QueryByCriteria fieldQuery = new QueryByCriteria(DataFieldConfig.class, fieldCriteria);
        fieldQuery.addOrderByAscending(DataFieldConfig.COL_USER_LONG_NAME);

        Map fieldsByTable = getBroker().getGroupedCollectionByQuery(fieldQuery,
                DataFieldConfig.COL_REFERENCE_TABLE_OID, tableCount);

        /*
         * Loads transcript column definitions that use selected reference tables.
         */
        Criteria transcriptColumnCriteria = new Criteria();
        transcriptColumnCriteria.addIn(TranscriptColumnDefinition.COL_REFERENCE_TABLE_OID, subQuery);

        QueryByCriteria transcriptColumnQuery = new QueryByCriteria(TranscriptColumnDefinition.class,
                transcriptColumnCriteria);

        Map transColumnByTable = getBroker().getGroupedCollectionByQuery(transcriptColumnQuery,
                TranscriptColumnDefinition.COL_REFERENCE_TABLE_OID, tableCount);

        /*
         * Create the grid, load the results, and initialize data
         */
        ReportDataGrid grid = new ReportDataGrid(tableCount, 6);
        QueryIterator tables = getBroker().getIteratorByQuery(tableQuery);
        m_preferenceValues = getPreferenceValues();

        try {
            while (tables.hasNext()) {
                ReferenceTable table = (ReferenceTable) tables.next();

                /*
                 * Analyze the codes for the reference table
                 */
                Collection codes = (Collection) codesByTable.get(table.getOid());

                int disabledCodes = 0;
                int totalCodes = 0;

                if (codes != null) {
                    Iterator codeIterator = codes.iterator();
                    while (codeIterator.hasNext()) {
                        ReferenceCode code = (ReferenceCode) codeIterator.next();

                        if (code.getDisabledIndicator()) {
                            disabledCodes++;
                        }

                        totalCodes++;
                    }
                }

                /*
                 * Get the list of fields that use the reference table
                 */
                boolean inUse = false;
                boolean firstTime = true;

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

                Collection fields = (Collection) fieldsByTable.get(table.getOid());
                if (fields != null) {
                    Iterator fieldIterator = (fields).iterator();
                    while (fieldIterator.hasNext()) {
                        DataFieldConfig field = (DataFieldConfig) fieldIterator.next();

                        DataDictionaryField dictionaryField = dictionary.findDataDictionaryField(field);
                        DataDictionaryTable dictionaryTable = dictionary
                                .findDataDictionaryTableByClass(dictionaryField.getDataTable().getClassName());

                        inUse = true;
                        appendRecord(grid, table, totalCodes, disabledCodes,
                                dictionaryField.getUserLongName(), dictionaryTable.getUserName(), firstTime);
                        firstTime = false;
                    }
                }

                /*
                 * Get the list of Transcript columns that use the reference table
                 */
                Collection transcriptColumns = (Collection) transColumnByTable.get(table.getOid());
                if (transcriptColumns != null) {
                    Iterator columnIterator = transcriptColumns.iterator();
                    while (columnIterator.hasNext()) {
                        TranscriptColumnDefinition definition =
                                (TranscriptColumnDefinition) columnIterator.next();
                        String definitionName =
                                definition.getTranscriptDefinition().getTranscriptDefinitionName();

                        inUse = true;
                        appendRecord(grid, table, totalCodes, disabledCodes,
                                definition.getGradeName(), definitionName, firstTime);
                        firstTime = false;
                    }
                }

                /*
                 * Check preferences that use the reference table
                 */
                if (m_preferenceValues.containsKey(table.getOid())) {
                    SystemPreferenceDefinition definition =
                            (SystemPreferenceDefinition) m_preferenceValues.get(table.getOid());

                    inUse = true;
                    appendRecord(grid, table, totalCodes, disabledCodes, definition.getName(),
                            "System Preferences", firstTime);
                    firstTime = false;
                }

                if (!inUse) {
                    appendRecord(grid, table, totalCodes, disabledCodes, null, null, true);
                }

            }
        } finally {
            tables.close();
        }

        return grid;
    }

    /**
     * Returns a collection of the various system preference definitions that may or may not make
     * use
     * of a reference table.
     *
     * @return Collection of <code>SystemPreferenceDefinition</code>'s
     */
    private Map getPreferenceValues() {
        PreferenceSet set = PreferenceManager.getPreferenceSet(getOrganization());
        List list = set.getVisiblePreferenceDefinitions();
        HashMap preferences = new HashMap(list.size());
        Iterator iterator = list.iterator();

        while (iterator.hasNext()) {
            SystemPreferenceDefinition definition = (SystemPreferenceDefinition) iterator.next();
            String value = PreferenceManager.getPreferenceValue(getOrganization(), definition.getKey());
            preferences.put(value, definition);
        }

        return preferences;
    }
}
