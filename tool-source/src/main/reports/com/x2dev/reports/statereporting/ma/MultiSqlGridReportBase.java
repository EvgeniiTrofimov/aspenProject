/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExtendedDataField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.utils.X2BaseException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Base class for Multi Sql Grid reports.
 *
 * @author Follett Software Company
 */
abstract public class MultiSqlGridReportBase extends ReportJavaSourceNet {
    // Static strings
    protected static final String QUERY_NAME = "QUERY_NAME";
    protected static final String FIELD_NAME = "FIELD";
    protected static final String REPORT_FIELD_EXCEPTION = "EXCEPTION";

    // Internal variables
    protected StringBuilder m_errorMessage = null;
    protected Map<Integer, String[]> m_headerStringMap = null;
    protected DatabaseOptimizer m_optimizer = null;
    protected String[] m_queryStrings = null;

    /**
     * Builds the query header map.
     *
     * @return Map
     */
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        return null;
    }

    /**
     * Calc show results.
     *
     * @param rsHeader String
     * @return true, if successful
     */
    abstract protected boolean calcShowResults(String rsHeader);

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        int queries = m_headerStringMap.size();
        ReportDataGrid grid = new ReportDataGrid(300, 8);

        // Grab a connection and prepare a statement; gets reused for each query
        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;

        // Track whether the header row has been added to the grid
        boolean headerSet;
        int iQueryLine = 0;

        try {
            for (int x = 0; x < queries; x++) {
                int fields = m_headerStringMap.get(Integer.valueOf(x)).length;
                headerSet = false;

                try {
                    // Get the next query and swap aliases for DB fields
                    String queryString = replaceAliasesWithDbFields(m_queryStrings[x]);
                    queryString = optimizeQuery(queryString);

                    if (m_errorMessage.length() > 0) {
                        String errorMessage = m_errorMessage.toString();
                        m_errorMessage = new StringBuilder();
                        throw new Exception(errorMessage);
                    }

                    statement = connection.prepareStatement(queryString);
                    results = statement.executeQuery();

                    while (results.next()) {
                        String rsHeader = results.getString(1);
                        if (calcShowResults(rsHeader)) {
                            // If this is a new query, set the header row first
                            if (!headerSet) {
                                iQueryLine = 1;
                                setQueryHeader(grid, fields, m_headerStringMap.get(Integer.valueOf(x)));
                                headerSet = true;
                            }
                            grid.append();
                            // Loop through the fields defined in the headerStringMap
                            // and put the results into the grid
                            grid.set(QUERY_NAME, m_headerStringMap.get(Integer.valueOf(x))[0]);
                            iQueryLine = iQueryLine + 1;
                            for (int i = 1; i < fields; i++) {
                                grid.set(FIELD_NAME + i, results.getString(i));
                            }
                            grid.set(FIELD_NAME + "10", Integer.valueOf(iQueryLine));
                        }
                    }
                } catch (Exception e) {
                    // Show error message about fail SQL statement or alias error
                    if (!headerSet) {
                        iQueryLine = 1;
                        setQueryHeader(grid, fields, m_headerStringMap.get(Integer.valueOf(x)));
                        setQueryFooter(grid, e);
                        headerSet = true;
                    }
                    e.printStackTrace();
                } finally {
                    if (results != null) {
                        results.close();
                        results = null;
                    }
                    if (statement != null) {
                        statement.close();
                        statement = null;
                    }
                }
            }
        } finally {
            getBroker().returnConnection();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Takes an alias and returns the database column name for the alias, which
     * is dependent on district setup.
     *
     * @param fieldAlias String
     * @return String
     */
    protected String getDatabaseFieldName(String fieldAlias) {
        /*
         * Alias is on a different table (DataFieldConfig) than the java name
         * for a field (DataField), so must query first for the DataFieldConfig
         * record (prefix of FDD), then get the DataField record (prefix of
         * FLD). In SQL, this class does the following... >> SELECT
         * fld_database_name FROM data_field >> INNER JOIN data_field_config on
         * FDD_FLD_OID = FLD_OID >> WHERE fdd_alias = "___";
         */
        int countRecord = 0;

        Criteria fddCriteria = new Criteria();
        fddCriteria.addEqualTo(DataFieldConfig.COL_ALIAS, fieldAlias);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class,
                fddCriteria);
        QueryIterator fddIterator = getBroker().getIteratorByQuery(query);
        try {
            while (fddIterator.hasNext()) {
                DataFieldConfig fdd = (DataFieldConfig) fddIterator.next();
                DataField fldRecord = (DataField) getBroker().getBeanByOid(DataField.class, fdd.getDataFieldOid());
                countRecord++;
                if (countRecord == 1) {
                    return fldRecord.getDatabaseName();
                }
                m_errorMessage.append(m_errorMessage.length() != 0 ? "\n" : "").append("Alias error: ")
                        .append("More than 1 field has alias " + fieldAlias);
                return null;
            }
        } catch (Exception e) {
            m_errorMessage.append(m_errorMessage.length() != 0 ? "\n" : "").append("Alias error: ")
                    .append("Exception looking for alias " + fieldAlias);
            return null;
        } finally {
            fddIterator.close();
        }

        // This return should only be reached if alias wasn't found
        int ctrFdxRecs = 0;

        Criteria fdxCriteria = new Criteria();
        fdxCriteria.addEqualTo(ExtendedDataField.COL_ALIAS, fieldAlias);
        QueryByCriteria fdxQuery = new QueryByCriteria(ExtendedDataField.class,
                fdxCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(fdxQuery);
        try {
            while (iterator.hasNext()) {
                ExtendedDataField fdx = (ExtendedDataField) iterator.next();
                DataFieldConfig fddRecord = (DataFieldConfig) getBroker()
                        .getBeanByOid(DataFieldConfig.class,
                                fdx.getDataFieldConfigOid());
                ctrFdxRecs++;
                if (ctrFdxRecs == 1) {
                    DataField fldxRecord = (DataField) getBroker()
                            .getBeanByOid(DataField.class,
                                    fddRecord.getDataFieldOid());
                    return fldxRecord.getDatabaseName();
                }
                m_errorMessage.append(m_errorMessage.length() != 0 ? "\n" : "").append("Alias error: ")
                        .append("More than 1 field has alias " + fieldAlias);
                return null;
            }
        } catch (Exception e) {
            m_errorMessage.append(m_errorMessage.length() != 0 ? "\n" : "").append("Alias error: ")
                    .append("Exception looking for alias " + fieldAlias);
            return null;
        } finally {
            iterator.close();
        }
        // This return should only be reached if alias wasn't found
        m_errorMessage.append(m_errorMessage.length() != 0 ? "\n" : "").append("Alias error: ")
                .append("Couldn't find field for alias " + fieldAlias);
        return "''";
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());
        initializeParams();
        m_errorMessage = new StringBuilder();
        m_headerStringMap = buildQueryHeaderMap();
        m_queryStrings = initializeQueries();
    }

    /**
     * Initialize params.
     */
    abstract protected void initializeParams();

    /**
     * Initialize queries.
     *
     * @return String[]
     */
    abstract protected String[] initializeQueries();

    /**
     * Left pad.
     *
     * @param value String
     * @param length String
     * @param pad String
     * @return String
     */
    protected String leftPad(String value, String length, String pad) {
        String leftPaddedString = null;
        switch (m_optimizer.getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
                leftPaddedString = "LPAD(" + value + "," + length + "," + pad + ")";
                break;

            case DatabaseOptimizerFactory.SQLSERVER:
                leftPaddedString = "RIGHT(" + pad + "+" + value + "," + length + ")";
                break;

            default:
                try {
                    throw new Exception("Database platform is not supported");
                } catch (Exception e) {
                    e.printStackTrace();
                }
        }

        return leftPaddedString;
    }

    /**
     * Replace aliases with db fields.
     *
     * @param queryString String
     * @return String
     */
    abstract protected String replaceAliasesWithDbFields(String queryString);

    /**
     * Sql cast to char.
     *
     * @param expression String
     * @param numOfChars int
     * @return String
     */
    protected String sqlCastToChar(String expression, int numOfChars) {
        StringBuilder wrappedExpr = new StringBuilder(expression);
        wrappedExpr.insert(0, "CAST(").append(" AS CHAR(" + numOfChars + "))");

        return wrappedExpr.toString();
    }

    /**
     * Sql string is not numeric.
     *
     * @param expression String
     * @return String
     */
    protected String sqlStringIsNotNumeric(String expression) {
        String isNotNumeric = null;
        switch (m_optimizer.getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
                isNotNumeric = expression + " NOT BETWEEN 0 AND 9";
                break;

            case DatabaseOptimizerFactory.SQLSERVER:
                isNotNumeric = "ISNUMERIC(" + expression + ") = 0";
                break;

            default:
                try {
                    throw new Exception("Database platform is not supported");
                } catch (Exception e) {
                    e.printStackTrace();
                }
        }

        return isNotNumeric;
    }

    /**
     * Optimize concat.
     *
     * @param query String
     * @return String
     */
    private String optimizeConcat(String query) {
        Pattern concat = Pattern.compile("(CONCAT)(\\()(.*)(\\)).+ErrData");
        Matcher matcher = concat.matcher(query);
        while (matcher.find()) {
            String group3 = matcher.group(3);
            Pattern hidden = Pattern.compile("\\( *[\\d]+, *[\\d]+\\)");
            Matcher hidMatcher = hidden.matcher(group3);
            while (hidMatcher.find()) {
                group3 = group3.replace(hidMatcher.group(0), hidMatcher.group(0).replace(",", "###"));
            }

            String[] concatElements = group3.split(",");
            query = query.replace(matcher.group(1) + matcher.group(2) + matcher.group(3) + matcher.group(4),
                    m_optimizer.concatenateStrings(Arrays.asList(concatElements)));
            hidden = Pattern.compile("\\( *[\\d]+### *[\\d]+\\)");
            hidMatcher = hidden.matcher(query);
            while (hidMatcher.find()) {
                query = query.replace(hidMatcher.group(0), hidMatcher.group(0).replace("###", ","));
            }
        }
        return query;
    }

    /**
     * Optimize if null.
     *
     * @param query String
     * @return String
     */
    private String optimizeIfNull(String query) {
        Pattern ifNull = Pattern.compile("(IFNULL)(\\()([^']+), *(\\'[^)]*\\' *)\\)");
        Matcher matcher = ifNull.matcher(query);
        while (matcher.find()) {
            query = query.replace(matcher.group(0),
                    m_optimizer.sqlIf(matcher.group(3) + " IS NULL", matcher.group(4), matcher.group(3)));
        }
        return query;
    }

    /**
     * Optimize length.
     *
     * @param query String
     * @return String
     */
    private String optimizeLength(String query) {
        Pattern concat = Pattern.compile("(LENGTH)\\(([^)]+)\\)");
        Matcher matcher = concat.matcher(query);
        while (matcher.find()) {
            query = query.replace(matcher.group(0), "LEN(" + matcher.group(2) + ")");
        }
        return query;
    }

    /**
     * Optimize query.
     *
     * @param query String
     * @return String
     */
    private String optimizeQuery(String query) {
        // Queries already for MySQL, so optimize if MSSQL is used
        if (m_optimizer.getPlatform() == DatabaseOptimizerFactory.SQLSERVER) {
            query = optimizeIfNull(query);
            query = optimizeConcat(query);
            query = optimizeTrim(query);
            query = optimizeLength(query);
        }
        return query;
    }

    /**
     * Optimize trim.
     *
     * @param query String
     * @return String
     */
    private String optimizeTrim(String query) {
        Pattern concat = Pattern.compile("([^a-zA-Z])(TRIM)(\\()([^)]+)(\\))");
        Matcher matcher = concat.matcher(query);
        while (matcher.find()) {
            if (query.contains(matcher.group(0))) {
                query = query.replace(matcher.group(2) + matcher.group(3) + matcher.group(4) + matcher.group(5),
                        "LTRIM(RTRIM(" + matcher.group(4) + "))");
            }
        }
        return query;
    }

    /**
     * Sets the query header.
     *
     * @param grid ReportDataGrid
     * @param fields int
     * @param queryHeader String[]
     */
    private void setQueryHeader(ReportDataGrid grid,
                                int fields,
                                String[] queryHeader) {
        grid.append();
        grid.set(QUERY_NAME, queryHeader[0]);
        for (int i = 1; i < fields; i++) {
            grid.set(FIELD_NAME + i, queryHeader[i]);
        }
        grid.set(FIELD_NAME + "10", Integer.valueOf(1));
    }

    /**
     * Sets the query footer.
     *
     * @param grid ReportDataGrid
     * @param e Exception
     */
    private void setQueryFooter(ReportDataGrid grid, Exception e) {
        if (e != null) {
            if (e instanceof SQLException) {
                grid.set(REPORT_FIELD_EXCEPTION, "SQL Exception: " + e.getMessage());
            } else {
                grid.set(REPORT_FIELD_EXCEPTION, e.getMessage());
            }
        }
    }
}
