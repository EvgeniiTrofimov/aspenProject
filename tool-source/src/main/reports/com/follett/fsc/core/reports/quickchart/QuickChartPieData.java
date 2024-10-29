/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.follett.fsc.core.reports.quickchart;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.QuickChartData;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.tools.QuickChartAction;
import java.math.BigDecimal;
import java.util.LinkedHashSet;
import java.util.logging.Level;

/**
 * Class which creates the data source for Quick Chart - Pie.
 */
public class QuickChartPieData extends QuickChartData {
    protected static final String GRAPH_SUBREPORT_ID = "SYS-QC-005-SUB1";
    protected static final String SUMMARY_SUBREPORT_ID = "SYS-QC-005-SUB2";

    private ReportDataGrid m_summaryData = null;

    /**
     * Builds the query which uses the criteria from the user's current list and
     * adds the necessary group by and sorting.
     * 
     * @return ColumnQuery
     */
    protected ColumnQuery buildQuery() {
        String categoryOrderSql = getCategoryFieldOrderSql();

        LinkedHashSet<String> orderColumns = new LinkedHashSet<String>();
        orderColumns.add(categoryOrderSql);

        // LinkedHashSet to preserve ordering and prevent duplicate fields
        LinkedHashSet<String> columns = new LinkedHashSet<String>();
        columns.add(getCountSql());
        columns.add(getCategoryFieldSql());
        columns.add(categoryOrderSql);

        BeanQuery currentQuery = createQueryByCriteria(getCurrentClass(), getCurrentCriteria());
        ColumnQuery query = new ColumnQuery(currentQuery, columns.toArray(new String[columns.size()]));

        addGroupBy(query, columns);
        addOrderBy(query, orderColumns);

        return query;
    }

    /**
     * Gather chart data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#gatherChartData()
     */
    @Override
    protected Object gatherChartData() throws Exception {
        // Use the summary grid, but consolidate rows less than the OTHER_GROUP_PERCENTAGE into a
        // single row
        ReportDataGrid grid = new ReportDataGrid();

        BigDecimal otherSum = new BigDecimal(0);
        BigDecimal totalCount = (BigDecimal) getParameter(TOTAL_COUNT);

        m_summaryData.beforeTop();
        while (m_summaryData.next()) {
            BigDecimal count = (BigDecimal) m_summaryData.get(COLUMN_COUNT);

            if (getCategoryOtherAggregate()
                    && (count.doubleValue() / totalCount.doubleValue()) < (getCategoryOtherAggregateSize() * .01)) {
                otherSum = otherSum.add(count);
            } else {
                grid.append(m_summaryData.getCurrentRow());
            }
        }

        // Add other group to grid
        if (otherSum.doubleValue() > 0) {
            grid.append();
            grid.set(COLUMN_COUNT, otherSum);
            grid.set(COLUMN_CATEGORY,
                    QuickChartAction.getResource(COLUMN_OTHER, getBroker().getPersistenceKey(), getLocale()));
        }

        m_summaryData.beforeTop();
        grid.beforeTop();

        return grid;
    }

    /**
     * Gather summary data.
     *
     * @return Object
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#gatherSummaryData()
     */
    @Override
    protected Object gatherSummaryData() {
        m_summaryData = new ReportDataGrid();

        String categoryField = getCategoryField(true);
        String categoryInterval = getCategoryInterval();

        QueryIterator iterator = null;
        try {
            BigDecimal totalCount = new BigDecimal(0);

            iterator = getBroker().getReportQueryIteratorByQuery(buildQuery());
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                BigDecimal count = getCountValue(row[0]);
                String category = getFieldValueAsString(categoryField, categoryInterval, row[1]);

                m_summaryData.append();
                m_summaryData.set(COLUMN_COUNT, count);
                m_summaryData.set(COLUMN_CATEGORY, category);

                totalCount = totalCount.add(count);
            }

            addParameter(TOTAL_COUNT, totalCount);
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }

        m_summaryData.beforeTop();

        return m_summaryData;
    }

    /**
     * Gets the sub report graph.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#getSubReportGraph()
     */
    @Override
    protected String getSubReportGraph() {
        return GRAPH_SUBREPORT_ID;
    }

    /**
     * Gets the sub report summary.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#getSubReportSummary()
     */
    @Override
    protected String getSubReportSummary() {
        return SUMMARY_SUBREPORT_ID;
    }
}
