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
package com.x2dev.procedures.statereporting.va;

import com.x2dev.procedures.sys.shared.StateReportExport;
import com.x2dev.utils.DataGrid;
import java.util.Arrays;

/**
 * The Class StudentRecordCollectionExport.
 */
public class StudentRecordCollectionExport extends StateReportExport {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    protected static final String PARAM_SORT = "sort";

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.procedures.sys.shared.StateReportExport#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = super.gatherData();
        Integer sort = (Integer) getParameter(PARAM_SORT);
        if (sort.intValue() == 0) // LASID sort selected
        {
            grid.sort(Arrays.asList(new String[] {"Local ID", "Status", "Entry Date"}),
                    Arrays.asList(new Boolean[] {Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}), true);
        }
        return grid;
    }

}