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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;

/**
 * Procedure to reset selected export format results to facilitate reprocessing.
 */
public class TNResetResultsProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    /**
     * Input params
     */
    private static final String INPUT_EFR_EXPORT_OIDS = "efrExportOids";
    private static final String INPUT_EFR_RESTAGING_OIDS = "efrRestagingOids";

    /**
     * Perform updates on two lists of EFR oids.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String exportOids = (String) getParameter(INPUT_EFR_EXPORT_OIDS);
        if (!StringUtils.isEmpty(exportOids)) {
            int num = updateComment(exportOids, "");
            logMessage("Number of export results updated = " + num);
        }

        exportOids = (String) getParameter(INPUT_EFR_RESTAGING_OIDS);
        if (!StringUtils.isEmpty(exportOids)) {
            int num = updateComment(exportOids, "Restaging");
            logMessage("Number of restaging results updated = " + num);
        }
    }

    /**
     * Update list of export format results with the specified comment.
     *
     * @param exportOids String
     * @param comment String
     * @return number of results updated
     */
    private int updateComment(String exportOids, String comment) {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList(exportOids.split(",")));

        UpdateQuery updateQuery = new UpdateQuery(ExportFormatResult.class, efrCriteria,
                ExportFormatResult.COL_COMMENT, comment);
        return getBroker().executeUpdateQuery(updateQuery);
    }

}
