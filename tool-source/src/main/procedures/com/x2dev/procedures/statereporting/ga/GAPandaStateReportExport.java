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
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.k12.tools.ResultHandler;
import com.x2dev.procedures.sys.shared.StateReportExport;

/**
 * The Class GAPandaStateReportExport.
 * Overridden StateReportExport to create file with *.csv extension instead of *.txt
 */
public class GAPandaStateReportExport extends StateReportExport {
    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        ResultHandler resultHandler = getJob().getResultHandler();
        String fileName = resultHandler.getFileName();
        if (fileName.endsWith(".txt")) {
            fileName = fileName.replace(".txt", ".csv");
        }
        return fileName;
    }
}
