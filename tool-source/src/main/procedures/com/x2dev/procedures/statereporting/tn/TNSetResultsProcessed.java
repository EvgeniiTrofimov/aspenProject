/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * The Class TNSetResultsProcessed.
 */
public class TNSetResultsProcessed extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    private DateFormat m_timeFormat = null;
    private DateFormat m_dateFormat = null;

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        List<ExportFormatDefinition> exportDefinitionList = new ArrayList<ExportFormatDefinition>();

        if (getParameter("exportIds") != null) {
            String[] exportIds = ((String) getParameter("exportIds")).split(",");

            for (String exportId : exportIds) {
                ExportFormatDefinition exportDefinition =
                        (ExportFormatDefinition) getBroker().getBeanByOid(ExportFormatDefinition.class, exportId);
                exportDefinitionList.add(exportDefinition);
            }

            for (ExportFormatDefinition definition : exportDefinitionList) {
                ExportFormatResult result = TNExportResultsHelper.getLatestResult(definition.getOid(), getBroker());
                if (result != null) {
                    TNExportResultsHelper.setResultProcessed(result, getBroker());

                    Long runDate = Long.valueOf(result.getRunDate());

                    logMessage("Latest result for " + definition.getName() + " on " + m_dateFormat.format(runDate) +
                            " " + m_timeFormat.format(runDate) + " set to processed");

                } else {
                    logMessage("Latest result not found for " + definition.getName());
                }
            }
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        Locale locale = userData.getLocale();

        if (m_dateFormat == null) {
            m_dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, locale);
            DateUtils.convertToFullYearFormat(m_dateFormat);
            m_dateFormat.setTimeZone(userData.getTimeZone());
        }

        if (m_timeFormat == null) {
            m_timeFormat = DateFormat.getTimeInstance(DateFormat.SHORT, locale);
            m_timeFormat.setTimeZone(userData.getTimeZone());
        }
    }
}
