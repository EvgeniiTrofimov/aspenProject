/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.k12.tools.sif.SifGlobals;
import com.follett.fsc.core.utils.debug.PerformanceData;
import com.follett.fsc.core.utils.debug.PerformanceLogResultsListener;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Follett Software Company
 * @copyright 2020
 *
 *            The listener interface for receiving sifPerformanceLogResults events.
 *            The class that is interested in processing a sifPerformanceLogResults
 *            event implements this interface, and the object created
 *            with that class is registered with a component using the
 *            component's <code>addSifPerformanceLogResultsListener<code> method. When
 *            the sifPerformanceLogResults event occurs, that object's appropriate
 *            method is invoked.
 *
 * @see SifPerformanceLogResultsEvent
 */
public class OnsisPerfLogResultsListener extends PerformanceLogResultsListener {

    private boolean m_logInterimResults = true;

    public String m_lastResults = null;

    /**
     * Instantiates a new sif performance log results listener.
     *
     * @param level Level
     * @param dumpEveryNotification boolean
     * @param logger Logger
     */
    public OnsisPerfLogResultsListener(Level level, boolean dumpEveryNotification, Logger logger) {
        super(level, dumpEveryNotification, logger);
    }

    public void setLogInterimResults(boolean logInterimResults) {
        m_logInterimResults = logInterimResults;
    }


    /**
     * Log interim results based on m_logInterimResults
     *
     * @see com.follett.fsc.core.utils.debug.PerformanceLogResultsListener#notifyResults(com.follett.fsc.core.utils.debug.PerformanceData)
     */
    @Override
    public void notifyResults(PerformanceData data) {
        if (m_logInterimResults) {
            super.notifyResults(data);
        }
    }

    /**
     * @see com.follett.fsc.core.utils.debug.PerformanceLogResultsListener#dumpResults(com.follett.fsc.core.utils.debug.PerformanceData)
     */
    @Override
    protected void dumpResults(PerformanceData data) {
        m_logger.log(m_level, "***** OnSIS Perf Stats node [" + SifGlobals.getSifNode() + "] thread "
                + data.getObservedThread().getName());

        super.dumpResults(data);

        m_lastResults = m_logger.toString();
        // System.out.println(m_lastResults);
        if (m_logger instanceof OnsisAccumulatingLogger) {
            ((OnsisAccumulatingLogger) m_logger).flush();
        }
    }
}
