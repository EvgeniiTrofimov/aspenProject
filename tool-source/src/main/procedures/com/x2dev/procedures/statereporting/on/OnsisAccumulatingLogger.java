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

import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

/**
 * @author Follett Software Company
 * @copyright 2020
 *
 *            The Class SifAccumulatingLogger (extends java.util.logging.Logger).
 *
 *            Accumulate log(message) calls into a List<String>.
 *            When flush() is called, append into a call to job.logMessage().
 *
 */
public class OnsisAccumulatingLogger extends java.util.logging.Logger {

    public static interface SifLog {

        /**
         * Log a message
         *
         * @param message
         */
        public abstract void logMessage(String message);

    }

    ProcedureJavaSource m_job = null;
    SifLog m_sifLog = null;
    protected List<String> m_buf = new ArrayList<>();

    /**
     * Instantiates a new sif accumulating logger.
     *
     * @param name String
     * @param resourceBundleName String
     */
    public OnsisAccumulatingLogger(String name, String resourceBundleName) {
        super(name, resourceBundleName);
    }

    /**
     * Instantiates a new sif accumulating logger.
     *
     * @param name String
     * @param resourceBundleName String
     * @param job ProcedureJavaSource
     */
    public OnsisAccumulatingLogger(String name, String resourceBundleName, ProcedureJavaSource job) {
        super(name, resourceBundleName);
        m_job = job;
    }

    public OnsisAccumulatingLogger(String name, String resourceBundleName, SifLog sifLog) {
        super(name, resourceBundleName);
        m_sifLog = sifLog;
    }


    /**
     * @see java.util.logging.Logger#log(java.util.logging.Level, java.lang.String)
     */
    @Override
    public void log(Level level, String msg) {
        m_buf.add(msg);
    }

    /**
     * Flush.
     *
     * @return
     */
    public String flush() {
        String message = "";

        if (m_buf.size() > 0) {
            message = StringUtils.convertCollectionToDelimitedString(m_buf, "\n");
            // System.out.println(message);

            if (m_job != null) {
                m_job.logMessage(message);
            }

            if (m_sifLog != null) {
                m_sifLog.logMessage(message);
            }

            m_buf.clear();
        }

        return message;
    }

    @Override
    public String toString() {
        return StringUtils.convertCollectionToDelimitedString(m_buf, "\n");
    }

    public List<String> getBuf() {
        return m_buf;
    }
}
