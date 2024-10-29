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
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.business.X2Broker;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;


/**
 * Used for "SYS-SPED-IL-ALL-37" report<br>
 * Report print list of other reports<br>
 * and these reports should implement api .
 */
public interface PrintAllReportAPI {

    /**
     * Sets the broker.
     *
     * @param broker void
     */
    void setBroker(X2Broker broker);

    /**
     * Fill grid.
     *
     * @param map Map<String,Object>
     * @param locale Locale
     * @return JRDataSource
     */
    JRDataSource fillGrid(Map<String, Object> map, Locale locale);
}
