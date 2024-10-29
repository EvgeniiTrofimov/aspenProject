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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.x2dev.procedures.sys.shared.StateReportExport;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/**
 * The Class RiSpedStateReportExport.
 */
public class RiSpedStateReportExport extends StateReportExport {
    private static final long serialVersionUID = 1L;
    private static final SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");

    private String m_fileName = null;

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        if (m_fileName == null) {
            StringBuilder filename = new StringBuilder();
            String toolName = getJob().getTool().getName().trim();
            toolName = toolName.replace(" ", "").replace(".", "");

            Organization organization = OrganizationManager.getRootOrganization(getBroker());
            long utcTime = System.currentTimeMillis() - TimeZone.getDefault().getRawOffset();
            long convertedTime = utcTime + OrganizationManager.getTimeZone(organization).getRawOffset();

            filename.append(toolName);
            filename.append("_");
            filename.append(m_dateFormat.format(new Date(convertedTime)));
            filename.append(".csv");
            m_fileName = filename.toString();
        }

        return m_fileName;
    }
}
