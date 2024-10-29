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

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.OrganizationManager;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;

/**
 * This helper class is used to create the header and trailer record for the TN State Reporting
 * Extracts.
 *
 * @author Follett
 *
 */
public class TNHeadingTrailing {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String REPORTS[] = {
            "010", "011", "015,015_V2", "016",
            "020", "021", "022",
            "030", "031",
            "040,040_V5,040_V6", "041", "042", "043", "044", "045,045_V2", "046", "047", "048", "049",
            "050", "051,051_V2", "052",
            "060,060_V3", "061", "062", "063",
            "080", "081", "082", "083",
            "090", "091", "092", "093"
    };
    private Calendar m_calendar;
    private Map<String, Integer> m_counts;
    private DateFormat m_dateFormat;
    private int m_fileSequence;
    private Organization m_org;

    /**
     * Initialize the helper.
     *
     * @param org Organization
     * @param fileSequence The file sequence number between 1 and 99 used to generate multiple
     *        reports for the same day
     */
    public TNHeadingTrailing(Organization org, int fileSequence) {
        m_dateFormat = new SimpleDateFormat("yyyyMMdd");
        m_calendar = new GregorianCalendar(OrganizationManager.getTimeZone(org));
        m_counts = new HashMap<String, Integer>(REPORTS.length);

        m_org = org;
        m_fileSequence = fileSequence;
        if (m_fileSequence < 1) {
            m_fileSequence = 1;
        }
        if (m_fileSequence > 99) {
            m_fileSequence = 99;
        }
    }

    /**
     * Get the heading record.
     *
     * @return String
     */
    public String getHeading() {
        return String.format("0010100%3s%8s%-20s%20s\r\n", m_org.getId(),
                m_dateFormat.format(m_calendar.getTime()), getFileName(), " ");
    }

    /**
     * Get the trailer record.
     *
     * @return String
     */
    public String getTrailer() {
        StringBuilder sb = new StringBuilder("9990600");
        sb.append(String.format("%3s", m_org.getId()));
        sb.append(m_dateFormat.format(m_calendar.getTime()));
        sb.append(String.format("%-20s", getFileName()));

        int totalCount = 0;
        for (String reportId : REPORTS) {
            String[] ids = reportId.split(",");
            int count = 0;
            for (String id : ids) {
                if (m_counts.containsKey(id)) {
                    count = m_counts.get(id).intValue();
                }
            }
            sb.append(String.format("%09d", Integer.valueOf(count)));
            totalCount += count;
        }
        // add header and trailer record
        totalCount += 2;
        sb.append(String.format("%09d", Integer.valueOf(totalCount)));
        sb.append(String.format("%2s\r\n", " "));

        return sb.toString();
    }

    /**
     * Increment the count for a particular report type.
     *
     * @param recordType String
     * @param count int
     */
    public void incementCount(String recordType, int count) {
        isValid(recordType);
        int newValue = m_counts.containsKey(recordType) ? m_counts.get(recordType).intValue() + count : count;
        m_counts.put(recordType, Integer.valueOf(newValue));
    }

    /**
     * Set the count for a particular record type.
     *
     * @param recordType String
     * @param records int
     */
    public void setCount(String recordType, int records) {
        isValid(recordType);
        m_counts.put(recordType, Integer.valueOf(records));
    }

    /**
     * Create the structured file name.
     *
     * @return String
     */
    private String getFileName() {
        return String.format("%3s%1d%02d%02dR%02d.EIS",
                m_org.getId(),
                Integer.valueOf(m_calendar.get(Calendar.YEAR) % 10),
                Integer.valueOf(m_calendar.get(Calendar.MONTH) + 1),
                Integer.valueOf(m_calendar.get(Calendar.DAY_OF_MONTH)),
                Integer.valueOf(m_fileSequence));
    }

    /**
     * Validate the record type.
     *
     * @param recordType String
     */
    private void isValid(String recordType) {
        if (!m_counts.containsKey(recordType)) {
            for (String reportId : REPORTS) {
                String[] ids = reportId.split(",");
                for (String id : ids) {
                    if (id.equals(recordType)) {
                        return;
                    }
                }

            }
            throw new IllegalArgumentException(
                    "The report type [" + recordType + "] must be one of " + Arrays.asList(REPORTS).toString());
        }
    }
}
