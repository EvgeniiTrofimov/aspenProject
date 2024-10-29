/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The Class OnsisConstants.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisConstants {
    public static final String ARRIVAL_STATUS_ARRIVED_ENG = "Arrived";
    public static final String ARRIVAL_STATUS_ARRIVED_FRE = "Arrivée";
    public static final List<String> ARRIVAL_STATUSES_ARRIVED =
            Arrays.asList(ARRIVAL_STATUS_ARRIVED_ENG, ARRIVAL_STATUS_ARRIVED_FRE);

    public static final String CONST_EMPTY = "";
    public static final String CONST_ENR_OP_TXT = "OP";

    public static final SimpleDateFormat DATE_FORMATTER_HH_COLON_MM = new SimpleDateFormat("hh:mm");
    public static final SimpleDateFormat DATE_FORMATTER_HH_mm_ss_aaa = new SimpleDateFormat("HH:mm:ss aaa");
    public static final SimpleDateFormat DATE_FORMATTER_YYYY_MM_DD_SLASHES = new SimpleDateFormat("yyyy/MM/dd");

    public static final DecimalFormat DECIMAL_FORMAT_ADE = new DecimalFormat("#####0.0000");
    public static final DecimalFormat DECIMAL_FORMAT_FTE = new DecimalFormat("#0.00");

    public static final String[] DEFAULT_ONSIS_CALENDAR_IDS = new String[] {"Regular", "Adult", "Summer"};

    public static final String DDX_ID_ONSIS_RECORDS = "ON-SIS-RECORD";

    public static final String ELEMENTS_PATH_DELIMITER = "/";

    public static final String FORMAT_PREFIX_EXSMS = "EXSMS-";
    public static final String FORMAT_PREFIX_ONSIS = "ONSI2-";

    public static final String INPUT_PARAM_DEBUG_DETAIL = "debugDetail";

    public static final List<String> SUBMISSION_SCHOOL_TYPE_CONTINUING_EDUCATION = Arrays.asList("JUNNS");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_CTCC_SECONDARY = Arrays.asList("CTCCSEC");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_JUNE_SECONDARY = Arrays.asList("JUNSEC1");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_ECPP = Arrays.asList("ECPP");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_JUNE =
            Arrays.asList("JUNELEM3", "JUNELEM4", "JUNELEM5", "JUNSEC1", "JUNSECSUS");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_OCTOBER =
            Arrays.asList("OCTELEM2", "OCTELEM3", "OCTSEC1");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_ELEMENTARY =
            Arrays.asList("JUNELEM3", "JUNELEM4", "JUNELEM5", "JUNELEMSUS", "MARELEM2", "MARELEM3", "OCTELEM2",
                    "OCTELEM3");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY =
            Arrays.asList("JUNSEC1", "JUNSECSUS", "MARSEC1", "MARSEC2", "OCTSEC1");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_MARCH =
            Arrays.asList("MARSEC1", "MARSEC2");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_OCTOBER =
            Arrays.asList("OCTSEC1");
    public static final List<String> SUBMISSION_SCHOOL_TYPE_SUMMER_SCHOOL = Arrays.asList("SUMSEC");

    public static final SimpleDateFormat TIME_FORMATTER_HHMMSS = new SimpleDateFormat("hhmmss");
    private static final Map<String, Long> s_timeByGetter = new HashMap<>();

    public static final String VALUE_ATTENDANCE_TYPE_FULL_TIME = "FT";
    public static final String VALUE_ATTENDANCE_TYPE_PART_TIME = "PT";
    public static final String VALUE_CANADA = "CAN";
    public static final Collection<String> VALUE_GRADES_ELEMENTARY_STATE_CODES =
            Arrays.asList("JK", "K", "1", "2", "3", "4", "5", "6", "7", "8");
    public static final Collection<String> VALUE_GRADES_ELEMENTARY =
            Arrays.asList("JK", "SK", "01", "02", "03", "04", "05", "06", "07", "08");
    public static final Collection<String> VALUE_GRADES_SECONDARY_STATE_CODES =
            Arrays.asList("9", "10", "11", "12", "P9");
    public static final String VALUE_LANGUAGE_TYPE_ENGLISH = "E";
    public static final String VALUE_LANGUAGE_TYPE_FRENCH = "F";
    public static final String VALUE_MOBILITY_TYPE_BEGINNER = "37";
    public static final String VALUE_MOBILITY_TYPE_OTHER_PUBLIC_ELEMENTARY = "31";
    public static final String VALUE_MOBILITY_TYPE_OTHER_PUBLIC_SECONDARY = "29";
    public static final String VALUE_MOBILITY_TYPE_OTHER_SEPARATE_ELEMENTARY = "32";
    public static final String VALUE_MOBILITY_TYPE_OTHER_SEPARATE_SECONDARY = "30";
    public static final String VALUE_MOBILITY_TYPE_SAME_PUBLIC_ELEMENTARY = "05";
    public static final String VALUE_MOBILITY_TYPE_SAME_PUBLIC_SECONDARY = "09";
    public static final String VALUE_MOBILITY_TYPE_SAME_SEPARATE_ELEMENTARY = "27";
    public static final String VALUE_MOBILITY_TYPE_SAME_SEPARATE_SECONDARY = "19";
    public static final List<String> VALUE_MOBILITY_TYPES_ADDRESS_NOT_REQUIRED =
            Arrays.asList(VALUE_MOBILITY_TYPE_OTHER_PUBLIC_ELEMENTARY, VALUE_MOBILITY_TYPE_OTHER_PUBLIC_SECONDARY,
                    VALUE_MOBILITY_TYPE_OTHER_SEPARATE_ELEMENTARY, VALUE_MOBILITY_TYPE_OTHER_SEPARATE_SECONDARY,
                    VALUE_MOBILITY_TYPE_SAME_PUBLIC_ELEMENTARY, VALUE_MOBILITY_TYPE_SAME_PUBLIC_SECONDARY,
                    VALUE_MOBILITY_TYPE_SAME_SEPARATE_ELEMENTARY, VALUE_MOBILITY_TYPE_SAME_SEPARATE_SECONDARY);
    public static final String VALUE_MOBILITY_TYPE_OSSD_ENDED_STATE_CODE = "61";
    public static final String VALUE_MOBILITY_TYPE_SHARE_ENDED_STATE_CODE = "64";
    public static final String VALUE_ONTARIO = "ON";
    public static final List<String> VALUE_SCHOOL_TYPES_SEPARATE = Arrays.asList("2", "3");
    public static final List<String> VALUE_SCHOOL_TYPES_SECONDARY = Arrays.asList("02", "03");
    public static final String VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD = "01";
    public static final String VALUE_STU_BRD_RES_STAT_E_LEARNING = "09";
    public static final String VALUE_STU_BRD_RES_STAT_SHARED = "08";
    public static final List<String> VALUES_STU_BRD_RES_STAT_OTHER_PUPIL_OF_BOARD =
            Arrays.asList("02", "03", "05", "07");


    /**
     * Accumulate elapsed.
     *
     * @param key String
     * @param elapsedMs long
     */
    public static void accumulateElapsed(String key, long elapsedMs) {
        if (elapsedMs > 0) {
            Long accumulator = s_timeByGetter.get(key);
            if (accumulator == null) {
                accumulator = Long.valueOf(0);
            }
            accumulator = accumulator + elapsedMs;
            s_timeByGetter.put(key, accumulator);
        }
    }

    /**
     * Accumulate from.
     *
     * @param key String
     * @param startTime long
     * @return long
     */
    public static long accumulateFrom(String key, long startTime) {
        long elapsedMs = System.currentTimeMillis() - startTime;
        accumulateElapsed(key, elapsedMs);
        return System.currentTimeMillis();
    }

    /**
     * Clear time by getters.
     */
    public static void clearTimeByGetters() {
        s_timeByGetter.clear();
    }


    /**
     * Return negative if a < b, 0 if equal, and positive if a > b.
     *
     * @param a PlainDate
     * @param b PlainDate
     * @param nullIsOld Treat a null date as infinitely old
     * @return int
     */
    public static int compareDates(PlainDate a, PlainDate b, boolean nullIsOld) {
        if (a == null && b == null) {
            return 0;
        }

        if (a == null && b != null) {
            if (nullIsOld) {
                return -1;
            }

            return 1;
        }

        if (a != null && b == null) {
            if (nullIsOld) {
                return 1;
            }

            return -1;
        }

        return a.compareTo(b);
    }

    /**
     * Format date.
     *
     * @param date the date
     * @param dateFormat the date format
     * @return the string
     */
    public static String formatDate(PlainDate date, SimpleDateFormat dateFormat) {
        if (date == null) {
            return null;
        }
        return dateFormat.format(date);
    }

    /**
     * Gets the time by getters.
     *
     * @return List
     */
    public static List<String> getTimeByGetters() {
        if (s_timeByGetter == null) {
            return new ArrayList<>();
        }

        return s_timeByGetter.entrySet().stream()
                // .filter(entry -> entry.getValue() >= 500)
                .sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
                // .map(entry -> com.x2dev.utils.StringUtils.padLeft("" + (entry.getValue() / 1000),
                // 4) + ": "
                .map(entry -> com.x2dev.utils.StringUtils.padLeft("" + (entry.getValue()), 7) + ": "
                        + entry.getKey())
                .collect(Collectors.toList());
    }

    /**
     * Parses the date.
     *
     * @param formattedDate String
     * @param dateFormat SimpleDateFormat
     * @return PlainDate
     */
    public static PlainDate parseDate(String formattedDate, SimpleDateFormat dateFormat) {
        if (StringUtils.isBlank(formattedDate)) {
            return null;
        }
        try {
            Date parsedDate = dateFormat.parse(formattedDate);
            return (parsedDate == null) ? null : new PlainDate(parsedDate);
        } catch (ParseException e) {
            //
        }
        return null;
    }

}
