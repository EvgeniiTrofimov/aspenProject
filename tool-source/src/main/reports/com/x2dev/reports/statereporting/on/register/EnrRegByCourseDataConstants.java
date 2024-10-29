/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2023 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on.register;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

public class EnrRegByCourseDataConstants {
    public enum EnrRegReport {
        OBC_ENR_REG002a("OBC_ENR_REG002a"),

        OBC_ENR_REG003("OBC_ENR_REG003"),

        OBC_ENR_REG004("OBC_ENR_REG004"),

        OBC_ENR_REG005("OBC_ENR_REG005"),

        OBC_ENR_REG006("OBC_ENR_REG006");

        private final String value;

        EnrRegReport(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    public static final String PARAM_CSK_CON_ED_PROGRAM_TYPES = "cskConEdProgramTypes";

    public static final String PARAM_MST_OIDS = "sectionOids";

    public static final String PARAM_MST_OIDS_STAFF_VIEW = "sectionOidsStaffView";

    public static final String PARAM_COURSE_SORT = "courseSort";

    public static final String PARAM_STUDENT_INFO_BY = "studentInfoBy";

    public static final String PARAM_COURSE_INCLUDE_DAYS = "courseIncludeDays";

    public static final String PARAM_COURSE_INCLUDE_TIMES = "courseIncludeTimes";

    public static final String PARAM_RPT_ID_PART1 = "rptIdPart1";

    public static final String PARAM_RPT_ID_PART2 = "rptIdPart2";

    public static final String PARAM_REPORT_NAME = "toolName";

    public static final String REPORT_ONTARIO_ALIAS_FOR_RPT_MAP = "ontarioAliasForRptMap";

    public static final String REPORT_PREFIX1 = "prefix1";

    public static final String REPORT_PREFIX2 = "prefix2";

    public static final String REPORT_DEC5_FMT_OUTPUT = "dec5FmtOutput";

    public static final String REPORT_BUILDING_CODES_RCD_MAP = "buildingCodesRcdMap";

    public static final String REPORT_COURSE_START_DATE_TO_MST_MAP = "classStartDateToMstOid";

    public static final String REPORT_COURSE_END_DATE_TO_MST_MAP = "classEndDateToMstOid";

    public static final String REPORT_COURSE_SUMMER_COURSE_iND_TO_MST_MAP = "classSummerCourseIndToMstOid";

    public static final String REPORT_COURSE_DELIVERY_TYPE_LIST_BY_ONSIS_CODE_MAP =
            "courseDeliveryTypeListByOnsisCodeMap";

    public static final String REPORT_ASSIGN_TOT_TO_MST_MAP = "assignTotToMstOidMap";

    public static final String REPORT_ASSIGN_CT_NON_OP_REG_TO_MST_MAP = "assignCtNonOpRegToMstOidMap";

    public static final String REPORT_ASSIGN_CT_NON_OP_SUMMER_TO_MST_MAP = "assignCtNonOpSummerToMstOidMap";

    public static final String REPORT_ASSIGN_CT_NON_OP_FT_TO_MST_MAP = "assignCtNonOpFtToMstOidMap";

    public static final String REPORT_PUPIL_CT_MALE_OCT_TO_MST_MAP = "pupilCtMaleOctToMstOidMap";

    public static final String REPORT_PUPIL_CT_FEMALE_OCT_TO_MST_MAP = "pupilCtFemaleOctToMstOidMap";

    public static final String REPORT_PUPIL_CT_MALE_JUN_TO_MST_MAP = "pupilCtMaleJunToMstOidMap";

    public static final String REPORT_PUPIL_CT_FEMALE_JUN_TO_MST_MAP = "pupilCtFemaleJunToMstOidMap";

    public static final String REPORT_PUPIL_CT_MALE_AUG_TO_MST_MAP = "pupilCtMaleAugToMstOidMap";

    public static final String REPORT_PUPIL_CT_FEMALE_AUG_TO_MST_MAP = "pupilCtFemaleAugToMstOidMap";

    public static final String REPORT_CLASS_DAY_LIST_TO_MST_MAP = "classDayListToMstOidMap";

    public static final String REPORT_CLASS_TIME_LIST_TO_MST_MAP = "classStartEndTimeToMstOidMap";

    public static final String REPORT_CLASS_DATE_LIST_TO_MST_MAP = "classDtListToMstOidMap";

    public static final String REPORT_ATT_FUNDED_CT_NON_OP_TO_MST_MAP = "attFundedCtNonOpToMstOidMap";

    public static final String FIELD_MASTER_SCHEDULE = "masterSchedule";

    public static final String FIELD_STUDENT = "student";

    public static final String FIELD_STUDENT_NUMBER = "studentNumber";

    public static final String FIELD_OP_IND = "opInd";

    public static final String FIELD_REG_TYPE_OCT = "regTypOct";

    public static final String FIELD_REG_TYPE_MAR = "regTypMar";

    public static final String FIELD_ASSIGN_CT = "assignCt";

    public static final String FIELD_ASSIGN_CT_NON_OP_REG = "assignCtNonOpReg";

    public static final String FIELD_ASSIGN_CT_NON_OP_SUMMER = "assignCtNonOpSummer";

    public static final String FIELD_ASSIGN_CT_FUNDED_REG = "assignCtFundedReg";

    public static final String FIELD_ASSIGN_CT_FUNDED_SUMMER = "assignCtFundedSummer";

    public static final String FIELD_CLASS_DTL_PAGE = "classDtlPage";

    public static final String FIELD_CLASS_DTL_PAGE_START = "classDtlPageStart";

    public static final String FIELD_CLASS_ATT_LIST = "classAttList";

    public static final String FIELD_ATT_FUNDED_CT_NON_OP = "attFundedCtNonOp";

    public static final List<String> CONST_ONTARIO_ALIAS_FOR_RPT_LIST = new ArrayList<>(Arrays.asList(
            "IMAGE_FILE_NAME_ONTARIO", "ALIAS_PSN_LEGAL_LAST_NAME", "ALIAS_PSN_LEGAL_FIRST_NAME",
            "ALIAS_CSK_COURSE_DELIVERY_TYPE", "CONST_CRS_DELIVERY_TYPE_CORR", "CONST_CRS_DELIVERY_TYPE_ELEARN_NOTLMS",
            "CONST_CRS_DELIVERY_TYPE_ELEARN_LMS", "CONST_CRS_DELIVERY_TYPE_IND_STUDY", "ALIAS_CSK_INTL_LANGUAGE",
            "ALIAS_MST_CON_ED_OFFERING_TYPE", "CONST_MST_CON_ED_OFFERING_TYPE_01_NIGHT_WEEKEND",
            "CONST_MST_CON_ED_OFFERING_TYPE_02_DAY", "CONST_MST_CON_ED_OFFERING_TYPE_03_AFTER_SKL",
            "CONST_MST_CON_ED_OFFERING_TYPE_04_CREDIT_ON_MATH", "CONST_MST_CON_ED_OFFERING_TYPE_05_ADDITIONAL_PREP",
            "CONST_MST_CON_ED_OFFERING_TYPE_13_7_8_REMEDIAL_LIT_NUM",
            "CONST_MST_CON_ED_OFFERING_TYPE_14_NON_CREDIT_9_10_REMEDIAL_LIT_NUM",
            "CONST_MST_CON_ED_OFFERING_TYPE_15_LIT_NUM_PARENTS_GUARDIANS", "ALIAS_MST_TIME_OF_DAY_CODE",
            "CONST_MST_TIME_OF_DAY_CODE_1_REG_DAY", "CONST_MST_TIME_OF_DAY_CODE_2_SUMMER",
            "CONST_MST_TIME_OF_DAY_CODE_3_WEEKENDS", "CONST_MST_TIME_OF_DAY_CODE_4_AFTER_SKL",
            "CONST_MST_TIME_OF_DAY_CODE_5_EXTENDED_SKL_DAY", "CONST_MST_TIME_OF_DAY_CODE_6_BEF_SKL_OR_LUNCH",
            "CONST_MST_TIME_OF_DAY_CODE_7_LATE_AFT_OR_EVE"));

    public static final String CONST_EMPTY = "";

    public static final String CONST_SPACE = " ";

    public static final String CONST_SPLIT_STR = "---";

    public static final String CONST_HYPHEN = "-";

    public static final String CONST_DELIMITER = ".";


    public static final String CONST_COMMA = ",";

    public static final String CONST_TRUE = "1";

    public static final int CONST_LIST_NOT_FOUND = -2;

    public static final DateFormat CONST_FORMAT_DATE_MST_IN = new SimpleDateFormat("yyyy-MM-dd");

    public static final String CONST_TOOLS_FOR_PREFIX = "tools.";

    public static final String CONST_NUM_FMT_STR_DEC5 = "#,##0.00000";

    public static final Collection<String> CONST_CRS_DELIVERY_TYPE_DUAL_CRED_ONSIS_CODE_LIST =
            new ArrayList<>(Arrays.asList("12", "13", "21"));

    public static final String CONST_REG_TYPE_SKL_CON_ED = "CE";

    public static final int CONST_CLASS_DTL_PAGE1_CT = 45;

    public static final int CONST_CLASS_DTL_PAGE2_CT = 75;

    public static final String CONST_ATT_NOT_ENROLLED = "-";

    public static final int CONST_ABS_CONSEC_CT_MIN = 3;

    public static final String CONST_SORT_CRS_NUM_SECTION = "courseNumber, section";

    public static final String CONST_SORT_CRS_DESC_SECTION = "courseDesc, section";

    public static final String CONST_STD_INFO_BY_ASSIGN = "byAssign";

    public static final String CONST_STD_INFO_BY_DATE = "byDate";

    public static final String CONST_CAL_HALF1_END = "-01-31";

    public static final String CONST_CAL_SUMMER_ENR_START = "-06-25";

    public static final String CONST_CAL_SUMMER_ASSIGN_AFTER = "-06-30";

    public static final int CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP = 10;

    public static final Collection<String> ENR_OTHER_PUPIL_CODES = new ArrayList<>(
            Arrays.asList("02", "03", "05", "07", "12", "13", "14", "15"));

    public static final String FIELD_REASON = "reason";

    public static final String FIELD_TOTAL_DAYS_FUNDED_REG = "totalDaysFundedReg";

    public static final String FIELD_TOTAL_DAYS_FUNDED_SUMMER = "totalDaysFundedSummer";

    public static final Map<String, String> ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP = new HashMap<>();
    static {
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("A", "A");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("C", "B");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("EC", "C");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("CEC", "CEC");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("CED", "CED");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("E", "E");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("G", "G");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("F", "H");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("SE", "N");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("P", "P");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("JP", "PA");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("R", "L");
        ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.put("S", "S");
    }
}
