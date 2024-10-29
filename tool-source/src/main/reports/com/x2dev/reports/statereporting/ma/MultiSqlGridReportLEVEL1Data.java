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
package com.x2dev.reports.statereporting.ma;

import java.util.HashMap;
import java.util.Map;

/**
 * The Class MultiSqlGridReportLEVEL1Data.
 */
public class MultiSqlGridReportLEVEL1Data extends MultiSqlGridReportBase {
    // Strings for aliases
    private static final String skl_StateId_alias = "skl-sif-district-id";
    private static final String sif_StateId_alias = "sif-district-id";
    private static final String sif_LeaName_alias = "sif-lea-name";
    private static final String skl_Doe15_alias = "DOE 15";
    private static final String std_AdjustedAttendance_alias = "DOE Adjusted Attendance";
    private static final String std_AdjustedMembership_alias = "DOE Adjusted Membership";

    // Input parameters
    Boolean m_chkAll = null;
    Boolean m_chkSchool = null;
    Boolean m_chkStudent = null;
    Boolean m_chkSchedule = null;

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#buildQueryHeaderMap()
     */
    @Override
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        Map<Integer, String[]> map = new HashMap<Integer, String[]>();
        map.put(Integer.valueOf(0),
                new String[] {"Error: 0200 - School: 01000 - Invalid State Code", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(1),
                new String[] {"Error: 0200 - School: 02000 - Invalid School Type", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(2),
                new String[] {"Error: 0200 - School: 02010 - Invalid School Level", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(3),
                new String[] {"Error: 0200 - School: 02020 - Invalid Start Grade for School Type", "ErrGrp", "ErrInfo",
                        "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(4),
                new String[] {"Error: 0200 - School: 02030 - Invalid End Grade for School Type", "ErrGrp", "ErrInfo",
                        "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(5),
                new String[] {"Error: 0200 - School: 02040 - Missing Calendar", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(6),
                new String[] {"Error: 0200 - School: 02050 - Incorrect Session Days", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(7),
                new String[] {"Error: 0300 - Student03010 - Student SASID Missing", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "Schoolid", "StudentName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(8),
                new String[] {"Error: 0400 - Schedule: 04010 - Synchronize Schedule Term", "ErrGrp", "ErrInfo",
                        "DistrictID", "DistrictName", "Schoolid", "StudentName", "ObjectID", "Ref_ID", "ErrData"});
        return map;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#calcShowResults(java.lang.String)
     */
    @Override
    protected boolean calcShowResults(String rsHeader) {
        return (m_chkAll.booleanValue()) || ((m_chkSchool.booleanValue()) && rsHeader.equals("0200 - School"))
                || ((m_chkStudent.booleanValue()) && rsHeader.equals("0300 - Student"))
                || ((m_chkSchedule.booleanValue()) && rsHeader.equals("0400 - Schedule"));
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeParams()
     */
    @Override
    protected void initializeParams() {
        m_chkAll = (Boolean) getParameter("chkAll");
        m_chkSchool = (Boolean) getParameter("chkSchool");
        m_chkStudent = (Boolean) getParameter("chkStudent");
        m_chkSchedule = (Boolean) getParameter("chkSchedule");
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeQueries()
     */
    @Override
    protected String[] initializeQueries() {
        return new String[] {

                // Error: 0200 - School: 01000 - Invalid State Code
                "SELECT '0200 - School' ErrGrp, '01000 - Invalid State Code' ErrInfo, "
                        + sif_StateId_alias
                        + " DistrictID, "
                        + sif_LeaName_alias
                        + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, SKL_SCHOOL_ID ObjectID,  "
                        + sif_StateId_alias
                        + " Ref_ID,  "
                        + "CONCAT('Archive =  ', IFNULL(SKL_ARCHIVE_IND,''), '^', "
                        + "'Inactive =  ', IFNULL(SKL_INACTIVE_IND,''), '^', "
                        + "'State Code =  (', IFNULL("
                        + skl_Doe15_alias
                        + ",''), ')' "
                        + ") ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "    ON "
                        + skl_StateId_alias
                        + "= "
                        + sif_StateId_alias
                        + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (TRIM(IFNULL("
                        + skl_Doe15_alias
                        + ",'')) = '' "
                        + "    OR "
                        + skl_Doe15_alias
                        + " = "
                        + leftPad("LEFT(" + skl_Doe15_alias + ", 1)", "LENGTH(" + skl_Doe15_alias + ")  ",
                                "LEFT(" + skl_Doe15_alias
                                        + ", 1)")
                        + ") ",

                // Error: 0200 - School 02000 - Invalid School Type
                " SELECT '0200 - School', '02000 - Invalid School Type', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + " SASID, "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + "CONCAT('Archive = ', IFNULL(SKL_ARCHIVE_IND,''), '^', "
                        + "'Inactive = ', IFNULL(SKL_INACTIVE_IND,''), '^', "
                        + "'School Type = ', IFNULL(SKL_SCHOOL_TYPE_CODE,''), "
                        + "'(',IFNULL(RCD_CODE_FEDERAL,''),')') ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = 'rtbSchoolType' "
                        + "AND SKL_SCHOOL_TYPE_CODE = RCD_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND IFNULL(RCD_CODE_FEDERAL,'') NOT IN( "
                        + "'0013', '0787', '0789', '1302', '1304', '1981', '2397', '2399', '2400', "
                        + "'2402', '2403', '2602') ",

                // Error: 0200 - School: 02010 - Invalid School Level
                "SELECT '0200 - School', '02010 - Invalid School Level', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + " SASID, "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + "CONCAT('Archive = ', IFNULL(SKL_ARCHIVE_IND,''), '^', "
                        + "'Inactive = ', IFNULL(SKL_INACTIVE_IND,''), '^', "
                        + "'School Level = ', IFNULL(SKL_SCHOOL_LEVEL_CODE,''), "
                        + "'(',IFNULL(RCD_CODE_FEDERAL,''),')') ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = 'rtbSchoolLevel' "
                        + "AND SKL_SCHOOL_LEVEL_CODE = RCD_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND IFNULL(RCD_CODE_FEDERAL,'') NOT IN( "
                        + "'0013', '0787', '0789', '1302', '1304', '1981', '2397', '2399', "
                        + "'2400', '2402', '2403', '2602') ",

                // Error: 0200 - School: 02020 - Invalid Start Grade for School Type
                "SELECT '0200 - School', '02020 - Invalid Start Grade for School Type', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + " SASID, "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + "CONCAT('Archive = ', IFNULL(SKL_ARCHIVE_IND,''), '^', "
                        + "'Inactive = ', IFNULL(SKL_INACTIVE_IND,''), '^', "
                        + "'School Level = ', "
                        + sqlCastToChar("IFNULL(SKL_SCHOOL_LEVEL_CODE, '')", 50)
                        + ", '^', "
                        + "'Start Grade = ',"
                        + sqlCastToChar("IFNULL(SKL_START_GRADE,'')", 50)
                        + " "
                        + ", '^','End Grade = ',"
                        + sqlCastToChar("SKL_START_GRADE + SKL_NUMBER_OF_GRADES ", 50)
                        + ") ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = 'rtbSchoolType' "
                        + "AND SKL_SCHOOL_TYPE_CODE = RCD_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND ( " + "(RCD_CODE_FEDERAL IN('0789', '1304', '1981', '2397') AND "
                        + "SKL_START_GRADE > 3) " + "OR "
                        + "(RCD_CODE_FEDERAL IN('2399', '2400', '2602') AND " + "SKL_START_GRADE NOT BETWEEN 4 AND 8) "
                        + "OR "
                        + "(RCD_CODE_FEDERAL IN('2402', '2403') AND " + "SKL_START_GRADE < 5) " + ") ",

                // Error: 0200 - School: 02030 - Invalid End Grade for School Type
                "SELECT '0200 - School', '02030 - Invalid End Grade for School Type', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + ", "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + "CONCAT('Archive = ', IFNULL(SKL_ARCHIVE_IND,''), '^', "
                        + "'Inactive = ', IFNULL(SKL_INACTIVE_IND,''), '^', "
                        + "'School Level = ', IFNULL(SKL_SCHOOL_LEVEL_CODE, ''), '^', "
                        + "'Start Grade = ', "
                        + sqlCastToChar("IFNULL(SKL_START_GRADE,'')", 50)
                        + ", '^', "
                        + "'End Grade = ', "
                        + sqlCastToChar("SKL_START_GRADE + SKL_NUMBER_OF_GRADES", 50)
                        + ") ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = 'rtbSchoolType' "
                        + "AND SKL_SCHOOL_TYPE_CODE = RCD_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND ( " + "(RCD_CODE_FEDERAL IN('0789', '1304', '1981', '2397') AND "
                        + "SKL_START_GRADE + SKL_NUMBER_OF_GRADES NOT BETWEEN 5 AND 8) " + "OR "
                        + "(RCD_CODE_FEDERAL IN('2399', '2400', '2602') AND "
                        + "SKL_START_GRADE + SKL_NUMBER_OF_GRADES NOT BETWEEN 5 AND 9) " + "OR "
                        + "(RCD_CODE_FEDERAL IN('2402', '2403') AND " + "SKL_START_GRADE + SKL_NUMBER_OF_GRADES < 9) "
                        + ") ",

                // Error: 0200 - School: 02040 - Missing Calendar
                "SELECT '0200 - School', '02040 - Missing Calendar', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + ", "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + "CONCAT(IFNULL(SKL_SCHOOL_NAME,'') , ' missing calendar for ', "
                        + "IFNULL(CTX_CONTEXT_ID,'')) ErrData "
                        + "FROM SCHOOL "
                        + "JOIN ORGANIZATION "
                        + "ON UPPER(ORG_OID) = '*DST' "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN DISTRICT_SCHOOL_YEAR_CONTEXT "
                        + "ON ORG_CTX_OID_CURRENT = CTX_OID "
                        + "LEFT JOIN CALENDAR_SCHOOL "
                        + "ON SKL_OID = CAS_SKL_OID "
                        + "AND CTX_OID = CAS_CTX_OID "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (CAS_OID IS NULL) ",

                // Error: 0200 - School: 02050 - Incorrect Session Days
                "SELECT '0200 - School', '02050 - Incorrect Session Days', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", "
                        + "SKL_OID, SKL_SCHOOL_NAME, "
                        + skl_Doe15_alias
                        + ", "
                        + sif_StateId_alias
                        + " Ref_ID, "
                        + " "
                        + "CONCAT( "
                        + "IFNULL(SKL_SCHOOL_NAME,''), ' - ', "
                        + sqlCastToChar("IFNULL(CAS_DAYS_IN_SESSION,'[Empty]')", 50)
                        + ", "
                        + "' out of ', "
                        + sqlCastToChar("IFNULL(CAS_REQ_IN_SESSION,'[Empty]') ", 50)
                        + ") ErrData "
                        + "FROM SCHOOL "
                        + "JOIN ORGANIZATION "
                        + "ON UPPER(ORG_OID) = '*DST' "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "LEFT JOIN DISTRICT_SCHOOL_YEAR_CONTEXT "
                        + "ON ORG_CTX_OID_CURRENT = CTX_OID "
                        + "LEFT JOIN CALENDAR_SCHOOL "
                        + "ON SKL_OID = CAS_SKL_OID "
                        + "AND CTX_OID = CAS_CTX_OID "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND ((CAS_OID IS NOT NULL AND CAS_DAYS_IN_SESSION < CAS_REQ_IN_SESSION) "
                        + "OR (CAS_OID IS NULL)) ",

                // Error: 0300 - Student03010 - Student SASID Missing
                "SELECT '0300 - Student','03010 - Student SASID Missing', " + skl_StateId_alias + ", "
                        + sif_LeaName_alias
                        + ", SKL_SCHOOL_ID, STD_NAME_VIEW, " + "STD_OID, TRIM(IFNULL(STD_GUID, '')), " + " "
                        + "CONCAT('Name=',IFNULL(STD_NAME_VIEW,''), '^OID=', "
                        + "STD_OID, '^SASID=(', TRIM(IFNULL(STD_ID_STATE,'')), "
                        + "')' " + ") " + " ErrData " + "FROM STUDENT " + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES " + "ON " + skl_StateId_alias + " = " + sif_StateId_alias
                        + " "
                        + "WHERE STD_ENROLLMENT_STATUS = 'Active' " + "AND TRIM(IFNULL(STD_ID_STATE,'')) = '' ",

                // Error: 0400 - Schedule: 04010 - Synchronize Schedule Term
                "SELECT '0400 - Schedule', '04010 - Synchronize Schedule Term', "
                        + skl_StateId_alias
                        + ", "
                        + sif_LeaName_alias
                        + ", SKL_OID, SKL_SCHOOL_NAME, SCH_SCHEDULE_NAME, '', "
                        + "CONCAT('Schedule dates are not within District Context Cal. (Schedule: ',"
                        + sqlCastToChar("SCH_START_DATE", 50)
                        + ",' - ',"
                        + sqlCastToChar("SCH_END_DATE", 50)
                        + ",' District Cal: ',"
                        + sqlCastToChar("CTX_START_DATE", 50)
                        + ",' - ',"
                        + sqlCastToChar("CTX_END_DATE", 50)
                        + ",')') AS ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON "
                        + skl_StateId_alias
                        + " = "
                        + sif_StateId_alias
                        + " "
                        + "INNER JOIN SCHEDULE ON SKL_OID = SCH_SKL_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = SCH_CTX_OID "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') AND CTX_CONTEXT_ID = '2014-2015' "
                        + "AND (SCH_START_DATE < CTX_START_DATE OR SCH_END_DATE > CTX_END_DATE) "};

    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#replaceAliasesWithDbFields(java.lang.String)
     */
    @Override
    protected String replaceAliasesWithDbFields(String queryString) {
        queryString = queryString.contains(skl_StateId_alias) ? queryString.replace(skl_StateId_alias,
                getDatabaseFieldName(skl_StateId_alias)) : queryString;
        queryString = queryString.contains(sif_StateId_alias) ? queryString.replace(sif_StateId_alias,
                getDatabaseFieldName(sif_StateId_alias)) : queryString;
        queryString = queryString.contains(sif_LeaName_alias) ? queryString.replace(sif_LeaName_alias,
                getDatabaseFieldName(sif_LeaName_alias)) : queryString;
        queryString = queryString.contains(skl_Doe15_alias)
                ? queryString.replace(skl_Doe15_alias, getDatabaseFieldName(skl_Doe15_alias))
                : queryString;
        queryString =
                queryString.contains(std_AdjustedAttendance_alias) ? queryString.replace(std_AdjustedAttendance_alias,
                        getDatabaseFieldName(std_AdjustedAttendance_alias)) : queryString;
        queryString =
                queryString.contains(std_AdjustedMembership_alias) ? queryString.replace(std_AdjustedMembership_alias,
                        getDatabaseFieldName(std_AdjustedMembership_alias)) : queryString;
        return queryString;
    }
}
