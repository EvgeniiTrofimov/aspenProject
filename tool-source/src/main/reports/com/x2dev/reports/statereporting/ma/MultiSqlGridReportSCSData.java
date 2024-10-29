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
 * The Class MultiSqlGridReportSCSData.
 */
public class MultiSqlGridReportSCSData extends MultiSqlGridReportBase {
    // Strings for aliases
    private static final String skl_StateId_alias = "skl-sif-district-id";
    private static final String sif_StateId_alias = "sif-district-id";
    private static final String sif_LeaName_alias = "sif-lea-name";
    private static final String skl_Doe15_alias = "DOE 15";
    private static final String wa10_mtc_alias = "WA10-MTC";
    private static final String sif_Attending_School_alias = "SIF Attending School";
    private static final String std_AdjustedAttendance_alias = "DOE Adjusted Attendance";
    private static final String std_AdjustedMembership_alias = "DOE Adjusted Membership";

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#buildQueryHeaderMap()
     */
    @Override
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        Map<Integer, String[]> map = new HashMap<Integer, String[]>();
        map.put(Integer.valueOf(0),
                new String[] {
                        "Error: SCS03 - Course Attending School - Must be 8 digit Code of either the school or Off Campus abbreviation (CLBRVK12,CLBRVCLG)",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "ErrData"});
        map.put(Integer.valueOf(1),
                new String[] {
                        "Error: SCS04: Local Course Code - Local code must be identified and between 1-20 characters.",
                        "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "ErrData"});
        map.put(Integer.valueOf(2),
                new String[] {
                        "Error:SCS05 (W10-MTC): Subject Area Course - Needs to match the 5 digit State Code or 7 digit CIP Code. This check will make sure it is populated and is 5 or 7 digits. Please refer to http://www.doe.mass.edu/infoservices/data/epims/DHAppendices.pdf for the Codes.",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "ErrData"});
        map.put(Integer.valueOf(3),
                new String[] {
                        "Error: SCS09: Course Level - Number of Credits a student can earn for enrolling in and completing a given course. If no credit, ``not applicable`` code should be used (9999).",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "ErrData"});
        map.put(Integer.valueOf(4),
                new String[] {
                        "Error: SCS10: Course Credit Available - Number of Credits a student can earn for enrolling in and completing a given course. If no credit, ``not applicable`` code should be used (9999).",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "ErrData"});
        map.put(Integer.valueOf(5),
                new String[] {
                        "SCS11 - Number of Credits awarded to a student who successfully completes the course must be less or equal to Credits Attempted.",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "ErrData"});
        return map;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#calcShowResults(java.lang.String)
     */
    @Override
    protected boolean calcShowResults(String rsHeader) {
        return true;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeParams()
     */
    @Override
    protected void initializeParams() {
        return;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeQueries()
     */
    @Override
    protected String[] initializeQueries() {
        return new String[] {
                "SELECT "
                        + "'Course Attending School' "
                        + ",'Must be 8 digits.' "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('SCS03 Code= ',IFNULL(" + sif_Attending_School_alias + ",'NULL')) AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(" + sif_Attending_School_alias + ") <> 8 ) "
                        + ";",


                "SELECT "
                        + "'Local Course Code ' AS Err_Code "
                        + ",'Invalid Length.' AS Err_Description "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('Course length = '," + sqlCastToChar("LENGTH(IFNULL(CSK_COURSE_NUMBER,''))", 50)
                        + ") AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (CSK_COURSE_NUMBER IS NULL OR LENGTH(CSK_COURSE_NUMBER) > 20) "
                        + ";",


                "SELECT "
                        + "'Subject Area Course' "
                        + ",'Must match State Code or CIP Code.' "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('SCS05 Code= ',IFNULL(" + wa10_mtc_alias + ",'NULL')) AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (IFNULL(LENGTH(" + wa10_mtc_alias + "),'0') NOT IN ('5','7') ) "
                        + ";",


                "SELECT "
                        + "'Course Level' "
                        + ",'Code Missing.' "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('SCS09 Code = ',CSK_ACADEMIC_LEVEL) AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Academic Level Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = CSK_ACADEMIC_LEVEL "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (RCD_CODE_FEDERAL IS NULL) "
                        + ";",


                "SELECT "
                        + "'Course Credit Available' "
                        + ",'Invalid Entry.' "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('SCS10 Code = ',"
                        + sqlCastToChar("IFNULL(CAST(CSK_CREDIT AS DECIMAL(19, 2)),'9999')", 50) + ") AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (" + sqlStringIsNotNumeric("LEFT(IFNULL(CSK_COURSE_NUMBER,'9'),1)") + ") "
                        + ";",


                "SELECT "
                        + "'Completed Credits.' "
                        + ",' > Credits Attempted.' "
                        + "," + skl_StateId_alias + " "
                        + "," + sif_LeaName_alias + " "
                        + ",SKL_OID "
                        + ",SKL_SCHOOL_NAME "
                        + ",IFNULL(CSK_COURSE_NUMBER,'') AS COURSE_NUMBER "
                        + ",CONCAT('Student = ',IFNULL(STD_NAME_VIEW,''),' | Credit Offered: ',"
                        + sqlCastToChar("CSK_CREDIT", 50) + ",' Credit Gained: ',"
                        + sqlCastToChar("IFNULL(TRN_TOTAL_CREDIT,'0')", 50) + ") AS ErrData "
                        + "FROM COURSE_SCHOOL "
                        + "INNER JOIN COURSE ON CSK_CRS_OID = CRS_OID "
                        + "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CTX_OID = CRS_CTX_OID "
                        + "INNER JOIN STUDENT_TRANSCRIPT ON TRN_CSK_OID = CSK_OID AND TRN_CTX_OID = CTX_OID "
                        + "INNER JOIN STUDENT ON STD_OID = TRN_STD_OID "
                        + "INNER JOIN ORGANIZATION ON ORG_CTX_OID_CURRENT = CRS_CTX_OID "
                        + "INNER JOIN SCHOOL ON CSK_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0 AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%' AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (CSK_CREDIT < " + m_optimizer.sqlStringToNumeric("IFNULL(TRN_TOTAL_CREDIT,'0')") + ") "
                        + ";"
        };
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#replaceAliasesWithDbFields(java.lang.String)
     */
    @Override
    protected String replaceAliasesWithDbFields(String queryString) {
        queryString = queryString.contains(skl_StateId_alias)
                ? queryString.replace(skl_StateId_alias, getDatabaseFieldName(skl_StateId_alias)) : queryString;
        queryString = queryString.contains(sif_StateId_alias)
                ? queryString.replace(sif_StateId_alias, getDatabaseFieldName(sif_StateId_alias)) : queryString;
        queryString = queryString.contains(sif_LeaName_alias)
                ? queryString.replace(sif_LeaName_alias, getDatabaseFieldName(sif_LeaName_alias)) : queryString;
        queryString = queryString.contains(skl_Doe15_alias)
                ? queryString.replace(skl_Doe15_alias, getDatabaseFieldName(skl_Doe15_alias)) : queryString;
        queryString = queryString.contains(wa10_mtc_alias)
                ? queryString.replace(wa10_mtc_alias, getDatabaseFieldName(wa10_mtc_alias)) : queryString;
        queryString = queryString.contains(sif_Attending_School_alias)
                ? queryString.replace(sif_Attending_School_alias, getDatabaseFieldName(sif_Attending_School_alias))
                : queryString;
        queryString = queryString.contains(std_AdjustedAttendance_alias)
                ? queryString.replace(std_AdjustedAttendance_alias, getDatabaseFieldName(std_AdjustedAttendance_alias))
                : queryString;
        queryString = queryString.contains(std_AdjustedMembership_alias)
                ? queryString.replace(std_AdjustedMembership_alias, getDatabaseFieldName(std_AdjustedMembership_alias))
                : queryString;
        return queryString;
    }

}
