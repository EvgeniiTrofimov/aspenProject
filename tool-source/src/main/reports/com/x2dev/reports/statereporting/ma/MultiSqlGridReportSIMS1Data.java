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
 * The Class MultiSqlGridReportSIMS1Data.
 */
public class MultiSqlGridReportSIMS1Data extends MultiSqlGridReportBase {
    // Strings for aliases
    private static final String skl_StateId_alias = "skl-sif-district-id";
    private static final String sif_StateId_alias = "sif-district-id";
    private static final String sif_LeaName_alias = "sif-lea-name";
    private static final String std_DoeStatus_alias = "DOE Status";
    private static final String std_CityOfBirth_alias = "DOE 08";
    private static final String std_ReasonForReporting_alias = "DOE 11";
    private static final String skl_Doe15_alias = "DOE 15";
    private static final String std_LowIncomeStatus_alias = "DOE 19";
    private static final String std_TitleIParticipation_alias = "DOE 20";
    private static final String std_LepStudent_alias = "DOE 21";
    private static final String std_ImmigrationStatus_alias = "DOE 22";
    private static final String std_CountryofOrigin_alias = "DOE 23";
    private static final String std_FirstNativeLanguage_alias = "DOE 24";
    private static final String std_LimitedEnglishProficiency_alias = "DOE 25";

    // Input parameters
    Boolean m_chkAll = null;
    Boolean m_chkSchool = null;
    Boolean m_chkStudent = null;

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#buildQueryHeaderMap()
     */
    @Override
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        Map<Integer, String[]> map = new HashMap<Integer, String[]>();
        map.put(Integer.valueOf(0),
                new String[] {"Error: 0300 - Student: 03030 - Invalid Local ID", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(1), new String[] {
                "Error: 0300 - Student: 03040 - DOE 03:First Name needs to be between 1 and 32 characters",
                "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(2), new String[] {
                "Error: 0300 - Student: 03050 - DOE 04:Middle Name needs to be between 1 and 32 characters",
                "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(3), new String[] {
                "Error:  0300 - Student: 03060 - DOE 05:Last Name needs to be between 1 and 32 characters",
                "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(4),
                new String[] {"Error: 0300 - Student: 03070 - DOE 06:DOB must be between 8 and 10 characters", "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                        "ErrData"});
        map.put(Integer.valueOf(5), new String[] {
                "Error: 0300 - Student: 03080 - DOE 08:City of Birth should only be the city or town, and no punctuations",
                "ErrGrp",
                "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(6),
                new String[] {"Error: 0300 - Student: 03090 - DOE 09:Gender needs to be M, F or N", "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                        "ErrData"});
        map.put(Integer.valueOf(7),
                new String[] {
                        "Error: 0300 - Student: 03100 - DOE 10: Race Codes. This is check to make sure populated and reference to a 2 digit code in MA Matrix",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(8),
                new String[] {"Error: 0300 - Student: 03110 - DOE 11: Reason for Reporting Must be 01,02,03", "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "Schoolid", "StudentName", "ObjectID", "Ref_ID",
                        "ErrData"});
        map.put(Integer.valueOf(9),
                new String[] {"Error: 0300 - Student: 03120 - DOE 12: Enrollment Status. Must match State's Matrix",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "Schoolid", "StudentName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(10), new String[] {
                "Error: 0300 - Student: 03130 - DOE 13: Reason for Enrollment. Must match State's Matrix (01-11)",
                "ErrGrp", "ErrInfo",
                "DistrictID", "DistrictName", "Schoolid", "StudentName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(11), new String[] {
                "Error:  0200 - School: 02140 - DOE 15: School State ID must be 8 Digits based on District Code",
                "ErrGrp", "ErrInfo",
                "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(12),
                new String[] {"Error:  0300 - Student: 03150 - Hispanic indicator needs to be either 0 or 1", "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                        "ErrData"});
        map.put(Integer.valueOf(13),
                new String[] {"Error: 0300 - Student: 03160 - DOE 19: Low Income should be 00-02", "ErrGrp", "ErrInfo",
                        "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(14), new String[] {
                "Error: 0300 - Student: 03170 - DOE 20: Acceptable Title 1 Participation Values are 00 - 27 (Reference State Code)",
                "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(15), new String[] {
                "Error: 0300 - Student: 03180 - DOE 21: Acceptable Values are 00 - 02 (Reference State Code)", "ErrGrp",
                "ErrInfo",
                "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(16),
                new String[] {
                        "Error: 0300 - Student: 03190 - DOE 22: Emergency Immigration, Acceptable Values are 00 - 01. If DOE 22 = 01, then DOE 23 (Country of origin) must have input (Reference State Code)",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(17),
                new String[] {
                        "Error: 0300 - Student: 03200 - DOE 24: First Native Language must be in accordance to Language Code (Reference Federal Code)",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(18), new String[] {
                "Error: 0300 - Student: 03210 - DOE 25: Limited English Proficiency, Acceptable Values are 00 - 01 (Reference State Code)",
                "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                "ErrData"});

        return map;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#calcShowResults(java.lang.String)
     */
    @Override
    protected boolean calcShowResults(String rsHeader) {
        return (m_chkAll.booleanValue()) || ((m_chkSchool.booleanValue()) && rsHeader.equals("0200 - School"))
                || ((m_chkStudent.booleanValue()) && rsHeader.equals("0300 - Student"));
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeParams()
     */
    @Override
    protected void initializeParams() {
        m_chkAll = (Boolean) getParameter("chkAll");
        m_chkSchool = (Boolean) getParameter("chkSchool");
        m_chkStudent = (Boolean) getParameter("chkStudent");
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeQueries()
     */
    @Override
    protected String[] initializeQueries() {
        return new String[] {

                // Error: 0300 - Student: 03030 - Invalid Local ID - Needs to be between 1 and 32
                // characters (0)
                "SELECT '0300 - Student' ErrGrp, '03030 - Invalid Local ID ' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND (LENGTH(IFNULL(STD_ID_LOCAL,'')) NOT BETWEEN 1 AND 32 "
                        + "OR STD_ID_LOCAL IS NULL) ",

                // Error: 0300 - Student: 03040 - DOE 03:First Name needs to be between 1 and 32
                // characters (1)
                "SELECT '0300 - Student' ErrGrp, '03040 - DOE 03: Invalid First Name' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ First Name = ',IFNULL(PSN_NAME_FIRST,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND (LENGTH(IFNULL(PSN_NAME_FIRST,'')) NOT BETWEEN 1 AND 32 "
                        + "OR PSN_NAME_FIRST IS NULL) ",

                // Error: 0300 - Student: 03050 - DOE 04:Middle Name needs to be between 1 and 32
                // characters (2)
                "SELECT '0300 - Student' ErrGrp, '03050 - DOE 04: Invalid Middle Name' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Middle Name = ',IFNULL(PSN_NAME_MIDDLE,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND (LENGTH(IFNULL(PSN_NAME_MIDDLE,' ')) NOT BETWEEN 1 AND 32) ",

                // Error: 0300 - Student: 03060 - DOE 05:Last Name needs to be between 1 and 32
                // characters (3)
                "SELECT '0300 - Student' ErrGrp, '03060 - DOE 05: Invalid Last Name' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Last Name = ',IFNULL(PSN_NAME_LAST,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND (LENGTH(IFNULL(PSN_NAME_LAST,'')) NOT BETWEEN 1 AND 32 "
                        + "OR PSN_NAME_LAST IS NULL) ",

                // Error: 0300 - Student: 03070 - DOE 06:DOB must be between 8 and 10 characters (4)
                "SELECT '0300 - Student' ErrGrp, '03070 - DOE 06: DOB is Incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Last Name = '," + sqlCastToChar("IFNULL(PSN_DOB,'NULL')", 50)
                        + ") ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND LENGTH(" + sqlCastToChar("IFNULL(PSN_DOB,'')", 50) + ") NOT BETWEEN 8 AND 10 ",

                // Error: 0300 - Student: 03080 - DOE 08:City of Birth should only be the city or
                // town, and no punctuations (5)
                "SELECT '0300 - Student' ErrGrp, '03080 - DOE 08: City of Birth is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ City of Birth = ',IFNULL( " + std_CityOfBirth_alias
                        + ",'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND (IFNULL( " + std_CityOfBirth_alias + ",'') LIKE '%,%' OR IFNULL( "
                        + std_CityOfBirth_alias + ",'') LIKE '%.%' "
                        + "OR IFNULL( " + std_CityOfBirth_alias + ",'') LIKE '%;%' OR IFNULL( " + std_CityOfBirth_alias
                        + ",'') LIKE '%:%') ",

                // Error: 0300 - Student: 03090 - DOE 09:Gender needs to be either M, F or N (6)
                "SELECT '0300 - Student' ErrGrp, '03090 - DOE 09: Gender is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Gender Code = ',IFNULL(PSN_GENDER_CODE,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND IFNULL(PSN_GENDER_CODE,'') NOT IN ('M','F','N') ",

                // Error: 0300 - Student: 03100 - DOE 10: Race Codes. This is check to make sure
                // populated and reference to a 2 digit code in MA Matrix (7)
                "SELECT '0300 - Student' ErrGrp, '03100 - DOE 10: Race Code is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Race Code = ',IFNULL(RAC_RACE_CODE,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "LEFT JOIN    PERSON_RACE "
                        + "ON RAC_PSN_OID = PSN_OID "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN       REF_TABLE  "
                        + "ON       RTB_USER_NAME = 'Race Codes' "
                        + "LEFT JOIN    REF_CODE "
                        + "ON       RCD_RTB_OID = RTB_OID AND RCD_CODE = RAC_RACE_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",


                // Error: 0300 - Student: 03110 - DOE 11: Reason for Reporting Must be 01,02,03 (8)
                "SELECT '0300 - Student' ErrGrp, '03110 - DOE 11: Reason for Reporting is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Reason for Reporting = ',IFNULL("
                        + std_ReasonForReporting_alias + ",'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN       REF_TABLE  "
                        + "ON       RTB_USER_NAME = 'Reasons for Reporting' "
                        + "LEFT JOIN    REF_CODE "
                        + "ON       RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_ReasonForReporting_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0300 - Student: 03120 - DOE 12: Enrollment Status. Must match State's
                // Matrix (9)
                "SELECT '0300 - Student' ErrGrp, '03120 - DOE 12: Enrollment Status is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Reason for Reporting = ',IFNULL(STD_ENROLLMENT_STATUS,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN       REF_TABLE  "
                        + "ON       RTB_USER_NAME = 'Enrollment Status Codes' "
                        + "LEFT JOIN    REF_CODE "
                        + "ON       RCD_RTB_OID = RTB_OID AND RCD_CODE = STD_ENROLLMENT_STATUS "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0300 - Student: 03130 - DOE 13: Reason for Enrollment. Must match State's
                // Matrix (01-11) (10)
                "SELECT '0300 - Student' ErrGrp, '03120 - DOE 13: Enrollment Status is incorrect' ErrInfo, "
                        + sif_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName, "
                        + "SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, IFNULL(STD_ID_STATE,'NULL') ObjectID,  "
                        + sif_StateId_alias + " Ref_ID,  "
                        + "CONCAT('Local ID = ', "
                        + "IFNULL(STD_ID_LOCAL,'NULL'),' ^ Reason for Reporting = ',IFNULL(STD_ENROLLMENT_STATUS,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN   SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN    ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN       REF_TABLE  "
                        + "ON       RTB_USER_NAME = 'Enrollment Status Codes' "
                        + "LEFT JOIN    REF_CODE "
                        + "ON       RCD_RTB_OID = RTB_OID AND RCD_CODE = STD_ENROLLMENT_STATUS "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + "= 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0200 - School: 02140 - DOE 15: School State ID must be 8 Digits based on
                // District Code (11)
                "SELECT "
                        + "'0200 - School' ErrGrp "
                        + ",'02140 - DOE 15 - School State ID is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + "," + skl_Doe15_alias + " ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID "
                        + ",CONCAT('DOE 15 = ',IFNULL(" + skl_Doe15_alias + ",'NULL')) ErrData "
                        + "FROM SCHOOL "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND LENGTH(IFNULL(" + skl_Doe15_alias + ",'NULL')) <> 8 ",

                // Error: 0300 - Student: 03150 - Hispanic indicator needs to be either 0 or 1 (12)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03140 - Hispanic Indicator is incorrect ' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT(' Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Hispanic Ind. = ',IFNULL(PSN_HISPANIC_LATINO_IND,'NULL')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND IFNULL(PSN_HISPANIC_LATINO_IND,'') NOT IN ('0','1') ",

                // Error: 0300 - Student: 03160 - DOE 19: Low Income should be 00-02 (State Code in
                // Ref Table) (13)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03160 - DOE 19: Low Income Status is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Low Income Status = ',IFNULL("
                        + std_LowIncomeStatus_alias + ",'Paid')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Low Income Status Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_LowIncomeStatus_alias + ",'Paid') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0300 - Student: 03170 - DOE 20: Acceptable Title 1 Participation Values
                // are 00 - 27 (Reference State Code) (14)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03110 - DOE 20:Title 1 Participation is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Title 1 = ',IFNULL("
                        + std_TitleIParticipation_alias + ",'Not Title I')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Title 1 Participation Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_TitleIParticipation_alias
                        + ",'Not Title I') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0300 - Student: 03180 - DOE 21: Acceptable Values are 00 - 02 (Reference
                // State Code) (15)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03180 - DOE 21:LEP Students in First Year is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ LEP Students = ',IFNULL("
                        + std_LepStudent_alias + ",'Does not apply')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN PERSON "
                        + "ON STD_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'LEP 1st Year Status Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_LepStudent_alias
                        + ",'Does not apply') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND RCD_CODE_STATE IS NULL ",

                // Error: 0300 - Student: 03190 - DOE 22: Emergency Immigration, Acceptable Values
                // are 00 - 01. If DOE 22 = 01, then DOE 23 (Country of origin) must have input
                // (Reference State Code) (16)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03190 - DOE 22:Immigration and/or DOE 23 Country of Origin incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',STD_ID_LOCAL,' ^ Immigration Status = ',IFNULL("
                        + std_ImmigrationStatus_alias + ",'No'), ' ^ Country of Origin = ',IFNULL("
                        + std_CountryofOrigin_alias + ",'500')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Immigration Status Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_ImmigrationStatus_alias + ",'No') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE_STATE IS NULL OR (IFNULL(" + std_CountryofOrigin_alias
                        + ",'') = '' AND RCD_CODE_STATE = '01')) ",

                // Error: 0300 - Student: 03200 - DOE 24: First Native Language must be in
                // accordance to Language Code (Reference Federal Code) (17)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03200 - DOE 24:First Native Language is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Language Code = ',IFNULL("
                        + std_FirstNativeLanguage_alias + ",'English')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Language Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_FirstNativeLanguage_alias
                        + ",'English') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03210 - DOE 25: Limited English Proficiency, Acceptable
                // Values are 00 - 01 (Reference State Code) (18)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03210 - DOE 25:Limited English Proficiency is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Limited English Proficiency = ',IFNULL("
                        + std_LimitedEnglishProficiency_alias + ",'Classwork in English')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'LEP Status Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_LimitedEnglishProficiency_alias
                        + ",'Classwork in English') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) "
        };
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
        queryString = queryString.contains(std_DoeStatus_alias) ? queryString.replace(std_DoeStatus_alias,
                getDatabaseFieldName(std_DoeStatus_alias)) : queryString;
        queryString = queryString.contains(skl_Doe15_alias)
                ? queryString.replace(skl_Doe15_alias, getDatabaseFieldName(skl_Doe15_alias))
                : queryString;
        queryString = queryString.contains(std_CityOfBirth_alias) ? queryString.replace(std_CityOfBirth_alias,
                getDatabaseFieldName(std_CityOfBirth_alias)) : queryString;
        queryString =
                queryString.contains(std_ReasonForReporting_alias) ? queryString.replace(std_ReasonForReporting_alias,
                        getDatabaseFieldName(std_ReasonForReporting_alias)) : queryString;
        queryString = queryString.contains(std_LowIncomeStatus_alias) ? queryString.replace(std_LowIncomeStatus_alias,
                getDatabaseFieldName(std_LowIncomeStatus_alias)) : queryString;
        queryString =
                queryString.contains(std_TitleIParticipation_alias) ? queryString.replace(std_TitleIParticipation_alias,
                        getDatabaseFieldName(std_TitleIParticipation_alias)) : queryString;
        queryString = queryString.contains(std_LepStudent_alias) ? queryString.replace(std_LepStudent_alias,
                getDatabaseFieldName(std_LepStudent_alias)) : queryString;
        queryString =
                queryString.contains(std_ImmigrationStatus_alias) ? queryString.replace(std_ImmigrationStatus_alias,
                        getDatabaseFieldName(std_ImmigrationStatus_alias)) : queryString;
        queryString = queryString.contains(std_CountryofOrigin_alias) ? queryString.replace(std_CountryofOrigin_alias,
                getDatabaseFieldName(std_CountryofOrigin_alias)) : queryString;
        queryString =
                queryString.contains(std_FirstNativeLanguage_alias) ? queryString.replace(std_FirstNativeLanguage_alias,
                        getDatabaseFieldName(std_FirstNativeLanguage_alias)) : queryString;
        queryString = queryString.contains(std_LimitedEnglishProficiency_alias)
                ? queryString.replace(std_LimitedEnglishProficiency_alias,
                        getDatabaseFieldName(std_LimitedEnglishProficiency_alias))
                : queryString;

        return queryString;
    }
}
