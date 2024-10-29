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
 * The Class MultiSqlGridReportEPIMSData.
 */
public class MultiSqlGridReportEPIMSData extends MultiSqlGridReportBase {
    // Strings for aliases
    private static final String skl_StateId_alias = "skl-sif-district-id";
    private static final String sif_StateId_alias = "sif-district-id";
    private static final String sif_LeaName_alias = "sif-lea-name";
    private static final String std_StaffStatus_alias = "SR09";

    // Input parameters
    private Boolean m_chkAll = null;
    private Boolean m_chkSchedule = null;
    private Boolean m_chkSchool = null;
    private Boolean m_chkStaff = null;
    private Boolean m_chkStudent = null;
    private Boolean m_chkStudentAttendance = null;

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#buildQueryHeaderMap()
     */
    @Override
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        Map<Integer, String[]> map = new HashMap<Integer, String[]>();
        map.put(Integer.valueOf(0),
                new String[] {"Error: 06010 - SR01: Staff must have 8 digit MEPID", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});

        map.put(Integer.valueOf(1),
                new String[] {"Error: 06020 - SR02: Staff should have Local ID between 0 and 20 Characters", "ErrGrp",
                        "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID",
                        "ErrData"});
        map.put(Integer.valueOf(2),
                new String[] {
                        "Error: 06030 - SR03: licensure information maintained in the Educator Licensure and Recruitment (ELAR) database. Acceptable Values: Alphanumeric between 2 and 20 characters. If this field is not applicable for the staff member use the Not Applicable code of â€œ00â€�",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(3), new String[] {
                "06040 - SR04: First Name must be between 1 and 30 characters and match staff roster record", "ErrGrp",
                "ErrInfo",
                "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});

        map.put(Integer.valueOf(4), new String[] {
                "Error: 06050 - SR05: Middle Name must be between 1 and 30 characters and match staff roster record",
                "ErrGrp", "ErrInfo",
                "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(5), new String[] {
                "Error: 06060 - SR06: Last Name must be between 1 and 30 characters and match staff roster record",
                "ErrGrp", "ErrInfo",
                "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(6),
                new String[] {"Error: 06070 - SR07: Date of Birth in format of mm/dd/yyyy", "ErrGrp", "ErrInfo",
                        "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(7),
                new String[] {
                        "Error: 06080 - SR08: Contains Staff Race/Ethnicity. This checks to make sure it contains a two digit code (per Race Matrix)",
                        "ErrGrp", "ErrInfo", "DistrictID", "DistrictName", "SchoolOid", "SchoolName", "ObjectID",
                        "Ref_ID", "ErrData"});
        map.put(Integer.valueOf(8),
                new String[] {"Error: 06090 - Email Address is missing", "ErrGrp", "ErrInfo", "DistrictID",
                        "DistrictName", "SchoolOid", "SchoolName", "ObjectID", "Ref_ID", "ErrData"});

        return map;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#calcShowResults(java.lang.String)
     */
    @Override
    protected boolean calcShowResults(String rsHeader) {
        return (m_chkAll.booleanValue()) || ((m_chkSchool.booleanValue()) && rsHeader.equals("0200 - School"))
                || ((m_chkStudent.booleanValue()) && rsHeader.equals("0300 - Student"))
                || ((m_chkSchedule.booleanValue()) && rsHeader.equals("0400 - Schedule"))
                || ((m_chkStudentAttendance.booleanValue()) && rsHeader.equals("0500 - Student Attendance"))
                || ((m_chkStaff.booleanValue()) && rsHeader.equals("0600 - Staff"));
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
        m_chkStudentAttendance = (Boolean) getParameter("chkStudentAttendance");
        m_chkStaff = (Boolean) getParameter("chkStaff");
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#initializeQueries()
     */
    @Override
    protected String[] initializeQueries() {
        return new String[] {

                // Error: 06010 - SR01: Staff must have 8 digit MEPID (0)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06010 - SR01: Staff MEPID  is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_ID_STATE ObjectID, "
                        + sif_StateId_alias + " Ref_ID "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ MEPID Length = ',LENGTH(IFNULL(STF_ID_STATE,'')))  ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(STF_ID_STATE,'')) <> 8) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06020 - SR02: Staff should have Local ID between 0 and 20 Characters (1)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06020 - SR02: Staff Local ID is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_ID_STATE ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ Local ID Length = ',LENGTH(IFNULL(STF_ID_LOCAL,''))) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(STF_ID_LOCAL,'')) NOT BETWEEN 1 AND 20) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06030 - SR03: licensure information maintained in the Educator Licensure
                // and Recruitment (ELAR) database. Acceptable Values: Alphanumeric between 2 and 20
                // characters. If this field is not applicable for the staff member use the Not
                // Applicable code of �00� (2)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06030 - SR03: Staff Licensure information is incorrect.' AS ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ License Number = ',IFNULL(SFC_CERTIFICATION_NUMBER,'NULL'),' ^ Length = ',LENGTH(IFNULL(SFC_CERTIFICATION_NUMBER,''))) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN STAFF_CERTIFICATION "
                        + "ON STF_OID = SFC_STF_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(SFC_CERTIFICATION_NUMBER,'00')) NOT BETWEEN 2 AND 20) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06040 - SR04: First Name must be between 1 and 30 characters and match
                // staff roster record (3)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06040 - SR04: Staff First Name is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ First Name = ',IFNULL(PSN_NAME_FIRST,'NULL'),' ^ Length = ',LENGTH(IFNULL(PSN_NAME_FIRST,''))) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON ON STF_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(PSN_NAME_FIRST,'')) NOT BETWEEN 1 AND 30) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06050 - SR05: Middle Name must be between 1 and 30 characters and match
                // staff roster record (4)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06050 - SR05: Staff Middle Name is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ Middle Name = ',IFNULL(PSN_NAME_MIDDLE,'NULL'),' ^ Length = ',"
                        + sqlCastToChar("LENGTH(IFNULL(PSN_NAME_MIDDLE,''))", 50) + ") ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON ON STF_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(PSN_NAME_MIDDLE,'  ')) NOT BETWEEN 1 AND 30) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06060 - SR06: Last Name must be between 1 and 30 characters and match
                // staff roster record (5)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06060 - SR06: Staff Last Name is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ Last Name = ',IFNULL(PSN_NAME_LAST,'NULL'),' ^ Length = ',LENGTH(IFNULL(PSN_NAME_LAST,''))) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON ON STF_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (LENGTH(IFNULL(PSN_NAME_LAST,'')) NOT BETWEEN 1 AND 30) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06070 - SR07: Date of Birth in format of mm/dd/yyyy (6)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06070 - SR07: Staff DOB is incorrect' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ DOB = ',"
                        + sqlCastToChar("IFNULL(PSN_DOB,'NULL')", 50) + ") ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON ON STF_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (IFNULL(PSN_DOB,'') NOT LIKE '%%%%-%%-%%') "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06080 - SR08: Contains Staff Race/Ethnicity. This checks to make sure it
                // contains a two digit code (per Race Matrix) (7)
                "SELECT "
                        + "'0600 - Staff' ErrGrp , '06080 - SR08: incorrect Race/Ethnicity.' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ Race/Ethnicity = ',IFNULL(RAC_RACE_CODE,'NULL')) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON ON STF_PSN_OID = PSN_OID "
                        + "LEFT JOIN PERSON_RACE ON RAC_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Race Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = RAC_RACE_CODE "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND RCD_CODE_STATE IS NULL "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' ",

                // Error: 06090 - Email Address is missing (8)
                "SELECT "
                        + "'0600 - Staff' ErrGrp, '06090 - Staff Email address is missing' ErrInfo "
                        + "," + skl_StateId_alias + " DistrictID, " + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid, SKL_SCHOOL_NAME SchoolName, STF_OID ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STF_ID_LOCAL,'NULL'),' ^ Email Address = ',IFNULL(PSN_EMAIL_01,'NULL')) ErrData "
                        + "FROM STAFF "
                        + "INNER JOIN PERSON "
                        + "ON STF_PSN_OID = PSN_OID "
                        + "INNER JOIN SCHOOL "
                        + "ON STF_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + " = " + sif_LeaName_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND (PSN_EMAIL_01 IS NULL) "
                        + "AND STF_STATUS ='Active' "
                        + "AND " + std_StaffStatus_alias + " <> 'Do Not Report' "

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
        queryString = queryString.contains(std_StaffStatus_alias) ? queryString.replace(std_StaffStatus_alias,
                getDatabaseFieldName(std_StaffStatus_alias)) : queryString;

        return queryString;
    }
}
