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
 * The Class MultiSqlGridReportSIMS2Data.
 */
public class MultiSqlGridReportSIMS2Data extends MultiSqlGridReportBase {
    // Strings for aliases
    private static final String skl_StateId_alias = "skl-sif-district-id";
    private static final String sif_StateId_alias = "sif-district-id";
    private static final String sif_LeaName_alias = "sif-lea-name";
    private static final String std_DoeStatus_alias = "DOE Status";
    private static final String std_AdjustedAttendance_alias = "DOE Adjusted Attendance";
    private static final String std_AdjustedMembership_alias = "DOE Adjusted Membership";
    private static final String std_DaysInAttendance_alias = "DOE 17";
    private static final String std_DaysInMembership_alias = "DOE 18";
    private static final String std_EnglishLearnersProficiency_alias = "DOE 26";
    private static final String std_AlternativeLanguage_alias = "DOE 27";
    private static final String std_TitleISchool_alias = "DOE 28";
    private static final String std_ChSeventyFour_alias = "DOE 31";
    private static final String std_SpedThreeToFive_alias = "DOE 32";
    private static final String std_HighSchoolComp_alias = "DOE 33";
    private static final String std_SpecialEdSix_alias = "DOE 34";
    private static final String std_CteTypeProgram_alias = "DOE 35";
    private static final String std_SpedNatureDisability_alias = "DOE 36";
    private static final String std_GraduateCompleteMass_alias = "DOE 37";
    private static final String std_SpedLevelOfNeed_alias = "DOE 38";
    private static final String std_FiveZeroFour_alias = "DOE 39";
    private static final String std_SpedEvaluationResults_alias = "DOE 40";
    private static final String std_ChapterSeventyFourSpecial_alias = "DOE 42";
    private static final String std_ChapSeventyFourProgramPart_alias = "DOE 43";
    private static final String std_NonChapterSeventyFour_alias = "DOE 44";
    private static final String std_DaysInTruancy_alias = "DOE 52";
    private static final String std_GiftedTalented_alias = "SIF Gifted Talented";

    Boolean m_chkAll = null;
    Boolean m_chkSchool = null;
    Boolean m_chkStudent = null;

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#buildQueryHeaderMap()
     */
    @Override
    protected Map<Integer, String[]> buildQueryHeaderMap() {
        Map<Integer, String[]> map = new HashMap<Integer, String[]>();
        map.put(Integer.valueOf(0), new String[] {
                "Error: 0300 - Student: 03220 - DOE 26: ELL Program Status, Acceptable Values are 00 - 04 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(1), new String[] {
                "Error: 0300 - Student: 03230 - DOE 27: Alternative Language, 00000000 should be used when not enrolled in alt program",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(2), new String[] {
                "Error: 0300 - Student: 03240 - DOE 28: Title I School Choice, Acceptable Values are 00 - 01 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(3), new String[] {
                "Error: 0300 - Student: 03250 - DOE 31: Chapter 74 Competency Attainment, Acceptable Values are 00 - 14,500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(4), new String[] {
                "Error: 0300 - Student: 03260 - DOE 32: Special Education Placement,Age 3-5, Acceptable Values are 00,01,05,30,31,32,34,36,38,42,44,45,46,48 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(5), new String[] {
                "Error: 0300 - Student: 03270 - DOE 33: High School Completer Plan, Acceptable Values are 00-09,500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(6), new String[] {
                "Error: 0300 - Student: 03280 - DOE 34: Special Education Placement, Ages 6-21, Acceptable Values are 00,01,10,20,40,41,50,60,70,90 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(7), new String[] {
                "Error: 0300 - Student: 03290 - DOE 35: CTE Type of Program, Acceptable Values are 00,02,04,14 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});

        map.put(Integer.valueOf(8), new String[] {
                "Error: 0300 - Student: 03300 - DOE 36: Special Education Nature of Disability, Acceptable Values are 00-13,500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});

        map.put(Integer.valueOf(9), new String[] {
                "Error: 0300 - Student: 03310 - DOE 37: Graduate, Completed Massachusetts Core Curriculum, Acceptable Values are 00-02 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(10), new String[] {
                "Error: 0300 - Student: 03320 - DOE 38: Secial Education - Level of Need, Acceptable Values are 00-02 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(11), new String[] {
                "Error: 0300 - Student: 03330 - DOE 39: 504 Plan, Acceptable Values are 00-01 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(12), new String[] {
                "Error: 0300 - Student: 03340 - DOE 40: Special Education - Evaluation Results, Acceptable Values are 00-09 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(13), new String[] {
                "Error: 0300 - Student: 03350 - DOE 42: Chapter 74 Special Population, Acceptable Values are 02,500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});

        map.put(Integer.valueOf(14), new String[] {
                "Error: 0300 - Student: 03360 - DOE 43: Chapter 74 Program Participation, Acceptable Six-digit Classifications of Instructional Program codes or 500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(15), new String[] {
                "Error: 0300 - Student: 03370 - DOE 44: Non-Chapter 74 Program Participation, Acceptable four-digit code and state title or 500 (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(16), new String[] {
                "Error: 0300 - Student: 03380 - Gifted and Talented Program, Acceptable values are Yes,No,Unknown (Reference State Code)",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(17), new String[] {
                "Error: 0300 - Student: 03390 - Days in Attendance cannot be > Days in Membership or Days in Att + Days truancy cannot be > Membership",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});
        map.put(Integer.valueOf(18), new String[] {
                "Error: 0300 - Student: 03400 - Days in Attendance cannot be > Days in Membership or Days in Att + Days truancy cannot be > Membership",
                "ErrGrp",
                "ErrInfo",
                "DistrictID",
                "DistrictName",
                "SchoolOid",
                "SchoolName",
                "ObjectID",
                "Ref_ID",
                "ErrData"});

        return map;
    }

    /**
     * @see com.x2dev.reports.statereporting.ma.MultiSqlGridReportBase#calcShowResults(java.lang.String)
     */
    @Override
    protected boolean calcShowResults(String rsHeader) {
        return (m_chkAll.booleanValue()) ||
                ((m_chkSchool.booleanValue()) && rsHeader.equals("0200 - School")) ||
                ((m_chkStudent.booleanValue()) && rsHeader.equals("0300 - Student"));
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

                // Error: 0300 - Student: 03220 - DOE 26: ELL Program Status, Acceptable Values are
                // 00 - 04 (Reference State Code) (19)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03220 - DOE_26:ELL Program Status is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ ELL Program Status = ',IFNULL("
                        + std_EnglishLearnersProficiency_alias + ",'Not enrolled in an ELLP')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'ELL Status Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_EnglishLearnersProficiency_alias
                        + ",'Not enrolled in an ELLP') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03230 - DOE 27: Alternative Language, 00000000 should be
                // used when not enrolled in alt program (Reference State Code) (20)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03230 - DOE_27:Alternative Language is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ ELL Program Status = ',IFNULL("
                        + std_AlternativeLanguage_alias + ",'N/A')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Alternative Education Programs' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_AlternativeLanguage_alias + ",'N/A') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03240 - DOE 28: Title I School Choice, Acceptable Values
                // are 00 - 01 (Reference State Code) (21)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03240 - DOE_28:Title I School Choice is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ ELL Program Status = ',IFNULL("
                        + std_TitleISchool_alias + ",'Does not apply')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Title 1 School Choice Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_TitleISchool_alias
                        + ",'Does not apply') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03250 - DOE 31: Chapter 74 Competency Attainment,
                // Acceptable Values are 00 - 14,500 (Reference State Code) (22)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03250 - DOE_31:Chapter 74 Competency Attainment is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Chapter 74 Competency Attainment = ',IFNULL("
                        + std_ChSeventyFour_alias + ",'Does not apply')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'CTE Competency Attainment Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_ChSeventyFour_alias
                        + ",'Does not apply') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03260 - DOE 32: Special Education Placement,Age 3-5,
                // Acceptable Values are 00,01,05,30,31,32,34,36,38,42,44,45,46,48 (Reference State
                // Code) (23)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03260 - DOE_32:Special Education Placement,Age 3-5 is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Special Ed Placement, Age 3-5 = ',IFNULL("
                        + std_SpedThreeToFive_alias + ",'<<NULL>>')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Educational Environment Codes (3-5)' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_SpedThreeToFive_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (IFNULL(" + std_SpedThreeToFive_alias + ", '') <> '') "
                        + "AND (IFNULL(RCD_CODE_STATE,'999') NOT IN ('00', '01', '05', '30', '31', '32', '34', '36', '38', '42', '44', '45', '46', '48')) ",

                // Error: 0300 - Student: 03270 - DOE 33: High School Completer Plan, Acceptable
                // Values are 00-09,500 (Reference State Code) (24)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03220 - DOE_33:High School Completer Plan is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ High School Completer Plan = ',IFNULL("
                        + std_HighSchoolComp_alias + ",'Does not apply')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'High School Completer Plans' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_HighSchoolComp_alias
                        + ",'Does not apply' ) "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03280 - DOE 34: Special Education Placement, Ages 6-21,
                // Acceptable Values are 00,01,10,20,40,41,50,60,70,90 (Reference State Code) (25)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03230 - DOE_34:Special Education Placement, Ages 6-21 is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Special Education Placement, Ages 6-21 = ',IFNULL("
                        + std_SpecialEdSix_alias + ",'Not special ed')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Educational Environment Codes (6-21)' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_SpecialEdSix_alias
                        + ",'Not special ed') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03290 - DOE 35: CTE Type of Program, Acceptable Values are
                // 00,02,04,14 (Reference State Code) (26)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03290 - DOE_35:CTE Type of Program is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ CTE Type of Program = ',IFNULL("
                        + std_CteTypeProgram_alias + ",'Not in career tech prog')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'CTE Program Types' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_CteTypeProgram_alias
                        + ",'Not in career tech prog') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03300 - DOE 36: Special Education Nature of Disability,
                // Acceptable Values are 00-13,500 (Reference State Code) (27)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03300 - DOE_36:Special Education Nature of Disability is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Special Education Nature of Disability = ',IFNULL("
                        + std_SpedNatureDisability_alias + ",'<<NULL>>')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'SPED Primary Disabilities' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_SpedNatureDisability_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (" + std_SpedNatureDisability_alias + " IS NOT NULL) "
                        + "AND (IFNULL(RCD_CODE_STATE,'999') NOT IN ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '500')) ",

                // Error: 0300 - Student: 03310 - DOE 37: Graduate, Completed Massachusetts Core
                // Curriculum, Acceptable Values are 00-02 (Reference State Code) (28)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03310 - DOE_37:Graduate, Completed Massachusetts Core Curriculum is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Graduate, Completed Massachusetts Core Curriculum = ',IFNULL("
                        + std_GraduateCompleteMass_alias + ",'Student is not a graduate')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'Graduation Status' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_GraduateCompleteMass_alias
                        + ",'Student is not a graduate') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03320 - DOE 38: Secial Education - Level of Need,
                // Acceptable Values are 00-02 (Reference State Code) (29)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03320 - DOE_38:Secial Education - Level of Need is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Special Education - Level of Need = ',IFNULL("
                        + std_SpedLevelOfNeed_alias + ",'<<NULL>>')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'SPED Levels of Need' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_SpedLevelOfNeed_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (" + std_SpedLevelOfNeed_alias + " IS NOT NULL) "
                        + "AND (RCD_CODE_STATE IS NULL) ",


                // Error: 0300 - Student: 03330 - DOE 39: 504 Plan, Acceptable Values are 00-01
                // (Reference State Code) (30)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03330 - DOE_39:504 Plan is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ 504 This Year = ',IFNULL("
                        + std_FiveZeroFour_alias + ",'Not 504')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME LIKE '%In 504 Plan%' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_FiveZeroFour_alias + ",'Not 504') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03340 - DOE 40: Special Education - Evaluation Results,
                // Acceptable Values are 00-09 (Reference State Code) (31)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03340 - DOE_40:Special Education - Evaluation Results is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Special Education - Evaluation Results = ',IFNULL("
                        + std_SpedEvaluationResults_alias + ",'Student not SPED/no eval')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'SPED Evaluation Results' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_SpedEvaluationResults_alias
                        + ",'Student not SPED/no eval') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03350 - DOE 42: Chapter 74 Special Population, Acceptable
                // Values are 02,500 (Reference State Code) (32)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03350 - DOE_42:Chapter 74 Special Population is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Chapter 74 Special Population = ',IFNULL("
                        + std_ChapterSeventyFourSpecial_alias + ",'Does not apply')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'CTE Special Population Codes' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = IFNULL(" + std_ChapterSeventyFourSpecial_alias
                        + ",'Does not apply') "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (RCD_CODE IS NULL) ",

                // Error: 0300 - Student: 03360 - DOE 43: Chapter 74 Program Participation,
                // Acceptable Six-digit Classifications of Instructional Program codes or 500
                // (Reference State Code) (33)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03360 - DOE_43:Chapter 74 Program Participation is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Chapter 74 Program Participation = ',IFNULL("
                        + std_ChapSeventyFourProgramPart_alias + ",'<<NULL>>')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'CTE Chapter 74 Programs' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_ChapSeventyFourProgramPart_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (" + std_ChapSeventyFourProgramPart_alias + " IS NOT NULL) "
                        + "AND (RCD_CODE_STATE IS NULL) ",

                // Error: 0300 - Student: 03370 - DOE 44: Non-Chapter 74 Program Participation,
                // Acceptable four-digit code and state title or 500 (Reference State Code) (34)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ",'03370 - DOE_44:Non-Chapter 74 Program Participation is incorrect' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Local ID = ',IFNULL(STD_ID_LOCAL,'NULL'),' ^ Non-Chapter 74 Program Participation = ',IFNULL("
                        + std_NonChapterSeventyFour_alias + ",'<<NULL>>')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "LEFT JOIN REF_TABLE "
                        + "ON RTB_USER_NAME = 'CTE Non-Chapter 74 Programs' "
                        + "LEFT JOIN REF_CODE "
                        + "ON RCD_RTB_OID = RTB_OID AND RCD_CODE = " + std_NonChapterSeventyFour_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (" + std_NonChapterSeventyFour_alias + " IS NOT NULL) "
                        + "AND (RCD_CODE_STATE IS NULL) ",

                // Error: 0300 - Student: 03380 - Gifted and Talented Program, Acceptable values are
                // Yes,No,Unknown (Reference State Code) (35)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ", '03380 - Gifted and Talented Program is incorrect'  ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Gifted Value = ',IFNULL(" + std_GiftedTalented_alias + ",'Unknown')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND IFNULL(" + std_GiftedTalented_alias
                        + ",'Unknown') NOT IN ('Yes','No','Unknown','0','1') ",

                // Error: 0300 - Student Attendance: 03390 - Days in Attendance cannot be > Days in
                // Membership or Days in Att + Days truancy cannot be > Membership (36)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ", '03390 - Days in Attendance error' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Days in Attendance = ',IFNULL(" + std_DaysInAttendance_alias
                        + ",'0'),' ^ Days in Truancy = ',IFNULL(" + std_DaysInTruancy_alias
                        + ",'0'),' ^ Days in Membership = ',IFNULL(" + std_DaysInMembership_alias + ",'0')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND CAST(IFNULL(" + std_DaysInAttendance_alias + ",'0') AS DECIMAL(11,4)) "
                        + "+ CAST(IFNULL(" + std_DaysInTruancy_alias + ",'0') AS DECIMAL(11,4)) "
                        + "> CAST(IFNULL(" + std_DaysInMembership_alias + ",'0') AS DECIMAL(11,4)) ",


                // Error: 0300 - Student: 03400 - Days in Attendance cannot be > Days in Membership
                // or Days in Att + Days truancy cannot be > Membership (37)
                "SELECT "
                        + "'0300 - Student' ErrGrp "
                        + ", '03400 - Days in Attendance error' ErrInfo "
                        + "," + sif_StateId_alias + " DistrictID "
                        + "," + sif_LeaName_alias + " DistrictName "
                        + ",SKL_OID SchoolOid "
                        + ",SKL_SCHOOL_NAME SchoolName "
                        + ",IFNULL(STD_ID_STATE,'NULL') ObjectID "
                        + "," + sif_StateId_alias + " Ref_ID  "
                        + ",CONCAT('Days in Attendance = ',IFNULL(" + std_DaysInAttendance_alias
                        + ",'0'),' ^ Days in Adjust Mem = ',IFNULL(" + std_AdjustedMembership_alias
                        + ",'0'),' ^ Days in Membership = ',IFNULL(" + std_DaysInMembership_alias + ",'0')) ErrData "
                        + "FROM STUDENT "
                        + "INNER JOIN SCHOOL "
                        + "ON STD_SKL_OID = SKL_OID "
                        + "LEFT JOIN ORGANIZATION_ATTRIBUTES "
                        + "ON " + skl_StateId_alias + "= " + sif_StateId_alias + " "
                        + "WHERE (SKL_ARCHIVE_IND = 0 AND SKL_INACTIVE_IND = 0  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%OUT%'  "
                        + "AND UPPER(SKL_SCHOOL_NAME) NOT LIKE '%HISTORY%') "
                        + "AND " + std_DoeStatus_alias + " = 'Report' "
                        + "AND (IFNULL(" + std_DaysInAttendance_alias + ",'0') > (IFNULL(" + std_DaysInMembership_alias
                        + ",'0') + IFNULL(" + std_AdjustedMembership_alias + ",'0'))) "

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
        queryString = queryString.contains(std_DoeStatus_alias)
                ? queryString.replace(std_DoeStatus_alias, getDatabaseFieldName(std_DoeStatus_alias)) : queryString;
        queryString = queryString.contains(std_AdjustedAttendance_alias)
                ? queryString.replace(std_AdjustedAttendance_alias, getDatabaseFieldName(std_AdjustedAttendance_alias))
                : queryString;
        queryString = queryString.contains(std_AdjustedMembership_alias)
                ? queryString.replace(std_AdjustedMembership_alias, getDatabaseFieldName(std_AdjustedMembership_alias))
                : queryString;
        queryString = queryString.contains(std_EnglishLearnersProficiency_alias)
                ? queryString.replace(std_EnglishLearnersProficiency_alias,
                        getDatabaseFieldName(std_EnglishLearnersProficiency_alias))
                : queryString;
        queryString =
                queryString.contains(std_AlternativeLanguage_alias) ? queryString.replace(std_AlternativeLanguage_alias,
                        getDatabaseFieldName(std_AlternativeLanguage_alias)) : queryString;
        queryString = queryString.contains(std_TitleISchool_alias)
                ? queryString.replace(std_TitleISchool_alias, getDatabaseFieldName(std_TitleISchool_alias))
                : queryString;
        queryString = queryString.contains(std_ChSeventyFour_alias)
                ? queryString.replace(std_ChSeventyFour_alias, getDatabaseFieldName(std_ChSeventyFour_alias))
                : queryString;
        queryString = queryString.contains(std_SpedThreeToFive_alias)
                ? queryString.replace(std_SpedThreeToFive_alias, getDatabaseFieldName(std_SpedThreeToFive_alias))
                : queryString;
        queryString = queryString.contains(std_HighSchoolComp_alias)
                ? queryString.replace(std_HighSchoolComp_alias, getDatabaseFieldName(std_HighSchoolComp_alias))
                : queryString;
        queryString = queryString.contains(std_SpecialEdSix_alias)
                ? queryString.replace(std_SpecialEdSix_alias, getDatabaseFieldName(std_SpecialEdSix_alias))
                : queryString;
        queryString = queryString.contains(std_CteTypeProgram_alias)
                ? queryString.replace(std_CteTypeProgram_alias, getDatabaseFieldName(std_CteTypeProgram_alias))
                : queryString;
        queryString = queryString.contains(std_SpedNatureDisability_alias) ? queryString.replace(
                std_SpedNatureDisability_alias, getDatabaseFieldName(std_SpedNatureDisability_alias)) : queryString;
        queryString = queryString.contains(std_GraduateCompleteMass_alias) ? queryString.replace(
                std_GraduateCompleteMass_alias, getDatabaseFieldName(std_GraduateCompleteMass_alias)) : queryString;
        queryString = queryString.contains(std_SpedLevelOfNeed_alias)
                ? queryString.replace(std_SpedLevelOfNeed_alias, getDatabaseFieldName(std_SpedLevelOfNeed_alias))
                : queryString;
        queryString = queryString.contains(std_FiveZeroFour_alias)
                ? queryString.replace(std_FiveZeroFour_alias, getDatabaseFieldName(std_FiveZeroFour_alias))
                : queryString;
        queryString = queryString.contains(std_SpedEvaluationResults_alias) ? queryString.replace(
                std_SpedEvaluationResults_alias, getDatabaseFieldName(std_SpedEvaluationResults_alias)) : queryString;
        queryString = queryString.contains(std_ChapterSeventyFourSpecial_alias) ? queryString
                .replace(std_ChapterSeventyFourSpecial_alias, getDatabaseFieldName(std_ChapterSeventyFourSpecial_alias))
                : queryString;
        queryString = queryString.contains(std_ChapSeventyFourProgramPart_alias)
                ? queryString.replace(std_ChapSeventyFourProgramPart_alias,
                        getDatabaseFieldName(std_ChapSeventyFourProgramPart_alias))
                : queryString;
        queryString = queryString.contains(std_NonChapterSeventyFour_alias) ? queryString.replace(
                std_NonChapterSeventyFour_alias, getDatabaseFieldName(std_NonChapterSeventyFour_alias)) : queryString;
        queryString = queryString.contains(std_GiftedTalented_alias)
                ? queryString.replace(std_GiftedTalented_alias, getDatabaseFieldName(std_GiftedTalented_alias))
                : queryString;
        queryString = queryString.contains(std_DaysInAttendance_alias)
                ? queryString.replace(std_DaysInAttendance_alias, getDatabaseFieldName(std_DaysInAttendance_alias))
                : queryString;
        queryString = queryString.contains(std_DaysInMembership_alias)
                ? queryString.replace(std_DaysInMembership_alias, getDatabaseFieldName(std_DaysInMembership_alias))
                : queryString;
        queryString = queryString.contains(std_DaysInTruancy_alias)
                ? queryString.replace(std_DaysInTruancy_alias, getDatabaseFieldName(std_DaysInTruancy_alias))
                : queryString;

        return queryString;
    }
}
