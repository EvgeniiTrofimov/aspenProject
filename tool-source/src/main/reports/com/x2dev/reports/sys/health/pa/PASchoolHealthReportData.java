/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisExtendedDataDictionary;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.health.HealthAliases;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "California school immunization record" report.
 *
 * @author X2 Development Corporation
 */
public class PASchoolHealthReportData extends ReportJavaSourceNet {

    /**
     * Categories
     */
    private static final String CATEGORY_DTP = "Diphtheria Tetanus Pertussis";
    private static final String CATEGORY_HEPA = "Hepatitis A";
    private static final String CATEGORY_HEPB = "Hepatitis B";
    private static final String CATEGORY_HIB = "Haemophilus influenzae type b";
    private static final String CATEGORY_HPV = "Human Papillomavirus";
    private static final String CATEGORY_MENING = "Meningococcal";
    private static final String CATEGORY_MMR = "Measles Mumps Rubella";
    private static final String CATEGORY_PCV = "Penumococcal Conjugate";
    private static final String CATEGORY_POLIO = "Polio";
    private static final String CATEGORY_ROT = "Rotavirus";
    private static final String CATEGORY_VAR = "Varicella";

    /**
     * Series Ids
     */
    private static final Collection SERIES_ID_HEPA = Arrays.asList("HepA");
    private static final Collection SERIES_ID_HEPB = Arrays.asList("HepB");
    private static final Collection SERIES_ID_HIB = Arrays.asList("Hib");
    private static final Collection SERIES_ID_HPV = Arrays.asList("HPV");
    private static final Collection SERIES_ID_DT = Arrays.asList("DTaP", "DTP", "DT");
    private static final Collection SERIES_ID_MCV4 = Arrays.asList("MCV4");
    private static final Collection SERIES_ID_MMR = Arrays.asList("MMR");
    private static final Collection SERIES_ID_MUMPS = Arrays.asList("Mumps");
    private static final Collection SERIES_ID_PCV = Arrays.asList("PCV");
    private static final Collection SERIES_ID_POLIO = Arrays.asList("OPV", "IPV");
    private static final Collection SERIES_ID_ROT = Arrays.asList("Rotavirus", "Rotarix");
    private static final Collection SERIES_ID_TD = Arrays.asList("Tdap", "Td");
    private static final Collection SERIES_ID_VAR = Arrays.asList("Varicella");

    /**
     * DDX IDs for Screenings
     */
    private static final String DDX_GROWTH = "HSC-GENERAL";
    private static final String DDX_HEARING = "HSC-HEARING";
    private static final String DDX_SCOLIOSIS = "HSC-POSTURAL";
    private static final String DDX_VISION = "HSC-VISION";

    /**
     * DDX Hearing Aliases
     */
    private static final String HEAR_L_250 = "hsc-hearing-l250";
    private static final String HEAR_L_500 = "hsc-hearing-l500";
    private static final String HEAR_L_1000 = "hsc-hearing-l1000";
    private static final String HEAR_L_2000 = "hsc-hearing-l2000";
    private static final String HEAR_L_4000 = "hsc-hearing-l4000";
    private static final String HEAR_L_8000 = "hsc-hearing-l8000";
    private static final String HEAR_R_250 = "hsc-hearing-r250";
    private static final String HEAR_R_500 = "hsc-hearing-r500";
    private static final String HEAR_R_1000 = "hsc-hearing-r1000";
    private static final String HEAR_R_2000 = "hsc-hearing-r2000";
    private static final String HEAR_R_4000 = "hsc-hearing-r4000";
    private static final String HEAR_R_8000 = "hsc-hearing-r8000";

    /**
     * Hearing Report Fields
     */
    private static final String RF_HEAR_DATE = "hDate";
    private static final String RF_HEAR_DATE_REF = "hDateRef";
    private static final String RF_HEAR_DATE_REF_COMPL = "hDateRefCompl";
    private static final String RF_HEAR_GRADE = "hGrade";
    private static final String RF_HEAR_L_250 = "l250";
    private static final String RF_HEAR_L_500 = "l500";
    private static final String RF_HEAR_L_1000 = "l1000";
    private static final String RF_HEAR_L_2000 = "l2000";
    private static final String RF_HEAR_L_4000 = "l4000";
    private static final String RF_HEAR_L_8000 = "l8000";
    private static final String RF_HEAR_R_250 = "r250";
    private static final String RF_HEAR_R_500 = "r500";
    private static final String RF_HEAR_R_1000 = "r1000";
    private static final String RF_HEAR_R_2000 = "r2000";
    private static final String RF_HEAR_R_4000 = "r4000";
    private static final String RF_HEAR_R_8000 = "r8000";

    /**
     * Growth Report Fields
     */
    private static final String RF_GROWTH_BMI = "gBMI";
    private static final String RF_GROWTH_BMI_PERCENT = "gBMIPercent";
    private static final String RF_GROWTH_DATE = "gDate";
    private static final String RF_GROWTH_GRADE = "gGrade";
    private static final String RF_GROWTH_HEIGHT = "gHeight";
    private static final String RF_GROWTH_PARENT_NOTE = "gParNote";
    private static final String RF_GROWTH_WEIGHT = "gWeight";

    /**
     * VISION Report Fields
     */
    private static final String RF_VISION_COLOR_FAIL = "vColorFail";
    private static final String RF_VISION_COLOR_PASS = "vColorPass";
    private static final String RF_VISION_CONVEX_FAIL = "vConvexFail";
    private static final String RF_VISION_CONVEX_PASS = "vConvexPass";
    private static final String RF_VISION_DATE = "vDate";
    private static final String RF_VISION_DATE_REF = "vDateRef";
    private static final String RF_VISION_DATE_REF_COMPL = "vDateRefCompl";
    private static final String RF_VISION_DEPTH_PERCEPTION_FAIL = "vDepthPerceptionFail";
    private static final String RF_VISION_DEPTH_PERCEPTION_PASS = "vDepthPerceptionPass";
    private static final String RF_VISION_FAR_L = "vFarL";
    private static final String RF_VISION_FAR_R = "vFarR";
    private static final String RF_VISION_GRADE = "vGrade";
    private static final String RF_VISION_LENS = "vLens";
    private static final String RF_VISION_NEAR_L = "vNearL";
    private static final String RF_VISION_NEAR_R = "vNearR";

    /**
     * Hearing Report Fields
     */
    private static final String RF_SCOLIOSIS_DATE = "sDate";
    private static final String RF_SCOLIOSIS_DATE_REF = "sDateRef";
    private static final String RF_SCOLIOSIS_DATE_REF_COMPL = "sDateRefCompl";
    private static final String RF_SCOLIOSIS_DATE_RESCR = "sDateRescr";
    private static final String RF_SCOLIOSIS_GRADE = "sGrade";
    private static final String RF_SCOLIOSIS_PREV_DIAGNOSE = "sPrevDiagnose";
    private static final String RF_SCOLIOSIS_RESULT = "sResult";
    private static final String RF_SCOLIOSIS_RESULT_RESCR = "sResultRescr";
    /**
     * Vision Aliases
     */
    public static final String ALIAS_HSC_VISION_COLOR = "hsc-vision-color";
    public static final String ALIAS_HSC_VISION_CONVEX = "hsc-vision-convex";
    public static final String ALIAS_HSC_VISION_DEPTH_PERCEPTION = "hsc-vision-depthPerception";
    public static final String ALIAS_HSC_VISION_FAR_L = "hsc-vision-far-L";
    public static final String ALIAS_HSC_VISION_FAR_R = "hsc-vision-far-R";
    public static final String ALIAS_HSC_VISION_L_BVA = "hsc-vision-bval";
    public static final String ALIAS_HSC_VISION_L_LVA = "hsc-vision-lval";
    public static final String ALIAS_HSC_VISION_L_RVA = "hsc-vision-rval";
    public static final String ALIAS_HSC_VISION_NEAR_L = "hsc-vision-near-L";
    public static final String ALIAS_HSC_VISION_NEAR_R = "hsc-vision-near-R";

    /**
     * Postural aliasis
     */
    public static final String ALIAS_HSC_SCOLIOSIS_PREV_DIAGNOSE = "hsc-postural-previous-diagnose";
    public static final String ALIAS_HSC_SCOLIOSIS_RESCR = "hsc-postural-rescreening";
    public static final String ALIAS_HSC_SCOLIOSIS_RESCR_DATE = "hsc-postural-rescreen-date";

    /**
     * Alias for the general parent notification.
     */
    public static final String ALIAS_HSC_GENERAL_NOTE_PARENT_DATE = "hsc-general-notification";

    /**
     * HSC Aliases
     */
    public static final String ALIAS_HSC_DATE_REF = "hscReferralDate";
    public static final String ALIAS_HSC_DATE_REF_COMPL = "hscReferralCompleteDate";

    /**
     * Report fields.
     */
    private static final String REPORT_FIELD_ALERTS = "alerts";
    private static final String REPORT_FIELD_DOB = "dob";
    private static final String REPORT_FIELD_DTP = "dtp";
    private static final String REPORT_FIELD_FATHER_FIRST_NAME = "fatherFirstName";
    private static final String REPORT_FIELD_FATHER_LAST_NAME = "fatherLastName";
    private static final String REPORT_FIELD_FATHER_MIDDLE_NAME = "fatherMiddleName";
    private static final String REPORT_FIELD_GRADES = "gradesList";
    private static final String REPORT_FIELD_GUARDIAN_FIRST_NAME = "guardianFirstName";
    private static final String REPORT_FIELD_GUARDIAN_LAST_NAME = "guardianLastName";
    private static final String REPORT_FIELD_GUARDIAN_MIDDLE_NAME = "guardianMiddleName";
    private static final String REPORT_FIELD_HEPB = "hepb";
    private static final String REPORT_FIELD_HEPA = "hepa";
    private static final String REPORT_FIELD_HIB = "hib";
    private static final String REPORT_FIELD_HPV = "hpv";
    private static final String REPORT_FIELD_MUMPS = "mumps";
    private static final String REPORT_FIELD_MMR = "mmr";
    private static final String REPORT_FIELD_MCV = "mcv";
    private static final String REPORT_FIELD_MOTHER_FIRST_NAME = "motherFirstName";
    private static final String REPORT_FIELD_MOTHER_LAST_NAME = "motherLastName";
    private static final String REPORT_FIELD_MOTHER_MIDDLE_NAME = "motherMiddleName";
    private static final String REPORT_FIELD_NAME_DISTR = "distrName";
    private static final String REPORT_FIELD_NAME_SKL = "sklName";
    private static final String REPORT_FIELD_SKL_YEAR = "sklYear";
    private static final String REPORT_FIELD_SKL_YEARS = "sklYearsList";
    private static final String REPORT_FIELD_PCV = "pcv";
    private static final String REPORT_FIELD_POLIO = "polio";
    private static final String REPORT_FIELD_ROTAVIRUS = "rot";
    private static final String REPORT_FIELD_SCR_RESULT = "scrResult";
    private static final String REPORT_FIELD_SEX = "sex";
    private static final String REPORT_FIELD_STD_OID = "stdOid";
    private static final String REPORT_FIELD_STUDENT_FIRST_NAME = "studentFirstName";
    private static final String REPORT_FIELD_STUDENT_GRADE = "grade";
    private static final String REPORT_FIELD_STUDENT_HOMEROOM = "studentHomeroom";
    private static final String REPORT_FIELD_STUDENT_LAST_NAME = "studentLastName";
    private static final String REPORT_FIELD_STUDENT_MIDDLE_NAME = "studentMiddleName";
    private static final String REPORT_FIELD_TDAP = "tdap";
    private static final String REPORT_FIELD_VAR = "var";

    /**
     * Other constants.
     */
    private static final String HSC_RESULT_FAIL = "Fail";
    private static final String HSC_RESULT_PASS = "Pass";
    private static final String HSC_VISION_RESULT_FOR_PASS = "20/40";
    private static final String HSC_VISION_RESULT_FOR_PASS_GR_1_2 = "CM";
    private static final int NUM_DOSES = 5;
    private static final String QUERY_BY_PARAM = "queryBy1";
    private static final String QUERY_STRING_PARAM = "queryString1";

    /**
     * Root report fields
     */
    private static final String ROOT_FIELD_DATA_SOURCE = "dataSource";
    private static final String ROOT_FIELD_REPORT_FORMAT = "reportFormat";

    /**
     * Subreports' ids.
     */
    private static final String SUB_ID_HEALTH_RECORD = "PA-SYS-HTH-REC";
    private static final String SUB_ID_IMMUNIZATION = "PA-SYS-HTH-IMN";
    private static final String SUB_ID_SCR_GROWTH = "PA-SYS-HTH-SCR-GROWTH";
    private static final String SUB_ID_SCR_HEAR = "PA-SYS-HTH-SCR-HEAR";
    private static final String SUB_ID_SCR_SCOLIOSIS = "PA-SYS-HTH-SCR-SCOLIOSIS";
    private static final String SUB_ID_SCR_VISION = "PA-SYS-HTH-SCR-VISION";

    /**
     * Report constants
     */
    private static String FATHER = "Father";
    private static String GUARDIAN = "Guardian";
    private static String MOTHER = "Mother";

    /**
     * Class members.
     */
    protected Map<String, Collection<HealthImmunizationDefinition>> m_categories;
    private Collection<DistrictSchoolYearContext> m_ctxList;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected Collection<HealthImmunizationDefinition> m_definitions;
    private List<String> m_gradeCodes =
            Arrays.asList("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");
    protected Map<String, Collection<Race>> m_races;
    private Map<String, List<HealthImmunizationDose>> m_seriesImmunizationDoseMap;
    private LinkedList<String> m_sklYears = new LinkedList<>();
    private Map<String, SisStudent> m_stdByOids;
    private Map<String, Map<String, HealthImmunizationSeries>> m_studentImmunizationSeriesMap;

    /**
     * Parents maps.
     */
    private Map<String, StudentContact> m_fatherMap;
    private Map<String, StudentContact> m_guardianMap;
    private Map<String, StudentContact> m_motherMap;

    /**
     * Report formats.
     */
    private Report m_subreportHealthRecord;
    private Report m_subreportImmunization;
    private Report m_subreportScrGrowth;
    private Report m_subreportScrHearing;
    private Report m_subreportScrScoliosis;
    private Report m_subreportScrVision;

    /**
     * Screening Maps
     */
    private Map<String, Collection<StudentAlert>> m_alrByStdMap;
    private Map<String, Collection<HealthScreening>> m_hscGrowthMap;
    private Map<String, Collection<HealthScreening>> m_hscHearingMap;
    private Map<String, Collection<HealthScreening>> m_hscScoliosisMap;
    private Map<String, Collection<HealthScreening>> m_hscVisionMap;

    /**
     * Growth java names.
     */
    private String m_growthBMI;
    private String m_growthBMIPercent;
    private String m_growthHeightIn;
    private String m_growthParNote;
    private String m_growthWeightKg;

    /**
     * Hearing java names.
     */
    private String m_hearingL250;
    private String m_hearingL500;
    private String m_hearingL1000;
    private String m_hearingL2000;
    private String m_hearingL4000;
    private String m_hearingL8000;
    private String m_hearingR250;
    private String m_hearingR500;
    private String m_hearingR1000;
    private String m_hearingR2000;
    private String m_hearingR4000;
    private String m_hearingR8000;

    /**
     * Hearing java names.
     */
    private String m_visionColor;
    private String m_visionConvex;
    private String m_visionDepthPerception;
    private String m_visionFarL;
    private String m_visionFarR;
    private String m_visionLBVA;
    private String m_visionLLVA;
    private String m_visionLRVA;
    private String m_visionNearL;
    private String m_visionNearR;

    /**
     * Hearing java names.
     */
    private String m_scoliosisPrevDiagnose;
    private String m_scoliosisRescr;
    private String m_scoliosisRescrDate;

    /**
     * Provides the report data source. The input data source is delivered
     * by the CAImmunization class and export format
     *
     * !!!Don't change order.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid dataGrid = new ReportDataGrid();
        loadContactMaps();
        for (String stdOid : m_stdByOids.keySet()) {
            SisStudent std = m_stdByOids.get(stdOid);
            // Health Record SUB
            fillGridRow(dataGrid, getPopulatedHealthRecordSubreport(std), m_subreportHealthRecord, stdOid);
            // Immunization SUB
            fillGridRow(dataGrid, getPopulatedImmunizationSubreport(std), m_subreportImmunization, stdOid);
            // Scoliosis SUB
            fillGridRow(dataGrid, getPopulatedScoliosisSubreport(std), m_subreportScrScoliosis, stdOid);
            // Vision SUB
            fillGridRow(dataGrid, getPopulatedVisionSubreport(std), m_subreportScrVision, stdOid);
            // Growth SUB
            fillGridRow(dataGrid, getPopulatedHearingSubreport(std), m_subreportScrHearing, stdOid);
            // Growth SUB
            fillGridRow(dataGrid, getPopulatedGrowthSubreport(std), m_subreportScrGrowth, stdOid);
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Returns definitions by category.
     *
     * @param category String
     * @return Collection
     */
    protected Collection<HealthImmunizationDefinition> getDefinitionsByCategory(String category) {
        Collection<HealthImmunizationDefinition> defs = m_categories.get(category);
        return defs;
    }

    /**
     * Returns series by definition and student.
     *
     * @param defn HealthImmunizationDefinition
     * @param stdOid String
     * @return Health immunization series
     */
    protected HealthImmunizationSeries getSeries(HealthImmunizationDefinition defn, String stdOid) {
        Map<String, HealthImmunizationSeries> seriesMap = m_studentImmunizationSeriesMap.get(defn.getOid());
        if (seriesMap == null) {
            seriesMap = new HashMap<String, HealthImmunizationSeries>(0);
        }
        return seriesMap.get(stdOid);
    }

    /**
     * Returns immunization dose by series.
     *
     * @param series HealthImmunizationSeries
     * @return List
     */
    protected List<HealthImmunizationDose> getSeriesImmunizationDose(HealthImmunizationSeries series) {
        return m_seriesImmunizationDoseMap.get(series.getOid());
    }

    /**
     * Returns student immunization series by student.
     *
     * @param student SisStudent
     * @return Map
     */
    protected Map<String, HealthImmunizationSeries> getStudentImmunizationSeries(SisStudent student) {
        return m_studentImmunizationSeriesMap.get(student.getOid());
    }

    /**
     * Adds parameters from the input to the report.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initReportsFormat();
        loadSchoolYears();
        initImmunizationCollections();
        loadHscGrowthMap();
        loadStudentAlertsMap();
        loadHscScoliosisMap();
        loadHscHearingMap();
        loadHscScoliosisMap();
        loadHscVisionMap();
        initGrowthScreeningFields();
        initHearingScreeningFields();
        initVisionFields();
        initScoliosisFields();
    }

    /**
     * Remember the currently selected student if this report is being run from the student module.
     *
     * @param userDate UserDataContainer
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userDate) {
        X2Criteria studentCriteria = new X2Criteria();
        SisStudent stdCurrent = userDate.getCurrentRecord(SisStudent.class);
        if (stdCurrent != null) {
            studentCriteria.addEqualTo(X2BaseBean.COL_OID, stdCurrent.getOid());
        } else {
            ContextList currentList = userDate.getCurrentList();
            if (currentList != null && currentList.getDataClass().equals(SisStudent.class)) {
                studentCriteria.addAndCriteria(getCurrentCriteria());
            }
        }
        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        if (queryBy != null && queryString != null && stdCurrent == null) {
            addUserCriteria(studentCriteria, queryBy, queryString, null, null);
        }
        m_stdByOids = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, studentCriteria),
                X2BaseBean.COL_OID, 1028);
    }

    /**
     * Calculate grade by date.
     *
     * @param currentGrade String
     * @param hscDate PlainDate
     * @return String
     */
    private String calculateGradeByDate(String currentGrade, PlainDate hscDate) {
        String calcGrade = currentGrade;
        int currentSklYear = getCurrentContext().getSchoolYear();
        for (DistrictSchoolYearContext ctx : m_ctxList) {
            if (!ctx.getStartDate().after(hscDate) && !ctx.getEndDate().before(hscDate)) {
                int hscDateYear = ctx.getSchoolYear();
                if (hscDateYear < currentSklYear) {
                    int indexOfCurentGrade = m_gradeCodes.indexOf(currentGrade);
                    int indexToSubtract = currentSklYear - hscDateYear;
                    if (indexToSubtract >= 0) {
                        int indexOfCalcGrade = indexOfCurentGrade - indexToSubtract;
                        if (m_gradeCodes.size() >= indexOfCalcGrade + 1 && indexOfCalcGrade >= 0) {
                            calcGrade = m_gradeCodes.get(indexOfCalcGrade);
                        }
                    }
                }
                break;
            }
        }
        return calcGrade;
    }

    /**
     * Fill grid row.
     *
     * @param grid ReportDataGrid
     * @param dataSource ReportDataGrid
     * @param subreport Report
     * @param stdOid String
     */
    private void fillGridRow(ReportDataGrid grid,
                             ReportDataGrid dataSource,
                             Report subreport,
                             String stdOid) {
        grid.append();
        grid.set(ROOT_FIELD_DATA_SOURCE, dataSource);
        ByteArrayInputStream reportFormat = new ByteArrayInputStream(subreport.getCompiledFormat());
        grid.set(ROOT_FIELD_REPORT_FORMAT, reportFormat);
        grid.set(REPORT_FIELD_STD_OID, stdOid);
    }

    /**
     * Gets the ctx by date.
     *
     * @param date PlainDate
     * @return District school year context
     */
    private DistrictSchoolYearContext getCtxByDate(PlainDate date) {
        if (m_ctxList == null) {
            loadSchoolYears();
        }
        DistrictSchoolYearContext ctxToReturn = null;
        for (DistrictSchoolYearContext ctx : m_ctxList) {
            if (!date.before(ctx.getStartDate()) && !date.after(ctx.getEndDate())) {
                ctxToReturn = ctx;
                break;
            }
        }
        return ctxToReturn;
    }

    /**
     * Gets the populated growth subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getPopulatedGrowthSubreport(SisStudent std) {
        ReportDataGrid subreportGrowth = new ReportDataGrid();
        populateGrowthTable(subreportGrowth, std);
        subreportGrowth.beforeTop();
        return subreportGrowth;
    }

    /**
     * Gets the populated health record subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getPopulatedHealthRecordSubreport(SisStudent std) {
        ReportDataGrid subreportHealthRecord = new ReportDataGrid();
        Collection<StudentEnrollment> enrs = std.getEnrollments();
        LinkedHashMap<String, KeyValuePair<String, String>> sklDistrMapBySklYear = new LinkedHashMap<>();
        for (StudentEnrollment enr : enrs) {
            PlainDate enrDate = enr.getEnrollmentDate();
            DistrictSchoolYearContext ctxByDate = getCtxByDate(enrDate);
            if (ctxByDate != null) {
                String ctxId = ctxByDate.getContextId();
                if (!sklDistrMapBySklYear.containsKey(ctxId)) {
                    KeyValuePair sklDistrPair = new KeyValuePair<String, String>(enr.getSchool().getName(),
                            enr.getSchool().getOrganization1().getName());
                    sklDistrMapBySklYear.put(ctxId, sklDistrPair);
                }
            }
        }
        String grade = std.getGradeLevel();
        List<String> allStdGrades = new ArrayList<>();
        List<String> allSklYears = new ArrayList<>();
        if (m_gradeCodes.contains(grade)) {
            allStdGrades.addAll(m_gradeCodes.subList(0, m_gradeCodes.indexOf(grade) + 1));
        }
        if (m_sklYears.contains(getCurrentContext().getContextId())) {
            allSklYears.addAll(
                    m_sklYears.subList(
                            m_sklYears.indexOf(getCurrentContext().getContextId()) - allStdGrades.size() + 1,
                            m_sklYears.indexOf(getCurrentContext().getContextId()) + 1));
        }
        if (!sklDistrMapBySklYear.isEmpty()) {
            for (String ctxId : sklDistrMapBySklYear.keySet()) {
                subreportHealthRecord.append();
                subreportHealthRecord.set(REPORT_FIELD_SKL_YEAR, ctxId);
                subreportHealthRecord.set(REPORT_FIELD_GRADES, allStdGrades);
                subreportHealthRecord.set(REPORT_FIELD_SKL_YEARS, allSklYears);
                subreportHealthRecord.set(REPORT_FIELD_NAME_SKL, sklDistrMapBySklYear.get(ctxId).getKey());
                subreportHealthRecord.set(REPORT_FIELD_NAME_DISTR, sklDistrMapBySklYear.get(ctxId).getValue());
                subreportHealthRecord.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
                subreportHealthRecord.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
                subreportHealthRecord.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
                subreportHealthRecord.set(REPORT_FIELD_STUDENT_MIDDLE_NAME, std.getPerson().getMiddleName());
                subreportHealthRecord.set(REPORT_FIELD_SEX, std.getPerson().getGenderCode());
                subreportHealthRecord.set(REPORT_FIELD_DOB, m_dateFormat.format(std.getPerson().getDob()));
                subreportHealthRecord.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
                if (m_fatherMap.containsKey(std.getOid())) {
                    StudentContact cntFather = m_fatherMap.get(std.getOid());
                    if (cntFather != null) {
                        subreportHealthRecord.set(REPORT_FIELD_FATHER_FIRST_NAME, cntFather.getPerson().getFirstName());
                        subreportHealthRecord.set(REPORT_FIELD_FATHER_LAST_NAME, cntFather.getPerson().getLastName());
                        subreportHealthRecord.set(REPORT_FIELD_FATHER_MIDDLE_NAME,
                                cntFather.getPerson().getMiddleName());
                    }
                }
                if (m_motherMap.containsKey(std.getOid())) {
                    StudentContact cntMother = m_motherMap.get(std.getOid());
                    if (cntMother != null) {
                        subreportHealthRecord.set(REPORT_FIELD_MOTHER_FIRST_NAME, cntMother.getPerson().getFirstName());
                        subreportHealthRecord.set(REPORT_FIELD_MOTHER_LAST_NAME, cntMother.getPerson().getLastName());
                        subreportHealthRecord.set(REPORT_FIELD_MOTHER_MIDDLE_NAME,
                                cntMother.getPerson().getMiddleName());
                    }
                }
                if (m_guardianMap.containsKey(std.getOid())) {
                    StudentContact cntGuardian = m_guardianMap.get(std.getOid());
                    if (cntGuardian != null) {
                        subreportHealthRecord.set(REPORT_FIELD_GUARDIAN_FIRST_NAME,
                                cntGuardian.getPerson().getFirstName());
                        subreportHealthRecord.set(REPORT_FIELD_GUARDIAN_LAST_NAME,
                                cntGuardian.getPerson().getLastName());
                        subreportHealthRecord.set(REPORT_FIELD_GUARDIAN_MIDDLE_NAME,
                                cntGuardian.getPerson().getMiddleName());
                    }
                }
            }
        }
        Collection<StudentAlert> alerts = m_alrByStdMap.get(std.getOid());
        if (alerts != null && !alerts.isEmpty()) {
            StringBuilder valueToSet = new StringBuilder();
            int place = 0;
            for (StudentAlert alr : alerts) {
                String descr = alr.getAlertDescription();
                if (!StringUtils.isEmpty(descr)) {
                    place += 1;
                    valueToSet.append(String.valueOf(place) + ". ");
                    valueToSet.append(descr);
                    valueToSet.append(System.getProperty("line.separator"));
                }
            }
            subreportHealthRecord.set(REPORT_FIELD_ALERTS, valueToSet.toString());
        }
        subreportHealthRecord.beforeTop();
        return subreportHealthRecord;
    }

    /**
     * Gets the populated screening 1 subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     * @throws ParseException exception
     */
    private ReportDataGrid getPopulatedHearingSubreport(SisStudent std) throws ParseException {
        ReportDataGrid subreportHearing = new ReportDataGrid();
        populateHearingTable(subreportHearing, std);
        subreportHearing.beforeTop();
        return subreportHearing;
    }

    /**
     * Gets the populated immunization subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getPopulatedImmunizationSubreport(SisStudent std) {
        String stdOid = std.getOid();
        ReportDataGrid subreportImmunization = new ReportDataGrid();
        subreportImmunization.append();
        subreportImmunization.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
        subreportImmunization.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
        subreportImmunization.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
        subreportImmunization.set(REPORT_FIELD_STUDENT_MIDDLE_NAME, std.getPerson().getMiddleName());
        subreportImmunization.set(REPORT_FIELD_DOB, m_dateFormat.format(std.getPerson().getDob()));
        subreportImmunization.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_DTP, SERIES_ID_DT, REPORT_FIELD_DTP);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_DTP, SERIES_ID_TD, REPORT_FIELD_TDAP);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_POLIO, SERIES_ID_POLIO, REPORT_FIELD_POLIO);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_HEPB, SERIES_ID_HEPB, REPORT_FIELD_HEPB);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_MMR, SERIES_ID_MMR, REPORT_FIELD_MMR);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_MMR, SERIES_ID_MUMPS, REPORT_FIELD_MUMPS);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_VAR, SERIES_ID_VAR, REPORT_FIELD_VAR);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_MENING, SERIES_ID_MCV4, REPORT_FIELD_MCV);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_HPV, SERIES_ID_HPV, REPORT_FIELD_HPV);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_HIB, SERIES_ID_HIB, REPORT_FIELD_HIB);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_PCV, SERIES_ID_PCV, REPORT_FIELD_PCV);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_HEPA, SERIES_ID_HEPA, REPORT_FIELD_HEPA);
        populateGridWithDose(subreportImmunization, stdOid, CATEGORY_ROT, SERIES_ID_ROT, REPORT_FIELD_ROTAVIRUS);
        subreportImmunization.beforeTop();
        return subreportImmunization;
    }

    /**
     * Gets the populated scoliosis subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     * @throws ParseException exception
     */
    private ReportDataGrid getPopulatedScoliosisSubreport(SisStudent std) throws ParseException {
        ReportDataGrid subreportScoliosis = new ReportDataGrid();
        populateScoliosisTable(subreportScoliosis, std);
        subreportScoliosis.beforeTop();
        return subreportScoliosis;
    }

    /**
     * Gets the populated vision subreport.
     *
     * @param std SisStudent
     * @return Report data grid
     */
    private ReportDataGrid getPopulatedVisionSubreport(SisStudent std) {
        ReportDataGrid subreportVision = new ReportDataGrid();
        populateVisionTable(subreportVision, std);
        subreportVision.beforeTop();
        return subreportVision;
    }

    /**
     * Inits the growth screening fields.
     */
    private void initGrowthScreeningFields() {
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(SisExtendedDataDictionary.COL_ID, DDX_GROWTH);
        SisExtendedDataDictionary lepDdx = (SisExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(SisExtendedDataDictionary.class, ddxCriteria));
        if (lepDdx != null) {
            DataDictionary growthDictByDdx =
                    DataDictionary.getDistrictDictionary(lepDdx, getBroker().getPersistenceKey());
            DataDictionaryField dictField =
                    growthDictByDdx.findDataDictionaryFieldByAlias(HealthAliases.SCREENING_HEIGHT_IN);
            if (dictField != null) {
                m_growthHeightIn = dictField.getJavaName();
            }
            dictField =
                    growthDictByDdx.findDataDictionaryFieldByAlias(HealthAliases.SCREENING_WEIGHT_KG);
            if (dictField != null) {
                m_growthWeightKg = dictField.getJavaName();
            }
            dictField =
                    growthDictByDdx.findDataDictionaryFieldByAlias(HealthAliases.SCREENING_BMI);
            if (dictField != null) {
                m_growthBMI = dictField.getJavaName();
            }
            dictField =
                    growthDictByDdx.findDataDictionaryFieldByAlias(HealthAliases.SCREENING_BMI_PERCENT);
            if (dictField != null) {
                m_growthBMIPercent = dictField.getJavaName();
            }
            dictField =
                    growthDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_GENERAL_NOTE_PARENT_DATE);
            if (dictField != null) {
                m_growthParNote = dictField.getJavaName();
            }
        }
    }

    /**
     * Inits the growth screening fields.
     */
    private void initHearingScreeningFields() {
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(SisExtendedDataDictionary.COL_ID, DDX_HEARING);
        SisExtendedDataDictionary lepDdx = (SisExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(SisExtendedDataDictionary.class, ddxCriteria));
        if (lepDdx != null) {
            DataDictionary hearingDictByDdx =
                    DataDictionary.getDistrictDictionary(lepDdx, getBroker().getPersistenceKey());
            DataDictionaryField dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_250);
            if (dictField != null) {
                m_hearingL250 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_500);
            if (dictField != null) {
                m_hearingL500 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_1000);
            if (dictField != null) {
                m_hearingL1000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_1000);
            if (dictField != null) {
                m_hearingL1000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_2000);
            if (dictField != null) {
                m_hearingL2000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_4000);
            if (dictField != null) {
                m_hearingL4000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_L_8000);
            if (dictField != null) {
                m_hearingL8000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_250);
            if (dictField != null) {
                m_hearingR250 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_500);
            if (dictField != null) {
                m_hearingR500 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_1000);
            if (dictField != null) {
                m_hearingR1000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_1000);
            if (dictField != null) {
                m_hearingR1000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_2000);
            if (dictField != null) {
                m_hearingR2000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_4000);
            if (dictField != null) {
                m_hearingR4000 = dictField.getJavaName();
            }
            dictField =
                    hearingDictByDdx.findDataDictionaryFieldByAlias(HEAR_R_8000);
            if (dictField != null) {
                m_hearingR8000 = dictField.getJavaName();
            }
        }
    }

    /**
     * Generates immunization collections for definition, categories, series and doses
     * used to determine given doses.
     *
     */
    private void initImmunizationCollections() {
        // definitions
        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);
        // categories
        QueryByCriteria categoryQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_categories =
                getBroker().getGroupedCollectionByQuery(categoryQuery, HealthImmunizationDefinition.COL_CATEGORIES, 20);
        // series
        X2Criteria seriesMapCriteria = new X2Criteria();
        seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, m_stdByOids.keySet());
        QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesMapCriteria);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, true);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_STUDENT_OID, true);
        m_studentImmunizationSeriesMap = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);
        // doses
        SubQuery seriesSubQuery = new SubQuery(HealthImmunizationSeries.class, X2BaseBean.COL_OID, seriesMapCriteria);
        X2Criteria doseMapCriteria = new X2Criteria();
        doseMapCriteria.addIn(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, seriesSubQuery);
        QueryByCriteria doseQuery = new QueryByCriteria(HealthImmunizationDose.class, seriesMapCriteria);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, true);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_DATE, true);
        m_seriesImmunizationDoseMap = getBroker().getGroupedCollectionByQuery(doseQuery,
                HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, 500);
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        m_subreportHealthRecord = ReportUtils.getReport(SUB_ID_HEALTH_RECORD, getBroker());
        m_subreportImmunization = ReportUtils.getReport(SUB_ID_IMMUNIZATION, getBroker());
        m_subreportScrScoliosis = ReportUtils.getReport(SUB_ID_SCR_SCOLIOSIS, getBroker());
        m_subreportScrVision = ReportUtils.getReport(SUB_ID_SCR_VISION, getBroker());
        m_subreportScrGrowth = ReportUtils.getReport(SUB_ID_SCR_GROWTH, getBroker());
        m_subreportScrHearing = ReportUtils.getReport(SUB_ID_SCR_HEAR, getBroker());
    
    }

    /**
     * Inits the scoliosis(postural) fields.
     */
    private void initScoliosisFields() {
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(SisExtendedDataDictionary.COL_ID, DDX_SCOLIOSIS);
        SisExtendedDataDictionary scoliosisDdx = (SisExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(SisExtendedDataDictionary.class, ddxCriteria));
        if (scoliosisDdx != null) {
            DataDictionary scoliosisDictByDdx =
                    DataDictionary.getDistrictDictionary(scoliosisDdx, getBroker().getPersistenceKey());
            DataDictionaryField dictField =
                    scoliosisDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_SCOLIOSIS_RESCR);
            if (dictField != null) {
                m_scoliosisRescr = dictField.getJavaName();
            }
            dictField =
                    scoliosisDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_SCOLIOSIS_RESCR_DATE);
            if (dictField != null) {
                m_scoliosisRescrDate = dictField.getJavaName();
            }
            dictField =
                    scoliosisDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_SCOLIOSIS_PREV_DIAGNOSE);
            if (dictField != null) {
                m_scoliosisPrevDiagnose = dictField.getJavaName();
            }
        }
    }

    /**
     * Inits the vision fields.
     */
    private void initVisionFields() {
        X2Criteria ddxCriteria = new X2Criteria();
        ddxCriteria.addEqualTo(SisExtendedDataDictionary.COL_ID, DDX_VISION);
        SisExtendedDataDictionary visionDdx = (SisExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(SisExtendedDataDictionary.class, ddxCriteria));
        if (visionDdx != null) {
            DataDictionary visionDictByDdx =
                    DataDictionary.getDistrictDictionary(visionDdx, getBroker().getPersistenceKey());
            DataDictionaryField dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_NEAR_L);
            if (dictField != null) {
                m_visionNearL = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_NEAR_R);
            if (dictField != null) {
                m_visionNearR = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_FAR_R);
            if (dictField != null) {
                m_visionFarR = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_FAR_L);
            if (dictField != null) {
                m_visionFarL = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_CONVEX);
            if (dictField != null) {
                m_visionConvex = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_COLOR);
            if (dictField != null) {
                m_visionColor = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_DEPTH_PERCEPTION);
            if (dictField != null) {
                m_visionDepthPerception = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_L_BVA);
            if (dictField != null) {
                m_visionLBVA = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_L_RVA);
            if (dictField != null) {
                m_visionLRVA = dictField.getJavaName();
            }
            dictField =
                    visionDictByDdx.findDataDictionaryFieldByAlias(ALIAS_HSC_VISION_L_LVA);
            if (dictField != null) {
                m_visionLLVA = dictField.getJavaName();
            }
        }
    }

    /**
     * Loads maps of StudentContacts (father, mother, guardian) keyed to the student OID.
     */
    private void loadContactMaps() {
        X2Criteria motherCriteria = new X2Criteria();
        motherCriteria.addEqualToIgnoreCase(StudentContact.COL_RELATIONSHIP_CODE, MOTHER);
        motherCriteria.addIn(StudentContact.COL_STUDENT_OID, m_stdByOids.keySet());
        motherCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        X2Criteria fatherCriteria = new X2Criteria();
        fatherCriteria.addEqualToIgnoreCase(StudentContact.COL_RELATIONSHIP_CODE, FATHER);
        fatherCriteria.addIn(StudentContact.COL_STUDENT_OID, m_stdByOids.keySet());
        fatherCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        X2Criteria guardianCriteria = new X2Criteria();
        guardianCriteria.addEqualTo(StudentContact.COL_RELATIONSHIP_CODE, GUARDIAN);
        guardianCriteria.addIn(StudentContact.COL_STUDENT_OID, m_stdByOids.keySet());
        guardianCriteria.addEqualTo(StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, motherCriteria);
        m_motherMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);

        query = new QueryByCriteria(StudentContact.class, fatherCriteria);
        m_fatherMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);

        query = new QueryByCriteria(StudentContact.class, guardianCriteria);
        m_guardianMap = getBroker().getMapByQuery(query, StudentContact.COL_STUDENT_OID, 5000);
    }

    /**
     * Load growth screenings.
     *
     */
    private void loadHscGrowthMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(HealthScreening.COL_STUDENT_OID, m_stdByOids.keySet());
        criteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_GROWTH);
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);
        query.addOrderBy(HealthScreening.COL_DATE, true);
        m_hscGrowthMap = getBroker().getGroupedCollectionByQuery(query, HealthScreening.COL_STUDENT_OID,
                m_stdByOids.keySet().size());
    }

    /**
     * Load hearing screenings.
     *
     */
    private void loadHscHearingMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(HealthScreening.COL_STUDENT_OID, m_stdByOids.keySet());
        criteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_HEARING);
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);
        query.addOrderBy(HealthScreening.COL_DATE, true);
        m_hscHearingMap = getBroker().getGroupedCollectionByQuery(query, HealthScreening.COL_STUDENT_OID,
                m_stdByOids.keySet().size());
    }

    /**
     * Load scoliosis screenings.
     *
     */
    private void loadHscScoliosisMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(HealthScreening.COL_STUDENT_OID, m_stdByOids.keySet());
        criteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_SCOLIOSIS);
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);
        query.addOrderBy(HealthScreening.COL_DATE, true);
        m_hscScoliosisMap = getBroker().getGroupedCollectionByQuery(query, HealthScreening.COL_STUDENT_OID,
                m_stdByOids.keySet().size());
    }

    /**
     * Load vision screenings.
     *
     */
    private void loadHscVisionMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(HealthScreening.COL_STUDENT_OID, m_stdByOids.keySet());
        criteria.addEqualTo(HealthScreening.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, DDX_VISION);
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);
        query.addOrderBy(HealthScreening.COL_DATE, true);
        m_hscVisionMap = getBroker().getGroupedCollectionByQuery(query, HealthScreening.COL_STUDENT_OID,
                m_stdByOids.keySet().size());
    }

    /**
     * Load school years.
     */
    private void loadSchoolYears() {
        if (m_ctxList == null) {
            QueryByCriteria ctxQuery = new QueryByCriteria(DistrictSchoolYearContext.class, new X2Criteria());
            ctxQuery.addOrderByAscending(DistrictSchoolYearContext.COL_CONTEXT_ID);
            Collection<String> ctxIds = getBroker()
                    .getGroupedCollectionByQuery(ctxQuery, DistrictSchoolYearContext.COL_CONTEXT_ID, 64).keySet();
            m_ctxList = getBroker().getCollectionByQuery(ctxQuery);
            if (ctxIds != null && !ctxIds.isEmpty()) {
                m_sklYears.addAll(ctxIds);
                Collections.sort(m_sklYears);
            }
        }
    }

    /**
     * Load alerts.
     *
     */
    private void loadStudentAlertsMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentAlert.COL_STUDENT_OID, m_stdByOids.keySet());
        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);
        m_alrByStdMap = getBroker().getGroupedCollectionByQuery(query, StudentAlert.COL_STUDENT_OID,
                m_stdByOids.keySet().size());
    }

    /**
     * Populate grid with dates and ages.
     *
     * @param dataGrid ReportDataGrid
     * @param stdOid String
     * @param category String
     * @param seriesIds Collection
     * @param reportField String
     */
    private void populateGridWithDose(ReportDataGrid dataGrid,
                                      String stdOid,
                                      String category,
                                      Collection seriesIds,
                                      String reportField) {
        List<PlainDate> dosesDates = new ArrayList<PlainDate>();
        Collection<HealthImmunizationDefinition> defnList = getDefinitionsByCategory(category);
        if (defnList != null && !defnList.isEmpty()) {
            for (HealthImmunizationDefinition defn : getDefinitionsByCategory(category)) {
                HealthImmunizationSeries series = getSeries(defn, stdOid);
                if (series != null && seriesIds.contains(defn.getSeriesId())) {
                    Collection<HealthImmunizationDose> doses = series.getImmunizationDoses();
                    for (HealthImmunizationDose dose : doses) {
                        if (dose.getDate() != null) {
                            if (!dosesDates.contains(dose.getDate())) {
                                dosesDates.add(dose.getDate());
                            }
                        }
                    }
                }
            }
            Collections.sort(dosesDates);
            if (dosesDates != null && !dosesDates.isEmpty()) {
                for (int i = 0; i < NUM_DOSES; i++) {
                    dataGrid.set(reportField + (i + 1),
                            dosesDates.size() > i ? m_dateFormat.format(dosesDates.get(i)) : null);
                }
            }
        }
    }

    /**
     * Populate growth table.
     *
     * @param grid ReportDataGrid
     * @param std SisStudent
     */
    private void populateGrowthTable(ReportDataGrid grid, SisStudent std) {
        String stdOid = std.getOid();
        Collection<HealthScreening> growthList = m_hscGrowthMap.get(stdOid);
        if (growthList != null && !growthList.isEmpty()) {
            for (HealthScreening growth : growthList) {
                PlainDate hscDate = growth.getDate();
                String result = growth.getResultCode();
                if (hscDate != null && "Pass".equalsIgnoreCase(result)) {
                    grid.append();
                    grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
                    grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
                    grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
                    grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
                    String calcGradeByDate = calculateGradeByDate(std.getGradeLevel(), hscDate);
                    grid.set(RF_GROWTH_DATE, m_dateFormat.format(hscDate));
                    grid.set(RF_GROWTH_GRADE, calcGradeByDate);
                    if (m_growthHeightIn != null) {
                        grid.set(RF_GROWTH_HEIGHT, growth.getFieldValueByBeanPath(m_growthHeightIn));
                    }
                    if (m_growthWeightKg != null) {
                        grid.set(RF_GROWTH_WEIGHT, growth.getFieldValueByBeanPath(m_growthWeightKg));
                    }
                    if (m_growthBMI != null) {
                        grid.set(RF_GROWTH_BMI, growth.getFieldValueByBeanPath(m_growthBMI));
                    }
                    if (m_growthBMIPercent != null) {
                        grid.set(RF_GROWTH_BMI_PERCENT, growth.getFieldValueByBeanPath(m_growthBMIPercent));
                    }
                    if (m_growthParNote != null && growth.getFieldValueByBeanPath(m_growthParNote) != null) {
                        PlainDate dateRef = DateUtils.getDate((String) growth.getFieldValueByBeanPath(m_growthParNote));
                        if (dateRef != null) {
                            grid.set(RF_GROWTH_PARENT_NOTE,
                                    m_dateFormat.format(dateRef));
                        }
                    }
                }
            }
        }
        if (grid.isEmpty()) {
            grid.append();
            grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
            grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
            grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
            grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
        }
    }

    /**
     * Populate hearing table.
     *
     * @param grid ReportDataGrid
     * @param std SisStudent
     */
    private void populateHearingTable(ReportDataGrid grid, SisStudent std) {
        String stdOid = std.getOid();
        Collection<HealthScreening> hearingScrs = m_hscHearingMap.get(stdOid);
        if (hearingScrs != null && !hearingScrs.isEmpty()) {
            for (HealthScreening hearing : hearingScrs) {
                PlainDate hscDate = hearing.getDate();
                if (hscDate != null) {
                    grid.append();
                    if (hearing.getResultCode() != null) {
                        grid.set(REPORT_FIELD_SCR_RESULT, hearing.getResultCode());
                    }
                    grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
                    grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
                    grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
                    grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
                    String calcGradeByDate = calculateGradeByDate(std.getGradeLevel(), hscDate);
                    grid.set(RF_HEAR_DATE, m_dateFormat.format(hscDate));
                    grid.set(RF_HEAR_GRADE, calcGradeByDate);
                    String dateRefStr = (String) hearing.getFieldValueByAlias(ALIAS_HSC_DATE_REF);
                    if (!StringUtils.isEmpty(dateRefStr)) {
                        PlainDate dateRef = DateUtils.getDate(dateRefStr);
                        if (dateRef != null) {
                            grid.set(RF_HEAR_DATE_REF, m_dateFormat.format(dateRef));
                        }
                    }
                    String dateRefComplStr =
                            (String) hearing.getFieldValueByAlias(ALIAS_HSC_DATE_REF_COMPL);
                    if (!StringUtils.isEmpty(dateRefComplStr)) {
                        PlainDate dateRefCompl = DateUtils.getDate(dateRefComplStr);
                        if (dateRefCompl != null) {
                            grid.set(RF_HEAR_DATE_REF_COMPL, m_dateFormat.format(dateRefCompl));
                        }
                    }
                    if (m_hearingL250 != null) {
                        grid.set(RF_HEAR_L_250, hearing.getFieldValueByBeanPath(m_hearingL250));
                    }
                    if (m_hearingL500 != null) {
                        grid.set(RF_HEAR_L_500, hearing.getFieldValueByBeanPath(m_hearingL500));
                    }
                    if (m_hearingL1000 != null) {
                        grid.set(RF_HEAR_L_1000, hearing.getFieldValueByBeanPath(m_hearingL1000));
                    }
                    if (m_hearingL2000 != null) {
                        grid.set(RF_HEAR_L_2000, hearing.getFieldValueByBeanPath(m_hearingL2000));
                    }
                    if (m_hearingL4000 != null) {
                        grid.set(RF_HEAR_L_4000, hearing.getFieldValueByBeanPath(m_hearingL4000));
                    }
                    if (m_hearingL8000 != null) {
                        grid.set(RF_HEAR_L_8000, hearing.getFieldValueByBeanPath(m_hearingL8000));
                    }
                    if (m_hearingR250 != null) {
                        grid.set(RF_HEAR_R_250, hearing.getFieldValueByBeanPath(m_hearingR250));
                    }
                    if (m_hearingR500 != null) {
                        grid.set(RF_HEAR_R_500, hearing.getFieldValueByBeanPath(m_hearingR500));
                    }
                    if (m_hearingR1000 != null) {
                        grid.set(RF_HEAR_R_1000, hearing.getFieldValueByBeanPath(m_hearingR1000));
                    }
                    if (m_hearingR2000 != null) {
                        grid.set(RF_HEAR_R_2000, hearing.getFieldValueByBeanPath(m_hearingR2000));
                    }
                    if (m_hearingR4000 != null) {
                        grid.set(RF_HEAR_R_4000, hearing.getFieldValueByBeanPath(m_hearingR4000));
                    }
                    if (m_hearingR8000 != null) {
                        grid.set(RF_HEAR_R_8000, hearing.getFieldValueByBeanPath(m_hearingR8000));
                    }
                }
            }
        }
        if (grid.isEmpty()) {
            grid.append();
            grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
            grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
            grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
            grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
        }
    }

    /**
     * Populate scoliosis table.
     *
     * @param subreportScoliosis ReportDataGrid
     * @param std SisStudent
     */
    private void populateScoliosisTable(ReportDataGrid subreportScoliosis, SisStudent std) {
        String stdOid = std.getOid();
        Collection<HealthScreening> scoliosisList = m_hscScoliosisMap.get(stdOid);
        if (scoliosisList != null && !scoliosisList.isEmpty()) {
            for (HealthScreening scoliosis : scoliosisList) {
                PlainDate hscDate = scoliosis.getDate();
                if (hscDate != null) {
                    subreportScoliosis.append();
                    subreportScoliosis.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
                    subreportScoliosis.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
                    subreportScoliosis.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
                    subreportScoliosis.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
                    String calcGradeByDate = calculateGradeByDate(std.getGradeLevel(), hscDate);
                    if (!StringUtils.isEmpty(calcGradeByDate)
                            && ("06".equalsIgnoreCase(calcGradeByDate) || "07".equalsIgnoreCase(calcGradeByDate))) {
                        subreportScoliosis.set(RF_SCOLIOSIS_GRADE, calcGradeByDate);
                        subreportScoliosis.set(RF_SCOLIOSIS_DATE, m_dateFormat.format(hscDate));
                        String scoliosisResult = scoliosis.getResultCode();
                        if (!StringUtils.isEmpty(scoliosisResult)) {
                            subreportScoliosis.set(RF_SCOLIOSIS_RESULT, scoliosisResult);
                        }
                        String dateRefStr = (String) scoliosis.getFieldValueByAlias(ALIAS_HSC_DATE_REF);
                        if (!StringUtils.isEmpty(dateRefStr)) {
                            PlainDate dateRef = DateUtils.getDate(dateRefStr);
                            if (dateRef != null) {
                                subreportScoliosis.set(RF_SCOLIOSIS_DATE_REF, m_dateFormat.format(dateRef));
                            }
                        }
                        String dateRefComplStr =
                                (String) scoliosis.getFieldValueByAlias(ALIAS_HSC_DATE_REF_COMPL);
                        if (!StringUtils.isEmpty(dateRefComplStr)) {
                            PlainDate dateRefCompl = DateUtils.getDate(dateRefComplStr);
                            if (dateRefCompl != null) {
                                subreportScoliosis.set(RF_SCOLIOSIS_DATE_REF_COMPL, m_dateFormat.format(dateRefCompl));
                            }
                        }
                        if (m_scoliosisRescr != null) {
                            String scoliosisRescr = (String) scoliosis.getFieldValueByBeanPath(m_scoliosisRescr);
                            if (!StringUtils.isEmpty(scoliosisRescr)
                                    && BooleanAsStringConverter.TRUE.equals(scoliosisRescr)
                                    && m_scoliosisRescrDate != null) {
                                String rescrDate = (String) scoliosis.getFieldValueByBeanPath(m_scoliosisRescrDate);
                                if (rescrDate != null) {
                                    subreportScoliosis.set(RF_SCOLIOSIS_DATE_RESCR,
                                            m_dateFormat.format(DateUtils.getDate(rescrDate)));
                                    if (!StringUtils.isEmpty(scoliosisResult)) {
                                        subreportScoliosis.set(RF_SCOLIOSIS_RESULT_RESCR, scoliosisResult);
                                    }
                                }
                            }
                        }
                        if (m_scoliosisPrevDiagnose != null) {
                            String scoliosisPrevDiagnose =
                                    (String) scoliosis.getFieldValueByBeanPath(m_scoliosisPrevDiagnose);
                            if (!StringUtils.isEmpty(scoliosisPrevDiagnose)
                                    && BooleanAsStringConverter.TRUE.equals(scoliosisPrevDiagnose)) {
                                subreportScoliosis.set(RF_SCOLIOSIS_PREV_DIAGNOSE, "Y");
                            }
                        }
                    }
                }
            }
        }
        if (subreportScoliosis.isEmpty()) {
            subreportScoliosis.append();
            subreportScoliosis.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
            subreportScoliosis.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
            subreportScoliosis.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
            subreportScoliosis.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
        }
    }

    /**
     * Populate vision table.
     *
     * @param grid ReportDataGrid
     * @param std SisStudent
     */
    private void populateVisionTable(ReportDataGrid grid, SisStudent std) {
        String stdOid = std.getOid();
        Collection<HealthScreening> visionList = m_hscVisionMap.get(stdOid);
        if (visionList != null && !visionList.isEmpty()) {
            for (HealthScreening vision : visionList) {
                PlainDate hscDate = vision.getDate();
                if (hscDate != null) {
                    grid.append();
                    grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
                    grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
                    grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
                    grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
                    String calcGradeByDate = calculateGradeByDate(std.getGradeLevel(), hscDate);
                    grid.set(RF_VISION_DATE, m_dateFormat.format(hscDate));
                    grid.set(RF_VISION_GRADE, calcGradeByDate);
                    String dateRefStr = (String) vision.getFieldValueByAlias(ALIAS_HSC_DATE_REF);
                    if (!StringUtils.isEmpty(dateRefStr)) {
                        PlainDate dateRef = DateUtils.getDate(dateRefStr);
                        if (dateRef != null) {
                            grid.set(RF_VISION_DATE_REF, m_dateFormat.format(dateRef));
                        }
                    }
                    String dateRefComplStr =
                            (String) vision.getFieldValueByAlias(ALIAS_HSC_DATE_REF_COMPL);
                    if (!StringUtils.isEmpty(dateRefComplStr)) {
                        PlainDate dateRefCompl = DateUtils.getDate(dateRefComplStr);
                        if (dateRefCompl != null) {
                            grid.set(RF_VISION_DATE_REF_COMPL, m_dateFormat.format(dateRefCompl));
                        }
                    }
                    String visionResult = vision.getResultCode();
                    if (HSC_RESULT_PASS.equalsIgnoreCase(visionResult)) {
                        grid.set(RF_VISION_NEAR_R, HSC_VISION_RESULT_FOR_PASS);
                        grid.set(RF_VISION_NEAR_L, HSC_VISION_RESULT_FOR_PASS);
                        grid.set(RF_VISION_FAR_R, HSC_VISION_RESULT_FOR_PASS);
                        grid.set(RF_VISION_FAR_L, HSC_VISION_RESULT_FOR_PASS);
                        if ("01".equals(calcGradeByDate)) {
                            grid.set(RF_VISION_CONVEX_PASS, HSC_VISION_RESULT_FOR_PASS_GR_1_2);
                        }
                        if ("01".equals(calcGradeByDate) || "02".equals(calcGradeByDate)) {
                            grid.set(RF_VISION_COLOR_PASS, HSC_VISION_RESULT_FOR_PASS_GR_1_2);
                            grid.set(RF_VISION_DEPTH_PERCEPTION_PASS, HSC_VISION_RESULT_FOR_PASS_GR_1_2);
                        }
                    } else if (HSC_RESULT_FAIL.equalsIgnoreCase(visionResult)) {
                        if (m_visionNearR != null) {
                            grid.set(RF_VISION_NEAR_R, vision.getFieldValueByBeanPath(m_visionNearR));
                        }
                        if (m_visionNearL != null) {
                            grid.set(RF_VISION_NEAR_L, vision.getFieldValueByBeanPath(m_visionNearL));
                        }
                        if (m_visionFarR != null) {
                            grid.set(RF_VISION_FAR_R, vision.getFieldValueByBeanPath(m_visionFarR));
                        }
                        if (m_visionFarL != null) {
                            grid.set(RF_VISION_FAR_L, vision.getFieldValueByBeanPath(m_visionFarL));
                        }
                        if (m_visionColor != null && ("01".equals(calcGradeByDate) || "02".equals(calcGradeByDate))) {
                            grid.set(RF_VISION_COLOR_FAIL, vision.getFieldValueByBeanPath(m_visionColor));
                        }
                        if (m_visionConvex != null && "01".equals(calcGradeByDate)) {
                            grid.set(RF_VISION_CONVEX_FAIL, vision.getFieldValueByBeanPath(m_visionConvex));
                        }
                        if (m_visionDepthPerception != null
                                && ("01".equals(calcGradeByDate) || "02".equals(calcGradeByDate))) {
                            grid.set(RF_VISION_DEPTH_PERCEPTION_FAIL,
                                    vision.getFieldValueByBeanPath(m_visionDepthPerception));
                        }
                    }
                    if (m_visionLBVA != null) {
                        grid.set(RF_VISION_LENS,
                                vision.getFieldValueByBeanPath(m_visionLBVA));
                    }
                    if (m_visionLRVA != null) {
                        grid.set(RF_VISION_LENS,
                                vision.getFieldValueByBeanPath(m_visionLRVA));
                    }
                    if (m_visionLLVA != null) {
                        grid.set(RF_VISION_LENS,
                                vision.getFieldValueByBeanPath(m_visionLLVA));
                    }
                }
            }
        }
        if (grid.isEmpty()) {
            grid.append();
            grid.set(REPORT_FIELD_STUDENT_FIRST_NAME, std.getPerson().getFirstName());
            grid.set(REPORT_FIELD_STUDENT_LAST_NAME, std.getPerson().getLastName());
            grid.set(REPORT_FIELD_STUDENT_HOMEROOM, std.getHomeroom());
            grid.set(REPORT_FIELD_STUDENT_GRADE, std.getGradeLevel());
        }
    }
}
