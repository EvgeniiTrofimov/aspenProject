/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting;

import static com.x2dev.utils.converters.BooleanAsStringConverter.TRUE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the student summary by civil rights categories report. This class creates a
 * report data grid containing student counts. The grid contains the following line items:
 *
 * Each row contains the following count columns:
 * <ul>
 * <li>Hispanic/Latino (HL)
 * <li>American Indian (IND)
 * <li>Asian (ASN)
 * <li>Native Hawaiian/Pacific Islander (HPI)
 * <li>Black or African American (BAA)
 * <li>White (WHI)
 * <li>Two or more races (MULTI)
 * <li>Active sped (IDEA) (Active special education students)
 * <li>Active 504 (504) (Active 504 students)
 * <li>Active LEP (LEP) (Based on DOE 26)
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class CivilRightsCategoriesSummaryData extends ReportJavaSourceNet {
    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "queryBy" input parameter. This value is a string.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "snapshot" input parameter. This value is a String. If this value is not null,
     * then the student selection should be just the members of the snapshot
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Alias of the student field containing the
     */
    public static final String STUDENT_CONSIDERED_LEP_ALIAS = "DOE 25";
    public static final String STUDENT_IS_LEP = "01";

    /*
     * Local constants
     */
    private static final Integer INTEGER_ZERO = Integer.valueOf(0);
    private static final int NUMBER_OF_COLUMNS = 20;
    private static final int NUMBER_OF_ROWS = 15;
    private static final int NUMBER_OF_TOTAL_COLUMNS = 30;
    private static final String RACE_MULTI = "MULTI";

    /*
     * Instance variables
     */
    private DataDictionary m_dictionary = null;
    private HashMap<String, Integer> m_gradeLevelLookup = null;
    // private DataDictionaryField m_lepField = null;
    private DataDictionaryField m_raceCodeField = null;
    private Map<String, Collection<Race>> m_raceLookup = null;

    // State Codes
    public static final String STATE_CODE_CT = "CT";
    public static final String STATE_CODE_FL = "FL";
    public static final String STATE_CODE_GA = "GA";
    public static final String STATE_CODE_IL = "IL";
    public static final String STATE_CODE_MA = "MA";
    public static final String STATE_CODE_MD = "MD";
    public static final String STATE_CODE_NH = "NH";
    public static final String STATE_CODE_NY = "NY";
    public static final String STATE_CODE_RI = "RI";
    public static final String STATE_CODE_VA = "VA";

    /**
     * Aliases
     */
    // Student
    public static final String ALIAS_CT_CRDC_LEP_IND = "PSIS14";
    public static final String ALIAS_GA_CRDC_LEP_IND = "DOE ELL";
    public static final String ALIAS_MA_CRDC_LEP_IND = "DOE 25";
    public static final String ALIAS_MD_CRDC_LEP_IND = "DOE ELL";
    public static final String ALIAS_IL_CRDC_LEP_IND = "DOE ELL STATUS";
    public static final String ALIAS_RI_CRDC_LEP_IND = "ESL Flag";
    public static final String ALIAS_DEFAULT_CRDC_LEP_IND = "CRDC LEP IND";

    public static final String ALIAS_CT_CRDC_GIFTED_IND = "PSIS27";
    public static final String ALIAS_FL_CRDC_GIFTED_IND = "FL Gifted";
    public static final String ALIAS_GA_CRDC_GIFTED_IND = "DOE Gift Referral";
    public static final String ALIAS_MA_CRDC_GIFTED_IND = "gifted";
    public static final String ALIAS_VA_CRDC_GIFTED_IND = "DOE GIFTED REFERRAL";
    public static final String ALIAS_DEFAULT_CRDC_GIFTED_IND = "CRDC GIFTED";

    public static final String ALIAS_VA_CRDC_GED_IND = "DOE GED";
    public static final String ALIAS_DEFAULT_CRDC_GED_IND = "CRDC GED";

    public static final String ALIAS_CT_CRDC_LEP_PROGRAM_IND = "PSIS15";
    public static final String ALIAS_IL_CRDC_LEP_PROGRAM_IND = "DOE CLS PERIODS WEEK";
    public static final String ALIAS_MA_CRDC_LEP_PROGRAM_IND = "DOE 26";
    public static final String ALIAS_DEFAULT_CRDC_LEP_PROGRAM_IND = "CRDC LEP PROGRAM IND";

    public static final String ALIAS_GA_CRDC_RETAINED = "DOE Retained";
    public static final String ALIAS_VA_CRDC_RETAINED = "DOE RETAINED";
    public static final String ALIAS_DEFAULT_CRDC_RETAINED = "CRDC RETAINED";

    // Course
    public static final String ALIAS_CT_CRDC_SCED_CODE = "DOE COURSE CODE";
    public static final String ALIAS_IL_CRDC_SCED_CODE = "DOE STATE COURSE ID";
    public static final String ALIAS_MA_CRDC_SCED_CODE = "WA10-MTC";
    public static final String ALIAS_MD_CRDC_SCED_CODE = "DOE SUBJECT";
    public static final String ALIAS_NY_CRDC_SCED_CODE = "DOE STATE COURSE";
    public static final String ALIAS_RI_CRDC_SCED_CODE = "RI Course ID";
    public static final String ALIAS_VA_CRDC_SCED_CODE = "DOE SCED COURSE";
    public static final String ALIAS_DEFAULT_CRDC_SCED_CODE = "CRDC COURSE ID";

    public static final String ALIAS_NY_CRDC_EXCLUDE_IND = "DOE EXCLUDE SCGD";
    public static final String ALIAS_DEFAULT_CRDC_EXCLUDE_IND = "DOE EXCLUDE CRDC";

    public static final String ALIAS_NY_CRDC_OVERRIDE = "DOE SCED OVERRIDE";
    public static final String ALIAS_DEFAULT_CRDC_OVERRIDE = "DOE SCED OVERRIDE";

    // ConductAction
    public static final String ALIAS_CT_CRDC_ARRESTED_IND = "DOE ARRESTED";
    public static final String ALIAS_GA_CRDC_EXPULSION_WITH_EDUCATION = "DOE CONTINUATION";
    public static final String ALIAS_GA_CRDC_EXPULSION_WITHOUT_EDUCATION = "DOE CONTINUATION";
    public static final String ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION = "SSDR 30";
    public static final String ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION = "SSDR 30";
    public static final String ALIAS_NH_CRDC_ARRESTED_IND = "DOE ARRESTED";
    public static final String ALIAS_NH_CRDC_REFERRAL_TO_LAW = "DOE INC REPORTED";
    public static final String ALIAS_VA_CRDC_REFERRAL_TO_LAW = "DOE INC REPORTED";

    // Staff
    public static final String ALIAS_DEFAULT_CRDC_SUBJECT_AREA = "CRDC SUBJECT AREA";

    // State Reference Codes
    public static final String REF_CODE_FL_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_FL_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_FL_CRDC_ASIAN = "A";
    public static final String REF_CODE_FL_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_FL_CRDC_PACIFIC_ISLANDER = "A";

    public static final String REF_CODE_GA_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_GA_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_GA_CRDC_ASIAN = "S";
    public static final String REF_CODE_GA_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_GA_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_IL_CRDC_CAUCASIAN = "16";
    public static final String REF_CODE_IL_CRDC_AFRICAN_AMERICAN = "14";
    public static final String REF_CODE_IL_CRDC_ASIAN = "13";
    public static final String REF_CODE_IL_CRDC_NATIVE_AMERICAN = "12";
    public static final String REF_CODE_IL_CRDC_PACIFIC_ISLANDER = "15";

    public static final String REF_CODE_MD_CRDC_CAUCASIAN = "5";
    public static final String REF_CODE_MD_CRDC_AFRICAN_AMERICAN = "3";
    public static final String REF_CODE_MD_CRDC_ASIAN = "2";
    public static final String REF_CODE_MD_CRDC_NATIVE_AMERICAN = "1";
    public static final String REF_CODE_MD_CRDC_PACIFIC_ISLANDER = "4";

    public static final String REF_CODE_NH_CRDC_CAUCASIAN = "5";
    public static final String REF_CODE_NH_CRDC_AFRICAN_AMERICAN = "4";
    public static final String REF_CODE_NH_CRDC_ASIAN = "2";
    public static final String REF_CODE_NH_CRDC_NATIVE_AMERICAN = "1";
    public static final String REF_CODE_NH_CRDC_PACIFIC_ISLANDER = "6";

    public static final String REF_CODE_NY_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_NY_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_NY_CRDC_ASIAN = "A";
    public static final String REF_CODE_NY_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_NY_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_RI_CRDC_CAUCASIAN = "E";
    public static final String REF_CODE_RI_CRDC_AFRICAN_AMERICAN = "C";
    public static final String REF_CODE_RI_CRDC_ASIAN = "B";
    public static final String REF_CODE_RI_CRDC_NATIVE_AMERICAN = "A";
    public static final String REF_CODE_RI_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_DEFAULT_CRDC_CAUCASIAN = "1";
    public static final String REF_CODE_DEFAULT_CRDC_AFRICAN_AMERICAN = "2";
    public static final String REF_CODE_DEFAULT_CRDC_ASIAN = "4";
    public static final String REF_CODE_DEFAULT_CRDC_NATIVE_AMERICAN = "8";
    public static final String REF_CODE_DEFAULT_CRDC_PACIFIC_ISLANDER = "16";

    public static final String REF_CODE_IL_CRDC_MALE = "01";
    public static final String REF_CODE_IL_CRDC_FEMALE = "02";
    public static final String REF_CODE_MD_CRDC_MALE = "1";
    public static final String REF_CODE_MD_CRDC_FEMALE = "2";
    public static final String REF_CODE_DEFAULT_CRDC_MALE = "M";
    public static final String REF_CODE_DEFAULT_CRDC_FEMALE = "F";

    public static final String FEDERAL_CODE_AP = "AP";
    public static final String FEDERAL_CODE_AG = "AG";
    public static final String FEDERAL_CODE_OM = "OM";
    public static final String FEDERAL_CODE_SC = "SC";
    public static final String FEDERAL_CODE_ELA = "ELA";

    public static final String CRDC_COURSE_ADV_MATH = "Advanced Mathematics";
    public static final String CRDC_COURSE_ALGEBRA_I = "Algebra I";
    public static final String CRDC_COURSE_ALGEBRA_II = "Algebra II";
    public static final String CRDC_COURSE_BIOLOGY = "Biology";
    public static final String CRDC_COURSE_CALCULUS = "Calculus";
    public static final String CRDC_COURSE_CHEMISTRY = "Chemistry";
    public static final String CRDC_COURSE_GEOMETRY = "Geometry";
    public static final String CRDC_COURSE_PHYSICS = "Physics";
    public static final String CRDC_COURSE_AP = "AP";

    public static final String CRDC_COURSE_COLUMN = "Course";
    public static final String CRDC_NUMBER_COLUMN = "Number of Classes";

    public static final String TABLE_LIST_OF_STUDENTS = "List of Students";

    protected static final String STF_COUNSELOR_TYPE = "Counselor";

    protected CrdcHelper m_crdcHelper;

    /**
     * Report gather data phase.
     *
     * @return JRDataSource
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(10, 10);
        initializeGrid(grid);

        // load CRDC helper
        Organization organization = OrganizationManager.getRootOrganization(getOrganization());
        Map<String, Object> parametersMap = new HashMap<String, Object>();
        m_crdcHelper = new CrdcHelper(getBroker(), getPrivilegeSet(), organization, parametersMap);

        /*
         * Initialize member variables
         */
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());

        // m_reportDate = new PlainDate();
        // m_lepField = m_dictionary.findDataDictionaryFieldByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        m_raceCodeField = m_dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        Selection studentSelection = getStudentSelection();
        m_raceLookup = getRaceLookup(studentSelection);

        calcStudentCounts(grid, studentSelection);

        /*
         * Delete the selection object - it's only needed to run the student query above.
         */
        getBroker().deleteBean(studentSelection);

        grid.beforeTop();

        return grid;
    }

    /**
     * Calc student counts.
     *
     * @param grid ReportDataGrid
     * @param studentSelection Selection
     */
    private void calcStudentCounts(ReportDataGrid grid, Selection studentSelection) {
        /*
         * Build student criteria.
         */
        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        /*
         * Generate query
         */
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);

        Collection<SisStudent> students = getBroker().getCollectionByQuery(query);

        for (SisStudent student : students) {
            for (int i = 0; i < NUMBER_OF_ROWS; i++) {
                if (includeStudentInRow(student, i)) {
                    grid.gotoRow(i);

                    for (int j = 1; j <= NUMBER_OF_COLUMNS; j++) // Column 0 is reserved for the row
                                                                 // label
                    {
                        String columnId = Integer.toString(j);
                        Integer currentCount = (Integer) grid.get(columnId);
                        if (currentCount == null) {
                            currentCount = INTEGER_ZERO;
                        }

                        if (includeStudentInColumn(student, j)) {
                            currentCount = Integer.valueOf(currentCount.intValue() + 1);
                        }

                        grid.set(columnId, currentCount);
                    }
                }
            }
        }
        for (int i = 0; i < NUMBER_OF_ROWS; i++) {
            grid.gotoRow(i);

            for (int j = NUMBER_OF_COLUMNS + 1; j <= NUMBER_OF_TOTAL_COLUMNS; j++) // Last 10
                                                                                   // columns are
                                                                                   // for totals
            {
                String columnId = Integer.toString(j);
                String maleColumnId = Integer.toString(j - NUMBER_OF_COLUMNS);
                String femaleColumnId = Integer.toString(j - NUMBER_OF_COLUMNS / 2);

                Integer maleCount = (Integer) grid.get(maleColumnId);
                if (maleCount == null) {
                    maleCount = INTEGER_ZERO;
                }
                Integer femaleCount = (Integer) grid.get(femaleColumnId);
                if (femaleCount == null) {
                    femaleCount = INTEGER_ZERO;
                }
                Integer currentCount = Integer.valueOf(maleCount.intValue() + femaleCount.intValue());

                grid.set(columnId, currentCount);
            }
        }

        // return grid;
    }

    /**
     * Load a map of Race code objects by PersonOid, based on a snapshot of students.
     *
     * @param studentSelection Selection
     * @return Map<String, Collection<Race>>
     */
    private Map<String, Collection<Race>> getRaceLookup(Selection studentSelection) {
        X2Criteria subCriteria = new X2Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);

        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);

        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);

        return getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 2048);
    }

    /**
     * Returns a Selection of the students.
     *
     * @return Selection
     */
    private Selection getStudentSelection() {
        Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());

        Collection<String> currentMembers = null;

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);

        X2Criteria criteria = new X2Criteria();

        /*
         * If we are in the school context, filter the list by the current school. This is not
         * necessary if current selection is being used, assuming that the user cannot create an
         * out-of-scope selection.
         */
        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            criteria.addNotEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
            criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
        }

        addUserCriteria(criteria, queryBy, queryString, null, null);
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
        currentMembers = getBroker().getSubQueryCollectionByQuery(subQuery);

        for (String oid : currentMembers) {
            SelectionObject selectedObject =
                    X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
            selectedObject.setObjectOid(oid);
            selection.addToSelectionObjects(selectedObject);
        }

        selection.setTimestamp(System.currentTimeMillis());
        getBroker().saveBeanForced(selection);

        return selection;
    }

    /**
     * Determine if a student has a specific race code.
     *
     * @param student SisStudent
     * @param stateRaceCode String
     * @return boolean
     */
    private boolean hasRaceCode(SisStudent student, String stateRaceCode) {
        boolean hasRaceCode = false;

        Collection<Race> raceCodes = m_raceLookup.get(student.getPersonOid());

        if (!student.getPerson().getHispanicLatinoIndicator()) {
            if (RACE_MULTI.equals(stateRaceCode)) {
                hasRaceCode = raceCodes != null && raceCodes.size() > 1;
            } else if (raceCodes != null && raceCodes.size() == 1) {
                for (Race race : raceCodes) {
                    String stateCode = m_dictionary.findStateReferenceCode(m_raceCodeField.getReferenceTableOid(),
                            race.getRaceCode());
                    String thisStateCode = stateCode;
                    if (StringUtils.isNumeric(stateCode)) {
                        thisStateCode = Integer.valueOf(stateCode).toString();
                    }

                    hasRaceCode = thisStateCode != null && thisStateCode.equals(stateRaceCode);
                }
            }
        }

        return hasRaceCode;
    }

    /**
     * Determine if a student qualifies for a specific column based on race, gender, sped, 504 and
     * LEP status.
     *
     * @param student SisStudent
     * @param columnNumber int
     * @return boolean
     */
    private boolean includeStudentInColumn(SisStudent student, int columnNumber) {
        boolean include = false;

        String gender = m_crdcHelper.getGender(student);
        boolean male = false;
        if (CrdcHelper.MALE.equals(gender)) {
            male = true;
        }

        String raceCode = null;
        switch (columnNumber) {
            case 1: // Hispanic/latino, male
                include = male && student.getPerson().getHispanicLatinoIndicator();
                break;

            case 2: // Indian, male
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.NATIVE_AMERICAN);
                include = male && hasRaceCode(student, raceCode);
                break;

            case 3: // Asian, male
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.ASIAN);
                include = male && hasRaceCode(student, raceCode);
                break;

            case 4: // Pac Islander/Hawaiian, male
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.PACIFIC_ISLANDER);
                include = male && hasRaceCode(student, raceCode);
                break;

            case 5: // Black, male
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.AFRICAN_AMERICAN);
                include = male && hasRaceCode(student, raceCode);
                break;

            case 6: // White, male
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.CAUCASIAN);
                include = male && hasRaceCode(student, raceCode);
                break;

            case 7: // Multiple races, male
                include = male && hasRaceCode(student, RACE_MULTI);
                break;

            case 8: // Special education, male
                include = male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;

            case 9: // 504, male
                include = male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;

            case 10: // LEP, male
                include = male && m_crdcHelper.isConsideredLep(student);
                break;


            case 11: // Hispanic/latino, female
                include = !male && student.getPerson().getHispanicLatinoIndicator();
                break;

            case 12: // Indian, female
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.NATIVE_AMERICAN);
                include = !male && hasRaceCode(student, raceCode);
                break;

            case 13: // Asian, female
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.ASIAN);
                include = !male && hasRaceCode(student, raceCode);
                break;

            case 14: // Pac Islander/Hawaiian, female
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.PACIFIC_ISLANDER);
                include = !male && hasRaceCode(student, raceCode);
                break;

            case 15: // Black, female
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.AFRICAN_AMERICAN);
                include = !male && hasRaceCode(student, raceCode);
                break;

            case 16: // White, female
                raceCode = m_crdcHelper.getRaceCode(CrdcHelper.Category.CAUCASIAN);
                include = !male && hasRaceCode(student, raceCode);
                break;

            case 17: // Multiple races, female
                include = !male && hasRaceCode(student, RACE_MULTI);
                break;

            case 18: // Special education, female
                include = !male && student.getSpedStatusCodeEnum() == SpedStatusCode.ACTIVE;
                break;

            case 19: // 504, female
                include = !male && student.getSection504StatusCodeEnum() == Section504StatusCode.ACTIVE;
                break;

            case 20: // LEP, female
                include = !male && m_crdcHelper.isConsideredLep(student);
                break;
        }

        return include;
    }

    /**
     * Determine if a student is in the specified row (grade level).
     *
     * @param student SisStudent
     * @param rowNumber int
     * @return true, if successful
     */
    private boolean includeStudentInRow(SisStudent student, int rowNumber) {
        boolean include = false;

        switch (rowNumber) {
            case 0: // Pre-Kindergarten males
                include = isInGradeRange(student, -1000, -1);
                break;

            case 14: // Total
                include = true;
                break;

            default: // K-12
                include = isInGradeRange(student, rowNumber - 1, rowNumber - 1);
                break;
        }

        return include;
    }

    /**
     * Initialize grid.
     *
     * @param grid ReportDataGrid
     */
    private void initializeGrid(ReportDataGrid grid) {
        // Note - number of rows appended here must match NUMBER_OF_ROWS

        grid.append(); // Row 0
        grid.set("0", "PK");

        grid.append(); // Row 1
        grid.set("0", "K");

        grid.append(); // Row 2
        grid.set("0", "01");

        grid.append(); // Row 3
        grid.set("0", "02");

        grid.append(); // Row 4
        grid.set("0", "03");

        grid.append(); // Row 5
        grid.set("0", "04");

        grid.append(); // Row 6
        grid.set("0", "05");

        grid.append(); // Row 7
        grid.set("0", "06");

        grid.append(); // Row 8
        grid.set("0", "07");

        grid.append(); // Row 9
        grid.set("0", "08");

        grid.append(); // Row 10
        grid.set("0", "09");

        grid.append(); // Row 11
        grid.set("0", "10");

        grid.append(); // Row 12
        grid.set("0", "11");

        grid.append(); // Row 13
        grid.set("0", "12");

        grid.append(); // Row 14
        grid.set("0", "Total");
    }

    /**
     * Determine if a student is in a specified grade range.
     *
     * @param student SisStudent
     * @param startGrade int
     * @param endGrade int
     * @return boolean
     */
    private boolean isInGradeRange(SisStudent student, int startGrade, int endGrade) {
        boolean include;
        Integer gradeLevel = m_gradeLevelLookup.get(student.getGradeLevel());
        include = gradeLevel != null &&
                gradeLevel.intValue() >= startGrade && gradeLevel.intValue() <= endGrade;
        return include;
    }

    /**
     * Civil Rights Data Collection Helper class
     * <p>
     * Helper class for Civil Rights Data Collection exports. You need to use
     * {@link #setPersonRaceMap(Map)}
     * and {@link #setReportDate(PlainDate)} to utilize this class.
     *
     * @author Follett Software Company
     *
     */
    public static class CrdcHelper extends StateReportData {

        /**
         * Category codes.
         */
        static public enum Category {
            HISPANIC {
                @Override
                public String toString() {
                    return "Hispanic or Latino of any race";
                }
            },
            NATIVE_AMERICAN {
                @Override
                public String toString() {
                    return "American Indian or Alaska Native";
                }
            },
            ASIAN {
                @Override
                public String toString() {
                    return "Asian";
                }
            },
            PACIFIC_ISLANDER {
                @Override
                public String toString() {
                    return "Native Hawaiian or Other Pacific Islander";
                }
            },
            AFRICAN_AMERICAN {
                @Override
                public String toString() {
                    return "Black or African American";
                }
            },
            CAUCASIAN {
                @Override
                public String toString() {
                    return "White";
                }
            },
            UNKNOWN {
                @Override
                public String toString() {
                    return "Unknown race";
                }
            },
            MULTI {
                @Override
                public String toString() {
                    return "Two or more races";
                }
            },
            IDEA {
                @Override
                public String toString() {
                    return "Students with Disabilities (IDEA)";
                }
            },
            LEP {
                @Override
                public String toString() {
                    // Limited English Proficiency
                    return "LEP";
                }
            },
            SECTION_504 {
                @Override
                public String toString() {
                    return "Section 504 Only";
                }
            }
        }

        /**
         * Discipline a student can receive.
         */
        public enum Discipline {
            CORPORAL_PUNISHMENT, IN_SCHOOL_SUSPENSION, OUT_OF_SHCOOL_SUSPENSION, EXPULSION_WITH_EDUCATION_SERVICES, EXPULSION_WITHOUT_EDUCATION_SERVICES, EXPULSION_UNDER_ZERO_TOLERANCE, REFERRAL_TO_LAW_ENFORCEMENT, SCHOOL_RELATED_ARREST
        }

        /**
         * List of program codes for programs of study
         */
        public static final String PROGRAM_CODE_GED_FEDERAL = "GED";
        public static final String PROGRAM_CODE_GED_STATE = "GED";
        public static final String ENROLLMENT_CODE_GC = "GC";
        public static final String PROGRAM_CODE_IB = "IB";

        /**
         * List of SCED course codes for Advanced Mathematics courses
         */
        public static final String[] ADVANCED_MATH_COURSES = new String[] {
                "02057", "02073", "02075", "02101", "02102", "02103", "02104",
                "02105", "02106", "02107", "02108", "02109", "02110", "02111",
                "02112", "02113", "02141", "02149", "02201", "02202", "02204",
                "02209", "02124", "02125", "02203", "02055", "02074", "10157"
        };

        /**
         * List of SCED course codes for Algebra I courses
         */
        public static final String[] ALGEBRA_1_COURSES = new String[] {
                "02052", "02053", "02054", "52052", "02052CC"
        };

        /**
         * List of SCED course codes for Algebra II courses
         */
        public static final String[] ALGEBRA_2_COURSES = new String[] {
                "02056", "02106"
        };

        /**
         * List of SCED course codes for AP Math courses
         */
        public static final String[] AP_MATH = new String[] {
                "02124", "02125", "02203", "02204"
        };

        /**
         * List of SCED course codes for AP Science courses
         */
        public static final String[] AP_SCIENCE = new String[] {
                "03056", "03106", "03155", "03156", "03207", "03151"
        };

        /**
         * List of SCED course codes for AP Other courses
         * ****Separate check**** leaving null for logic comparison
         */
        // ??? Was commented out
        // public static final String[] AP_OTHER = null;
        public static final String[] AP_OTHER = new String[] {
                "01005", "01006", "04004", "04056", "04057", "04104", "04157",
                "04158", "04159", "04203", "04204", "04205", "04256", "05114",
                "05153", "05171", "05172", "06112", "06113", "06132", "06133",
                "06212", "06313", "10157", "10158",
                "04052", "04101", "05117", "06148",
        };

        /**
         * List of All AP SCED course codes.
         */
        public static final String[] AP_ALL = new String[] {
                "02124", "02125", "02203", "02204",
                "03056", "03106", "03155", "03156", "03207", "03151",
                "01005", "01006", "04004", "04056", "04057", "04104", "04157",
                "04158", "04159", "04203", "04204", "04205", "04256", "05114",
                "05153", "05171", "05172", "06112", "06113", "06132", "06133",
                "06212", "06313", "10157", "10158",
                "04052", "04101", "05117", "06148",
        };

        /**
         * List of SCED course codes for Biology courses
         */
        public static final String[] BIOLOGY_COURSES = new String[] {
                "03051", "03052"
        };

        /**
         * List of SCED course codes for Calculus courses
         */
        public static final String[] CALCULUS_COURSES = new String[] {
                "02121", "02122", "02123", "02125", "02124"
        };

        /**
         * List of SCED course codes for Chemistry courses
         */
        public static final String[] CHEMISTRY_COURSES = new String[] {
                "03101", "03102", "03103", "03104", "03105", "03108"
        };

        /**
         * List of SCED course codes for Geometry courses
         */
        public static final String[] GEOMETRY_COURSES = new String[] {
                "52072", "02072"
        };

        /**
         * List of SCED course codes for International Baccalaureate (IB) Diploma programs
         */
        public static final String[] IB_COURSES = new String[] {
                "01007", "02131", "02132", "02133", "02134", "03057", "03107",
                "03157", "03160", "03206", "03208", "04003", "04054", "04066",
                "04206", "04253", "04257", "04262", "04304", "04309", "05115",
                "05173", "06110", "06111", "06130", "06131", "06150", "06151",
                "06170", "06171", "06190", "06191", "06210", "06211", "06250",
                "06251", "06270", "06271", "06290", "06291", "06311", "06331",
                "06410", "06411", "06430", "06431", "06450", "06451", "06490",
                "06491", "06510", "06511", "06530", "06531", "06590", "06591",
                "06610", "06611", "06650", "06651", "06670", "06671", "06690",
                "06691", "06710", "06711", "06712", "06730", "06731", "06732",
                "06770", "06771", "06790", "06791", "06830", "06831", "06850",
                "06851", "06870", "06871", "10007", "10159", "12059", "51007",
                "52132", "53203", "54171", "55202", "56101", "56121", "56141",
                "56161", "56201", "56281", "56401", "56421", "56441", "56501",
                "56521", "56601", "56701", "56721", "56761", "56801", "56821",
                "58040", "71052", "72260", "73041"
        };

        /**
         * List of SCED course codes for Physics courses
         */
        public static final String[] PHYSICS_COURSES = new String[] {
                "03151", "03152"
        };

        /**
         * List of SCED course codes for Foreign Language courses
         */
        public static final String[] AP_FOREIGN_LANGUAGE = new String[] {
                "06112", "06113", "06132", "06133", "06212", "06313", "06A01", "06A02", "06A03", "06A04"
        };

        /**
         * Category
         */
        public static final String CATEGORY = "Category";

        /**
         * Male code
         */
        public static final String MALE = "Male";

        /**
         * Female code
         */
        public static final String FEMALE = "Female";

        /**
         * Does the {@code array} contain {@code value}?.
         *
         * @param array String[]
         * @param value String
         * @return true, if successful
         */
        public static boolean arrayContains(String[] array, String value) {
            boolean contains = false;

            if (value != null) {
                value = value.trim();
            }

            for (int i = 0; i < array.length; i++) {
                if (array[i].equals(value)) {
                    contains = true;
                    break;
                }
            }
            return contains;
        }

        /**
         * Translate a String to a boolean. Case insensitive.
         * <p>
         * Example:
         * 
         * <pre>
         * +------------+-------------+
         * | s | result |
         * +------------+-------------+
         * | Y | true |
         * | Yes | true |
         * | 1 | true |
         * +------------+-------------+
         * </pre>
         *
         * @param s String
         * @return true, if successful
         */
        public static boolean logical(String s) {
            return (s != null) && s.matches("(?i)y(es)?|0*1|" + TRUE);
        }

        /**
         * Creates a generic data grid used by CRDC:
         * 
         * <pre>
         * +-------------------------------------------+------+--------+
         * | Category | Male | Female |
         * +-------------------------------------------+------+--------+
         * | Hispanic or Latino of any race | 0 | 0 |
         * | American Indian or Alaska Native | 0 | 0 |
         * | Asian | 0 | 0 |
         * | Native Hawaiian or Other Pacific Islander | 0 | 0 |
         * | Black or African American | 0 | 0 |
         * | White | 0 | 0 |
         * | Unknown race | 0 | 0 |
         * | Two or more races | 0 | 0 |
         * | Students with Disabilities (IDEA) | 0 | 0 |
         * | LEP | 0 | 0 |
         * | Section 504 Only | 0 | 0 |
         * +-------------------------------------------+------+--------+
         * </pre>
         * 
         * .
         *
         * @return DataGrid
         */
        public static DataGrid generateDataGrid() {
            DataGrid grid = new DataGrid();
            for (Category category : Category.values()) {
                grid.append();
                grid.set(CrdcHelper.CATEGORY, category);
                grid.set(CrdcHelper.MALE, Integer.valueOf(0));
                grid.set(CrdcHelper.FEMALE, Integer.valueOf(0));
            }
            return grid;
        }

        /**
         * Map of action codes by discipline
         */
        private EnumMap<Discipline, Collection<String>> m_actionCodes;

        /**
         * Map of student's conduct actions by discipline
         */
        private EnumMap<Discipline, Map<String, Collection<ConductAction>>> m_discipline;

        /**
         * State code value for female
         */
        private String m_femaleStateCode;

        /**
         * Field on the COURSE table that identifies what kind of class it is
         */
        private String m_fieldCourseScedCode;

        private String m_fieldCourseScedExclude;

        private String m_fieldCourseScedCodeOverride;

        /**
         * Map of LEP program by student oids
         */
        private Map<String, Collection<StudentProgramParticipation>> m_lepMap;

        /**
         * State code value for male
         */
        private String m_maleStateCode;

        /**
         * Map of person's races
         */
        private Map<String, Collection<Race>> m_personRaceMap;

        /**
         * Set of student OIDs that have a 504 Student Ed. Plan that is "Active" or
         * that the report date is between the effective date and end date
         */
        private Set<String> m_plansSet;

        /**
         * Map of race state codes
         */
        private Map<Category, String> m_raceCodes;

        /**
         * The report date
         */
        private PlainDate m_reportDate;

        /**
         * The start date (used to query program participations)
         */
        private PlainDate m_startDate;
        // private PlainDate m_endDate;

        /**
         * State abbreviation
         */
        protected String m_state;
        // protected ReportJavaSourceNet m_data;

        /**
         * Constructor.
         *
         * @param broker X2Broker
         * @param privilegeSet PrivilegeSet
         * @param organization Organization
         * @param parametersMap Map<String,Object>
         */
        public CrdcHelper(X2Broker broker, PrivilegeSet privilegeSet, Organization organization,
                Map<String, Object> parametersMap) {
            if (organization == null) {
                throw new X2RuntimeException();
            }

            setBroker(broker);
            setPrivilegeSet(privilegeSet);
            setOrganization(organization);
            setParameters(parametersMap);
            setSchoolContext(false);

            m_state = PreferenceManager.getPreferenceValue(organization, SystemPreferenceDefinition.STATE);
            m_startDate = organization.getCurrentContext().getStartDate();
            m_reportDate = new PlainDate();

            m_actionCodes = new EnumMap<Discipline, Collection<String>>(Discipline.class);
            m_discipline = new EnumMap<Discipline, Map<String, Collection<ConductAction>>>(Discipline.class);
            for (Discipline discipline : Discipline.values()) {
                m_discipline.put(discipline, new HashMap<String, Collection<ConductAction>>());
                m_actionCodes.put(discipline, new ArrayList<String>());
            }

            loadGenderCodes();
            loadRaceCodes();
            loadLepPrograms();
            loadEdPlans();
        }

        /**
         * Get the student's gender
         * <p>
         * If the student's person's gender code equals the state's male code, that student is
         * {@value #MALE}.
         * <p>
         * If the student's person's gender code equals the state's female code, that student is
         * {@value #FEMALE}.
         *
         * @param student SisStudent
         * @return String
         */
        public String getGender(SisStudent student) {
            String genderCodeAsString = student.getPerson().getGenderCode();
            String genderState = lookupStateValue(Person.class, Person.COL_GENDER_CODE, genderCodeAsString);
            String result = null;
            if (m_maleStateCode.equals(genderState)) {
                result = MALE;
            } else if (m_femaleStateCode.equals(genderState)) {
                result = FEMALE;
            }

            return result;
        }

        /**
         * Gets the race code.
         *
         * @param category Category
         * @return String
         */
        public String getRaceCode(Category category) {
            return m_raceCodes.get(category);
        }

        /**
         * Returns the javaName for a 'retained' field on the STUDENT table if it exists for the
         * state
         * <p>
         * Returns an empty string if the state doesn't have one.
         *
         * @return String
         */
        public String getRetainedCode() {
            String retainedCode = "";
            if (m_state.equals(STATE_CODE_GA)) {
                retainedCode = translateAliasToJavaName(ALIAS_GA_CRDC_RETAINED, true);
            } else if (m_state.equals(STATE_CODE_VA)) {
                retainedCode = translateAliasToJavaName(ALIAS_VA_CRDC_RETAINED, true);
            } else {
                retainedCode = translateAliasToJavaName(ALIAS_DEFAULT_CRDC_RETAINED, false);
            }

            return retainedCode;
        }

        /**
         * Returns the javaName for a 'retained' field on the STUDENT table if it exists for the
         * state
         * <p>
         * Returns an empty string if the state doesn't have one.
         *
         * @return State report validation error
         */
        public StateReportValidationError getValidationErrorRetained() {
            StateReportValidationError error = null;
            String alias = null;

            if (m_state.equals(STATE_CODE_GA)) {
                alias = ALIAS_GA_CRDC_RETAINED;
            } else if (m_state.equals(STATE_CODE_VA)) {
                alias = ALIAS_VA_CRDC_RETAINED;
            } else {
                alias = ALIAS_DEFAULT_CRDC_RETAINED;
            }

            error = checkForAlias(alias, ALIAS_DEFAULT_CRDC_RETAINED,
                    " The calculation for Retained will not gather information as necessary.");
            return error;
        }

        /**
         * Get the java name for the SCED course field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseCodeFieldName() {
            if (m_fieldCourseScedCode == null) {
                if (m_state.equals(STATE_CODE_MA)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_MA_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_VA)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_VA_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_CT)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_CT_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_IL)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_IL_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_MD)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_MD_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_NY_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_RI)) {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_RI_CRDC_SCED_CODE, true);
                } else {
                    m_fieldCourseScedCode = translateAliasToJavaName(ALIAS_DEFAULT_CRDC_SCED_CODE, false);
                }
            }

            return m_fieldCourseScedCode;
        }

        /**
         * Get the java name for the SCED course exclude field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseCodeOverrideFieldName() {
            if (m_fieldCourseScedCodeOverride == null) {
                if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedCodeOverride = translateAliasToJavaName(ALIAS_NY_CRDC_OVERRIDE, false);
                } else {
                    m_fieldCourseScedCodeOverride = translateAliasToJavaName(ALIAS_DEFAULT_CRDC_OVERRIDE, false);
                }
            }

            return m_fieldCourseScedCodeOverride;
        }

        /**
         * Get the java name for the SCED course exclude field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseExcludeFieldName() {
            if (m_fieldCourseScedExclude == null) {
                if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedExclude = translateAliasToJavaName(ALIAS_NY_CRDC_EXCLUDE_IND, false);
                } else {
                    m_fieldCourseScedExclude = translateAliasToJavaName(ALIAS_DEFAULT_CRDC_EXCLUDE_IND, false);
                }
            }

            return m_fieldCourseScedExclude;
        }

        /**
         * Get the java name for the SCED course field on the COURSE table.
         *
         * @return State report validation error
         */
        public StateReportValidationError getValidationErrorScedCourseCode() {
            StateReportValidationError error = null;
            String alias = null;

            if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_VA)) {
                alias = ALIAS_VA_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_CT)) {
                alias = ALIAS_CT_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_IL)) {
                alias = ALIAS_IL_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_MD)) {
                alias = ALIAS_MD_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_NY)) {
                alias = ALIAS_NY_CRDC_SCED_CODE;
            } else if (m_state.equals(STATE_CODE_RI)) {
                alias = ALIAS_RI_CRDC_SCED_CODE;
            } else {
                alias = ALIAS_DEFAULT_CRDC_SCED_CODE;
            }
            error = checkForAlias(alias, ALIAS_DEFAULT_CRDC_SCED_CODE,
                    " The calculation for calculating the number of students in a course will not gather information as necessary. Please add this to the course table.");
            return error;
        }

        /**
         * Get the student's race in {@link Category} form
         * <p>
         * This method will check the {@code student}'s race in this order:
         * <ol>
         * <li>If the student's hispanic/latino indicator is checked, return {@link
         * Category#HISPANIC HISPANIC}</li>
         * <li>If the student has 2 or more {@code Race} records, return {@link Category#MULTI
         * MULTI}</li>
         * <li>If the student has 0 {@code Race} records, return {@link Category#UNKNOWN
         * UNKNOWN}</li>
         * <li>If the student's {@code Race}'s state value matches one of the {@code m_raceState***}
         * fields,
         * return the corresponding race's Category</li>
         * </ol>
         * .
         *
         * @param student SisStudent
         * @return Category
         */
        public Category getStudentRace(SisStudent student) {
            if (m_personRaceMap == null) {
                throw new X2RuntimeException();
            }

            Category race = null;
            if (student.getPerson().getHispanicLatinoIndicator()) {
                race = Category.HISPANIC;
            } else {
                Collection<Race> races = m_personRaceMap.get(student.getPersonOid());
                if (races != null) {
                    if (races.size() > 1) {
                        race = Category.MULTI;
                    } else {
                        for (Race pRaces : races) {
                            race = Category.UNKNOWN;

                            String raceCode = pRaces.getRaceCode();
                            if (raceCode != null) {
                                String raceCodeStateValue = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                                // Some customer sites have the leading zero and some don't so we
                                // will have to deal with it either way.
                                // If there is a leading zero in the state code then remove it.
                                if (raceCodeStateValue != null) {
                                    if (raceCodeStateValue.length() > 1 && raceCodeStateValue.startsWith("0")) {
                                        raceCodeStateValue = raceCodeStateValue.substring(1);
                                    }

                                    if (StringUtils.isEqual(raceCodeStateValue, m_raceCodes.get(Category.CAUCASIAN))) {
                                        race = Category.CAUCASIAN;
                                    } else if (StringUtils.isEqual(raceCodeStateValue,
                                            m_raceCodes.get(Category.AFRICAN_AMERICAN))) {
                                        race = Category.AFRICAN_AMERICAN;
                                    } else if (StringUtils.isEqual(raceCodeStateValue,
                                            m_raceCodes.get(Category.ASIAN))) {
                                        race = Category.ASIAN;
                                    } else if (StringUtils.isEqual(raceCodeStateValue,
                                            m_raceCodes.get(Category.NATIVE_AMERICAN))) {
                                        race = Category.NATIVE_AMERICAN;
                                    } else if (StringUtils.isEqual(raceCodeStateValue,
                                            m_raceCodes.get(Category.PACIFIC_ISLANDER))) {
                                        race = Category.PACIFIC_ISLANDER;
                                    } else {
                                        race = Category.UNKNOWN;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    race = Category.UNKNOWN;
                }
            }

            return race;
        }

        /**
         * Does the {@code student} have a conduct action related to a {@code discipline}?
         * <p>
         * This only checks if there exists a conduct action, and it does not count them.
         * <p>
         * Note: This only applies to disciplines NOT in-school suspension or out-of-school
         * suspension
         *
         * @param student SisStudent
         * @param discipline Discipline
         * @return true, if successful
         */
        public boolean hasConductAction(SisStudent student, Discipline discipline) {
            boolean hasConductAction = false;
            Map<String, Collection<ConductAction>> studentActionMap = m_discipline.get(discipline);
            Collection<ConductAction> conductActions = studentActionMap.get(student.getOid());
            if (conductActions != null && conductActions.size() > 0) {
                Collection<String> disciplineCodes = m_actionCodes.get(discipline);

                // override: for Georgia or Massachusetts and discipline == expulsion without ed
                // services, go through all the expulsion
                // actions and make sure all of them DON'T have this field true
                if (m_state.matches(STATE_CODE_GA + "|" + STATE_CODE_MA)
                        && discipline == Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES) {
                    for (ConductAction action : conductActions) {
                        String eduSvcField;
                        if (m_state.equals(STATE_CODE_GA)) {
                            eduSvcField =
                                    (String) action.getFieldValueByAlias(ALIAS_GA_CRDC_EXPULSION_WITHOUT_EDUCATION);
                            hasConductAction = logical(eduSvcField);
                        } else if (m_state.equals(STATE_CODE_MA)) {
                            eduSvcField =
                                    (String) action.getFieldValueByAlias(ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION);
                            if (!StringUtils.isEmpty(eduSvcField)) {
                                String stateCode = lookupReferenceCodeByAlias(ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION,
                                        eduSvcField,
                                        ReferenceMapTypeCode.STATE.ordinal());
                                hasConductAction = !"Y".equals(stateCode);
                            }
                        }

                        if (!hasConductAction) {
                            break;
                        }
                    }
                } else {
                    for (ConductAction action : conductActions) {
                        // override: for Connecticut and New Hampshire, there's an 'arrested' field
                        // right on the conduct action/incident
                        if (m_state.matches(STATE_CODE_CT + "|" + STATE_CODE_NH)
                                && discipline == Discipline.SCHOOL_RELATED_ARREST) {
                            String arrestedField = "";
                            if (m_state.equals(STATE_CODE_CT)) {
                                arrestedField = (String) action.getFieldValueByAlias(ALIAS_CT_CRDC_ARRESTED_IND);
                            } else if (m_state.equals(STATE_CODE_NH)) {
                                ConductIncident incident = action.getIncident();
                                arrestedField = (String) incident.getFieldValueByAlias(ALIAS_NH_CRDC_ARRESTED_IND);
                            }
                            hasConductAction = logical(arrestedField);
                            if (hasConductAction) {
                                break;
                            }
                        }

                        // override: for Virginia and New Hampshire, there's a "reported to law" on
                        // the conduct incident
                        else if (m_state.matches(STATE_CODE_VA + "|" + STATE_CODE_NH)
                                && discipline == Discipline.REFERRAL_TO_LAW_ENFORCEMENT) {
                            ConductIncident incident = action.getIncident();
                            if (incident != null) {
                                String reportedField =
                                        (String) incident.getFieldValueByAlias(ALIAS_VA_CRDC_REFERRAL_TO_LAW);
                                hasConductAction = logical(reportedField);
                                if (hasConductAction) {
                                    break;
                                }
                            }
                        }

                        // override: for Georgia or Massachusetts, there's a continuing education on
                        // conduct action
                        else if (m_state.matches(STATE_CODE_GA + "|" + STATE_CODE_MA)
                                && discipline == Discipline.EXPULSION_WITH_EDUCATION_SERVICES) {
                            String eduSvcField;
                            if (m_state.equals(STATE_CODE_GA)) {
                                eduSvcField =
                                        (String) action.getFieldValueByAlias(ALIAS_GA_CRDC_EXPULSION_WITH_EDUCATION);
                                hasConductAction = logical(eduSvcField);
                            } else if (m_state.equals(STATE_CODE_MA)) {
                                eduSvcField =
                                        (String) action.getFieldValueByAlias(ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION);
                                if (!StringUtils.isEmpty(eduSvcField)) {
                                    String stateCode =
                                            lookupReferenceCodeByAlias(ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION,
                                                    eduSvcField,
                                                    ReferenceMapTypeCode.STATE.ordinal());
                                    hasConductAction = "Y".equals(stateCode);
                                }
                            }

                            if (hasConductAction) {
                                break;
                            }
                        }

                        // everyone else uses an action code
                        else {
                            String codeFromAction = action.getActionCode();
                            hasConductAction = disciplineCodes.contains(codeFromAction);
                            if (hasConductAction) {
                                break;
                            }
                        }
                    }
                }
            }
            return hasConductAction;
        }

        /**
         * Does the {@code student} have a conduct action related to a {@code discipline}?
         * <p>
         * This only checks if there exists a conduct action, and it does not count them.
         * <p>
         * Note: This only applies to disciplines NOT in-school suspension or out-of-school
         * suspension
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorHasConductAction() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;
            if (m_state.equals(STATE_CODE_GA)) {
                alias = ALIAS_GA_CRDC_EXPULSION_WITHOUT_EDUCATION;
            } else if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION;
                referenceTable = true;
            }

            errors.add(checkForAlias(alias, null,
                    " The calculation for calculating incidents of expulsion. Please add this to the conduct incident table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, null));
            }

            return errors;
        }

        /**
         * Gets the validation error has conduct action expel no ED.
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorHasConductActionExpelNoED() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;

            if (m_state.matches(STATE_CODE_CT + "|" + STATE_CODE_NH)) {
                alias = ALIAS_CT_CRDC_ARRESTED_IND;
            }
            // ??? NH
            else if (m_state.matches(STATE_CODE_VA + "|" + STATE_CODE_NH)) {
                alias = ALIAS_VA_CRDC_REFERRAL_TO_LAW;
            } else if (m_state.equals(STATE_CODE_GA)) {
                alias = ALIAS_GA_CRDC_EXPULSION_WITH_EDUCATION;
            } else if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION;
                referenceTable = true;
            }

            errors.add(checkForAlias(alias, null,
                    " The calculation for calculating incidents of expulsion with no education. Please add this to the conduct incident table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, null));
            }

            return errors;
        }

        /**
         * Checks for refenence table.
         *
         * @param alias String
         * @return true, if successful
         */
        public boolean hasRefenenceTable(String alias) {
            DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                return true;
            }

            return false;
        }

        /**
         * Check for reference table.
         *
         * @param alias String
         * @param CRDC_ALIAS String
         * @return StateReportValidationError
         */
        private StateReportValidationError checkForReferenceTable(String alias, String CRDC_ALIAS) {
            if (!hasRefenenceTable(alias)) {
                String message = "Unable to locate reference table for [" + alias;
                if (CRDC_ALIAS != null && !CRDC_ALIAS.equals(alias)) {
                    message += "] or alias [" + CRDC_ALIAS;
                }
                message += "].";
                return new StateReportValidationError("CRDC", "CRDC", "Missing Reference Table", message);
            }

            return null;
        }

        /**
         * Check for alias.
         *
         * @param alias String
         * @param CRDC_ALIAS String
         * @param calculationMessage String
         * @return StateReportValidationError
         */
        private StateReportValidationError checkForAlias(String alias, String CRDC_ALIAS, String calculationMessage) {
            if (translateAliasToJavaName(alias, false) == null && alias != null) {
                String message = "Unable to locate alias [" + alias;
                if (CRDC_ALIAS != null && !CRDC_ALIAS.equals(alias)) {
                    message += "] or alias [" + CRDC_ALIAS;
                }
                message += "]." + calculationMessage;
                return new StateReportValidationError("CRDC", "CRDC", "Missing Alias", message);
            }
            return null;
        }

        /**
         * Add a student to a standard grid.
         * <p>
         * Checks the student's race, IDEA status, Section 504 status, and LEP
         *
         * @param grid DataGrid
         * @param student SisStudent
         */
        public void includeStudent(DataGrid grid, SisStudent student) {
            // race
            Category category = getStudentRace(student);
            addStudentToCategory(grid, category, student);

            // Students with Disabilities (IDEA)
            if (isSped(student)) {
                addStudentToCategory(grid, Category.IDEA, student);
            }

            // Section 504
            if (isSection504(student)) {
                addStudentToCategory(grid, Category.SECTION_504, student);
            }

            // LEP
            if (isConsideredLep(student)) {
                addStudentToCategory(grid, Category.LEP, student);
            }
        }

        /**
         * Is the {@code student} considered an Limited English Proficiency student?
         * <p>
         * Note: not to be confused with {@link #isInLepProgram} which checks if
         * the {@code student} is <em>in</em> an LEP program.
         *
         * @param student SisStudent
         * @return true, if is considered lep
         */
        public boolean isConsideredLep(SisStudent student) {
            boolean isConsideredLep = false;

            if (m_state.equals(STATE_CODE_MA)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = lookupReferenceCodeByAlias(ALIAS_MA_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "01".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_CT)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            } else if (m_state.equals(STATE_CODE_GA)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_GA_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = lookupReferenceCodeByAlias(ALIAS_GA_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "Y".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_IL)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_IL_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = lookupReferenceCodeByAlias(ALIAS_IL_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(stateCode)) {
                        isConsideredLep = stateCode.matches("10|11|12|13");
                    }
                }
            } else if (m_state.equals(STATE_CODE_MD)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_MD_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            } else if (m_state.equals(STATE_CODE_NH)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = lookupReferenceCodeByAlias(ALIAS_DEFAULT_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "Y".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_NY)) {
                isConsideredLep = false;
                Collection<StudentProgramParticipation> programs = m_lepMap.get(student.getOid());

                if (programs != null && programs.size() > 0) {
                    for (StudentProgramParticipation program : programs) {
                        String programCode = program.getProgramCode();
                        PlainDate stateDate = program.getStartDate();
                        PlainDate endDate = program.getEndDate();

                        // If the student has any 0231 programs codes he/she is considered to be LEP
                        // Eligible
                        if (("0231".equals(programCode))
                                && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                && (endDate == null
                                        || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                            isConsideredLep = true;
                            break;
                        }
                    }
                }
            } else if (m_state.equals(STATE_CODE_RI)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_RI_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            }

            return isConsideredLep;
        }

        /**
         * Is the {@code student} considered an Limited English Proficiency student?
         * <p>
         * Note: not to be confused with {@link #isInLepProgram} which checks if
         * the {@code student} is <em>in</em> an LEP program.
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorIsConsideredLep() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;

            if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_LEP_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_CT)) {
                alias = ALIAS_CT_CRDC_LEP_IND;
            } else if (m_state.equals(STATE_CODE_GA)) {
                alias = ALIAS_GA_CRDC_LEP_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_MD)) {
                alias = ALIAS_MD_CRDC_LEP_IND;
            } else if (m_state.equals(STATE_CODE_IL)) {
                alias = ALIAS_IL_CRDC_LEP_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_RI)) {
                alias = ALIAS_RI_CRDC_LEP_IND;
            } else {
                alias = ALIAS_DEFAULT_CRDC_LEP_IND;
            }

            errors.add(checkForAlias(alias, ALIAS_DEFAULT_CRDC_LEP_IND,
                    " The calculation for calculating students who are LEP. Please add this to the student table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, ALIAS_DEFAULT_CRDC_LEP_IND));
            }
            return errors;
        }

        /**
         * Is the {@code student} gifted/talented?
         * <p>
         * If there is no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is gifted
         */
        public boolean isGifted(SisStudent student) {
            boolean isGifted = false;

            if (m_state.equals(STATE_CODE_VA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_VA_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            } else if (m_state.equals(STATE_CODE_CT)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = lookupReferenceCodeByAlias(ALIAS_CT_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("0[2-9]"); // 02 through 09
                    }
                }
            } else if (m_state.equals(STATE_CODE_GA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_GA_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = lookupReferenceCodeByAlias(ALIAS_GA_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("[1-6]"); // 1 through 6
                    }
                }
            } else if (m_state.equals(STATE_CODE_FL)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_FL_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = lookupReferenceCodeByAlias(ALIAS_FL_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("A|B");
                    }
                }
            } else if (m_state.equals(STATE_CODE_MA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            } else {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            }

            return isGifted;
        }

        /**
         * Is the {@code student} gifted/talented?
         * <p>
         * If there is no state implementation, this is always false.
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorIsGifted() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;

            if (m_state.equals(STATE_CODE_VA)) {
                alias = ALIAS_VA_CRDC_GIFTED_IND;
            } else if (m_state.equals(STATE_CODE_CT)) {
                alias = ALIAS_CT_CRDC_GIFTED_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_GA)) {
                alias = ALIAS_GA_CRDC_GIFTED_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_FL)) {
                alias = ALIAS_FL_CRDC_GIFTED_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_GIFTED_IND;
            } else {
                alias = ALIAS_DEFAULT_CRDC_GIFTED_IND;
            }

            errors.add(checkForAlias(alias, ALIAS_DEFAULT_CRDC_GIFTED_IND,
                    " The calculation for calculating students who are marked Gifted. Please add this to the student table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, ALIAS_DEFAULT_CRDC_GIFTED_IND));
            }
            return errors;
        }

        /**
         * Is the student in a GED program?
         * <p>
         * If there is no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is in ged
         */
        public boolean isInGed(SisStudent student) {
            boolean isInGed = false;

            if (m_state.equals(STATE_CODE_VA)) {
                String gedFieldValue = (String) student.getFieldValueByAlias(ALIAS_VA_CRDC_GED_IND);
                String gedStateValue = lookupReferenceCodeByAlias(ALIAS_VA_CRDC_GED_IND,
                        gedFieldValue,
                        ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(gedStateValue)) {
                    isInGed = gedStateValue.matches("1"); // might be 2 (ISAEP) or 3 (GAD)
                }
            } else {
                isInGed = logical((String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_GED_IND));
            }

            return isInGed;
        }

        /**
         * Gets the validation error is in ged.
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorIsInGed() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;

            if (m_state.equals(STATE_CODE_VA)) {
                alias = ALIAS_VA_CRDC_GED_IND;
            } else {
                alias = ALIAS_DEFAULT_CRDC_GED_IND;
            }

            errors.add(checkForAlias(alias, ALIAS_DEFAULT_CRDC_GED_IND,
                    " The calculation for students who are in GED. Please add this to the student table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, ALIAS_DEFAULT_CRDC_GED_IND));
            }
            return errors;
        }

        /**
         * Is the {@code student} <em>in an</em> LEP program?
         * <p>
         * If there's no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is in lep program
         * @throws X2RuntimeException if the program participation map is not initialized (if the
         *         state requires it),
         *         call {@link #setLepPrograms(Map)} to fix this
         */
        public boolean isInLepProgram(SisStudent student) {
            boolean isInLepProgram = false;

            if (m_state.equals(STATE_CODE_CT)) {
                String lepProgramFieldValue = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_LEP_PROGRAM_IND);
                if (!StringUtils.isEmpty(lepProgramFieldValue)) {
                    String lepProgramStateValue = lookupReferenceCodeByAlias(ALIAS_CT_CRDC_LEP_PROGRAM_IND,
                            lepProgramFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(lepProgramStateValue)) {
                        isInLepProgram = lepProgramStateValue.matches("01|02|03|05|07");
                    }
                }
            } else if (m_state.equals(STATE_CODE_IL)) {
                if (m_lepMap == null) {
                    throw new X2RuntimeException();
                }

                Collection<StudentProgramParticipation> programs = m_lepMap.get(student.getOid());

                if (programs != null && programs.size() > 0) {
                    for (StudentProgramParticipation program : programs) {
                        String lepFieldValue = (String) program.getFieldValueByAlias(ALIAS_IL_CRDC_LEP_PROGRAM_IND);
                        if (!StringUtils.isEmpty(lepFieldValue)) {
                            String lepStateValue = lookupReferenceCodeByAlias(ALIAS_IL_CRDC_LEP_PROGRAM_IND,
                                    lepFieldValue,
                                    ReferenceMapTypeCode.STATE.ordinal());
                            if (!StringUtils.isEmpty(lepStateValue)) {
                                // isInLepProgram = lepStateValue.matches("01|02|03");
                                if (lepStateValue.matches("01|02|03")) {
                                    isInLepProgram = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if (m_state.equals(STATE_CODE_MA)) {
                String lepProgramFieldValue = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_LEP_PROGRAM_IND);
                if (!StringUtils.isEmpty(lepProgramFieldValue)) {
                    String lepProgramStateValue = lookupReferenceCodeByAlias(ALIAS_MA_CRDC_LEP_PROGRAM_IND,
                            lepProgramFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(lepProgramStateValue)) {
                        isInLepProgram = lepProgramStateValue.matches("01|02|03");
                    }
                }
            } else if (m_state.equals(STATE_CODE_NY)) {
                if (m_lepMap == null) {
                    throw new X2RuntimeException();
                }

                isInLepProgram = false;
                Collection<StudentProgramParticipation> programs = m_lepMap.get(student.getOid());

                if (programs != null && programs.size() > 0) {
                    for (StudentProgramParticipation program : programs) {
                        String programCode = program.getProgramCode();
                        PlainDate stateDate = program.getStartDate();
                        PlainDate endDate = program.getEndDate();

                        // If the student has any of the LEP Programs and currently active
                        if (programCode.matches("5676|5687|5698|5709")
                                && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                && (endDate == null
                                        || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                            isInLepProgram = true;
                            break;
                        }
                    }
                }
            } else {
                // for every other state, fall back to using "is considered an LEP"
                isInLepProgram = isConsideredLep(student);
            }

            return isInLepProgram;
        }

        /**
         * Gets the validation errors is in lep program.
         *
         * @return Array list
         */
        public ArrayList<StateReportValidationError> getValidationErrorsIsInLepProgram() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String alias = null;
            boolean referenceTable = false;

            if (m_state.equals(STATE_CODE_CT)) {
                alias = ALIAS_CT_CRDC_LEP_PROGRAM_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_MA)) {
                alias = ALIAS_MA_CRDC_LEP_PROGRAM_IND;
                referenceTable = true;
            } else if (m_state.equals(STATE_CODE_IL)) {
                loadLepPrograms();
                if (m_lepMap == null) {
                    errors.add(new StateReportValidationError("CRDC", "CRDC", "Missing Map",
                            "Unable to load student LEP programs"));
                }

                alias = ALIAS_IL_CRDC_LEP_PROGRAM_IND;
                referenceTable = true;
            }

            errors.add(checkForAlias(alias, ALIAS_DEFAULT_CRDC_LEP_PROGRAM_IND,
                    " The calculation for calculating students who are marked Gifted. Please add this to the student table."));
            if (referenceTable) {
                errors.add(checkForReferenceTable(alias, ALIAS_DEFAULT_CRDC_LEP_PROGRAM_IND));
            }
            return errors;
        }

        /**
         * Check for the A student is counted as Section 504 if the following is true:
         * <p>
         * <ol>
         * <li>If the student's section 504 status is {@link Section504StatusCode#ACTIVE ACTIVE},
         * then the student is.</li>
         * <li>If the student's section 504 status is {@link Section504StatusCode#EXITED EXITED}
         * <em>and</em>
         * the student's section 504 last end date is after the report date.</li>
         * </ol>
         *
         * @param student SisStudent
         * @return true, if is section 504
         */
        public boolean isSection504(SisStudent student) {
            if (m_reportDate == null) {
                throw new X2RuntimeException();
            }

            boolean isSection504 = false;

            // 1. student has a 504 student ed plan that is 'active' and report date is between
            // effective/end date
            if (m_plansSet.contains(student.getOid())) {
                isSection504 = true;
            } else {
                // 2. std_504_status = active OR std_504_status = Exited and end date is after
                // report date
                Section504StatusCode section504StatusCode = student.getSection504StatusCodeEnum();
                if (section504StatusCode == Section504StatusCode.ACTIVE) {
                    isSection504 = true;
                } else if (section504StatusCode == Section504StatusCode.EXITED) {
                    PlainDate section504LastEndDate = student.getSection504LastEndDate();
                    if (section504LastEndDate != null && section504LastEndDate.after(m_reportDate)) {
                        isSection504 = true;
                    }
                }
            }
            return isSection504;
        }

        /**
         * Is the {@code student}'s sped status {@link SpedStatusCode#ACTIVE ACTIVE}?.
         *
         * @param student SisStudent
         * @return true, if is sped
         */
        public boolean isSped(SisStudent student) {
            if (m_reportDate == null) {
                throw new X2RuntimeException();
            }

            boolean isSped = false;
            SpedStatusCode spedStatusCode = student.getSpedStatusCodeEnum();
            if (spedStatusCode == SpedStatusCode.ACTIVE) {
                isSped = true;
            } else if (spedStatusCode == SpedStatusCode.EXITED) {
                PlainDate spedExitDate = student.getSpedExitDate();
                if (spedExitDate != null && spedExitDate.after(m_reportDate)) {
                    isSped = true;
                }
            }
            return isSped;
        }

        /**
         * Set conduct actions for a discipline.
         *
         * @param discipline the type of discipline
         * @param conductActionMap the map of conduct actions by {@code studentOid}
         * @param actionCodes conduct action codes pertaining to the {@code discipline}
         */
        public void setConductActions(Discipline discipline,
                                      Map<String, Collection<ConductAction>> conductActionMap,
                                      Collection<String> actionCodes) {
            if (conductActionMap != null) {
                m_discipline.get(discipline).putAll(conductActionMap);
            }

            if (actionCodes != null) {
                m_actionCodes.get(discipline).addAll(actionCodes);
            }
        }

        /**
         * Set the person-race map for fast retrieval.
         *
         * @param personRaceMap Map<String,Collection<Race>>
         */
        public void setPersonRaceMap(Map<String, Collection<Race>> personRaceMap) {
            m_personRaceMap = personRaceMap;
        }

        /**
         * Set the report date for use checking student's sped, section 504 status.
         *
         * @param reportDate void
         */
        public void setReportDate(PlainDate reportDate) {
            m_reportDate = reportDate;
        }

        /**
         * Set the start date (Optional)
         * <p>
         * By default, it is the district's beginning year.
         *
         * @param startDate void
         */
        public void setStartDate(PlainDate startDate) {
            m_startDate = startDate;
        }

        /**
         * Set End Date.
         *
         * @param grid DataGrid
         * @param category Category
         * @param student SisStudent
         */
        /*
         * public void setEndDate(PlainDate endDate)
         * {
         * m_endDate = endDate;
         * }
         */


        /**
         * Increments a cell to a grid
         * <p>
         * It assumes the grid is in the following format (which is generated
         * from {@link #generateDataGrid()}):
         * <p>
         * 
         * <pre>
         * +-------------------------------------------+------+--------+
         * | Category                                  | Male | Female |
         * +-------------------------------------------+------+--------+
         * | Hispanic or Latino of any race            | 0    | 0      |
         * | American Indian or Alaska Native          | 0    | 0      |
         * | Asian                                     | 0    | 0      |
         * | Native Hawaiian or Other Pacific Islander | 0    | 0      |
         * | Black or African American                 | 0    | 0      |
         * | White                                     | 0    | 0      |
         * | Unknown race                              | 0    | 0      |
         * | Two or more races                         | 0    | 0      |
         * | Students with Disabilities (IDEA)         | 0    | 0      |
         * | LEP                                       | 0    | 0      |
         * | Section 504 Only                          | 0    | 0      |
         * +-------------------------------------------+------+--------+
         * </pre>
         */
        private void addStudentToCategory(DataGrid grid,
                                          Category category,
                                          SisStudent student) {
            grid.gotoRow(category.ordinal());
            String gender = getGender(student);
            if (!StringUtils.isEmpty(gender)) {
                Integer count = (Integer) grid.get(gender);
                grid.set(gender, Integer.valueOf(count.intValue() + 1));
            }
        }

        /**
         * Load students' ed plans (for section 504 checking).
         */
        private void loadEdPlans() {
            m_plansSet = new HashSet<String>();
            String ddx504 = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.SECTION_504_DEFAULT_DEFINITION);
            if (!StringUtils.isEmpty(ddx504)) {
                /*
                 * SELECT SEP_STD_OID
                 * FROM STUDENT_ED_PLAN
                 * WHERE SEP_DDX_OID = 'ddxStandard504' AND
                 * (SEP_STATUS = '1' OR
                 * (SEP_EFFECTIVE_DATE <= 'm_reportDate' AND
                 * (SEP_END_DATE >= 'm_reportDate' OR
                 * SEP_END_DATE IS NULL)))
                 */

                X2Criteria planCriteria = new X2Criteria();
                planCriteria.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID, ddx504);

                X2Criteria endDate = new X2Criteria();
                endDate.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, m_reportDate);
                X2Criteria endDateNullCriteria = new X2Criteria();
                endDateNullCriteria.addIsNull(StudentEdPlan.COL_END_DATE);
                endDate.addOrCriteria(endDateNullCriteria);

                X2Criteria effectiveDate = new X2Criteria();
                effectiveDate.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, m_reportDate);
                effectiveDate.addAndCriteria(endDate);

                X2Criteria statusCriteria = new X2Criteria();
                statusCriteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE,
                        Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));
                statusCriteria.addOrCriteria(effectiveDate);
                planCriteria.addAndCriteria(statusCriteria);

                SubQuery subQuery = new SubQuery(StudentEdPlan.class, StudentEdPlan.COL_STUDENT_OID, planCriteria);
                Collection<String> studentOids = getBroker().getSubQueryCollectionByQuery(subQuery);
                m_plansSet.addAll(studentOids);
            }
        }

        /**
         * Get state-specific gender codes.
         */
        private void loadGenderCodes() {
            /*
             * By default...
             *
             * Male = M
             * Female = F
             */
            m_maleStateCode = REF_CODE_DEFAULT_CRDC_MALE;
            m_femaleStateCode = REF_CODE_DEFAULT_CRDC_FEMALE;

            if (m_state.equals(STATE_CODE_IL)) {
                /*
                 * Male = 01
                 * Female = 02
                 */
                m_maleStateCode = REF_CODE_IL_CRDC_MALE;
                m_femaleStateCode = REF_CODE_IL_CRDC_FEMALE;
            } else if (m_state.equals(STATE_CODE_MD)) {
                m_maleStateCode = REF_CODE_MD_CRDC_MALE;
                m_femaleStateCode = REF_CODE_MD_CRDC_FEMALE;
            }
        }

        /**
         * Load LEP programs (if states have them).
         */
        private void loadLepPrograms() {
            if (m_state.equals(STATE_CODE_IL)) {
                X2Criteria lepCriteria = new X2Criteria();
                lepCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_startDate);
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
                X2Criteria endDateCriteriaB = new X2Criteria();
                endDateCriteriaB.addNotNull(StudentProgramParticipation.COL_END_DATE);
                endDateCriteria.addOrCriteria(endDateCriteriaB);
                lepCriteria.addAndCriteria(endDateCriteria);

                String ellField = translateAliasToJavaName(ALIAS_IL_CRDC_LEP_PROGRAM_IND, true);
                lepCriteria.addNotEmpty(ellField, getBroker().getPersistenceKey());

                BeanQuery lepQuery = new BeanQuery(StudentProgramParticipation.class, lepCriteria);
                m_lepMap = getBroker().getGroupedCollectionByQuery(lepQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            } else if (m_state.equals(STATE_CODE_NY)) {
                String NYLepCodes[] = new String[] {"0231", "5676", "5687", "5698", "5709"};
                List NYLepCodeslist = Arrays.asList(NYLepCodes);

                X2Criteria lepCriteria = new X2Criteria();
                lepCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, NYLepCodeslist);
                lepCriteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

                BeanQuery lepQuery = new BeanQuery(StudentProgramParticipation.class, lepCriteria);
                m_lepMap = getBroker().getGroupedCollectionByQuery(lepQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            }
        }

        /**
         * Load race codes, depending on the states.
         */
        private void loadRaceCodes() {
            m_raceCodes = new HashMap<Category, String>();

            if (m_state.equals(STATE_CODE_FL)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = A
                 * Native American = I
                 * Pacific Islander = A
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_FL_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_FL_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_FL_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_FL_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_FL_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_GA)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = S
                 * Native American = I
                 * Pacific Islander = P
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_GA_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_GA_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_GA_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_GA_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_GA_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_IL)) {
                /*
                 * Caucasian = 16
                 * African American = 14
                 * Asian = 13
                 * Native American = 12
                 * Pacific Islander = 15
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_IL_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_IL_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_IL_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_IL_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_IL_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_MD)) {
                /*
                 * Caucasian = 5
                 * African American = 3
                 * Asian = 2
                 * Native American = 1
                 * Pacific Islander = 4
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_MD_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_MD_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_MD_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_MD_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_MD_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_NH)) {
                /*
                 * Caucasian = 5
                 * African American = 4
                 * Asian = 2
                 * Native American = 1
                 * Pacific Islander = 6
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_NH_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_NH_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_NH_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_NH_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_NH_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_NY)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = A
                 * Native American = I
                 * Pacific Islander = P
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_NY_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_NY_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_NY_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_NY_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_NY_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_RI)) {
                /*
                 * Caucasian = E
                 * African American = C
                 * Asian = B
                 * Native American = A
                 * Pacific Islander = B (same as Asian)
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_RI_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_RI_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_RI_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_RI_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_RI_CRDC_PACIFIC_ISLANDER);
            } else // by default, MA and CA uses these
            {
                /*
                 * Caucasian = 1
                 * African American = 2
                 * Asian = 4
                 * Native American = 8
                 * Pacific Islander = 16
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_DEFAULT_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_DEFAULT_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_DEFAULT_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_DEFAULT_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_DEFAULT_CRDC_PACIFIC_ISLANDER);
            }
        }

    }

}
