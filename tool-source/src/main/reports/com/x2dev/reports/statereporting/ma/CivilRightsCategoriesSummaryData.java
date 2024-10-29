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
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
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
    private DataDictionaryField m_lepField = null;
    private DataDictionaryField m_raceCodeField = null;
    private Map<String, Collection<Race>> m_raceLookup = null;

    /**
     * Report gather data phase.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid(10, 10);
        initializeGrid(grid);

        /*
         * Initialize member variables
         */
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());

        m_lepField = m_dictionary.findDataDictionaryFieldByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        m_raceCodeField = m_dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        Selection studentSelection = getStudentSelection();
        m_raceLookup = getRaceLookup(studentSelection);

        addStudentCounts(grid, studentSelection);

        /*
         * Delete the selection object - it's only needed to run the student query above.
         */
        getBroker().deleteBean(studentSelection);

        grid.beforeTop();

        return grid;
    }

    /**
     * Adds the student counts.
     *
     * @param grid ReportDataGrid
     * @param studentSelection Selection
     */
    private void addStudentCounts(ReportDataGrid grid, Selection studentSelection) {
        /*
         * Build student criteria.
         */
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID,
                Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        /*
         * Generate query
         */
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                for (int i = 0; i < NUMBER_OF_ROWS; i++) {
                    if (includeStudentInRow(student, i)) {
                        grid.gotoRow(i);

                        for (int j = 1; j <= NUMBER_OF_COLUMNS; j++) // Column 0 is reserved for the
                                                                     // row label
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
                                                                                       // columns
                                                                                       // are for
                                                                                       // totals
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
        } finally {
            iterator.close();
        }
    }

    /**
     * Load a map of Race code objects by PersonOid, based on a snapshot of students.
     *
     * @param studentSelection Selection
     * @return Map<String, Collection<Race>>
     */
    private Map<String, Collection<Race>> getRaceLookup(Selection studentSelection) {
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, studentSelection.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                + X2BaseBean.COL_OID);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);

        Criteria raceCriteria = new Criteria();
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
            SelectionObject selectedObject = X2BaseBean.newInstance(SelectionObject.class,
                    getBroker().getPersistenceKey());
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

        SisPerson person = student.getPerson();
        String genderCode = person != null ? person.getGenderCode() : null;
        boolean male = "M".equalsIgnoreCase(genderCode);

        switch (columnNumber) {
            case 1: // Hispanic/latino, male
                include = male && student.getPerson().getHispanicLatinoIndicator();
                break;

            case 2: // Indian, male
                include = male && hasRaceCode(student, "8");
                break;

            case 3: // Asian, male
                include = male && hasRaceCode(student, "4");
                break;

            case 4: // Pac Islander/Hawaiian, male
                include = male && hasRaceCode(student, "16");
                break;

            case 5: // Black, male
                include = male && hasRaceCode(student, "2");
                break;

            case 6: // White, male
                include = male && hasRaceCode(student, "1");
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
                include = male && isLep(student);
                break;

            case 11: // Hispanic/latino, female
                include = !male && student.getPerson().getHispanicLatinoIndicator();
                break;

            case 12: // Indian, female
                include = !male && hasRaceCode(student, "8");
                break;

            case 13: // Asian, female
                include = !male && hasRaceCode(student, "4");
                break;

            case 14: // Pac Islander/Hawaiian, female
                include = !male && hasRaceCode(student, "16");
                break;

            case 15: // Black, female
                include = !male && hasRaceCode(student, "2");
                break;

            case 16: // White, female
                include = !male && hasRaceCode(student, "1");
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
                include = !male && isLep(student);
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
     * Determine if the student is LEP.
     *
     * @param student SisStudent
     * @return boolean
     */
    private boolean isLep(SisStudent student) {
        boolean include = false;

        String lepValue = (String) student.getFieldValueByAlias(STUDENT_CONSIDERED_LEP_ALIAS);
        if (lepValue != null) {
            String stateCode = m_dictionary.findStateReferenceCode(m_lepField.getReferenceTableOid(), lepValue);
            include = STUDENT_IS_LEP.equalsIgnoreCase(stateCode);
        }

        return include;
    }
}
