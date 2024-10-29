/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradeInputInitializer;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.web.gradebook.ScoreGrid;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Grade Input Sheets" report. This report is intended for use in the school
 * view's office grade input area. It is only capable of displaying post columns.
 * <p>
 * Its primary difference from the "Gradebook Sheet" report is the ability to print for multiple
 * sections at a time. The Gradebook Sheet report cannot do so because the columns it displays are
 * taken from the current ScoreGrid instance, which may be specific to a single section.
 *
 * @author X2 Development Corporation
 */
public class GradeInputSheetsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Input parameters
    private static final String COLUMNS_TO_INCLUDE = "columns";
    private static final String COLUMNS_SOURCE = "columnsSource";
    private static final String GRADE_TERM_OID_PARAM = "gradeTermOid";
    private static final String GRADE_TYPE_PARAM = "gradeType";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String SORT_PARAM = "sort";

    // Report parameters
    private static final String COLUMN_HEADER_PARAM = "masterHeaderLookup";
    private static final String COLUMN_LOOKUP_PARAM = "masterColumnLookup";
    private static final String GRADE_TERM_PARAM = "gradeTerm";
    private static final String GRID_PARAM = "grid";

    private int m_averageDecimals;
    private HashMap m_userSpecifiedColumns;
    private HashMap m_gradeInputInitializers;
    private GradeTerm m_gradeTerm;
    private HashMap m_gradeTermLookup;
    private int m_gradeType;
    private ReportDataGrid m_grid;
    private HashMap m_masterColumnLookup;
    private HashMap m_masterHeaderLookup;
    private SisStaff m_staff;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_grid = new ReportDataGrid(5000, 30);
        m_masterColumnLookup = new HashMap(300);
        m_masterHeaderLookup = new HashMap(300);

        String gradeTermOid = (String) getParameter(GRADE_TERM_OID_PARAM);
        m_gradeTerm = (GradeTerm) getBroker().getBeanByOid(GradeTerm.class, gradeTermOid);

        m_gradeType = ((Integer) getParameter(GRADE_TYPE_PARAM)).intValue();

        /*
         * The GradeInputInitializer object is used to determine the columns collected for a given
         * grade term. These are the columns that appear on the report. Since a
         * GradeInputInitializer can be associated with a single Schedule object only, we store
         * instances in a HashMap keyed on schedule OID. This allows us to instantiate only one
         * GradeInputInitializer per schedule processed.
         */
        m_gradeInputInitializers = new HashMap(5);

        initializeGradeTermLookup();

        QueryByCriteria query = createQueryByCriteria(MasterSchedule.class, buildCriteria());
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        QueryIterator masterIterator = getBroker().getIteratorByQuery(query);
        try {
            while (masterIterator.hasNext()) {
                MasterSchedule section = (MasterSchedule) masterIterator.next();
                addSection(section);
            }
        } finally {
            masterIterator.close();
        }

        // Add report parameters
        addParameter(GRADE_TERM_PARAM, m_gradeTerm);
        addParameter(COLUMN_HEADER_PARAM, m_masterHeaderLookup);
        addParameter(COLUMN_LOOKUP_PARAM, m_masterColumnLookup);
        addParameter(GRID_PARAM, m_grid);

        m_grid.beforeTop();

        return m_grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_averageDecimals = 0;
        String decimalsPreference = userData.getPreferenceValue(SisPreferenceConstants.GRADES_AVERAGE_DECIMALS);
        if (!StringUtils.isEmpty(decimalsPreference) && StringUtils.isNumeric(decimalsPreference)) {
            m_averageDecimals = Integer.parseInt(decimalsPreference);
        }

        m_staff = (SisStaff) userData.getStaff();
    }

    /**
     * Adds the passed section to the main data grid.
     *
     * @param section MasterSchedule
     */
    private void addSection(MasterSchedule section) {
        Collection<TranscriptColumnDefinition> columnsForSection = getColumnsForSection(section);

        if (!CollectionUtils.isEmpty(columnsForSection)) {
            Collection<SectionReportingStandard> standards = section.getSectionReportingStandards(getBroker());

            /*
             * Get a ScoreGrid for the section to add and append it to the overall ReportDataGrid
             *
             * Always construct the grid using CALCULATION_MODE_WEIGHT_ALL and false for
             * calculateFromStandards since this report doesn't include any averages.
             */
            ScoreGrid scoreGrid =
                    new ScoreGrid(section, m_averageDecimals, null, AverageCalculator.CALCULATION_MODE_WEIGHT_ALL,
                            false, standards, getBroker());

            HashMap<String, String> columnMap = new HashMap<String, String>(32);
            HashMap<String, String> headerMap = new HashMap<String, String>(32);

            /*
             * Need to check the columns in the score grid because those have the rubrics broken
             * down by criteria.
             */
            int columnNumber = 0;
            Collection<String> validColumnOids = new LinkedList<String>();
            Collection<String> invalidColumnOids = new LinkedList<String>();
            Collection<GradebookColumnDefinition> columns = scoreGrid.getColumnDefinitions();

            for (GradebookColumnDefinition column : columns) {
                if (column.getColumnTypeCode() != GradebookColumnDefinition.COLUMN_TYPE_COMMENT &&
                        column.getColumnTypeCode() != TranscriptColumnDefinition.COLUMN_TYPE_COMMENT &&
                        !column.isHidden()) {
                    if (columnsForSection.contains(column.getTranscriptColumnDefinition())) {
                        columnMap.put(Integer.toString(columnNumber),
                                ScoreGrid.COLUMN_CODE_PRETEXT + column.getColumnCode());
                        headerMap.put(Integer.toString(columnNumber++), column.getColumnCode());

                        validColumnOids.add(column.getOid());
                    } else if (column.getRubricCriterion() != null) {
                        String parentOid = column.getRubricParentColumnOid();

                        if (isValidParentOid(parentOid, validColumnOids, invalidColumnOids, columnsForSection)) {
                            columnMap.put(Integer.toString(columnNumber), parentOid + "." +
                                    column.getRubricCriterion().getOid());
                            headerMap.put(Integer.toString(columnNumber++), column.getColumnCode());
                        }
                    }
                }
            }

            // Only include this section if it has collected columns
            if (columnMap != null && !columnMap.isEmpty()) {
                m_masterColumnLookup.put(section, columnMap);
                m_masterHeaderLookup.put(section, headerMap);

                m_grid.append(scoreGrid);
            }
        }
    }

    /**
     * Returns if the passed rubric parent column OID is valid for the current selection of grade
     * term and grade type.
     *
     * @param oid String
     * @param validOids Collection<String>
     * @param invalidOids Collection<String>
     * @param columnsForSection Collection<TranscriptColumnDefinition>
     * @return boolean
     */
    private boolean isValidParentOid(String oid,
                                     Collection<String> validOids,
                                     Collection<String> invalidOids,
                                     Collection<TranscriptColumnDefinition> columnsForSection) {
        boolean isValid = false;

        if (validOids.contains(oid)) {
            isValid = true;
        } else if (invalidOids.contains(oid)) {
            isValid = false;
        } else {
            GradebookColumnDefinition column =
                    (GradebookColumnDefinition) getBroker().getBeanByOid(GradebookColumnDefinition.class, oid);

            isValid = columnsForSection.contains(column.getTranscriptColumnDefinition());

            /*
             * Update the collections to keep the number of getBeanByOid calls down.
             */
            if (isValid) {
                validOids.add(oid);
            } else {
                invalidOids.add(oid);
            }
        }

        return isValid;
    }

    /**
     * Build the criteria based on user input.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);

        if (!(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY).equals(queryBy)) {
            criteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
            criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
            criteria.addGreaterThan(MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));

            if (m_staff != null) {
                criteria.addEqualTo(MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER +
                        ScheduleTeacher.COL_STAFF_OID, m_staff.getOid());
            }
        }

        if (StringUtils.isInteger(queryBy)) {
            switch (Integer.parseInt(queryBy)) {
                case 1: // Staff name
                    criteria.addBeginsWithIgnoreCase(MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER +
                            SisStaff.COL_NAME_VIEW, queryString);
                    break;

                case 2: // Course view
                    criteria.addBeginsWithIgnoreCase(MasterSchedule.COL_COURSE_VIEW, queryString);
                    break;

                default: // All
                    break;
            }
        } else {
            addUserCriteria(criteria, queryBy, queryString, null, null);
        }

        return criteria;
    }

    /**
     * Returns the columns to display on the report for the passed section. If columns for the
     * selected grade term and grade type are being displayed, a grade input initializer is used
     * to get the columns. Otherwise the list of columns specified on the report input dialog
     * are used.
     *
     * @param section MasterSchedule
     * @return Collection<TranscriptColumnDefinition>
     */
    private Collection<TranscriptColumnDefinition> getColumnsForSection(MasterSchedule section) {
        Collection<TranscriptColumnDefinition> columnsForSection;
        Integer columnsSource = (Integer) getParameter(COLUMNS_SOURCE);
        if (columnsSource.intValue() == 0) {
            /*
             * Get the GradeInputInitializer object used to find the columns active for the report
             * grade term. If one doesn't exist in the map, create a new one and save it in the map
             * for later use.
             */
            GradeInputInitializer initializer =
                    (GradeInputInitializer) m_gradeInputInitializers.get(section.getScheduleOid());
            if (initializer == null) {
                initializer = new GradeInputInitializer(getBroker(), section.getSchedule(),
                        m_gradeTerm);
                m_gradeInputInitializers.put(section.getScheduleOid(), initializer);
            }

            /*
             * Get the columns to display on the grade sheet for this course. The columns included
             * are those of the specified type (progress or term), collected in the specified grade
             * term, and belonging to the course's transcript definition.
             */

            boolean includeTermGrades =
                    m_gradeType == TranscriptColumnDefinition.GRADE_TYPE_TERM;
            boolean includeProgressGrades =
                    m_gradeType == TranscriptColumnDefinition.GRADE_TYPE_PROGRESS;

            columnsForSection =
                    initializer.getColumnsForTerm(section, includeTermGrades, includeProgressGrades);
        } else {
            columnsForSection = getUserSpecifiedColumns(section);
        }

        return columnsForSection;
    }

    /**
     * Returns an ordered Collection of TranscriptColumnDefinition objects corresponding as
     * specified in the COLUMNS_TO_INCLUDE report input parameter.
     *
     * @param section MasterSchedule
     * @return Collection of TranscriptColumnDefinition objects
     */
    private Collection getUserSpecifiedColumns(MasterSchedule section) {
        if (m_userSpecifiedColumns == null) {
            m_userSpecifiedColumns = new HashMap();
        }

        /*
         * Check to see if the columns for the passed section's transcript definition has already
         * been loaded. If not, load them and place them in the map,
         */
        List columnsForSection = (List) m_userSpecifiedColumns.get(
                section.getSchoolCourse().getTranscriptDefinitionOid());

        if (columnsForSection == null) {
            String specifiedColumns = (String) getParameter(COLUMNS_TO_INCLUDE);

            // must be final to be used in the anonymous inner class below
            final List columnList =
                    StringUtils.convertDelimitedStringToList(specifiedColumns, ',', true);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID,
                    section.getSchoolCourse().getTranscriptDefinitionOid());
            criteria.addIn(TranscriptColumnDefinition.COL_GRADE_COLUMN_HEADER, columnList);

            QueryByCriteria columnQuery =
                    new QueryByCriteria(TranscriptColumnDefinition.class, criteria);

            columnsForSection = new ArrayList(getBroker().getCollectionByQuery(columnQuery));

            // Sort the collection based on each column's position in the column list
            Comparator sortComparator = new Comparator() {
                @Override
                public int compare(Object object1, Object object2) {
                    TranscriptColumnDefinition column1 = (TranscriptColumnDefinition) object1;
                    TranscriptColumnDefinition column2 = (TranscriptColumnDefinition) object2;

                    Integer position1 = Integer.valueOf(columnList.indexOf(column1.getGradeColumnHeader()));
                    Integer position2 = Integer.valueOf(columnList.indexOf(column2.getGradeColumnHeader()));

                    return position1.compareTo(position2);
                }
            };

            Collections.sort(columnsForSection, sortComparator);

            m_userSpecifiedColumns.put(section.getSchoolCourse().getTranscriptDefinitionOid(), columnsForSection);
        }

        return columnsForSection;
    }

    /**
     * Initializes the grade term lookup map (m_gradeTermLookup). This HashMap is loaded once at the
     * beginning of the report procedure. It serves as a quick lookup for a GradeTerm bean by
     * GradeTermDefinition OID and grade term number. The HashMap is keyed on GradeTermDefinition
     * OID. The value is a HashMap keyed on grade term number (Integer).
     */
    private void initializeGradeTermLookup() {
        m_gradeTermLookup = new HashMap(5);

        QueryByCriteria gradeTermQuery = new QueryByCriteria(GradeTerm.class);

        QueryIterator gradeTerms = getBroker().getIteratorByQuery(gradeTermQuery);
        try {
            while (gradeTerms.hasNext()) {
                GradeTerm gradeTerm = (GradeTerm) gradeTerms.next();
                HashMap numberMap =
                        (HashMap) m_gradeTermLookup.get(gradeTerm.getGradeTermDefinitionOid());
                if (numberMap == null) {
                    numberMap = new HashMap(10);
                }

                numberMap.put(Integer.valueOf(gradeTerm.getGradeTermNum()), gradeTerm);
                m_gradeTermLookup.put(gradeTerm.getGradeTermDefinitionOid(), numberMap);
            }
        } finally {
            gradeTerms.close();
        }
    }
}
