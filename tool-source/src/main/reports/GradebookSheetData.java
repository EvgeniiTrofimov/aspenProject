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

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookColumnType;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.web.gradebook.GradeInputUtils;
import com.x2dev.sis.web.gradebook.LimitedColumnScoreGrid;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Gradebook Sheet" report. This report must be run from a grade input page
 * (i.e., a page where the current node is a LimitedColumnScoreGrid instance). It can be run for
 * only one section at a time. It displays the columns that currently appear on the grade input
 * page.
 * <p>
 * Because the current grade input page may display more columns than can fit on a single page,
 * additional pages are added if necessary to display all columns. To accomplish this, the gradebook
 * sheet format is included as a subreport. The main data source is a grid, each row of which
 * represents a "page" of the report.
 *
 * @author X2 Development Corporation
 */
public class GradebookSheetData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Report parameters
    private static final String ALTERNATE_DISPLAY_PARAM = "alternateDisplay";
    private static final String HIDE_NAME_PARAM = "hideStudentNames";
    private static final String ORIENTATION_PARAM = "orientation";

    private static final String COL_COLUMN_HEADER_MAP = "columnHeaderMap";
    private static final String COL_COLUMN_MAP = "columnMap";
    private static final String COL_DATASOURCE = "datasource";
    private static final String COL_FORMAT = "format";
    private static final String COL_STANDARDS_MODE = "standardsMode";

    // private static final String LANDSCAPE_ID = "L";
    private static final String REPORT_ID_PREFIX = "SYS-GBK-001-";
    private static final String SUBREPORT_ID_PREFIX = "SYS-GBK-001-SUB-";
    private static final String PORTRAIT_ID = "P";

    // Grid field used for sorting
    private static final String SORT_FIELD = "sortField";

    private static final int MAX_PORTRAIT_COLUMNS = 8;
    private static final int MAX_LANDSCAPE_COLUMNS = 13;

    private int m_alternateDisplay;
    private List<GradebookColumnDefinition> m_columns;
    private LimitedColumnScoreGrid m_grid = null;
    private boolean m_showPointsInHeader;

    /**
     * Additional logic added.
     * Now also resorts grid by NameView after any job is finished.
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        m_grid.beforeTop();
        while (m_grid.next()) {
            SisStudent student = m_grid.getStudent();
            m_grid.set("sortField", student.getNameView());
        }

        m_grid.beforeTop();
        m_grid.sort("sortField", true);
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        boolean hideNames = ((Boolean) getParameter(HIDE_NAME_PARAM)).booleanValue();
        if (hideNames) {
            resortGrid();
        }

        ReportDataGrid pages = new ReportDataGrid();
        byte[] subreportFormat = getSubreportFormat();
        int columnCount = m_columns.size();
        int maxColumnsPerPage =
                PORTRAIT_ID.equals(getParameter(ORIENTATION_PARAM)) ? MAX_PORTRAIT_COLUMNS : MAX_LANDSCAPE_COLUMNS;
        int pageCount = (int) Math.ceil((double) columnCount / maxColumnsPerPage);

        setFormatId(REPORT_ID_PREFIX + getParameter(ORIENTATION_PARAM));

        boolean hasStandardsScore = hasStandardsScore();

        for (int i = 0; i < pageCount; i++) {
            HashMap columnMap = new HashMap(columnCount);
            HashMap columnHeaderMap = new HashMap(columnCount);

            int columnNumber = 0;
            int startColumn = i * maxColumnsPerPage;
            int endColumn = Math.min(startColumn + maxColumnsPerPage, columnCount);

            for (int j = startColumn; j < endColumn; j++) {
                GradebookColumnDefinition column = m_columns.get(j);

                String columnId = null;
                String columnHeader = null;
                if (column.getColumnTypeCode() == GradebookColumnDefinition.COLUMN_TYPE_IMPLICIT_AVERAGE) {
                    columnId = column.getImplicitAverageKey();
                    columnHeader = GradeInputUtils.getAverageLabel(columnId,
                            m_grid.getSchoolContext(), true, false, getBroker(), getLocale());

                    if (columnId.endsWith(AverageCalculator.PORTAL_AVERAGE_CODE_SUFFIX)) {
                        String suffix = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(getLocale(), "label.gradebook.portalView");
                        columnHeader += " " + suffix;
                    }
                } else if (column.getRubricCriterion() != null) {
                    columnId = column.getRubricParentColumnOid() + "." + column.getRubricCriterion().getOid();
                    columnHeader = column.getColumnCode();
                } else if (!(column.isPostColumn()
                        && column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_COMMENT)
                        && column.getColumnTypeCode() != GradebookColumnDefinition.COLUMN_TYPE_MISSING) {
                    if (!hasStandardsScore) {
                        columnId = column.getColumnCode();
                        columnHeader = column.getColumnCode();

                        if (m_showPointsInHeader && column.getTotalPoints() != null) {
                            columnHeader += "\n" + column.getTotalPoints().toString() + " pts";
                        }
                    } else {
                        columnId = column.getOid() + "_" + m_grid.getSelectedStandardOid();
                        columnHeader = column.getColumnCode();

                        GradebookColumnType type = column.getColumnType();
                        if (type != null) {
                            columnHeader += "\n" + type.getColumnType();
                        }
                    }
                }

                if (columnId != null) {
                    columnMap.put(Integer.toString(columnNumber), columnId);
                    columnHeaderMap.put(Integer.toString(columnNumber), columnHeader);

                    columnNumber++;
                }
            }

            pages.append();
            pages.set(COL_FORMAT, subreportFormat);
            pages.set(COL_DATASOURCE, m_grid);
            pages.set(COL_COLUMN_HEADER_MAP, columnHeaderMap);
            pages.set(COL_COLUMN_MAP, columnMap);
            pages.set(COL_STANDARDS_MODE, hasStandardsScore);
        }

        pages.beforeTop();

        return pages;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_alternateDisplay = getParameter(ALTERNATE_DISPLAY_PARAM) == null ? 0
                : ((Integer) getParameter(ALTERNATE_DISPLAY_PARAM)).intValue();
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
        LimitedColumnScoreGrid currentGrid = (LimitedColumnScoreGrid) userData.getCurrentGrid();

        m_grid = currentGrid;
        m_grid.changeStudentFilter(currentGrid.getStudentFilter(), getBroker());

        m_columns = new LinkedList(currentGrid.getColumnDefinitions());

        m_grid.beforeTop();

        m_showPointsInHeader = Boolean.parseBoolean(userData.getPreferenceValue(
                SisPreferenceConstants.GRADEBOOK_SHOW_POINTS_IN_HEADER));
    }

    /**
     * Returns the compiled subreport format.
     *
     * @return byte[]
     */
    private byte[] getSubreportFormat() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_ID_PREFIX + getParameter(ORIENTATION_PARAM));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report subreport = (Report) getBroker().getBeanByQuery(query);
        return subreport.getCompiledFormat();
    }

    /**
     * Returns true if the report should pull ReportingStandardScore from the cell instead of the
     * GradebookScore for display.
     *
     * @return boolean
     */
    private boolean hasStandardsScore() {
        boolean hasStandardsScore = false;

        // Determine if in standards mode
        boolean hasReportingStandards = m_grid.hasReportingStandards() && m_grid.getCurrentAssignmentOid() != null
                && m_grid.getSectionReportingStandards(m_grid.getCurrentAssignmentOid()) != null;
        boolean standardsMode = m_grid.getView() == LimitedColumnScoreGrid.VIEW_STANDARDS;
        String selectedStandardOid = m_grid.getSelectedStandardOid();

        if (standardsMode && !hasReportingStandards && !m_grid.isPostColumnTerm()
                && !StringUtils.isEmpty(selectedStandardOid)) {
            hasStandardsScore = true;
        }

        return hasStandardsScore;
    }

    /**
     * Sorts grid by the student LASID.
     */
    private void resortGrid() {
        m_grid.beforeTop();
        while (m_grid.next()) {
            SisStudent student = m_grid.getStudent();
            m_grid.set(SORT_FIELD, m_alternateDisplay == 0 ? student.getLocalId() : student.getStateId());
        }

        m_grid.beforeTop();
        m_grid.sort(SORT_FIELD, true);
    }
}
