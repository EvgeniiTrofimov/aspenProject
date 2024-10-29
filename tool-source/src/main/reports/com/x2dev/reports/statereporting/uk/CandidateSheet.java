/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExamEntry;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.sis.model.beans.ExamSectionOption;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.web.exams.ExamEntryGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports3.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Candidate Sheet" report.
 *
 * @author X2 Development Corporation
 */
public class CandidateSheet extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters
     */
    private static final String SUBREPORT_ID_PARAM = "subreportId";

    /*
     * Report parameters
     */
    private static final String AWARDING_BODY_PARAM = "awardingBody";
    private static final String EXAM_ENTRY_PARAM = "examEntry";
    private static final String SEASON_PARAM = "season";
    private static final String SECTION_PARAM = "section";
    private static final String SERIES_PARAM = "series";

    /*
     * Grid fields
     */
    private static final String FIELD_COLUMN_HEADER_MAP = "columnHeaderMap";
    private static final String FIELD_DATA_SOURCE = "datasource";
    private static final String FIELD_FORMAT = "format";
    private static final String FIELD_OPTION_OID_MAP = "optionOidMap";

    /*
     * All filter for series and awarding body.
     */
    private static final String FILTER_ALL = "all";

    private ExamEntryGrid m_examEntryGrid;
    private Map<String, ExamEntry> m_examEntries;
    private Collection<String> m_optionOids;
    private List<ExamSectionOption> m_sectionOptions;
    private Collection<String> m_studentOids;
    private InputStream m_subreportStream;

    /*
     * Maximum columns per page constant
     */
    private static final int MAX_COLUMNS_PER_PAGE = 20;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid pages = new ReportDataGrid(5);

        int columnCount = m_sectionOptions.size();
        int pageCount = (int) Math.ceil((double) columnCount / MAX_COLUMNS_PER_PAGE);

        if (pageCount > 0) {
            loadOptions();
            loadStudents();

            loadExamEntries();
        }

        for (int i = 0; i < pageCount; i++) {
            Map optionOidMap = new HashMap(columnCount);
            Map columnHeaderMap = new HashMap(columnCount);

            int columnNumber = 0;
            int startColumn = i * MAX_COLUMNS_PER_PAGE;
            int endColumn = Math.min(startColumn + MAX_COLUMNS_PER_PAGE, columnCount);

            for (int j = startColumn; j < endColumn; j++) {
                ExamSectionOption sectionOption = m_sectionOptions.get(j);

                ExamOption option = sectionOption.getOption();

                String columnHeader = option.getOptionEntryCode() + " - " + option.getTitle();

                optionOidMap.put(Integer.toString(columnNumber), option.getOid());
                columnHeaderMap.put(Integer.toString(columnNumber), columnHeader);

                columnNumber++;
            }

            pages.append();
            pages.set(FIELD_DATA_SOURCE, m_examEntryGrid);
            pages.set(FIELD_FORMAT, m_subreportStream);
            pages.set(FIELD_COLUMN_HEADER_MAP, columnHeaderMap);
            pages.set(FIELD_OPTION_OID_MAP, optionOidMap);
        }

        addParameters();

        pages.beforeTop();
        return pages;
    }

    /**
     * Initialize.
     *
     * @throws ToolRunException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws ToolRunException {
        String subreportId = (String) getParameter(SUBREPORT_ID_PARAM);

        Report subreport = ReportUtils.getReport(subreportId, getBroker());
        if (subreport == null) {
            String message = "";
            if (StringUtils.isEmpty(subreportId)) {
                message = "Input definition is missing a \"subreportId\" parameter";
            } else {
                message = "Subreport with an ID of " + subreportId + " was not found.";
            }

            throw new ToolRunException(message);
        }

        m_subreportStream = new ByteArrayInputStream(subreport.getCompiledFormat());
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_examEntryGrid = (ExamEntryGrid) userData.getCurrentGrid();

        m_sectionOptions = new LinkedList(m_examEntryGrid.getSectionOptions());
    }

    /**
     * Adds parameters to report.
     */
    private void addParameters() {
        addParameter(AWARDING_BODY_PARAM, m_examEntryGrid.getSelectedAwardingBody());

        addParameter(EXAM_ENTRY_PARAM, m_examEntries);

        addParameter(SECTION_PARAM, m_examEntryGrid.getSection());

        String selectedSeasonOid = m_examEntryGrid.getSelectedSeasonOid();
        ExamSeason season = (ExamSeason) getBroker().getBeanByOid(ExamSeason.class, selectedSeasonOid);
        addParameter(SEASON_PARAM, season);

        String selectedSeriesOid = m_examEntryGrid.getSelectedSeriesOid();
        ExamSeries series = null;
        if (!FILTER_ALL.equals(selectedSeriesOid)) {
            series = (ExamSeries) getBroker().getBeanByOid(ExamSeries.class, selectedSeriesOid);
        }
        addParameter(SERIES_PARAM, series);
    }

    /**
     * Loads a map of ExamEntry beans keyed on a concatenation of student OID and option OID.
     */
    private void loadExamEntries() {
        m_examEntries = new HashMap<String, ExamEntry>();

        Criteria criteria = new Criteria();
        criteria.addIn(ExamEntry.COL_STUDENT_OID, m_studentOids);
        criteria.addIn(ExamEntry.COL_OPTION_OID, m_optionOids);

        QueryByCriteria query = new QueryByCriteria(ExamEntry.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExamEntry entry = (ExamEntry) iterator.next();

                m_examEntries.put(entry.getStudentOid() + "|" + entry.getOptionOid(), entry);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a collection of option OIDs.
     */
    private void loadOptions() {
        m_optionOids = new HashSet<String>((int) (m_sectionOptions.size() * 1.5));

        for (ExamSectionOption sectionOption : m_sectionOptions) {
            m_optionOids.add(sectionOption.getOptionOid());
        }
    }

    /**
     * Loads a collection of student OIDs present in the grid.
     */
    private void loadStudents() {
        m_studentOids = new HashSet<String>((int) (m_examEntryGrid.rowCount() * 1.5));

        m_examEntryGrid.beforeTop();
        while (m_examEntryGrid.next()) {
            SisStudent student = m_examEntryGrid.getStudent();
            m_studentOids.add(student.getOid());
        }

        m_examEntryGrid.beforeTop();
    }
}
