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
package com.x2dev.procedures.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamResult;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Exam Results List Export".
 *
 * @author X2 Development Corporation
 */
public class ExamResultsListExportData extends ExportJavaSource {
    /*
     * Input parameters
     */
    private static final String INCLUDE_UCI_PARAM = "includeUci";
    private static final String SEASON_OID_PARAM = "seasonOid";
    private static final String SERIES_OID_PARAM = "seriesOid";
    private static final String SORT_PARAM = "sort";
    private static final String SPLIT_NAME_PARAM = "splitName";
    private static final String RESULTS_TYPE_PARAM = "examType";
    private static final String GRADES_FIELD_PARAM = "gradesField";

    /*
     * Grid columns (listed in positional order)
     */
    private static final String COL_STUDENT_ID = "Student ID";
    private static final String COL_UCI = "UCI";
    private static final String COL_STUDENT_NAME = "Name";
    private static final String COL_FIRST_NAME = "First Name";
    private static final String COL_LAST_NAME = "Last Name";
    private static final String COL_STUDENT_YOG = "Year";
    private static final String COL_YEAR_GROUP = "Year group";
    private static final String COL_SUBJECT_CODE = "Subject Code";
    private static final String COL_SUBJECT_TITLE = "Subject Title";
    private static final String COL_RESULT = "Result";
    private static final String COL_CERTIFICATE = "Cert";
    private static final String COL_EXAM_BOARD = "Exam Board";

    // UCI Alias
    private static final String UCI_ALIAS = "DFE UCI";

    // Private variables
    private Map<String, String> m_awardingBodies;
    private List<String> m_columns;
    private boolean m_includeUci;
    private boolean m_splitName;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_columns.size());

        Criteria criteria = new Criteria();
        criteria.addNotNull(ExamResult.COL_STUDENT_OID);

        /*
         * If series was selected on input definition, filter exam results by series, else, filter
         * by selected season.
         */
        String seriesOid = (String) getParameter(SERIES_OID_PARAM);
        if (!StringUtils.isEmpty(seriesOid)) {
            criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_SERIES_OID, seriesOid);
        } else {
            String seasonOid = (String) getParameter(SEASON_OID_PARAM);
            criteria.addIn(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                    ExamSeries.COL_SEASON_OID, StringUtils.convertDelimitedStringToList(seasonOid, ',', true));
        }

        String type = (String) getParameter(RESULTS_TYPE_PARAM);
        if (!type.startsWith(SELECTION_SPECIAL_CASE_PREFIX)) {
            criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_EXAM_TYPE_ITEM_CODE, type);
        }

        QueryByCriteria query = new QueryByCriteria(ExamResult.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        /*
         * Within a student, sort results by option result code.
         */
        query.addOrderByAscending(ExamResult.COL_OPTION_RESULT_CODE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExamResult result = (ExamResult) iterator.next();

                addResultToGrid(grid, result);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_includeUci = ((Boolean) getParameter(INCLUDE_UCI_PARAM)).booleanValue();
        m_splitName = ((Boolean) getParameter(SPLIT_NAME_PARAM)).booleanValue();

        initializeColumns();

        m_awardingBodies = populateAwardingBodyMap();
    }

    /**
     * Adds the necessary values from the given bean to a new record in the grid.
     *
     * @param grid DataGrid
     * @param result ExamResult
     */
    private void addResultToGrid(DataGrid grid, ExamResult result) {
        grid.append();

        grid.set(COL_STUDENT_ID, result.getStudent().getLocalId());

        if (m_includeUci) {
            grid.set(COL_UCI, result.getStudent().getFieldValueByAlias(UCI_ALIAS));
        }

        if (m_splitName) {
            grid.set(COL_FIRST_NAME, result.getStudent().getPerson().getFirstName());
            grid.set(COL_LAST_NAME, result.getStudent().getPerson().getLastName());
        } else {
            grid.set(COL_STUDENT_NAME, result.getStudent().getNameView());
        }

        grid.set(COL_STUDENT_YOG, String.valueOf(result.getStudent().getYog()));
        grid.set(COL_YEAR_GROUP, result.getStudent().getGradeLevel());

        ExamOption option = result.getOption();
        grid.set(COL_SUBJECT_CODE, result.getOptionResultCode());
        grid.set(COL_SUBJECT_TITLE, result.getOption().getTitle());

        String grades = result.getGrades();
        String gradeField = (String) getParameter(GRADES_FIELD_PARAM);
        if ("1".equals(gradeField)) {
            grades = result.getFirstGrade();
        }
        grid.set(COL_RESULT, grades);

        grid.set(COL_CERTIFICATE, option.getExamTypeQualificationCert());

        String awardingBody = "";
        if (m_awardingBodies.get(option.getAwardingBody()) != null) {
            awardingBody = m_awardingBodies.get(option.getAwardingBody());
        }
        grid.set(COL_EXAM_BOARD, awardingBody);
    }

    /**
     * Adds the 8 elements to the column names list.
     */
    private void initializeColumns() {
        m_columns = new ArrayList<String>(11);
        m_columns.add(COL_STUDENT_ID);

        if (m_includeUci) {
            m_columns.add(COL_UCI);
        }

        if (m_splitName) {
            m_columns.add(COL_FIRST_NAME);
            m_columns.add(COL_LAST_NAME);
        } else {
            m_columns.add(COL_STUDENT_NAME);
        }

        m_columns.add(COL_STUDENT_YOG);
        m_columns.add(COL_YEAR_GROUP);
        m_columns.add(COL_SUBJECT_CODE);
        m_columns.add(COL_SUBJECT_TITLE);
        m_columns.add(COL_RESULT);
        m_columns.add(COL_CERTIFICATE);
        m_columns.add(COL_EXAM_BOARD);
    }

    /**
     * Returns a map of awarding body descriptions keyed on codes.
     * <p>
     * Descriptions are parsed out of their codes, if present within parenthesis.
     *
     * @return Map&lt;String, String&gt;
     */
    private Map<String, String> populateAwardingBodyMap() {
        Map<String, String> awardingBodyDescriptions = new HashMap<String, String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField awardingBodyField =
                dictionary.findDataDictionaryField(ExamOption.class.getName(), ExamOption.COL_AWARDING_BODY);

        if (awardingBodyField != null && awardingBodyField.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, awardingBodyField.getReferenceTableOid());
            criteria.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.FALSE);
            criteria.addNotEmpty(ReferenceCode.COL_CODE, getBroker().getPersistenceKey());
            criteria.addNotEmpty(ReferenceCode.COL_DESCRIPTION, getBroker().getPersistenceKey());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ReferenceCode refCode = (ReferenceCode) iterator.next();

                    String description = refCode.getDescription();
                    if (description.contains("(")) {
                        description = description.substring(0, description.indexOf('('));
                    }

                    awardingBodyDescriptions.put(refCode.getCode(), description);
                }
            } finally {
                iterator.close();
            }
        }

        return awardingBodyDescriptions;
    }
}
