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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamResult;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Exam Result Broadsheet Export".
 *
 * @author X2 Development Corporation
 */
public class ExamResultsBroadsheetExportData extends ExportJavaSource {
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

    // UCI Alias
    private static final String UCI_ALIAS = "DFE UCI";

    private static final int INITIAL_MAP_SIZE = 1000;

    // Private variables
    private List<String> m_columns;
    private X2Criteria m_criteria;
    private boolean m_includeUci;
    private String m_seasonOid;
    private String m_seriesOid;
    private boolean m_splitName;
    private Map<String, SisStudent> m_students;
    private List<String> m_subjectHeaders;

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

        QueryByCriteria query = new QueryByCriteria(ExamResult.class, m_criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        Map<String, Collection<ExamResult>> resultsByStudent = getGroupedCollectionByQuery(query,
                new String[] {ExamResult.COL_STUDENT_OID},
                new int[] {INITIAL_MAP_SIZE});
        if (resultsByStudent != null) {
            for (String studentOid : resultsByStudent.keySet()) {
                SisStudent student = m_students.get(studentOid);
                Collection<ExamResult> results = resultsByStudent.get(studentOid);

                addStudentToGrid(grid, student, results);
            }
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
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
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
        m_seasonOid = (String) getParameter(SEASON_OID_PARAM);
        m_seriesOid = (String) getParameter(SERIES_OID_PARAM);
        m_splitName = ((Boolean) getParameter(SPLIT_NAME_PARAM)).booleanValue();

        buildCriteria();

        initializeSubjectHeaders();

        initializeColumns();

        loadStudents();
    }

    /**
     * Adds a student's results to the grid.
     *
     * @param grid DataGrid
     * @param student SisStudent
     * @param results Collection<ExamResult>
     */
    private void addStudentToGrid(DataGrid grid, SisStudent student, Collection<ExamResult> results) {
        grid.append();

        grid.set(COL_STUDENT_ID, student.getLocalId());

        if (m_includeUci) {
            grid.set(COL_UCI, student.getFieldValueByAlias(UCI_ALIAS));
        }

        if (m_splitName) {
            grid.set(COL_FIRST_NAME, student.getPerson().getFirstName());
            grid.set(COL_LAST_NAME, student.getPerson().getLastName());
        } else {
            grid.set(COL_STUDENT_NAME, student.getNameView());
        }

        grid.set(COL_STUDENT_YOG, String.valueOf(student.getYog()));
        grid.set(COL_YEAR_GROUP, student.getGradeLevel());

        for (ExamResult result : results) {
            String optionResultCode = result.getOptionResultCode();
            String optionTitle = result.getOption().getTitle();
            String subjectHeader = buildSubjectHeader(optionResultCode, optionTitle);

            String grades = result.getGrades();
            String gradeField = (String) getParameter(GRADES_FIELD_PARAM);
            if ("1".equals(gradeField)) {
                grades = result.getFirstGrade();
            }
            grid.set(subjectHeader, grades);
        }
    }

    /**
     * Builds criteria based on user input.
     */
    private void buildCriteria() {
        m_criteria = new X2Criteria();
        m_criteria.addNotEmpty(ExamResult.COL_STUDENT_OID, getBroker().getPersistenceKey());

        if (!StringUtils.isEmpty(m_seriesOid)) {
            m_criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_SERIES_OID, m_seriesOid);
        } else {
            m_criteria.addIn(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                    ExamSeries.COL_SEASON_OID, StringUtils.convertDelimitedStringToList(m_seasonOid, ',', true));
        }

        String type = (String) getParameter(RESULTS_TYPE_PARAM);
        if (!type.startsWith(SELECTION_SPECIAL_CASE_PREFIX)) {
            m_criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_EXAM_TYPE_ITEM_CODE, type);
        }
    }

    /**
     * Returns a concatenation of optionResultCode and optionTitle.
     *
     * @param optionResultCode String
     * @param optionTitle String
     * @return String
     */
    private String buildSubjectHeader(String optionResultCode, String optionTitle) {
        StringBuilder key = new StringBuilder();

        if (!StringUtils.isEmpty(optionResultCode)) {
            key.append(optionResultCode);
        }

        if (!StringUtils.isEmpty(optionTitle)) {
            if (!StringUtils.isEmpty(key.toString())) {
                key.append(" - ");
            }

            key.append(optionTitle);
        }

        return key.toString();
    }

    /**
     * Executes the passed query and loads the results into nested Maps based on the passed key
     * columns. The beans contained in the inner Map's value are beans of the passed query's root
     * data class.
     * <p>
     * If the query yields no results, an empty Map is returned.
     * <p>
     * Custom method. Overrides <code>BeanManager.getGroupedCollectionByQuery(..)</b> by
     * retaining the order in which keys are added to the map.
     *
     * @param query Query
     * @param columnKeys The columns containing the values on which the Maps will
     *        be keyed
     * @param mapSizes The sizes to initialize the Maps to; this value should be
     *        relatively close to the number of distinct values for
     *        each column in the query results
     * @return Map Map depth is equal to length of columnKeys
     */
    private Map getGroupedCollectionByQuery(Query query, String[] columnKeys, int[] mapSizes) {
        Map<Object, Object> map = new LinkedHashMap<Object, Object>(mapSizes[0]);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                X2BaseBean bean = (X2BaseBean) iterator.next();

                Map<Object, Object> innerMap = map;
                Map<Object, Object> outerMap = map;

                // Access/build nested maps
                for (int columnIndex = 0; columnIndex < columnKeys.length - 1; columnIndex++) {
                    Object key = bean.getFieldValueByBeanPath(columnKeys[columnIndex]);
                    innerMap = (LinkedHashMap<Object, Object>) outerMap.get(key);

                    if (innerMap == null) {
                        innerMap = new LinkedHashMap<Object, Object>(mapSizes[columnIndex]);
                        outerMap.put(key, innerMap);
                    }

                    outerMap = innerMap;
                }

                // Add bean to innermost map's list
                Object listKey = bean.getFieldValueByBeanPath(columnKeys[columnKeys.length - 1]);
                List<X2BaseBean> list = (List<X2BaseBean>) outerMap.get(listKey);

                if (list == null) {
                    list = new LinkedList<X2BaseBean>();
                    innerMap.put(listKey, list);
                }

                list.add(bean);
            }
        } finally {
            iterator.close();
        }

        return map;
    }

    /**
     * Adds the elements to the column names list.
     */
    private void initializeColumns() {
        m_columns = new ArrayList<String>();
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

        for (String subjectHeader : m_subjectHeaders) {
            m_columns.add(subjectHeader);
        }
    }

    /**
     * Builds a list of subject headers (option result code + option title) which will be added to
     * the column list.
     */
    private void initializeSubjectHeaders() {
        m_subjectHeaders = new ArrayList<String>();

        QueryByCriteria query = new QueryByCriteria(ExamResult.class, m_criteria);
        query.addOrderByAscending(ExamResult.COL_OPTION_RESULT_CODE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExamResult result = (ExamResult) iterator.next();

                String optionResultCode = result.getOptionResultCode();
                String optionTitle = result.getOption().getTitle();
                String key = buildSubjectHeader(optionResultCode, optionTitle);

                if (!m_subjectHeaders.contains(key)) {
                    m_subjectHeaders.add(key);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a map of student beans keyed on student OID.
     */
    private void loadStudents() {
        Criteria studentCriteria = new Criteria();

        SubQuery studentSub = new SubQuery(ExamResult.class, ExamResult.COL_STUDENT_OID, m_criteria);
        studentCriteria.addIn(X2BaseBean.COL_OID, studentSub);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);

        m_students = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, INITIAL_MAP_SIZE);
    }
}
