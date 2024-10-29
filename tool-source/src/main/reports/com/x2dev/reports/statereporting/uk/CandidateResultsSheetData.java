/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamResult;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Candidate Results" report for UK schools.
 *
 * @author X2 Development Corporation
 */
public class CandidateResultsSheetData extends ReportJavaSourceNet {
    /*
     * Input parameters
     */
    private static final String ACTIVE_ONLY_PARAM = "activeOnly";
    private static final String CONTEXT_OID_PARAM = "contextOid";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String RESULTS_TYPE_PARAM = "type";
    private static final String SEASON_OID_PARAM = "seasonOid";
    private static final String SORT_PARAM = "sort";

    /*
     * Grid fields
     */
    private static final String FIELD_BOARD = "board";
    private static final String FIELD_EXAM_OPTION = "option";
    private static final String FIELD_EXAM_RESULT = "result";
    private static final String FIELD_STUDENT = "student";

    private Map<String, String> m_awardingBodies;
    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(10);

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                ExamSeries.REL_SEASON + PATH_DELIMITER + ExamSeason.COL_DISTRICT_CONTEXT_OID, contextOid);

        String seasonOid = (String) getParameter(SEASON_OID_PARAM);
        if (!StringUtils.isEmpty(seasonOid)) {
            criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.REL_SERIES + PATH_DELIMITER +
                    ExamSeries.COL_SEASON_OID, seasonOid);
        }

        String type = (String) getParameter(RESULTS_TYPE_PARAM);
        if (!type.startsWith(SELECTION_SPECIAL_CASE_PREFIX)) {
            criteria.addEqualTo(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_EXAM_TYPE_ITEM_CODE, type);
        }

        if (m_currentStudent != null) {
            criteria.addEqualTo(ExamResult.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(ExamResult.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                        getSchool().getOid());
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        SisStudent.COL_ENROLLMENT_STATUS));
            }

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, ExamResult.COL_STUDENT_OID);
        }

        QueryByCriteria query = createQueryByCriteria(ExamResult.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Within a student, sort results based on option entry code
         */
        query.addOrderByAscending(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_EXAM_TYPE_ITEM_CODE);
        query.addOrderByAscending(ExamResult.REL_OPTION + PATH_DELIMITER + ExamOption.COL_OPTION_ENTRY_CODE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExamResult result = (ExamResult) iterator.next();

                ExamOption option = result.getOption();

                grid.append();
                grid.set(FIELD_BOARD, m_awardingBodies.get(option.getAwardingBody()));
                grid.set(FIELD_EXAM_OPTION, option);
                grid.set(FIELD_EXAM_RESULT, result);
                grid.set(FIELD_STUDENT, result.getStudent());
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        loadAwardingBodyMap();
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
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Returns a map of awarding body descriptions keyed on codes.
     * <p>
     * Descriptions are parsed out of their codes, if present within parenthesis.
     */
    private void loadAwardingBodyMap() {
        m_awardingBodies = new HashMap<String, String>();

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

                    m_awardingBodies.put(refCode.getCode(), description);
                }
            } finally {
                iterator.close();
            }
        }
    }
}
