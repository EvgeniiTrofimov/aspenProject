/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the IEP progress report. This report is designed to run from 3 different areas,
 * each with differing results:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Area</th>
 * <th>Result</th>
 * </tr>
 * <tr>
 * <td>IEP list</td>
 * <td>Progress records for all IEPs in the selection within the
 * specified date range</td>
 * </tr>
 * <tr>
 * <td>Progress list</td>
 * <td>Progress records for the current IEP within the specified
 * date range</td>
 * </tr>
 * <tr>
 * <td>Progress detail</td>
 * <td>The current progress record</td>
 * </tr>
 * </table>
 * <p>
 * The data source for this report is a <code>ReportDataGrid</code>. For each progress record,
 * a row is added for each text field displayed. This supports a "floating text" effect on the
 * report format.
 *
 * @author X2 Development Corporation
 */
public class GaIepProgressReportData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Optional parameter to filter the report by a specific school year.
     */
    public static final String CONTEXT_OID = "contextOid";

    /**
     * End date in the progress report date window. Only progress reports dated prior to and
     * including this date will be included.
     */
    public static final String END_DATE = "endDate";

    /**
     * boolean in the input window that when checked will include unposted reports in this report
     */
    public static final String INCLUDE_UNPOSTED = "includeUnposted";

    /**
     * Report parameter containing the reporting period to filter on. This parameter is optional.
     */
    public static final String REPORT_PERIOD = "reportPeriod";

    /**
     * Report parameter containing the total number of progress reports included.
     */
    public static final String REPORT_TOTALS = "reportTotals";

    /**
     * Oid of a specific staff member to filter by. Chosen on input screen.
     */
    public static final String STAFF_OID = "staffOid";

    /**
     * Start date in the progress report date window. Only progress reports dated after and
     * including this date will be included.
     */
    public static final String START_DATE = "startDate";

    /**
     * Columns populated for goal and objective data in grid
     */
    private static final String COL_FIELD_ID = "fieldId";
    private static final String COL_PROGRESS_BEAN = "progress";
    private static final String COL_STUDENT = "student";
    private static final String COL_DESCRIPTION = "description";
    private static final String COL_PROGRESS_CODE = "progressCode";
    private static final String COL_PROGRESS_GRID = "progressGrid";
    private static final String COL_IEP_DICTIONARY = "dictionary";

    /**
     * Text prefixes use in report
     */
    private static final String PREFIX_GOAL = "Goal";
    private static final String PREFIX_OBJECTIVE = "Objective";

    /**
     * Alias prefixes used in report
     */
    private static final String PREFIX_OBJECTIVE_PROGRESS = "igp-progress-";

    /**
     * Constants used in report
     */
    private static final String CONSTANT_BLANK = "";
    private static final String CONSTANT_SPACE = " ";
    private static final String CONSTANT_PROGRESS_CODE = "Progress Code";
    private static final String CONSTANT_DESCRIPTION = "Goal or Objective Description";

    /**
     * Reference table oids used in report
     */
    private static final String OID_REF_TABLE_PROGRESS_CODE = "rtbGoalPrgCode";

    /**
     * Parameters used in report
     */
    private static final String PARAM_PROGRESS_GRID_FORMAT = "progressGridFormat";

    /**
     * Subreport Formats
     */
    private static final String FORMAT_PROGRESS_GRID = "SYS-SPED-GA-PR-SUB";

    /**
     * Member variables
     */
    private IepData m_iep = null;
    private IepGoalProgress m_progress = null;
    private DataDictionary m_iepDictionary = null;
    private Map<String, ReferenceCode> m_progressCodeRefCodes = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid progressGrid = new ReportDataGrid();

        Object startDate = getParameter(START_DATE);
        Object endDate = getParameter(END_DATE);

        // get goal progress reference table
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID,
                OID_REF_TABLE_PROGRESS_CODE);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        m_progressCodeRefCodes = getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 32);

        Criteria criteria = new Criteria();

        if (startDate != null) {
            criteria.addGreaterOrEqualThan(IepGoalProgress.COL_DATE, startDate);
        }

        if (endDate != null) {
            criteria.addLessOrEqualThan(IepGoalProgress.COL_DATE, endDate);
        }

        String staffOid = (String) getParameter(STAFF_OID);
        if (!StringUtils.isEmpty(staffOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_STAFF_OID, staffOid);
        }

        Object includeUnposted = getParameter(INCLUDE_UNPOSTED);
        if (includeUnposted != null && !((Boolean) includeUnposted).booleanValue()) {
            criteria.addEqualTo(IepGoalProgress.COL_POSTED_INDICATOR, Boolean.TRUE);
        }

        String postingPeriod = (String) getParameter(REPORT_PERIOD);
        if (!StringUtils.isEmpty(postingPeriod)) {
            criteria.addEqualTo(IepGoalProgress.COL_REPORT_PERIOD, postingPeriod);
        }

        String contextOid = (String) getParameter(CONTEXT_OID);
        if (contextOid != null) {
            criteria.addEqualTo(IepGoalProgress.COL_DISTRICT_CONTEXT_OID, contextOid);
        }

        if (m_progress != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_progress.getOid());
        } else if (m_iep != null) {
            criteria.addEqualTo(IepGoalProgress.COL_IEP_DATA_OID, m_iep.getOid());
        } else {
            criteria.addIn(IepGoalProgress.COL_IEP_DATA_OID,
                    new SubQuery(IepData.class, X2BaseBean.COL_OID, getCurrentCriteria()));
        }

        QueryByCriteria query = new QueryByCriteria(IepGoalProgress.class, criteria);

        if (m_progress == null && m_iep == null) {
            applyCurrentSort(query);
        }

        query.addOrderByAscending(IepGoalProgress.REL_IEP_GOAL + "." + IepGoal.COL_ID);
        query.addOrderByAscending(IepGoalProgress.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepGoalProgress progress = (IepGoalProgress) iterator.next();

                initializeIepDictionary(progress);

                // Create grid used for progress information to grid
                progressGrid.append();
                progressGrid.set(COL_IEP_DICTIONARY, m_iepDictionary);
                progressGrid.set(COL_STUDENT, progress.getStudent());
                progressGrid.set(COL_PROGRESS_BEAN, progress);

                // Create grid used for goal and objective information to grid
                ReportDataGrid goalObjectiveGrid = new ReportDataGrid();

                // Adding header information to grid
                goalObjectiveGrid.append();
                String fieldId = CONSTANT_BLANK;
                String progressCode = CONSTANT_PROGRESS_CODE;
                String description = CONSTANT_DESCRIPTION;

                goalObjectiveGrid.set(COL_FIELD_ID, fieldId);
                goalObjectiveGrid.set(COL_DESCRIPTION, description);
                goalObjectiveGrid.set(COL_PROGRESS_CODE, progressCode);

                // Add goal information to grid
                goalObjectiveGrid.append();
                fieldId = PREFIX_GOAL + CONSTANT_SPACE + progress.getIepGoal().getId();
                progressCode = progress.getProgressCode();
                progressCode = getProgressDescription(progressCode);
                description = progress.getIepGoal().getGoal();

                goalObjectiveGrid.set(COL_FIELD_ID, fieldId);
                goalObjectiveGrid.set(COL_DESCRIPTION, description);
                goalObjectiveGrid.set(COL_PROGRESS_CODE, progressCode);

                // Get + sort objectives
                Collection<IepGoalObjective> objectives = progress.getIepGoal().getIepGoalObjectives();
                objectives = CollectionUtils.sortBeans(objectives, IepGoalObjective.COL_SEQUENCE_NUMBER, true, true);

                // Add objectives information to grid
                int objectiveCount = 0;
                for (IepGoalObjective objective : objectives) {
                    // Add Objective name + number to grid
                    goalObjectiveGrid.append();
                    fieldId = PREFIX_OBJECTIVE + CONSTANT_SPACE + objective.getSequenceNumber();
                    goalObjectiveGrid.set(COL_FIELD_ID, fieldId);

                    // Add progress code to grid
                    String objectiveProgressAias = PREFIX_OBJECTIVE_PROGRESS + objectiveCount;
                    progressCode = (String) progress.getFieldValueByAlias(objectiveProgressAias, m_iepDictionary);
                    goalObjectiveGrid.set(COL_PROGRESS_CODE, getProgressDescription(progressCode));
                    goalObjectiveGrid.set(COL_DESCRIPTION, objective.getObjective());

                    objectiveCount++;
                }

                goalObjectiveGrid.beforeTop();

                progressGrid.set(COL_PROGRESS_GRID, goalObjectiveGrid);
            }
        } finally {
            iterator.close();
        }
        progressGrid.beforeTop();
        addSubReport();
        return progressGrid;
    }

    /**
     * Returns progressCode state code if there is one, otherwise returns original progressCode.
     *
     * @param progressCode String
     * @return progressCode
     */
    private String getProgressDescription(String progressCode) {
        if (m_progressCodeRefCodes.containsKey(progressCode)) {
            ReferenceCode refCode = m_progressCodeRefCodes.get(progressCode);
            progressCode = refCode.getDescription();
        }
        return progressCode;
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
        if (!userData.getCurrentList().getDataClass().equals(IepData.class)) {
            m_iep = userData.getCurrentRecord(IepData.class);
        }

        m_progress = userData.getCurrentRecord(IepGoalProgress.class);
    }

    /**
     * Adds the subreport used by the report.
     */
    private void addSubReport() {
        Report report = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, FORMAT_PROGRESS_GRID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        report = (Report) getBroker().getBeanByQuery(query);

        addParameter(PARAM_PROGRESS_GRID_FORMAT, new ByteArrayInputStream(report.getCompiledFormat()));
    }

    /**
     * Initializes m_iepDictionary, if it has not already been initialized.
     *
     * @param progress IepGoalProgress
     */
    private void initializeIepDictionary(IepGoalProgress progress) {
        if (m_iepDictionary == null) {
            ExtendedDictionaryAttributes iepExtendedDataDictionary = progress.getIepData().getExtendedDataDictionary();
            m_iepDictionary = DataDictionary.getDistrictDictionary(iepExtendedDataDictionary,
                    getOrganization().getPersistenceKey());
        }
    }
}
