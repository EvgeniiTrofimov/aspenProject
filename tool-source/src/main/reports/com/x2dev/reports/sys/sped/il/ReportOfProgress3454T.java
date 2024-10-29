/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a SYS-SPED-IL-3454T form -based report.
 *
 * 
 * @author Follett Software Company
 */
public class ReportOfProgress3454T extends BeanReport {

    private static final String ALIAS_IGP_COMMENTS = "igp-comments";
    private static final String ALIAS_IGP_COMPLETED = "igp-completed";
    private static final String ALIAS_IGP_EXPECTED = "igp-expected";
    private static final String ALIAS_IGP_GOAL = "igp-goal";
    private static final String ALIAS_IGP_GOAL_NUMBER = "igp-goal-number";
    private static final String ALIAS_IGP_NOT_EXPECTED = "igp-not-expected";
    private static final String ALIAS_IGP_PROGRESS_CODE = "igp-progress-code";
    private static final String ALIAS_IEP_OPTION1_DATE = "iep-option-1-date";
    private static final String ALIAS_MARKING_PERIOD = "igp-marking-period";

    private static final String GRIG_FIELD_END_LINE = "endLine";

    private static final String PARAM_DATE_OF_MEETING = "meetingDate";
    private static final String PARAM_DOB = "DOB";
    private static final String PARAM_MARKING_PERIOD = "markingPeriod";
    private static final String PARAM_PARENT_CONFERENCE = "parentConference";

    private static final String PARAM_PROGRESS_PERIOD = "progressPeriod";
    private static final String PARAM_OWNER = "owner";

    private static final String RTB_IEP_PROGRESS_RPORT_PERIOD = "rtbIepRepPer";


    private ReportDataGrid m_grid;
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();

    private Comparator<IepGoalProgress> m_progressComparator = new Comparator<IepGoalProgress>() {


        @Override
        public int compare(IepGoalProgress arg0, IepGoalProgress arg1) {
            String key0 = getKeyForCompare(arg0);
            String key1 = getKeyForCompare(arg1);
            return key0.compareTo(key1);
        }

        private String getKeyForCompare(IepGoalProgress progress) {

            String reportPeriod = progress.getReportPeriod();
            // max length it is ESY (3) we need make the same length for other codes because
            // different length can be reason wrong sorting.
            // we need sort by group goal.getId() + reportPeriod + markingPeriod and inside it is
            // group by progress oid
            // example (Key consists of three parts: 3 length +4 length + 3 length)

            // abc + def + 123 = abcdef123 (with space abcdef 123);
            // abc + def + 567 = abcdef567 (with space abcdef 567);
            // ab + cdef + 234 = abcdef234 (with space ab cdef234);
            // 3 Variant will between 1 and 2 but it's wrong. 1 and 2 it is one group, 3 it is
            // second group.
            reportPeriod = makeMinLength(reportPeriod, ' ', 3);
            String markingPeriod = (String) progress.getFieldValueByAlias(ALIAS_MARKING_PERIOD, getDictionary());
            markingPeriod = makeMinLength(markingPeriod, ' ', 3);
            IepGoal goal = progress.getIepGoal();
            String goalId = goal.getId();
            goalId = makeMinLength(goalId, ' ', 10);
            return markingPeriod + reportPeriod + goalId + progress.getOid();

        }

        /**
         * in <code>targetString</code> make minimal Length like <code>minLength</code><br>
         * for realize it logic add <code>addCharToEnd</code> to the end of
         * <code>targetString</code> if
         * <code>targetString</code> length less then <code>minLength</code>
         * 
         * @param targetString
         * @param addCharToEnd
         * @param minLength
         * @return
         */
        private String makeMinLength(String targetString, char addCharToEnd, int minLength) {
            targetString = (targetString == null ? IlSpedHelper.EMPTY : targetString);
            while (targetString.length() < minLength) {
                targetString = targetString + addCharToEnd;

            }
            return targetString;
        }

    };

    private Map<String, ReferenceCode> m_progressReportPeriod = null;

    private List<String> m_reportPeriods = new ArrayList<String>();
    Map<String, List<String>> m_uniquieGoalInGroup = new HashMap<String, List<String>>();
    private static final long serialVersionUID = 1L;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());


        IepData storageTable = (IepData) getFormStorage();
        m_grid = new ReportDataGrid();

        if (storageTable.getOid() != null) {
            loadProgressReportPeriodMap();
            loadReportPeriods();

            addParameter(PARAM_DATE_OF_MEETING, m_ilSpedHelper.formatDate(storageTable.getMeetingDate()));
            addParameter(PARAM_DOB, m_ilSpedHelper.formatDate(storageTable.getStudent().getPerson().getDob()));
            addParameter(PARAM_OWNER, storageTable);

            addParameter(PARAM_PARENT_CONFERENCE, getParameter(PARAM_PARENT_CONFERENCE));


            addParameter(ALIAS_IEP_OPTION1_DATE, m_ilSpedHelper
                    .formatDate(storageTable.getFieldValueByAlias(ALIAS_IEP_OPTION1_DATE, getDictionary())));

            Collection<IepGoal> goalCollection = storageTable.getIepGoals();
            fillGoals(goalCollection);

        } else {
            m_grid.append();

        }
        m_grid.beforeTop();
        // m_grid.sort(new ArrayList<String>(Arrays.asList(PARAM_MARKING_PERIOD,
        // PARAM_PROGRESS_PERIOD)), false);
        return m_grid;
    }

    /**
     * return map for reference table oid
     * key is ReferenceCode oid value ReferenceCode.
     *
     * @param rtbOid String
     * @return Map
     */
    private Map<String, ReferenceCode> createRefTableCodesMap(String rtbOid) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Map<String, ReferenceCode> returnMap = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 10);

        return returnMap;
    }

    /**
     * fill grid rows in next order: marking period, report period, goal.
     *
     * @param goals Collection<IepGoal>
     */
    private void fillGoals(Collection<IepGoal> goals) {

        TreeSet<IepGoalProgress> goalProgresses = new TreeSet(m_progressComparator);
        for (IepGoal goal : goals) {
            goalProgresses.addAll(getFilteredProgresses(goal));
        }
        for (IepGoalProgress goalProgress : goalProgresses) {
            String reprotPeriod = goalProgress.getReportPeriod();
            String goalID = goalProgress.getIepGoal().getId();
            String markingPeriod = (String) goalProgress.getFieldValueByAlias(ALIAS_MARKING_PERIOD, getDictionary());
            String lastOid = getLastOid(goalProgresses, reprotPeriod, markingPeriod, goalID);
            m_grid.append();
            if (isFirstGoal(goalProgress)) {
                m_grid.set(ALIAS_IGP_GOAL_NUMBER, goalID);
                m_grid.set(ALIAS_IGP_GOAL, goalProgress.getIepGoal().getGoal());

            }
            m_grid.set("oid", goalProgress.getOid());
            m_grid.set(PARAM_PROGRESS_PERIOD, reprotPeriod);
            m_grid.set(PARAM_MARKING_PERIOD, markingPeriod);

            m_grid.set(ALIAS_IGP_COMPLETED, goalProgress.getProgressCode());
            m_grid.set(ALIAS_IGP_EXPECTED, goalProgress.getFieldValueByAlias(ALIAS_IGP_EXPECTED,
                    getDictionary()));
            m_grid.set(ALIAS_IGP_NOT_EXPECTED, goalProgress.getFieldValueByAlias(ALIAS_IGP_NOT_EXPECTED,
                    getDictionary()));
            m_grid.set(ALIAS_IGP_COMMENTS, goalProgress.getFieldValueByAlias(ALIAS_IGP_COMMENTS,
                    getDictionary()));
            m_grid.set(ALIAS_IGP_PROGRESS_CODE, goalProgress.getFieldValueByAlias(ALIAS_IGP_PROGRESS_CODE,
                    getDictionary()));
            if (lastOid != null && goalProgress.getOid().equals(lastOid)) {
                m_grid.set(GRIG_FIELD_END_LINE, Boolean.valueOf(true));

            } else {
                m_grid.set(GRIG_FIELD_END_LINE, Boolean.valueOf(false));
            }

        }

    }

    /**
     * filter IepGoalProgress in IepGoal<br>
     * for filter used user input parameters .
     *
     * @param iepGoal IepGoal
     * @return Collection
     */
    private Collection<IepGoalProgress> getFilteredProgresses(IepGoal iepGoal) {

        Collection<IepGoalProgress> progresses = new ArrayList<IepGoalProgress>();
        for (IepGoalProgress progress : iepGoal.getIepGoalProgress()) {
            String reportPeriod = progress.getReportPeriod();
            String markingPeriod = (String) progress.getFieldValueByAlias(ALIAS_MARKING_PERIOD, getDictionary());
            if (!StringUtils.isEmpty(markingPeriod) && isInPeriod(reportPeriod)) {
                progresses.add(progress);
            }
        }

        return progresses;
    }

    /**
     * return key for IepGoalProgress.
     *
     * @param goalProgress IepGoalProgress
     * @return String
     */
    private String getGroupKey(IepGoalProgress goalProgress) {
        String reportPeriod = goalProgress.getReportPeriod();
        String markingPeriod = (String) goalProgress.getFieldValueByAlias(ALIAS_MARKING_PERIOD, getDictionary());

        return markingPeriod + reportPeriod;
    }

    /**
     * return last oid from IepGoalProgress where <br>
     * getReportPeriod like input values from this method<br>
     * progressPeriod - getReportPeriod<br>
     * .
     *
     * @param goalProgresses Collection<IepGoalProgress>
     * @param progressPeriod String
     * @param markingPeriod String
     * @param goalId String
     * @return String
     */
    private String getLastOid(Collection<IepGoalProgress> goalProgresses,
                              String progressPeriod,
                              String markingPeriod,
                              String goalId) {
        String lastOid = null;

        for (IepGoalProgress progress : goalProgresses) {
            if (goalId.equals(progress.getIepGoal().getId())) {
                String pPeriod = progress.getReportPeriod();
                String mPeriod = (String) progress.getFieldValueByAlias(ALIAS_MARKING_PERIOD, getDictionary());

                if (pPeriod.equals(progressPeriod) && mPeriod.equals(markingPeriod)) {
                    lastOid = progress.getOid();
                }
            }

        }

        return lastOid;
    }

    /**
     * return true if key for this IepGoalProgress met in first time<br>
     * true - mean that it is first IepGoalProgress with this key.
     *
     * @param goalProgress IepGoalProgress
     * @return true, if is first goal
     * @see ReportOfProgress3454T#getGroupKey(IepGoalProgress)
     */
    private boolean isFirstGoal(IepGoalProgress goalProgress) {
        boolean isFirst = false;
        String goalOid = goalProgress.getIepGoalOid();

        List<String> groups = m_uniquieGoalInGroup.get(goalOid);
        if (groups == null) {
            groups = new ArrayList<String>();
            m_uniquieGoalInGroup.put(goalOid, groups);
        }

        String groupKey = getGroupKey(goalProgress);
        if (!groups.contains(groupKey)) {
            groups.add(groupKey);
            isFirst = true;
        }

        return isFirst;
    }


    /**
     * match reportPeriod parameter with user input parameter<br>
     * if user chose the same value - return true.
     *
     * @param reportPeriod String
     * @return true, if is in period
     */
    private boolean isInPeriod(String reportPeriod) {
        boolean isInPeriod = !StringUtils.isEmpty(reportPeriod) && m_reportPeriods.contains(reportPeriod);
        return isInPeriod;

    }

    /**
     * load Reference code map for "IEP Progress Reporting Periods" reference table.
     *
     * @see ReportOfProgress3454T#createRefTableCodesMap(String)
     */
    private void loadProgressReportPeriodMap() {
        m_progressReportPeriod = createRefTableCodesMap(RTB_IEP_PROGRESS_RPORT_PERIOD);
    }


    /**
     * load user data for "reprotPeriod" input parameter into m_reportPeriods List.
     */
    private void loadReportPeriods() {
        String prPerid = (String) getParameter(PARAM_PROGRESS_PERIOD);
        if (!StringUtils.isEmpty(prPerid)) {
            List<String> refCodeOids = new ArrayList<String>(Arrays.asList(prPerid.split(IlSpedHelper.COMMA)));
            for (String refCodeOid : refCodeOids) {
                ReferenceCode refCode = m_progressReportPeriod.get(refCodeOid);
                m_reportPeriods.add(refCode.getCode());
            }

        }

    }



}
