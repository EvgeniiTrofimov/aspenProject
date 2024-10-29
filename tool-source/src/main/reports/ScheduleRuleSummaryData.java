/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ScheduleRule;
import com.x2dev.sis.model.beans.ScheduleRuleDefinition;
import com.x2dev.sis.web.SisUserDataContainer;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Schedule Rule Summary" report.
 *
 * @author X2 Development Corporation
 */
public class ScheduleRuleSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String RULE_DEFINITION_OID = "ruleDefinitionOid";

    private ScheduleRuleDefinition m_ruleDefinition;
    private String m_scheduleOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        if (m_ruleDefinition != null) {
            criteria.addEqualTo(ScheduleRule.COL_SCHEDULE_RULE_DEFINITION_OID, m_ruleDefinition.getOid());
            criteria.addEqualTo(ScheduleRule.COL_SCHEDULE_OID, m_scheduleOid);
        } else {
            criteria.addEqualTo(ScheduleRule.COL_SCHEDULE_OID, m_scheduleOid);
            String ruleOid = (String) getParameter(RULE_DEFINITION_OID);
            if (!StringUtils.isEmpty(ruleOid)) {
                criteria.addEqualTo(ScheduleRule.COL_SCHEDULE_RULE_DEFINITION_OID, ruleOid);
            }
        }

        QueryByCriteria query = new QueryByCriteria(ScheduleRule.class, criteria);
        query.addOrderByAscending(ScheduleRule.COL_SUMMARY_VIEW);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
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
        m_ruleDefinition = userData.getCurrentRecord(ScheduleRuleDefinition.class);
        m_scheduleOid = ((SisUserDataContainer) userData).getBuildScheduleOid();
    }
}
