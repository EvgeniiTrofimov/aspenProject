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
import com.x2dev.sis.model.beans.SchedulePattern;
import com.x2dev.sis.model.beans.SchedulePatternMember;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Pattern List" report.
 *
 * @author X2 Development Corporation
 */
public class SchedulePatternListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String PATTERN_SET_OID_PARAM = "patternSetOid";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchedulePatternMember.COL_PATTERN_SET_OID, getParameter(PATTERN_SET_OID_PARAM));

        QueryByCriteria query = new QueryByCriteria(SchedulePatternMember.class, criteria);

        query.addOrderByAscending(SchedulePatternMember.REL_PATTERN + "." + SchedulePattern.COL_SCHEDULE_PATTERN_TYPE);
        query.addOrderByAscending(SchedulePatternMember.REL_PATTERN + "." + SchedulePattern.COL_DAYS);
        query.addOrderByAscending(SchedulePatternMember.REL_PATTERN + "." + SchedulePattern.COL_PERIODS);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
