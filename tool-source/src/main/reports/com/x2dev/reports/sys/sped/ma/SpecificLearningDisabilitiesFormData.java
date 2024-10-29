/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ma;

import static com.x2dev.sis.model.business.sped.MassachusettsAliases.SLD_OBSERVER_OID;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Specific Learning Disabilities form. This class provides access to the
 * following information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>Support for a subreport that displays the name and role of each IEP team member. The data
 * source for this subreport is a <code>ReportDataGrid</code> containing <code>IepTeamMember</code>
 * objects
 * <li>A parameter containing the observer <code>Staff</code> object
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class SpecificLearningDisabilitiesFormData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final long serialVersionUID = 1L;

    public static final String FIELD_TEAM_MEMBER = "teamMember";
    public static final String PARAM_OBSERVER = "observer";
    public static final String TEAM_SUBREPORT_FORMAT_ID = "SYS-SPED-MA-SLD-T";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.reports.sys.shared.SimpleFormData#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        if (!isBlank()) {
            String observerOid = (String) getFormStorage().getFieldValueByAlias(SLD_OBSERVER_OID, getDictionary());

            if (!StringUtils.isEmpty(observerOid)) {
                SisStaff observer = (SisStaff) getBroker().getBeanByOid(SisStaff.class, observerOid);
                addParameter(PARAM_OBSERVER, observer);
            }
            Report teamSubreport = ReportUtils.getReport(TEAM_SUBREPORT_FORMAT_ID, getBroker());

            addParameter(ReportConstants.FIELD_FORMAT, teamSubreport.getCompiledFormat());
            addParameter(ReportConstants.FIELD_DATA_SOURCE, getTeamMembers());

            return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        }
        return gatherEmptyReportData();

    }

    /**
     * Returns 'JREmptyDataSource' object accordnig to report's engine version
     *
     * @return
     */
    Object gatherEmptyReportData() {
        String engineVersion = ((Report) getJob().getTool()).getEngineVersion();
        if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
            return new net.sf.jasperreports.engine.JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
            return new net.sf.jasperreports3.engine.JREmptyDataSource();
        }
        return new net.sf.jasperreports5.engine.JREmptyDataSource();
    }

    /**
     * Returns a grid containing a row for each team member. The returned grid is used as the
     * data source to the team subreport.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid getTeamMembers() {
        ReportDataGrid teamGrid = new ReportDataGrid();

        if (isBlank()) {
            IepTeamMember blankTeamMember = new IepTeamMember(getBroker().getPersistenceKey());

            for (int i = 0; i < 10; i++) {
                teamGrid.append();
                teamGrid.set(FIELD_TEAM_MEMBER, blankTeamMember);
            }
        } else {
            IepData iep = (IepData) getFormInstance().getOwnerObject(getBroker());

            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, iep.getOid());

            QueryByCriteria query = new QueryByCriteria(IepTeamMember.class, criteria);
            query.addOrderByAscending(IepTeamMember.COL_NAME_VIEW);

            QueryIterator teamMembers = getBroker().getIteratorByQuery(query);
            try {
                while (teamMembers.hasNext()) {
                    teamGrid.append();
                    teamGrid.set(FIELD_TEAM_MEMBER, teamMembers.next());
                }
            } finally {
                teamMembers.close();
            }
        }

        teamGrid.beforeTop();

        return teamGrid;
    }
}
