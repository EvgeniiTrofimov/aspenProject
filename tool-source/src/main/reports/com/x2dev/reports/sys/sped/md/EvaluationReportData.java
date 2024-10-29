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
package com.x2dev.reports.sys.sped.md;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import java.util.Arrays;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Evaluation Report Form, a Wicomico County/Maryland special education form.
 * This report provides a <code>SimpleFormDataSource</code> as well as support for subreports that
 * display IEP team members and disabilities.
 *
 * @author X2 Development Corporation
 */
public class EvaluationReportData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String COL_FORM = "form";
    public static final String COL_STUDENT = "student";
    public static final String COL_RESPONSE = "response";

    public static final String DISABILITIES_FORMAT_ID = "SYS-SPED-MD-EVAL-D";
    public static final String TEAM_FORMAT_ID = "SYS-SPED-MD-EVAL-T";

    public static final String PARAM_TEAM_FORMAT = "teamFormat";
    public static final String PARAM_TEAM_DATASOURCE = "teamDataSource";
    public static final String PARAM_DISABILITIES_FORMAT = "disabilitiesFormat";
    public static final String PARAM_DISABILITIES_DATASOURCE = "disabilitiesDataSource";

    private static final String FIELD_TEAM_MEMBER = "teamMember";

    private static final String FIELD_DISABILITY_CODE = "disabilityCode";
    private static final String FIELD_PRIMARY = "primary";

    private Map<String, Report> m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadSubReports();

        addParameter(PARAM_TEAM_FORMAT, getSubreportFormat(TEAM_FORMAT_ID));
        addParameter(PARAM_TEAM_DATASOURCE, getTeamMembers());

        addParameter(PARAM_DISABILITIES_FORMAT, getSubreportFormat(DISABILITIES_FORMAT_ID));
        addParameter(PARAM_DISABILITIES_DATASOURCE, getDisabilities());

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Returns a grid containing a row for each disability. The returned grid is used as the data
     * source to the disabilities subreport.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid getDisabilities() {
        ReportDataGrid disabilityGrid = new ReportDataGrid();

        GenericFormData formStorage = (GenericFormData) getFormStorage();

        for (GenericFormChildData child : formStorage.getGenericFormDataChildren(getBroker())) {
            String disability = (String) child.getFieldValueByAlias("eval-rpt-disability", getDictionary());
            Boolean primary =
                    Boolean.valueOf((String) child.getFieldValueByAlias("eval-rpt-disability-primary", getDictionary()));

            disabilityGrid.append();
            disabilityGrid.set(FIELD_DISABILITY_CODE, disability);
            disabilityGrid.set(FIELD_PRIMARY, primary);
        }

        disabilityGrid.beforeTop();

        return disabilityGrid;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param formatId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String formatId) {
        Report report = m_subReports.get(formatId);
        return report.getCompiledFormat();
    }

    /**
     * Returns a grid containing a row for each team member. The returned grid is used as the
     * data source to the team subreport. If a blank form is being run, a fixed number of blank
     * team member objects are added.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid getTeamMembers() {
        ReportDataGrid teamGrid = new ReportDataGrid();

        IepData iep = (IepData) getFormOwner();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, iep.getOid());

        QueryByCriteria query = new QueryByCriteria(IepTeamMember.class, criteria);
        query.addOrderByAscending(IepTeamMember.COL_NAME_VIEW);

        if (isBlank()) {
            int blankTeamMembers = 15;

            for (int i = 0; i < blankTeamMembers; i++) {
                teamGrid.append();
                teamGrid.set(FIELD_TEAM_MEMBER, new IepTeamMember(getBroker().getPersistenceKey()));
            }
        } else {
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

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {DISABILITIES_FORMAT_ID,
                TEAM_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 2);
    }
}
