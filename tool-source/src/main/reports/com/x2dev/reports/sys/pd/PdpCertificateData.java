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

package com.x2dev.reports.sys.pd;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.PdActivitySection;
import com.x2dev.sis.model.beans.StaffPdActivity;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the PDP Certificate report. This report returns a list of all staff that are
 * taking classes in the selection of Pd Activity Sections.
 *
 * @author X2 Development Corporation
 */
public class PdpCertificateData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    private PdActivitySection m_section;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        if (m_section != null) {
            criteria.addEqualTo(StaffPdActivity.COL_PD_SECTION_OID, m_section.getOid());
        } else {
            addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null,
                    StaffPdActivity.class, PdActivitySection.class, StaffPdActivity.COL_PD_SECTION_OID);
        }

        QueryByCriteria query = new QueryByCriteria(StaffPdActivity.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()), true, getLocale());
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
        m_section = userData.getCurrentRecord(PdActivitySection.class);
    }
}
