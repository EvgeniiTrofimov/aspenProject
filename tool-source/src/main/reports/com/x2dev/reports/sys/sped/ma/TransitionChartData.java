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

import static com.x2dev.sis.model.business.sped.MassachusettsAliases.TRANSITION_CHART_COMPLETED_BY;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStaff;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the Transition Chart. This class provides access to the
 * following information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>A parameter containing the completed by <code>Staff</code> object
 * </ul>
 * 
 * @author X2 Development Corporation
 */
public class TransitionChartData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String PARAM_COMPLETED_BY = "completedBy";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.reports.sys.shared.SimpleFormData#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        if (!isBlank()) {
            IepData iep = (IepData) getFormStorage();

            String completedByOid = (String) iep.getFieldValueByAlias(TRANSITION_CHART_COMPLETED_BY, getDictionary());
            if (completedByOid != null) {
                SisStaff staff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, completedByOid);
                addParameter(PARAM_COMPLETED_BY, staff);
            }
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }
}
