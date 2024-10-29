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

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class ReportOfProgress3454U extends BeanReport {
    private static final String ALIAS_IGP_GOAL = "igp-goal";
    private static final String ALIAS_IGP_GOAL_NUMBER = "igp-goal-number";
    private static final String ALIAS_IEP_OPTION2_DATE = "iep-option-2-date";



    private static final String PARAM_DATE_OF_MEETING = "meetingDate";
    private static final String PARAM_DOB = "DOB";
    private static final String PARAM_IEP_PARENT_CONFERENCE = "iep-parent-conference";
    private static final String PARAM_GOAL_PROGRESS_REPORT = "goal-progress-report";
    private static final String PARAM_GOAL_REPORT_CARD = "goal-report-card";
    private static final String PARAM_IGP_PROGRESS_REPORT_Q = "igp-progress-report-q";
    private static final String PARAM_IGP_REPORT_CARD_Q = "igp-report-card-q";
    private static final String PARAM_OWNER = "owner";
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private ReportDataGrid m_grid;
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();

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
            addParameter(PARAM_DATE_OF_MEETING, m_ilSpedHelper.formatDate(storageTable.getMeetingDate()));
            addParameter(PARAM_DOB, m_ilSpedHelper.formatDate(storageTable.getStudent().getPerson().getDob()));
            addParameter(PARAM_OWNER, storageTable);
            addParameter(PARAM_IGP_REPORT_CARD_Q, storageTable.getFieldValueByAlias(PARAM_IGP_REPORT_CARD_Q,
                    getDictionary()));
            addParameter(PARAM_IGP_PROGRESS_REPORT_Q, storageTable.getFieldValueByAlias(PARAM_IGP_PROGRESS_REPORT_Q,
                    getDictionary()));
            addParameter(PARAM_IEP_PARENT_CONFERENCE,
                    storageTable.getFieldValueByAlias(PARAM_IEP_PARENT_CONFERENCE, getDictionary()));

            addParameter(PARAM_GOAL_REPORT_CARD,
                    storageTable.getFieldValueByAlias(PARAM_GOAL_REPORT_CARD, getDictionary()));
            addParameter(PARAM_GOAL_PROGRESS_REPORT, storageTable.getFieldValueByAlias(PARAM_GOAL_PROGRESS_REPORT,
                    getDictionary()));
            addParameter(ALIAS_IEP_OPTION2_DATE, m_ilSpedHelper
                    .formatDate(storageTable.getFieldValueByAlias(ALIAS_IEP_OPTION2_DATE, getDictionary())));

            Collection<IepGoal> goalCollection = storageTable.getIepGoals();
            for (IepGoal goal : goalCollection) {
                m_grid.append();
                fillGoal(goal);
                /*
                 * InputStream image = new
                 * ByteArrayInputStream(storageTable.getStudent().getPerson().getPrimaryPhoto().
                 * getPhoto());
                 * m_grid.set(ALIAS_IGP_IMAGE, image);
                 */
            }
        } else {
            for (int i = 0; i < 3; i++) {
                m_grid.append();
            }
        }
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * fill current row on m_grid using data from IepGoal.
     *
     * @param goal IepGoal
     */
    private void fillGoal(IepGoal goal) {
        m_grid.set(ALIAS_IGP_GOAL_NUMBER, goal.getId());
        m_grid.set(ALIAS_IGP_GOAL, goal.getGoal());
    }


}
