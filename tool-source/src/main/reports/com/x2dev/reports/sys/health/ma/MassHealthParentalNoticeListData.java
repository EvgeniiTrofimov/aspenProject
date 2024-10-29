/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2002-2018 Follett School Solutions.
 * All rights reserved.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.ma;

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.ArrayList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Prepares data for the Mass Health Parental Notice report.
 * This report works for a list of students, reporting one student per page.
 *
 * @author X2 Development Corporation
 */
public class MassHealthParentalNoticeListData extends MassHealthParentalNoticeData {

    /**
     * member variables
     */
    private List<SisStudent> m_currentStudents;

    /**
     * Gather data.
     *
     * @return JRDataSource JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = null;

        if (m_currentStudents != null && m_currentStudents.size() > 0) {
            grid = new ReportDataGrid();
            for (SisStudent student : m_currentStudents) {
                prepareMassHealthNotice(grid, student);
            }
            grid.beforeTop();
        } else {
            return super.gatherData();
        }
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        ContextList list = userData.getCurrentList();
        if (list.getDataClass().isAssignableFrom(SisStudent.class)) {
            m_currentStudents = new ArrayList(userData.getCurrentList().getAllRecords());
        }
    }
}
