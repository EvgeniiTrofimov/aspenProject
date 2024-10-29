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
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.TeacherSection;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Teacher Schedule (Sheet)" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherScheduleSheetData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "schedule sort" report parameter. The value is an String.
     */
    public static final String SCHEDULE_SORT_PARAM = "scheduleSort";

    /**
     * Name for the "teacher sort" report parameter. The value is an String.
     */
    public static final String TEACHER_SORT_PARAM = "teacherSort";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(TeacherSection.REL_SECTION + "." + Section.COL_SCHEDULE_OID,
                m_reportHelper.getScheduleOid());

        if (!StringUtils.isEmpty(m_reportHelper.getStaffOid())) {
            criteria.addEqualTo(TeacherSection.COL_STAFF_OID, m_reportHelper.getStaffOid());
            /*
             * Set the school associated with the teacher
             */
            addParameter(SCHOOLNAME_KEY, getSchool().getName());
            addParameter(SCHOOL_KEY, getSchool());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(criteria, queryBy, queryString, SisStaff.class, TeacherSection.COL_STAFF_OID);
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getTeacherSectionClass(), criteria);

        /*
         * Build the sort based on user input.
         */
        String sortBy = (String) getParameter(TEACHER_SORT_PARAM);
        applyUserSort(query, sortBy);

        /*
         * The teacher sort order always has to end with the staff OID so that schedules are grouped
         * properly.
         */
        query.addOrderByAscending(TeacherSection.COL_STAFF_OID);

        /*
         * Within each teacher we can order schedules by either course view or schedule view.
         */
        String sortSchedulesBy = (String) getParameter(SCHEDULE_SORT_PARAM);
        applyUserSort(query, sortSchedulesBy);

        /*
         * Execute the query
         */
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
        m_reportHelper = new ScheduleReportHelper(userData);
    }
}
