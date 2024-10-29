/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Attendance Homeroom Input" report. This report lists students grouped
 * by homeroom.
 *
 * @author X2 Development Corporation
 */
public class AttendanceHomeroomInputData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the homeroom-to-staff report parameter. The value is a Map of String objects to
     * Staff beans.
     */
    public static final String HOMEROOM_TO_STAFF_MAP = "homeroomToStaffMap";

    /**
     * Name for the start date report parameter. The value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private ApplicationContext m_context;
    private String m_staffOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Reset the start date to Monday and set it as a report parameter.
         */
        PlainDate startDate = DateUtils.getStartOfWeek((PlainDate) getParameter(START_DATE_PARAM));
        addParameter(START_DATE_PARAM, startDate);

        /*
         * Add the homeroom-to-teacher map as a report parameter.
         */
        X2Criteria staffCriteria = new X2Criteria();

        /*
         * In the staff view, the report needs to be scoped to only the current staff
         */
        if (m_staffOid != null && m_context != null && m_context.equals(ApplicationContext.STAFF)) {
            staffCriteria.addEqualTo(X2BaseBean.COL_OID, m_staffOid);
        }

        int queryBy = getParameter(QUERY_BY_PARAM) == null ? 0 : ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // Teacher name
                staffCriteria.addContainsIgnoreCase(SisStaff.COL_NAME_VIEW, getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // Homeroom
                staffCriteria.addEqualTo(SisStaff.COL_HOMEROOM, getParameter(QUERY_STRING_PARAM));
                staffCriteria.addOrEqualTo(SisStaff.COL_HOMEROOM2, getParameter(QUERY_STRING_PARAM));
                break;

            case 3: // Current Selection
                staffCriteria = getCurrentCriteria();
                break;

            default: // All
                break;
        }

        Map homeroomToStaff = ReportUtils.buildHomeroomToStaffMap(getBroker(), staffCriteria,
                getOrganization(), isSchoolContext() ? getSchool() : null);
        addParameter(HOMEROOM_TO_STAFF_MAP, homeroomToStaff);

        StudentContextReportHelper schoolYearHelper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        /*
         * Query all the active students who have non-empty homeroom. Filter by school if necessary.
         */
        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(schoolYearHelper.getSchoolOidField(), getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
        }

        criteria.addAndCriteria(schoolYearHelper.getActiveStudentCriteria());
        criteria.addNotEmpty(schoolYearHelper.getHomeroomField(), getBroker().getPersistenceKey());

        if (!homeroomToStaff.isEmpty()) {
            criteria.addIn(schoolYearHelper.getHomeroomField(), homeroomToStaff.keySet());
        } else {
            addNoMatchCriteria(criteria);
        }

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderByAscending(schoolYearHelper.getSchoolOidField());
        query.addOrderByAscending(schoolYearHelper.getHomeroomField());
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

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
        m_context =
                userData.getSessionNavConfig() == null ? null : userData.getSessionNavConfig().getApplicationContext();
        m_staffOid = userData.getStaffOid();
    }
}
