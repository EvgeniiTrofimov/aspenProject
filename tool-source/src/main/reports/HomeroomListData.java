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

import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Homeroom List report. This report prints a the list of students for
 * one or more homerooms. Homeroom is always the primary sort order of this report.
 *
 * @author X2 Development Corporation
 */
public class HomeroomListData extends ReportJavaSourceNet {
    /*
     * TODO: This report is a perfect candidate for the next generation of reports: data source
     * sharing. This data source is nearly identical to the StudentListData class. The only
     * difference is here we want to limit the choices for some input parameters. Also, here we need
     * a map of homerooms to teacher names (but that could be moved to StudentListData as an
     * optional report paramter).
     *
     * In short, this class should go away once formats are de-coupled from data sources.
     */

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the homeroom-to-staff report parameter. The value is a Map of String objects to
     * Staff beans.
     */
    public static final String HOMEROOM_TO_STAFF_MAP = "homeroomToStaffMap";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            /*
             * Include secondary students of the school if needed.
             */
            if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                criteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
            }
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        }

        addUserCriteria(criteria,
                (String) getParameter(QUERY_BY_PARAM),
                (String) getParameter(QUERY_STRING_PARAM),
                null,
                null);

        /*
         * Build and sort the query
         */
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Add the homeroom-to-staff map as a report parameter.
         */
        Map<String, Staff> homeroomToStaff = ReportUtils.buildHomeroomToStaffMap(getBroker(),
                getOrganization(), isSchoolContext() ? getSchool() : null);
        addParameter(HOMEROOM_TO_STAFF_MAP, homeroomToStaff);

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
