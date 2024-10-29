/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Health Screening Record report.
 *
 * @author X2 Development Corporation
 */
public class HealthScreeningRecordData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Last date in the report date range. The value is a PlainDate.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "query by" input parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" input parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" input parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * First date in the report date range. The value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();
        criteria.addLessOrEqualThan(HealthScreening.COL_DATE, endDate);
        criteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, startDate);

        if (m_currentStudent != null) {
            criteria.addEqualTo(HealthScreening.COL_STUDENT_OID, m_currentStudent.getOid());
        } else {
            // Get records for current school only (if applicable).
            if (isSchoolContext()) {
                criteria.addEqualTo(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                        getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(HealthScreening.class));
            }

            addUserCriteria(criteria,
                    (String) getParameter(QUERY_BY_PARAM),
                    (String) getParameter(QUERY_STRING_PARAM),
                    SisStudent.class,
                    HealthScreening.COL_STUDENT_OID);
        }

        /*
         * Build and sort the query based on user input
         */
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);

        query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_NAME);
        query.addOrderByAscending(HealthScreening.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Add report parameters
         */
        addParameter(START_DATE_PARAM, startDate);
        addParameter(END_DATE_PARAM, endDate);

        /*
         * Execute query and return.
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
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }
}
