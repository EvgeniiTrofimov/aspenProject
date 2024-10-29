/*
 * ====================================================================
 *
 * \ * X2 Development Corporation
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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the following student reports:
 * <ul>
 * <li>Address Labels
 * <li>Student Directory
 * <li>Student List
 * </ul>
 * These reports simply select students from the current school (with an optional criteria for YOG
 * or homeroom) and order the results by YOG, homeroom, or last name.
 *
 * @author X2 Development Corporation
 */
public class StudentProfileData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
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

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        QueryByCriteria query = null;

        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        if (m_currentStudent != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
            query = new QueryByCriteria(SisStudent.class, criteria);
        } else {
            StudentContextReportHelper helper =
                    new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);

            if (isSchoolContext() && !queryBy.contains(CURRENT_KEY)) {
                criteria.addEqualTo(helper.getSchoolOidField(), getSchool().getOid());

                /*
                 * Include secondary students of the school if needed.
                 */
                if (((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                    criteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
                }
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                criteria.addAndCriteria(helper.getActiveStudentCriteria());

            }

            query = createQueryByCriteria(SisStudent.class, criteria);

            /*
             * Build the sort based on user input (first sort by the school)
             */
            query.addOrderByAscending(helper.getSchoolRelationship() + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(helper.getSchoolOidField());
            applyUserSort(query, (String) getParameter(SORT_PARAM));
        }

        /*
         * Execute the query and return the results
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
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }
}
