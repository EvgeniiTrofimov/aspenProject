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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Enrollment Activity" report which shows all enrollment activity in the
 * district within a date range.
 *
 * @author X2 Development Corporation
 */
public class EnrollmentActivityData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Report parameter name for the start of the date range. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);

        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

        /*
         * If the report is being run from the school context, include a subquery that returns
         * ALL enrollment records for students that have at least one enrollment record for
         * the school within the date window. We do not simply apply a filter on school here because
         * we want all records involved in transfers to appear. (Only one enrollment record
         * corresponding to a transfer to or from a given school contains that school's OID.)
         */
        if (isSchoolContext()) {
            Criteria subQueryCriteria = new Criteria();
            subQueryCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            subQueryCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
            subQueryCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());

            SubQuery subQuery =
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, subQueryCriteria);

            criteria.addIn(StudentEnrollment.COL_STUDENT_OID, subQuery);
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentEnrollment.class));
        }

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
