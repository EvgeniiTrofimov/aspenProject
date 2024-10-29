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

import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Conduct Action List report. This report lists conduct actions that meet
 * the following criteria:
 * <ul>
 * <li>Occur within a specified date range
 * <li>Assigned to a particular YOG (optional)
 * <li>Have a particular code (optional)
 * <li>Are open (optional)
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class ConductActionListData extends SecondaryStudentDataSource {
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the comma-delimited list of action codes. This value is a String.
     */
    public static final String CODES_PARAM = "codes";

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the "open only" indicator. This value is a Boolean.
     */
    public static final String OPEN_ONLY_PARAM = "openOnly";

    /**
     * Name for the "query by" report parameter. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" report parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. This value is an Integer.
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
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, getParameter(START_DATE_PARAM));
        criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, getParameter(END_DATE_PARAM));

        String codes = (String) getParameter(CODES_PARAM);
        if (!StringUtils.isEmpty(codes)) {
            criteria.addIn(ConductAction.COL_ACTION_CODE, StringUtils.convertDelimitedStringToList(codes, ','));
        }

        Boolean openOnly = (Boolean) getParameter(OPEN_ONLY_PARAM);
        if (openOnly.booleanValue()) {
            criteria.addEqualTo(ConductAction.COL_CLOSED_INDICATOR, Boolean.FALSE);
        }

        if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                criteria.addAndCriteria(getStudentObjectCriteria(ConductAction.COL_STUDENT_OID,
                        ConductAction.REL_STUDENT,
                        ConductAction.COL_SCHOOL_OID));
            } else {
                criteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
            }
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(ConductAction.class));
        }

        addUserCriteria(criteria,
                (String) getParameter(QUERY_BY_PARAM),
                (String) getParameter(QUERY_STRING_PARAM),
                null,
                null);

        /*
         * Build the sort order based on user input.
         *
         * The sort always begins with the school name in case we're running the report for multiple
         * schools.
         *
         * The sort always ends with incident fields to group actions appropriately.
         */
        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        applyUserSort(query, ((String) getParameter(SORT_PARAM)));

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
