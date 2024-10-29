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
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.UnsupportedDatabaseException;
import com.follett.fsc.core.framework.persistence.adjusters.OrderByAdjuster;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;

/**
 * Prepares the data for the following student reports:
 *
 * These reports simply select students from the current school (with an optional criteria for YOG
 * or homeroom) and order the results by YOG, homeroom, or last name.
 *
 * @author X2 Development Corporation
 */
public class BirthdayListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Report parameter name for the birthdays to include. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection date" report parameter. The value is a PlainDate.
     */
    public static final String QUERY_DATE_PARAM = "queryDate";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * We must note the platform because the date-range queries are specific to the underlying RDBMS
     *
     * MySQL: DAYOFMONTH(PSN_DOB), MONTH(PSN_DOB)
     *
     * SQL Server: DATEPART(d, PSN_DOB), DATEPART(m, PSN_DOB)
     *
     */

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

        StringBuilder sql = new StringBuilder(512);

        Calendar calendar = null;
        PlainDate reportDate = (PlainDate) getParameter(QUERY_DATE_PARAM);
        if (reportDate != null) {
            calendar = Calendar.getInstance();
            calendar.setTime((PlainDate) getParameter(QUERY_DATE_PARAM));
        }

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // Current selection
                criteria = getCurrentCriteria();
                break;

            case 1: // All
                // Nothing to add, just use the school and active filters as appropriate.
                break;

            case 2: // Today
                sql.append(getDateQuery(calendar));
                break;

            case 3: // This week
                sql.append(getWeekQuery(calendar));
                break;

            case 4: // This month
                sql.append(getMonthQuery(calendar));
                break;
        }

        criteria.addNotNull(SisStudent.REL_PERSON + "." + SisPerson.COL_DOB);

        if (isSchoolContext() && queryBy != 0) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

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
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        }

        StringBuilder orderBy = new StringBuilder("ORDER BY ");
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Start Date
                orderBy.append("STD_SKL_OID, ");
                orderBy.append(getDateOrderBy());
                orderBy.append(", STD_NAME_VIEW");
                break;

            case 1: // Name view
            default:
                orderBy.append("STD_SKL_OID, STD_NAME_VIEW, ");
                orderBy.append(getDateOrderBy());
                break;
        }

        if (sql.length() > 0) {
            criteria.addSql(sql.toString());
        }
        BeanQuery query = new BeanQuery(SisStudent.class, criteria, false);
        query.setQueryAdjuster(new OrderByAdjuster(orderBy.toString()));

        /*
         * Execute the query and return the results
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Creates an ORDER BY clause for sorting birthdays.
     *
     * @return String, does not contain leading or trailing spaces nor does it begin with ORDER BY
     */
    private String getDateOrderBy() {
        return getMonthExpression("PSN_DOB") + ", " + getDayExpression("PSN_DOB");
    }

    /**
     * Creates SQL conditions for finding a birthday on a specific month and day. The results of
     * this method are suitable for use with Criteria#addSql(String).
     *
     * @param calendar set to the month and day desired for querying
     *
     * @return String, does not contain leading or trailing spaces
     */
    private String getDateQuery(Calendar calendar) {
        int month = calendar.get(Calendar.MONTH) + 1;
        int date = calendar.get(Calendar.DAY_OF_MONTH);

        StringBuilder sql = new StringBuilder(64);
        sql.append(getMonthExpression("PSN_DOB"));
        sql.append(" = ");
        sql.append(month);
        sql.append(" AND ");
        sql.append(getDayExpression("PSN_DOB"));
        sql.append(" = ");
        sql.append(date);

        return sql.toString();
    }

    /**
     * Returns the SQL expression that returns the day as a numeric value
     *
     *
     * @param dateField String
     * @return String, depends on the database
     */
    private String getDayExpression(String dateField) {
        String monthValue = null;

        switch (DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey()).getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
                monthValue = "DAYOFMONTH(" + dateField + ")";
                break;

            case DatabaseOptimizerFactory.SQLSERVER:
                monthValue = "DATEPART(d," + dateField + ")";
                break;

            default:
                throw new UnsupportedDatabaseException(
                        DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey()).getPlatform());
        }

        return monthValue;
    }

    /**
     * Returns the SQL expression that returns the month as a numeric value.
     *
     * @param dateField String
     * @return String, depends on the database
     */
    private String getMonthExpression(String dateField) {
        String monthValue = null;

        switch (DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey()).getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
                monthValue = "MONTH(" + dateField + ")";
                break;

            case DatabaseOptimizerFactory.SQLSERVER:
                monthValue = "DATEPART(m," + dateField + ")";
                break;

            default:
                throw new UnsupportedDatabaseException(
                        DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey()).getPlatform());
        }

        return monthValue;
    }

    /**
     * Creates SQL conditions for finding a birthday within a month's date range. The results of
     * this method are suitable for use with Criteria#addSql(String).
     *
     * @param calendar set to a date within the month desired for querying
     *
     * @return String, does not contain leading or trailing spaces
     */
    private String getMonthQuery(Calendar calendar) {
        int month = calendar.get(Calendar.MONTH) + 1;
        return getMonthExpression("PSN_DOB") + " = " + month;
    }

    /**
     * Creates SQL conditions for finding a birthday within a week's date range. The results of this
     * method are suitable for use with Criteria#addSql(String).
     *
     * @param calendar set to a date within the week desired for querying
     *
     * @return String, does not contain leading or trailing spaces
     */
    private String getWeekQuery(Calendar calendar) {
        StringBuilder sql = new StringBuilder(64);

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
        int startDate = calendar.get(Calendar.DATE);
        int startMonth = calendar.get(Calendar.MONTH) + 1;

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.SATURDAY);
        int endDate = calendar.get(Calendar.DATE);
        int endMonth = calendar.get(Calendar.MONTH) + 1;

        if (startMonth == endMonth) {
            sql.append(getDayExpression("PSN_DOB"));
            sql.append(" >= ");
            sql.append(startDate);
            sql.append(" AND ");
            sql.append(getDayExpression("PSN_DOB"));
            sql.append(" <= ");
            sql.append(endDate);
            sql.append(" AND ");
            sql.append(getMonthExpression("PSN_DOB"));
            sql.append(" = ");
            sql.append(startMonth);
        } else // End of the week is actually in the next month
        {
            sql.append("((");
            sql.append(getDayExpression("PSN_DOB"));
            sql.append(" >= ");
            sql.append(startDate);
            sql.append(" AND ");
            sql.append(getMonthExpression("PSN_DOB"));
            sql.append(" = ");
            sql.append(startMonth);
            sql.append(") OR (");
            sql.append(getDayExpression("PSN_DOB"));
            sql.append(" <= ");
            sql.append(endDate);
            sql.append(" AND ");
            sql.append(getMonthExpression("PSN_DOB"));
            sql.append(" = ");
            sql.append(endMonth);
            sql.append("))");
        }

        return sql.toString();
    }

}
