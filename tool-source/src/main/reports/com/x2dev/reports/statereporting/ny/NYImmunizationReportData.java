/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ny;

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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Bus List report. This report prints the list of students sorted/grouped
 * by AM or PM bus.
 *
 * @author X2 Development Corporation
 */
public class NYImmunizationReportData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Immunization Oids
     */
    public static final String IMMUNIZATION_OIDS_PARAM = "ImmunizationOids";

    /**
     * queryBy search options
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * queryString search options
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Starting report date
     */
    public static final String START_DATE_PARAM = "date";

    /**
     * @throws X2BaseException
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */

    private SisStudent m_currentStudent;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws X2BaseException {
        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();
        ReportDataGrid grid = new ReportDataGrid();
        ArrayList<String> sort = new ArrayList<String>();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        }


        String queryBy = null;
        /*
         * If we're in the context of a single student, run for just that student.
         */
        if (m_currentStudent != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            /*
             * Build query based on user input
             */
            queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID);
        }

        // AppGlobals.getLog().severe("List of Immunization Oids : " +
        // getParameter(IMMUNIZATION_OIDS_PARAM));

        Collection<String> immunizationFieldOids =
                StringUtils.convertDelimitedStringToList((String) getParameter(IMMUNIZATION_OIDS_PARAM), ',');

        // AppGlobals.getLog().severe("Collection of Immunization Oids : " + immunizationFieldOids);

        criteria.addIn(
                SisStudent.REL_IMMUNIZATION_SERIES + PATH_DELIMITER
                        + HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + PATH_DELIMITER + X2BaseBean.COL_OID,
                immunizationFieldOids);

        // AppGlobals.getLog().severe("Criteria : " + criteria.toString());

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        // Get only distinct students, not students times the number of waivers.
        query.setDistinct(true);

        /*
         * Build the sort based on user input
         *
         * If we are not in the context of a school, sort by the school first to support school
         * grouping on the format.
         */
        if (!isSchoolContext()) {
            query.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        }

        // AppGlobals.getLog().severe("Query : " + query.toString());

        QueryIterator students = null;
        try {
            students = getBroker().getIteratorByQuery(query);
        } catch (org.apache.ojb.broker.PersistenceBrokerSQLException e) {
            if (queryBy.equals("##snapshot")) {
                return grid;
            }
            throw e;
        }


        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                for (HealthImmunizationSeries immunization : student.getImmunizationSeries(getBroker())) {
                    if (immunizationFieldOids.contains(immunization.getImmunizationDefinition().getOid())) {
                        if (immunization.getWaivedIndicator()) {

                            grid.append();
                            grid.set("student", student);
                            grid.set("nameView", student.getNameView());
                            grid.set("yog", Integer.valueOf(student.getYog()));
                            grid.set("school", student.getSchool());
                            grid.set("schoolName", student.getSchool().getName());
                            grid.set("immunization", immunization.getFirstIdentifyingValue());
                            grid.set("reason", immunization.getFieldValueByAlias("DOE IMMUN EXEMPT"));
                        }
                    }
                }
            }
        } finally {
            students.close();
        }


        // Add the user sort param last.
        sort.add("immunization");
        sort.add("schoolName");
        sort.add((String) getParameter(SORT_PARAM));

        // AppGlobals.getLog().severe("SORT --> " + sort.toString());

        // Sort by immunizations, then the user param
        grid.sort(sort, true);

        // AppGlobals.getLog().severe("GRID --> " + grid.toString());

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    // @Override
    // protected void saveState(UserDataContainer userData) throws X2BaseException
    // {
    // this.runOnApplicationServer();
    // super.saveState(userData);
    // }

}
