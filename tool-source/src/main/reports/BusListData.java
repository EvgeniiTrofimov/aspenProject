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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Bus List report. This report prints the list of students sorted/grouped
 * by AM or PM bus.
 *
 * @author X2 Development Corporation
 */
public class BusListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
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
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws X2BaseException {
        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();

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

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String sortColumn = (String) getParameter(SORT_PARAM);

        if ((SELECTION_SPECIAL_CASE_PREFIX + "all").equals(queryBy)) {
            DataDictionaryField aliasField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                            .findDataDictionaryFieldByAlias(sortColumn.replace(ALIAS_PREFIX, ""));

            if (aliasField != null) {
                criteria.addNotEmpty(aliasField.getJavaName(), getBroker().getPersistenceKey());
            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "BIZ-00006",
                        new Object[] {sortColumn.replace(ALIAS_PREFIX, "")});
            }
        } else {
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);
        }

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        /*
         * Build the sort based on user input.
         */
        applyUserSort(query, sortColumn);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
