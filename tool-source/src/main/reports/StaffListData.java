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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.StaffSchoolAssociation;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the following staff reports:
 * <ul>
 * <li>Staff Directory
 * <li>Staff List
 * <li>Staff Labels
 * </ul>
 * These reports simply select staff from the current school and order the results by last name or
 * department.
 *
 * @author X2 Development Corporation
 */
public class StaffListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "group by school" report parameter. The value is a Boolean.
     */
    public static final String GROUP_BY_SCHOOL_PARAM = "groupBySchool";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "secondary staff" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STAFF_PARAM = "secondaryStaff";

    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        QueryByCriteria query = createQueryByCriteria(SisStaff.class, buildCriteria());
        buildSortOrder(query);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Build the criteria based on user input.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = new Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
            criteria = getCurrentCriteria();
        } else {
            if (isSchoolContext()) {
                criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());

                if (((Boolean) getParameter(SECONDARY_STAFF_PARAM)).booleanValue()) {
                    criteria.addOrCriteria(getSecondaryCriteria());
                }
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStaff.class));
            }
        }

        if (((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue()) {
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
        }

        return criteria;
    }

    /**
     * Updates the passed query with sorting based on user input.
     *
     * @param query QueryByCriteria
     */
    private void buildSortOrder(QueryByCriteria query) {
        /*
         * Sort by the school first if the report is grouped by school. We add the school OID as a
         * sort parameter just in case two schools have the same name (unlikely but possible).
         */
        if (((Boolean) getParameter(GROUP_BY_SCHOOL_PARAM)).booleanValue() && !isSchoolContext()) {
            query.addOrderByAscending(SisStaff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStaff.COL_SCHOOL_OID);

            /*
             * Staff aren't necessarily associated with a school. We don't want the sort to
             * eliminate records (see ticket S10010842).
             */
            query.setPathOuterJoin(SisStaff.REL_SCHOOL);
        }

        /*
         * Now sort the staff members based on the user's selection.
         */
        applyUserSort(query, (String) getParameter(SORT_PARAM));
    }

    /**
     * Builds criteria to get Staff with a secondary association with the current school.
     *
     * @return Criteria
     */
    private Criteria getSecondaryCriteria() {
        Criteria criteria = new Criteria();

        Criteria secondaryCriteria = new Criteria();
        secondaryCriteria.addEqualTo(StaffSchoolAssociation.COL_SCHOOL_OID, getSchool().getOid());
        secondaryCriteria.addEqualTo(StaffSchoolAssociation.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        SubQuery secondarySub = new SubQuery(StaffSchoolAssociation.class,
                StaffSchoolAssociation.COL_STAFF_OID, secondaryCriteria);

        criteria.addIn(X2BaseBean.COL_OID, secondarySub);

        return criteria;
    }
}
