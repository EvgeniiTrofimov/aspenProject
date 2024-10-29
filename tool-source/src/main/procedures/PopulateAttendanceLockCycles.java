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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Populates cycle and max lock cycle for the organization in which the procedure is ran.
 * If can populate for a specific year or for every year.
 */
public class PopulateAttendanceLockCycles extends ProcedureJavaSource {
    /**
     * Number of days in each cycle.
     */
    public static final int DAYS_PER_CYCLE = 20;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource.execute()
     */
    @Override
    protected void execute() throws Exception {
        String districtContextOid = (String) getParameter("contextOid");
        if (!StringUtils.isEmpty(districtContextOid)) {
            executeForContext(districtContextOid);
        } else {
            QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
            query.addOrderByAscending(DistrictSchoolYearContext.COL_SCHOOL_YEAR);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    DistrictSchoolYearContext year = (DistrictSchoolYearContext) iterator.next();
                    executeForContext(year.getOid());
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Populates the cycle and max lock cycle for the passed context year in the
     * current organization context.
     *
     * @param districtContextOid String
     */
    private void executeForContext(String districtContextOid) {
        QueryIterator iterator = getCalendarIterator(districtContextOid);

        try {
            int dayCount = 0;
            int cycleCount = 1;

            while (iterator.hasNext()) {
                DistrictCalendar date = (DistrictCalendar) iterator.next();

                date.setCycle(getDisplayValue(cycleCount));
                date.setLockCycle(getDisplayValue(cycleCount + 1));

                getBroker().saveBeanForced(date);

                if (date.getInSessionIndicator()) {
                    dayCount++;

                    if (dayCount == DAYS_PER_CYCLE) {
                        cycleCount++;
                        dayCount = 0;
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Gets the DistrictCalendar iterator for the passed schol year in the current
     * organization context.
     *
     * @param districtContextOid String
     * @return Query iterator
     */
    private QueryIterator getCalendarIterator(String districtContextOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, districtContextOid);

        // The first null is the current organization.
        boolean firstNull = true;

        for (int level = 1; level <= 5; level++) {
            String oid = (String) getOrganization().getFieldValueByBeanPath("organization" + level + "Oid");
            if (StringUtils.isEmpty(oid) && firstNull) {
                oid = getOrganization().getOid();
                firstNull = false;
            }

            if (!StringUtils.isEmpty(oid)) {
                criteria.addEqualTo("organization" + level + "Oid", oid);
            } else {
                criteria.addEmpty("organization" + level + "Oid", getBroker().getPersistenceKey());
            }
        }

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderByAscending(DistrictCalendar.COL_DATE);

        return getBroker().getIteratorByQuery(query);
    }

    /**
     * Returns a two character padded string of the passed int.
     *
     * @param cycleCount int
     * @return String
     */
    private String getDisplayValue(int cycleCount) {
        return StringUtils.padLeft(String.valueOf(cycleCount), 2, '0');
    }
}
