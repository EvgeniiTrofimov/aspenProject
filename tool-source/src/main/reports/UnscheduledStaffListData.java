/*
 * ====================================================================
 *
 * X2 Development Corporation
 * A wholly owned subsidiary of Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.StaffSchoolAssociation;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleDay;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Available Covers" report and related reports. This report lists
 * unscheduled staff by period for a particular school
 *
 * @author X2 Development Corporation
 */
public class UnscheduledStaffListData extends ReportJavaSourceNet {
    private static final String PARAM_DATE = "date";
    private static final String PARAM_PERIOD = "period";

    private static final String GRID_PERIOD_ID = "period";
    private static final String GRID_STAFF_NAME = "staff";
    private static final String GRID_DEPARTMENT = "department";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        PlainDate date = (PlainDate) getParameter(PARAM_DATE);
        String periodParam = (String) getParameter(PARAM_PERIOD);

        ScheduleManager scheduleManager = new ScheduleManager(getBroker());

        Schedule schedule = ((SisSchool) getSchool()).getActiveSchedule();
        Collection<SchedulePeriod> schedulePeriods = schedule.getSchedulePeriods();

        // A period was selected, replace the list of all periods with the selected period
        if (!StringUtils.isEmpty(periodParam)) {
            // Get the OID for the period specified in the input
            Map<Object, SchedulePeriod> periodMap = CollectionUtils.getPropertyMap(schedulePeriods, X2BaseBean.COL_OID);
            SchedulePeriod keepPeriod = periodMap.get(periodParam);
            schedulePeriods = new ArrayList<SchedulePeriod>();
            schedulePeriods.add(keepPeriod);
        }

        String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STAFF_ACTIVE_CODE);
        ScheduleDay day = scheduleManager.getDayWithCalendar(schedule.getOid(), date,
                scheduleManager.getMostCommonCalendar(schedule, null));
        if (day != null) {
            for (SchedulePeriod schedulePeriod : schedulePeriods) {
                Collection<String> scheduledStaffOids =
                        scheduleManager.getStaffOidsForSections(schedule.getOid(), date, schedulePeriod.getNumber());

                Criteria criteria = new Criteria();
                criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
                criteria.addOrCriteria(getSecondaryCriteria());

                criteria.addEqualTo(Staff.COL_STATUS, activeCode);
                if (!scheduledStaffOids.isEmpty()) {
                    criteria.addNotIn(X2BaseBean.COL_OID, scheduledStaffOids);
                }

                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);

                QueryIterator iterator = getBroker().getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        SisStaff staff = (SisStaff) iterator.next();

                        grid.append();
                        grid.set(GRID_PERIOD_ID, schedulePeriod.getId());
                        grid.set(GRID_STAFF_NAME, staff.getNameView());
                        grid.set(GRID_DEPARTMENT, staff.getDepartmentCode());
                    }
                } finally {
                    iterator.close();
                }

            }
        }

        /*
         * Sort it out by
         *
         * The JRXML does a group-by with "Student Name" and "Group" so this is needed.
         */

        grid.sort(Arrays.asList(new String[] {GRID_PERIOD_ID, GRID_DEPARTMENT, GRID_STAFF_NAME}), false);
        grid.beforeTop();

        return grid;
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
