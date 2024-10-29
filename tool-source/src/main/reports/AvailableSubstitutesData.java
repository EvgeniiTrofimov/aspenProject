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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.license.LicenseProduct;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.ScheduleBellPeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffAttendanceSub;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JREmptyDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class that prepares the data for the Available Substitutes report, which lists the substitute
 * teachers available for a given day.
 *
 * @author X2 Development Corporation
 */
public class AvailableSubstitutesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /** Date to check for available substitutes */
    public static final String DATE_PARAM = "date";

    /** Parameter for a boolean indicating whether or not any substitutes are available */
    public static final String SUBSTITUTES_AVAILABLE_PARAM = "substitutesAvailable";

    /** Map for Available Periods for a Staff Member */
    public static final String AVAILABLE_PERIODS_PARAM = "availablePeriods";

    private boolean m_uk;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate date = (PlainDate) getParameter(DATE_PARAM);

        JRDataSource dataSource = new JREmptyDataSource();

        /*
         * T10002346
         * We re-wrote the report to use collection instead of one big query with two "NotIn"
         * statements
         * is because with the two "NotIn" statements it can bring down the mysql server.
         */
        if (date != null) {
            if (!m_uk) {
                // Create the attendance subquery
                X2Criteria attendanceCriteria = StaffAttendanceManager.getAttendanceCriteria(date, date, null,
                        getOrganization().getOid(), null);

                String substituteOidColumnFromSfa = StaffAttendance.REL_STAFF_ATTENDANCE_SUB + PATH_DELIMITER
                        + StaffAttendanceSub.COL_SUBSTITUTE_OID;
                attendanceCriteria.addNotEmpty(substituteOidColumnFromSfa, getBroker().getPersistenceKey());

                SubQuery attendanceQuery =
                        new SubQuery(StaffAttendance.class, substituteOidColumnFromSfa, attendanceCriteria);
                Collection staffNotInclude = getBroker().getSubQueryCollectionByQuery(attendanceQuery);

                // Create the leave subquery
                X2Criteria leaveCriteria =
                        StaffAttendanceManager.getLeaveCriteria(date, date, null, getOrganization().getOid(), null);
                leaveCriteria.addNotEmpty(StaffLeave.COL_SUBSTITUTE_OID, getBroker().getPersistenceKey());

                SubQuery leaveQuery = new SubQuery(StaffLeave.class, StaffLeave.COL_SUBSTITUTE_OID, leaveCriteria);
                staffNotInclude.addAll(getBroker().getSubQueryCollectionByQuery(leaveQuery));

                // Create the overall query
                String substituteCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_SUBSTITUTE_CODE);

                Criteria staffCriteria = new Criteria();
                staffCriteria.addEqualTo(SisStaff.COL_STAFF_TYPE, substituteCode);
                if (!staffNotInclude.isEmpty()) {
                    staffCriteria.addNotIn(X2BaseBean.COL_OID, staffNotInclude);
                }

                staffCriteria.addAndCriteria(getOrganizationCriteria(SisStaff.class));

                QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);
                staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);

                QueryIterator staff = getBroker().getIteratorByQuery(staffQuery);

                Boolean substitutesAvailable = null;
                if (staff.hasNext()) {
                    dataSource = new QueryIteratorDataSource(staff);
                    substitutesAvailable = Boolean.TRUE;
                } else {
                    substitutesAvailable = Boolean.FALSE;
                }

                addParameter(SUBSTITUTES_AVAILABLE_PARAM, substitutesAvailable);
                addParameter(AVAILABLE_PERIODS_PARAM, new HashMap());
            } else {
                // Get Bell Schedule Periods
                SisSchool school = (SisSchool) getSchool();
                Schedule schedule = school.getActiveSchedule();
                ScheduleBell bellSchedule = ScheduleUtils.getBellScheduleForDay(schedule, date, getBroker());
                Collection<ScheduleBellPeriod> dayBellPeriods = bellSchedule.getScheduleBellPeriods();

                // Make a list of StaffOid of people who are not available for this day.
                Collection staffNotInclude = new ArrayList();

                // Exclude staff who are absent for that day.
                String schoolOid = null;
                if (getSchool() != null) {
                    schoolOid = getSchool().getOid();
                }
                String orgOid = null;
                orgOid = getOrganization().getOid();

                X2Criteria absentCriteria =
                        StaffAttendanceManager.getAttendanceCriteria(date, date, schoolOid, orgOid, null);
                SubQuery absentQuery =
                        new SubQuery(StaffAttendance.class, StaffAttendance.COL_STAFF_OID, absentCriteria);
                Collection absentStaffOids = getBroker().getSubQueryCollectionByQuery(absentQuery);
                staffNotInclude.addAll(absentStaffOids);

                // Exclude staff who were substituting for the full day (Period Oid is null)
                // These are from older records before implementation of the Staff Attendance Sub
                // table.
                X2Criteria coverForDayCriteria =
                        StaffAttendanceManager.getAttendanceSubCriteria(date, date, schoolOid, orgOid, null);
                coverForDayCriteria.addNotEmpty(StaffAttendanceSub.COL_SUBSTITUTE_OID, getBroker().getPersistenceKey());
                coverForDayCriteria.addEmpty(StaffAttendanceSub.COL_SCHEDULE_PERIOD_OID,
                        getBroker().getPersistenceKey());
                SubQuery coverForDayQuery = new SubQuery(StaffAttendanceSub.class,
                        StaffAttendanceSub.COL_SUBSTITUTE_OID, coverForDayCriteria);
                Collection coverForDayStaffOids = getBroker().getSubQueryCollectionByQuery(coverForDayQuery);
                staffNotInclude.addAll(coverForDayStaffOids);

                // Exclude staff who are on leave for that day.
                X2Criteria onLeaveCriteria =
                        StaffAttendanceManager.getLeaveCriteria(date, date, schoolOid, orgOid, null);
                SubQuery onLeaveQuery = new SubQuery(StaffLeave.class, StaffLeave.COL_STAFF_OID, onLeaveCriteria);
                Collection onLeaveStaffOids = getBroker().getSubQueryCollectionByQuery(onLeaveQuery);
                staffNotInclude.addAll(onLeaveStaffOids);

                // Exclude staff who are substituting for another while they are on leave.
                // At this time, it is assumed that the person is covering for the full day.
                X2Criteria leaveCriteria = StaffAttendanceManager.getLeaveCriteria(date, date, schoolOid, orgOid, null);
                leaveCriteria.addNotEmpty(StaffLeave.COL_SUBSTITUTE_OID, getBroker().getPersistenceKey());
                SubQuery leaveQuery = new SubQuery(StaffLeave.class, StaffLeave.COL_SUBSTITUTE_OID, leaveCriteria);
                Collection coverLeaveStaffOids = getBroker().getSubQueryCollectionByQuery(leaveQuery);
                staffNotInclude.addAll(coverLeaveStaffOids);

                // Get a list of staff member who are "substitute" staff type
                // Exclude all staffOids from list generated above.
                String substituteCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_SUBSTITUTE_CODE);

                Criteria staffCriteria = new Criteria();
                staffCriteria.addEqualTo(SisStaff.COL_STAFF_TYPE, substituteCode);
                if (!staffNotInclude.isEmpty()) {
                    staffCriteria.addNotIn(X2BaseBean.COL_OID, staffNotInclude);
                }

                staffCriteria.addAndCriteria(getOrganizationCriteria(SisStaff.class));

                QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);
                staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);
                Collection<Staff> staffs = getBroker().getCollectionByQuery(staffQuery);

                // Get a list of Staff Oid for staff members that are on already substituting from
                // someone else within the date range
                X2Criteria attendanceSubCriteria =
                        StaffAttendanceManager.getAttendanceSubCriteria(date, date, schoolOid, orgOid, null);
                attendanceSubCriteria.addNotEmpty(StaffAttendanceSub.COL_SUBSTITUTE_OID,
                        getBroker().getPersistenceKey());
                attendanceSubCriteria.addNotEmpty(StaffAttendanceSub.COL_SCHEDULE_PERIOD_OID,
                        getBroker().getPersistenceKey());
                QueryByCriteria attendanceSubQuery =
                        new QueryByCriteria(StaffAttendanceSub.class, attendanceSubCriteria);
                QueryIterator staffAttendanceSubs = getBroker().getIteratorByQuery(attendanceSubQuery);

                ArrayList<String> busyPeriods = new ArrayList();
                try {
                    while (staffAttendanceSubs.hasNext()) {
                        StaffAttendanceSub staffAttendanceSub = (StaffAttendanceSub) staffAttendanceSubs.next();
                        busyPeriods.add(staffAttendanceSub.getSubstituteOid() + "-"
                                + staffAttendanceSub.getSchedulePeriodOid());
                    }
                } finally {
                    staffAttendanceSubs.close();
                }

                HashMap availableMap = new HashMap();

                // Determine which Periods the Staff are free.
                Boolean substitutesAvailable = null;
                if (!staffs.isEmpty()) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(getExtendedDictionary(),
                            getBroker().getPersistenceKey());
                    dataSource = new BeanCollectionDataSource(staffs, dictionary, getLocale());
                    substitutesAvailable = Boolean.TRUE;
                    for (Staff subStaff : staffs) {
                        String staffOid = subStaff.getOid();
                        String availablePeriodsStr = "";
                        int i = 0;
                        for (ScheduleBellPeriod bellPeriod : dayBellPeriods) {
                            String periodOid = bellPeriod.getSchedulePeriodOid();
                            String busyKey = staffOid + "-" + periodOid;
                            if (!busyPeriods.contains(busyKey)) {
                                String periodName = bellPeriod.getSchedulePeriod() == null ? bellPeriod.getName()
                                        : bellPeriod.getSchedulePeriod().getName();

                                if (i == 0) {
                                    availablePeriodsStr = periodName;
                                } else {
                                    availablePeriodsStr = availablePeriodsStr + ", " + periodName;
                                }
                                i++;
                            }
                        }
                        availableMap.put(staffOid, availablePeriodsStr);
                    }
                } else {
                    substitutesAvailable = Boolean.FALSE;
                }

                addParameter(SUBSTITUTES_AVAILABLE_PARAM, substitutesAvailable);
                addParameter(AVAILABLE_PERIODS_PARAM, availableMap);
            }
        }

        return dataSource;
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
        m_uk = userData.checkLicense(LicenseProduct.UK);
    }

}
