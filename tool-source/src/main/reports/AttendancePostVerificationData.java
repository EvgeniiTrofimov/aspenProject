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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffAttendanceSub;
import com.x2dev.sis.model.beans.StaffPostAttendance;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.tools.reports.AttendancePostTimeComparator;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Daily Attendance Post report. This report lists all the staff, ordered
 * by school, and what time (if ever) they posted daily attendance.
 *
 * @author X2 Development Corporation
 */
public class AttendancePostVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the attendance date. This value is a PlainDate object.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Reoprt column name which contains the StaffPostAttendance bean.
     */
    public static final String FIELD_POST = "Post";

    /**
     * Report column name which contains the school which the record is
     * associated with
     */
    public static final String FIELD_SCHOOL = "School";

    /**
     * Report column name which contains the Staff bean.
     */
    public static final String FIELD_STAFF = "Staff";

    /**
     * Report column which contains what should be shown for the staff's name
     */
    public static final String FIELD_STAFF_VIEW = "StaffView";

    /**
     * Report parameter name for the sort order. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Report parameter name for preference to show "No Posts" only. This value is
     * a boolean.
     */
    private static final String SHOW_NO_POSTS_ONLY_INPUT = "onlyNoPosts";

    /**
     * Report parameter name for the map of staff OIDs to StaffPostAttendance beans. This
     * value is a Map.
     */
    public static final String STAFF_TO_POST_MAP = "staffOidToPostBeans";

    /**
     * Other report variables.
     */
    private String m_postedByResourceMessage;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Start by retrieving the user input for the report parameters: Date, Sort, Schools
         */
        PlainDate date = (PlainDate) getParameter(DATE_PARAM);

        boolean sortByName = true;

        if (((Integer) getParameter(SORT_PARAM)).intValue() == 1) {
            sortByName = false;
        }

        Map<String, String> attendancePreferenceBySchool = new HashMap<String, String>();
        Map<String, Collection<String>> dailyCoverStfOidsBySklOid = new HashMap<String, Collection<String>>();

        Criteria schoolCriteria = new Criteria();

        if (isSchoolContext()) {
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            schoolCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            schoolCriteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        }

        /*
         * Get all post data for the given date. Build a map between the combined key(staff
         * OID-attendanceType
         * and the StaffPostAttendance bean.
         */
        Criteria postCriteria = new Criteria();
        postCriteria.addNotEqualTo(StaffPostAttendance.COL_ATTENDANCE_TYPE,
                Integer.valueOf(StaffPostAttendance.PERIOD_ATTENDANCE));
        postCriteria.addEqualTo(StaffPostAttendance.COL_DATE, date);

        if (isSchoolContext()) {
            postCriteria.addEqualTo(StaffPostAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            postCriteria.addAndCriteria(getOrganizationCriteria(StaffPostAttendance.class));
        }

        QueryByCriteria postQuery = new QueryByCriteria(StaffPostAttendance.class, postCriteria);

        HashMap<String, StaffPostAttendance> staffOidsAttTypeToPostBeans = new HashMap(300);
        QueryIterator postData = getBroker().getIteratorByQuery(postQuery);
        try {
            while (postData.hasNext()) {
                StaffPostAttendance postBean = (StaffPostAttendance) postData.next();

                SisSchool school = postBean.getSchool();
                String schoolOid = school.getOid();
                String attendancePreference = attendancePreferenceBySchool.get(schoolOid);
                if (StringUtils.isEmpty(attendancePreference)) {
                    attendancePreference = PreferenceManager.getPreferenceValue(school,
                            SisPreferenceConstants.ATT_CLASSROOM_INPUT_TYPE);
                    attendancePreferenceBySchool.put(schoolOid, attendancePreference);
                }

                Collection<String> dailyCoverStaffOids = dailyCoverStfOidsBySklOid.get(schoolOid);
                if (dailyCoverStaffOids == null) {
                    dailyCoverStaffOids = getDailyCoverStaffOids(school, date);
                    dailyCoverStfOidsBySklOid.put(schoolOid, dailyCoverStaffOids);
                }

                String attPreferenceKey = null;
                if (StudentAttendance.INPUT_TYPE_HOMEROOM.equals(attendancePreference) &&
                        dailyCoverStaffOids.contains(postBean.getStaffResponsibleOid())) {
                    /*
                     * Note on postBean.getPeriodView():
                     * This is a hack for story S-12490. If a staff is responsible for two or more
                     * homerooms for a day (either
                     * staff has a primary homeroom and covers for another staff with different
                     * homeroom, or a
                     * sub staff covers for two staffs with two different homerooms), then, we make
                     * use of
                     * SPA_PERIOD_VIEW to store and uniquely identify SPA records by homeroom.
                     */
                    attPreferenceKey = postBean.getPeriodView();
                } else if (StudentAttendance.INPUT_TYPE_PERIOD.equals(attendancePreference) &&
                        dailyCoverStaffOids.contains(postBean.getStaffResponsibleOid())) {
                    attPreferenceKey = postBean.getMasterScheduleOid();
                }

                staffOidsAttTypeToPostBeans
                        .put(postBean.getStaffResponsibleOid() + "-" + postBean.getAttendanceType() + "-"
                                + attPreferenceKey, postBean);
            }
        } finally {
            postData.close();
        }

        /*
         * Get all the staff responsible for posting daily attendance. The list of staff is always
         * sorted first by school but it could be sorted next by either staff name or post time.
         */
        AttendancePostTimeComparator postTimeComparator = null;
        if (!sortByName) {
            postTimeComparator = new AttendancePostTimeComparator(staffOidsAttTypeToPostBeans, SisStaff.COL_NAME_VIEW);
        }

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
        schoolQuery.addOrderByAscending(SisSchool.COL_SCHOOL_ID);
        schoolQuery.addOrderByAscending(X2BaseBean.COL_OID); // Just in case multiple schools have
                                                             // the same ID

        ReportDataGrid grid = new ReportDataGrid();
        QueryIterator schools = getBroker().getIteratorByQuery(schoolQuery);
        try {
            while (schools.hasNext()) {
                // Get the staff for the current school
                SisSchool school = (SisSchool) schools.next();

                String attPreference = attendancePreferenceBySchool.get(school.getOid());
                if (!attendancePreferenceBySchool.containsKey(school.getOid())) {
                    attPreference = PreferenceManager.getPreferenceValue(school,
                            SisPreferenceConstants.ATT_CLASSROOM_INPUT_TYPE);
                }

                LinkedList<SisStaff> sortedStaff = new LinkedList();
                sortedStaff.addAll(retrieveSortedStaff(school, date, sortByName, postTimeComparator, attPreference));

                Map<String, StaffAttendanceSub> coveringStaffByAbsentStaffOid =
                        populateSubMap(school, date, attPreference);

                String attendancePreference =
                        PreferenceManager.getPreferenceValue(school, SisPreferenceConstants.ATT_CLASSROOM_INPUT_TYPE);

                // foreach staff in the school, add them to the ReportDataGrid
                for (SisStaff staff : sortedStaff) {
                    if (attendancePreference.equals(StudentAttendance.INPUT_TYPE_HOMEROOM)) {
                        if (!StringUtils.isEmpty(staff.getHomeroom())) {
                            createRow(grid, staffOidsAttTypeToPostBeans, coveringStaffByAbsentStaffOid, staff,
                                    StaffPostAttendance.DAILY_ATTENDANCE, school, attendancePreference);
                        }

                        if (!StringUtils.isEmpty(staff.getHomeroom2())) {
                            createRow(grid, staffOidsAttTypeToPostBeans, coveringStaffByAbsentStaffOid, staff,
                                    StaffPostAttendance.DAILY_ATTENDANCE_HOMEROOM2, school, attendancePreference);
                        }
                    } else {
                        createRow(grid, staffOidsAttTypeToPostBeans, coveringStaffByAbsentStaffOid, staff,
                                StaffPostAttendance.DAILY_ATTENDANCE, school, attendancePreference);
                    }
                }
            }
        } finally {
            schools.close();
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(UserDataContainer userData)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_postedByResourceMessage = WebUtils.getMessages(userData).getMessage(getLocale(),
                "label.dailyAttendancePostVerification.report.postedBy");
    }

    /**
     * Creates a row in the ReportDataGrid to be displayed in the report. Column types:
     * Staff, string, StaffPostAttendance, School. The 2nd column is a formatted string with what
     * to display in the Staff column. If only 1 homeroom exists, only the Staff.COL_NAMEVIEW is
     * shown. But if 2 homerooms are set, then we append the homeroom name to the end to know which
     * homeroom the data corresponds to.
     *
     * @param grid ReportDataGrid
     * @param map HashMap<String,StaffPostAttendance>
     * @param coveringStaffByAbsentStaffOid Map<String,StaffAttendanceSub>
     * @param staff SisStaff
     * @param attType int
     * @param school SisSchool
     * @param attendancePreference String
     */
    private void createRow(ReportDataGrid grid,
                           HashMap<String, StaffPostAttendance> map,
                           Map<String, StaffAttendanceSub> coveringStaffByAbsentStaffOid,
                           SisStaff staff,
                           int attType,
                           SisSchool school,
                           String attendancePreference) {
        boolean showNoPostsOnly = ((Boolean) getParameter(SHOW_NO_POSTS_ONLY_INPUT)).booleanValue();

        String staffFormat = staff.getNameView();
        String homeroom1 = staff.getHomeroom();
        String homeroom2 = staff.getHomeroom2();

        if (attendancePreference.equals(StudentAttendance.INPUT_TYPE_HOMEROOM)) {
            if (homeroom1 != null && homeroom2 != null) {
                if (attType == StaffPostAttendance.DAILY_ATTENDANCE) {
                    staffFormat += " - " + staff.getHomeroom();
                } else if (attType == StaffPostAttendance.DAILY_ATTENDANCE_HOMEROOM2) {
                    staffFormat += " - " + staff.getHomeroom2();
                }
            }
        }

        String attPreferenceKey = null;
        String spaKey = null;

        String staffOid = staff.getOid();
        if (coveringStaffByAbsentStaffOid.containsKey(staffOid)) {
            StaffAttendanceSub sab = coveringStaffByAbsentStaffOid.get(staffOid);

            if (StudentAttendance.INPUT_TYPE_HOMEROOM.equals(attendancePreference)) {
                attPreferenceKey = homeroom1;
            } else if (StudentAttendance.INPUT_TYPE_PERIOD.equals(attendancePreference) &&
                    sab.getSubstituteType() == StaffAttendanceSub.SubstituteType.PERIOD.ordinal()) {
                attPreferenceKey = sab.getMasterScheduleOid();
            }

            SisStaff coveringStaff = sab.getSubstitute();
            staffOid = coveringStaff.getOid();

            spaKey = staffOid + "-" + attType + "-" + attPreferenceKey;

            /*
             * The attPreferenceKey is only relevant if the staff covered multiple homerooms during
             * a single day, otherwise the periodView field on the SPA record will be null.
             */
            if (!map.containsKey(spaKey) &&
                    !StringUtils.isEmpty(attPreferenceKey) &&
                    StudentAttendance.INPUT_TYPE_HOMEROOM.equals(attendancePreference)) {
                attPreferenceKey = null;
                spaKey = staffOid + "-" + attType + "-" + attPreferenceKey;
            }

            String coverPostMessage = "";
            if (map.containsKey(spaKey)) {
                coverPostMessage = m_postedByResourceMessage;
            } else {
                coverPostMessage = "Covered by";
            }

            staffFormat += " (" + coverPostMessage + " " + coveringStaff.getNameView() + ")";
        }

        String key = staffOid + "-" + attType + "-" + attPreferenceKey;
        StaffPostAttendance spa = map.get(key);

        if (spa != null && !ObjectUtils.matchStrict(spa.getStaffOid(), spa.getStaffResponsibleOid()) &&
                !coveringStaffByAbsentStaffOid.containsKey(staffOid)) {
            staffFormat += " (" + m_postedByResourceMessage + " " + spa.getStaff().getNameView() + ")";
        }

        if (spa != null && !showNoPostsOnly) {
            grid.append();
            grid.set(FIELD_STAFF, staff);
            grid.set(FIELD_STAFF_VIEW, staffFormat);
            grid.set(FIELD_POST, spa);
            grid.set(FIELD_SCHOOL, school);
        } else if (spa == null) {
            grid.append();
            grid.set(FIELD_STAFF, staff);
            grid.set(FIELD_STAFF_VIEW, staffFormat);
            grid.set(FIELD_POST, null);
            grid.set(FIELD_SCHOOL, school);
        }
    }

    /**
     * Returns a collection of staff OIDs who cover as a daily sub for absent staff.
     *
     * @param school SisSchool
     * @param date PlainDate
     * @return Collection
     */
    private Collection<String> getDailyCoverStaffOids(SisSchool school, PlainDate date) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StaffAttendanceSub.COL_SUBSTITUTE_TYPE,
                Integer.valueOf(StaffAttendanceSub.SubstituteType.DAILY.ordinal()));
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER +
                StaffAttendance.COL_DATE, date);
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER +
                StaffAttendance.COL_SCHOOL_OID, school.getOid());

        SubQuery coverOidsSub = new SubQuery(StaffAttendanceSub.class, StaffAttendanceSub.COL_SUBSTITUTE_OID, criteria);

        return getBroker().getSubQueryCollectionByQuery(coverOidsSub);
    }

    /**
     * Returns a map of covering staff attendance sub bean keyed on absent staff OID.
     *
     * @param school SisSchool
     * @param date PlainDate
     * @param attPreference String
     * @return Map<String, StaffAttendanceSub>
     */
    private Map<String, StaffAttendanceSub> populateSubMap(SisSchool school, PlainDate date, String attPreference) {
        Map<String, StaffAttendanceSub> coveringStaffByAbsentStaffOid = new HashMap<String, StaffAttendanceSub>();

        SchedulePeriod attendanceInputPeriod = null;
        if (StudentAttendance.INPUT_TYPE_PERIOD.equals(attPreference)) {
            int periodNumber = Integer
                    .parseInt(PreferenceManager.getPreferenceValue(school, SisPreferenceConstants.ATT_INPUT_PERIOD));
            ScheduleStructureManager structureManager = new ScheduleStructureManager(getBroker());
            attendanceInputPeriod = structureManager.getPeriod(school.getActiveScheduleOid(), periodNumber);
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER +
                StaffAttendance.COL_DATE, date);
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER +
                StaffAttendance.COL_SCHOOL_OID, school.getOid());
        criteria.addNotEmpty(StaffAttendanceSub.COL_SUBSTITUTE_OID, getBroker().getPersistenceKey());
        criteria.addNotNull(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER +
                StaffAttendance.COL_STAFF_OID);

        QueryByCriteria query = new QueryByCriteria(StaffAttendanceSub.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                StaffAttendanceSub sab = (StaffAttendanceSub) iterator.next();

                boolean isDailyCover = (sab.getSubstituteType() == StaffAttendanceSub.SubstituteType.DAILY.ordinal());

                if (isDailyCover ||
                        StudentAttendance.INPUT_TYPE_HOMEROOM.equals(attPreference) ||
                        (StudentAttendance.INPUT_TYPE_PERIOD.equals(attPreference) &&
                                attendanceInputPeriod != null &&
                                attendanceInputPeriod.getOid().equals(sab.getSchedulePeriodOid()))) {
                    coveringStaffByAbsentStaffOid.put(sab.getStaffAttendance().getStaffOid(), sab);
                }
            }
        } finally {
            iterator.close();
        }

        return coveringStaffByAbsentStaffOid;
    }

    /**
     * Returns the list of staff members responsible for posting daily attendance on the given date
     * at the given school. The list will be sorted by either staff name or by post time depending
     * upon the value of sortByName.
     *
     * @param school SisSchool
     * @param date PlainDate
     * @param sortByName If true the resulting list is sorted by staff name and then
     *        homeroom; otherwise the list is sorted by post time and then by
     *        staff name
     * @param postTimeComparator A comparator for StaffPostAttendance beans, this value can be
     *        null if sortByName is true
     * @param attendancePreference String
     * @return A List of Staff beans
     */
    private List retrieveSortedStaff(SisSchool school,
                                     PlainDate date,
                                     boolean sortByName,
                                     AttendancePostTimeComparator postTimeComparator,
                                     String attendancePreference) {
        /*
         * If the school takes attendance by homeroom then we just look for active staff that have a
         * non-empty homeroom. If the school takes attendance by period then we look for staff that
         * teach a section that meets at the preferred period on the given date.
         */
        X2Criteria staffCriteria = new X2Criteria();
        staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, school.getOid());

        try {
            if (attendancePreference.equals(StudentAttendance.INPUT_TYPE_HOMEROOM)) {
                /*
                 * select *
                 * from Staff
                 * where schoolOid = school.getOid()
                 * and status = "Active"
                 * and (homeroom != null or homeroom2 != null)
                 */
                String statusCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                staffCriteria.addEqualTo(SisStaff.COL_STATUS, statusCode);

                X2Criteria subStaffCriteria = new X2Criteria();
                subStaffCriteria.addNotNull(SisStaff.COL_HOMEROOM);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addNotNull(SisStaff.COL_HOMEROOM2);

                subStaffCriteria.addOrCriteria(orCriteria);

                staffCriteria.addAndCriteria(subStaffCriteria);
            } else {
                int period = Integer.parseInt(
                        PreferenceManager.getPreferenceValue(school, SisPreferenceConstants.ATT_INPUT_PERIOD));
                ScheduleManager manager = new ScheduleManager(getBroker());
                Collection staffOids = manager.getStaffOidsForSections(school.getActiveScheduleOid(), date, period);
                if (staffOids.isEmpty()) {
                    staffCriteria.addEqualTo("0", "1");
                } else {
                    staffCriteria.addIn(X2BaseBean.COL_OID, staffOids);
                }
            }
        } catch (Exception e) {
            /*
             * Do nothing. The likelihood of an exception being thrown is very small. Even if
             * one is thrown then in the worst case scenario the homeroom filter or the IN criteria
             * will not be applied and all teachers will be returned - not a big deal.
             */
        }

        /*
         * We have the staff criteria, now let's sort the results.
         */
        QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);

        List sortedStaff = null;
        if (sortByName) {
            staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);
            staffQuery.addOrderByAscending(SisStaff.COL_HOMEROOM);
            sortedStaff = new ArrayList(getBroker().getCollectionByQuery(staffQuery));
        } else {
            sortedStaff = new ArrayList(getBroker().getCollectionByQuery(staffQuery));
            Collections.sort(sortedStaff, postTimeComparator);
        }

        return sortedStaff;
    }
}
