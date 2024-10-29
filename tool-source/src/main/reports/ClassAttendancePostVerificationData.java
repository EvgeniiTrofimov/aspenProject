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
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffAttendanceSub;
import com.x2dev.sis.model.beans.StaffPostAttendance;
import com.x2dev.sis.model.business.StudentPeriodAttendanceManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.AttendancePostTimeComparator;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Class Attendance Post History" report. This report is
 * intended for use in the school view, Class History. It finds all sections for
 * a specific date and period. Then lists whether or not attendance data was posted
 * for each of the sections
 * 
 * @author X2 Development Corporation
 */
public class ClassAttendancePostVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the map of absent staff OIDs to covering staff beans. The value is
     * a Map.
     */
    private static final String ABSENT_STAFF_OID_TO_COVER_STAFF_MAP_PARAM = "absentStaffOidToCoveringStaff";

    /**
     * Parameter used to obtain a date value. This date value is a PlainDate
     */
    private static final String DATE_INPUT = "date";

    /**
     * Report parameter name for the map of staff OIDs to StaffPostAttendance beans. The value is an
     * Map.
     */
    public static final String MASTER_TO_POST_MAP_PARAM = "masterOidToPostBeans";

    /**
     * Report parameter name for the map of master to Staff beans. The value is a Map.
     */
    public static final String MASTER_TO_STAFF_VIEW_PARAM = "masterOidToStaffView";

    /**
     * Parameter name for the input field for period picklist
     */
    public static final String PERIOD_BEAN_PARAM = "periodBean";

    /**
     * Parameter used to obtain a period value. The period value is a String
     */
    private static final String PERIOD_INPUT = "period";

    /**
     * Report parameter name for preference to show "No Posts" only. This value is
     * a boolean
     */
    private static final String SHOW_NO_POSTS_ONLY_INPUT = "onlyNoPosts";

    /**
     * Parameter used to obtain which column to sort by. This value is a string
     */
    private static final String SORT_INPUT = "sort";

    /**
     * Parameter used to obtain whether to use ascending or descending sort in the query.
     * This value is a boolean
     */
    private static final String SORT_ORDER_INPUT = "sortOrder";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Grab input parameters
         */
        PlainDate date = (PlainDate) getParameter(DATE_INPUT);
        String periodOid = (String) getParameter(PERIOD_INPUT);
        int sort = ((Integer) getParameter(SORT_INPUT)).intValue();
        boolean sortOrder = ((Boolean) getParameter(SORT_ORDER_INPUT)).booleanValue();
        boolean showNoPostsOnly = ((Boolean) getParameter(SHOW_NO_POSTS_ONLY_INPUT)).booleanValue();

        SchedulePeriod period = (SchedulePeriod) getBroker().getBeanByOid(SchedulePeriod.class, periodOid);

        // Get the Track Attendance By Period preference value for the school
        boolean trackByPeriod = StudentPeriodAttendanceManager.getTrackAttendanceByPeriodPreferenceValue(getSchool());

        List<MasterSchedule> sortedSections = null;
        Map<String, Staff> sectionStaffViews = new HashMap<String, Staff>();

        /*
         * Select MST records on the input date & period
         */
        ScheduleManager schManager = new ScheduleManager(getBroker());
        Criteria sectionCriteria = schManager.getSections(((SisSchool) getSchool()).getActiveScheduleOid(),
                date,
                periodOid);

        if (sectionCriteria != null) {
            SubQuery sectionSubQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, sectionCriteria);

            /*
             * Select SPA records on the input date & period att type
             * where the SPA master oid is in (select MST records on the input date & period)
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StaffPostAttendance.COL_DATE, date);
            criteria.addEqualTo(StaffPostAttendance.COL_ATTENDANCE_TYPE,
                    Integer.valueOf(StaffPostAttendance.PERIOD_ATTENDANCE));
            criteria.addIn(StaffPostAttendance.COL_MASTER_SCHEDULE_OID, sectionSubQuery);

            if (trackByPeriod) {
                criteria.addContains(StaffPostAttendance.COL_PERIOD_VIEW, period.getId());
            }

            QueryByCriteria postQuery = createQueryByCriteria(StaffPostAttendance.class, criteria);

            HashMap<String, StaffPostAttendance> masterOidsToPostBeans = new HashMap<String, StaffPostAttendance>(128);
            QueryIterator postData = getBroker().getIteratorByQuery(postQuery);
            try {
                while (postData.hasNext()) {
                    StaffPostAttendance postBean = (StaffPostAttendance) postData.next();
                    masterOidsToPostBeans.put(postBean.getMasterScheduleOid(), postBean);
                }
            } finally {
                postData.close();
            }

            if (showNoPostsOnly) {
                /*
                 * Query now looks like...
                 *
                 * Select MST records on the input date & time
                 * where SPA master oid is not in
                 * (Select SPA records on the input date & class type)
                 */
                X2Criteria postSubCriteria = new X2Criteria();
                postSubCriteria.addEqualTo(StaffPostAttendance.COL_DATE, date);
                postSubCriteria.addEqualTo(StaffPostAttendance.COL_ATTENDANCE_TYPE,
                        Integer.valueOf(StaffPostAttendance.PERIOD_ATTENDANCE));
                postSubCriteria.addNotNull(StaffPostAttendance.COL_MASTER_SCHEDULE_OID);

                if (trackByPeriod) {
                    postSubCriteria.addContains(StaffPostAttendance.COL_PERIOD_VIEW, period.getId());
                }

                SubQuery postSubQuery = new SubQuery(StaffPostAttendance.class,
                        StaffPostAttendance.COL_MASTER_SCHEDULE_OID, postSubCriteria);
                sectionCriteria.addNotIn(X2BaseBean.COL_OID, postSubQuery);
            }

            QueryByCriteria sectionQuery = createQueryByCriteria(MasterSchedule.class, sectionCriteria);

            addParameter(MASTER_TO_POST_MAP_PARAM, masterOidsToPostBeans);
            addParameter(PERIOD_BEAN_PARAM, period);

            /*
             * Comparator used to allow sorting of columns not in MasterSchedule, but those in the
             * StaffPostAttendance hashmap masterOidsToPostBeans
             */
            AttendancePostTimeComparator postTimeComparator =
                    new AttendancePostTimeComparator(masterOidsToPostBeans, MasterSchedule.COL_STAFF_VIEW);

            /*
             * Name or Timestamp sort in either ascending or descending order
             */
            switch (sort) {
                case 0:
                    sectionQuery.addOrderBy(MasterSchedule.COL_STAFF_VIEW, sortOrder);
                    sortedSections = new ArrayList<MasterSchedule>(getBroker().getCollectionByQuery(sectionQuery));
                    break;
                case 1:
                    sortedSections = new ArrayList<MasterSchedule>(getBroker().getCollectionByQuery(sectionQuery));
                    Collections.sort(sortedSections, postTimeComparator);
                    break;
            }
        } else {
            sortedSections = new ArrayList(0);
        }


        for (MasterSchedule section : sortedSections) {
            /*
             * For the list of sorted sections if it is a split section then find the teacher
             * who is scheduled for that date and periodOid.
             */
            Staff primaryStaff = section.getPrimaryStaff();
            if (section.getSplitTeacherIndicator()) {
                for (ScheduleTeacher teacher : section.getTeacherSections()) {
                    if (!teacher.getScheduleMasterTeacherMatrix().isEmpty() &&
                            schManager.isScheduled(section, date, periodOid, teacher.getStaffOid())) {
                        primaryStaff = teacher.getStaff();
                        break;
                    }
                }
            }
            // Put the staff into the map whether it be the primary or the overriding primary.
            sectionStaffViews.put(section.getOid(), primaryStaff);
        }

        addParameter(MASTER_TO_STAFF_VIEW_PARAM, sectionStaffViews);
        addParameter(ABSENT_STAFF_OID_TO_COVER_STAFF_MAP_PARAM, populateCoverMap(date, periodOid, getSchool()));

        return new JRBeanCollectionDataSource(sortedSections);
    }

    /**
     * Returns a map of covering staff keyed on absent staff OID.
     *
     * @param date PlainDate
     * @param periodOid String
     * @param school School
     * @return Map&lt;String, SisStaff&gt;
     */
    private Map<String, SisStaff> populateCoverMap(PlainDate date, String periodOid, School school) {
        Map<String, SisStaff> coveringStaffByAbsentStaffOid = new HashMap<String, SisStaff>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER + StaffAttendance.COL_DATE, date);
        criteria.addEqualTo(StaffAttendanceSub.REL_STAFF_ATTENDANCE + PATH_DELIMITER + StaffAttendance.COL_SCHOOL_OID,
                school.getOid());

        QueryByCriteria query = new QueryByCriteria(StaffAttendanceSub.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StaffAttendanceSub sab = (StaffAttendanceSub) iterator.next();

                int substituteType = sab.getSubstituteType();
                if (substituteType == StaffAttendanceSub.SubstituteType.DAILY.ordinal() ||
                        (substituteType == StaffAttendanceSub.SubstituteType.PERIOD.ordinal() &&
                                !StringUtils.isEmpty(sab.getSchedulePeriodOid())
                                && sab.getSchedulePeriodOid().equals(periodOid))) {
                    coveringStaffByAbsentStaffOid.put(sab.getStaffAttendance().getStaffOid(), sab.getSubstitute());
                }
            }
        } finally {
            iterator.close();
        }

        return coveringStaffByAbsentStaffOid;
    }
}
