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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JREmptyDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Attendance Classroom Input report. This report lists all the students,
 * grouped by section, for each class that meets during the input period for the specified date.
 *
 * @author X2 Development Corporation
 */
public class AttendanceClassroomInputData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the "week of" date. This value is a PlainDate object.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Report parameter name for the List of PlainDate objects.
     */
    public static final String DATES_PARAM = "dates";

    /**
     * Report parameter name for the List of ScheduleDay beans.
     */
    public static final String DAYS_PARAM = "days";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the enumerated "selection string" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /*
     * Context from where the report is being executed from
     */
    private ApplicationContext m_context;
    /*
     * User's staff OID
     */
    private String m_staffOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        JRDataSource studentSchedules = null;

        String attendancePreference =
                PreferenceManager.getPreferenceValue(getSchool(), SisPreferenceConstants.ATT_CLASSROOM_INPUT_TYPE);
        if (attendancePreference.equals(StudentAttendance.INPUT_TYPE_PERIOD)) {
            ScheduleManager scheduleManager = new ScheduleManager(getBroker());


            /*
             * Prepare the lists of schedule days and dates that will be used by the format.
             */
            PlainDate weekday = DateUtils.getStartOfWeek((PlainDate) getParameter(DATE_PARAM));

            Calendar calendar = Calendar.getInstance();
            calendar.setTime(weekday);

            List<PlainDate> dates = new ArrayList<PlainDate>(5);
            List<ScheduleDay> days = new ArrayList<ScheduleDay>(5);
            for (int i = 0; i < 5; i++) {
                ScheduleDay day = scheduleManager.getDayWithCalendar(((SisSchool) getSchool()).getActiveScheduleOid(),
                        weekday,
                        scheduleManager.getMostCommonCalendar(((SisSchool) getSchool()).getActiveSchedule(), null));

                dates.add(weekday);
                days.add(day);

                calendar.add(Calendar.DATE, 1);
                weekday = new PlainDate(calendar.getTime());
            }
            addParameter(DATES_PARAM, dates);
            addParameter(DAYS_PARAM, days);

            /*
             * Create the filters on StudentSchedule. We filter by term and period (the term filter
             * implies a schedule filter).
             */
            X2Criteria criteria = new X2Criteria();

            /*
             * In the staff view, the report needs to be scoped to only students the teacher has
             * in classes.
             */
            if (m_staffOid != null && m_context != null && m_context.equals(ApplicationContext.STAFF)) {
                criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());

                criteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER +
                        ScheduleTeacher.COL_STAFF_OID, m_staffOid);

                criteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
            }

            int queryBy =
                    getParameter(QUERY_BY_PARAM) == null ? 5 : ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            Collection masterScheduleOids = new ArrayList(0);
            switch (queryBy) {
                case 0: // Current Selection
                    Criteria masterCriteria = getCurrentCriteria();
                    SubQuery masterQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, masterCriteria);
                    masterScheduleOids = getBroker().getSubQueryCollectionByQuery(masterQuery);
                    if (!masterScheduleOids.isEmpty()) {
                        criteria.addIn(StudentSchedule.COL_SECTION_OID, masterScheduleOids);
                    } else {
                        addNoMatchCriteria(criteria);
                    }
                    break;

                case 2: // Section
                    criteria.addEqualTo(StudentSchedule.REL_SECTION + "." + MasterSchedule.COL_COURSE_VIEW,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 3: // Teacher
                    criteria.addContainsIgnoreCase(StudentSchedule.REL_SECTION + "." + MasterSchedule.COL_STAFF_VIEW,
                            getParameter(QUERY_STRING_PARAM));
                    break;

                case 4: // Course
                    criteria.addEqualTo(StudentSchedule.REL_SECTION + "." + MasterSchedule.REL_SCHOOL_COURSE + "."
                            + SchoolCourse.COL_NUMBER, getParameter(QUERY_STRING_PARAM));
                    break;

                default: // All
                    break;
            }

            /*
             * Get the full set of terms scheduled this week. We check all dates because a term
             * could start in the middle of the week (see ticket T30023434).
             */
            Set<String> termOids = new HashSet<String>(16);
            for (PlainDate date : dates) {
                termOids.addAll(scheduleManager.getTermOids(((SisSchool) getSchool()).getActiveScheduleOid(), date));

            }
            if (!termOids.isEmpty()) {
                criteria.addIn(StudentSchedule.REL_SECTION + "." + MasterSchedule.COL_SCHEDULE_TERM_OID, termOids);
            } else {
                addNoMatchCriteria(criteria);
            }

            criteria.addEqualTo(StudentSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());

            // Sections that meet during the attendance period.
            Integer period = Integer.valueOf(
                    PreferenceManager.getPreferenceValue(getSchool(), SisPreferenceConstants.ATT_INPUT_PERIOD));
            criteria.addEqualTo(StudentSchedule.REL_SECTION + "." +
                    MasterSchedule.REL_MASTER_TERMS + "." +
                    MasterTerm.REL_MASTER_MATRICES + "." +
                    MasterScheduleMatrix.REL_SCHEDULE_MATRIX + "." +
                    ScheduleMatrix.REL_SCHEDULE_PERIOD + "." +
                    SchedulePeriod.COL_NUMBER, period);

            /*
             * Sort the results.
             */
            QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);
            query.addOrderByAscending(StudentSchedule.REL_SECTION + "." + MasterSchedule.COL_STAFF_VIEW);
            query.addOrderByAscending(StudentSchedule.REL_SECTION + "." + MasterSchedule.COL_COURSE_VIEW);
            query.addOrderByAscending(StudentSchedule.COL_SECTION_OID);
            query.addOrderByAscending(StudentSchedule.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

            /*
             * Set the distinct flag on the results because the StudentSchedule table will match
             * multiple entries in ScheduleMatrix with period X depending upon how many days meet
             * during that period. We use DISTINCT rather than an IN on MasterSchedule OIDs
             * because MySQL 4.1.1 might not handle the double IN (there's already an IN on the
             * term). MySQL 4.1.3 should handle the double IN but Hanover hasn't been upgraded yet
             * and they need this report ASAP!
             *
             * - BTC, 2004-09-02
             */
            query.setDistinct(true);

            studentSchedules =
                    new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
        } else {
            /*
             * The selected school does not take attendance by period so this report makes not
             * sense. We'll just return an empty data set and the user will see an error message.
             */
            studentSchedules = new JREmptyDataSource();
        }

        return studentSchedules;
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
        m_context =
                userData.getSessionNavConfig() == null ? null : userData.getSessionNavConfig().getApplicationContext();
        m_staffOid = userData.getStaffOid();
    }

}
