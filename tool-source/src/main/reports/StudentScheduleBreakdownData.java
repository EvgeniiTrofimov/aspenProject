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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleAttributes;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Schedule Breakdown" report.
 *
 * @author X2 Development Corporation
 */
public class StudentScheduleBreakdownData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_ACTIVITY_PERCENT = "activity";
    private static final String FIELD_CLASS_PERCENT = "class";
    private static final String FIELD_CREDITS = "totalCredits";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDY_PERCENT = "study";
    private static final String FIELD_TOTAL_PERCENT = "total";

    private ScheduleReportHelper m_reportHelper;
    private ScheduleManager m_scheduleManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_scheduleManager = new ScheduleManager(getBroker());

        Criteria criteria = new Criteria();
        if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            Criteria attributeCriteria = new Criteria();
            attributeCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID,
                    m_reportHelper.getStudentScheduleOid());
            attributeCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);

            SubQuery attributeSubQuery = new SubQuery(StudentScheduleAttributes.class,
                    StudentScheduleAttributes.COL_STUDENT_OID, attributeCriteria);
            criteria.addIn(X2BaseBean.COL_OID, attributeSubQuery);
        }

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);

        addUserCriteria(criteria, queryBy, queryString, null, null);

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        if (activeOnly) {
            criteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        }

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(studentQuery, sort);

        ReportDataGrid grid = new ReportDataGrid(10);
        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (iterator.hasNext()) {
                SisStudent currentStudent = (SisStudent) iterator.next();

                grid.append();
                grid.set(FIELD_STUDENT, currentStudent);
                grid.set(FIELD_CREDITS, new BigDecimal(0.0));
                grid.set(FIELD_TOTAL_PERCENT, Double.valueOf(0.0));
                grid.set(FIELD_CLASS_PERCENT, Double.valueOf(0.0));
                grid.set(FIELD_STUDY_PERCENT, Double.valueOf(0.0));
                grid.set(FIELD_ACTIVITY_PERCENT, Double.valueOf(0.0));

                Map scheduleMap = getStudentSchedules(currentStudent.getOid());
                if (scheduleMap != null && !scheduleMap.isEmpty()) {
                    Collection activities = (Collection) scheduleMap.get(SchoolCourse.MASTER_TYPE_ACTIVITY);
                    Collection classes = (Collection) scheduleMap.get(SchoolCourse.MASTER_TYPE_CLASS);
                    Collection studies = (Collection) scheduleMap.get(SchoolCourse.MASTER_TYPE_STUDY);
                    Collection fullSchedule = new ArrayList(20);

                    double totalPercent = 0.0;
                    Double activityPercent = Double.valueOf(0.0);
                    Double classPercent = Double.valueOf(0.0);
                    Double studyPercent = Double.valueOf(0.0);

                    if (activities != null) {
                        activityPercent = calculatePercentScheduled(currentStudent, activities);
                        totalPercent += activityPercent.doubleValue();
                        fullSchedule.addAll(activities);
                    }

                    if (classes != null) {
                        classPercent = calculatePercentScheduled(currentStudent, classes);
                        totalPercent += classPercent.doubleValue();
                        fullSchedule.addAll(classes);
                    }

                    if (studies != null) {
                        studyPercent = calculatePercentScheduled(currentStudent, studies);
                        totalPercent += studyPercent.doubleValue();
                        fullSchedule.addAll(studies);
                    }

                    saveGridRow(grid, activityPercent, classPercent, studyPercent, totalPercent, fullSchedule);
                } else {
                    int filterBy = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();
                    if (filterBy == 1) {
                        grid.deleteRow();
                    }
                }
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Sums up the total of credits that the student is taking.
     *
     * @param schedules Collection
     * @return total credits in a BigDecimal
     */
    private BigDecimal calculateCredits(Collection schedules) {
        double total = 0.0;

        Iterator iterator = schedules.iterator();
        while (iterator.hasNext()) {
            StudentSection studentSection = (StudentSection) iterator.next();

            String sectionType = studentSection.getSection().getType();
            SchoolCourse schoolCourse = studentSection.getSection().getSchoolCourse();

            if (sectionType.equals(SchoolCourse.MASTER_TYPE_CLASS)) {
                total += schoolCourse.getCredit() != null ? schoolCourse.getCredit().doubleValue() : 0.0;
            }
        }

        return new BigDecimal(total);
    }

    /**
     * Calculates the scheduled bits percentage.
     *
     * @param student SisStudent
     * @param schedules Collection
     * @return percent scheduled
     */
    private Double calculatePercentScheduled(SisStudent student, Collection schedules) {
        Schedule schedule = m_reportHelper.getSchedule();

        ScheduleBell bellSchedule = m_scheduleManager.getStudentBellSchedule(student.getOid(), schedule);
        int periods = m_scheduleManager.getMaxNumberOfPeriodsCanBeUsed(m_reportHelper.getSchedule(), bellSchedule);

        int totalBits = schedule.getDays() * periods * schedule.getTerms();
        int bitsScheduledPercentage = 0;
        if (totalBits != 0) {
            int[] bitsScheduled = m_scheduleManager.getScheduledBits(schedules, m_reportHelper.getSectionClass());
            bitsScheduledPercentage = (bitsScheduled[0] + bitsScheduled[1] + bitsScheduled[2]) * 100 / totalBits;
        }

        return Double.valueOf(bitsScheduledPercentage);
    }

    /**
     * Returns a map of a Collection of student schedules grouped by the section type (class, study,
     * duty, activity) keyed to the section type.
     *
     * @param studentOid String
     * @return Map of Collections
     */
    private Map getStudentSchedules(String studentOid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentSection.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        return getBroker().getGroupedCollectionByQuery(query, StudentSection.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE, 15);
    }

    /**
     * Save the grid row if the student meets the criteria. Otherwise drop the student from the
     * grid.
     *
     * @param grid ReportDataGrid
     * @param activity Double
     * @param classes Double
     * @param study Double
     * @param totalPercent double
     * @param fullSchedule Collection
     */
    private void saveGridRow(ReportDataGrid grid,
                             Double activity,
                             Double classes,
                             Double study,
                             double totalPercent,
                             Collection fullSchedule) {
        int filterBy = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();
        boolean isDropped = false;

        switch (filterBy) {
            case 1: // Over
                if (totalPercent <= 100.00) {
                    isDropped = true;
                }
                break;

            case 2: // Under
                if (totalPercent >= 100.00) {
                    isDropped = true;
                }
                break;

            case 3: // Under not including studies
                if ((activity.doubleValue() + classes.doubleValue()) >= 100.00) {
                    isDropped = true;
                }
                break;

            case 4: // Not scheduled
                if (totalPercent > 0.0) {
                    isDropped = true;
                }
                break;

            default: // All
                break;
        }

        if (isDropped) {
            grid.deleteRow();
        } else {
            grid.set(FIELD_ACTIVITY_PERCENT, activity);
            grid.set(FIELD_CLASS_PERCENT, classes);
            grid.set(FIELD_STUDY_PERCENT, study);
            grid.set(FIELD_TOTAL_PERCENT, Double.valueOf(totalPercent));
            grid.set(FIELD_CREDITS, calculateCredits(fullSchedule));
        }
    }
}
