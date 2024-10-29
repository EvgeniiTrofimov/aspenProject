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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildStudentSchedule;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.schedule.future.StudentScheduleChangeReportHelper;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Schedule (Sheet)" report. This report is intended to be run in
 * school or build view
 * of a student's schedule. Currently not intended to be run in district or intermediate views.
 *
 * @author X2 Development Corporation
 */
public class StudentScheduleSheetData extends ReportJavaSourceNet {
    /**
     * Name for the "effective date" report parameter. The value is a PlainDate.
     */
    public static final String EFFECTIVE_DATE_PARAM = "effectiveDate";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "schedule sort" report parameter. The value is a String.
     */
    public static final String SCHEDULE_SORT_PARAM = "scheduleSort";

    /**
     * Name for the "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    // Grid fields
    public static final String FIELD_SCHEDULE_DISPLAY = "scheduleDisplay";
    public static final String FIELD_SECTION = "section";
    public static final String FIELD_STUDENT = "student";
    public static final String FIELD_TERM_VIEW = "termView";

    private static final long serialVersionUID = 1L;

    private ScheduleReportHelper m_reportHelper;

    /**
     * Appends values needed on the jrxml to the grid. A grid is used when there are student's with
     * pending StudentScheduleChange
     * records.
     *
     * @param grid DataGrid
     * @param studentSchedules Collection<StudentSchedule>
     */
    protected void appendValuesToGrid(DataGrid grid, Collection<StudentSchedule> studentSchedules) {
        grid.beforeTop();

        for (StudentSection studentSection : studentSchedules) {
            grid.append();
            grid.set(FIELD_STUDENT, studentSection.getStudent());
            grid.set(FIELD_SCHEDULE_DISPLAY, studentSection.getScheduleDisplay());
            grid.set(FIELD_SECTION, studentSection.getSection());
            grid.set(FIELD_TERM_VIEW, studentSection.getTermView());
        }

        grid.beforeTop();
    }

    /**
     * Builds a sorted grid of StudentSchedule records merged with pending StudentScheduleChange
     * records.
     *
     * @param effectiveDate PlainDate
     * @param query QueryByCriteria
     * @param scheduleSort String
     * @return JRDataSource
     */
    protected ReportDataGrid buildGridWithPendingStudentSchedules(PlainDate effectiveDate,
                                                                  QueryByCriteria query,
                                                                  String scheduleSort) {
        ReportDataGrid grid = new ReportDataGrid();

        Schedule schedule = (Schedule) getBroker().getBeanByOid(Schedule.class, m_reportHelper.getScheduleOid());

        // Ensure effective date is now or later and within the bounds of the schedules start and
        // end dates.
        effectiveDate = StudentScheduleChangeReportHelper.validateEffectiveDate(effectiveDate, schedule, getTimeZone());

        Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        String effectiveDateString = converter.javaToString(effectiveDate);
        addParameter(EFFECTIVE_DATE_PARAM, effectiveDateString);

        // Calling this method in the SSCRP rather than the broker is important for maintaining the
        // order of the hashmap for displaying results in the report.
        Map<String, Collection<StudentSchedule>> studentSchedules =
                StudentScheduleChangeReportHelper.getGroupedCollectionByQuery(query,
                        new String[] {StudentSection.COL_STUDENT_OID},
                        new int[] {256},
                        getBroker());

        StudentScheduleChangeReportHelper sscHelper =
                new StudentScheduleChangeReportHelper((X2Criteria) query.getCriteria(),
                        effectiveDate,
                        m_reportHelper.getScheduleOid(),
                        null, // school oid
                        getBroker());

        Map<String, Map<StudentSchedule, StudentScheduleChange>> pendingStudentSchedules =
                sscHelper.getPendingStudentSchedules();

        Collection<StudentSchedule> mergedStudentScheduleRecords = sscHelper.mergeStudentScheduleRecords(
                studentSchedules, pendingStudentSchedules, getUserSortFields(scheduleSort));

        appendValuesToGrid(grid, mergedStudentScheduleRecords);

        return grid;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria criteria = new X2Criteria();
        JRDataSource dataSource = null;

        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentSection.COL_STUDENT_OID, m_reportHelper.getStudentOid());
            criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

            /*
             * Set the school associated with the student
             */
            addParameter(SCHOOLNAME_KEY, getSchool().getName());
            addParameter(SCHOOL_KEY, getSchool());

            /*
             * Update the criteria to include schedules from other schools.
             */
            addStudentScheduleFromOtherSchool(criteria, m_reportHelper.getStudentOid());
        } else {
            /*
             * Build the schedule criteria based on the school's current schedule and the optional
             * student filters from the user input.
             */
            criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            /*
             * Update the criteria to include schedules from other schools.
             */
            addStudentScheduleFromOtherSchool(criteria, m_reportHelper.getStudentOid());

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, StudentSection.COL_STUDENT_OID);
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        /*
         * Build the sort based on user input.
         */
        String sort = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(query, sort);

        /*
         * The student sort order always has to end with the student OID so that schedules are
         * grouped properly.
         */
        query.addOrderByAscending(StudentSection.COL_STUDENT_OID);

        /*
         * Within each student we can order schedules by either course view, schedule view, or term.
         */
        String scheduleSort = (String) getParameter(SCHEDULE_SORT_PARAM);
        applyUserSort(query, scheduleSort);

        /*
         * If we have an effective date we need to merge pending StudentScheduleChange records that
         * would be effective as of the date
         * selected with StudentSchedule records.
         */
        PlainDate effectiveDate = (PlainDate) getParameter(EFFECTIVE_DATE_PARAM);

        if (effectiveDate != null && isSchoolContext()
                && !(m_reportHelper.getStudentSectionClass().equals(BuildStudentSchedule.class))) {
            dataSource = buildGridWithPendingStudentSchedules(effectiveDate, query, scheduleSort);
        } else {
            dataSource = new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
            addParameter(EFFECTIVE_DATE_PARAM, "N/A");
        }

        return dataSource;
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
     * Adds criteria to get student schedule from other schools for the same context year.
     * This method will be only called in School view. In Build view, it is impossible to identify
     * the
     * student schedules from other schools since there can be multiple scenarios for the same
     * context
     * year.
     *
     * @param existingCriteria Criteria
     * @param studentOid The value can be null which means the criteria applies to all students
     */
    private void addStudentScheduleFromOtherSchool(Criteria existingCriteria, String studentOid) {
        if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
            /*
             * Retrive other active schedule oids other than the current one for the same context
             * year.
             */
            X2Criteria otherActiveScheduleOidCriteria = new X2Criteria();
            otherActiveScheduleOidCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_reportHelper.getSchedule().getDistrictContextOid());
            otherActiveScheduleOidCriteria.addNotEmpty(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    getBroker().getPersistenceKey());
            otherActiveScheduleOidCriteria.addNotEqualTo(SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    m_reportHelper.getSchedule().getOid());

            SubQuery otherActiveScheduleOidQuery = new SubQuery(SchoolScheduleContext.class,
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, otherActiveScheduleOidCriteria);
            Collection otherScheduleOids = getBroker().getSubQueryCollectionByQuery(otherActiveScheduleOidQuery);
            if (!otherScheduleOids.isEmpty()) {
                Criteria scheduleFromOtherSchoolCriteria = new Criteria();
                scheduleFromOtherSchoolCriteria.addEqualTo(
                        StudentSchedule.REL_STUDENT + "." + SisStudent.COL_SCHOOL_OID, getSchool().getOid());
                scheduleFromOtherSchoolCriteria.addIn(StudentSchedule.COL_SCHEDULE_OID, otherScheduleOids);
                if (!StringUtils.isEmpty(studentOid)) {
                    scheduleFromOtherSchoolCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, studentOid);
                }

                existingCriteria.addOrCriteria(scheduleFromOtherSchoolCriteria);
            }
        }
    }
}
