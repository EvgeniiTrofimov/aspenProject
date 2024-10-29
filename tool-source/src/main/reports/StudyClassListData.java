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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Study Class List" report. This report lists the students enrolled in
 * each study section for each day.
 *
 * @author X2 Development Corporation
 */
public class StudyClassListData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "schedule term OID" report parameter. The value is a String.
     */
    public static final String SCHEDULE_TERM_OID_PARAM = "scheduleTermOid";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_DAY = "day";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_STUDENT_SCHEDULE_DISPLAY = "scheduleDisplay";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_SCHEDULE_TERM_VIEW = "termView";

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(100, 6);

        if (m_reportHelper.getSchedule() != null) {
            Criteria criteria = new Criteria();
            /*
             * First query out all the study sections
             */
            criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            criteria.addEqualTo(Section.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_STUDY);

            List<String> termOids = new ArrayList<String>();
            List<String> termMaps = new ArrayList<String>();

            String scheduleTermOid = (String) getParameter(SCHEDULE_TERM_OID_PARAM);
            if (!StringUtils.isEmpty(scheduleTermOid)) {
                ScheduleManager scheduleManager = new ScheduleManager(getBroker());

                ScheduleTerm scheduleTerm =
                        (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, scheduleTermOid);

                Class studentSectionClass = m_reportHelper.getStudentSectionClass();

                if (studentSectionClass.equals(StudentSchedule.class)) {
                    termOids.addAll(scheduleManager.getCoveredTermOids(scheduleTerm));
                    termOids.addAll(scheduleManager.getCoveringTermOids(scheduleTerm));
                } else {
                    termMaps.addAll(scheduleManager.getCoveredTermMaps(scheduleTerm));
                    termMaps.addAll(scheduleManager.getCoveringTermMaps(scheduleTerm));
                }

                if (!termOids.isEmpty()) {
                    criteria.addIn(Section.COL_SCHEDULE_TERM_OID, termOids);
                }
            }

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, m_reportHelper.getStudentSectionClass(),
                    StudentSection.COL_SECTION_OID);

            QueryByCriteria query = new QueryByCriteria(m_reportHelper.getSectionClass(), criteria);
            /*
             * Build the sort based on user input.
             */
            String sortBy = (String) getParameter(SORT_PARAM);
            applyUserSort(query, sortBy);

            QueryIterator allStudySections = getBroker().getIteratorByQuery(query);
            try {
                while (allStudySections.hasNext()) {
                    X2BaseBean studySection = (X2BaseBean) allStudySections.next();

                    Iterator dayIt = m_reportHelper.getSchedule().getScheduleDays().iterator();
                    while (dayIt.hasNext()) {
                        ScheduleDay day = (ScheduleDay) dayIt.next();

                        Iterator studentSchedules =
                                getStudentSchedulesForDay(studySection.getOid(), day, termOids, termMaps).iterator();
                        while (studentSchedules.hasNext()) {
                            StudentSection studentSection = (StudentSection) studentSchedules.next();

                            SisStudent student = studentSection.getStudent();
                            String termView = studentSection.getTermView();
                            String scheduleDisplay = studentSection.getScheduleDisplay();

                            grid.append();
                            grid.set(FIELD_DAY, day);
                            grid.set(FIELD_SECTION, studySection);
                            grid.set(FIELD_STUDENT, student);
                            grid.set(FIELD_STUDENT_SCHEDULE_TERM_VIEW, termView);
                            grid.set(FIELD_STUDENT_SCHEDULE_DISPLAY, scheduleDisplay);
                        }
                    }
                }
            } finally {
                allStudySections.close();
            }

            grid.beforeTop();
        }

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
     * Returns the list of student schedules for the passed section and day.
     *
     * @param sectionOid String
     * @param day ScheduleDay
     * @param coveringTerms Collection
     * @param coveringTermMaps Collection<String>
     * @return Collection Of StudentSchedule or BuildStudentSchedule objects.
     */
    private Collection getStudentSchedulesForDay(String sectionOid,
                                                 ScheduleDay day,
                                                 Collection coveringTerms,
                                                 Collection<String> coveringTermMaps) {
        Class studentSectionClass = m_reportHelper.getStudentSectionClass();

        Criteria studentScheduleCriteria = new Criteria();
        studentScheduleCriteria.addEqualTo(StudentSection.COL_SECTION_OID, sectionOid);

        if (studentSectionClass.equals(StudentSchedule.class)) {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER +
                    StudentScheduleTerm.REL_STUDENT_SCHEDULE_MATRICES + PATH_DELIMITER +
                    StudentScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER + ScheduleMatrix.COL_SCHEDULE_DAY_OID,
                    day.getOid());

            if (coveringTerms != null && !coveringTerms.isEmpty()) {
                studentScheduleCriteria.addIn(StudentSchedule.REL_STUDENT_SCHEDULE_TERMS + PATH_DELIMITER +
                        StudentScheduleTerm.COL_SCHEDULE_TERM_OID, coveringTerms);
            }
        } else if (studentSectionClass.equals(BuildStudentSchedule.class)) {
            if (coveringTermMaps != null && !coveringTermMaps.isEmpty()) {
                studentScheduleCriteria.addIn(BuildStudentSchedule.COL_TERM_MAP, coveringTermMaps);
            }
        }

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(studentSectionClass, studentScheduleCriteria, true);
        studentScheduleQuery
                .addOrderByAscending(StudentSection.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);

        Collection studentSchedules = new ArrayList();
        if (studentSectionClass.equals(StudentSchedule.class)) {
            studentSchedules = getBroker().getCollectionByQuery(studentScheduleQuery);
        } else if (studentSectionClass.equals(BuildStudentSchedule.class)) {
            QueryIterator studentScheduleIterator = getBroker().getIteratorByQuery(studentScheduleQuery);
            try {
                while (studentScheduleIterator.hasNext()) {
                    BuildStudentSchedule studentSchedule = (BuildStudentSchedule) studentScheduleIterator.next();
                    ScheduleMap map = studentSchedule.getScheduleMap();

                    if (map != null && map.isScheduledForDay(day.getNumber())) {
                        studentSchedules.add(studentSchedule);
                    }
                }
            } finally {
                studentScheduleIterator.close();
            }
        }

        return studentSchedules;
    }
}
