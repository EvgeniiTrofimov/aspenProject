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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.GradeLevelHistory;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Graduation Statistic report. This is a two-page report that prints
 * the graduation statistics for a YOG on the first part, then the drop-out details on the second.
 *
 * @author X2 Development Corporation
 */
public class GraduationStatisticsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "dropout status" input parameters. This value is a String.
     */
    public static final String DROPOUT_STATUS_PARAM = "dropoutStatus";

    /**
     * Name for the "graduate status" input parameters. This value is a String.
     */
    public static final String GRADUATE_STATUS_PARAM = "graduateStatus";

    /**
     * Name for the "include details" input parameter. This value is a Boolean.
     */
    public static final String INCLUDE_DETAILS_PARAM = "includeDetails";

    /**
     * Name for the "include statistics" input parameter. This value is a Boolean.
     */
    public static final String INCLUDE_STATS_PARAM = "includeStats";

    /**
     * Name for the "year list" report parameter. This value is a Collection.
     */
    public static final String PARAM_YEAR_LIST = "yearList";

    /**
     * Name for the "YOG" input parameter. This value is an Integer.
     */
    public static final String YOG_PARAM = "yog";

    /*
     * Main grid fields
     */
    private static final String FIELD_DATASOURCE = "datasource";
    private static final String FIELD_FORMAT = "format";

    /*
     * Subreport IDs
     */
    private static final String DETAIL_REPORT_ID = "SYS-ADM-013-DETAILS";
    private static final String STATISTIC_REPORT_ID = "SYS-ADM-013-STATS";

    /*
     * Statistic Grid fields
     */
    private static final String FIELD_ACTIVE_COUNT = "activeCount";
    private static final String FIELD_DROPOUT_CURRENT = "dropoutCurrent";
    private static final String FIELD_DROPOUT_ONE = "dropoutOne";
    private static final String FIELD_DROPOUT_TWO = "dropoutTwo";
    private static final String FIELD_DROPOUT_THREE = "dropoutThree";
    private static final String FIELD_SCHOOL = "school";

    /*
     * Detail Grid fields
     */
    private static final String FIELD_ENROLLMENT_RECORD = "enrollment";
    private static final String FIELD_GRADE = "grade";

    private Map m_activeStudentMap;
    private DistrictSchoolYearContext m_baseContext;
    private int m_cutoffYear;
    private ReportDataGrid m_detailGrid;
    private Map m_dropoutMapCurrent;
    private Map m_dropoutMapOne;
    private Map m_dropoutMapTwo;
    private Map m_dropoutMapThree;
    private String m_dropoutStatus;
    private Map m_gradeHistory;
    private String m_graduateStatus;
    private boolean m_includeDetails;
    private boolean m_includeStats;
    private ReportDataGrid m_statisticGrid;
    private ArrayList m_yearList;
    private int m_yog;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.reporting.ReportDataSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid pageGrid = new ReportDataGrid(2, 5);
        m_detailGrid = new ReportDataGrid(5);
        m_statisticGrid = new ReportDataGrid(5);

        if (m_includeDetails || m_includeStats) {
            if (m_includeStats) {
                loadActiveGraduated();
            }

            loadSchoolYears();
            loadDropoutStudents(); // Must be run after loadSchoolYears

            Criteria schoolCriteria = new Criteria();
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            if (isSchoolContext()) {
                schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            } else {
                schoolCriteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schoolQuery.addOrderByAscending(X2BaseBean.COL_OID);

            QueryIterator iterator = getBroker().getIteratorByQuery(schoolQuery);
            try {
                while (iterator.hasNext()) {
                    SisSchool school = (SisSchool) iterator.next();

                    if (m_includeStats) {
                        buildStatisticData(school);
                    }

                    if (m_includeDetails) {
                        buildDetailData(school);
                    }
                }
            } finally {
                iterator.close();
            }

            Report subReport;

            if (m_includeStats) {
                pageGrid.append();
                subReport = ReportUtils.getReport(STATISTIC_REPORT_ID, getBroker());
                pageGrid.set(FIELD_FORMAT, new ByteArrayInputStream(subReport.getCompiledFormat()));
                pageGrid.set(FIELD_DATASOURCE, m_statisticGrid);
            }

            if (m_includeDetails) {
                pageGrid.append();
                subReport = ReportUtils.getReport(DETAIL_REPORT_ID, getBroker());
                pageGrid.set(FIELD_FORMAT, new ByteArrayInputStream(subReport.getCompiledFormat()));
                pageGrid.set(FIELD_DATASOURCE, m_detailGrid);
            }
        }

        /*
         * Add parameters
         */
        addParameter(PARAM_YEAR_LIST, m_yearList);
        addParameter(YOG_PARAM, String.valueOf(m_yog));

        pageGrid.beforeTop();
        return pageGrid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see @see
     *      com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_yog = ((Integer) getParameter(YOG_PARAM)).intValue();
        m_dropoutStatus = (String) getParameter(DROPOUT_STATUS_PARAM);
        m_graduateStatus = (String) getParameter(GRADUATE_STATUS_PARAM);
        m_includeDetails = ((Boolean) getParameter(INCLUDE_DETAILS_PARAM)).booleanValue();
        m_includeStats = ((Boolean) getParameter(INCLUDE_STATS_PARAM)).booleanValue();
        m_gradeHistory = new HashMap(1024);

        if (m_yog < getCurrentContext().getSchoolYear()) {
            Criteria contextCriteria = new Criteria();
            contextCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, String.valueOf(m_yog));
            QueryByCriteria contextQuery = new QueryByCriteria(DistrictSchoolYearContext.class, contextCriteria);
            m_baseContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(contextQuery);
        } else {
            m_baseContext = getCurrentContext();
        }

        m_cutoffYear = m_baseContext.getSchoolYear() + (m_yog - m_baseContext.getSchoolYear()) - 3;
    }

    /**
     * Sets the given set of enrollment records into the Detail Grid.
     *
     * @param enrollments Collection
     * @param context DistrictSchoolYearContext
     */
    private void appendEnrollments(Collection enrollments, DistrictSchoolYearContext context) {
        if (enrollments != null) {
            Iterator enrollmentIterator = enrollments.iterator();

            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();

                m_detailGrid.append();
                m_detailGrid.set(FIELD_ENROLLMENT_RECORD, enrollment);

                GradeLevelHistory history = (GradeLevelHistory) m_gradeHistory.get(enrollment.getStudentOid());
                if (history == null) {
                    history = new GradeLevelHistory(enrollment.getStudentOid(),
                            enrollment.getSchool().getNumberOfGrades(), getOrganization(), getBroker());
                    m_gradeHistory.put(enrollment.getStudentOid(), history);
                }
                m_detailGrid.set(FIELD_GRADE, history.getGradeLevel(enrollment.getStudentOid(),
                        context.getSchoolYear()));
            }
        }
    }

    /**
     * Constructs the data to populate the Details portion of the report.
     *
     * @param school SisSchool
     */
    private void buildDetailData(SisSchool school) {
        DistrictSchoolYearContext context = (DistrictSchoolYearContext) m_yearList.get(3);
        Collection enrollments = (Collection) m_dropoutMapThree.get(school.getOid() + context.getOid());
        appendEnrollments(enrollments, context);

        context = (DistrictSchoolYearContext) m_yearList.get(2);
        enrollments = (Collection) m_dropoutMapTwo.get(school.getOid() + context.getOid());
        appendEnrollments(enrollments, context);

        context = (DistrictSchoolYearContext) m_yearList.get(1);
        enrollments = (Collection) m_dropoutMapOne.get(school.getOid() + context.getOid());
        appendEnrollments(enrollments, context);

        context = (DistrictSchoolYearContext) m_yearList.get(0);
        enrollments = (Collection) m_dropoutMapCurrent.get(school.getOid() + context.getOid());
        appendEnrollments(enrollments, context);
    }

    /**
     * Constructs the data to populate the Statistics portion of the report.
     *
     * @param school SisSchool
     */
    private void buildStatisticData(SisSchool school) {
        m_statisticGrid.append();
        m_statisticGrid.set(FIELD_SCHOOL, school);

        Long dropoutCountCurrent;
        Long dropoutCountOne;
        Long dropoutCountTwo;
        Long dropoutCountThree;

        Long activeCount = (Long) m_activeStudentMap.get(school.getOid());

        Collection dropoutsCurrent = (Collection) m_dropoutMapCurrent.get(school.getOid() +
                ((DistrictSchoolYearContext) m_yearList.get(0)).getOid());

        Collection dropoutsOne = (Collection) m_dropoutMapOne.get(school.getOid() +
                ((DistrictSchoolYearContext) m_yearList.get(1)).getOid());

        Collection dropoutsTwo = (Collection) m_dropoutMapTwo.get(school.getOid() +
                ((DistrictSchoolYearContext) m_yearList.get(2)).getOid());

        Collection dropoutsThree = (Collection) m_dropoutMapThree.get(school.getOid() +
                ((DistrictSchoolYearContext) m_yearList.get(3)).getOid());

        if (activeCount == null) {
            activeCount = Long.valueOf(0);
        }

        dropoutCountCurrent = countStudents(dropoutsCurrent);
        dropoutCountOne = countStudents(dropoutsOne);
        dropoutCountTwo = countStudents(dropoutsTwo);
        dropoutCountThree = countStudents(dropoutsThree);

        m_statisticGrid.set(FIELD_ACTIVE_COUNT, activeCount);
        m_statisticGrid.set(FIELD_DROPOUT_CURRENT, dropoutCountCurrent);
        m_statisticGrid.set(FIELD_DROPOUT_ONE, dropoutCountOne);
        m_statisticGrid.set(FIELD_DROPOUT_TWO, dropoutCountTwo);
        m_statisticGrid.set(FIELD_DROPOUT_THREE, dropoutCountThree);
    }

    /**
     * Returns the number of individual students based on the passed enrollment records.
     *
     * @param enrollments Collection
     * @return Long
     */
    private Long countStudents(Collection enrollments) {
        int students = 0;

        if (enrollments != null) {
            Iterator enrollmentIterator = enrollments.iterator();
            SisStudent lastStudent = null;

            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                SisStudent student = enrollment.getStudent();

                if (lastStudent == null || !student.equals(lastStudent)) {
                    students++;
                }

                lastStudent = student;
            }
        }

        return Long.valueOf(students);
    }



    /**
     * Puts the number of active or graduated students into a map keyed to the School OID.
     */
    private void loadActiveGraduated() {
        /*
         * Load active students
         */
        m_activeStudentMap = new HashMap(128);

        Criteria activeCriteria = new Criteria();
        activeCriteria.addEqualTo(SisStudent.COL_YOG, String.valueOf(m_yog));
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        /*
         * Build query
         */
        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, "count(*)"};
        ReportQueryByCriteria activeQuery =
                new ReportQueryByCriteria(SisStudent.class, columns, activeCriteria);

        activeQuery.addGroupBy(SisStudent.COL_SCHOOL_OID);
        activeQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(activeQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String schoolOid = (String) row[0];
                String countString = row[1].toString();
                Long count = Long.valueOf(countString);

                m_activeStudentMap.put(schoolOid, count);
            }
        } finally {
            iterator.close();
        }

        /*
         * Load graduated students
         */
        X2Criteria gradCriteria = new X2Criteria();
        gradCriteria.addEqualTo(StudentEnrollment.COL_YOG, String.valueOf(m_yog));
        gradCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_baseContext.getStartDate());
        gradCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_baseContext.getEndDate());
        gradCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, m_graduateStatus);

        /*
         * Build query
         */
        /*
         * Build query
         */
        columns = new String[] {StudentEnrollment.COL_SCHOOL_OID, "count(*)"};
        ReportQueryByCriteria gradQuery =
                new ReportQueryByCriteria(StudentEnrollment.class, columns, gradCriteria);

        gradQuery.addGroupBy(StudentEnrollment.COL_SCHOOL_OID);
        gradQuery.addOrderByAscending(StudentEnrollment.COL_SCHOOL_OID);

        ReportQueryIterator gradIterator = getBroker().getReportQueryIteratorByQuery(gradQuery);
        try {
            while (gradIterator.hasNext()) {
                Object[] row = (Object[]) gradIterator.next();

                String schoolOid = (String) row[0];
                String countString = row[1].toString();
                Long count = Long.valueOf(countString);

                Long previousCount = (Long) m_activeStudentMap.get(schoolOid);
                if (previousCount == null) {
                    previousCount = Long.valueOf(0);
                    m_activeStudentMap.put(schoolOid, previousCount);
                }

                previousCount = Long.valueOf(previousCount.intValue() + count.intValue());
            }
        } finally {
            gradIterator.close();
        }
    }

    /**
     * Returns a map of the number of students who dropped out during the passed school year context
     * keyed to a combination of the school OID and the context OID.
     *
     * If the school year of the passed context is outside of the cutoff (ie: for YOG = 2009
     * (freshman)
     * the cutoff year is 2006. Any prior year would not be included in the HS), then return an
     * empty Map.
     *
     * @param context DistrictSchoolYearContext
     *
     * @return Map
     */
    private Map loadDropoutsForYear(DistrictSchoolYearContext context) {
        Map dropOutMap = new HashMap(32);

        // Only get if given context is inside cutoff point
        if (context.getSchoolYear() >= m_cutoffYear) {
            X2Criteria dropoutCriteria = new X2Criteria();
            dropoutCriteria.addEqualTo(StudentEnrollment.COL_YOG, String.valueOf(m_yog));
            dropoutCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
            dropoutCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, m_dropoutStatus);

            dropoutCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, context.getStartDate());
            dropoutCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, context.getEndDate());

            /*
             * Build query
             */
            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, dropoutCriteria);
            query.addOrderByAscending(StudentEnrollment.COL_YOG);
            query.addOrderByAscending(StudentEnrollment.COL_SCHOOL_OID);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByAscending(StudentEnrollment.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    StudentEnrollment enrollment = (StudentEnrollment) iterator.next();
                    SisSchool school = enrollment.getSchool();

                    Collection enrollments = (Collection) dropOutMap.get(school.getOid() + context.getOid());

                    if (enrollments == null) {
                        enrollments = new LinkedList();
                        dropOutMap.put(school.getOid() + context.getOid(), enrollments);
                    }

                    enrollments.add(enrollment);
                }
            } finally {
                iterator.close();
            }
        }

        return dropOutMap;
    }

    /**
     * Loads the number of students who dropped out for each of the 4 past years into their
     * respective Maps.
     */
    private void loadDropoutStudents() {
        m_dropoutMapCurrent = loadDropoutsForYear((DistrictSchoolYearContext) m_yearList.get(0));
        m_dropoutMapOne = loadDropoutsForYear((DistrictSchoolYearContext) m_yearList.get(1));
        m_dropoutMapTwo = loadDropoutsForYear((DistrictSchoolYearContext) m_yearList.get(2));
        m_dropoutMapThree = loadDropoutsForYear((DistrictSchoolYearContext) m_yearList.get(3));
    }

    /**
     * Loads the school years used in the report. The base year and the past 3 years.
     */
    private void loadSchoolYears() {
        Criteria yearCriteria = new Criteria();
        yearCriteria.addEqualTo(DistrictSchoolYearContext.COL_ORGANIZATION1_OID, getOrganization().getOid());
        yearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                String.valueOf(m_baseContext.getSchoolYear()));
        yearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                String.valueOf(m_baseContext.getSchoolYear() - 3));

        QueryByCriteria yearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, yearCriteria);
        yearQuery.addOrderByDescending(DistrictSchoolYearContext.COL_SCHOOL_YEAR);

        m_yearList = new ArrayList(getBroker().getCollectionByQuery(yearQuery));
    }
}
