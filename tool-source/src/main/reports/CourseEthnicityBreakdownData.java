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
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.MasterTerm;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentSection;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Course Ethnicity Breakdown report. This report presents a summary of
 * the student's ethnicity information that are enrolled in each section in the master schedule.
 * The exact sections included in the report can be specified with input parameters.
 *
 * @author X2 Development Corporation
 */
public class CourseEthnicityBreakdownData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "course grouping" report parameter. The value is a Boolean.
     */
    public static final String COURSE_GROUP_PARAM = "courseGroup";

    /**
     * Name for the "includeStudyClass" report parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDY_CLASS = "includeStudyClass";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Subreport constants
     */
    private static final String ETHNICITY_PARAMTERS = "ethnicityParameters";
    private static final String HISPANIC_PARAMTERS = "hispanicParameters";
    private static final String SUBREPORT_BEAN = "subreport";
    private static final String SUBREPORT_DATASOURCES = "subreportDataSources";
    private static final String SUBREPORT_ID = "SYS-SCH-014-SUB";

    /*
     * Grid field constants.
     */
    private static final String FIELD_CATEGORY = "category";
    private static final String FIELD_FEMALES = "females";
    private static final String FIELD_GROUP_OID = "groupOid";
    private static final String FIELD_GROUP_NAME = "groupName";
    private static final String FIELD_GROUP_DESCRIPTION = "groupDescription";
    private static final String FIELD_MALES = "males";
    private static final String FIELD_TOTAL = "total";

    /*
     * Report constants
     */
    private static final String HISPANIC = "Hispanic";
    private static final String NON_HISPANIC = "Non-Hispanic";
    private static final String ETHNICITY = "Ethnicity";
    private static final String MALE = "M";

    private boolean m_courseGroup;
    private DataDictionary m_dictionary;
    private Map<String, Collection<String>> m_personRaces;
    private ScheduleReportHelper m_reportHelper;
    private Map<String, Collection<X2BaseBean>> m_rosters;
    private Report m_subreport;
    private Map<String, ReportDataGrid> m_subreportDataSources;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();
        m_dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        m_subreportDataSources = new HashMap<String, ReportDataGrid>(10);
        m_subreport = getSubreport();

        m_courseGroup = ((Boolean) getParameter(COURSE_GROUP_PARAM)).booleanValue();

        if (!StringUtils.isEmpty(m_reportHelper.getSectionOid())) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_reportHelper.getSectionOid());
        } else {
            /*
             * Build the schedule criteria based on the school's current schedule and the optional
             * section filters from the user input.
             */
            criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

            /*
             * Exclude study class when required.
             */
            boolean includeStudyClass = ((Boolean) getParameter(INCLUDE_STUDY_CLASS)).booleanValue();
            if (!includeStudyClass) {
                criteria.addNotEqualTo(Section.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                        SchoolCourse.MASTER_TYPE_STUDY);
            }

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            if ("term".equals(queryBy)) {
                Criteria termCriteria = new Criteria();
                termCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, m_reportHelper.getTimeScheduleOid());
                termCriteria.addEqualTo(ScheduleTerm.COL_CODE, queryString);

                QueryByCriteria termQuery = new QueryByCriteria(ScheduleTerm.class, termCriteria);
                ScheduleTerm term = (ScheduleTerm) getBroker().getBeanByQuery(termQuery);

                ScheduleManager scheduleManager = new ScheduleManager(getBroker());

                if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
                    Collection coveredTerms = scheduleManager.getCoveredTermOids(term);
                    criteria.addIn(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.COL_SCHEDULE_TERM_OID,
                            coveredTerms);
                } else if (m_reportHelper.getSectionClass().equals(BuildMasterSchedule.class)) {
                    Collection coveredTerms = scheduleManager.getCoveredTermMaps(term);
                    criteria.addIn(BuildMasterSchedule.COL_TERM_MAP, coveredTerms);
                }
            } else {
                addUserCriteria(criteria, queryBy, queryString, null, null);
            }
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getSectionClass(), criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        ReportDataGrid grid = new ReportDataGrid(10000, 15);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SchoolCourse lastCourse = null;
            loadRosters(criteria);
            loadStudentRaces(criteria);

            while (iterator.hasNext()) {
                X2BaseBean group = (X2BaseBean) iterator.next();

                Collection studentSchedules = null;
                if (m_courseGroup) {
                    SchoolCourse course = ((Section) group).getSchoolCourse();
                    studentSchedules = m_rosters.get(course.getOid());

                    if (lastCourse == null || !course.equals(lastCourse)) {
                        grid.append();
                        grid.set(FIELD_GROUP_OID, course.getOid());
                        grid.set(FIELD_GROUP_NAME, course.getNumber());
                        grid.set(FIELD_GROUP_DESCRIPTION, course.getDescription());
                    }

                    lastCourse = course;
                    group = course;
                } else {
                    String name = ((Section) group).getCourseView();
                    String description = ((Section) group).getDescription();

                    grid.append();
                    grid.set(FIELD_GROUP_OID, group.getOid());
                    grid.set(FIELD_GROUP_NAME, name);
                    grid.set(FIELD_GROUP_DESCRIPTION, description);

                    studentSchedules = m_rosters.get(group.getOid());
                }

                if (studentSchedules == null) {
                    studentSchedules = new ArrayList(0);
                }
                addHispanicData(group, studentSchedules);
                addEthnicData(group, studentSchedules);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();

        /*
         * Add report parameters.
         */
        HashMap eParameters = new HashMap(10);
        HashMap hParameters = new HashMap(10);
        hParameters.put("includeTotals", Boolean.TRUE);
        addParameter(HISPANIC_PARAMTERS, hParameters);

        eParameters.put("includeTotals", Boolean.FALSE);
        addParameter(ETHNICITY_PARAMTERS, eParameters);

        addParameter(SUBREPORT_BEAN, m_subreport);
        addParameter(SUBREPORT_DATASOURCES, m_subreportDataSources);

        return grid;
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
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Calculates totals of students' hispanic data of the passed class and roster.
     *
     * @param group X2BaseBean
     * @param studentSchedules Collection
     */
    private void addEthnicData(X2BaseBean group, Collection studentSchedules) {
        ReportDataGrid summary = getEthnicSummaryGrid(studentSchedules);
        m_subreportDataSources.put(group.getOid() + ETHNICITY, summary);
    }

    /**
     * Calculates totals of students' ethnicity data of the passed class and roster.
     *
     * @param group X2BaseBean
     * @param studentSchedules Collection
     */
    private void addHispanicData(X2BaseBean group, Collection studentSchedules) {
        ReportDataGrid summary = getHispanicSummaryGrid(studentSchedules);
        m_subreportDataSources.put(group.getOid() + HISPANIC, summary);
    }

    /**
     * Returns a collection of the available ethnic codes from the reference table used in
     * Race.RaceCode.
     *
     * @return A Collection of ReferenceCode beans
     */
    private Collection<ReferenceCode> getEthnicCodes() {
        DataDictionaryField field = m_dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
        return field.getReferenceTable() != null ? field.getReferenceTable().getReferenceCodes() : new ArrayList(0);
    }

    /**
     * Returns a grid of the ethnic information for the passed collection of students.
     *
     * @param studentSchedules Collection
     * @return ReportDataGrid
     */
    private ReportDataGrid getEthnicSummaryGrid(Collection studentSchedules) {
        ReportDataGrid grid = new ReportDataGrid(100, 5);

        for (ReferenceCode referenceCode : getEthnicCodes()) {
            String code = referenceCode.getCode();

            grid.append();
            grid.set(FIELD_CATEGORY, code);

            grid.set(FIELD_TOTAL, Integer.valueOf(0));
            grid.set(FIELD_MALES, Integer.valueOf(0));
            grid.set(FIELD_FEMALES, Integer.valueOf(0));

            Iterator studentIterator = studentSchedules.iterator();
            while (studentIterator.hasNext()) {
                StudentSection studentSection = (StudentSection) studentIterator.next();
                SisStudent student = studentSection.getStudent();

                Collection personEthnicity = m_personRaces.get(student.getPersonOid());
                if (personEthnicity != null && personEthnicity.contains(code)) {
                    // Increase overall total
                    int total = ((Integer) grid.get(FIELD_TOTAL)).intValue() + 1;
                    grid.set(FIELD_TOTAL, Integer.valueOf(total));

                    // Increase gender totals
                    if (student.getPerson().getGenderCode().equals(MALE)) {
                        total = ((Integer) grid.get(FIELD_MALES)).intValue() + 1;
                        grid.set(FIELD_MALES, Integer.valueOf(total));
                    } else {
                        total = ((Integer) grid.get(FIELD_FEMALES)).intValue() + 1;
                        grid.set(FIELD_FEMALES, Integer.valueOf(total));
                    }
                }
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns a gird containing the hispanic information for the passed students.
     *
     * @param studentSchedules Collection
     * @return ReportDataGrid
     */
    private ReportDataGrid getHispanicSummaryGrid(Collection studentSchedules) {
        ReportDataGrid grid = new ReportDataGrid(100, 4);

        int hispanicTotal = 0;
        int hispanicMales = 0;
        int hispanicFemales = 0;

        int nonTotal = 0;
        int nonMales = 0;
        int nonFemales = 0;

        Iterator studentIterator = studentSchedules.iterator();
        while (studentIterator.hasNext()) {
            StudentSection studentSection = (StudentSection) studentIterator.next();
            SisStudent student = studentSection.getStudent();

            if (student.getPerson().getHispanicLatinoIndicator()) {
                hispanicTotal++;

                if (MALE.equals(student.getPerson().getGenderCode())) {
                    hispanicMales++;
                } else {
                    hispanicFemales++;
                }
            } else {
                nonTotal++;

                if (MALE.equals(student.getPerson().getGenderCode())) {
                    nonMales++;
                } else {
                    nonFemales++;
                }
            }
        }

        // Set grid
        grid.append();
        grid.set(FIELD_CATEGORY, HISPANIC);
        grid.set(FIELD_TOTAL, Integer.valueOf(hispanicTotal));
        grid.set(FIELD_MALES, Integer.valueOf(hispanicMales));
        grid.set(FIELD_FEMALES, Integer.valueOf(hispanicFemales));

        grid.append();
        grid.set(FIELD_CATEGORY, NON_HISPANIC);
        grid.set(FIELD_TOTAL, Integer.valueOf(nonTotal));
        grid.set(FIELD_MALES, Integer.valueOf(nonMales));
        grid.set(FIELD_FEMALES, Integer.valueOf(nonFemales));

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the subreport bean.
     *
     * @return Report
     */
    private Report getSubreport() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        return (Report) getBroker().getBeanByQuery(query);
    }

    /**
     * Populates a Map of Collections of StudentSchedules keyed to their
     * MasterSchedule/BuildMasterSchedule Oid.
     *
     * @param masterCriteria The criteria for the master schedules we are running the report for.
     */
    private void loadRosters(Criteria masterCriteria) {
        Criteria criteria = new Criteria();
        criteria.addIn(StudentSection.COL_SECTION_OID,
                new SubQuery(m_reportHelper.getSectionClass(), X2BaseBean.COL_OID, masterCriteria));

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        if (m_courseGroup) {
            m_rosters = getBroker().getGroupedCollectionByQuery(query,
                    StudentSection.REL_SECTION + PATH_DELIMITER + Section.COL_SCHOOL_COURSE_OID, 500);
        } else {
            m_rosters = getBroker().getGroupedCollectionByQuery(query, StudentSection.COL_SECTION_OID, 500);
        }
    }

    /**
     * Populates the map of person OIDs to race code collections.
     *
     * @param masterCriteria The criteria for the master schedules we are running the report for.
     */
    private void loadStudentRaces(Criteria masterCriteria) {
        /*
         * Build the query
         */
        Criteria subCriteria = new Criteria();
        subCriteria.addIn(StudentSchedule.COL_SECTION_OID,
                new SubQuery(m_reportHelper.getSectionClass(), X2BaseBean.COL_OID, masterCriteria));

        SubQuery subQuery = new SubQuery(m_reportHelper.getStudentSectionClass(),
                StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_PERSON_OID, subCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(Race.COL_PERSON_OID, subQuery);

        QueryByCriteria query = new QueryByCriteria(Race.class, criteria);

        QueryIterator raceBeans = getBroker().getIteratorByQuery(query);

        /*
         * Load the results
         */
        try {
            m_personRaces = new HashMap<String, Collection<String>>(4096);
            while (raceBeans.hasNext()) {
                Race raceBean = (Race) raceBeans.next();

                Collection<String> races = m_personRaces.get(raceBean.getPersonOid());
                if (races == null) {
                    races = new ArrayList<String>(5);
                    m_personRaces.put(raceBean.getPersonOid(), races);
                }

                races.add(raceBean.getRaceCode());
            }
        } finally {
            raceBeans.close();
        }
    }
}
