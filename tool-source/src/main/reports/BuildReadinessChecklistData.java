/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.UserAccessLog;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.SchedConstants;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.EngineHarness;
import com.x2dev.sis.model.business.schedule.EngineResults;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Build Readiness Checklist" report.
 *
 * @author X2 Development Corporation
 */
public class BuildReadinessChecklistData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // Input parameters
    private static final String SCHEDULE_OID_PARAM = "scheduleOid";

    // Report fields
    private static final String STEP_NAME_FIELD = "stepName";
    private static final String STEP_NOTES_FIELD = "stepNotes";
    private static final String STEP_STATUS_FIELD = "stepStatus";

    // Report parameters
    private static final String OUTCOME_PARAM = "outcome";
    private static final String PROGRESS_PARAM = "progress";
    private static final String SCENARIO_PARAM = "scenario";

    // Step names
    private static final String STEP_01_BUILD_YEAR_DEFINED = "1. Define the build year context";
    private static final String STEP_02_DISTRICT_CATALOG_SET_UP =
            "2. Set up the district course catalog for the build year";
    private static final String STEP_03_SCHOOL_BUILD_YEAR_SET = "3. Assign the build year to the school";
    private static final String STEP_04_SCHOOL_CATALOG_SET_UP =
            "4. Set up the school course catalog for the build year";
    private static final String STEP_05_STUDENT_REQUESTS_ENTERED = "5. Enter student requests";
    private static final String STEP_06_SCENARIO_CREATED = "6. Create a schedule scenario in the build view";
    private static final String STEP_07_TIME_STRUCTURE_SET_UP = "7. Create terms, days, and periods";
    private static final String STEP_08_PATTERNS_CREATED = "8. Create patterns and pattern sets";
    private static final String STEP_09_ATTRIBUTES_SET = "9. Set student, staff, course, and room attributes";
    private static final String STEP_10_REQUESTS_ANALYZED = "10. Analyze student course requests";
    private static final String STEP_11_SECTIONS_INITIALIZED = "11. Initialize workspace sections";
    private static final String STEP_12_TEACHER_ASSIGNMENTS_SET_UP = "12. Assign teachers to sections";
    private static final String STEP_13_RULES_CREATED = "13. Create rules";
    private static final String STEP_14_BUILD_VALIDATED = "14. Validate for build";

    // Checklist step statuses
    private static final int STATUS_COMPLETE = 1;
    private static final int STATUS_ERROR = 3;
    private static final int STATUS_WARNING = 2;

    // Other constants
    private static final int STEP_DEFAULT_WEIGHT = 8;

    // Private variables
    private int m_progress;
    private Schedule m_scenario;
    private int m_scenarioDays;
    private int m_scenarioPeriods;
    private int m_schoolBuildCourses;
    private int m_sectionsToSchedule;
    private UserAccessLog m_userAccessLog;
    private Set<String> m_validSteps;
    private Collection<BuildMasterSchedule> m_workspaceSections;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();

        /*
         * Go through the Build Readiness checklist steps. The order of the steps is important.
         */
        checkBuildYearDefinition(grid);
        checkBuildDistrictCatalog(grid);
        checkSchoolBuildYear(grid);
        checkBuildSchoolCatalog(grid);
        checkStudentRequestEntry(grid);
        checkScenario(grid);
        checkTimeStructure(grid);
        checkPatterns(grid);
        checkScheduleAttributes(grid);
        checkStudentRequestAnalysis(grid);
        checkInitializedSections(grid);
        checkTeacherAssignments(grid);
        checkRules(grid);
        checkScheduleValidation(grid);

        // Add parameters
        addParameter(OUTCOME_PARAM, Boolean.valueOf(m_progress > 84 ? true : false));
        addParameter(PROGRESS_PARAM, Integer.valueOf(m_progress));
        addParameter(SCENARIO_PARAM, m_scenario);

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_progress = 0;

        m_scenario = null;
        String scheduleOid = (String) getParameter(SCHEDULE_OID_PARAM);
        if (!StringUtils.isEmpty(scheduleOid)) {
            m_scenario = (Schedule) getBroker().getBeanByOid(Schedule.class, scheduleOid);
        }

        m_sectionsToSchedule = 0;
        m_validSteps = new HashSet<String>();
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
        m_userAccessLog = userData.getUserAccessLog();
    }

    /**
     * Checks if there are courses in the district course catalog defined for the build year.
     *
     * @param grid ReportDataGrid
     */
    private void checkBuildDistrictCatalog(ReportDataGrid grid) {
        grid.append();

        // Load the organization level for the catalog as defined in the root org preferences
        Organization ownerOrganization = getOrganization().getRootOrganization();
        String courseOwnerLevelOid = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                SystemPreferenceDefinition.SYS_COURSE_INTERMEDIATEOWNER_ORGANIZATION);

        if (!StringUtils.isEmpty(courseOwnerLevelOid)) {
            OrganizationDefinition ownerLevel = (OrganizationDefinition) getBroker()
                    .getBeanByOid(OrganizationDefinition.class, courseOwnerLevelOid);
            ownerOrganization = m_scenario.getSchool().getParentOrganization().getOrganization(ownerLevel.getLevel());
        }

        grid.set(STEP_NAME_FIELD, STEP_02_DISTRICT_CATALOG_SET_UP.replaceAll("district",
                ownerOrganization.getOrganizationDefinition().getName()));

        // Build criteria for the current year courses in the organization
        Criteria currentCriteria = new Criteria();
        currentCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        currentCriteria.addAndCriteria(OrganizationManager.getOrganizationCriteria(ownerOrganization.getOid()));

        QueryByCriteria currentQuery = new QueryByCriteria(Course.class, currentCriteria);

        int currentCourses = getBroker().getCount(currentQuery);
        int buildCourses = 0;

        if (m_validSteps.contains(STEP_01_BUILD_YEAR_DEFINED)) {
            int stepStatus = STATUS_ERROR;

            // Build criteria for the build year courses in the organization
            Criteria buildCriteria = new Criteria();
            buildCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, m_scenario.getDistrictContextOid());
            buildCriteria.addAndCriteria(OrganizationManager.getOrganizationCriteria(ownerOrganization.getOid()));

            QueryByCriteria buildQuery = new QueryByCriteria(Course.class, buildCriteria);

            buildCourses = getBroker().getCount(buildQuery);

            if (buildCourses > (currentCourses * 0.5)) {
                m_progress += STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_02_DISTRICT_CATALOG_SET_UP);

                if (buildCourses < (currentCourses * 0.8)) {
                    stepStatus = STATUS_WARNING;
                } else {
                    stepStatus = STATUS_COMPLETE;
                }
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }

        grid.set(STEP_NOTES_FIELD, "Number of current year courses: " + currentCourses +
                "; Number of build year courses: " + buildCourses);
    }

    /**
     * Checks if there are courses in the school course catalog defined for the build year.
     *
     *
     * @param grid ReportDataGrid
     */
    private void checkBuildSchoolCatalog(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_04_SCHOOL_CATALOG_SET_UP);

        Criteria currentCriteria = new Criteria();
        currentCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        currentCriteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        QueryByCriteria currentQuery = new QueryByCriteria(SchoolCourse.class, currentCriteria);

        int currentCourses = getBroker().getCount(currentQuery);
        int stepStatus = STATUS_ERROR;

        Criteria buildCriteria = new Criteria();
        buildCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        buildCriteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_DISTRICT_CONTEXT_OID, m_scenario.getDistrictContextOid());

        QueryByCriteria buildQuery = new QueryByCriteria(SchoolCourse.class, buildCriteria);

        m_schoolBuildCourses = getBroker().getCount(buildQuery);

        if (m_schoolBuildCourses > (currentCourses * 0.5)) {
            m_progress += STEP_DEFAULT_WEIGHT;
            m_validSteps.add(STEP_04_SCHOOL_CATALOG_SET_UP);

            if (m_schoolBuildCourses < (currentCourses * 0.8)) {
                stepStatus = STATUS_WARNING;
            } else {
                stepStatus = STATUS_COMPLETE;
            }
        }

        grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        grid.set(STEP_NOTES_FIELD, "Number of current year courses: " + currentCourses +
                "; Number of build year courses: " + m_schoolBuildCourses);
    }

    /**
     * Checks if the school year of the selected scenario is greater than or equal to the current
     * year.
     *
     * @param grid ReportDataGrid
     */
    private void checkBuildYearDefinition(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_01_BUILD_YEAR_DEFINED);

        int stepStatus = STATUS_ERROR;

        int buildYear = m_scenario.getDistrictContext().getSchoolYear();
        int currentYear = getCurrentContext().getSchoolYear();

        if (buildYear >= currentYear) {
            stepStatus = STATUS_COMPLETE;

            m_progress += STEP_DEFAULT_WEIGHT;
            m_validSteps.add(STEP_01_BUILD_YEAR_DEFINED);
        }

        grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        grid.set(STEP_NOTES_FIELD, ("Current year: " + currentYear + "; Build year: " + buildYear));
    }

    /**
     * Checks if the sections are initialized in the workspace.
     *
     * @param grid ReportDataGrid
     */
    private void checkInitializedSections(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_11_SECTIONS_INITIALIZED);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_ERROR;

            Criteria currentCriteria = new Criteria();
            currentCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID,
                    ((SisSchool) getSchool()).getActiveScheduleOid());

            QueryByCriteria currentQuery = new QueryByCriteria(MasterSchedule.class, currentCriteria);

            int currentSections = getBroker().getCount(currentQuery);

            Criteria buildCriteria = new Criteria();
            buildCriteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scenario.getOid());

            QueryByCriteria buildQuery = new QueryByCriteria(BuildMasterSchedule.class, buildCriteria);

            m_workspaceSections = getBroker().getCollectionByQuery(buildQuery);
            int initializedSections = m_workspaceSections.size();

            grid.set(STEP_NOTES_FIELD, "Number of sections in the current schedule: " + currentSections +
                    "\nNumber of sections in the workspace: " + initializedSections +
                    "\nSum of course section counts: " + m_sectionsToSchedule);

            int stepProgress = 0;

            if (initializedSections > 0) {
                stepProgress = STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_11_SECTIONS_INITIALIZED);

                if (initializedSections == m_sectionsToSchedule) {
                    stepStatus = STATUS_COMPLETE;
                } else {
                    stepStatus = STATUS_WARNING;
                }
            }

            m_progress += stepProgress;

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if patterns and pattern sets are created.
     *
     * @param grid ReportDataGrid
     */
    private void checkPatterns(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_08_PATTERNS_CREATED);

        if (m_validSteps.contains(STEP_07_TIME_STRUCTURE_SET_UP)) {
            int stepStatus = STATUS_ERROR;

            Criteria patternCriteria = new Criteria();
            patternCriteria.addEqualTo(SchedulePattern.COL_SCHOOL_OID, getSchool().getOid());

            QueryByCriteria patternQuery = new QueryByCriteria(SchedulePattern.class, patternCriteria);

            int patterns = 0;
            int matchingPatterns = 0;

            QueryIterator patternsIterator = getBroker().getIteratorByQuery(patternQuery);
            try {
                while (patternsIterator.hasNext()) {
                    SchedulePattern pattern = (SchedulePattern) patternsIterator.next();

                    if (pattern.getDays() == m_scenarioDays && pattern.getPeriods() == m_scenarioPeriods) {
                        matchingPatterns++;
                    }

                    patterns++;
                }
            } finally {
                patternsIterator.close();
            }

            Criteria patternSetCriteria = new Criteria();
            patternSetCriteria.addEqualTo(SchedulePatternSet.COL_OWNER_OID, getSchool().getOid());

            QueryByCriteria patternSetQuery = new QueryByCriteria(SchedulePatternSet.class, patternSetCriteria);

            int patternSets = getBroker().getCount(patternSetQuery);

            if (patterns > 0 && patternSets > 0 && matchingPatterns > 0) {
                stepStatus = STATUS_COMPLETE;
                m_progress += STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_08_PATTERNS_CREATED);
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
            grid.set(STEP_NOTES_FIELD, "Patterns: " + patterns + "; Pattern Sets: " + patternSets +
                    "\nPatterns matching scenario's structure: " + matchingPatterns);
        }
    }

    /**
     * Checks if there are any schedule rules created.
     *
     * @param grid ReportDataGrid
     */
    private void checkRules(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_13_RULES_CREATED);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_ERROR;

            int currentRules = ((SisSchool) getSchool()).getActiveSchedule() != null
                    ? ((SisSchool) getSchool()).getActiveSchedule().getScheduleRules().size()
                    : 0;

            int buildRules = m_scenario.getScheduleRules().size();

            if (buildRules > 0) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_13_RULES_CREATED);
            } else {
                stepStatus = STATUS_WARNING;
            }

            grid.set(STEP_NOTES_FIELD, "Number of active schedule rules: " + currentRules +
                    "; Number of build scenario rules: " + buildRules);
            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if a schedule build scenario is selected in the input.
     *
     * @param grid ReportDataGrid
     */
    private void checkScenario(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_06_SCENARIO_CREATED);

        if (m_validSteps.contains(STEP_03_SCHOOL_BUILD_YEAR_SET)) {
            int stepStatus = STATUS_ERROR;
            String note = null;

            if (m_scenario != null) {
                note = "Selected scenario: " + m_scenario.getName();
                stepStatus = STATUS_COMPLETE;

                m_progress += STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_06_SCENARIO_CREATED);
            } else {
                Criteria sharedScenarioCriteria = new Criteria();
                sharedScenarioCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());
                sharedScenarioCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        m_scenario.getDistrictContextOid());

                Criteria buildScenarioCriteria = new Criteria();
                buildScenarioCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, getSchool().getOid());
                buildScenarioCriteria.addEqualTo(Schedule.COL_DISTRICT_CONTEXT_OID, m_scenario.getDistrictContextOid());
                buildScenarioCriteria.addNotIn(X2BaseBean.COL_OID, new SubQuery(SchoolScheduleContext.class,
                        SchoolScheduleContext.COL_SHARED_SCHEDULE_OID, sharedScenarioCriteria));

                QueryByCriteria query = new QueryByCriteria(Schedule.class, buildScenarioCriteria);

                int availableScenarios = getBroker().getCount(query);
                if (availableScenarios > 0) {
                    note = "Available scenarios for the " + m_scenario.getDistrictContext().getSchoolYear() +
                            " build year: " + availableScenarios + ". Select one when running this report.";
                } else {
                    note = "Create a build scenario and select it when you run this report";
                }
            }

            grid.set(STEP_NOTES_FIELD, note);
            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if the student, staff, course and room schedule attributes are set.
     *
     * @param grid ReportDataGrid
     */
    private void checkScheduleAttributes(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_09_ATTRIBUTES_SET);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_ERROR;

            /*
             * Students
             */
            Criteria studentCriteria = new Criteria();
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            studentCriteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int activeStudents = getBroker().getCount(studentQuery);

            Criteria studentAttribuesCriteria = new Criteria();
            studentAttribuesCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
            studentAttribuesCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID,
                    m_scenario.getStudentScheduleOid());

            QueryByCriteria studentAttributessQuery =
                    new QueryByCriteria(StudentScheduleAttributes.class, studentAttribuesCriteria);

            int studentsToSchedule = getBroker().getCount(studentAttributessQuery);

            /*
             * Staff
             */
            Criteria staffCriteria = new Criteria();
            staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
            staffCriteria.addEqualTo(SisStaff.COL_STATUS,
                    PreferenceManager.getPreferenceValue(getOrganization(), STAFF_ACTIVE_CODE));

            QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);

            int activeStaff = getBroker().getCount(staffQuery);

            Criteria staffAttributesCriteria = new Criteria();
            staffAttributesCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_PARTICIPATE_INDICATOR,
                    Boolean.TRUE);
            staffAttributesCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_OID,
                    m_scenario.getStaffScheduleOid());

            QueryByCriteria staffAttributesQuery =
                    new QueryByCriteria(StaffScheduleAttributes.class, staffAttributesCriteria);

            int staffToSchedule = getBroker().getCount(staffAttributesQuery);

            /*
             * Courses
             */
            Criteria courseCriteria = new Criteria();
            courseCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
            courseCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_scenario.getCourseScheduleOid());

            QueryByCriteria coursesToScheduleQuery = new QueryByCriteria(SchoolCourseSchedule.class, courseCriteria);

            int invalidPpcCourses = 0;
            int invalidBaseTpyCourses = 0;
            int invalidCoveredTpyCourses = 0;

            Collection<SchoolCourseSchedule> coursesToSchedule =
                    getBroker().getCollectionByQuery(coursesToScheduleQuery);
            for (SchoolCourseSchedule courseAttribute : coursesToSchedule) {
                m_sectionsToSchedule += courseAttribute.getSections();

                if (courseAttribute.getPeriodsPerCycle() == null
                        || courseAttribute.getPeriodsPerCycle().doubleValue() <= 0.0) {
                    invalidPpcCourses++;
                }

                if (courseAttribute.getCoveredTermsPerYear() <= 0) {
                    invalidCoveredTpyCourses++;
                }

                if (courseAttribute.getBaseTermsPerYear() <= 0) {
                    invalidBaseTpyCourses++;
                }
            }

            courseCriteria.addIsNull(SchoolCourseSchedule.COL_PATTERN_SET_OID);

            QueryByCriteria noPatternQuery = new QueryByCriteria(SchoolCourseSchedule.class, courseCriteria);

            int noPatternCourses = getBroker().getCount(noPatternQuery);

            /*
             * Rooms
             */
            Criteria roomCriteria = new Criteria();
            roomCriteria.addEqualTo(RoomScheduleAttributes.COL_SCHEDULE_OID, m_scenario.getRoomScheduleOid());

            QueryByCriteria roomQuery = new QueryByCriteria(RoomScheduleAttributes.class, roomCriteria);
            int rooms = getBroker().getCount(roomQuery);

            roomCriteria.addEqualTo(RoomScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);

            QueryByCriteria roomsToScheduleQuery = new QueryByCriteria(RoomScheduleAttributes.class, roomCriteria);
            int roomsToSchedule = getBroker().getCount(roomsToScheduleQuery);

            StringBuilder note = new StringBuilder(512);
            note.append("Students to schedule: " + studentsToSchedule + " (" + activeStudents
                    + " currently active students)" +
                    "\nStaff to schedule: " + staffToSchedule + " (" + activeStaff + " currently active staff)" +
                    "\nRooms to schedule: " + roomsToSchedule + " (" + rooms + " rooms)" +
                    "\nCourses to schedule: " + coursesToSchedule.size() + " (" + m_schoolBuildCourses
                    + " school courses for build year)");

            if (noPatternCourses > 0) {
                note.append("\nCourses to schedule without pattern sets: " + noPatternCourses);
            }

            if (invalidPpcCourses > 0) {
                note.append("\nCourses to schedule with invalid \"Periods per cycle\": " + invalidPpcCourses);
            }

            if (invalidCoveredTpyCourses > 0) {
                note.append(
                        "\nCourses to schedule with invalid \"Covered terms per year\": " + invalidCoveredTpyCourses);
            }

            if (invalidBaseTpyCourses > 0) {
                note.append("\nCourses to schedule with invalid \"Base terms per year\": " + invalidBaseTpyCourses);
            }

            grid.set(STEP_NOTES_FIELD, note.toString());

            int stepProgress = 0;

            if (studentsToSchedule > 0) {
                stepProgress += 2;
            }

            if (staffToSchedule > 0) {
                stepProgress += 2;
            }

            if (roomsToSchedule > 0) {
                stepProgress += 2;
            }

            if (coursesToSchedule.size() > 0 && noPatternCourses == 0 && invalidPpcCourses == 0 &&
                    invalidCoveredTpyCourses == 0 && invalidBaseTpyCourses == 0) {
                stepProgress += 2;
            }

            m_progress += stepProgress;

            if (stepProgress == STEP_DEFAULT_WEIGHT) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_09_ATTRIBUTES_SET);
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if the scenario is valid for build.
     *
     * @param grid ReportDataGrid
     */
    private void checkScheduleValidation(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_14_BUILD_VALIDATED);

        if (m_validSteps.contains(STEP_08_PATTERNS_CREATED) &&
                m_validSteps.contains(STEP_09_ATTRIBUTES_SET) &&
                m_validSteps.contains(STEP_11_SECTIONS_INITIALIZED)) {
            int stepStatus = STATUS_ERROR;

            EngineHarness engineHarness = new EngineHarness(getBroker(), m_userAccessLog);
            EngineResults engineResults =
                    engineHarness.validate(m_scenario, Integer.valueOf(EngineHarness.VALIDATION_TYPE_BUILD), getLocale());

            Criteria validationCriteria = new Criteria();
            validationCriteria.addEqualTo(ScheduleFeedback.COL_SCHEDULE_OID, m_scenario.getOid());
            validationCriteria.addEqualTo(ScheduleFeedback.COL_RUN_TYPE, SchedConstants.SCHEDULE_TYPE_BUILD);

            Criteria errorsCriteria = new Criteria();
            errorsCriteria.addEqualTo(ScheduleFeedback.COL_LEVEL, ScheduleFeedback.FEED_BACK_LEVEL_ERROR);
            errorsCriteria.addAndCriteria(validationCriteria);

            QueryByCriteria errorsQuery = new QueryByCriteria(ScheduleFeedback.class, errorsCriteria);

            int errors = getBroker().getCount(errorsQuery);

            Criteria warningsCriteria = new Criteria();
            warningsCriteria.addEqualTo(ScheduleFeedback.COL_LEVEL, ScheduleFeedback.FEED_BACK_LEVEL_WARNING);
            warningsCriteria.addAndCriteria(validationCriteria);

            QueryByCriteria warningsQuery = new QueryByCriteria(ScheduleFeedback.class, warningsCriteria);

            int warnings = getBroker().getCount(warningsQuery);

            int stepProgress = 0;
            if (errors <= 20 && errors > 10) {
                stepProgress = 3;
            } else if (errors <= 10 && errors > 5) {
                stepProgress = 6;
            } else if (errors <= 5 && errors > 0) {
                stepProgress = 9;
            }

            if (engineResults.getSuccessIndicator()) {
                stepStatus = STATUS_COMPLETE;
                stepProgress = 12;
                m_validSteps.add(STEP_14_BUILD_VALIDATED);
            }

            m_progress += stepProgress;

            grid.set(STEP_NOTES_FIELD, "Fatal errors: " + errors + "; Warnings: " + warnings);
            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if the build year is assigned to the selected school.
     *
     * @param grid ReportDataGrid
     */
    private void checkSchoolBuildYear(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_03_SCHOOL_BUILD_YEAR_SET);

        String buildYear = "Not set";

        if (m_validSteps.contains(STEP_01_BUILD_YEAR_DEFINED)) {
            int stepStatus = STATUS_ERROR;

            if (m_scenario.getDistrictContextOid().equals(getSchool().getBuildContextOid())) {
                buildYear = String.valueOf(getSchool().getBuildContext().getSchoolYear());
                stepStatus = STATUS_COMPLETE;

                m_progress += STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_03_SCHOOL_BUILD_YEAR_SET);
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }

        grid.set(STEP_NOTES_FIELD, "School build year: " + buildYear);
    }

    /**
     * Checks if there are any student requests entered for the build year.
     *
     * @param grid ReportDataGrid
     */
    private void checkStudentRequestEntry(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_05_STUDENT_REQUESTS_ENTERED);

        Criteria currentCriteria = new Criteria();
        currentCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, getSchool().getOid());
        currentCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        QueryByCriteria currentQuery = new QueryByCriteria(CourseRequest.class, currentCriteria);

        int currentRequests = getBroker().getCount(currentQuery);
        int stepStatus = STATUS_ERROR;

        Criteria buildCriteria = new Criteria();
        buildCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, getSchool().getOid());
        buildCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_scenario.getDistrictContextOid());

        QueryByCriteria buildQuery = new QueryByCriteria(CourseRequest.class, buildCriteria);

        int buildRequests = getBroker().getCount(buildQuery);

        if (buildRequests > currentRequests * 0.5) {
            int stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.5);
            m_validSteps.add(STEP_05_STUDENT_REQUESTS_ENTERED);

            if (buildRequests < (currentRequests * 0.8)) {
                stepProgress += (int) (STEP_DEFAULT_WEIGHT * 0.25);
                stepStatus = STATUS_WARNING;
            } else {
                stepProgress = STEP_DEFAULT_WEIGHT;
                stepStatus = STATUS_COMPLETE;
            }

            m_progress += stepProgress;
        }

        grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        grid.set(STEP_NOTES_FIELD, "Number of current year requests: " + currentRequests +
                "; Number of build year requests: " + buildRequests);
    }

    /**
     * Checks if there are any over-subscribed courses or any over-subscribing students.
     *
     * @param grid ReportDataGrid
     */
    private void checkStudentRequestAnalysis(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_10_REQUESTS_ANALYZED);

        if (m_validSteps.contains(STEP_07_TIME_STRUCTURE_SET_UP) &&
                m_validSteps.contains(STEP_09_ATTRIBUTES_SET)) {
            int stepStatus = STATUS_WARNING;
            m_validSteps.add(STEP_10_REQUESTS_ANALYZED);

            int overSubscribedCourses = getOverSubscribedCourses();
            int overSubscribingStudents = getOverSubscribingStudents();

            if (overSubscribedCourses == 0 && overSubscribingStudents == 0) {
                stepStatus = STATUS_COMPLETE;
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
            grid.set(STEP_NOTES_FIELD, "Number of over-subscribed courses: " + overSubscribedCourses +
                    "\nNumber of over-subscribing students: " + overSubscribingStudents);
        }
    }

    /**
     * Checks if teacher assignments are set.
     *
     * @param grid ReportDataGrid
     */
    private void checkTeacherAssignments(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_12_TEACHER_ASSIGNMENTS_SET_UP);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_WARNING;
            m_validSteps.add(STEP_12_TEACHER_ASSIGNMENTS_SET_UP);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scenario.getOid());
            criteria.addNotNull(BuildMasterSchedule.COL_PRIMARY_STAFF_OID);

            QueryByCriteria query = new QueryByCriteria(BuildMasterSchedule.class, criteria);

            int assignedSections = getBroker().getCount(query);
            int overallSections = m_workspaceSections.size();

            String assignedPercentage = "0";
            if (overallSections > 0) {
                assignedPercentage = new BigDecimal(String.valueOf(((double) assignedSections) /
                        ((double) overallSections) * 100.00)).setScale(2, RoundingMode.HALF_UP).toString();
            }

            // Find all the staff that have minimum assignments set.
            Criteria staffCriteria = new Criteria();
            staffCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_OID, m_scenario.getStaffScheduleOid());
            staffCriteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_PARTICIPATE_INDICATOR, Boolean.TRUE);
            staffCriteria.addGreaterThan(StaffScheduleAttributes.COL_MINIMUM_ASSIGNMENTS, Integer.valueOf(0));
            QueryByCriteria staffQuery = new QueryByCriteria(StaffScheduleAttributes.class, staffCriteria);
            Map<String, StaffScheduleAttributes> staffMap =
                    getBroker().getMapByQuery(staffQuery, StaffScheduleAttributes.COL_STAFF_OID, 100);

            int numberOfTeachers = staffMap.size();
            int numberOfTeachersMeeting = 0;
            String minimumTeacherValue = "";

            if (!staffMap.isEmpty()) {
                // Find the count of assignments by staff
                String[] columns =
                        new String[] {BuildMasterSchedule.COL_PRIMARY_STAFF_OID, "COUNT(*)"};

                ReportQueryByCriteria reportQuery =
                        new ReportQueryByCriteria(BuildMasterSchedule.class, columns, criteria);
                reportQuery.addGroupBy(new String[] {BuildMasterSchedule.COL_PRIMARY_STAFF_OID});


                try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery)) {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String staffOid = row[0].toString();
                        Long assignedCount = (Long) row[1];

                        // Make sure the staff has a minimum assignment value set.
                        if (staffMap.containsKey(staffOid)) {
                            int min = staffMap.get(staffOid).getMinimumAssignments();
                            if (min <= assignedCount.intValue()) {
                                // Bump if the assigned count is greater or equal than the minimum.
                                numberOfTeachersMeeting++;
                            }
                        }
                    }
                }

                // Calculate the percentage of teacher reaching their minimum assignments.
                String minimumPercentage = new BigDecimal(String.valueOf(((double) numberOfTeachersMeeting) /
                        ((double) numberOfTeachers) * 100.00)).setScale(2, RoundingMode.HALF_UP).toString();

                minimumTeacherValue = "\nTeachers meeting minimum assignments: " + minimumPercentage +
                        "% (" + numberOfTeachersMeeting + "/" + numberOfTeachers + ")";
            }

            grid.set(STEP_NOTES_FIELD, "Teacher assignments set: " + assignedPercentage +
                    "% (" + assignedSections + "/" + overallSections + ")" + minimumTeacherValue);

            int stepProgress = 0;
            if (assignedSections > overallSections * 0.25 && assignedSections < overallSections * 0.5) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.25);
            } else if (assignedSections >= overallSections * 0.5 && assignedSections < overallSections * 0.75) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.5);
            } else if (assignedSections >= overallSections * 0.75 && assignedSections < overallSections) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.75);
            } else if (assignedSections != 0 && assignedSections == overallSections
                    && numberOfTeachersMeeting == numberOfTeachers) {
                stepStatus = STATUS_COMPLETE;
                stepProgress = STEP_DEFAULT_WEIGHT;
            }

            m_progress += stepProgress;

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        }
    }

    /**
     * Checks if the terms days and periods are set up for the scenario.
     *
     * @param grid ReportDataGrid
     */
    private void checkTimeStructure(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_07_TIME_STRUCTURE_SET_UP);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_ERROR;

            boolean areTermsSetUp = false;
            boolean areDaysSetUp = false;
            boolean arePeriodsSetUp = false;

            SisSchool school = ((SisSchool) getSchool());
            int activeTerms =
                    school.getActiveSchedule() != null ? school.getActiveSchedule().getScheduleTerms().size() : 0;

            int activeDays =
                    school.getActiveSchedule() != null ? school.getActiveSchedule().getScheduleDays().size() : 0;

            int activePeriods =
                    school.getActiveSchedule() != null ? school.getActiveSchedule().getSchedulePeriods().size() : 0;

            int scenarioTerms = m_scenario.getScheduleTerms().size();
            if (scenarioTerms > 0) {
                areTermsSetUp = true;
                m_progress += 2;
            }

            m_scenarioDays = m_scenario.getScheduleDays().size();
            if (m_scenarioDays > 0) {
                areDaysSetUp = true;
                m_progress += 3;
            }

            m_scenarioPeriods = m_scenario.getSchedulePeriods().size();
            if (m_scenarioPeriods > 0) {
                arePeriodsSetUp = true;
                m_progress += 3;
            }

            if (areTermsSetUp && areDaysSetUp && arePeriodsSetUp) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_07_TIME_STRUCTURE_SET_UP);
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
            grid.set(STEP_NOTES_FIELD, "Active schedule: " + activeTerms + " terms; " + activeDays + " days; "
                    + activePeriods + " periods" +
                    "\nBuild scenario: " + scenarioTerms + " terms; " + m_scenarioDays + " days; " + m_scenarioPeriods
                    + " periods");
        }
    }

    /**
     * Returns the number of courses that have more requests than the total maximum enrollment of
     * their sections.
     *
     * @return int
     */
    private int getOverSubscribedCourses() {
        int overSubscribedCourses = 0;

        /*
         * Build sections query
         */
        Criteria sectionCriteria = new Criteria();
        sectionCriteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scenario.getOid());

        QueryByCriteria sectionQuery = new QueryByCriteria(BuildMasterSchedule.class, sectionCriteria);

        Map<String, List<BuildMasterSchedule>> sectionsByCourseOid =
                getBroker().getGroupedCollectionByQuery(sectionQuery, BuildMasterSchedule.COL_SCHOOL_COURSE_OID, 4096);

        /*
         * Build course attributes and course requests queries
         */
        Criteria courseCriteria = new Criteria();
        courseCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);
        courseCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_scenario.getCourseScheduleOid());

        Criteria requestCriteria = new Criteria();
        requestCriteria.addIn(CourseRequest.COL_SCHOOL_COURSE_OID,
                new SubQuery(SchoolCourseSchedule.class, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, courseCriteria));

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);

        Map<String, List<CourseRequest>> requestsByCourseOid =
                getBroker().getGroupedCollectionByQuery(requestQuery, CourseRequest.COL_SCHOOL_COURSE_OID, 4096);

        QueryByCriteria courseQuery = new QueryByCriteria(SchoolCourseSchedule.class, courseCriteria);

        QueryIterator courseAttributes = getBroker().getIteratorByQuery(courseQuery);
        try {
            while (courseAttributes.hasNext()) {
                SchoolCourseSchedule courseAttribute = (SchoolCourseSchedule) courseAttributes.next();

                int courseRequests = 0;
                if (requestsByCourseOid.get(courseAttribute.getSchoolCourseOid()) != null) {
                    courseRequests = requestsByCourseOid.get(courseAttribute.getSchoolCourseOid()).size();
                }

                int courseSize = 0;

                List<BuildMasterSchedule> sections = sectionsByCourseOid.get(courseAttribute.getSchoolCourseOid());
                if (sections != null && sections.size() > 0) {
                    for (BuildMasterSchedule section : sections) {
                        courseSize += section.getEnrollmentMax();
                    }
                } else {
                    courseSize = courseAttribute.getSections() * courseAttribute.getEnrollmentMax();
                }

                if (courseRequests > courseSize) {
                    overSubscribedCourses++;
                }
            }
        } finally {
            courseAttributes.close();
        }

        return overSubscribedCourses;
    }

    /**
     * Returns the number of students that are requesting more courses than they can be scheduled
     * for in a schedule cycle.
     *
     * @return int
     */
    private int getOverSubscribingStudents() {
        int overSubscribingStudents = 0;

        /*
         * Load the course attributes into a map keyed to the school course OID.
         */
        Criteria courseCriteria = new Criteria();
        courseCriteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_scenario.getCourseScheduleOid());

        QueryByCriteria courseQuery = new QueryByCriteria(SchoolCourseSchedule.class, courseCriteria);

        Map<String, SchoolCourseSchedule> courseAttributesByCourseOid =
                getBroker().getMapByQuery(courseQuery, SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, 4096);

        /*
         * Build the requests query.
         */
        Criteria requestCriteria = new Criteria();
        requestCriteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, m_scenario.getDistrictContextOid());
        requestCriteria.addEqualTo(CourseRequest.COL_SCHOOL_OID, getSchool().getOid());
        requestCriteria.addNotEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, Boolean.TRUE);

        QueryByCriteria requestQuery = new QueryByCriteria(CourseRequest.class, requestCriteria);
        requestQuery.addOrderByAscending(CourseRequest.COL_STUDENT_OID);

        double availableSlots = m_scenarioPeriods * m_scenarioDays;
        ScheduleSlotSet slotSet = new ScheduleSlotSet(availableSlots);

        String lastStudentOid = null;
        boolean studentOversubscribed = false;

        QueryIterator requests = getBroker().getIteratorByQuery(requestQuery);
        try {
            while (requests.hasNext()) {
                CourseRequest request = (CourseRequest) requests.next();

                String studentOid = request.getStudentOid();
                if (!ObjectUtils.match(studentOid, lastStudentOid)) {
                    slotSet.resetScheduledSlots();
                    studentOversubscribed = false;
                }

                if (!studentOversubscribed) {
                    SchoolCourseSchedule courseAttribute =
                            courseAttributesByCourseOid.get(request.getSchoolCourseOid());
                    if (courseAttribute != null && courseAttribute.getBaseTermsPerYear() > 0) {
                        BigDecimal scheduledSlots = new BigDecimal((courseAttribute.getPeriodsPerCycle().doubleValue()
                                * courseAttribute.getCoveredTermsPerYear()) /
                                courseAttribute.getBaseTermsPerYear()).setScale(4, RoundingMode.HALF_UP);

                        slotSet.addToScheduledSlots(scheduledSlots.doubleValue());
                    }

                    if (slotSet.isOverscheduled()) {
                        studentOversubscribed = true;
                        overSubscribingStudents++;
                    }
                }

                lastStudentOid = studentOid;
            }
        } finally {
            requests.close();
        }

        return overSubscribingStudents;
    }

    /**
     * Inner class that stores the number of filled schedule slots out of all the available slots.
     */
    private class ScheduleSlotSet {
        private double m_availableSlots;
        private double m_scheduledSlots;

        /**
         * Constructs a ScheduleSlotSet instance.
         *
         * @param availableSlots double
         */
        public ScheduleSlotSet(double availableSlots) {
            m_availableSlots = availableSlots;
            resetScheduledSlots();
        }

        /**
         * Adds to the number of scheduled slots.
         *
         * @param scheduledSlots double
         */
        public void addToScheduledSlots(double scheduledSlots) {
            m_scheduledSlots += scheduledSlots;
        }

        /**
         * Returns true if the slot set is over scheduled, false otherwise.
         *
         * @return boolean
         */
        public boolean isOverscheduled() {
            return m_scheduledSlots > m_availableSlots ? true : false;
        }

        /**
         * Resets the scheduled slots to 0.
         */
        public void resetScheduledSlots() {
            m_scheduledSlots = 0.0;
        }
    }
}
