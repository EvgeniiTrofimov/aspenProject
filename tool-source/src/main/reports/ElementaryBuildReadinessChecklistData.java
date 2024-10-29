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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Elementary Build Readiness Checklist" report.
 *
 * @author X2 Development Corporation
 */
public class ElementaryBuildReadinessChecklistData extends ReportJavaSourceNet {
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
    private static final String STEP_05_STUDENT_PROGRAMS_ENTERED = "5. Assign student programs";
    private static final String STEP_06_SCENARIO_CREATED = "6. Create a schedule scenario in the build view";
    private static final String STEP_07_TIME_STRUCTURE_SET_UP = "7. Create terms and days";
    private static final String STEP_08_ATTRIBUTES_SET = "8. Set course, student and staff attributes";
    private static final String STEP_09_SECTIONS_INITIALIZED = "9. Initialize workspace sections";
    private static final String STEP_10_TEACHER_ASSIGNMENTS_SET_UP = "10. Assign teachers to sections";
    private static final String STEP_11_RULES_CREATED = "11. Create rules";

    // Checklist step statuses
    private static final int STATUS_COMPLETE = 1;
    private static final int STATUS_ERROR = 3;
    private static final int STATUS_WARNING = 2;

    // Other constants
    private static final int STEP_DEFAULT_WEIGHT = 10;

    // Private variables
    private int m_progress;
    private Schedule m_scenario;
    private int m_scenarioDays;
    private int m_schoolBuildCourses;
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
        checkStudentProgramEntry(grid);
        checkScenario(grid);
        checkTimeStructure(grid);
        checkScheduleAttributes(grid);
        checkInitializedSections(grid);
        checkTeacherAssignments(grid);
        checkRules(grid);

        // Add parameters
        addParameter(OUTCOME_PARAM, Boolean.valueOf(m_progress > 66 ? true : false));
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
        m_validSteps = new HashSet<String>();
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
        int currentYear = getOrganization().getRootOrganization().getCurrentContext().getSchoolYear();

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
        grid.set(STEP_NAME_FIELD, STEP_09_SECTIONS_INITIALIZED);

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
                    "\nNumber of sections in the workspace: " + initializedSections);

            int stepProgress = 0;

            if (initializedSections > 0) {
                stepProgress = STEP_DEFAULT_WEIGHT;
                m_validSteps.add(STEP_09_SECTIONS_INITIALIZED);

                if (initializedSections > 0) {
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
     * Checks if there are any schedule rules created.
     *
     * @param grid ReportDataGrid
     */
    private void checkRules(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_11_RULES_CREATED);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_ERROR;

            int currentRules = ((SisSchool) getSchool()).getActiveSchedule() != null
                    ? ((SisSchool) getSchool()).getActiveSchedule().getScheduleRules().size() : 0;

            int buildRules = m_scenario.getScheduleRules().size();

            if (buildRules > 0) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_11_RULES_CREATED);
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
        grid.set(STEP_NAME_FIELD, STEP_08_ATTRIBUTES_SET);

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
            courseCriteria.addNotEqualTo(SchoolCourseSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_PROGRAM);
            courseCriteria.addNotEqualTo(SchoolCourseSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_LUNCH);
            courseCriteria.addNotEqualTo(SchoolCourseSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_PLANNING);

            QueryByCriteria coursesToScheduleQuery = new QueryByCriteria(SchoolCourseSchedule.class, courseCriteria);

            int invalidBaseTpyCourses = 0;
            int invalidCoveredTpyCourses = 0;

            Collection<SchoolCourseSchedule> coursesToSchedule =
                    getBroker().getCollectionByQuery(coursesToScheduleQuery);
            for (SchoolCourseSchedule courseAttribute : coursesToSchedule) {

                if (courseAttribute.getCoveredTermsPerYear() <= 0) {
                    invalidCoveredTpyCourses++;
                }

                if (courseAttribute.getBaseTermsPerYear() <= 0) {
                    invalidBaseTpyCourses++;
                }
            }

            StringBuilder note = new StringBuilder(512);
            note.append("Students to schedule: " + studentsToSchedule + " (" + activeStudents
                    + " currently active students)" +
                    "\nStaff to schedule: " + staffToSchedule + " (" + activeStaff + " currently active staff)" +
                    "\nCourses to schedule: " + coursesToSchedule.size() + " (" + m_schoolBuildCourses
                    + " school courses for build year)");

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
                stepProgress += 4;
            }

            if (staffToSchedule > 0) {
                stepProgress += 2;
            }

            if (coursesToSchedule.size() > 0 && invalidCoveredTpyCourses == 0 && invalidBaseTpyCourses == 0) {
                stepProgress += 4;
            }

            m_progress += stepProgress;

            if (stepProgress == 10) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_08_ATTRIBUTES_SET);
            }

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
     * Checks if how many students have no programs assigned
     *
     * @param grid ReportDataGrid
     */
    private void checkStudentProgramEntry(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_05_STUDENT_PROGRAMS_ENTERED);

        /*
         * Gather all the student schedule group
         */
        X2Criteria studentScheduleGroup = new X2Criteria();
        studentScheduleGroup.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID,
                m_scenario.getDistrictContextOid());
        studentScheduleGroup.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID, m_scenario.getSchoolOid());

        QueryByCriteria studentGroupQuery = new QueryByCriteria(StudentScheduleGroup.class, studentScheduleGroup);
        Collection<StudentScheduleGroup> scheduleGroups = getBroker().getCollectionByQuery(studentGroupQuery);

        int totalStudentsWithProgram = 0;
        for (StudentScheduleGroup group : scheduleGroups) {
            X2Criteria studentInGroupCriteria = ElementaryScheduleManager.getStudentScheduleGroupMemberCriteria(
                    m_scenario, group, group.getParentStudentScheduleGroupOid() == null);

            QueryByCriteria studentsInGroupQuery = new QueryByCriteria(SisStudent.class, studentInGroupCriteria);
            Collection<SisStudent> students = getBroker().getCollectionByQuery(studentsInGroupQuery);

            if (group.getProgram() != null) {
                totalStudentsWithProgram += students.size();
            }

        }
        int stepStatus = STATUS_ERROR;

        Criteria studentCriteria = new Criteria();
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID, m_scenario.getStudentScheduleOid());
        studentCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_INCLUDE_INDICATOR, Boolean.TRUE);

        QueryByCriteria studentQuery = new QueryByCriteria(StudentScheduleAttributes.class, studentCriteria);

        int totalStudents = getBroker().getCount(studentQuery);

        if (totalStudentsWithProgram > totalStudents * 0.5) {
            int stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.5);
            m_validSteps.add(STEP_05_STUDENT_PROGRAMS_ENTERED);

            if (totalStudentsWithProgram < (totalStudents * 0.8)) {
                stepProgress += (int) (STEP_DEFAULT_WEIGHT * 0.25);
                stepStatus = STATUS_WARNING;
            } else {
                stepProgress = STEP_DEFAULT_WEIGHT;
                stepStatus = STATUS_COMPLETE;
            }

            m_progress += stepProgress;
        }

        grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
        grid.set(STEP_NOTES_FIELD, "Number of students to schedule: " + totalStudents +
                "; Number of students without program: " + (totalStudents - totalStudentsWithProgram));
    }

    /**
     * Checks if teacher assignments are set.
     *
     * @param grid ReportDataGrid
     */
    private void checkTeacherAssignments(ReportDataGrid grid) {
        grid.append();
        grid.set(STEP_NAME_FIELD, STEP_10_TEACHER_ASSIGNMENTS_SET_UP);

        if (m_validSteps.contains(STEP_06_SCENARIO_CREATED)) {
            int stepStatus = STATUS_WARNING;
            m_validSteps.add(STEP_10_TEACHER_ASSIGNMENTS_SET_UP);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scenario.getOid());
            criteria.addNotEqualTo(BuildMasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_LUNCH);
            criteria.addNotEqualTo(BuildMasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_RECESS);
            criteria.addNotNull(BuildMasterSchedule.COL_PRIMARY_STAFF_OID);

            QueryByCriteria query = new QueryByCriteria(BuildMasterSchedule.class, criteria);
            int assignedSections = getBroker().getCount(query);

            Criteria sectionShouldHaveTeacherCriteria = new Criteria();
            sectionShouldHaveTeacherCriteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scenario.getOid());
            sectionShouldHaveTeacherCriteria.addNotEqualTo(
                    BuildMasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_LUNCH);
            sectionShouldHaveTeacherCriteria.addNotEqualTo(
                    BuildMasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_RECESS);

            QueryByCriteria sectionShouldHaveTeacherQuery =
                    new QueryByCriteria(BuildMasterSchedule.class, sectionShouldHaveTeacherCriteria);
            int overallSections = getBroker().getCount(sectionShouldHaveTeacherQuery);

            String assignedPercentage = "0";
            if (overallSections > 0) {
                assignedPercentage = new BigDecimal(String.valueOf(((double) assignedSections) /
                        ((double) overallSections) * 100.00)).setScale(2, RoundingMode.HALF_UP).toString();
            }

            grid.set(STEP_NOTES_FIELD, "Teacher assignments set: " + assignedPercentage +
                    "% (" + assignedSections + "/" + overallSections + ")");

            int stepProgress = 0;
            if (assignedSections > overallSections * 0.25 && assignedSections < overallSections * 0.5) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.25);
            } else if (assignedSections >= overallSections * 0.5 && assignedSections < overallSections * 0.75) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.5);
            } else if (assignedSections >= overallSections * 0.75 && assignedSections < overallSections) {
                stepProgress = (int) (STEP_DEFAULT_WEIGHT * 0.75);
            } else if (assignedSections != 0 && assignedSections == overallSections) {
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

            SisSchool school = ((SisSchool) getSchool());
            int activeTerms =
                    school.getActiveSchedule() != null ? school.getActiveSchedule().getScheduleTerms().size() : 0;

            int activeDays =
                    school.getActiveSchedule() != null ? school.getActiveSchedule().getScheduleDays().size() : 0;

            int scenarioTerms = m_scenario.getScheduleTerms().size();
            if (scenarioTerms > 0) {
                areTermsSetUp = true;
                m_progress += 5;
            }

            m_scenarioDays = m_scenario.getScheduleDays().size();
            if (m_scenarioDays > 0) {
                areDaysSetUp = true;
                m_progress += 5;
            }

            if (areTermsSetUp && areDaysSetUp) {
                stepStatus = STATUS_COMPLETE;
                m_validSteps.add(STEP_07_TIME_STRUCTURE_SET_UP);
            }

            grid.set(STEP_STATUS_FIELD, Integer.valueOf(stepStatus));
            grid.set(STEP_NOTES_FIELD, "Active schedule: " + activeTerms + " terms; " + activeDays + " days " +
                    "\nBuild scenario: " + scenarioTerms + " terms; " + m_scenarioDays + " days ");
        }
    }
}
