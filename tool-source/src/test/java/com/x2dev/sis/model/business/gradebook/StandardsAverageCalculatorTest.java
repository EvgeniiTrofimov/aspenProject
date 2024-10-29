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

package com.x2dev.sis.model.business.gradebook;

import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.procedures.CreditUpdateTest;
import com.x2dev.sis.web.gradebook.ScoreGrid;
import com.x2dev.utils.ByteArrayCompiler;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Test;

/**
 * JUnit for the "Average of Standards" calculator.
 *
 * @author X2 Development Corporation
 */
public class StandardsAverageCalculatorTest extends X2BaseTest {
    private ModelBroker m_broker;
    private SisSchool m_school;
    private MasterSchedule m_section;
    private SisStaff m_staff;
    private SisStudent m_student1;
    private SisStudent m_student2;
    private SisStudent m_student3;
    private GradeTerm m_term1;
    private GradeTerm m_term2;


    // Refer to method createTestData() for score values
    private GradebookColumnType m_testCategory;
    private GradebookColumnDefinition m_test11;
    private GradebookColumnDefinition m_test12;
    private GradebookColumnDefinition m_test13;
    private GradebookColumnDefinition m_test21;
    private GradebookColumnDefinition m_test22;
    private GradebookColumnDefinition m_test23;

    private ScoreGrid m_grid;
    private RubricRatingScale m_scale;

    private RubricDefinition m_rubric;
    private RubricCriterion m_criterion1;
    private RubricCriterion m_criterion2;

    private SectionReportingStandard m_standard1;
    private SectionReportingStandard m_standard2;
    private GradeAverageCalculation m_averageCalculation;

    /**
     * Test "Average of Standards" using the default Trend averages.
     */
    @Test
    public void testOverallAveragesWithTrends() {

        // Overall calculators
        String standardsAverage1Id = StandardsOverallAverageCalculator.getIdentifier(m_term1.getOid());
        String standardsAverage2Id = StandardsOverallAverageCalculator.getIdentifier(m_term2.getOid());

        // Term 1 trend calculators
        String trends11AverageId = StandardsTrendCalculator.getIdentifier(m_term1.getOid(), m_standard1.getOid());
        String trends12AverageId = StandardsTrendCalculator.getIdentifier(m_term1.getOid(), m_standard2.getOid());

        // Term 2 trend calculators
        String trends21AverageId = StandardsTrendCalculator.getIdentifier(m_term2.getOid(), m_standard1.getOid());
        String trends22AverageId = StandardsTrendCalculator.getIdentifier(m_term2.getOid(), m_standard2.getOid());

        // ** STUDENT 1 **
        m_grid.gotoRow(m_student1.getOid());

        // Get all trend averages
        double t11Avg = m_grid.getAverageNumeric(trends11AverageId).doubleValue();
        double t12Avg = m_grid.getAverageNumeric(trends12AverageId).doubleValue();
        double t21Avg = m_grid.getAverageNumeric(trends21AverageId).doubleValue();
        double t22Avg = m_grid.getAverageNumeric(trends22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        double t1Avg = (t11Avg + t12Avg) / 2;
        double t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 2 **
        m_grid.gotoRow(m_student2.getOid());

        // Get all trend averages 1 for each standard (2) across 2 terms
        t11Avg = m_grid.getAverageNumeric(trends11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(trends12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(trends21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(trends22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 3 **
        m_grid.gotoRow(m_student3.getOid());

        // Get all trend averages 1 for each standard (2) across 2 terms
        t11Avg = m_grid.getAverageNumeric(trends11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(trends12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(trends21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(trends22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components.
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

    }

    /**
     * Test "Average of Standards" by overriding the calculator with a GradeAverageCalculation
     * procedure
     * specifying using the StandarsAverageCalculators.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws X2BaseException exception
     */
    @Test
    public void testOverallAverageWithAveragesProcedure() throws IOException, X2BaseException {
        // Create the grade average calculation procedure
        createGradeAverageCalculationProcedure(false);

        // Get the two term based Overall Average Calculators
        String standardsAverage1Id = StandardsOverallAverageCalculator.getIdentifier(m_term1.getOid());
        String standardsAverage2Id = StandardsOverallAverageCalculator.getIdentifier(m_term2.getOid());

        // Get Average Calculators for each term and each standard within the term
        String avg11AverageId = StandardsAverageCalculator.getIdentifier(m_term1.getOid(), m_standard1.getOid());
        String avg12AverageId = StandardsAverageCalculator.getIdentifier(m_term1.getOid(), m_standard2.getOid());
        String avg21AverageId = StandardsAverageCalculator.getIdentifier(m_term2.getOid(), m_standard1.getOid());
        String avg22AverageId = StandardsAverageCalculator.getIdentifier(m_term2.getOid(), m_standard2.getOid());

        // ** STUDENT 1 **
        m_grid.gotoRow(m_student1.getOid());

        double t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        double t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        double t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        double t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        double t1Avg = (t11Avg + t12Avg) / 2;
        double t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 2 **
        m_grid.gotoRow(m_student2.getOid());

        t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 3 **
        m_grid.gotoRow(m_student3.getOid());

        t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components.
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // Remove the average calculator procedure
        cleanupGradeAverageCalculationProcedure();
    }


    /**
     * Test "Average of Standards" by overriding the calculator with a GradeAverageCalculation
     * procedure
     * specifying using the StandarsAverageCalculators.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws X2BaseException exception
     */
    @Test
    public void testOverallAverageWithTrendsProcedure() throws IOException, X2BaseException {
        // Create the grade average calculation procedure
        createGradeAverageCalculationProcedure(true);

        // Get the two term based Overall Average Calculators
        String standardsAverage1Id = StandardsOverallAverageCalculator.getIdentifier(m_term1.getOid());
        String standardsAverage2Id = StandardsOverallAverageCalculator.getIdentifier(m_term2.getOid());

        // Get Average Calculators for each term and each standard within the term
        String avg11AverageId = StandardsTrendCalculator.getIdentifier(m_term1.getOid(), m_standard1.getOid());
        String avg12AverageId = StandardsTrendCalculator.getIdentifier(m_term1.getOid(), m_standard2.getOid());
        String avg21AverageId = StandardsTrendCalculator.getIdentifier(m_term2.getOid(), m_standard1.getOid());
        String avg22AverageId = StandardsTrendCalculator.getIdentifier(m_term2.getOid(), m_standard2.getOid());

        // ** STUDENT 1 **
        m_grid.gotoRow(m_student1.getOid());

        double t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        double t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        double t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        double t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        double t1Avg = (t11Avg + t12Avg) / 2;
        double t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 2 **
        m_grid.gotoRow(m_student2.getOid());

        t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // ** STUDENT 3 **
        m_grid.gotoRow(m_student3.getOid());

        t11Avg = m_grid.getAverageNumeric(avg11AverageId).doubleValue();
        t12Avg = m_grid.getAverageNumeric(avg12AverageId).doubleValue();
        t21Avg = m_grid.getAverageNumeric(avg21AverageId).doubleValue();
        t22Avg = m_grid.getAverageNumeric(avg22AverageId).doubleValue();

        // Average of averages is simple (sum of all components) / (number of components)
        t1Avg = (t11Avg + t12Avg) / 2;
        t2Avg = (t21Avg + t22Avg) / 2;
        // Expect averages within 0.05 differential since we don't have access to the unrounded
        // components.
        assertTrue(Math.abs(t1Avg - m_grid.getAverageNumeric(standardsAverage1Id).doubleValue()) <= 0.05);
        assertTrue(Math.abs(t2Avg - m_grid.getAverageNumeric(standardsAverage2Id).doubleValue()) <= 0.05);

        // Remove the average calculator procedure
        cleanupGradeAverageCalculationProcedure();
    }



    /**
     * Sets the up.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.test.X2BaseTest#setUp()
     */
    @Override
    public void setUp() throws Exception {
        super.setUp();
        createTestData();
    }


    /**
     * Sets the reporting score.
     *
     * @param student SisStudent
     * @param rsa ReportingStandardAssignment
     * @param score String
     */
    private void setReportingScore(SisStudent student,
                                   ReportingStandardAssignment rsa,
                                   String score,
                                   GradebookScore scoreBean) {
        ReportingStandardScore rsScore = new ReportingStandardScore(getUser().getPersistenceKey());
        rsScore.setSectionReportingStandardOid(rsa.getSectionReportingStandardOid());
        rsScore.setScore(score);
        rsScore.setGradebookScoreOid(scoreBean.getOid());
        saveTemporaryBean(m_broker, rsScore);

        m_grid.setReportingStandardScore(student.getOid(), rsa.getColumnDefinitionOid(), rsScore);
    }

    /**
     * Creates, temporarily saves, and returns a test assignment based on the passed parameters.
     *
     * @param code String
     * @param categoryOid String
     * @param weight double
     * @param totalPoints double
     * @param gradeTermOid String
     * @param gradeScaleOid String
     * @param dueDate PlainDate
     * @return GradebookColumnDefinition
     */
    private GradebookColumnDefinition createAssignment(String code,
                                                       String categoryOid,
                                                       double weight,
                                                       double totalPoints,
                                                       String gradeTermOid,
                                                       String gradeScaleOid,
                                                       PlainDate dueDate) {
        GradebookColumnDefinition column = new GradebookColumnDefinition(getUser().getPersistenceKey());
        column.setColumnCode(code);
        column.setColumnName(code);
        column.setColumnTypeOid(categoryOid);
        column.setColumnWeight(new BigDecimal(weight));
        column.setTotalPoints(new BigDecimal(totalPoints));
        column.setGradeTermOid(gradeTermOid);
        column.setGradeScaleOid(gradeScaleOid);
        column.setMasterScheduleOid(m_section.getOid());
        column.setStaffOid(m_staff.getOid());
        column.setDateAssigned(dueDate);
        column.setDateDue(dueDate);
        saveTemporaryBeanForced(m_broker, column);

        return column;
    }

    /**
     * Creates, temporarily saves, and returns a test category based on the passed parameters.
     *
     * @param code String
     * @return GradebookColumnType
     */
    private GradebookColumnType createCategory(String code) {
        GradebookColumnType category = new GradebookColumnType(getUser().getPersistenceKey());
        category.setColumnType(code);
        category.setColumnTypeDescription(code);
        category.setMasterScheduleOid(m_section.getOid());
        category.setStaffOid(m_staff.getOid());
        saveTemporaryBeanForced(m_broker, category);

        return category;
    }


    /**
     * Creates, temporarily saves, and returns a student schedule for the passed student and
     * <code>m_section</code>.
     *
     * @param studentOid String
     * @return StudentSchedule
     */
    private StudentSchedule createStudentSchedule(String studentOid) {
        StudentSchedule studentSchedule = new StudentSchedule(getUser().getPersistenceKey());
        studentSchedule.setSectionOid(m_section.getOid());
        studentSchedule.setStudentOid(studentOid);
        saveTemporaryBeanForced(m_broker, studentSchedule);

        return studentSchedule;
    }

    /**
     * Creates test objects required by the test cases.
     */
    private void createTestData() {
        m_broker = new ModelBroker(getPrivilegeSet());

        m_staff = X2BaseBean.newInstance(SisStaff.class, getUser().getPersistenceKey());
        saveTemporaryBeanForced(m_broker, m_staff);

        m_school = X2BaseBean.newInstance(SisSchool.class, getUser().getPersistenceKey());
        m_school.setOrganization1Oid("*dst");
        saveTemporaryBeanForced(m_broker, m_school);

        // 2 terms
        GradeTermDefinition termDefinition = new GradeTermDefinition(getUser().getPersistenceKey());
        saveTemporaryBeanForced(m_broker, termDefinition);

        // Create grade term dates and such.
        createGradeTerms(termDefinition);

        // Create a test section and staff
        TranscriptDefinition transcriptDefinition = new TranscriptDefinition(getUser().getPersistenceKey());
        transcriptDefinition.setGradeTermDefinitionOid(termDefinition.getOid());
        saveTemporaryBeanForced(m_broker, transcriptDefinition);

        SchoolCourse course = new SchoolCourse(getUser().getPersistenceKey());
        course.setTranscriptDefinitionOid(transcriptDefinition.getOid());
        course.setSchoolOid(m_school.getOid());
        saveTemporaryBeanForced(m_broker, course);

        Schedule schedule = new Schedule(getUser().getPersistenceKey());
        schedule.setSchoolOid(m_school.getOid());
        saveTemporaryBeanForced(m_broker, schedule);

        m_section = new MasterSchedule(getUser().getPersistenceKey());
        course.setMasterType(SchoolCourse.MASTER_TYPE_CLASS);
        m_section.setPrimaryStaffOid(m_staff.getOid());
        m_section.setSchoolCourseOid(course.getOid());
        m_section.setScheduleOid(schedule.getOid());
        saveTemporaryBeanForced(m_broker, m_section);

        // Create 3 students and enroll them in the test section
        m_student1 = getTestObject(SisStudent.class, m_broker);
        m_student1.setEnrollmentStatus("Active");
        m_broker.saveBeanForced(m_student1);

        m_student2 = getTestObject(SisStudent.class, m_broker);
        m_student2.setEnrollmentStatus("Active");
        m_broker.saveBeanForced(m_student2);

        m_student3 = getTestObject(SisStudent.class, m_broker);
        m_student3.setEnrollmentStatus("Active");
        m_broker.saveBeanForced(m_student3);

        createStudentSchedule(m_student1.getOid());
        createStudentSchedule(m_student2.getOid());
        createStudentSchedule(m_student3.getOid());

        // Create rubric rating scale
        createRatingScale();

        // Create rubric criterion for the standards
        createRubricCriterion();

        // Create Section Reporting Standards
        createSectionReportingStandards();

        // Create test categories and assignments
        m_testCategory = createCategory("Test");

        // Term 1 is t -20 -> t-11
        Calendar d1Date = Calendar.getInstance();
        d1Date.add(Calendar.DATE, -15);

        // Term 2 is t-10 -> t
        Calendar d2Date = Calendar.getInstance();
        d2Date.add(Calendar.DATE, -5);

        // Term 1 - Create assignments
        m_test11 = createAssignment("Test11", m_testCategory.getOid(), 1, 1, m_term1.getOid(), null,
                new PlainDate(d1Date.getTime()));
        d1Date.add(Calendar.DATE, 1);
        m_test12 = createAssignment("Test12", m_testCategory.getOid(), 1, 1, m_term1.getOid(), null,
                new PlainDate(d1Date.getTime()));
        d1Date.add(Calendar.DATE, 1);
        m_test13 = createAssignment("Test13", m_testCategory.getOid(), 1, 1, m_term1.getOid(), null,
                new PlainDate(d1Date.getTime()));

        // Term 2 - Create assignments
        m_test21 = createAssignment("Test21", m_testCategory.getOid(), 1, 1, m_term2.getOid(), null,
                new PlainDate(d2Date.getTime()));
        d2Date.add(Calendar.DATE, 1);
        m_test22 = createAssignment("Test22", m_testCategory.getOid(), 1, 1, m_term2.getOid(), null,
                new PlainDate(d2Date.getTime()));
        d2Date.add(Calendar.DATE, 1);
        m_test23 = createAssignment("Test23", m_testCategory.getOid(), 1, 1, m_term2.getOid(), null,
                new PlainDate(d2Date.getTime()));

        List<GradebookColumnDefinition> assignments =
                Arrays.asList(m_test11, m_test12, m_test13, m_test21, m_test22, m_test23);

        // Create section reporting standard assignments for each Gradebook assignment.
        for (GradebookColumnDefinition assignment : assignments) {
            createReportingStandardAssignments(assignment);
        }

        // For each student we need 6 * 2 scores. First six first term, second six second term.
        String[][] scores = new String[][] {
                { // Term 1
                        "1.0", "1.5",
                        "2.0", "1.5",
                        "3.0", "1.5",
                        // Term 2
                        "1.5", "1.5",
                        "2.5", "1.5",
                        "3.5", "4.0"},
                // Student 2
                {// Term 1
                        "2.0", "2.5",
                        "3.0", "2.5",
                        "4.0", "2.5",
                        // Term 2
                        "3.5", "2.5",
                        "3.5", "2.5",
                        "4.0", "4.1"},
                // Student 3
                {// Term 1
                        "3.0", "4.1",
                        "3.0", "4.1",
                        "3.0", "4.1",
                        // Term 2
                        "1.0", "2.0",
                        "1.0", "2.0",
                        "1.0", "2.0"},
        };

        List<SectionReportingStandard> standards = Arrays.asList(m_standard1, m_standard2);

        // Finally, create a grid and set scores
        m_grid = new ScoreGrid(m_section, 2, null, AverageCalculator.CALCULATION_MODE_WEIGHT_ALL, true,
                standards, m_broker);

        // Fill in the scores for the 3 students across the 6 assignments each with 2 standard
        // scores
        int i = 0;
        for (SisStudent student : Arrays.asList(m_student1, m_student2, m_student3)) {
            int j = 0;
            for (GradebookColumnDefinition assignment : assignments) {

                GradebookScore scoreBean = getScoreBean(student, assignment);

                for (ReportingStandardAssignment rsa : assignment.getReportingStandardAssignments()) {
                    setReportingScore(student, rsa, scores[i][j], scoreBean);
                    j++;
                }
            }
            i++;
        }
    }

    private GradebookScore getScoreBean(SisStudent student, GradebookColumnDefinition assignment) {
        GradebookScore scoreBean = new GradebookScore(getUser().getPersistenceKey());
        scoreBean.setStudentOid(student.getOid());
        scoreBean.setColumnDefinitionOid(assignment.getOid());
        scoreBean.setScore("");
        saveTemporaryBeanForced(m_broker, scoreBean);
        return scoreBean;
    }

    /**
     * Creates the grade average calculation procedure.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws X2BaseException exception
     */
    private void createGradeAverageCalculationProcedure(boolean useTrend) throws IOException, X2BaseException {

        // Point to the existing procedure that has been customized
    	
    	String procedureLocation = "/OverallStandardsAverageUsingAveragesCalculatorProcedure.java";
    	if(useTrend) {
    		procedureLocation = "/OverallStandardsAverageCalculatorProcedure.java";
    	}
    	
    	InputStream stream = StandardsAverageCalculatorTest.class.getResourceAsStream(procedureLocation);
        Assert.assertNotNull(stream);
    	
        byte[] javaSource = IOUtils.toByteArray(stream);
        byte[] compiledSource = ByteArrayCompiler.compile(javaSource, AppGlobals.getClasspath());

        // Create the ToolSourceCode for the procedure
        ToolSourceCode sourceCode = X2BaseBean.newInstance(ToolSourceCode.class, getUser().getPersistenceKey());
        sourceCode.setSourceCode(new String(javaSource));
        sourceCode.setCompiledCode(compiledSource);
        saveTemporaryBean(m_broker, sourceCode);

        // Create the procedure
        SisProcedure procedure = X2BaseBean.newInstance(SisProcedure.class, getUser().getPersistenceKey());
        procedure.setCategory("Average Calculation");
        procedure.setId("TEST-ID");
        procedure.setWeight(1);
        procedure.setSourceCodeOid(sourceCode.getOid());
        saveTemporaryBeanForced(m_broker, procedure);

        // Create the GradeAverageCalculation for the right category and attach the procedure
        m_averageCalculation = new GradeAverageCalculation(getUser().getPersistenceKey());
        m_averageCalculation.setSchoolOid(m_school.getOid());
        m_averageCalculation
                .setId(GradeAverageCalculation.GradeAverageCalculationType.OVERALL_STANDARDS_AVERAGE.ordinal());
        m_averageCalculation.setProcedureOid(procedure.getOid());
        m_averageCalculation.setName("OSA");
        m_averageCalculation.setDescription("Overall Standards Averages");
        m_averageCalculation.setGradeColumnHeader("OSA Header");
        saveTemporaryBean(m_broker, m_averageCalculation);
    }

    /**
     * Cleanup grade average calculation procedure.
     */
    private void cleanupGradeAverageCalculationProcedure() {
        // Remove the procedure from the grade average calculation
        m_averageCalculation.setProcedureOid(null);
        saveTemporaryBean(m_broker, m_averageCalculation);
    }

    /**
     * Creates the grade terms.
     *
     * @param termDefinition GradeTermDefinition
     */
    private void createGradeTerms(GradeTermDefinition termDefinition) {

        m_term1 = new GradeTerm(getUser().getPersistenceKey());
        m_term1.setGradeTermDefinitionOid(termDefinition.getOid());
        m_term1.setGradeTermId("T1");
        m_term1.setGradeTermNum(1);
        saveTemporaryBeanForced(m_broker, m_term1);

        m_term2 = new GradeTerm(getUser().getPersistenceKey());
        m_term2.setGradeTermDefinitionOid(termDefinition.getOid());
        m_term2.setGradeTermId("T2");
        m_term2.setGradeTermNum(2);
        saveTemporaryBeanForced(m_broker, m_term2);

        // T1 is from -20 to -11 offset of today
        Calendar t1Start = Calendar.getInstance();
        t1Start.add(Calendar.DATE, -20);
        Calendar t1End = Calendar.getInstance();
        t1End.add(Calendar.DATE, -11);

        // T2 is from -10 to today
        Calendar t2Start = Calendar.getInstance();
        t2Start.add(Calendar.DATE, -10);
        Calendar t2End = Calendar.getInstance();

        GradeTermDate d1 = new GradeTermDate(getUser().getPersistenceKey());
        d1.setStartDate(new PlainDate(t1Start.getTime()));
        d1.setEndDate(new PlainDate(t1End.getTime()));
        d1.setGradeTermOid(m_term1.getOid());
        d1.setSchoolOid(m_school.getOid());
        saveTemporaryBeanForced(m_broker, d1);

        GradeTermDate d2 = new GradeTermDate(getUser().getPersistenceKey());
        d2.setStartDate(new PlainDate(t2Start.getTime()));
        d2.setEndDate(new PlainDate(t2End.getTime()));
        d2.setGradeTermOid(m_term2.getOid());
        d2.setSchoolOid(m_school.getOid());
        saveTemporaryBeanForced(m_broker, d2);
    }

    /**
     * Creates the reporting standard assignments.
     *
     * @param column GradebookColumnDefinition
     */
    private void createReportingStandardAssignments(GradebookColumnDefinition column) {
        ReportingStandardAssignment a1 = new ReportingStandardAssignment(getUser().getPersistenceKey());
        a1.setColumnDefinitionOid(column.getOid());
        a1.setSectionReportingStandardOid(m_standard1.getOid());
        saveTemporaryBean(m_broker, a1);

        ReportingStandardAssignment a2 = new ReportingStandardAssignment(getUser().getPersistenceKey());
        a2.setColumnDefinitionOid(column.getOid());
        a2.setSectionReportingStandardOid(m_standard2.getOid());
        saveTemporaryBean(m_broker, a2);
    }

    /**
     * Creates the section reporting standards.
     */
    private void createSectionReportingStandards() {
        m_standard1 = new SectionReportingStandard(getUser().getPersistenceKey());
        m_standard1.setColumnHeader("Header 1");
        m_standard1.setMasterScheduleOid(m_section.getOid());
        m_standard1.setName("SRS1");
        m_standard1.setRubricCriterionOid(m_criterion1.getOid());
        m_standard1.setRubricRatingScaleOid(m_scale.getOid());
        saveTemporaryBean(m_broker, m_standard1);

        m_standard2 = new SectionReportingStandard(getUser().getPersistenceKey());
        m_standard2.setColumnHeader("Header 2");
        m_standard2.setMasterScheduleOid(m_section.getOid());
        m_standard2.setName("SRS2");
        m_standard2.setRubricCriterionOid(m_criterion2.getOid());
        m_standard2.setRubricRatingScaleOid(m_scale.getOid());
        saveTemporaryBean(m_broker, m_standard2);
    }

    /**
     * Creates the rubric criterion.
     */
    private void createRubricCriterion() {
        /*
         * Creating the rubric in a single transaction allows a single RubricListener to
         * efficiently update the calculated fields on the RubricDefinition and RubricCriterion.
         */
        m_broker.beginTransaction();
        try {
            m_rubric = new RubricDefinition(getUser().getPersistenceKey());
            m_rubric.setDecimals(2);
            m_rubric.setId("AllTest");
            m_rubric.setMaximumPoints(new BigDecimal(5));
            m_rubric.setName("All Tests");
            saveTemporaryBeanForced(m_broker, m_rubric);

            m_criterion1 = createRubricCriterion("Crit1", "Criteria1", m_rubric.getOid(), 0);
            m_criterion1.setRubricRatingScaleOid(m_scale.getOid());
            saveTemporaryBeanForced(m_broker, m_criterion1);


            m_criterion2 = createRubricCriterion("Crit2", "Criteria2", m_rubric.getOid(), 1);
            saveTemporaryBeanForced(m_broker, m_criterion2);

        } finally {
            m_broker.commitTransaction();
        }
    }

    /**
     * Creates the rating scale.
     */
    public void createRatingScale() {

        m_scale = new RubricRatingScale(getUser().getPersistenceKey());
        m_scale.setName("RRS1");
        m_scale.setMaximumPoints(new BigDecimal(5.0));
        m_scale.setRatingCount(5);
        saveTemporaryBean(m_broker, m_scale);


        createRating("0.10", 0.10, 1.00, m_scale.getOid());
        createRating("1.10", 1.10, 2.00, m_scale.getOid());
        createRating("2.10", 2.10, 3.00, m_scale.getOid());
        createRating("3.10", 3.10, 4.00, m_scale.getOid());
        createRating("4.10", 4.10, 5.00, m_scale.getOid());
    }

    /**
     * Creates the rating.
     *
     * @param name String
     * @param cutOff double
     * @param points double
     * @param scaleOid String
     * @return RubricRatingScalePoints
     */
    public RubricRatingScalePoints createRating(String name, double cutOff, double points, String scaleOid) {
        RubricRatingScalePoints rsp =
                X2BaseBean.newInstance(RubricRatingScalePoints.class, getUser().getPersistenceKey());
        rsp.setCutoff(new BigDecimal(cutOff).setScale(2, RoundingMode.HALF_UP));
        rsp.setPoints(new BigDecimal(points).setScale(2, RoundingMode.HALF_UP));
        rsp.setName(name);
        rsp.setRubricRatingScaleOid(scaleOid);
        saveTemporaryBeanForced(m_broker, rsp);

        return rsp;
    }

    /**
     * Creates a rubric criterion.
     *
     * @param columnHeader String
     * @param name String
     * @param RBDOid String
     * @param seqNo int
     * @return RubricCriterion
     */
    private RubricCriterion createRubricCriterion(String columnHeader, String name, String RBDOid, int seqNo) {
        RubricCriterion criterion = new RubricCriterion(getUser().getPersistenceKey());
        criterion.setColumnHeader(columnHeader);
        criterion.setCriteriaWeight(new BigDecimal(1));
        criterion.setName(name);
        criterion.setRubricDefinitionOid(RBDOid);
        criterion.setSequenceNumber(seqNo);

        return criterion;
    }
}
