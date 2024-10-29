/*
 * ====================================================================
 *
 * X2 Development Corporation
 * A wholly owned subsidiary of Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.test.X2DataTest;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentGradePoint;
import com.x2dev.sis.procedures.StandardGpaProcedure;
import com.x2dev.sis.tools.procedures.GradePointAverageProcedure;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Test;

/**
 * TODO: Edit description.
 *
 * @author X2 Development Corporation
 */
public class StandardGPAProcedureCalcTest extends X2DataTest {
    private static final String BASELINE_STUDENT_OID = "stdBaseLn";

    private static final String GRADE_TERM_OID = "GTMavgtst08004";
    private static final String GRADEPOINT_AVERAGE_DEF_OID = "GPDavgtst00000";

    // for future testing ... to ensure that the student record was updated.
    // private static final String GPA_FIELD = "fddX2000000365"; // stdFieldB010
    // private static final String GPA_RANK = "fddX2000000390"; // stdFieldB035

    private static final String ORGANIZAION_OID = "*dst";
    private static final String SCHOOL_OID = "SKLavgtst01001";
    private static final String SCHOOL_YEAR_CONTEXT_OID = "ctxTSTcurrent";
    private static final String STUDENT_OID = "stdavgtst0SCBI";

    private static final String TEST_DATA_FILE = "standardGPAProcedureCalcDataSet.xml";

    private ModelBroker m_broker = null;
    private GradePointAverageProcedure m_gpaProcedure = null;
    private SisSchool m_school;

    /**
     * Sets the up.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.test.X2BaseTest#setUp()
     */
    @Override
    public void setUp() throws Exception {
        super.setUp();

        m_broker = new ModelBroker(getPrivilegeSet());

        Organization organization = (Organization) m_broker.getBeanByOid(Organization.class, ORGANIZAION_OID);
        m_school = (SisSchool) m_broker.getBeanByOid(SisSchool.class, SCHOOL_OID);
        DistrictSchoolYearContext context = (DistrictSchoolYearContext) m_broker
                .getBeanByOid(DistrictSchoolYearContext.class, SCHOOL_YEAR_CONTEXT_OID);
        GradePointAverageDefinition gpaDefinition = (GradePointAverageDefinition) m_broker
                .getBeanByOid(GradePointAverageDefinition.class, GRADEPOINT_AVERAGE_DEF_OID);

        Boolean activeOnly = Boolean.TRUE;
        String gradeTermOid = GRADE_TERM_OID;
        Integer yog = Integer.valueOf(2005);

        m_gpaProcedure = new StandardGpaProcedure();

        Method method = GradePointAverageProcedure.class.getDeclaredMethod("initialize",
                GradePointAverageDefinition.class,
                Organization.class,
                SisSchool.class,
                boolean.class,
                String.class,
                Integer.class,
                DistrictSchoolYearContext.class,
                X2Broker.class);
        method.setAccessible(true);
        method.invoke(m_gpaProcedure,
                gpaDefinition,
                organization,
                m_school,
                activeOnly,
                gradeTermOid,
                yog,
                context,
                m_broker);

    }

    /**
     * Tests recalculating the StudentGradePoints for all students.
     */
    @Test
    public void testGPTRecalculationForAll() {
        X2Criteria studentGradePointCriteria = new X2Criteria();
        studentGradePointCriteria.addEqualTo(StudentGradePoint.COL_GRADE_POINT_AVERAGE_DEFINITION_OID,
                GRADEPOINT_AVERAGE_DEF_OID);
        studentGradePointCriteria.addNotLike(StudentGradePoint.COL_STUDENT_OID, BASELINE_STUDENT_OID + "%");
        QueryByCriteria studentGradePointQuery =
                new QueryByCriteria(StudentGradePoint.class, studentGradePointCriteria);

        try {
            runGPAProcedure(null);

            /*
             * Validate base line student data with the newly calculated student data
             */
            X2Criteria baselineGradePointCriteria = new X2Criteria();
            baselineGradePointCriteria.addLike(StudentGradePoint.COL_STUDENT_OID, BASELINE_STUDENT_OID + "%");
            QueryByCriteria baselineGradePointQuery =
                    new QueryByCriteria(StudentGradePoint.class, baselineGradePointCriteria);
            Map<String, Map<String, StudentGradePoint>> baselineGradePoints =
                    m_broker.getNestedMapByQuery(baselineGradePointQuery, StudentGradePoint.COL_DISTRICT_CONTEXT_OID,
                            StudentGradePoint.COL_STUDENT_OID, 5, 5);
            Collection<StudentGradePoint> studentGradePoints = m_broker.getCollectionByQuery(studentGradePointQuery);
            for (StudentGradePoint studentGradePoint : studentGradePoints) {
                /*
                 * Get the baseline studentGradePoint for the matching school year
                 */
                StudentGradePoint baselineGradePoint =
                        baselineGradePoints.get(studentGradePoint.getDistrictContextOid())
                                .get(BASELINE_STUDENT_OID + studentGradePoint.getStudentOid().substring(9));
                assertNotNull("Baseline data is null", baselineGradePoint);

                assertEquals("GPAs are not equal", studentGradePoint.getGpa().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getGpa().setScale(5, RoundingMode.UP));
                assertEquals("Grade Pointss are not equal",
                        studentGradePoint.getGradePoints().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getGradePoints().setScale(5, RoundingMode.UP));
                assertEquals("Course Weights are not equal",
                        studentGradePoint.getCourseWeight().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCourseWeight().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeGradePoints are not equal",
                        studentGradePoint.getCumulativeGradePoints().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeGradePoints().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeCourseWeights are not equal",
                        studentGradePoint.getCumulativeCourseWeight().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeCourseWeight().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeGPAs are not equal",
                        studentGradePoint.getCumulativeGpa().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeGpa().setScale(5, RoundingMode.UP));

                /*
                 * TODO - uncomment when the 'clearStudentGradePoints' method no longer deletes
                 * anything
                 */
                // assertEquals("OIDs have not changed", studentGradePoint.getOid(),
                // baselineGradePoint.getOid());
            }

        } finally {
            /*
             * delete the newly created data for these students
             */
            m_broker.deleteByQuery(studentGradePointQuery);
        }
    }

    /**
     * Tests recalculating the StudentGradePoints for the student "Alexander, Krystopher"
     * OID = stdavgtst0SCBI.
     */
    @Test
    public void testGPTRecalculationForStudent() {
        X2Criteria studentGradePointCriteria = new X2Criteria();
        studentGradePointCriteria.addEqualTo(StudentGradePoint.COL_STUDENT_OID, STUDENT_OID);
        QueryByCriteria studentGradePointQuery =
                new QueryByCriteria(StudentGradePoint.class, studentGradePointCriteria);

        try {
            /*
             * Validate that the data is the initial data from the data set
             */
            Collection<StudentGradePoint> studentGradePoints = m_broker.getCollectionByQuery(studentGradePointQuery);
            for (StudentGradePoint studentGradePoint : studentGradePoints) {
                assertEquals("GPA == 4.00000", studentGradePoint.getGpa(),
                        BigDecimal.valueOf(4.0).setScale(5, RoundingMode.UP));
                assertEquals("DateCalculated == Jan 1, 2000", studentGradePoint.getDateCalculated(),
                        java.sql.Date.valueOf("2000-01-01"));
            }

            runGPAProcedure(STUDENT_OID);

            /*
             * Validate base line student data with the newly calculated student data
             */
            X2Criteria baselineGradePointCriteria = new X2Criteria();
            baselineGradePointCriteria.addEqualTo(StudentGradePoint.COL_STUDENT_OID,
                    BASELINE_STUDENT_OID + STUDENT_OID.substring(9));
            QueryByCriteria baselineGradePointQuery =
                    new QueryByCriteria(StudentGradePoint.class, baselineGradePointCriteria);
            Map<String, StudentGradePoint> baselineGradePoints =
                    m_broker.getMapByQuery(baselineGradePointQuery, StudentGradePoint.COL_DISTRICT_CONTEXT_OID, 5);
            studentGradePoints = m_broker.getCollectionByQuery(studentGradePointQuery);
            for (StudentGradePoint studentGradePoint : studentGradePoints) {
                /*
                 * Get the baseline studentGradePoint for the matching school year
                 */
                StudentGradePoint baselineGradePoint =
                        baselineGradePoints.get(studentGradePoint.getDistrictContextOid());
                assertNotNull("Baseline data is null", baselineGradePoint);

                assertEquals("GPAs are not equal", studentGradePoint.getGpa().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getGpa().setScale(5, RoundingMode.UP));
                assertEquals("Grade Pointss are not equal",
                        studentGradePoint.getGradePoints().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getGradePoints().setScale(5, RoundingMode.UP));
                assertEquals("Course Weights are not equal",
                        studentGradePoint.getCourseWeight().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCourseWeight().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeGradePoints are not equal",
                        studentGradePoint.getCumulativeGradePoints().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeGradePoints().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeCourseWeights are not equal",
                        studentGradePoint.getCumulativeCourseWeight().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeCourseWeight().setScale(5, RoundingMode.UP));
                assertEquals("CumulativeGPAs are not equal",
                        studentGradePoint.getCumulativeGpa().setScale(5, RoundingMode.UP),
                        baselineGradePoint.getCumulativeGpa().setScale(5, RoundingMode.UP));

                /*
                 * TODO - uncomment when the 'clearStudentGradePoints' method no longer deletes
                 * anything
                 */
                // assertEquals("OIDs have not changed", studentGradePoint.getOid(),
                // baselineGradePoint.getOid());
            }

        } finally {
            /*
             * delete the newly created data for this student
             */
            m_broker.deleteByQuery(studentGradePointQuery);
        }
    }

    /**
     * Runs the <code>populateStudentGradePoints()</code> and <code>populateGpaHistory()</code>
     * methods on the <code>StandardGpaProcedure</code>.
     *
     * @param studentOID String
     */
    private void runGPAProcedure(String studentOID) {
        LinkedList validationErrors = new LinkedList();

        try {
            Method method;
            method = GradePointAverageProcedure.class.getDeclaredMethod("clearStudentGradePoints", String.class,
                    SisSchool.class);
            method.setAccessible(true);
            method.invoke(m_gpaProcedure, studentOID, m_school);

            method = GradePointAverageProcedure.class.getDeclaredMethod("populateStudentGradePoints", String.class);
            method.setAccessible(true);
            Collection errors = (Collection) method.invoke(m_gpaProcedure, studentOID);
            validationErrors.addAll(errors);

            // Only update GPA History when we are updating student records
            boolean updateStudent = true;
            if (updateStudent) {
                method = GradePointAverageProcedure.class.getDeclaredMethod("clearGpaHistory");
                method.setAccessible(true);
                method.invoke(m_gpaProcedure);

                method = GradePointAverageProcedure.class.getDeclaredMethod("populateGpaHistory");
                method.setAccessible(true);
                errors = (Collection) method.invoke(m_gpaProcedure);
                validationErrors.addAll(errors);
            }
        } catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException e) {
            e.printStackTrace();
        }
        AppGlobals.logValidationErrors(validationErrors);



    }

    /**
     * Gets the data set file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.test.X2DataTest#getDataSetFile()
     */
    @Override
    protected String getDataSetFile() {
        return TEST_DATA_FILE;
    }


}
