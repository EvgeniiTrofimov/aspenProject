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
package com.x2dev.procedures;

import com.follett.fsc.core.k12.beans.BeanConstants;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.business.GradePointCalculation;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.procedures.StandardGpaProcedure;
import com.x2dev.sis.tools.procedures.GradePointAverageProcedure;
import com.x2dev.utils.ObjectUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import org.junit.Assert;
import org.junit.Test;
import mockit.Deencapsulation;
import mockit.Mocked;
import mockit.NonStrictExpectations;
import mockit.Tested;
import mockit.Verifications;

/**
 * The Class StandardGPAProcedureTest.
 */
public class StandardGPAProcedureTest {
    @Tested
    StandardGpaProcedure tested;
    @Mocked
    GradePointAverageDefinition gpaDef;
    @Mocked
    GradePointAverageProcedure gpaProcedure;
    @Mocked
    GradesManager gradesManager;
    @Mocked
    GradeScale scale;
    @Mocked
    GradeScaleGradeDefinition gradeDefinition;
    @Mocked
    GradeScaleGradeDefinition parentGradeDefinition;

    /**
     * Test calculate GP adoesnt change without weight.
     */
    @Test
    public void testCalculateGPAdoesntChangeWithoutWeight() {
        double points = 10;
        double weight = -1;
        double gpa = tested.calculateGPA(points, weight);
        Assert.assertEquals("Gpa should be zero!", Double.valueOf(0), Double.valueOf(gpa));
    }

    /**
     * Test calculate GP adoesnt changes with weight.
     */
    @Test
    public void testCalculateGPAdoesntChangesWithWeight() {
        double points = 10;
        double weight = 1;
        double gpa = tested.calculateGPA(points, weight);
        Assert.assertEquals("Gpa should 10!", Double.valueOf(10), Double.valueOf(gpa));
    }

    /**
     * Test calculate GPA accepts bad data.
     */
    @Test
    public void testCalculateGPAAcceptsBadData() {
        double points = -10;
        double weight = 1;
        double gpa = tested.calculateGPA(points, weight);
        Assert.assertEquals("Gpa should -10!", Double.valueOf(-10), Double.valueOf(gpa));
    }

    /**
     * Test something with calculate points.
     */
    @Test
    public void testSomethingWithCalculatePoints() {
        final String grade = "A";
        final BigDecimal numericValue = new BigDecimal(90);

        new NonStrictExpectations(grade, numericValue, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getNumericValue(grade, scale, null, null);
                result = numericValue;
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String academicLevel = null;
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;

        GradePointCalculation returnValue =
                tested.calculatePoints(null, grade, null, scale, courseCredit, courseWeight, academicLevel,
                        scheduleTerm, columnOid, null, null);

        returnValue.getPoints();
    }

    /**
     * Test calculate points calls to get numeric when letter is sent.
     */
    @Test
    public void testCalculatePointsCallsToGetNumericWhenLetterIsSent() {

        new NonStrictExpectations(ObjectUtils.class) {
            {
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        final String grade = "A";
        String academicLevel = null;
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        tested.calculatePoints(null, grade, null, scale, courseCredit, courseWeight, academicLevel,
                scheduleTerm, columnOid, null, null);

        new Verifications() {
            {
                gradesManager.getNumericValue(grade, scale, (SisSchool) (any), null);
                times = 1;
            }
        };
    }

    /**
     * Test calculate points calls to get letter when grade is sent.
     */
    @Test
    public void testCalculatePointsCallsToGetLetterWhenGradeIsSent() {
        new NonStrictExpectations(ObjectUtils.class) {
            {
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        final String grade = "90";
        String academicLevel = null;
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;

        tested.calculatePoints(null, grade, null, scale, courseCredit, courseWeight, academicLevel,
                scheduleTerm, columnOid, null, null);

        new Verifications() {
            {
                gradesManager.getLetterValue(new BigDecimal(grade), scale, (SisSchool) (any), null);
                times = 1;
            }
        };
    }

    /**
     * Test calculate points gets points when grade definition exists.
     */
    @Test
    public void testCalculatePointsGetsPointsWhenGradeDefinitionExists() {
        final String grade = "90";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;

                gradeDefinition.getParentGradeScaleGrade();
                result = null;

                parentGradeDefinition.getParentGradeScaleGrade();
                result = null;

                gradeDefinition.getOid();
                result = "gsgdOID";

                ObjectUtils.match(any, any);
                result = Boolean.FALSE;
            }
        };

        final String academicLevel = "level";
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        tested.calculatePoints(null, grade, null, scale, courseCredit, courseWeight, academicLevel,
                scheduleTerm, columnOid, null, null);


        new Verifications() {
            {
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                times = 1;
            }
        };
    }


    /**
     * Test calculate points gets points when grade definition with parent exists.
     */
    @Test
    public void testCalculatePointsGetsPointsWhenGradeDefinitionParentExists() {
        final String grade = "90";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;

                gradeDefinition.getParentGradeScaleGrade();
                result = parentGradeDefinition;

                parentGradeDefinition.getParentGradeScaleGrade();
                result = null;

                gradeDefinition.getParentGradeScaleGradeOid();
                result = "gsgPOID";

                parentGradeDefinition.getParentGradeScaleGradeOid();
                result = null;

                gradeDefinition.getOid();
                result = "gsgdOID";

                parentGradeDefinition.getOid();
                result = "gsgPOID";

                ObjectUtils.match(any, any);
                result = false;
            }
        };

        final String academicLevel = "level";
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;

        tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);


        new Verifications() {
            {
                Deencapsulation.invoke(tested, "getPoints", "gsgPOID", academicLevel);
                times = 1;
            }
        };
    }

    /**
     * Test calculate points gets points when grade definition does not exists.
     */
    @Test
    public void testCalculatePointsGetsPointsWhenGradeDefinitionDoesNotExists() {
        final String grade = "90";

        new NonStrictExpectations(grade) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = null;
            }
        };

        final String academicLevel = "level";
        String scheduleTerm = null;
        boolean currentYear = false;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        tested.calculatePoints(grade, academicLevel, scheduleTerm, scale, currentYear, courseWeight, courseCredit,
                columnOid);

        new Verifications() {
            {
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                times = 0;
            }
        };
    }

    /**
     * Test calculate points sets scale with grade points.
     *
     * @param bd BigDecimal
     */
    @Test
    public void testCalculatePointsSetsScaleWithGradePoints(@Mocked final BigDecimal bd) {
        final String grade = "90";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        final String academicLevel = "level";
        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);

        new Verifications() {
            {
                bd.setScale(tested.getPointValueScale(), RoundingMode.HALF_UP);
                times = 1;
            }
        };
    }

    /**
     * Test calculate points doesnt set scale without grade points.
     *
     * @param bd BigDecimal
     */
    @Test
    public void testCalculatePointsDoesntSetScaleWithoutGradePoints(@Mocked final BigDecimal bd) {
        final String grade = "90";
        final String academicLevel = "level";

        new NonStrictExpectations(grade) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getParentGradeScaleGrade();
                result = null;

                parentGradeDefinition.getParentGradeScaleGrade();
                result = null;

                gradeDefinition.getOid();
                result = "gsgdOID";
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                result = null;
            }
        };

        String scheduleTerm = null;
        boolean currentYear = false;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        tested.calculatePoints(grade, academicLevel, scheduleTerm, scale, currentYear, courseWeight, courseCredit,
                columnOid);

        new Verifications() {
            {
                bd.setScale(tested.getPointValueScale(), RoundingMode.HALF_UP);
                times = 0;
            }
        };
    }

    /**
     * Test calculate points current year true effects term weight value.
     */
    @Test
    public void testCalculatePointsCurrentYearTrueEffectsTermWeightValue() {
        final String grade = "90";
        final String academicLevel = "level";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                ObjectUtils.match(any, any);
                result = true;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);


        Assert.assertFalse("This value is something",
                courseWeight.doubleValue() == returnValue.getWeight().doubleValue());
    }

    /**
     * Test calculate points current year false doesnt effect term weight value.
     */
    @Test
    public void testCalculatePointsCurrentYearFalseDoesntEffectTermWeightValue() {
        final String grade = "90";
        final String academicLevel = "level";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);

        Assert.assertEquals("Weight shouldn't change when the current year is false",
                Double.valueOf(courseWeight.doubleValue()), returnValue.getWeight());
    }

    /**
     * Test calculate points updates weight when weight by credit on.
     */
    @Test
    public void testCalculatePointsUpdatesWeightWhenWeightByCreditOn() {
        final String grade = "90";
        final String academicLevel = "level";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "weightByCredit");
                result = Boolean.TRUE;
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                result = new BigDecimal(50);
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);

        Assert.assertFalse("Credit weight is on, so value should have changed",
                courseWeight.doubleValue() == returnValue.getWeight().doubleValue());
    }

    /**
     * Test calculate points updates weight when weight by credit off.
     */
    @Test
    public void testCalculatePointsUpdatesWeightWhenWeightByCreditOff() {
        final String grade = "90";
        final String academicLevel = "level";

        new NonStrictExpectations(grade, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "weightByCredit");
                result = Boolean.FALSE;
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                result = new BigDecimal(50);
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);


        Assert.assertEquals("Weight should have changed", Double.valueOf(courseWeight.doubleValue()),
                returnValue.getWeight());
    }

    /**
     * Test calculate points updates points.
     */
    @Test
    public void testCalculatePointsUpdatesPoints() {
        final String grade = "90";
        final String academicLevel = "level";
        final BigDecimal score = new BigDecimal(50);


        new NonStrictExpectations(grade, score, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(grade, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", academicLevel);
                result = score;
                ObjectUtils.match(any, any);
                result = false;
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;

        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);

        Assert.assertFalse("The points returned should not equal the score",
                score.doubleValue() == returnValue.getPoints().doubleValue());
    }

    /**
     * Test calculate points updates points bump taken in.
     */
    @Test
    public void testCalculatePointsUpdatesPointsBumpTakenIn() {
        final String grade = "A";
        final String academicLevelOne = "level1";
        final String academicLevelTwo = "level2";
        final BigDecimal score = new BigDecimal(50);


        new NonStrictExpectations(score, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(anyString, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                gradesManager.getNumericValue(anyString, scale, null, null);
                result = score;
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", anyString);
                result = score;
                Deencapsulation.invoke(tested, "isBump", "gsgdOID", academicLevelOne);
                result = Boolean.TRUE;
                Deencapsulation.invoke(tested, "isBump", "gsgdOID", academicLevelTwo);
                result = Boolean.FALSE;
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValueNull = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevelOne, scheduleTerm, columnOid, null, null);

        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevelTwo, scheduleTerm, columnOid, null, null);


        Assert.assertFalse("Values should be different because weight was taken into account",
                returnValueNull.getPoints().doubleValue() == returnValue.getPoints().doubleValue());
    }

    /**
     * Test calculate points updates points bump but no numeric.
     */
    @Test
    public void testCalculatePointsUpdatesPointsBumpButNoNumeric() {
        final String grade = "NA";
        final String academicLevel = "level";
        final BigDecimal score = new BigDecimal(50);


        new NonStrictExpectations(score, ObjectUtils.class) {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradesManager");
                result = gradesManager;
                gradesManager.getGradeDefinition(anyString, scale, null, null);
                result = gradeDefinition;
                gradeDefinition.getOid();
                result = "gsgdOID";
                gradesManager.getNumericValue(anyString, scale, null, null);
                result = null;
                Deencapsulation.invoke(tested, "getPoints", "gsgdOID", anyString);
                result = score;
                Deencapsulation.invoke(tested, "isBump", "gsgdOID", academicLevel);
                result = Boolean.TRUE;
                ObjectUtils.match(any, any);
                result = false;
            }
        };

        String scheduleTerm = null;
        BigDecimal courseWeight = new BigDecimal(2.0);
        BigDecimal courseCredit = new BigDecimal(3.5);
        String columnOid = null;
        GradePointCalculation returnValue = tested.calculatePoints(null, grade, null, scale, courseCredit,
                courseWeight, academicLevel, scheduleTerm, columnOid, null, null);


        Assert.assertNull("Points should not be set on this code path", returnValue.getPoints());
    }

    /**
     * Test get course and student SQL respects inclusion type.
     */
    @Test
    public void testGetCourseAndStudentSQLRespectsInclusionType() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradePointAverageDefinition");
                result = gpaDef;
                gpaDef.getCourseMemberType();
                result = Integer.valueOf(BeanConstants.INCLUSION_TYPE_EXCLUDE);
                gpaDef.getOid();
                result = "theGpaDefOid";
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have AND CSK_OID NOT IN in sql", sql.contains("AND CSK_OID NOT IN"));
    }

    /**
     * Test get course and student SQL respects inclusion type out.
     */
    @Test
    public void testGetCourseAndStudentSQLRespectsInclusionTypeOut() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradePointAverageDefinition");
                result = gpaDef;
                gpaDef.getCourseMemberType();
                result = Integer.valueOf(BeanConstants.INCLUSION_TYPE_INCLUDE);
                gpaDef.getOid();
                result = "theGpaDefOid";
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have AND CSK_OID IN in sql", sql.contains("AND CSK_OID IN"));
    }

    /**
     * Test get course and student SQL respects member type.
     */
    @Test
    public void testGetCourseAndStudentSQLRespectsMemberType() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradePointAverageDefinition");
                result = gpaDef;
                gpaDef.getCourseMemberType();
                result = Integer.valueOf(GradePointAverageDefinition.COURSE_MEMBER_TYPE_GPA_INDICATOR);
                gpaDef.getOid();
                result = "theGpaDefOid";
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have AND CSK_GPA_IND = '1' in sql", sql.contains("AND CSK_GPA_IND = '1'"));
    }

    /**
     * Test get course and student SQL rank indicator.
     */
    @Test
    public void testGetCourseAndStudentSQLRankIndicator() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradePointAverageDefinition");
                result = gpaDef;
                gpaDef.getRankAllIndicator();
                result = Boolean.TRUE;
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertFalse("Should have AND STD_RANK_INCLUDE_IND = '1' in sql",
                sql.contains(" AND STD_RANK_INCLUDE_IND = '1'"));
    }

    /**
     * Test get course and student SQL rank indicator off.
     */
    @Test
    public void testGetCourseAndStudentSQLRankIndicatorOff() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getGradePointAverageDefinition");
                result = gpaDef;
                gpaDef.getRankAllIndicator();
                result = Boolean.FALSE;
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have AND STD_RANK_INCLUDE_IND = '1' in sql",
                sql.contains(" AND STD_RANK_INCLUDE_IND = '1'"));
    }

    /**
     * Test get course and student SQL yog.
     */
    @Test
    public void testGetCourseAndStudentSQLYog() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getYog");
                result = Integer.valueOf(1037);
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have AND STD_YOG = 1037 in sql", sql.contains(" AND STD_YOG = 1037"));
    }

    /**
     * Test get course and student SQL no yog.
     */
    @Test
    public void testGetCourseAndStudentSQLNoYog() {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "getYog");
                result = null;
            }
        };

        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertFalse("Should not have AND STD_YOG in sql", sql.contains(" AND STD_YOG = "));
    }

    /**
     * Test get course and student SQL district preference called on active only.
     *
     * @param sManager StudentManager
     */
    @Test
    public void testGetCourseAndStudentSQLDistrictPreferenceCalledOnActiveOnly(@Mocked StudentManager sManager) {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "isActiveOnly");
                result = Boolean.TRUE;
            }
        };

        Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        new Verifications() {
            {
                StudentManager.getActiveStudentDirectSQL((Organization) any, "STD_ENROLLMENT_STATUS");
                times = 1;
            }
        };
    }

    /**
     * Test get course and student SQL district preference not called on active only.
     *
     * @param sManager StudentManager
     */
    @Test
    public void testGetCourseAndStudentSQLDistrictPreferenceNotCalledOnActiveOnly(@Mocked StudentManager sManager) {
        new NonStrictExpectations() {
            {
                Deencapsulation.invoke(gpaProcedure, "isActiveOnly");
                result = Boolean.FALSE;
            }
        };

        Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        new Verifications() {
            {
                StudentManager.getActiveStudentDirectSQL((Organization) any, "STD_ENROLLMENT_STATUS");
                times = 0;
            }
        };
    }

    /**
     * Test get course and student SQL student oid added when there.
     */
    @Test
    public void testGetCourseAndStudentSQLStudentOidAddedWhenThere() {
        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", "studentOID");

        Assert.assertTrue("Should have STD_OID = 'studentOID' in sql", sql.contains("STD_OID = 'studentOID'"));
    }

    /**
     * Test get course and student SQL student oid empty.
     */
    @Test
    public void testGetCourseAndStudentSQLStudentOidEmpty() {
        String sql = Deencapsulation.invoke(tested, "getCourseAndStudentSQL", String.class);

        Assert.assertFalse("Should not have STD_OID = in sql", sql.contains("STD_OID = "));
    }

    /**
     * Test get grade range sql puts in dummy.
     *
     * @param sManager StudentManager
     */
    @Test
    public void testGetGradeRangeSqlPutsInDummy(@Mocked StudentManager sManager) {
        new NonStrictExpectations() {
            {
                StudentManager.getGradeLevelRange(anyString, anyString, (X2Broker) any);
                result = new ArrayList();
            }
        };

        String sql = Deencapsulation.invoke(tested, "getGradeRangeSQL");

        Assert.assertTrue("Should have dummy in the sql", sql.contains("dummy"));
    }

    /**
     * Test get grade range sql puts in list.
     *
     * @param sManager StudentManager
     */
    @Test
    public void testGetGradeRangeSqlPutsInList(@Mocked StudentManager sManager) {
        final ArrayList<String> resultList = new ArrayList<String>();
        resultList.add("1");
        resultList.add("2");
        resultList.add("3");

        new NonStrictExpectations(resultList) {
            {
                StudentManager.getGradeLevelRange(anyString, anyString, (X2Broker) any);
                result = resultList;
            }
        };

        String sql = Deencapsulation.invoke(tested, "getGradeRangeSQL");

        Assert.assertTrue("Should have TRN_GRADE_LEVEL IN ('1','2','3') in the sql",
                sql.contains("TRN_GRADE_LEVEL IN ('1','2','3')"));
    }
}
