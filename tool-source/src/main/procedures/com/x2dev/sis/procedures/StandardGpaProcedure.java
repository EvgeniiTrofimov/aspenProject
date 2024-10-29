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
package com.x2dev.sis.procedures;

import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.UnsupportedDatabaseException;
import com.follett.fsc.core.k12.beans.BeanConstants;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.sis.model.beans.GpaMemberGrade;
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradePointCalculation;
import com.x2dev.sis.tools.procedures.GradePointAverageProcedure;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for standard weighted GPA computation.
 *
 * @author X2 Development Corporation
 */
public class StandardGpaProcedure extends GradePointAverageProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Standard implementation of calculatePoints. Returns null by default. The other implementation
     * of calculatePoints below contains all logic.
     *
     * @param grade String
     * @param academicLevel String
     * @param scheduleTerm String
     * @param gradeScale GradeScale
     * @param currentYear boolean
     * @param courseWeight BigDecimal
     * @param courseCredit BigDecimal
     * @param columnOid String
     * @return GradePointCalculation
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#calculatePoints(java.
     *      lang.String, java.lang.String, java.lang.String,
     *      com.follett.fsc.core.k12.beans.GradeScale,
     *      boolean, java.math.BigDecimal, java.math.BigDecimal, java.lang.String)
     */
    @Override
    public GradePointCalculation calculatePoints(String grade,
                                                 String academicLevel,
                                                 String scheduleTerm,
                                                 GradeScale gradeScale,
                                                 boolean currentYear,
                                                 BigDecimal courseWeight,
                                                 BigDecimal courseCredit,
                                                 String columnOid) {
        return null;
    }

    /**
     * Calculates points and weight for a single grade. Points are computed based on:
     * <ol>
     * <li>whether or not the grade points represent a "bump"
     * <li>the weighting value to use; either course credits or course weight
     * <ol>
     * The point formula for each grade is as follows:
     * <p>
     * BUMP:
     *
     * <pre>
     * (numeric grade value + points) * (weight * (termweight / 100))
     * </pre>
     *
     * NON-BUMP:
     *
     * <pre>
     * (points) * (weight * (termweight / 100))
     * </pre>
     *
     * @param grade String
     * @param academicLevel String
     * @param scheduleTerm String
     * @param gradeScale GradeScale
     * @param currentYear boolean
     * @param courseWeight BigDecimal
     * @param courseCredit BigDecimal
     * @param columnOid String
     * @param transcriptOid String
     * @param schoolCourseOid String
     * @return GradePointCalculation
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#calculatePoints(java.
     *      lang.String, java.lang.String, java.lang.String,
     *      com.follett.fsc.core.k12.beans.GradeScale,
     *      boolean, java.math.BigDecimal, java.math.BigDecimal, java.lang.String, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public GradePointCalculation calculatePoints(String studentOid,
                                                 String grade,
                                                 String contextOid,
                                                 GradeScale gradeScale,
                                                 BigDecimal courseCredit,
                                                 BigDecimal courseWeight,
                                                 String academicLevel,
                                                 String scheduleTerm,
                                                 String columnOid,
                                                 String transcriptOid,
                                                 String schoolCourseOid) {
        boolean currentYear = ObjectUtils.match(contextOid, getContext().getOid());
        Double points = null;

        grade = grade.trim();

        String letterGrade;
        BigDecimal numericGrade;
        if (StringUtils.isNumeric(grade)) {
            numericGrade = new BigDecimal(grade);
            letterGrade = getGradesManager().getLetterValue(numericGrade, gradeScale, getSchool(), schoolCourseOid);
        } else {
            numericGrade = getGradesManager().getNumericValue(grade, gradeScale, getSchool(), schoolCourseOid);
            letterGrade = grade;
        }

        GradeScaleGradeDefinition gradeDefinition =
                getGradesManager().getGradeDefinition(letterGrade, gradeScale, getSchool().getOid(), schoolCourseOid);

        // Determine the grade point value
        BigDecimal gradePoints = null;
        if (gradeDefinition != null) {
            gradePoints = getPoints(
                    gradeDefinition.getParentGradeScaleGrade() != null ? gradeDefinition.getParentGradeScaleGradeOid()
                            : gradeDefinition.getOid(),
                    academicLevel);
            if (gradePoints != null) { // set custom scale
                gradePoints = gradePoints.setScale(getPointValueScale(), RoundingMode.HALF_UP);
            }
        }

        // Determine the term weight value
        double termWeightValue = 1;
        if (currentYear) {
            BigDecimal termWeight = getTermWeight(getGradeTermOid(), scheduleTerm, columnOid);
            if (termWeight != null) {
                termWeightValue = termWeight.doubleValue() / 100;
            }
        }

        Double weight = null;

        // Calculate points and update totals
        if (gradePoints != null) {
            weight = Double.valueOf(
                    (weightByCredit() ? courseCredit.doubleValue() : courseWeight.doubleValue()) * termWeightValue);
            if (isBump(gradeDefinition.getOid(), academicLevel)) {
                if (numericGrade != null) {
                    points = Double.valueOf(
                            (numericGrade.doubleValue() + gradePoints.doubleValue()) * weight.doubleValue());
                }
            } else {
                points = Double.valueOf(gradePoints.doubleValue() * weight.doubleValue());
            }
        }

        return new GradePointCalculation(points, weight, null);
    }

    /**
     * Calculate GPA.
     *
     * @param points double
     * @param weight double
     * @return double
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#calculateGPA(double,
     *      double)
     */
    @Override
    public double calculateGPA(double points, double weight) {
        double gpa = 0;

        if (weight > 0) {
            gpa = points / weight;
        }

        return gpa;
    }

    /**
     * Gets the current grades sql.
     *
     * @param studentOid String
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#getCurrentGradesSql(java
     *      .lang.String)
     */
    @Override
    public String getCurrentGradesSql(String studentOid) {
        StringBuilder sql = new StringBuilder(1024);

        DatabaseOptimizer optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());

        String creditExpression = null;
        switch (optimizer.getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
            case DatabaseOptimizerFactory.SQLSERVER:
                creditExpression = optimizer.sqlIf(
                        "TRN_POTENTIAL_CREDIT_OVERRIDE IS NOT NULL AND TRN_POTENTIAL_CREDIT_OVERRIDE != ''",
                        "TRN_POTENTIAL_CREDIT_OVERRIDE",
                        "CSK_CREDIT");
                break;

            default:
                throw new UnsupportedDatabaseException(optimizer.getPlatform());
        }

        QueryByCriteria query = getMemberGradeQuery(GpaMemberGrade.COLUMN_TYPE_CURRENT);

        QueryIterator memberGrades = getBroker().getIteratorByQuery(query);
        try {
            while (memberGrades.hasNext()) {
                GpaMemberGrade grade = (GpaMemberGrade) memberGrades.next();
                TranscriptColumnDefinition column = grade.getTranscriptColumnDefinition();

                String columnName = column.getDataFieldConfig().getDataField().getDatabaseName();

                sql.append("SELECT CTX_SCHOOL_YEAR       as " + COL_SCHOOL_YEAR + ", ");
                sql.append("TRN_STD_OID                  as " + COL_STUDENT_OID + ", ");
                sql.append(columnName + "  as " + COL_GRADE + ", ");
                sql.append("       TRN_CTX_OID                  as " + COL_CONTEXT_OID + ", ");
                sql.append("'" + column.getGradeScaleOid() + "' as " + COL_GRADESCALE_OID + ", ");
                sql.append(creditExpression + "  as " + COL_CREDIT + ", ");
                sql.append("       CSK_WEIGHT                   as " + COL_WEIGHT + ", ");
                sql.append("       CSK_ACADEMIC_LEVEL           as " + COL_ACADEMIC_LEVEL + ", ");
                sql.append("       MST_TERM_VIEW                as " + COL_SCHEDULE_TERM_CODE + ", ");
                sql.append("'" + column.getOid() + "' as " + COL_COLUMN_OID + ", ");
                sql.append("       TRN_OID                      as " + COL_TRANSCRIPT_OID + ", ");
                sql.append("       CSK_OID                      as " + COL_SCHOOL_COURSE_OID);

                sql.append("  FROM STUDENT_TRANSCRIPT ");

                sql.append(" INNER JOIN COURSE_SCHOOL ");
                sql.append("    ON TRN_CSK_OID = CSK_OID ");
                sql.append(" INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ");
                sql.append("    ON TRN_CTX_OID = CTX_OID ");
                sql.append(" INNER JOIN STUDENT ");
                sql.append("    ON TRN_STD_OID = STD_OID ");
                sql.append(" INNER JOIN SCHEDULE_MASTER ");
                sql.append("    ON TRN_MST_OID = MST_OID ");

                /*
                 * Include only transcripts from the current year
                 */
                sql.append(" WHERE TRN_CTX_OID = '" + getContext().getOid() + "' ");

                /*
                 * Exclude transcripts flagged with "Excluded from rank"
                 */
                sql.append(" AND TRN_EXCLUDE_RANK_IND <> '1' ");

                /*
                 * Include only the correct courses and students
                 */
                sql.append(getCourseAndStudentSQL(studentOid));

                /*
                 * Include only transcripts from the GPA's grade range
                 */
                sql.append(getGradeRangeSQL());

                /*
                 * Include only transcripts with a non-null grade value
                 */
                sql.append(" AND " + columnName + " IS NOT NULL ");

                /*
                 * Include only transcripts matching the current member grade's schedule term
                 */
                sql.append(" AND MST_TERM_VIEW = '" + grade.getScheduleTermCode() + "'");

                if (memberGrades.hasNext()) {
                    sql.append(" UNION ALL ");
                }
            }
        } finally {
            memberGrades.close();
        }

        /*
         * Important: order by school year and student. This order is essential during the
         * calculation process.
         */
        if (sql.length() > 0) {
            sql.append(" ORDER BY " + COL_SCHOOL_YEAR + "," + COL_STUDENT_OID);
        }
        return sql.toString();
    }

    /**
     * Gets the point value scale.
     *
     * @return int
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#getPointValueScale()
     */
    @Override
    public int getPointValueScale() {
        return 4;
    }

    /**
     * Gets the previous grades sql.
     *
     * @param studentOid String
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.GradePointAverageProcedure#getPreviousGradesSql(
     *      java.lang.String)
     */
    @Override
    public String getPreviousGradesSql(String studentOid) {
        StringBuilder sql = new StringBuilder(1024);

        DatabaseOptimizer optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());

        String creditExpression = null;
        switch (optimizer.getPlatform()) {
            case DatabaseOptimizerFactory.MYSQL:
            case DatabaseOptimizerFactory.SQLSERVER:
                creditExpression = optimizer.sqlIf(
                        "TRN_POTENTIAL_CREDIT_OVERRIDE IS NOT NULL AND TRN_POTENTIAL_CREDIT_OVERRIDE != ''",
                        "TRN_POTENTIAL_CREDIT_OVERRIDE",
                        "CSK_CREDIT");
                break;

            default:
                throw new UnsupportedDatabaseException(optimizer.getPlatform());
        }

        QueryByCriteria query = getMemberGradeQuery(GpaMemberGrade.COLUMN_TYPE_PREVIOUS);
        GpaMemberGrade grade = (GpaMemberGrade) getBroker().getBeanByQuery(query);
        if (grade != null) {
            TranscriptColumnDefinition column = grade.getTranscriptColumnDefinition();
            String columnName = column.getDataFieldConfig().getDataField().getDatabaseName();

            sql.append("SELECT CTX_SCHOOL_YEAR       as " + COL_SCHOOL_YEAR + ", ");
            sql.append("TRN_STD_OID                  as " + COL_STUDENT_OID + ", ");
            sql.append(columnName + "  as " + COL_GRADE + ", ");
            sql.append("       TRN_CTX_OID                  as " + COL_CONTEXT_OID + ", ");
            sql.append("'" + column.getGradeScaleOid() + "' as " + COL_GRADESCALE_OID + ", ");
            sql.append(creditExpression + "  as " + COL_CREDIT + ", ");
            sql.append("       CSK_WEIGHT                   as " + COL_WEIGHT + ", ");
            sql.append("       CSK_ACADEMIC_LEVEL           as " + COL_ACADEMIC_LEVEL + ", ");
            sql.append("'" + column.getOid() + "' as " + COL_COLUMN_OID + ", ");
            sql.append("       TRN_OID                      as " + COL_TRANSCRIPT_OID + ",");
            sql.append("       CSK_OID                      as " + COL_SCHOOL_COURSE_OID);


            sql.append("  FROM STUDENT_TRANSCRIPT ");

            sql.append(" INNER JOIN COURSE_SCHOOL ");
            sql.append("    ON TRN_CSK_OID = CSK_OID ");
            sql.append(" INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ");
            sql.append("    ON TRN_CTX_OID = CTX_OID ");
            sql.append(" INNER JOIN STUDENT ");
            sql.append("    ON TRN_STD_OID = STD_OID ");

            /*
             * Include only transcripts from the current year
             */
            sql.append(" WHERE CTX_SCHOOL_YEAR < " + getContext().getSchoolYear());

            /*
             * Exclude transcripts flagged with "Excluded from rank"
             */
            sql.append(" AND TRN_EXCLUDE_RANK_IND <> '1' ");

            /*
             * Include only the correct courses and students
             */
            sql.append(getCourseAndStudentSQL(studentOid));

            /*
             * Include only transcripts from the GPA's grade range
             */
            sql.append(getGradeRangeSQL());

            /*
             * Include only transcripts with a non-null grade value
             */
            sql.append(" AND " + columnName + " IS NOT NULL ");

            /*
             * Important: order by school year and student. This order is essential during the
             * calculation process.
             */
            sql.append(" ORDER BY " + COL_SCHOOL_YEAR + "," + COL_STUDENT_OID);
        }

        return sql.toString();
    }

    /**
     * Returns an SQL fragment common to all the grade queries run in this procedure. The
     * SQL returned begins an AND clause and is used to retrieve grades belonging to relevant
     * courses and students.
     *
     * @param studentOid optionally, a student OID to limit the results to a single student
     *
     * @return String
     */
    private String getCourseAndStudentSQL(String studentOid) {
        StringBuilder sql = new StringBuilder(256);
        String gpaDefinitionOid = getGradePointAverageDefinition().getOid();

        /*
         * Include courses based on the GPA's course member type.
         */
        int courseMemberType = getGradePointAverageDefinition().getCourseMemberType();

        if (courseMemberType != GradePointAverageDefinition.COURSE_MEMBER_TYPE_GPA_INDICATOR) {
            if (courseMemberType == BeanConstants.INCLUSION_TYPE_EXCLUDE) {
                sql.append(" AND CSK_OID NOT IN ");
            } else {
                sql.append(" AND CSK_OID IN ");
            }

            sql.append(" (SELECT GPC_CSK_OID ");
            sql.append("    FROM GRADE_GPA_MEMBER_COURSE ");
            sql.append("   WHERE GPC_GPD_OID = '" + gpaDefinitionOid + "') ");
        } else {
            sql.append(" AND CSK_GPA_IND = '1'");
        }

        /*
         * If the "rank all" indicator is true on the GPA definition, include all students.
         * Otherwise obey the student rank indicator
         */
        if (!getGradePointAverageDefinition().getRankAllIndicator()) {
            sql.append(" AND STD_RANK_INCLUDE_IND = '1'");
        }

        /*
         * If a YOG was specified, include only that YOG
         */
        if (getYog() != null) {
            sql.append(" AND STD_YOG = " + getYog() + " ");
        }

        /*
         * Include only students from the current school
         */
        sql.append("     AND STD_SKL_OID = '" + getSchool().getOid() + "' ");

        /*
         * Include only active students if specified
         */
        if (isActiveOnly()) {
            sql.append(" AND " + StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
        }

        if (studentOid != null) {
            sql.append(" AND STD_OID = '" + studentOid + "'");
        }

        return sql.toString();
    }

    /**
     * Returns an SQL fragment used to retrieve only transcripts that were taken during the GPA's
     * grade level range.
     *
     * @return String
     */
    private String getGradeRangeSQL() {
        StringBuilder sql = new StringBuilder(256);

        List gradeLevelRange = StudentManager.getGradeLevelRange(getStartGradeLevel(),
                getEndGradeLevel(),
                getBroker());

        sql.append(" AND TRN_GRADE_LEVEL IN (");

        if (gradeLevelRange.isEmpty()) {
            sql.append("'dummy'");
        } else {
            String gradeLevelList = StringUtils.convertCollectionToDelimitedString(gradeLevelRange, ",", "'");
            sql.append(gradeLevelList);
        }

        sql.append(") ");

        return sql.toString();
    }

    /**
     * Returns a Query that finds the relevant GpaMemberGrade beans for the passed member type.
     *
     * @param memberType int
     * @return QueryByCriteria
     * @parm memberType either <code>GpaMemberGrade.COLUMN_TYPE_CURRENT</code> or
     *       <code>GpaMemberGrade.COLUMN_TYPE_PREVIOUS</code>
     */
    private QueryByCriteria getMemberGradeQuery(int memberType) {
        Criteria memberGradeCriteria = new Criteria();
        memberGradeCriteria.addEqualTo(GpaMemberGrade.COL_GRADE_POINT_AVERAGE_DEFINITION_OID,
                getGradePointAverageDefinition().getOid());
        memberGradeCriteria.addEqualTo(GpaMemberGrade.COL_COLUMN_TYPE, Integer.valueOf(memberType));

        if (memberType == GpaMemberGrade.COLUMN_TYPE_CURRENT) {
            memberGradeCriteria.addEqualTo(GpaMemberGrade.COL_GRADE_TERM_OID, getGradeTermOid());
        }

        return new QueryByCriteria(GpaMemberGrade.class, memberGradeCriteria);
    }
}
