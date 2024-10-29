/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScalePoints;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentGradePoint;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for CreditsEarned export.
 *
 * @author X2 Development Corporation
 */

public class CreditsEarned extends StateReportData {
    /**
     * Entity class for Student Record Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class CreditsEarnedEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public CreditsEarnedEntity() {
            // no argument constructor
        }

        /**
         * Local variables for reporting information.
         */
        protected BigDecimal m_cumulativeWeightedGPA;
        protected BigDecimal m_cumulativeCourseWeight;
        protected BigDecimal m_cumulativeUnweightedGPA;
        protected BigDecimal m_highSchoolGPABasis;

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            CreditsEarned stdData = (CreditsEarned) data;
            Student student = (Student) bean;
            Collection<StudentGradePoint> grades = stdData.m_gradePointMap.get(student.getOid());
            StudentGradePoint grade = null;
            BigDecimal gradePointsTotal = new BigDecimal(0);

            if (grades != null) {
                grade = grades.iterator().next();
                for (StudentGradePoint gradePoint : grades) {
                    gradePointsTotal = gradePointsTotal.add(gradePoint.getGpa());
                }
            }

            m_highSchoolGPABasis = stdData.m_maxGradeScalePoints.getGradePoints();
            if (grade != null) {
                m_cumulativeWeightedGPA = grade.getCumulativeGpa();
                m_cumulativeCourseWeight = grade.getCumulativeCourseWeight();
                m_cumulativeUnweightedGPA =
                        gradePointsTotal.divide(new BigDecimal(grades.size()), 2, RoundingMode.HALF_UP);
            }
        }

        /**
         * Returns the cumulative weighted GPA for the current student.
         *
         * @return BigDecimal
         */
        public BigDecimal getCumulativeWeightedGPA() {
            if (m_cumulativeWeightedGPA == null) {
                m_cumulativeWeightedGPA = new BigDecimal(0);
            }
            return m_cumulativeWeightedGPA.setScale(2, RoundingMode.HALF_UP);
        }

        /**
         * Returns the cumulative course weight for the current student.
         *
         * @return BigDecimal
         */
        public BigDecimal getCumulativeCourseWeight() {
            if (m_cumulativeCourseWeight == null) {
                m_cumulativeCourseWeight = new BigDecimal(0);
            }
            return m_cumulativeCourseWeight.setScale(2, RoundingMode.HALF_UP);
        }

        /**
         * Returns the cumulative unweighted GPA for the current student.
         *
         * @return BigDecimal
         */
        public BigDecimal getCumulativeUnweightedGPA() {
            if (m_cumulativeUnweightedGPA == null) {
                m_cumulativeUnweightedGPA = new BigDecimal(0);
            }
            return m_cumulativeUnweightedGPA.setScale(2, RoundingMode.HALF_UP);
        }

        /**
         * Returns the high school GPA basis for the current student.
         *
         * @return BigDecimal
         */
        public BigDecimal getHighSchoolGPABasis() {
            return m_highSchoolGPABasis.setScale(2, RoundingMode.HALF_UP);
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

    }

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<StudentGradePoint>> m_gradePointMap;
    protected ReferenceCode m_maxGradeValue;
    protected Map<String, Collection<GradeScalePoints>> m_gradePoints;
    protected Map<String, GradeScale> m_gradeScales;
    protected GradeScalePoints m_maxGradeScalePoints;
    protected String m_excludeStdField;

    protected static final String PARAM_SAVE_RESULTS = "saveResults";

    /**
     * Retrieves the high school GPA basis.
     */
    protected class RetrieveHighSchoolGPABasis implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((CreditsEarnedEntity) entity).getHighSchoolGPABasis();
        }
    }

    /**
     * Retrieves the total Credits from the CreditsEarned entity.
     */
    protected class RetrieveTotalCredits implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((CreditsEarnedEntity) entity).getCumulativeCourseWeight();
        }
    }

    /**
     * Retrieves the unweighted GPA from the CreditsEarned entity.
     */
    protected class RetrieveUnweightedGPA implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((CreditsEarnedEntity) entity).getCumulativeUnweightedGPA();
        }
    }

    /**
     * Retrieves the weighted GPA from the CreditsEarned entity.
     */
    protected class RetrieveWeightedGPA implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((CreditsEarnedEntity) entity).getCumulativeWeightedGPA();
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);

        Criteria criteria = getStudentCriteria();

        QueryByCriteria query = new QueryByCriteria(Student.class, criteria);
        applyInputSort(query, null);
        loadStudentGradeData(criteria);
        setGradeScalePoints();

        setQuery(query);

        setEntityClass(CreditsEarnedEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("CREDITS-CREDITS", new RetrieveTotalCredits());
        calcs.put("CREDITS-WEIGHTEDGPA", new RetrieveWeightedGPA());
        calcs.put("CREDITS-UNWEIGHTGPA", new RetrieveUnweightedGPA());
        calcs.put("CREDITS-GPABASIS", new RetrieveHighSchoolGPABasis());

        HashMap validators = new HashMap<String, FieldRetriever>();
        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        List<String> enrollTypes = new ArrayList<String>();
        enrollTypes.add(StudentEnrollment.WITHDRAWAL);
        enrollTypes.add(StudentEnrollment.ENTRY);

        PlainDate startDate = getCurrentContext().getStartDate();

        // With Enrollment records within the active date range and of the type E,W.
        Criteria activityCriteria = new Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollTypes);

        applyInputCriteria(activityCriteria, false, StudentEnrollment.REL_STUDENT);
        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activityCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT
                    + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.

        Criteria enrollCriteria = new Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        Criteria activeCriteria = new Criteria();
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        applyInputCriteria(activeCriteria, true, null);
        if (!isSchoolContext()) {
            activeCriteria.addNotEqualTo(
                    Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            activeCriteria.addNotEqualTo(
                    Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activeCriteria.addNotEqualTo(m_excludeStdField, BooleanAsStringConverter.TRUE);
        }

        Criteria reportingCriteria = new Criteria();
        reportingCriteria.addOrCriteria(activeCriteria);
        reportingCriteria.addOrCriteria(enrollCriteria);

        return reportingCriteria;
    }

    /**
     * sets GradeScalePoints with high school GPA basis.
     */
    private void setGradeScalePoints() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();
        GradeScalePoints gsp;
        // m_maxGradeScalePoints = null;

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(GradeScalePoints.COL_ACADEMIC_LEVEL, "Standard");
        QueryByCriteria query = new QueryByCriteria(GradeScalePoints.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        BigDecimal maxGradeValue = new BigDecimal(0);

        try {
            while (iterator.hasNext()) {
                gsp = (GradeScalePoints) iterator.next();
                if (gsp.getGradePoints().compareTo(maxGradeValue) > 0) {
                    maxGradeValue = gsp.getGradePoints();
                    m_maxGradeScalePoints = gsp;
                }
                // m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the student grade point data required by this export.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentGradeData(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentGradePoint.COL_STUDENT_OID, studentsSubQuery);

        QueryByCriteria gradePointQuery = new QueryByCriteria(StudentGradePoint.class, criteria);
        gradePointQuery.addOrderByDescending(StudentGradePoint.COL_CUMULATIVE_COURSE_WEIGHT);

        m_gradePointMap =
                getBroker().getGroupedCollectionByQuery(gradePointQuery, StudentGradePoint.COL_STUDENT_OID, 1024);
    }
}
