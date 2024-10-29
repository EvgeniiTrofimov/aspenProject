/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Prepares the data for the Student Assessment Report
 * <p>
 * This report simply select students from the current school (with an optional criteria for
 * students with no test scores and active SPED student) and order the results by last name, YOG,
 * or date the test was taken. It will also allow the user to select if they want all test scores
 * to meet the criteria or any of the test scores.
 *
 * @author X2 Development Corporation
 */
public class StudentAssessmentScoresData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_SPED_ONLY_PARAM = "activeSpedOnly";

    /**
     * Name for the "Test name" report parameter. The value is a String.
     */
    public static final String ASSESSMENT_OID_PARAM = "assessmentOid";

    /**
     * Name for the "Grade Levels" report parameter. The value is a String.
     */
    public static final String GRADE_LEVELS_PARAM = "gradeLevels";

    /**
     * Name for the "students with no test scores" report parameter. The value is a Boolean.
     */
    public static final String NO_SCORES_PARAM = "studentNoTest";

    /**
     * Name for the "Records to Match" report parameter. The value is an Integer.
     */
    public static final String RECORDS_MATCH_PARAM = "recordsMatch";

    /**
     * Name for the enumerated "maximum score" report parameter. The value is an Integer.
     */
    public static final String SCORE_MAX_PARAM = "scoreRangeMax";

    /**
     * Name for the enumerated "minimum score" report parameter. The value is an Integer.
     */
    public static final String SCORE_MIN_PARAM = "scoreRangeMin";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private static final String COL_ASSESSMENT = "assessment";
    private static final String COL_STUDENT = "student";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
        }

        String gradeLevels = (String) getParameter(GRADE_LEVELS_PARAM);
        criteria.addIn(SisStudent.COL_GRADE_LEVEL, StringUtils.convertDelimitedStringToList(gradeLevels, ',', true));

        boolean activeSpedOnly = ((Boolean) getParameter(ACTIVE_SPED_ONLY_PARAM)).booleanValue();
        if (activeSpedOnly) {
            String activeCode =
                    PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
            criteria.addEqualTo(SisStudent.COL_SPED_STATUS_CODE, activeCode);
        }

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        /*
         * Build the sort based on user input
         *
         * If we are not in the context of a school, sort by the school first to support school
         * grouping on the format.
         */
        if (!isSchoolContext()) {
            query.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        }

        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Cycles through students and adds to grid based on select "all" or "any".
         */
        double minScore = ((BigDecimal) getParameter(SCORE_MIN_PARAM)).doubleValue();
        double maxScore = ((BigDecimal) getParameter(SCORE_MAX_PARAM)).doubleValue();

        boolean addNoScores = ((Boolean) getParameter(NO_SCORES_PARAM)).booleanValue();

        int match = ((Integer) getParameter(RECORDS_MATCH_PARAM)).intValue();

        Map<String, Collection<StudentAssessment>> assessmentsByStudent = getAssessmentsByStudent(criteria);

        ReportDataGrid grid = new ReportDataGrid();
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                Collection<StudentAssessment> assessments = assessmentsByStudent.get(student.getOid());

                if (assessments == null) {
                    if (addNoScores) {
                        grid.append();
                        grid.set(COL_STUDENT, student);
                    }
                } else {
                    boolean includeStudent = (match == 0 ? true : false);

                    List<StudentAssessment> assessmentsInRange = new ArrayList<StudentAssessment>(assessments.size());
                    for (StudentAssessment assessment : assessments) {
                        double score = 0.0;
                        BigDecimal scaleScore = assessment.getScaleScore();
                        if (scaleScore != null) {
                            score = scaleScore.doubleValue();
                        }

                        boolean inRange = ((score >= minScore) && (score <= maxScore));

                        if (inRange) {
                            assessmentsInRange.add(assessment);
                        }

                        if (match == 0) // All
                        {
                            includeStudent = (includeStudent && inRange);
                        } else // Any
                        {
                            includeStudent = (includeStudent || inRange);
                        }
                    }

                    if (includeStudent) {
                        for (StudentAssessment assessmentInRange : assessmentsInRange) {
                            grid.append();
                            grid.set(COL_STUDENT, student);
                            grid.set(COL_ASSESSMENT, assessmentInRange);
                        }
                    }
                }
            }
        } finally {
            students.close();
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Returns the lookup table of student assessment records keyed on student OID.
     *
     * @param studentCriteria Criteria
     * @return A Map of Collections of StudentAssessment beans keyed on student OID.
     */
    private Map<String, Collection<StudentAssessment>> getAssessmentsByStudent(Criteria studentCriteria) {
        Criteria criteria = new Criteria();

        criteria.addIn(StudentAssessment.COL_STUDENT_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));
        criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, getParameter(ASSESSMENT_OID_PARAM));

        QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, StudentAssessment.COL_STUDENT_OID, 1024);
    }
}
