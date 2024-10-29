/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the 'Secondary School Information' report.
 */
public class SecondarySchoolInformationData extends StudentReportJavaSource {
    /**
     * Report parameter name for the date. This value is a PlainDate object.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Report parameter name for the end date. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the school.
     */
    public static final String ORGANIZATION_PARAM = "organization";

    /**
     * Report parameter name for the school.
     */
    public static final String SCHOOL_PARAM = "school";

    /**
     * Report parameter name for the start date. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Grid columns
     */
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_SECONDARY_SCHOOL = "secondarySchool";
    private static final String COLUMN_START_DATE = "startDate";
    private static final String COLUMN_END_DATE = "endDate";

    /**
     * 'Query By' parameters
     */
    private static final String PARAM_ALL_CROSS_ENROLLMENT = "##allCrossEnrollments";
    private static final String PARAM_PRIMARY_CROSS_ENROLLED = "##primaryCrossEnrolled";
    private static final String PARAM_STUDENT_CROSS_ENROLLED = "##studentCrossEnrolled";

    /**
     * Member / variables
     */
    private Map<String, Collection<StudentSchool>> m_studentSecondarySchoolMap =
            new HashMap<String, Collection<StudentSchool>>();

    /**
     * @see com.x2dev.sis.tools.reports.StudentReportJavaSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Get the school, start and end date
        SisSchool school = (SisSchool) getSchool();

        // Add the school, organization, and current date parameter
        addParameter(SCHOOL_PARAM, school);
        addParameter(ORGANIZATION_PARAM, school.getOrganization1());
        addParameter(DATE_PARAM, new PlainDate());

        // Prepare data grid.
        ReportDataGrid dataGrid = new ReportDataGrid();

        Collection<SisStudent> students = getStudents();
        for (SisStudent student : students) {
            Collection<StudentSchool> secondarySchoolCol = m_studentSecondarySchoolMap.get(student.getOid());
            if (secondarySchoolCol != null) {
                for (StudentSchool secondarySchool : secondarySchoolCol) {
                    dataGrid.append();
                    dataGrid.set(COLUMN_STUDENT, student);
                    dataGrid.set(COLUMN_SECONDARY_SCHOOL, secondarySchool.getSchool().getName());
                    dataGrid.set(COLUMN_START_DATE, secondarySchool.getStartDate());
                    dataGrid.set(COLUMN_END_DATE, secondarySchool.getEndDate());
                }
            }
        }

        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A QueryIterator of Student beans
     */
    private Collection<SisStudent> getStudents() {
        PlainDate startDate = (PlainDate) getParameters().get(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameters().get(END_DATE_PARAM);

        X2Criteria studentCriteria = new X2Criteria();
        Criteria defaultCriteria = buildCriteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (!queryBy.isEmpty()) {
            Criteria secondaryCriteria = new Criteria();

            // Check the cross-enrollment dates
            if (startDate != null) {
                Criteria endDateCriteria = new Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, startDate);

                X2Criteria orEndDateCriteria = new X2Criteria();
                orEndDateCriteria.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(orEndDateCriteria);
                secondaryCriteria.addAndCriteria(endDateCriteria);
            }
            if (endDate != null) {
                secondaryCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, endDate);
            }

            if (PARAM_PRIMARY_CROSS_ENROLLED.equals(queryBy)) {
                // Only include students that has secondary school information not in this school
                secondaryCriteria.addNotEqualTo(StudentSchool.COL_SCHOOL_OID, getSchool().getOid());
                studentCriteria.addAndCriteria(defaultCriteria);
            } else if (PARAM_STUDENT_CROSS_ENROLLED.equals(queryBy)) {
                // Only include students that has secondary school information in this school
                secondaryCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, getSchool().getOid());
            } else if (!PARAM_ALL_CROSS_ENROLLMENT.equals(queryBy)) // it's 'all' or 'current
                                                                    // Selection' at this point
            {
                studentCriteria.addAndCriteria(defaultCriteria);
            }

            BeanQuery secondaryQuery = createQueryByCriteria(StudentSchool.class, secondaryCriteria);
            m_studentSecondarySchoolMap =
                    getBroker().getGroupedCollectionByQuery(secondaryQuery, StudentSchool.COL_STUDENT_OID, 0);

            SubQuery secondarySubQuery = new SubQuery(secondaryQuery, StudentSchool.COL_STUDENT_OID);

            if (PARAM_ALL_CROSS_ENROLLMENT.equals(queryBy)) {
                // Student in primary school and has secondary school
                X2Criteria secondaryCriteria1 = new X2Criteria();
                secondaryCriteria1.addAndCriteria(defaultCriteria);
                secondaryCriteria1.addIn(X2BaseBean.COL_OID, secondarySubQuery);
                studentCriteria.addOrCriteria(secondaryCriteria1);

                // Student has this school as secondary school
                Criteria secondarySchoolCriteria = new Criteria();
                secondarySchoolCriteria.addEqualTo(
                        Student.REL_STUDENT_SCHOOLS + ModelProperty.PATH_DELIMITER + StudentSchool.COL_SCHOOL_OID,
                        getSchool().getOid());
                SubQuery subQuery2 = new SubQuery(Student.class, X2BaseBean.COL_OID, secondarySchoolCriteria);
                Criteria secondaryCriteria2 = new Criteria();
                secondaryCriteria2.addIn(X2BaseBean.COL_OID, subQuery2);
                studentCriteria.addOrCriteria(secondaryCriteria2);
            } else {
                studentCriteria.addIn(X2BaseBean.COL_OID, secondarySubQuery);
            }

            // Include active students only or not
            boolean activeOnly = getParameter(ACTIVE_ONLY_PARAM) != null
                    ? ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue() : false;
            if (activeOnly) {
                Criteria activeCriteria = new Criteria();
                activeCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        Student.COL_ENROLLMENT_STATUS));
                studentCriteria.addAndCriteria(activeCriteria);
            }
        }

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, studentCriteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        return getBroker().getCollectionByQuery(query);
    }
}
