/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2008 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GridCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.SisStudentManager;
import com.x2dev.utils.StringUtils;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Report for finding students who are enrolled in a school that is not
 * their neighborhood school according to their address grid code.
 *
 * @author X2 Development Corporation
 */
public class NeighborhoodSchoolVarianceData extends ReportJavaSourceNet {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name of the report grid column that contains the student's name. The value is a String.
     */
    public static final String GRID_NAME_VIEW = "nameView";

    /**
     * Name of the report grid column that contains the student's grade. The value is a String.
     */
    public static final String GRID_GRADE = "grade";

    /**
     * Name of the report grid column that contains the student's address. The value is a String.
     */
    public static final String GRID_ADDRESS_VIEW = "addressView";

    /**
     * Name of the report grid column that contains the student's school's name. The value is a
     * String.
     */
    public static final String GRID_SCHOOL = "school";

    /**
     * Name of the grid code that the students address maps to.
     */
    public static final String GRID_GRID_CODE = "gridCode";

    /**
     * Name of the report grid column that contains the student's school's name. The value is a
     * String.
     */
    public static final String GRID_LOCAL_SCHOOL = "localSchool";

    /**
     * The reason for the variance, from the students last enrollment record.
     */
    public static final String GRID_VARIANCE_REASON = "reason";

    /**
     * Query columns for the report information.
     */
    private static final String[] QUERY_COLUMNS = new String[] {X2BaseBean.COL_OID,
            SisStudent.COL_NAME_VIEW,
            SisStudent.COL_GRADE_LEVEL,
            SisStudent.COL_ADDRESS_VIEW,
            SisStudent.COL_SCHOOL_OID,
            SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME,
            SisStudent.REL_SCHOOL + "." + SisSchool.COL_SCHOOL_ID,
            SisStudent.REL_PERSON + "." + SisPerson.REL_PHYSICAL_ADDRESS + "." + SisAddress.COL_GRID_CODE_OID};

    /**
     * A representation of the core fields for a Student.
     */
    private class StudentInfo {
        public String addressView;
        public String gradeLevel;
        public String gridCodeOid;
        public String nameView;
        public String oid;
        public String school;
        // @SuppressWarnings("unused")
        // public String schoolId;
        public String schoolOid;

        /**
         * Constructs a StudentInfo object with initial values based on the query results in the
         * given row.
         *
         * @param row Object[]
         */
        StudentInfo(Object[] row) {
            oid = (String) row[0];
            nameView = (String) row[1];
            gradeLevel = (String) row[2];
            addressView = (String) row[3];
            schoolOid = (String) row[4];
            school = (String) row[5];
            // schoolId = (String) row[6];
            gridCodeOid = (String) row[7];
        }
    }

    /**
     * Initiate report data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // Locate all active students.
        Criteria criteria = new Criteria();

        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, null);

        int totalStudents = getBroker().getCount(studentQuery);
        int estimatedStudents = (int) (totalStudents * 0.05);

        // Assumption: < 5% of students are out of neighborhood.
        ReportDataGrid studentsGrid = new ReportDataGrid(estimatedStudents, QUERY_COLUMNS.length + 1);

        getOutOfNeighborhoodStudents(studentsGrid);

        studentsGrid.beforeTop();

        return studentsGrid;
    }

    /**
     * Add the student, address and school information to the report data grid.
     *
     * @param grid ReportDataGrid
     * @param info StudentInfo
     * @param gridCode GridCode
     * @param nbSchool School
     * @param reason String
     */
    private void appendStudent(ReportDataGrid grid,
                               NeighborhoodSchoolVarianceData.StudentInfo info,
                               GridCode gridCode,
                               School nbSchool,
                               String reason) {
        grid.append();
        /*
         * Student values
         */
        grid.set(GRID_NAME_VIEW, info.nameView);
        grid.set(GRID_GRADE, info.gradeLevel);
        grid.set(GRID_ADDRESS_VIEW, info.addressView);
        grid.set(GRID_SCHOOL, info.school);
        grid.set(GRID_GRID_CODE, gridCode == null ? null : gridCode.getGridCode());
        grid.set(GRID_LOCAL_SCHOOL, nbSchool == null ? null : nbSchool.getName());
        grid.set(GRID_VARIANCE_REASON, reason);
    }

    /**
     * Build the data grid from a student query. Check each student for address, grid and school.
     *
     * @param studentsGrid ReportDataGrid
     * @return void
     */
    private void getOutOfNeighborhoodStudents(ReportDataGrid studentsGrid) {
        X2Broker broker = getBroker();

        // Get a map of numeric grade levels.
        HashMap<String, Integer> gradeLevelMap = StudentManager.buildNumericGradeLevelMap(broker);

        // Get a Map of active schools.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, criteria);
        Map<String, School> schools = broker.getMapByQuery(schoolQuery, X2BaseBean.COL_OID, 10);

        // Search for active students with addresses.
        criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
        }
        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, QUERY_COLUMNS, criteria);
        query.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        // Iterate through all active students.
        ReportQueryIterator rows = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (rows.hasNext()) {
                Object[] row = (Object[]) rows.next();
                StudentInfo info = new StudentInfo(row);

                // Grade level.
                Integer gradeLevel = gradeLevelMap.get(info.gradeLevel);

                // Grid code.
                GridCode gridCode = null;
                if (!StringUtils.isEmpty(info.gridCodeOid)) {
                    gridCode = (GridCode) broker.getBeanByOid(GridCode.class, info.gridCodeOid);
                }

                // Find correct school.
                School nbSchool = null;
                if (gridCode != null && gradeLevel != null) {
                    nbSchool = StudentManager.findGradeLevelSchool(info.oid, gradeLevel.intValue(), schools, false,
                            broker);
                }

                // If schools do not match, add to report.
                if (nbSchool == null || !nbSchool.getOid().equals(info.schoolOid)) {
                    // Lookup last enrollment, get reason.
                    SisStudent student = (SisStudent) broker.getBeanByOid(SisStudent.class, info.oid);
                    StudentEnrollment lastEnrollment =
                            SisStudentManager.getStudentEnrollment(student.getOid(), StudentEnrollment.ENTRY, broker);
                    String reason = null;
                    if (lastEnrollment != null) {
                        reason = lastEnrollment.getReasonCode();
                    }
                    appendStudent(studentsGrid, info, gridCode, nbSchool, reason);
                }
            }
        } finally {
            rows.close();
        }
    }
}
