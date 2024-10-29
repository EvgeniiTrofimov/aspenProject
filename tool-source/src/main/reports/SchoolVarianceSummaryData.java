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
import com.follett.fsc.core.k12.beans.QueryIterator;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
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
public class SchoolVarianceSummaryData extends ReportJavaSourceNet {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

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
     * Name of the report grid column that contains the total for a school/reason.
     */
    public static final String GRID_TOTAL = "rowTotal";

    /**
     * The reason for the variance, from the students last enrollment record.
     */
    public static final String GRID_VARIANCE_REASON = "reason";

    /**
     * A Map key defining unknown school or unknown reason.
     */
    public static final String MAP_OTHER = "other";

    /**
     * A map key defining the total row for the report.
     */
    public static final String MAP_TOTAL = "Total";

    /**
     * Query columns for the report information.
     */
    private static final String[] QUERY_COLUMNS = new String[] {X2BaseBean.COL_OID,
            SisStudent.COL_GRADE_LEVEL,
            SisStudent.COL_SCHOOL_OID,
            SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME,
            SisStudent.REL_PERSON + "." + SisPerson.REL_PHYSICAL_ADDRESS + "." + SisAddress.COL_GRID_CODE_OID};

    /**
     * A representation of the core fields for a Student.
     */
    private static class StudentInfo {
        public String gradeLevel;
        public String gridCodeOid;
        public String oid;

        // public String school; MJM - remove this because it is not used.
        public String schoolOid;

        /**
         * Constructs a StudentInfo object with initial values based on the query results in the
         * given row.
         *
         * @param row Object[]
         */
        StudentInfo(Object[] row) {
            oid = (String) row[0];
            gradeLevel = (String) row[1];
            schoolOid = (String) row[2];
            // school = (String) row[3];
            gridCodeOid = (String) row[4];
        }
    }

    private Map<String, Integer> m_totalsMap;

    /**
     * Initiate report data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid studentsGrid = new ReportDataGrid();

        getOutOfNeighborhoodStudents(studentsGrid);

        studentsGrid.beforeTop();

        return studentsGrid;
    }

    /**
     * Append a row of data representing a school, reason, total and school list.
     *
     * @param grid the data grid to put the data into.
     * @param name The neighborhood school name.
     * @param rsnKey The reason code
     * @param rsnMap A map by registered school OID of counts.
     * @param schoolList A list of School objects, for order and cache.
     */
    private void appendReasonRow(ReportDataGrid grid,
                                 String name,
                                 String rsnKey,
                                 Map<String, Integer> rsnMap,
                                 List<SisSchool> schoolList) {
        // Get row total.
        int total = 0;
        for (Integer vals : rsnMap.values()) {
            total += vals.intValue();
        }

        grid.append();
        /*
         * Student values
         */
        grid.set(GRID_LOCAL_SCHOOL, name);
        grid.set(GRID_VARIANCE_REASON, rsnKey);
        grid.set(GRID_TOTAL, Integer.valueOf(total));
        for (SisSchool skl : schoolList) {
            Integer sklVal = rsnMap.get(skl.getOid());
            if (sklVal != null) {
                // FieldB001 is an alias indicating which school column to place this data in.
                grid.set(skl.getFieldB001(), sklVal);
            }
        }
    }

    /**
     * Add a block of rows to the grid that represent one neighborhood school,
     * its total row and each reason row.
     *
     * @param grid the data grid to put the data into.
     * @param name The name of the neighborhoos school.
     * @param nbMap A map by school OID of maps of counts.
     * @param schoolList A list of School objects, for order and cache.
     */
    private void appendSection(ReportDataGrid grid,
                               String name,
                               Map<String, Map<String, Integer>> nbMap,
                               List<SisSchool> schoolList) {
        // Set up total values by registered school.
        // map key is the registered school OID.
        // map value is the Integer total count for that school.
        Map<String, Integer> sklTtlMap = new HashMap<String, Integer>();

        // Loop through reasons.
        for (String rsnKey : nbMap.keySet()) {
            Map<String, Integer> rsnMap = nbMap.get(rsnKey);
            // Loop through registered schools.
            for (String regSklKey : rsnMap.keySet()) {
                Integer iVal = rsnMap.get(regSklKey);

                // Running total for registered school. Find the registered school entry.
                Integer regSklTtl = sklTtlMap.get(regSklKey);
                if (regSklTtl == null) {
                    regSklTtl = iVal;
                } else {
                    regSklTtl = Integer.valueOf(iVal.intValue() + regSklTtl.intValue());
                }
                sklTtlMap.put(regSklKey, regSklTtl);
            }
        }

        // Add the total row for the school.
        appendReasonRow(grid, name, MAP_TOTAL, sklTtlMap, schoolList);

        // Add a row for each reason.
        for (String rsnKey : nbMap.keySet()) {
            Map<String, Integer> rsnMap = nbMap.get(rsnKey);
            appendReasonRow(grid, name, rsnKey, rsnMap, schoolList);
        }
    }

    /**
     * Build the data grid from a student query. Check each student for address, grid and school.
     *
     * @param studentsGrid ReportDataGrid
     * @return void
     */
    private void getOutOfNeighborhoodStudents(ReportDataGrid studentsGrid) {
        // Define a map to hold grand total.
        m_totalsMap = new HashMap<String, Integer>();

        // Define a map to hold school totals.
        // First level, key on neighborhood school OID or "other".
        // Second level, key on Reason code or "other".
        // Third level, key on registered school OID.
        // Final value is Integer count of students that match.
        Map<String, Map<String, Map<String, Integer>>> fullMap =
                new HashMap<String, Map<String, Map<String, Integer>>>();

        X2Broker broker = getBroker();

        // Get a map of numeric grade levels.
        HashMap<String, Integer> gradeLevelMap = StudentManager.buildNumericGradeLevelMap(broker);

        // Get a list of active schools in order.
        List<SisSchool> schoolList = getSchoolList();
        Map<String, School> schoolMap = new HashMap<String, School>();
        for (SisSchool school : schoolList) {
            schoolMap.put(school.getOid(), school);
        }

        // Search for active students with addresses.
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, QUERY_COLUMNS, criteria);
        query.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);

        // Iterate through all active students with a physical addresses.
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
                    nbSchool = StudentManager.findGradeLevelSchool(info.oid, gradeLevel.intValue(), schoolMap, false,
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
                    // increment the counts of the appropriate schools/reason.
                    incrementCount(fullMap, info, nbSchool == null ? MAP_OTHER : nbSchool.getOid(),
                            StringUtils.isEmpty(reason) ? MAP_OTHER : reason);
                }
            }
        } finally {
            rows.close();
        }

        // Apply counts to the grid.
        // Get a list of schools in order and attach alias names to those that have registered
        // school data.
        int sklPos = 0;
        for (SisSchool school : schoolList) {
            Integer total = m_totalsMap.get(school.getOid());
            if (total != null) {
                // Set the school alias and add the alias to the report as a parameter.
                sklPos++;
                if (sklPos <= 15) {
                    // report form only has room for 15. The rest are unfortunately ignored. :(
                    String fieldAlias = "school_" + Integer.toString(sklPos);
                    String paramAlias = "school_h_" + Integer.toString(sklPos);
                    school.setFieldB001(fieldAlias);
                    addParameter(paramAlias, school.getName());
                }
            }
        }

        // grand totals line.
        appendReasonRow(studentsGrid, MAP_TOTAL, "", m_totalsMap, schoolList);
        Map<String, Map<String, Integer>> nbEntry;
        for (int inbs = 0; inbs < schoolList.size(); inbs++) {
            SisSchool nbSchool = schoolList.get(inbs);
            nbEntry = fullMap.get(nbSchool.getOid());
            if (nbEntry != null) {
                appendSection(studentsGrid, nbSchool.getName(), nbEntry, schoolList);
            }
        }
        nbEntry = fullMap.get(MAP_OTHER);
        if (nbEntry != null) {
            appendSection(studentsGrid, MAP_OTHER, nbEntry, schoolList);
        }
    }

    /**
     * Returns a list of active schools ordered by school name.
     * The school beans are clones of the beans returned from
     * the cache so we can manipulate them without side effects.
     * They will not be saved.
     *
     * @return a List of active schools in order.
     */
    private List<SisSchool> getSchoolList() {
        ArrayList<SisSchool> list = new ArrayList<SisSchool>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
        query.addOrderByAscending(SisSchool.COL_NAME);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisSchool school = (SisSchool) iterator.next();
                list.add(school.clone());
            }
        } finally {
            iterator.close();
        }
        return list;
    }

    /**
     * For the school and reason values provided, find and increment the proper counter for that
     * student.
     *
     * @param fullMap nested maps by neighborhood school OID, reason code, and registered school
     *        OID.
     * @param info a StudentInfo record containing the registered school OID.
     * @param nbSchoolOid The neighborhood school OID.
     * @param reason a reason code.
     */
    private void incrementCount(Map<String, Map<String, Map<String, Integer>>> fullMap,
                                SchoolVarianceSummaryData.StudentInfo info,
                                String nbSchoolOid,
                                String reason) {
        String regSchoolOid = info.schoolOid;
        String reasonCode = reason == null ? MAP_OTHER : reason;

        // Get the neighborhood school from the first map by schoolOID.
        Map<String, Map<String, Integer>> nbEntry = fullMap.get(nbSchoolOid);
        if (nbEntry == null) {
            nbEntry = new TreeMap<String, Map<String, Integer>>();
            fullMap.put(nbSchoolOid, nbEntry);
        }
        // Get the reason map from the second map by reason code.
        Map<String, Integer> reasonEntry = nbEntry.get(reasonCode);
        if (reasonEntry == null) {
            reasonEntry = new HashMap<String, Integer>();
            nbEntry.put(reasonCode, reasonEntry);
        }
        // Get the registered school count from the third map by registered school OID.
        Integer count = reasonEntry.get(regSchoolOid);
        if (count == null) {
            count = Integer.valueOf(1);
        } else {
            count = Integer.valueOf(count.intValue() + 1);
        }
        reasonEntry.put(regSchoolOid, count);

        // Increment grand total by registered school OID.
        count = m_totalsMap.get(regSchoolOid);
        if (count == null) {
            count = Integer.valueOf(1);
        } else {
            count = Integer.valueOf(count.intValue() + 1);
        }
        m_totalsMap.put(regSchoolOid, count);
    }
}
