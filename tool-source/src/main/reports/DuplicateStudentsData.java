/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import java.math.BigDecimal;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Duplicate Students" report. This report attempts to identify multiple
 * records in the Student table that represent the same, physical individual.
 *
 * @author X2 Development Corporation
 */
public class DuplicateStudentsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "check LASIDs" report parameter. The value is a Boolean.
     */
    public static final String CHECK_LOCAL_IDS_PARAM = "checkLocalId";

    /**
     * Name for the "check person IDs (SSNs)" report parameter. The value is a Boolean.
     */
    public static final String CHECK_PERSON_IDS_PARAM = "checkPersonId";

    /**
     * Name for the "check SASIDs" report parameter. The value is a Boolean.
     */
    public static final String CHECK_STATE_IDS_PARAM = "checkStateId";

    /**
     * Name of the report grid column that contains the student's address. The value is a String.
     */
    public static final String GRID_ADDRESS_VIEW = "addressView";

    /**
     * Name of the report grid column that contains the actual duplicated ID. The value is a String.
     */
    public static final String GRID_DUPLICATE_ID = "duplicateId";

    /**
     * Name of the report grid column that contains the type of duplicated ID. The value is an
     * Integer:
     * <ul>
     * <li>1 -> Local ID
     * <li>2 -> State ID
     * <li>3 -> Person ID
     * </ul>
     */
    public static final String GRID_ID_TYPE = "idType";

    /**
     * Name of the report grid column that contains the student's status. The value is a String.
     */
    public static final String GRID_ENROLLMENT_STATUS = "enrollmentStatus";

    /**
     * Name of the report grid column that contains the student's local ID. The value is a String.
     */
    public static final String GRID_LOCAL_ID = "localId";

    /**
     * Name of the report grid column that contains the student's name. The value is a String.
     */
    public static final String GRID_NAME_VIEW = "nameView";

    /**
     * Name of the report grid column that contains the student's OID. The value is a String.
     */
    public static final String GRID_OID = "oid";

    /**
     * Name of the report grid column that contains the student's person's ID. The value is a
     * String.
     */
    public static final String GRID_PERSON_ID = "personId";

    /**
     * Name of the report grid column that contains the indicator for whether or not to display the
     * duplicated ID. This is used for records that represent a second, third, or more student for
     * a single ID. The value is a Boolean.
     */
    public static final String GRID_PRINT = "print";

    /**
     * Name of the report grid column that contains the student's school's name. The value is a
     * String.
     */
    public static final String GRID_SCHOOL = "school";

    /**
     * Name of the report grid column that contains the student's state ID. The value is a String.
     */
    public static final String GRID_STATE_ID = "stateId";

    /**
     * Name of the report grid column that contains the student's YOG. The value is an Integer.
     */
    public static final String GRID_YOG = "yog";

    private static final String[] QUERY_COLUMNS = new String[] {X2BaseBean.COL_OID,
            SisStudent.COL_LOCAL_ID,
            SisStudent.COL_STATE_ID,
            SisStudent.REL_PERSON + "." + SisPerson.COL_PERSON_ID,
            SisStudent.COL_NAME_VIEW,
            SisStudent.COL_ADDRESS_VIEW,
            SisStudent.COL_YOG,
            SisStudent.COL_ENROLLMENT_STATUS,
            SisStudent.REL_SCHOOL + "." + SisSchool.COL_SCHOOL_ID};

    private static final int LOCAL_ID_INDEX = 1;
    private static final int PERSON_ID_INDEX = 3;
    private static final int STATE_ID_INDEX = 2;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Assumption: < 5% of students are duplicated
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, null);
        int totalStudents = getBroker().getCount(studentQuery);
        int estimatedDuplicateStudents = (int) (totalStudents * 0.05);

        ReportDataGrid duplicateStudents =
                new ReportDataGrid(estimatedDuplicateStudents, QUERY_COLUMNS.length + 1);

        if (((Boolean) getParameter(CHECK_LOCAL_IDS_PARAM)).booleanValue()) {
            getDuplicateIds(duplicateStudents, LOCAL_ID_INDEX);
        }

        if (((Boolean) getParameter(CHECK_STATE_IDS_PARAM)).booleanValue()) {
            getDuplicateIds(duplicateStudents, STATE_ID_INDEX);
        }

        if (((Boolean) getParameter(CHECK_PERSON_IDS_PARAM)).booleanValue()) {
            getDuplicateIds(duplicateStudents, PERSON_ID_INDEX);
        }

        duplicateStudents.beforeTop();

        return duplicateStudents;
    }

    /**
     * Appends the student information to the grid.
     *
     * @param grid ReportDataGrid
     * @param student StudentInfo
     * @param idType int
     * @param print boolean
     */
    private void appendRecord(ReportDataGrid grid, StudentInfo student, int idType, boolean print) {
        grid.append();

        /*
         * Behavior values
         */
        grid.set(GRID_ID_TYPE, Integer.valueOf(idType));
        switch (idType) {
            case LOCAL_ID_INDEX:
                grid.set(GRID_DUPLICATE_ID, student.localId);
                break;

            case STATE_ID_INDEX:
                grid.set(GRID_DUPLICATE_ID, student.stateId);
                break;

            case PERSON_ID_INDEX:
                grid.set(GRID_DUPLICATE_ID, student.personId);
                break;
        }
        grid.set(GRID_PRINT, Boolean.valueOf(print));

        /*
         * Student values
         */
        grid.set(GRID_ADDRESS_VIEW, student.addressView);
        grid.set(GRID_LOCAL_ID, student.localId);
        grid.set(GRID_NAME_VIEW, student.nameView);
        grid.set(GRID_OID, student.oid);
        grid.set(GRID_PERSON_ID, student.personId);
        grid.set(GRID_SCHOOL, student.school);
        grid.set(GRID_STATE_ID, student.stateId);
        grid.set(GRID_ENROLLMENT_STATUS, student.enrollmentStatus);
        grid.set(GRID_YOG, student.yog);
    }

    /**
     * Checks for duplicate IDs for a given type (as specified by the column index). Duplicates are
     * added to the grid.
     *
     * @param grid ReportDataGrid
     * @param idColumn index of the column that contains the ID to check
     * @return void
     */
    private void getDuplicateIds(ReportDataGrid grid, int idColumn) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(QUERY_COLUMNS[idColumn], getBroker().getPersistenceKey());

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(SisStudent.class, QUERY_COLUMNS, criteria);
        query.addOrderByAscending(QUERY_COLUMNS[idColumn]);
        query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(SisStudent.COL_ENROLLMENT_STATUS);
        query.addOrderByAscending(SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);

        String lastId = "";
        StudentInfo lastStudent = null;
        boolean appendLastStudent = true;

        ReportQueryIterator rows = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (rows.hasNext()) {
                Object[] row = (Object[]) rows.next();
                StudentInfo student = new StudentInfo(row);

                String id = (String) row[idColumn];
                if (lastId.equals(id)) {
                    if (appendLastStudent) {
                        appendRecord(grid, lastStudent, idColumn, true);
                        appendLastStudent = false;
                    }

                    appendRecord(grid, student, idColumn, false);
                } else {
                    appendLastStudent = true;
                }

                lastId = id;
                lastStudent = student;
            }
        } finally {
            rows.close();
        }
    }

    /**
     * A lightweight representation of the core identifying fields for a Student bean.
     */
    private class StudentInfo {
        public String addressView;
        public String enrollmentStatus;
        public String localId;
        public String nameView;
        public String oid;
        public String personId;
        public String school;
        public String stateId;
        public Integer yog;

        /**
         * Constructs a StudentInfo object with initial values based on the query results in the
         * given row.
         *
         * @param row Object[]
         */
        StudentInfo(Object[] row) {
            oid = (String) row[0];
            localId = (String) row[1];
            stateId = (String) row[2];
            personId = (String) row[3];
            nameView = (String) row[4];
            addressView = (String) row[5];
            BigDecimal yog_bd = (BigDecimal) row[6];
            if (yog_bd != null) {
                yog = Integer.valueOf(yog_bd.intValue());
            }
            enrollmentStatus = (String) row[7];
            school = (String) row[8];
        }
    }
}
