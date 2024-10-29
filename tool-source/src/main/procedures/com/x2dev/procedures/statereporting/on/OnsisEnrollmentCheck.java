/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.List;

/**
 * The Class OnsisEnrollmentCheck.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisEnrollmentCheck extends ExportJavaSource {

    /**
     * The Class EnrollmentRecord.
     */
    public static class EnrollmentRecord {
        public static final String[] COLUMNS = {
                // 0
                StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_NAME_VIEW,
                // 1
                StudentEnrollment.REL_STUDENT + "." + SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME,
                // 2
                StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_ENROLLMENT_STATUS,
                // 3
                StudentEnrollment.REL_SCHOOL + "." + SisSchool.COL_NAME,
                // 4
                StudentEnrollment.COL_ENROLLMENT_TYPE,
                // 5
                StudentEnrollment.COL_ENROLLMENT_DATE,
                // 6
                StudentEnrollment.COL_SCHOOL_OID,
                // 7
                StudentEnrollment.COL_STUDENT_OID,
                // 8
                StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_LOCAL_ID
        };
        public static final String FIELD_STUDENT_NAME = "Student Name";
        public static final String FIELD_CURRENT_SCHOOL = "Current School";
        public static final String FIELD_ENROLLMENT_STATUS = "Status";
        public static final String FIELD_ENROLLMENT_SCHOOL = "School";
        public static final String FIELD_ENROLLMENT_TYPE = "Type";
        public static final String FIELD_ENROLLMENT_DATE = "Date";
        public static final String FIELD_SCHOOL_OID = "sklOid";
        public static final String FIELD_STUDENT_OID = "stdOid";
        public static final String FIELD_LOCAL_ID = "LocalId";
        public static final List<String> FIELDS =
                Arrays.asList(FIELD_STUDENT_NAME, FIELD_CURRENT_SCHOOL, FIELD_ENROLLMENT_STATUS,
                        FIELD_ENROLLMENT_SCHOOL, FIELD_ENROLLMENT_TYPE,
                        FIELD_ENROLLMENT_DATE, FIELD_SCHOOL_OID, FIELD_STUDENT_OID, FIELD_LOCAL_ID);
        Object[] m_values;

        /**
         * Instantiates a new enrollment record.
         *
         * @param row Object[]
         */
        public EnrollmentRecord(Object[] row) {
            m_values = row;
        }

        /**
         * Gets the enrollment date.
         *
         * @return Plain date
         */
        public PlainDate getEnrollmentDate() {
            return new PlainDate((java.util.Date) m_values[5]);
        }

        /**
         * Gets the enrollment type.
         *
         * @return String
         */
        public String getEnrollmentType() {
            return (String) m_values[4];
        }

        /**
         * Gets the skl oid.
         *
         * @return String
         */
        public String getSklOid() {
            return (String) m_values[6];
        }

        /**
         * Gets the std oid.
         *
         * @return String
         */
        public String getStdOid() {
            return (String) m_values[7];
        }

        /**
         * Adds the to grid.
         *
         * @param grid DataGrid
         */
        public void addToGrid(DataGrid grid) {
            grid.append();
            for (int index = 0; index < FIELDS.size(); ++index) {
                grid.set(FIELDS.get(index), m_values[index] == null ? "" : m_values[index].toString());
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        return "EnrolmentCheck.csv";
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, getCurrentStudentQuery());
        ColumnQuery query = new ColumnQuery(StudentEnrollment.class, EnrollmentRecord.COLUMNS, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, true);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, true);

        DataGrid grid = new DataGrid();
        String stdOid = null;
        String sklOid = null;
        try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                EnrollmentRecord record = new EnrollmentRecord(row);
                if (record.getStdOid().equals(stdOid)) {
                    if (sklOid == null) {
                        if (!StudentEnrollment.ENTRY.equals(record.getEnrollmentType())) {
                            record.addToGrid(grid);
                        }
                        if (!StudentEnrollment.WITHDRAWAL.equals(record.getEnrollmentType())) {
                            sklOid = record.getSklOid();
                        }
                    } else {
                        if (sklOid.equals(record.getSklOid())) {
                            if (record.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                                sklOid = null;
                            } else if (record.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                                record.addToGrid(grid);
                            }
                        } else {
                            sklOid = record.getSklOid();
                            record.addToGrid(grid);
                        }
                    }
                } else {
                    stdOid = record.getStdOid();
                    sklOid = record.getSklOid();
                    if (!StudentEnrollment.ENTRY.equals(record.getEnrollmentType())) {
                        record.addToGrid(grid);
                    }
                }
            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return EnrollmentRecord.FIELDS;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return getColumnNames();
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets the current student query.
     *
     * @return Sub query
     */
    private SubQuery getCurrentStudentQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, this.getCurrentContext().getStartDate());
        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, criteria);
    }

}
