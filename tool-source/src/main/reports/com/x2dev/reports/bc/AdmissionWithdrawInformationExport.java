/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports admission withdrawal information (student enrollment E/Ws) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class AdmissionWithdrawInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Grid fields
    private static final String FIELD_SKL_ID = "School Number";
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM_CODE = "Homeroom";
    private static final String FIELD_STF_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_ENR_ADMIT_DATE = "Admit Date";
    private static final String FIELD_ENR_ADMISSION_DESCRIPTION = "Admission Description";
    private static final String FIELD_ENR_WITHDRAW_DATE = "Withdraw Date";
    private static final String FIELD_ENR_WITHDRAW_DESCRIPTION = "Withdraw Description";

    // Other constants
    private static final int FIELD_COUNT = 11;

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_entryCodesMap;
    private DateFormat m_formatter;
    private Map<String, ReferenceCode> m_withdrawalCodesMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, buildCriteria());
        query.addOrderByAscending(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) iterator.next();
                appendBeanToGrid(grid, enrollment);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_formatter = new SimpleDateFormat("dd-MMM-yyyy");

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SKL_ID);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM_CODE);
        m_columns.add(FIELD_STF_TEACHER_NAME);
        m_columns.add(FIELD_ENR_ADMIT_DATE);
        m_columns.add(FIELD_ENR_ADMISSION_DESCRIPTION);
        m_columns.add(FIELD_ENR_WITHDRAW_DATE);
        m_columns.add(FIELD_ENR_WITHDRAW_DESCRIPTION);

        // Load entry codes
        String tableOid =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            m_entryCodesMap = table.getCodeMap(getBroker());
        }

        // Load withdrawal codes
        tableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            m_withdrawalCodesMap = table.getCodeMap(getBroker());
        }
    }

    /**
     * Appends the enrollment span to the grid.
     *
     * @param grid ReportDataGrid
     * @param enrollment StudentEnrollment
     */
    private void appendBeanToGrid(ReportDataGrid grid, StudentEnrollment enrollment) {
        boolean deleteRow = false;
        try {
            SisStudent student = enrollment.getStudent();
            SisSchool school = enrollment.getSchool();
            String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
            String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

            if (student != null && student.getPerson() != null && school != null) {
                grid.append();
                deleteRow = true;

                grid.set(FIELD_SKL_ID, school.getSchoolId());
                grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                grid.set(FIELD_STD_LAST_NAME, student.getPerson().getLastName());
                grid.set(FIELD_STD_FIRST_NAME, student.getPerson().getFirstName());
                grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                grid.set(FIELD_STD_HOMEROOM_CODE, student.getHomeroom());

                // Homeroom teacher
                Map<String, Staff> homeroomToStaffMap = getHomeroomToStaffMap(school);
                if (homeroomToStaffMap != null) {
                    Staff staff = homeroomToStaffMap.get(student.getHomeroom());
                    if (staff != null) {
                        grid.set(FIELD_STF_TEACHER_NAME, staff.getNameView());
                    }
                }

                // Entry information
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    grid.set(FIELD_ENR_ADMIT_DATE, m_formatter.format(enrollment.getEnrollmentDate()));

                    // Set reference code description instead of field data if reference table
                    // exists.
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    if (m_entryCodesMap != null && m_entryCodesMap.containsKey(enrollmentCode)) {
                        ReferenceCode code = m_entryCodesMap.get(enrollmentCode);
                        enrollmentCode = code.getDescription();
                    }
                    grid.set(FIELD_ENR_ADMISSION_DESCRIPTION, enrollmentCode);
                }

                // Withdrawal information
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    grid.set(FIELD_ENR_WITHDRAW_DATE, m_formatter.format(enrollment.getEnrollmentDate()));

                    // Set reference code description instead of field data if reference table
                    // exists.
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    if (m_withdrawalCodesMap != null && m_withdrawalCodesMap.containsKey(enrollmentCode)) {
                        ReferenceCode code = m_withdrawalCodesMap.get(enrollmentCode);
                        enrollmentCode = code.getDescription();
                    }
                    grid.set(FIELD_ENR_WITHDRAW_DESCRIPTION, enrollmentCode);
                }
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(StudentEnrollment.class.getName());
            strBldr.append(" with OID: [");
            strBldr.append(enrollment.getOid());
            SisStudent student = enrollment.getStudent();
            if (student != null) {
                strBldr.append("] for the Student with Local ID: [");
                strBldr.append(student.getLocalId());
                strBldr.append("].");
            } else {
                strBldr.append("] as it has no related Student.");
            }

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Builds the criteria for returning the student enrollment records.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL,
                StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));

        return criteria;
    }
}
