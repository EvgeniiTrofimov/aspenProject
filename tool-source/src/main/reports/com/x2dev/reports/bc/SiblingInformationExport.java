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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentStudent;
import com.follett.fsc.core.k12.business.StudentManager;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports sibling information for BC's GDE. Sibling are student contacts with a relationship code
 * of "Sibling" (as
 * defined in the input definition).
 *
 * @author Follett Software Company
 */
public class SiblingInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Grid fields
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_SURNAME = "Surname";
    private static final String FIELD_USUAL_NAME = "Usual First Name";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_RELATIONSHIP = "Relationship";
    private static final String FIELD_SIBLING_ID = "Sibling Student Number";
    private static final String FIELD_SIBLING_SURNAME = "Sibling Surname";
    private static final String FIELD_SIBLING_USUAL_NAME = "Sibling Usual First Name";
    private static final String FIELD_SIBLING_SCHOOL = "Sibling School";

    // Aliases
    private static final String ALIAS_SURNAME = "psn-surname";
    private static final String ALIAS_USUAL_NAME = "psn-preferred-first-name";

    // Other constants
    private static final int FIELD_COUNT = 14;

    private List<String> m_columns;
    private StudentContextReportHelper m_helper;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(StudentStudent.class, buildCriteria());
        query.addOrderByAscending(StudentStudent.REL_STUDENT + PATH_DELIMITER + Student.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(StudentStudent.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
        query.addOrderByAscending(StudentStudent.COL_STUDENT_OID);
        query.addOrderByAscending(StudentStudent.REL_STUDENT_REL + PATH_DELIMITER + Student.COL_NAME_VIEW);
        query.addOrderByAscending(StudentStudent.COL_STUDENT_REL_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentStudent sibling = (StudentStudent) iterator.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = (SisStudent) sibling.getStudent();

                    Student siblingStudent = sibling.getStudentRel();
                    Person siblingPerson = siblingStudent.getPerson();

                    if (siblingPerson != null) {
                        grid.append();
                        deleteRow = true;

                        String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                        String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                        /*
                         * Set student information
                         */
                        grid.set(FIELD_STUDENT_ID, student.getLocalId());
                        grid.set(FIELD_SURNAME, student.getPerson().getFieldValueByAlias(ALIAS_SURNAME));
                        grid.set(FIELD_USUAL_NAME, student.getPerson().getFieldValueByAlias(ALIAS_USUAL_NAME));
                        grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                        grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                        grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                        grid.set(FIELD_HOMEROOM, student.getHomeroom());
                        grid.set(FIELD_SCHOOL_NUMBER, student.getSchool().getSchoolId());

                        /*
                         * Set the sibling information
                         */
                        grid.set(FIELD_RELATIONSHIP, sibling.getRelationshipCode());
                        grid.set(FIELD_SIBLING_SURNAME, siblingPerson.getLastName());
                        grid.set(FIELD_SIBLING_USUAL_NAME, siblingPerson.getFirstName());
                        grid.set(FIELD_SIBLING_ID, siblingStudent.getLocalId());
                        grid.set(FIELD_SIBLING_SCHOOL, siblingStudent.getSchool().getSchoolId());

                        /*
                         * Pull the student's homeroom teacher
                         */
                        Map<String, Staff> staffMap = getHomeroomToStaffMap(student.getSchool());
                        if (staffMap != null) {
                            Staff staff = staffMap.get(student.getHomeroom());
                            if (staff != null) {
                                grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                            }
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(sibling.getOid());
                    Student student = sibling.getStudent();
                    Student siblingStudent = sibling.getStudentRel();
                    if (student != null && siblingStudent != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("] with sibing Student of Local ID:[");
                        strBldr.append(siblingStudent.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student or sibling Student.");
                    }


                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }

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
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
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

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_SURNAME);
        m_columns.add(FIELD_USUAL_NAME);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_RELATIONSHIP);
        m_columns.add(FIELD_SIBLING_ID);
        m_columns.add(FIELD_SIBLING_SURNAME);
        m_columns.add(FIELD_SIBLING_USUAL_NAME);
        m_columns.add(FIELD_SIBLING_SCHOOL);

        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
    }

    /**
     * Returns the criteria for the siblings (contacts with specific relationship codes) in the
     * extract.
     * 
     * @return String
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentStudent.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship(),
                StudentStudent.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField()));
        criteria.addAndCriteria(m_helper.getActiveStudentCriteria(StudentStudent.REL_STUDENT + PATH_DELIMITER));

        if (getSchool() != null) {
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    StudentStudent.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER,
                    getCurrentContext().getOid(),
                    getSchool().getOid(),
                    null,
                    null,
                    getBroker().getPersistenceKey());

            criteria.addOrCriteria(secondaryCriteria);
        }

        return criteria;
    }
}
