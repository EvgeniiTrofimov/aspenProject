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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports student programs (during entered date range) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StudentProgramsExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Grid fields
    private static final String FIELD_DISTRICT_NAME = "District Name";
    private static final String FIELD_SCHOOL_ID = "School Number";
    private static final String FIELD_SCHOOL_NAME = "School Name";
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_PROGRAM_DESCRIPTION = "Program Description";
    private static final String FIELD_PROGRAM_START_DATE = "Program Start Date";
    private static final String FIELD_PROGRAM_END_DATE = "Program End Date";
    private static final String FIELD_PROGRAM_START_YEAR = "Year";

    // Aliases
    private static final String ALIAS_START_YEAR = "pgm-start-year";

    // Other constants
    private static final int FIELD_COUNT = 13;

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_programCodeMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy");
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, buildCriteria());
        QueryIterator programs = getBroker().getIteratorByQuery(query);
        try {
            while (programs.hasNext()) {
                StudentProgramParticipation program = (StudentProgramParticipation) programs.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = program.getStudent();
                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                    String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                    SisSchool currentSchool = student.getSchool(getCurrentContext().getOid(), getBroker());

                    grid.append();
                    deleteRow = true;

                    /*
                     * Student fields
                     */
                    grid.set(FIELD_DISTRICT_NAME, student.getSchool().getParentOrganization().getName());
                    grid.set(FIELD_SCHOOL_ID, currentSchool.getSchoolId());
                    grid.set(FIELD_SCHOOL_NAME, currentSchool.getName());
                    grid.set(FIELD_STUDENT_ID, student.getLocalId());
                    grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_HOMEROOM, homeroom);

                    /*
                     * Program fields
                     */
                    grid.set(FIELD_PROGRAM_START_DATE, formatDate(program.getStartDate(), formatter));
                    grid.set(FIELD_PROGRAM_END_DATE, formatDate(program.getEndDate(), formatter));
                    grid.set(FIELD_PROGRAM_START_YEAR, program.getFieldValueByAlias(ALIAS_START_YEAR));

                    /*
                     * Student's homeroom teacher
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(currentSchool);
                    if (staffMap != null) {
                        Staff staff = staffMap.get(homeroom);
                        if (staff != null) {
                            grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                        }
                    }

                    /*
                     * Program description based on reference code. If no reference code exists then
                     * the code is used
                     */
                    String programCodeDescription = program.getProgramCode();
                    if (m_programCodeMap.containsKey(programCodeDescription)) {
                        programCodeDescription = m_programCodeMap.get(programCodeDescription).getDescription();
                    }
                    grid.set(FIELD_PROGRAM_DESCRIPTION, programCodeDescription);
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(program.getOid());
                    SisStudent student = program.getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
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
            programs.close();
        }

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
        m_columns.add(FIELD_DISTRICT_NAME);
        m_columns.add(FIELD_SCHOOL_ID);
        m_columns.add(FIELD_SCHOOL_NAME);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_PROGRAM_DESCRIPTION);
        m_columns.add(FIELD_PROGRAM_START_DATE);
        m_columns.add(FIELD_PROGRAM_END_DATE);
        m_columns.add(FIELD_PROGRAM_START_YEAR);

        /*
         * Get reference lookup for the program codes
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE,
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_programCodeMap = referenceTable.getCodeMap();
            }
        }
    }

    /**
     * Builds the criteria for returning the program participation records included in the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        DistrictSchoolYearContext context = getCurrentContext();

        /*
         * Get records with an end date in range or no end date
         */
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, context.getStartDate());

        X2Criteria emptyEndCriteria = new X2Criteria();
        emptyEndCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        endDateCriteria.addOrCriteria(emptyEndCriteria);
        criteria.addAndCriteria(endDateCriteria);

        /*
         * Get records with a start date in range or no start date
         */
        X2Criteria startDateCriteria = new X2Criteria();
        startDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, context.getEndDate());

        X2Criteria emptyStartCriteria = new X2Criteria();
        emptyStartCriteria.addEmpty(StudentProgramParticipation.COL_START_DATE, getBroker().getPersistenceKey());

        startDateCriteria.addOrCriteria(emptyStartCriteria);
        criteria.addAndCriteria(startDateCriteria);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.REL_SCHOOL,
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_SCHOOL_OID));

        if (getSchool() != null) {
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS +
                            PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey());

            primaryCriteria.addOrCriteria(secondaryCriteria);
        }

        criteria.addAndCriteria(primaryCriteria);

        return criteria;
    }

    /**
     * Formats the passed date. If the date is NULL, empty string is returned.
     *
     * @param date PlainDate
     * @param formatter DateFormat
     * @return String
     */
    private String formatDate(PlainDate date, DateFormat formatter) {
        String value = "";

        if (date != null) {
            value = formatter.format(date);
        }

        return value;
    }
}
